# This file provides a class that parses the Code -member
# of the PDL::PP code.
#
# This is what makes the nice loops go around etc.
#

package PDL::PP::Code;

use strict;
use warnings;
use Carp;

sub get_pdls { @{$_[0]}{qw(ParNames ParObjs)} }

my @code_args_always = qw(BadFlag SignatureObj GenericTypes ExtraGenericSwitches HaveBroadcasting Name);
sub make_args {
  my ($target) = @_;
  ("${target}CodeParsed", ["${target}CodeUnparsed","Bad${target}CodeUnparsed?",@code_args_always]);
}

# Do the appropriate substitutions in the code.
sub new {
    my($class,$code,$badcode,
       $handlebad, $sig,$generictypes,$extrageneric,$havebroadcasting,$name,
       $dont_add_brcloop, $backcode, $nulldatacheck) = @_;
    my $parnames = $sig->names_sorted;
    $handlebad = !!$handlebad;

    confess "Error: missing name argument to PDL::PP::Code->new call!\n"
      unless defined $name;
    confess "Error: empty or undefined GenericTypes!\n"
      unless @{$generictypes || []};

    $badcode //= $code if $handlebad;

    # last two arguments may not be supplied
    #
    # "backcode" is a flag to the PDL::PP::Broadcastloop class indicating the broadcastloop
    #   is for writeback code (typically used for writeback of data from child to parent PDL

    $dont_add_brcloop ||= !$havebroadcasting; # two have identical (though inverted) meaning so only track one

    # C++ style comments
    #
    # This regexp isn't perfect because it doesn't cope with
    # literal string constants.
    #
    $code =~ s,//.*?\n,,g;

    if ($::PP_VERBOSE) {
	print "Processing code for $name\n";
	print "DONT_ADD_BRCLOOP!\n" if $dont_add_brcloop;
	print "EXTRAGEN: {" .
	  join(" ",
	       map "$_=>$$extrageneric{$_}", sort keys %$extrageneric)
	    . "}\n";
	print "ParNAMES: ",(join ',',@$parnames),"\n";
	print "GENTYPES: ", @$generictypes, "\n";
	print "HandleBad: $handlebad\n";
    }
    my $this = bless {
	IndObjs => $sig->dims_obj,
	ParNames => $parnames,
	ParObjs => $sig->objs,
	Sig => $sig,
	Gencurtype => [], # stack to hold GenType in generic switches
	ftypes_vars => {},
	ftypes_type => undef,
        Generictypes => $generictypes,   # so that MacroAccess can check it
        Name => $name,
        NullDataCheck => $nulldatacheck,
    }, $class;

    my @codes = $code;
    push @codes, $badcode if $handlebad && ($code ne $badcode || $badcode =~ /PDL_BAD_CODE|PDL_IF_BAD/);
    my (@coderefs, @sizeprivs);
    for my $c (@codes) {
      # First, separate the code into an array of C fragments (strings),
      # variable references (strings starting with $) and
      # loops (array references, 1. item = variable.
      my ( $broadcastloops, $coderef, $sizeprivs ) =
          $this->separate_code( "{$c}" );
      # Now, if there is no explicit broadcastlooping in the code,
      # enclose everything into it.
      if(!$broadcastloops && !$dont_add_brcloop) {
          print "Adding broadcastloop...\n" if $::PP_VERBOSE;
          $coderef = $coderef->enter(('PDL::PP::'.($backcode ? 'BackCode' : '').'BroadcastLoop')->new);
      }
      # Enclose it all in a generic switch.
      my $if_gentype = ($code.($badcode//'')) =~ /PDL_IF_GENTYPE_/;
      $coderef = $coderef->enter(PDL::PP::GenericSwitch->new($generictypes, undef,
        [grep {!$extrageneric->{$_}} @$parnames],'$PRIV(__datatype)',$if_gentype));
      # Do we have extra generic switches?
      # If we do, first reverse the hash:
      my %glh;
      push @{$glh{$extrageneric->{$_}}},$_ for sort keys %$extrageneric;
      my $no = 0;
      $coderef = $coderef->enter(PDL::PP::GenericSwitch->new($generictypes,$no++,
        $glh{$_},$_,$if_gentype)) for sort keys %glh;
      push @coderefs, $coderef;
      push @sizeprivs, $sizeprivs;
    }
    amalgamate_sizeprivs(@sizeprivs) if @sizeprivs > 1;
    my $sizeprivs = $sizeprivs[0];
    my $coderef = PDL::PP::BadSwitch->new( @coderefs );
    print "SIZEPRIVSX: ",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;

    my $pobjs = $sig->objs;
    # Then, in this form, put it together what we want the code to actually do.
    print "SIZEPRIVS: ",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;
    $this->{Code} = (join '',sort values %$sizeprivs).
       ($dont_add_brcloop?'':join '', map "$_\n",
        'if (!$PRIV(broadcast).incs) $CROAK("broadcast.incs NULL");',
        'PDL_COMMENT("broadcastloop declarations")',
        'int __brcloopval;',
        'register PDL_Indx __tind0,__tind1; PDL_COMMENT("counters along dim")',
        'register PDL_Indx __tnpdls = $PRIV(broadcast).npdls;',
        'PDL_COMMENT("dims here are how many steps along those dims")',
        (map "register PDL_Indx __tinc0_$parnames->[$_] = PDL_BRC_INC(\$PRIV(broadcast).incs,__tnpdls,$_,0);", 0..$#$parnames),
        (map "register PDL_Indx __tinc1_$parnames->[$_] = PDL_BRC_INC(\$PRIV(broadcast).incs,__tnpdls,$_,1);", 0..$#$parnames),
        eol_protect(
         "#define ".$this->broadcastloop_macroname($backcode, 'START') . " " .
           $this->broadcastloop_start($this->func_name($backcode))
        ),
        eol_protect(
         "#define ".$this->broadcastloop_macroname($backcode, 'END') . " " .
           $this->broadcastloop_end
        ),
        (grep $_, map $_->get_incregisters, @$pobjs{sort keys %$pobjs}),
       ).
       $this->params_declare.
       $coderef->get_str($this,[])
       ;
    $this->{Code};
} # new

# amalgamate sizeprivs from Code/BadCode segments
# (sizeprivs is a simple hash, with each element
# containing a string - see PDL::PP::Loop)
sub amalgamate_sizeprivs {
  my ($sizeprivs, $bad_sizeprivs) = @_;
  while ( my ( $bad_key, $bad_str ) = each %$bad_sizeprivs ) {
    my $str = $$sizeprivs{$bad_key};
    die "ERROR: sizeprivs problem in PP/PDLCode.pm (BadVal stuff)\n"
        if defined $str and $str ne $bad_str;
    $$sizeprivs{$bad_key} = $bad_str;  # copy over
  }
}

sub eol_protect {
  my ($text) = @_;
  join " \\\n", grep /\S/, split /\n/, $text;
}

sub params_declare {
    my ($this) = @_;
    my ($ord,$pdls) = $this->get_pdls;
    my %istyped = map +($_=>1), grep $pdls->{$_}{FlagTypeOverride}, @$ord;
    my @decls = map $_->get_xsdatapdecl($istyped{$_->name} ? "PDL_TYPE_PARAM_".$_->name : "PDL_TYPE_OP", $this->{NullDataCheck}, $istyped{$_->name} ? "PDL_PPSYM_PARAM_".$_->name : "PDL_PPSYM_OP"),
      map $pdls->{$_}, @$ord;
    my @param_names = ("PDL_TYPE_OP", "PDL_PPSYM_OP", map +("PDL_TYPE_PARAM_$_","PDL_PPSYM_PARAM_$_"), grep $istyped{$_}, @$ord);
    <<EOF;
#ifndef PDL_DECLARE_PARAMS_$this->{Name}_$this->{NullDataCheck}
#define PDL_DECLARE_PARAMS_$this->{Name}_$this->{NullDataCheck}(@{[join ',', @param_names]}) \\
  @{[join " \\\n  ", @decls]}
#endif
EOF
}

sub func_name { $_[1] ? "writebackdata" : "readdata" }

sub broadcastloop_macroname {
    my ($this, $backcode, $which) = @_;
    "PDL_BROADCASTLOOP_${which}_$this->{Name}_".$this->func_name($backcode);
}

sub broadcastloop_start {
    my ($this, $funcname) = @_;
    my ($ord,$pdls) = $this->get_pdls;
    <<EOF;
PDL_BROADCASTLOOP_START(
  $funcname,
  \$PRIV(broadcast),
  \$PRIV(vtable),
@{[ PDL::PP::indent 2, join "", map $pdls->{$ord->[$_]}->do_pointeraccess." += __offsp[$_];\n", 0..$#$ord ]}  ,
  (@{[ PDL::PP::indent 2, join "", map ",".$pdls->{$ord->[$_]}->do_pointeraccess." += __tinc1_$ord->[$_] - __tinc0_$ord->[$_] * __tdims0\n", 0..$#$ord ]}  ),
  (@{[ PDL::PP::indent 2, join "", map ",".$pdls->{$ord->[$_]}->do_pointeraccess." += __tinc0_$ord->[$_]\n", 0..$#{$ord} ]}  )
)
EOF
}

sub broadcastloop_end {
    my ($this) = @_;
    my ($ord,$pdls) = $this->get_pdls();
    <<EOF;
PDL_BROADCASTLOOP_END(
  \$PRIV(broadcast),
@{[ PDL::PP::indent 2, join "", map $pdls->{$ord->[$_]}->do_pointeraccess." -= __tinc1_$ord->[$_] * __tdims1 + __offsp[$_];\n", 0..$#$ord ]}
)
EOF
}

sub sig {$_[0]->{Sig}}

# This sub determines the index name for this index.
# For example, a(x,y) and x0 becomes [x,x0]
sub make_loopind { my($this,$ind) = @_;
  ($ind, my $cntrlval) = split /\s*=\s*/, $ind;
  my $orig = $ind;
  while(!$this->{IndObjs}{$ind}) {
    if(!((chop $ind) =~ /[0-9]/)) {
      confess("Index not found for $_ ($ind)!\n");
    }
  }
  my ($initval, $endval, $inc) = split /\s*:\s*/, $cntrlval//'';
  [$ind,$orig,$initval,$endval,$inc];
}

my %access2class = (
  GENERIC => 'PDL::PP::GentypeAccess',
  PPSYM => 'PDL::PP::PpsymAccess',
);

sub process {
    my ($this, $code, $stack_ref, $broadcastloops_ref, $sizeprivs) = @_;
    while($code) {
	# Parse next statement
	$code =~ s/^(.*?) # First, some noise is allowed. This may be bad.
	    ( \$(ISBAD|ISGOOD|SETBAD)\s*\(\s*\$?[a-zA-Z_]\w*\s*\([^)]*\)\s*\)   # $ISBAD($a(..)), ditto for ISGOOD and SETBAD
	        |\$[a-zA-Z_]\w*\s*\([^)]*\)  # $a(...): access
		|\bloop\s*\([^)]+\)\s*%\{   # loop(..) %{
		|\btypes\s*\([^)]+\)\s*%\{  # types(..) %{
		|\b(?:thread|broadcast)loop\s*%\{         # broadcastloop %{
		|%}                        # %}
		|$)//xs
		    or confess("Invalid program $code");
	my $control = $2;
	# Store the user code.
	# Some day we shall parse everything.
	push @{$stack_ref->[-1]},$1;
	# Then, our control.
	if (!$control) { print("No \$2!\n") if $::PP_VERBOSE; next; }
	if($control =~ /^loop\s*\(([^)]+)\)\s*%\{/) {
	    my $ob = PDL::PP::Loop->new([split ',',$1], $sizeprivs,$this);
	    print "SIZEPRIVSXX: $sizeprivs,",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;
	    push @{$stack_ref->[-1]},$ob;
	    push @$stack_ref,$ob;
	} elsif($control =~ /^types\s*\(([^)]+)\)\s*%\{/) {
	    my $ob = PDL::PP::Types->new($1,$this);
	    push @{$stack_ref->[-1]},$ob;
	    push @$stack_ref,$ob;
	} elsif($control =~ /^(?:thread|broadcast)loop\s*%\{/) {
	    my $ob = PDL::PP::BroadcastLoop->new;
	    push @{$stack_ref->[-1]},$ob;
	    push @$stack_ref,$ob;
	    $$broadcastloops_ref++;
	} elsif($control =~ /^%}/) {
	    pop @$stack_ref;
	} else {
	    my ($rest, @add) = $this->expand($control.$code);
	    push @{$stack_ref->[-1]}, @add;
	    $code = $rest;
	}
    } # while: $code
}

# my ( $broadcastloops, $coderef, $sizeprivs ) = $this->separate_code( $code );
#
# separates the code into an array of C fragments (strings),
# variable references (strings starting with $) and
# loops (array references, 1. item = variable.
#
sub separate_code {
    my ( $this, $code ) = @_;
    # First check for standard code errors:
    $this->catch_code_errors($code);
    my @stack = my $coderef = PDL::PP::Block->new;
    my $broadcastloops = 0;
    my $sizeprivs = {};
    $this->process($code, \@stack, \$broadcastloops, $sizeprivs);
    ( $broadcastloops, $coderef, $sizeprivs );
} # sub: separate_code()

my $macro_pat = qr/\w+/;
sub expand {
    my ($this, $text) = @_;
    my (undef, $pdl, $inds, $rest) = PDL::PP::Rule::Substitute::macro_extract($text, $macro_pat);
    my @add;
    if($pdl =~ /^T/) {@add = PDL::PP::MacroAccess->new($pdl,$inds,
			   $this->{Generictypes},$this->{Name});}
    elsif(my $c = $access2class{$pdl}) {@add = $c->new($pdl,$inds)}
    elsif($pdl =~ /^(P|)(ISBAD|ISGOOD|SETBAD)(VAR|)$/) {
	my ($opcode, $name) = ($2);
	my $get = $1 || $3;
	if (!$get) {
	    $inds =~ s/^\$?([a-zA-Z_]\w*)\s*//; # $ is optional
	    $name = $1;
	    $inds = substr $inds, 1, -1; # chop off brackets
	} elsif ($get eq 'P') {
	    ($name, $inds) = PDL::PP::Rule::Substitute::split_cpp($inds);
	} else {
	    ($inds, $name) = PDL::PP::Rule::Substitute::split_cpp($inds);
	}
	@add = PDL::PP::BadAccess->new($opcode,$get,$name,$inds,$this);
    }
    elsif($this->{ParObjs}{$pdl}) {@add = PDL::PP::Access->new($pdl,$inds)}
    else {
	confess "unknown construct $pdl($inds)";
    }
    ($rest, @add);
}

# This is essentially a collection of regexes that look for standard code
# errors and croaks with an explanation if they are found.
sub catch_code_errors {
  my ($this, $code_string) = @_;
  my $prefix = "pp_def($this->{Name}): ";
  report_error("${prefix}Expected dimension name after 'loop' and before '%{'", $1)
    if $code_string =~ /(.*\bloop\s*%\{)/s;
}

# Report an error as precisely as possible. If they have #line directives
# in the code string, use that in the reporting; otherwise, use standard
# Carp mechanisms
my $line_re = qr/(?:PDL_LINENO_START|#\s*line)\s+(\d+)\s+"([^"]*)"/;
sub report_error {
    my ($message, $code) = @_;
    # Just croak if they didn't supply a #line directive:
    croak($message) if $code !~ $line_re;
    # Find the line at which the error occurred:
    my $line = 0;
    my $filename;
    LINE: foreach (split /\n/, $code) {
	    $line++;
	    if (/$line_re/) {
		    $line = $1;
		    $filename = $2;
	    }
    }
    die "$message at $filename line $line\n";
}

#####################################################################
#
# Encapsulate the parsing code objects
#
# All objects have two methods:
# 	new - constructor
#	get_str - get the string to be put into the xsub.

package PDL::PP::Block;

sub new { my($type) = @_; bless [],$type; }

sub myoffs { 0 }
sub myextraindent { 0 }
sub myprelude {}
sub mypostlude {}

sub get_str {
    my ($this,$parent,$context) = @_;
    my $str = $this->myprelude($parent,$context);
    $str .= PDL::PP::indent 2, $this->get_str_int($parent,$context)//'';
    $str .= $this->mypostlude($parent,$context)//'';
    return $str;
}

sub get_str_int {
  my ( $this, $parent, $context ) = @_;
  my $nth=0;
  my $str = "";
  MYLOOP: while(1) {
    my $it = $this->can('myitemstart') && $this->myitemstart($parent,$nth);
    last MYLOOP if $nth and !$it;
    $str .= $it//'';
    $str .= PDL::PP::indent $this->myextraindent, join '', $this->get_contained($parent,$context);
    $str .= $it if $it = $this->can('myitemend') && $this->myitemend($parent,$nth);
    $nth++;
  }
  return $str;
} # get_str_int()

sub get_contained {
  my ($this, $parent, $context) = @_;
  map ref($_) ? $_->get_str($parent, $context) : $_,
    @$this[$this->myoffs..$#$this];
}

sub enter {
  my ($this, $new) = @_;
  push @$new, $this;
  $new;
}

###########################
#
# Deal with bad code
# - ie create something like
#   if ( badflag ) { badcode } else { goodcode }
#
package PDL::PP::BadSwitch;
our @ISA = "PDL::PP::Block";

sub new {
    my($type,$good,$bad) = @_;
    return bless [$good,$bad], $type;
}

sub get_str {
    my ($this,$parent,$context) = @_;
    my $good = $this->[0];
    my $good_str = <<EOF;
#define PDL_IF_BAD(t,f) f
@{[ $good->get_str($parent,$context)
]}#undef PDL_IF_BAD
EOF
    return $good_str if !defined(my $bad  = $this->[1]);
    my $str = <<EOF;
if ( \$PRIV(bvalflag) ) { PDL_COMMENT("** do 'bad' Code **")
  #define PDL_BAD_CODE
  #define PDL_IF_BAD(t,f) t
@{[ PDL::PP::indent 2, $bad->get_str($parent,$context)
]}  #undef PDL_BAD_CODE
  #undef PDL_IF_BAD
} else { PDL_COMMENT("** else do 'good' Code **")
@{[ PDL::PP::indent 2, $good_str
]}}
EOF
}

package PDL::PP::Loop;
our @ISA = "PDL::PP::Block";

sub new { my($type,$args,$sizeprivs,$parent) = @_;
  my $this = bless [$args],$type;
  for (@$args) {
    print "SIZP $sizeprivs, $_\n" if $::PP_VERBOSE;
    my $i = $parent->make_loopind($_);
    my $i_size = $parent->sig->dims_obj->ind_obj($i->[0])->get_size;
    $sizeprivs->{$i->[0]} = "register PDL_Indx __$i->[0]_size = $i_size;\n";
    print "SP :",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;
  }
  return $this;
}

sub myoffs { return 1; }
sub myprelude { my($this,$parent,$context) = @_;
  my $text = "";
  push @$context, map {
    my $i = $parent->make_loopind($_);
    my ($loopdim, $loopvar, $loopstart, $loopend, $loopinc) = @$i;
    my $loopstopvar = "__${loopvar}_stop";
    $loopinc ||= 1; my $cmp;
    if ($loopinc =~ /^-/) {
      $loopstart = !(defined $loopstart && length $loopstart) ? "(__${loopdim}_size-1)" :
        $loopstart =~ /^-/ ? "PDLMIN((__${loopdim}_size$loopstart), (__${loopdim}_size-1))" :
        "PDLMIN($loopstart, (__${loopdim}_size-1))";
      $cmp = ">=";
      $loopend = !$loopend ? 0 :
        $loopend =~ /^-/ ? "PDLMAX((__${loopdim}_size$loopend),0)" :
        "PDLMAX(($loopend),0)";
    } else {
      # count upwards
      $loopstart = !$loopstart ? 0 :
        $loopstart =~ /^-/ ? "PDLMAX((__${loopdim}_size$loopstart),0)" :
        "PDLMAX(($loopstart),0)";
      $cmp = "<";
      $loopend = !(defined $loopend && length $loopend) ? "(__${loopdim}_size)" :
        $loopend =~ /^-/ ? "(__${loopdim}_size$loopend)" :
        "PDLMIN($loopend, (__${loopdim}_size))";
    }
    $text .= "{PDL_COMMENT(\"Open $_\") PDL_EXPAND2(register PDL_Indx $loopvar=$loopstart, $loopstopvar=$loopend); for(; $loopvar$cmp$loopstopvar; $loopvar+=$loopinc) {";
    $i;
  } @{$this->[0]};
  $text;
}
sub mypostlude { my($this,$parent,$context) = @_;
  splice @$context, - ($#{$this->[0]}+1);
  return join '', map "}} PDL_COMMENT(\"Close $_\")", @{$this->[0]};
}

package PDL::PP::GenericSwitch;
use Carp;
our @ISA = "PDL::PP::Block";

# make the typetable from info in PDL::Types
use PDL::Types ':All';
my %type2canonical = map +($_->ppsym=>$_,$_->identifier=>$_), types();
my @typetable = map [$_->ppsym, $_], types();
sub get_generictyperecs { my($types) = @_;
  my @bad = grep !$type2canonical{$_}, @$types;
  confess "Invalid GenericType (@bad)!" if @bad;
  my %wanted; @wanted{map $type2canonical{$_}->ppsym, @$types} = ();
  [ map $_->[1], grep exists $wanted{$_->[0]}, @typetable ];
}

# Types: BSULFD
sub new {
    my ($type,$types,$name,$varnames,$whattype,$if_gentype) = @_;
    my %vars; @vars{@$varnames} = ();
    bless [get_generictyperecs($types), $name, \%vars, $whattype, $if_gentype], $type;
}

sub myoffs {5}
sub myextraindent { 2 }

sub myprelude {
    my ($this,$parent,$context) = @_;
    push @{$parent->{Gencurtype}}, undef; # so that $GENERIC can get at it
    die "ERROR: need to rethink NaN support in GenericSwitch\n"
	if defined $this->[1] and $parent->{ftypes_type};
    qq[switch ($this->[3]) { PDL_COMMENT("Start generic switch")\n];
}

my @GENTYPE_ATTRS = qw(integer real unsigned);
sub myitemstart {
    my ($this,$parent,$nth) = @_;
    my $item = $this->[0][$nth] || return "";
    $parent->{Gencurtype}[-1] = $item;
    @$parent{qw(ftypes_type ftypes_vars)} = ($item, $this->[2]) if defined $this->[1];
    my ($ord,$pdls) = $parent->get_pdls;
    my %istyped = map +($_=>1), grep $pdls->{$_}{FlagTypeOverride}, @$ord;
    my @param_ctypes = ($item->ctype, $item->ppsym,
      map +($pdls->{$_}->adjusted_type($item)->ctype,
        $pdls->{$_}->adjusted_type($item)->ppsym),
      grep $istyped{$_}, @$ord);
    my $decls = keys %{$this->[2]} == @$ord
      ? "PDL_DECLARE_PARAMS_$parent->{Name}_$parent->{NullDataCheck}(@{[join ',', @param_ctypes]})\n"
      : join '', map $_->get_xsdatapdecl($_->adjusted_type($item)->ctype, $parent->{NullDataCheck}, $_->adjusted_type($item)->ppsym),
          map $parent->{ParObjs}{$_}, sort keys %{$this->[2]};
    my @gentype_decls = !$this->[4] ? () : map "#define PDL_IF_GENTYPE_".uc($_)."(t,f) ".
	($item->$_ ? 't' : 'f')."\n",
	@GENTYPE_ATTRS;
    "case @{[$item->sym]}: {\n" .
	PDL::PP::indent 2, join '',
	@gentype_decls,
	$decls;
}

sub myitemend {
    my ($this,$parent,$nth) = @_;
    my $item = $this->[0][$nth] || return "";
    join '',
	"\n",
	(!$this->[4] ? () : map "#undef PDL_IF_GENTYPE_".uc($_)."\n", @GENTYPE_ATTRS),
	"} break;\n";
}

sub mypostlude {
    my($this,$parent,$context) = @_;
    pop @{$parent->{Gencurtype}};  # and clean up the Gentype stack
    $parent->{ftypes_type} = undef if defined $this->[1];
    my $supported = join '', map $_->ppsym, @{$this->[0]};
    "  default: return PDL->make_error(PDL_EUSERERROR, \"PP INTERNAL ERROR in $parent->{Name}: unhandled datatype(%d), only handles ($supported)! PLEASE MAKE A BUG REPORT\\n\", $this->[3]);\n}\n";
}

####
#
# This relies on PP.pm making sure that initbroadcaststruct always sets
# up the two first dimensions even when they are not necessary.
#
package PDL::PP::BroadcastLoop;
use Carp;
our @ISA = "PDL::PP::Block";

sub new {
	my $type   = shift;
	bless [],$type;
}
sub myoffs { return 0; }
sub myprelude {
    my($this,$parent,$context,$backcode) = @_;
    $parent->broadcastloop_macroname($backcode, 'START');
}

sub mypostlude {my($this,$parent,$context,$backcode) = @_;
    $parent->broadcastloop_macroname($backcode, 'END');
}

# Simple subclass of BroadcastLoop to implement writeback code
#
#
package PDL::PP::BackCodeBroadcastLoop;
use Carp;
our @ISA = "PDL::PP::BroadcastLoop";

sub myprelude {
    my($this,$parent,$context,$backcode) = @_;
    # Set backcode flag if not defined. This will make the parent
    #   myprelude emit proper writeback code
    $this->SUPER::myprelude($parent, $context, $backcode // 1);
}

sub mypostlude {
    my($this,$parent,$context,$backcode) = @_;
    # Set backcode flag if not defined. This will make the parent
    #   mypostlude emit proper writeback code
    $this->SUPER::mypostlude($parent, $context, $backcode // 1);
}

###########################
#
# Encapsulate a types() switch
#
package PDL::PP::Types;
use Carp;
use PDL::Types ':All';
our @ISA = "PDL::PP::Block";
my %types = map +($_=>1), ppdefs_all; # BSUL....

sub new {
    my($type,$ts,$parent) = @_;
    my @bad = grep !$types{$_}, my @ts = split '', $ts;
    confess "Invalid type access (@bad) in '$ts'!" if @bad;
    bless [+{map +($_=>1), @ts}],$type; }
sub myoffs { return 1; }

sub get_str {
  my ($this,$parent,$context) = @_;
  confess "types() outside a generic switch"
    unless defined(my $type = $parent->{Gencurtype}[-1]);
  return '' if !$this->[0]{$type->ppsym};
  join '', $this->get_contained($parent,$context);
}


package PDL::PP::Access;
use Carp;

sub new { my($type,$pdl,$inds) = @_;
    bless [$pdl,$inds],$type;
}

sub get_str { my($this,$parent,$context) = @_;
    $parent->{ParObjs}{$this->[0]}->do_access($this->[1],$context)
	if defined($parent->{ParObjs}{$this->[0]});
}

###########################
# Encapsulate a check on whether a value is good or bad
# handles both checking (good/bad) and setting (bad)
package PDL::PP::BadAccess;
use Carp;

sub new {
    my ( $type, $opcode, $get, $name, $inds, $parent ) = @_;
    die "\nIt looks like you have tried a $get \$${opcode}() macro on an" .
	" unknown ndarray <$name($inds)>\n"
	unless defined($parent->{ParObjs}{$name});
    bless [$opcode, $get, $name, $inds], $type;
}

sub _isbad { "PDL_ISBAD2($_[0],$_[1],$_[2],$_[3])" }
our %ops = (
    ISBAD => \&_isbad,
    ISGOOD => sub {'!'.&_isbad},
    SETBAD => sub{join '=', @_[0,1]},
);
my %getters = (
    '' => sub {my ($obj, $inds, $context)=@_; $obj->do_access($inds,$context)},
    P => sub {my ($obj, $inds)=@_; $obj->do_pointeraccess.$inds},
    VAR => sub {my ($obj, $inds)=@_; $inds},
);

sub get_str {
    my ($this,$parent,$context) = @_;
    my ($opcode, $get, $name, $inds) = @$this;
    confess "generic type access outside a generic switch in $name"
      unless defined $parent->{Gencurtype}[-1];
    print "PDL::PP::BadAccess sent [$opcode] [$name] [$inds]\n" if $::PP_VERBOSE;
    die "ERROR: unknown check <$opcode> sent to PDL::PP::BadAccess\n"
	unless defined( my $op = $ops{$opcode} );
    die "ERROR: something screwy in PDL::PP::BadAccess (PP/PDLCode.pm)\n"
	unless defined( my $obj = $parent->{ParObjs}{$name} );
    my $lhs = $getters{$get}->($obj, $inds, $context);
    my $rhs = "${name}_badval";
    print "DBG:  [$lhs $op $rhs]\n" if $::PP_VERBOSE;
    my $type = exists $parent->{ftypes_vars}{$name}
	? $parent->{ftypes_type}
	: $obj->adjusted_type($parent->{Gencurtype}[-1]);
    $op->($lhs, $rhs, $type->ppsym, $rhs."_isnan");
}


package PDL::PP::MacroAccess;
use Carp;
use PDL::Types ':All';
my $types = join '',ppdefs_all;

sub new {
    my ($type, $pdl, $inds, $gentypes, $name) = @_;
    $pdl =~ /^\s*T([A-Z]+)\s*$/
      or confess("Macroaccess wrong in $name (allowed types $types): was '$pdl'\n");
    my @ilst = split '', $1;
    my @lst = PDL::PP::Rule::Substitute::split_cpp($inds);
    confess "Macroaccess: different nos of args $pdl (@{[scalar @lst]}=@lst) vs (@{[scalar @ilst]}=@ilst)\n" if @lst != @ilst;
    my %type2value; @type2value{@ilst} = @lst;
    confess "$name has no Macro for generic type $_ (has $pdl)\n"
	for grep !exists $type2value{$_}, @$gentypes;
    my %gts; @gts{@$gentypes} = ();
    warn "Macro for unsupported generic type identifier $_\n"
	for grep !exists $gts{$_}, @ilst;
    bless [\%type2value, $name], $type;
}

sub get_str {
    my ($this, $parent, $context) = @_;
    my ($type2value, $name) = @{$this};
    confess "generic type access outside a generic switch in $name"
      unless defined $parent->{Gencurtype}[-1];
    $type2value->{$parent->{Gencurtype}[-1]->ppsym};
}

package PDL::PP::GentypeAccess;
use Carp;

sub new { my($type,$pdl,$inds) = @_; bless [$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
  confess "generic type access outside a generic switch"
    unless defined(my $type = $parent->{Gencurtype}[-1]);
  return $type->ctype if !$this->[0];
  my $pobj = $parent->{ParObjs}{$this->[0]} // confess "not a defined parname";
  $pobj->adjusted_type($type)->ctype;
}

package PDL::PP::PpsymAccess;
use Carp;

sub new { my($type,$pdl,$inds) = @_; bless [$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
  confess "generic type access outside a generic switch"
    unless defined(my $type = $parent->{Gencurtype}[-1]);
  return $type->ppsym if !$this->[0];
  my $pobj = $parent->{ParObjs}{$this->[0]} // confess "not a defined parname";
  $pobj->adjusted_type($type)->ppsym;
}

1;
