# This file provides a class that parses the Code -member
# of the PDL::PP code.
#
# This is what makes the nice loops go around etc.
#

package PDL::PP::Code;
use Carp;
our @CARP_NOT;

use strict;

sub get_pdls {my($this) = @_; return ($this->{ParNames},$this->{ParObjs});}

my @code_args_always = qw(SignatureObj GenericTypes ExtraGenericLoops HaveThreading Name);
sub make_args {
  my ($which) = @_;
  ("Parsed$which", [$which,"_Bad$which",@code_args_always]);
}

# Do the appropriate substitutions in the code.
sub new {
    my($class,$code,$badcode,
       $sig,$generictypes,$extrageneric,$havethreading,$name,
       $dont_add_thrloop, $backcode ) = @_;
    my $parnames = $sig->names_sorted;

    die "Error: missing name argument to PDL::PP::Code->new call!\n"
      unless defined $name;
    confess "Error: empty or undefined GenericTypes!\n"
      unless @{$generictypes || []};

    # simple way of handling bad code check
    my $handlebad = defined($badcode);

    # last two arguments may not be supplied
    #
    # "backcode" is a flag to the PDL::PP::Threadloop class indicating thre threadloop
    #   is for writeback code (typically used for writeback of data from child to parent PDL

    $dont_add_thrloop ||= !$havethreading; # two have identical (though inverted) meaning so only track one

    # C++ style comments
    #
    # This regexp isn't perfect because it doesn't cope with
    # literal string constants.
    #
    $code =~ s,//.*?\n,,g;

    if ($::PP_VERBOSE) {
	print "Processing code for $name\n";
	print "DONT_ADD_THRLOOP!\n" if $dont_add_thrloop;
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
	Gencurtype => [], # stack to hold GenType in generic loops
	ftypes_vars => {},
	ftypes_type => undef,
        Generictypes => $generictypes,   # so that MacroAccess can check it
        Name => $name,
    }, $class;

    # First, separate the code into an array of C fragments (strings),
    # variable references (strings starting with $) and
    # loops (array references, 1. item = variable.
    #
    my ( $threadloops, $coderef, $sizeprivs ) =
	$this->separate_code( "{$code\n}" );

    # Now, if there is no explicit threadlooping in the code,
    # enclose everything into it.
    if(!$threadloops && !$dont_add_thrloop) {
	print "Adding threadloop...\n" if $::PP_VERBOSE;
	my $nc = $coderef;
	$coderef = $backcode
	  ? PDL::PP::BackCodeThreadLoop->new() : PDL::PP::ThreadLoop->new();
	push @{$coderef},$nc;
    }

    # repeat for the bad code, then stick good and bad into
    # a BadSwitch object which creates the necessary
    # 'if (bad) { badcode } else { goodcode }' code
    #
    # NOTE: amalgamate sizeprivs from good and bad code
    #
    if ( $handlebad ) {
	print "Processing 'bad' code...\n" if $::PP_VERBOSE;
	my ( $bad_threadloops, $bad_coderef, $bad_sizeprivs ) =
	    $this->separate_code( "{$badcode\n}" );

	if(!$bad_threadloops && !$dont_add_thrloop) {
	    print "Adding 'bad' threadloop...\n" if $::PP_VERBOSE;
	    my $nc = $bad_coderef;
	    if( !$backcode ){ # Normal readbackdata threadloop
		    $bad_coderef = PDL::PP::ThreadLoop->new();
	    }
	    else{  # writebackcode threadloop
		    $bad_coderef = PDL::PP::BackCodeThreadLoop->new();
	    }
	    push @{$bad_coderef},$nc;
	}

	my $good_coderef = $coderef;
	$coderef = PDL::PP::BadSwitch->new( $good_coderef, $bad_coderef );

	# amalgamate sizeprivs from Code/BadCode segments
	# (sizeprivs is a simple hash, with each element
	# containing a string - see PDL::PP::Loop)
	while ( my ( $bad_key, $bad_str ) = each %$bad_sizeprivs ) {
	    my $str = $$sizeprivs{$bad_key};
	    die "ERROR: sizeprivs problem in PP/PDLCode.pm (BadVal stuff)\n"
		if defined $str and $str ne $bad_str;
	    $$sizeprivs{$bad_key} = $bad_str;  # copy over
	}

    } # if: $handlebad

    print "SIZEPRIVSX: ",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;

    # Enclose it all in a genericloop.
    my $nc = $coderef;
    $coderef = PDL::PP::GenericLoop->new($generictypes, undef,
	  [grep {!$extrageneric->{$_}} @$parnames],'$PRIV(__datatype)');
    push @{$coderef},$nc;

    # Do we have extra generic loops?
    # If we do, first reverse the hash:
    my %glh;
    for(sort keys %$extrageneric) {
	push @{$glh{$extrageneric->{$_}}},$_;
    }
    my $no = 0;
    for(sort keys %glh) {
	my $nc = $coderef;
	$coderef = PDL::PP::GenericLoop->new($generictypes,$no++,
					    $glh{$_},$_);
	push @$coderef,$nc;
    }

    my $pobjs = $sig->objs;
    # Then, in this form, put it together what we want the code to actually do.
    print "SIZEPRIVS: ",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;
    $this->{Code} = (join '',sort values %$sizeprivs).
       ($dont_add_thrloop?'':PDL::PP::pp_line_numbers __LINE__, join "\n",
        'PDL_COMMENT("threadloop declarations")',
        'int __thrloopval;',
        'register PDL_Indx __tind0,__tind1; PDL_COMMENT("counters along dim")',
        'register PDL_Indx __tnpdls = $PRIV(pdlthread).npdls;',
        'PDL_COMMENT("dims here are how many steps along those dims")',
        (map "register PDL_Indx __tinc0_$_ = \$PRIV(pdlthread).incs[$_];", 0..$#$parnames),
        (map "register PDL_Indx __tinc1_$_ = \$PRIV(pdlthread).incs[__tnpdls+$_];", 0.. $#$parnames),
        $this->threadloop_start,
        $this->threadloop_end,
       ).
       $this->params_declare.
       join('',map $_->get_incregisters, @$pobjs{sort keys %$pobjs}).
       $coderef->get_str($this,[])
       ;
    $this->{Code};

} # new()

sub params_declare {
    my ($this) = @_;
    my ($ord,$pdls) = $this->get_pdls;
    my @decls = map $_->get_xsdatapdecl("PDL_PARAMTYPE_".$_->name),
      map $pdls->{$_}, @$ord;
    my @param_names = map "PDL_PARAMTYPE_$_", @$ord;
    PDL::PP::pp_line_numbers(__LINE__, <<EOF);
#define PDL_DECLARE_PARAMS_$this->{Name}(@{[join ',', @param_names]}) \\
  @{[join " \\\n", @decls]}
EOF
}

sub func_name { $_[1] ? "writebackdata" : "readdata" }
sub threadloop_start_name { "PDL_STARTTHREADLOOP_$_[0]{Name}" }
sub threadloop_end_name { "PDL_ENDTHREADLOOP_$_[0]->{Name}" }

sub threadloop_start {
    my ($this) = @_;
    my ($ord,$pdls) = $this->get_pdls;
    my $macro_name = $this->threadloop_start_name;
    PDL::PP::pp_line_numbers(__LINE__, <<EOF);
#define $macro_name(funcName) \\
__thrloopval = PDL->startthreadloop(&(\$PRIV(pdlthread)),\$PRIV(vtable)->funcName, __privtrans); \\
if ( __thrloopval < 0 ) die("Error starting threadloop"); \\
if ( __thrloopval ) return; \\
       do { \\
	    PDL_Indx *__tdims = PDL->get_threaddims(&\$PRIV(pdlthread)); \\
	    if (!__tdims) die("Error in get_threaddims"); \\
	    register PDL_Indx __tdims1 = __tdims[1]; \\
	    register PDL_Indx __tdims0 = __tdims[0]; \\
	    register PDL_Indx *__offsp = PDL->get_threadoffsp(&\$PRIV(pdlthread)); \\
	    if (!__offsp ) die("Error in get_threadoffsp"); \\
      PDL_COMMENT("incs are each pdl's stride, declared at func start") \\
      PDL_COMMENT("offs are each pthread's starting offset into each pdl") \\
@{[ join " \\\n", map $pdls->{$ord->[$_]}->do_pointeraccess." += __offsp[$_];", 0..$#$ord ]} \\
      for( __tind1 = 0 ; \\
	    __tind1 < __tdims1 ; \\
	    __tind1++ \\
	    PDL_COMMENT("step by tinc1, undoing inner-loop of tinc0*tdims0") \\
@{[ join " \\\n", map "\t\t,".$pdls->{$ord->[$_]}->do_pointeraccess." += __tinc1_$_ - __tinc0_$_ * __tdims0", 0..$#$ord ]} \\
	 ) \\
      { \\
	 for( __tind0 = 0 ; \\
	      __tind0 < __tdims0 ; \\
	      __tind0++ \\
@{[ join " \\\n", map "\t\t,".$pdls->{$ord->[$_]}->do_pointeraccess." += __tinc0_${_}", 0..$#{$ord} ]} \\
	   ) { \\
      PDL_COMMENT("This is the tightest threadloop. Make sure inside is optimal.")
EOF
}

sub threadloop_end {
    my ($this) = @_;
    my ($ord,$pdls) = $this->get_pdls();
    my $macro_name = $this->threadloop_end_name;
    PDL::PP::pp_line_numbers(__LINE__, <<EOF);
#define $macro_name \\
} \\
} \\
PDL_COMMENT("undo outer-loop of tinc1*tdims1, and original per-pthread offset") \\
@{[ join " \\\n", map $pdls->{$ord->[$_]}->do_pointeraccess." -= __tinc1_$_ * __tdims1 + __offsp[$_];", 0..$#$ord ]} \\
__thrloopval = PDL->iterthreadloop(&\$PRIV(pdlthread),2); \\
if ( __thrloopval < 0 ) die("Error in iterthreadloop"); \\
} while(__thrloopval);
EOF
}

sub sig {$_[0]->{Sig}}

# This sub determines the index name for this index.
# For example, a(x,y) and x0 becomes [x,x0]
sub make_loopind { my($this,$ind) = @_;
	my $orig = $ind;
	while(!$this->{IndObjs}{$ind}) {
		if(!((chop $ind) =~ /[0-9]/)) {
			confess("Index not found for $_ ($ind)!\n");
		}
		}
	return [$ind,$orig];
}

my %access2class = (
  GENERIC => 'PDL::PP::GentypeAccess',
  PPSYM => 'PDL::PP::PpsymAccess',
);

sub process {
    my ($this, $code, $stack_ref, $threadloops_ref, $sizeprivs) = @_;
    while($code) {
	# Parse next statement
	$code =~ s/^(.*?) # First, some noise is allowed. This may be bad.
	    ( \$(ISBAD|ISGOOD|SETBAD)\s*\(\s*\$?[a-zA-Z_]\w*\s*\([^)]*\)\s*\)   # $ISBAD($a(..)), ditto for ISGOOD and SETBAD
	        |\$[a-zA-Z_]\w*\s*\([^)]*\)  # $a(...): access
		|\bloop\s*\([^)]+\)\s*%\{   # loop(..) %{
		|\btypes\s*\([^)]+\)\s*%\{  # types(..) %{
		|\bthreadloop\s*%\{         # threadloop %{
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
	} elsif($control =~ /^threadloop\s*%\{/) {
	    my $ob = PDL::PP::ThreadLoop->new;
	    push @{$stack_ref->[-1]},$ob;
	    push @$stack_ref,$ob;
	    $$threadloops_ref++;
	} elsif($control =~ /^%}/) {
	    pop @$stack_ref;
	} else {
	    my ($rest, @add) = $this->expand($control.$code);
	    push @{$stack_ref->[-1]}, @add;
	    $code = $rest;
	}
    } # while: $code
}

# my ( $threadloops, $coderef, $sizeprivs ) = $this->separate_code( $code );
#
# separates the code into an array of C fragments (strings),
# variable references (strings starting with $) and
# loops (array references, 1. item = variable.
#
sub separate_code {
    my ( $this, $code ) = @_;
    # First check for standard code errors:
    catch_code_errors($code);
    my @stack = my $coderef = PDL::PP::Block->new;
    my $threadloops = 0;
    my $sizeprivs = {};
    $this->process($code, \@stack, \$threadloops, $sizeprivs);
    ( $threadloops, $coderef, $sizeprivs );
} # sub: separate_code()

sub expand {
    my ($this, $text) = @_;
    my (undef, $pdl, $inds, $rest) = PDL::PP::Rule::Substitute::macro_extract($text);
    my @add;
    if($pdl =~ /^T/) {@add = PDL::PP::MacroAccess->new($pdl,$inds,
			   $this->{Generictypes},$this->{Name});}
    elsif(my $c = $access2class{$pdl}) {@add = $c->new($pdl,$inds)}
    elsif($pdl =~ /^(ISBAD|ISGOOD|SETBAD)$/) {
	$inds =~ s/^\$?([a-zA-Z_]\w*)\s*//; # $ is optional
	@add = PDL::PP::BadAccess->new($pdl,$1,$inds,$this);
    }
    elsif($this->{ParObjs}{$pdl}) {@add = PDL::PP::Access->new($pdl,$inds)}
    else {
	@add = "\$$pdl(";
	# assumption: the only "control" that will happen in macro args is another macro
	$this->process($inds, [\@add], undef, undef);
	push @add, ")";
    }
    ($rest, @add);
}

# This is essentially a collection of regexes that look for standard code
# errors and croaks with an explanation if they are found.
sub catch_code_errors {
    my $code_string = shift;
    # Look for constructs like
    #   loop %{
    # which is invalid - you need to specify the dimension over which it
    # should loop
    report_error('Expected dimension name after "loop" and before "%{"', $1)
	    if $code_string =~ /(.*\bloop\s*%\{)/s;
}

# Report an error as precisely as possible. If they have #line directives
# in the code string, use that in the reporting; otherwise, use standard
# Carp mechanisms
my $line_re = qr/#\s*line\s+(\d+)\s+"([^"]*)"/;
sub report_error {
    my ($message, $code) = @_;
    # Just croak if they didn't supply a #line directive:
    confess($message) if $code !~ $line_re;
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

###########################
#
# Encapsulate a block

package PDL::PP::Block;

sub new { my($type) = @_; bless [],$type; }

sub myoffs { 0 }
sub myprelude {}
sub myitem { "" }
sub mypostlude {}

sub get_str {
    my ($this,$parent,$context) = @_;
    my $str = $this->myprelude($parent,$context);
    $str .= $this->get_str_int($parent,$context);
    $str .= $this->mypostlude($parent,$context);
    return $str;
}

sub get_str_int {
  my ( $this, $parent, $context ) = @_;
  my $nth=0;
  my $str = "";
  MYLOOP: while(1) {
    my $it = $this->myitem($parent,$nth);
    last MYLOOP if $nth and !$it;
    $str .= $it;
    $str .= join '', $this->get_contained($parent,$context);
    $nth++;
  }
  return $str;
} # get_str_int()

sub get_contained {
  my ($this, $parent, $context) = @_;
  map ref($_) ? $_->get_str($parent, $context) : $_,
    @$this[$this->myoffs..$#$this];
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
    my $bad  = $this->[1];
    my $str = PDL::PP::pp_line_numbers(__LINE__, <<EOF);
if ( \$PRIV(bvalflag) ) { PDL_COMMENT("** do 'bad' Code **")
#define PDL_BAD_CODE
  @{[ $bad->get_str($parent,$context) ]}
#undef PDL_BAD_CODE
} else { PDL_COMMENT("** else do 'good' Code **")
  @{[ $good->get_str($parent,$context) ]}
}
EOF
}

###########################
#
# Encapsulate a loop

package PDL::PP::Loop;
our @ISA = "PDL::PP::Block";

sub new { my($type,$args,$sizeprivs,$parent) = @_;
	my $this = bless [$args],$type;
	for(@{$this->[0]}) {
		print "SIZP $sizeprivs, $_\n" if $::PP_VERBOSE;
		my $i = $parent->make_loopind($_);
		my $i_size = $parent->sig->ind_obj($i->[0])->get_size;
		$sizeprivs->{$i->[0]} =
		  "register PDL_Indx __$i->[0]_size = $i_size;\n";
		print "SP :",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;
	}
	return $this;
}

sub myoffs { return 1; }
sub myprelude { my($this,$parent,$context) = @_;
	my $text = "";
	push @$context, map {
		my $i = $parent->make_loopind($_);
# Used to be $PRIV(.._size) but now we have it in a register.
		$text .= PDL::PP::pp_line_numbers(__LINE__, <<EOF);
{PDL_COMMENT(\"Open $_\") register PDL_Indx $_;
for($_=0; $_<(__$i->[0]_size); $_++) {
EOF
		$i;
	} @{$this->[0]};
	$text;
}
sub mypostlude { my($this,$parent,$context) = @_;
	splice @$context, - ($#{$this->[0]}+1);
	return join '', map PDL::PP::pp_line_numbers(__LINE__-1, "}} PDL_COMMENT(\"Close $_\")"), @{$this->[0]};
}

###########################
#
# Encapsulate a generic type loop
package PDL::PP::GenericLoop;
our @ISA = "PDL::PP::Block";

# make the typetable from info in PDL::Types
use PDL::Types ':All';
my @typetable = map [$_->ppsym, $_], types();
sub get_generictyperecs { my($types) = @_;
    my %wanted; @wanted{@$types} = ();
    [ map $_->[1], grep exists $wanted{$_->[0]}, @typetable ];
}

# Types: BSULFD
sub new {
    my ($type,$types,$name,$varnames,$whattype) = @_;
    my %vars; @vars{@$varnames} = ();
    bless [get_generictyperecs($types), $name, \%vars, $whattype], $type;
}

sub myoffs {4}

sub myprelude {
    my ($this,$parent,$context) = @_;
    push @{$parent->{Gencurtype}}, undef; # so that $GENERIC can get at it
    die "ERROR: need to rethink NaN support in GenericLoop\n"
	if defined $this->[1] and $parent->{ftypes_type};
    <<WARNING_EATER;
PDL_COMMENT("Start generic loop")
	switch($this->[3]) { case -42: PDL_COMMENT("Warning eater") {(void)1;
WARNING_EATER
}

sub myitem {
    my ($this,$parent,$nth) = @_;
    my $item = $this->[0]->[$nth] || return "";
    $parent->{Gencurtype}[-1] = $item;
    @$parent{qw(ftypes_type ftypes_vars)} = ($item, $this->[2]) if defined $this->[1];
    my ($ord,$pdls) = $parent->get_pdls;
    my @param_ctypes = map $pdls->{$_}->adjusted_type($item)->ctype, @$ord;
    my $decls = keys %{$this->[2]} == @$ord
      ? PDL::PP::pp_line_numbers(__LINE__-1, "\t\tPDL_DECLARE_PARAMS_$parent->{Name}(@{[join ',', @param_ctypes]})\n")
      : join '', map $_->get_xsdatapdecl($_->adjusted_type($item)->ctype),
          map $parent->{ParObjs}{$_}, sort keys %{$this->[2]};
    join '',
	PDL::PP::pp_line_numbers(__LINE__-1, "\t} break; case @{[$item->sym]}: {\n"),
	$decls;
}

sub mypostlude {
    my($this,$parent,$context) = @_;
    pop @{$parent->{Gencurtype}};  # and clean up the Gentype stack
    $parent->{ftypes_type} = undef if defined $this->[1];
    my $supported = join '', map $_->ppsym, @{$this->[0]};
    "\tbreak;}
	default:barf(\"PP INTERNAL ERROR in $parent->{Name}: unhandled datatype(%d), only handles ($supported)! PLEASE MAKE A BUG REPORT\\n\", $this->[3]);}\n";
}

####
#
# This relies on PP.pm making sure that initthreadstruct always sets
# up the two first dimensions even when they are not necessary.
#
package PDL::PP::ThreadLoop;
use Carp;
our @ISA = "PDL::PP::Block";
our @CARP_NOT;

sub new {
	my $type   = shift;
	bless [],$type;
}
sub myoffs { return 0; }
sub myprelude {
    my($this,$parent,$context, $backcode) = @_;
    $parent->threadloop_start_name.'('.$parent->func_name($backcode).')';
}

# Should possibly fold out thread.dims[0] and [1].
sub mypostlude {my($this,$parent,$context) = @_;
    $parent->threadloop_end_name;
}

# Simple subclass of ThreadLoop to implement writeback code
#
#
package PDL::PP::BackCodeThreadLoop;
use Carp;
our @ISA = "PDL::PP::ThreadLoop";
our @CARP_NOT;

sub myprelude {
    my($this,$parent,$context, $backcode) = @_;

    # Set backcode flag if not defined. This will make the parent
    #   myprelude emit proper writeback code
    $backcode = 1 unless defined($backcode);

    $this->SUPER::myprelude($parent, $context, $backcode);
}


###########################
#
# Encapsulate a types() switch
#
package PDL::PP::Types;
use Carp;
use PDL::Types ':All';
our @ISA = "PDL::PP::Block";
our @CARP_NOT;
my %types = map +($_=>1), ppdefs_all; # BSUL....

sub new {
    my($type,$ts,$parent) = @_;
    my @bad = grep !$types{$_}, my @ts = split '', $ts;
    confess "Invalid type access (@bad) in '$ts'!" if @bad;
    bless [+{map +($_=>1), @ts}],$type; }
sub myoffs { return 1; }

sub get_str {
  my ($this,$parent,$context) = @_;
  confess "types() outside a generic loop"
    unless defined(my $type = $parent->{Gencurtype}[-1]);
  return '' if !$this->[0]{$type->ppsym};
  join '', $this->get_contained($parent,$context);
}


###########################
#
# Encapsulate an access

package PDL::PP::Access;
use Carp;
our @CARP_NOT;

sub new { my($type,$pdl,$inds,$parent) = @_;
    bless [$pdl,$inds],$type;
}

sub get_str { my($this,$parent,$context) = @_;
    $parent->{ParObjs}{$this->[0]}->do_access($this->[1],$context)
	if defined($parent->{ParObjs}{$this->[0]});
}

###########################
#
# Encapsulate a check on whether a value is good or bad
# handles both checking (good/bad) and setting (bad)
#
# Integer types (BSUL) + floating point when no NaN (FD)
#   $ISBAD($a(n))  -> $a(n) == a_badval
#   $ISGOOD($a())     $a()  != a_badval
#   $SETBAD($a())     $a()   = a_badval
#
# floating point with NaN
#   $ISBAD($a(n))  -> !isnan($a(n)) == 0
#   $ISGOOD($a())     !isnan($a())  != 0
#   $SETBAD($a())     $a()           = PDL->bvals.Float (or .Double)
#
# I've also got it so that the $ on the pdl name is not
# necessary - so $ISBAD(a(n)) is also accepted, so as to reduce the
# amount of line noise. This is actually done by the regexp
# in the separate_code() sub at the end of the file.
#
# note:
#   we also expand out $a(n) etc as well here
#
# To do:
#   need to allow use of F,D without NaN
#

package PDL::PP::BadAccess;
use Carp;
our @CARP_NOT;

sub new {
    my ( $type, $opcode, $name, $inds, $parent ) = @_;
    $inds = substr $inds, 1, -1; # chop off brackets
    # trying to avoid auto creation of hash elements
    my $check = $parent->{ParObjs};
    die "\nIt looks like you have tried a \$${opcode}() macro on an\n" .
	"  unknown ndarray <$name($inds)>\n"
	unless exists($check->{$name}) and defined($check->{$name});

    return bless [$opcode, $name, $inds], $type;
}

our %ops = ( ISBAD => '==', ISGOOD => '!=', SETBAD => '=' );

sub get_str {
    my($this,$parent,$context) = @_;

    my $opcode = $this->[0];
    my $name   = $this->[1];
    my $inds   = $this->[2];

    print "PDL::PP::BadAccess sent [$opcode] [$name] [$inds]\n" if $::PP_VERBOSE;

    my $op = $ops{$opcode};
    die "ERROR: unknown check <$opcode> sent to PDL::PP::BadAccess\n"
	unless defined $op;

    my $obj = $parent->{ParObjs}{$name};
    die "ERROR: something screwy in PDL::PP::BadAccess (PP/PDLCode.pm)\n"
	unless defined( $obj );

    my ( $lhs, $rhs ) = ($obj->do_access($inds,$context), "${name}_badval");

    print "DBG:  [$lhs $op $rhs]\n" if $::PP_VERBOSE;
    return "$lhs $op $rhs";
}


###########################
#
# Encapsulate a macroaccess

package PDL::PP::MacroAccess;
use Carp;
use PDL::Types ':All';
my $types = join '',ppdefs_all;
our @CARP_NOT;

sub new {
    my ($type, $pdl, $inds, $gentypes, $name) = @_;
    $pdl =~ /^\s*T([A-Z]+)\s*$/
      or confess("Macroaccess wrong in $name (allowed types $types): was '$pdl'\n");
    my @ilst = split '', $1;
    my @lst = split ',', $inds, -1;
    confess "Macroaccess: different nos of args $pdl $inds\n" if @lst != @ilst;
    my %type2value; @type2value{@ilst} = @lst;
    warn "$name has no Macro for generic type $_ (has $pdl)\n"
	for grep !exists $type2value{$_}, @$gentypes;
    my %gts; @gts{@$gentypes} = ();
    warn "Macro for unsupported generic type identifier $_\n"
	for grep !exists $gts{$_}, @ilst;
    bless [\%type2value, $name], $type;
}

sub get_str {
    my ($this, $parent, $context) = @_;
    my ($type2value, $name) = @{$this};
    confess "generic type access outside a generic loop in $name"
      unless defined $parent->{Gencurtype}[-1];
    $type2value->{$parent->{Gencurtype}[-1]->ppsym};
}


###########################
#
# Encapsulate a GentypeAccess

package PDL::PP::GentypeAccess;
use Carp;
our @CARP_NOT;

sub new { my($type,$pdl,$inds) = @_; bless [$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
  confess "generic type access outside a generic loop"
    unless defined(my $type = $parent->{Gencurtype}[-1]);
  return $type->ctype if !$this->[0];
  my $pobj = $parent->{ParObjs}{$this->[0]} // confess "not a defined parname";
  $pobj->adjusted_type($type)->ctype;
}

package PDL::PP::PpsymAccess;
use Carp;
our @CARP_NOT;

sub new { my($type,$pdl,$inds) = @_; bless [$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
  confess "generic type access outside a generic loop"
    unless defined(my $type = $parent->{Gencurtype}[-1]);
  return $type->ppsym if !$this->[0];
  my $pobj = $parent->{ParObjs}{$this->[0]} // confess "not a defined parname";
  $pobj->adjusted_type($type)->ctype;
}

1;
