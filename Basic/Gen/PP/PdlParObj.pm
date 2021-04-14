package PDL::PP::PdlParObj;

use strict;
use warnings;
use Carp;
use PDL::Types ':All';
use PDL::Config;

my $usenan = $PDL::Config{BADVAL_USENAN} || 0;
our $macros = <<'EOF';
#define PDL_REDODIMS(declini, cast, type, flag, name, pdlname) \
  declini name ## _datap = (cast(PDL_REPRP_TRANS(pdlname, flag))); \
  declini name ## _physdatap = (cast(pdlname->data));

#define PDL_REDODIMS_BADVAL(declini, cast, type, flag, name, pdlname) \
  PDL_REDODIMS(declini, cast, type, flag, name, pdlname) \
  type name ## _badval = 0; \
  PDL_Anyval name ## _anyval_badval = PDL->get_pdl_badvalue(pdlname); \
  ANYVAL_TO_CTYPE(name ## _badval, type, name ## _anyval_badval);
EOF

# split regex $re separated arglist
# but ignore bracket-protected bits
# (i.e. text that is within matched brackets)
my $prebrackreg = qr/^([^\(\{\[]*)/;
sub splitprotected ($$) {
  my ($re,$txt) = @_;
  return () if !defined $txt || $txt =~ /^\s*$/;
  my ($got,$pre) = (1,'');
  my @chunks = ('');
  my $ct = 0; # infinite loop protection
  while ($got && $txt =~ /[({\[]/ && $ct++ < 1000) {
    # print "iteration $ct\n";
    ($got,$txt,$pre) =
      Text::Balanced::extract_bracketed($txt,'{}()[]',$prebrackreg);
    my @partialargs = split $re, $pre, -1;
    $chunks[-1] .= shift @partialargs if @partialargs;
    push @chunks, @partialargs;
    $chunks[-1] .= $got;
  }
  confess "possible infinite parse loop, splitting '$txt' "
			   if $ct >= 1000;
  my @partialargs = split $re, $txt, -1;
  $chunks[-1] .= shift @partialargs if @partialargs;
  push @chunks, @partialargs if @partialargs;
  # print STDERR "args found: $#chunks\n";
  # print STDERR "splitprotected $txt on $re: [",join('|',@chunks),"]\n";
  return @chunks;
}

# null != [0]
#  - in Core.

#{package PDL;
# sub isnull {
#   my $this = shift;
#   return ($this->getndims==1 && $this->getdim(0)==0) ? 1:0 }
#}

1;

#__DATA__

# need for $badflag is due to hacked get_xsdatapdecl() 
# - this should disappear when (if?) things are done sensibly
#
my $typeregex = join '|', map $_->ppforcetype, types;
my $complex_regex = join '|', qw(real complex);
our $pars_re = qr/^
	\s*((?:$complex_regex|$typeregex)\b[+]*|)\s*	# $1: first option
	(?:
	\[([^]]*)\]   	# $2: The initial [option] part
	)?\s*
	(\w+)          	# $3: The name
	\(([^)]*)\)  		# $4: The indices
/x;
sub new {
	my($type,$string,$number,$badflag) = @_;
	$badflag ||= 0;
	my $this = bless {Number => $number, BadFlag => $badflag},$type;
	# Parse the parameter string. Note that the regexes for this match were
	# originally defined here, but were moved to PDL::PP for FullDoc parsing.
	$string =~ $pars_re
		 or confess "Invalid pdl def $string (regex $pars_re)\n";
	my($opt1,$opt2,$name,$inds) = ($1,$2,$3,$4);
	map {$_ = '' unless defined($_)} ($opt1,$opt2,$inds); # shut up -w
	print "PDL: '$opt1', '$opt2', '$name', '$inds'\n"
		  if $::PP_VERBOSE;
# Set my internal variables
	$this->{Name} = $name;
	$this->{Flags} = [(split ',',$opt2),($opt1?$opt1:())];
	for(@{$this->{Flags}}) {
		/^io$/ and $this->{FlagW}=1 or
		/^nc$/ and $this->{FlagNCreat}=1 or
		/^o$/ and $this->{FlagOut}=1 and $this->{FlagCreat}=1 and $this->{FlagW}=1 or
		/^oca$/ and $this->{FlagOut}=1 and $this->{FlagCreat}=1 and $this->{FlagW}=1
			and $this->{FlagCreateAlways}=1 or
		/^t$/ and $this->{FlagTemp}=1 and $this->{FlagCreat}=1 and $this->{FlagW}=1 or
		/^phys$/ and $this->{FlagPhys} = 1 or
		/^real$/ and $this->{FlagReal} = 1 or
		/^complex$/ and $this->{FlagComplex} = 1 or
		/^((?:$typeregex)[+]*)$/ and $this->{Type} = $1 and $this->{FlagTyped} = 1 or
		confess("Invalid flag $_ given for $string\n");
	}
#	if($this->{FlagPhys}) {
#		# warn("Warning: physical flag not implemented yet");
#	}
	if ($this->{FlagTyped} && $this->{Type} =~ s/[+]$// ) {
	  $this->{FlagTplus} = 1;
	}
	$this->{Type} &&= PDL::Type->new($this->{Type});
	if($this->{FlagNCreat}) {
		delete $this->{FlagCreat};
		delete $this->{FlagCreateAlways};
	}
	my @inds = map{
		s/\s//g; 		# Remove spaces
		$_;
	} split ',', $inds;
	$this->{RawInds} = [@inds];
	return $this;
}

sub name {return (shift)->{Name}}

sub add_inds {
	my($this,$dimsobj) = @_;
	$this->{IndObjs} = [map {$dimsobj->get_indobj_make($_)}
		@{$this->{RawInds}}];
	my %indcount;
	$this->{IndCounts} = [
		map {
			0+($indcount{$_->name}++);
		} @{$this->{IndObjs}}
	];
	$this->{IndTotCounts} = [
		map {
			($indcount{$_->name});
		} @{$this->{IndObjs}}
	];
}


# do the dimension checking for perl level threading
# assumes that IndObjs have been created
sub perldimcheck {
  my ($this,$pdl) = @_;
  croak ("can't create ".$this->name) if $pdl->isnull &&
    !$this->{FlagCreat};
  return 1 if $pdl->isnull;
  my $rdims = @{$this->{RawInds}};
  croak ("not enough dimensions for ".$this->name)
    if ($pdl->threadids)[0] < $rdims;
  my @dims = $pdl->dims;
  my ($i,$ind) = (0,undef);
  for $ind (@{$this->{IndObjs}}) {
    $ind->add_value($dims[$i++]);
  }
  return 0; # not creating
}

sub finalcheck {
  my ($this,$pdl) = @_;
  return [] if $pdl->isnull;
  my @corr = ();
  my @dims = $pdl->dims;
  my ($i,$ind) = (0,undef);
  for $ind (@{$this->{IndObjs}}) {
    push @corr,[$i-1,$ind->{Value},$dims[$i-1]] if $dims[$i++] != $ind->{Value};
  }
  return [@corr];
}

# get index sizes for a parameter that has to be created
sub getcreatedims {
  my $this = shift;
  return map
    { croak "can't create: index size ".$_->name." not initialised"
	if !defined($_->{Value}) || $_->{Value} < 1;
      $_->{Value} } @{$this->{IndObjs}};
}

sub adjusted_type {
  my ($this, $generic) = @_;
  confess "adjusted_type given undefined generic type\n" if !defined $generic;
  return $generic->realversion if $this->{FlagReal};
  return $generic unless $this->{FlagTyped};
  return $this->{Type}->numval > $generic->numval
    ? $this->{Type} : $generic
    if $this->{FlagTplus};
  $this->{Type};
}

sub get_nname{ my($this) = @_;
	"(\$PRIV(pdls[$this->{Number}]))";
}

sub get_nnflag { my($this) = @_;
	"(\$PRIV(vtable->per_pdl_flags[$this->{Number}]))";
}


# XXX There might be weird backprop-of-changed stuff for [phys].
#
# Have changed code to assume that, if(!$this->{FlagCreat})
# then __creating[] will == 0
#  -- see make_redodims_thread() in ../PP.pm
#
sub get_xsnormdimchecks { 
    my($this) = @_;
    my $pdl   = $this->get_nname;
    my $iref  = $this->{IndObjs};
    my $ninds = 0+scalar(@$iref);

    my $str = PDL::PP::pp_line_numbers(__LINE__, "");
    $str .= "if(!__creating[$this->{Number}]) {\n" if $this->{FlagCreat};
    
    # Dimensional Promotion when number of dims is less than required:
    #   Previous warning message now commented out,
    #   which means we only need include the code if $ninds > 0
    #
    if ( $ninds > 0 ) {
	$str .= "   if(($pdl)->ndims < $ninds) {\n" .
	    join('', map { 
		my $size = $iref->[$_-1]->get_size();      
		"      if (($pdl)->ndims < $_ && $size <= 1) $size = 1;\n"
		} (1..$ninds)) 
# XXX why is this here, commented, and not removed? If re-inserted, be sure to use PDL_COMMENT
##		."      /* \$CROAK(\"Too few dimensions for argument \'$this->{Name}\'\\n\"); */\n"
		. "   }\n";
    }

    # Now, the real check.
    my $no = 0;
    for( @$iref ) {
	my $siz = $_->get_size();
	my $dim = "($pdl)->dims[$no]";
	my $ndims = "($pdl)->ndims";
	$str .= "   if($siz == -1 || ($ndims > $no && $siz == 1)) {\n" .
	        "      $siz = $dim;\n" .
		"   } else if($ndims > $no && $siz != $dim) {\n" .
		"      if($dim != 1) {\n" .
                "         \$CROAK(\"Wrong dimensions for parameter '@{[ $this->name ]}'\\n\");\n" .
		"      }\n   }\n";
	$no++;
    } 

    $str .= "PDL->make_physical(($pdl));\n" if $this->{FlagPhys};

    if ( $this->{FlagCreat} ) { 
	$str .= "} else {\n";
	
	# We are creating this pdl.
	$str .= " PDL_Indx dims[".($ninds+1)."]; PDL_COMMENT(\"Use ninds+1 to avoid smart (stupid) compilers\")";
	$str .= join "",
	(map {"dims[$_] = ".$iref->[$_]->get_size().";"} 0 .. $#$iref);
	my $istemp = $this->{FlagTemp} ? 1 : 0;
	$str .="\n PDL->thread_create_parameter(&\$PRIV(__pdlthread),$this->{Number},dims,$istemp);\n";
	$str .= "}";
    }
    return $str;
    
} # sub: get_xsnormdimchecks()

sub get_incname {
	my($this,$ind) = @_;
	if($this->{IndTotCounts}[$ind] > 1) {
	    "__inc_".$this->{Name}."_".($this->{IndObjs}[$ind]->name).$this->{IndCounts}[$ind];
	} else {
	    "__inc_".$this->{Name}."_".($this->{IndObjs}[$ind]->name);
	}
}

sub get_incdecls {
	my($this) = @_;
	if(scalar(@{$this->{IndObjs}}) == 0) {return "";}
	(join '',map {
		"PDL_Indx ".($this->get_incname($_)).";";
	} (0..$#{$this->{IndObjs}}) ) . ";"
}

sub get_incregisters {
	my($this) = @_;
	if(scalar(@{$this->{IndObjs}}) == 0) {return "";}
	(join '',map {
		"register PDL_Indx ".($this->get_incname($_))." = \$PRIV(".
			($this->get_incname($_)).");\n";
	} (0..$#{$this->{IndObjs}}) )
}

sub get_incdecl_copy {
	my($this,$fromsub,$tosub) = @_;
	PDL::PP::pp_line_numbers(__LINE__, join '',map {
		my $iname = $this->get_incname($_);
		&$fromsub($iname)."=".&$tosub($iname).";";
	} (0..$#{$this->{IndObjs}}))
}

sub get_incsets {
	my($this,$str) = @_;
	my $no=0;
	PDL::PP::pp_line_numbers(__LINE__, join '',map {
               "if($str->ndims <= $_ || $str->dims[$_] <= 1)
		  \$PRIV(".($this->get_incname($_)).") = 0; else
		 \$PRIV(".($this->get_incname($_)).
			") = ".($this->{FlagPhys}?
				   "$str->dimincs[$_];" :
				   "PDL_REPRINC($str,$_);");
	} (0..$#{$this->{IndObjs}}) )
}

# Print an access part.
sub do_access {
	my($this,$inds,$context) = @_;
	my $pdl = $this->{Name};
# Parse substitutions into hash
	my %subst = map
	 {/^\s*(\w+)\s*=>\s*(\S*)\s*$/ or confess "Invalid subst $_\n"; ($1,$2)}
	 	splitprotected ',',$inds;
# Generate the text
	my $text;
	$text = "(${pdl}_datap)"."[";
	$text .= join '+','0',map {
		$this->do_indterm($pdl,$_,\%subst,$context);
	} (0..$#{$this->{IndObjs}});
	$text .= "]";
# If not all substitutions made, the user probably made a spelling
# error. Barf.
	if(scalar(keys %subst) != 0) {
		confess("Substitutions left: ".(join ',',keys %subst)."\n");
	}
       $text;
}

sub has_dim {
	my($this,$ind) = @_;
	my $h = 0;
	for(@{$this->{IndObjs}}) {
		$h++ if $_->name eq $ind;
	}
	return $h;
}

sub do_resize {
	my($this,$ind,$size) = @_;
	my @c;my $index = 0;
	for(@{$this->{IndObjs}}) {
		push @c,$index if $_->name eq $ind; $index ++;
	}
	my $pdl = $this->get_nname;
	return PDL::PP::pp_line_numbers(__LINE__, (join '',map {"$pdl->dims[$_] = $size;\n"} @c).
		"PDL->resize_defaultincs($pdl);PDL->allocdata($pdl);".
		$this->get_xsdatapdecl(undef,1));
}

sub do_pdlaccess {
	my($this) = @_;
	PDL::PP::pp_line_numbers(__LINE__, '$PRIV(pdls['.$this->{Number}.'])');

}

sub do_pointeraccess {
	my($this) = @_;
	return $this->{Name}."_datap";
}

sub do_physpointeraccess {
	my($this) = @_;
	return $this->{Name}."_physdatap";
}

sub do_indterm { my($this,$pdl,$ind,$subst,$context) = @_;
# Get informed
	my $indname = $this->{IndObjs}[$ind]->name;
	my $indno = $this->{IndCounts}[$ind];
	my $indtot = $this->{IndTotCounts}[$ind];
# See if substitutions
	my $substname = ($indtot>1 ? $indname.$indno : $indname);
	my $incname = $indname.($indtot>1 ? $indno : "");
	my $index;
	if(defined $subst->{$substname}) {$index = delete $subst->{$substname};}
	else {
# No => get the one from the nearest context.
		for(reverse @$context) {
			if($_->[0] eq $indname) {$index = $_->[1]; last;}
		}
	}
	if(!defined $index) {confess "Access Index not found: $pdl, $ind, $indname
		On stack:".(join ' ',map {"($_->[0],$_->[1])"} @$context)."\n" ;}
#	return "\$PRIV(".($this->get_incname($ind))."*". $index .")";
# Now we have them in register variables -> no PRIV
       return "(".($this->get_incname($ind))."*".
               "PP_INDTERM(".$this->{IndObjs}[$ind]->get_size().", $index))";
}

# XXX hacked to create a variable containing the bad value for 
# this piddle. 
# This is a HACK (Doug Burke 07/08/00)
# XXX
#
sub get_xsdatapdecl { 
    my($this,$genlooptype,$asgnonly) = @_;
    my $ptype = $this->adjusted_type($genlooptype);
    my $type = $ptype->ctype;
    my $pdl = $this->get_nname;
    my $flag = $this->get_nnflag;
    my $name = $this->{Name};
    my $declini = ($asgnonly ? "" : "$type *");
    my $cast = ($type ? "($type *)" : "");
    my $macro = "PDL_REDODIMS";
    # assuming we always need this
    # - may not be true - eg if $asgnonly ??
    # - not needed for floating point types when using NaN as bad values
    $macro = "PDL_REDODIMS_BADVAL" if $this->{BadFlag} and $ptype and
	!($usenan * $ptype->usenan);
    PDL::PP::pp_line_numbers(__LINE__, "$macro($declini, $cast, $type, $flag, $name, $pdl)");
}

1;
