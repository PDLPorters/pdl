# This file provides a class that parses the Code -member
# of the PDL::PP code.
#
# This is what makes the nice loops go around etc.
use strict;

package PDL::PP::Code;
use Carp;

sub get_pdls {my($this) = @_; return ($this->{ParNames},$this->{ParObjs});}

# Do the appropriate substitutions in the code.
sub new { my($type,$code,$parnames,$parobjs,$indobjs,$generictypes,
	    $extrageneric,$havethreading, $dont_add_thrloop, $nogeneric_loop) = @_;

         # C++ style comments
         #
         # This regexp isn't perfect because it doesn't cope with
         # literal string constants.
         #
         $code =~ s,//.*?\n,,g;

	if($::PP_VERBOSE) {
		if($dont_add_thrloop) {
			print "DONT_ADD_THRLOOP!\n";
		}
		print "EXTRAGEN: $extrageneric ",%$extrageneric,"\n";
		print "ParNAMES: ",(join ',',@$parnames),"\n";
		print "GENTYPES: ", @$generictypes, "\n";
	}
	my $sizeprivs = {};
	my($this) = bless {
		IndObjs => $indobjs,
		ParNames => $parnames,
		ParObjs => $parobjs,
		Gencurtype => [], # stack to hold GenType in generic loops
	},$type;
	$_ = "{".
	   (join '',map {$_->get_incregisters();} (values %{$this->{ParObjs}})).
	    "$code}";
# First, separate the code into an array of C fragments (strings),
# variable references (strings starting with $) and
# loops (array references, 1. item = variable.
	my $coderef = new PDL::PP::Block;
	my @stack = ($coderef);
	my $control;
	my $threadloops = 0;
	while($_) {
# Parse next statement
		s/^(.*?) # First, some noise is allowed. This may be bad.
		   ( \$[a-zA-Z_]+\s*\([^)]*\)   # $a(...): access
		    |\bloop\s*\([^)]+\)\s*%{   # loop(..) %{
		    |\btypes\s*\([^)]+\)\s*%{  # types(..) %{
		    |\bthreadloop\s*%{        # threadloop %{
		    |%}                     # %}
		    |$)//xs
			or confess("Invalid program $_");
		$control = $2;
# Store the user code.
# Some day we shall parse everything.
		push @{$stack[-1]},$1;
# Then, our control.
		if($control) {
			if($control =~ /^loop\s*\(([^)]+)\)\s*%{/) {
				my $ob = new PDL::PP::Loop([split ',',$1],
					$sizeprivs,$this);
				print "SIZEPRIVSXX: $sizeprivs,",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;
				push @{$stack[-1]},$ob;
				push @stack,$ob;
			} elsif($control =~ /^types\s*\(([^)]+)\)\s*%{/) {
				my $ob = new PDL::PP::Types($1);
				push @{$stack[-1]},$ob;
				push @stack,$ob;
			} elsif($control =~ /^threadloop\s*%{/) {
				my $ob = new PDL::PP::ThreadLoop();
				push @{$stack[-1]},$ob;
				push @stack,$ob;
				$threadloops ++;
			} elsif($control =~ /^\$[a-zA-Z_]+\s*\([^)]*\)/) {
				push @{$stack[-1]},new PDL::PP::Access($control,$this);
			} elsif($control =~ /^%}/) {
				pop @stack;
			} else {
				confess("Invalid control: $control\n");
			}
		} else {
			print("No \$2!\n") if $::PP_VERBOSE;
		}
	}
	print "SIZEPRIVSX: ",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;
# Now, if there is no explicit threadlooping in the code,
# enclose everything into it.
	if(!$threadloops && !$dont_add_thrloop && $havethreading) {
		print "Adding threadloop...\n" if $::PP_VERBOSE;
		my $nc = $coderef;
		$coderef = new PDL::PP::ThreadLoop();
		push @{$coderef},$nc;
	}
# Enclose it all in a genericloop.
	unless ($nogeneric_loop) {
# XXX Make genericloop understand denied pointers;...
		my $nc = $coderef;
		$coderef = new PDL::PP::GenericLoop($generictypes,"",
			[grep {!$extrageneric->{$_}} @$parnames],'$PRIV(__datatype)');
		push @{$coderef},$nc;
	}
# Do we have extra generic loops?
# If we do, first reverse the hash:
	my %glh;
	for(keys %$extrageneric) {
		push @{$glh{$extrageneric->{$_}}},$_;
	}
	my $no = 0;
	for(keys %glh) {
		my $nc = $coderef;
		$coderef = new PDL::PP::GenericLoop($generictypes,$no++,
							$glh{$_},$_);
		push @$coderef,$nc;
	}
# Then, in this form, put it together what we want the code to actually do.
	print "SIZEPRIVS: ",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;
	$this->{Code} = "{".(join '',values %$sizeprivs).
	  $coderef->get_str($this,[])
	  ."}";
	$this->{Code};
}

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

sub myoffs { return 0; }
sub myprelude {}
sub myitem {return "";}
sub mypostlude {}
sub get_str {my ($this,$parent,$context) = @_;
   my $str = $this->myprelude($parent,$context);
   my $it; my $nth=0;
   MYLOOP: while(1) {
    $it = $this->myitem($parent,$nth);
    if($nth && !$it) {last MYLOOP}
    $str .= $it;
    $str .= (join '',map {ref $_ ? $_->get_str($parent,$context) : $_}
	@{$this}[$this->myoffs()..$#{$this}]);
    $nth ++;
   }
   $str .= $this->mypostlude($parent,$context);
   $str;
}

###########################
#
# Encapsulate a loop

package PDL::PP::Loop;
@PDL::PP::Loop::ISA = "PDL::PP::Block";

sub new { my($type,$args,$sizeprivs,$parent) = @_;
	my $this = bless [$args],$type;
	for(@{$this->[0]}) {
		print "SIZP $sizeprivs, $_\n" if $::PP_VERBOSE;
		my $i = $parent->make_loopind($_);
		$sizeprivs->{$i->[0]} =
		  "register int __$i->[0]_size = \$PRIV(__$i->[0]_size);\n";
		print "SP :",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;
	}
	return $this;
}

sub myoffs { return 1; }
sub myprelude { my($this,$parent,$context) = @_;
	my $text = ""; my $i;
	push @$context, map {
		$i = $parent->make_loopind($_);
# Used to be $PRIV(.._size) but now we have it in a register.
		$text .= "{/* Open $_ */ register int $_;
			for($_=0; $_<(__$i->[0]_size); $_++) {";
		$i;
	} @{$this->[0]};
	return $text;
}
sub mypostlude { my($this,$parent,$context) = @_;
	splice @$context, - ($#{$this->[0]}+1);
	return join '',map {"}} /* Close $_ */"} @{$this->[0]};
}

###########################
#
# Encapsulate a generic type loop

package PDL::PP::GenericLoop;
@PDL::PP::GenericLoop::ISA = "PDL::PP::Block";

# Types: BSULFD,
sub new { my($type,$types,$name,$varnames,$whattype) = @_;
	bless [(PDL::PP::get_generictyperecs($types)),$name,$varnames,
		$whattype],$type;
}

sub myoffs {4}


sub myprelude { my($this,$parent,$context) = @_;
	push @{$parent->{Gencurtype}},'PDL_undef'; # so that $GENERIC can get at it
	"/* Start generic loop */\n".
	(join '',map{
		"#undef THISIS$this->[1]_$_\n#define THISIS$this->[1]_$_(a)\n"
	}(qw/B S U L F D/)).
	"\tswitch($this->[3]) { case -42: /* Warning eater */ {1;\n";
}

sub myitem { my($this,$parent,$nth) = @_;
#	print "GENERICITEM\n";
	my $item = $this->[0]->[$nth];
	if(!$item) {return "";}
	$parent->{Gencurtype}->[-1] = $item->[1];
	"\t} break; case $item->[0]: {\n".
	(join '',map {
		"#undef THISIS$this->[1]_$_\n#define THISIS$this->[1]_$_(a)\n";
	}(qw/B S U L F D/)).
	"#undef THISIS$this->[1]_$item->[3]\n#define THISIS$this->[1]_$item->[3](a) a\n".
	(join '',map{
		# print "DAPAT: '$_'\n";
		$parent->{ParObjs}{$_}->get_xsdatapdecl($item->[1]);
	} (@{$this->[2]})) ;
}

sub mypostlude { my($this,$parent,$context) = @_;
	pop @{$parent->{Gencurtype}};  # and clean up the Gentype stack
	"\tbreak;}
	default:barf(\"PP INTERNAL ERROR! PLEASE MAKE A BUG REPORT\\n\");}\n";
}


###########################
#
# Encapsulate a threadloop.
# There are several different

package PDL::PP::ThreadLoop;
sub new {
	return PDL::PP::ComplexThreadLoop->new(@_);
}

package PDL::PP::SimpleThreadLoop;
use Carp;
@PDL::PP::SimpleThreadLoop::ISA = "PDL::PP::Block";

sub new { my($type) = @_; bless [],$type; }
sub myoffs { return 0; }
sub myprelude {my($this,$parent,$context) = @_;
 my $no;
 my ($ord,$pdls) = $parent->get_pdls();
'	/* THREADLOOPBEGIN */
 if(PDL->startthreadloop(&($PRIV(__pdlthread)),$PRIV(vtable)->readdata,
 	__privtrans))) return;
   do {
 '.(join '',map {"${_}_datap += \$PRIV(__pdlthread).offs[".(0+$no++)."];\n"}
 		@$ord).'
';
}

sub mypostlude {my($this,$parent,$context) = @_;
 my $no;
 my ($ord,$pdls) = $parent->get_pdls();
'	/* THREADLOOPEND */
 '.(join '',map {"${_}_datap -= \$PRIV(__pdlthread).offs[".(0+$no++)."];\n"}
 		@$ord).'
      } while(PDL->iterthreadloop(&$PRIV(__pdlthread),0));
 '
}

####
#
# This relies on PP.pm making sure that initthreadloop always sets
# up the two first dimensions even when they are not necessary.
#
package PDL::PP::ComplexThreadLoop;
use Carp;
@PDL::PP::ComplexThreadLoop::ISA = "PDL::PP::Block";


sub new { my($type) = @_; bless [],$type; }
sub myoffs { return 0; }
sub myprelude {my($this,$parent,$context) = @_;
 my $no; my $no2=-1; my $no3=-1;
 my ($ord,$pdls) = $parent->get_pdls();
'	/* THREADLOOPBEGIN */
 if(PDL->startthreadloop(&($PRIV(__pdlthread)),$PRIV(vtable)->readdata,
 	__tr)) return;
   do { register int __tind1=0,__tind2=0;
        register int __tnpdls = $PRIV(__pdlthread).npdls;
      register int __tdims1 = $PRIV(__pdlthread.dims[1]);
      register int __tdims0 = $PRIV(__pdlthread.dims[0]);
      register int *__offsp = PDL->get_threadoffsp(&$PRIV(__pdlthread));
 '.(join '',map {$no2++;
            "register int __tinc0_".($no2)." = \$PRIV(__pdlthread).incs[$no2];"}
	     @$ord).
   (join '',map {$no3++;
            "register int __tinc1_".($no3)." = \$PRIV(__pdlthread).incs[__tnpdls+$no3];"}
	     @$ord).
   (join '',map {"${_}_datap += __offsp[".(0+$no++)."];\n"}
 		@$ord).'
	for(__tind2=0; __tind2<__tdims1 ; __tind2++) {
	 for(__tind1=0; __tind1<__tdims0 ; __tind1++) {
	  /* This is the tightest threadloop. Make sure inside is optimal. */
';
}

# Should possibly fold out thread.dims[0] and [1].
sub mypostlude {my($this,$parent,$context) = @_;
 my $no; my $no0; my $no1; my $no2; my $no3; my $no4; my $no5;
 my ($ord,$pdls) = $parent->get_pdls();
'	/* THREADLOOPEND */
	 '.(join '',map {"${_}_datap += __tinc0_".(0+$no0++).";\n"}
 		@$ord).'
	 } '
	 .(join '',map {"${_}_datap += __tinc1_".(0+$no1++)."
	     			     - __tinc0_".(0+$no2++)." *
				       __tdims0;\n"}
 		@$ord).'
	} '.
     (join '',map {"${_}_datap -= __tinc1_".(0+$no3++)." *
     				  __tdims1;"}
 		@$ord).'
 '.(join '',map {"${_}_datap -= \$PRIV(__pdlthread).offs[".(0+$no++)."];\n"}
 		@$ord).'
      } while(PDL->iterthreadloop(&$PRIV(__pdlthread),2));
 '
}



###########################
#
# Encapsulate a types() switch

package PDL::PP::Types;
use Carp;
@PDL::PP::Types::ISA = "PDL::PP::Block";

sub new { my($type,$ts) = @_;
	$ts =~ /[BSULFD]+/ or confess "Invalid type access with '$ts'!";
	bless [$ts],$type; }
sub myoffs { return 1; }
sub myprelude {my($this,$parent,$context) = @_;
	"\n#if ". (join '||',map {"(THISIS_$_(1)+0)"} split '',$this->[0])."\n";
}

sub mypostlude {my($this,$parent,$context) = @_;
	"\n#endif\n"
}


###########################
#
# Encapsulate an access

package PDL::PP::Access;
use Carp;

sub new { my($type,$str,$parent) = @_;
	$str =~ /^\$([a-zA-Z_]+)\s*\(([^)]*)\)/ or
		confess ("Access wrong: '$str'\n");
	my($pdl,$inds) = ($1,$2);
	if($pdl =~ /^T/) {new PDL::PP::MacroAccess($pdl,$inds);}
	elsif($pdl =~ /^P$/) {new PDL::PP::PointerAccess($pdl,$inds);}
	elsif($pdl =~ /^PP$/) {new PDL::PP::PhysPointerAccess($pdl,$inds);}
        elsif($pdl =~ /^SIZE$/) {new PDL::PP::SizeAccess($pdl,$inds);}
        elsif($pdl =~ /^RESIZE$/) {new PDL::PP::ReSizeAccess($pdl,$inds);}
        elsif($pdl =~ /^GENERIC$/) {new PDL::PP::GentypeAccess($pdl,$inds);}
	elsif($pdl =~ /^PDL$/) {new PDL::PP::PdlAccess($pdl,$inds);}
	elsif(!defined $parent->{ParObjs}{$pdl}) {new PDL::PP::OtherAccess($pdl,$inds);}
	else {
		bless [$pdl,$inds],$type;
	}
}

sub get_str { my($this,$parent,$context) = @_;
#	print "AC: $this->[0]\n";
	$parent->{ParObjs}{$this->[0]}->do_access($this->[1],$context)
	 if defined($parent->{ParObjs}{$this->[0]});
}

###########################
#
# Just some other substituted thing.

package PDL::PP::OtherAccess;
sub new { my($type,$pdl,$inds) = @_; bless [$pdl,$inds],$type; }
sub get_str {my($this) = @_;return "\$$this->[0]($this->[1])"}


###########################
#
# Encapsulate a Pointeraccess

package PDL::PP::PointerAccess;
use Carp;

sub new { my($type,$pdl,$inds) = @_; bless [$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
	croak ("can't access undefined pdl ".$this->[0])
	  unless defined($parent->{ParObjs}{$this->[0]});
#	$parent->{ParObjs}{$this->[0]}->{FlagPaccess} = 1;
	$parent->{ParObjs}{$this->[0]}->{FlagPhys} = 1;
	$parent->{ParObjs}{$this->[0]}->do_pointeraccess();
}


###########################
#
# Encapsulate a PhysPointeraccess

package PDL::PP::PhysPointerAccess;
use Carp;

sub new { my($type,$pdl,$inds) = @_; bless [$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
	$parent->{ParObjs}{$this->[0]}->do_physpointeraccess()
	 if defined($parent->{ParObjs}{$this->[0]});
}

###########################
#
# Encapsulate a PDLaccess

package PDL::PP::PdlAccess;
use Carp;

sub new { my($type,$pdl,$inds) = @_; bless [$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
	croak ("can't access undefined pdl ".$this->[0])
	  unless defined($parent->{ParObjs}{$this->[0]});
	$parent->{ParObjs}{$this->[0]}->do_pdlaccess();
}

###########################
#
# Encapsulate a macroaccess

package PDL::PP::MacroAccess;
use Carp;

sub new { my($type,$pdl,$inds) = @_; bless [$pdl,$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
	my ($pdl,$inds) = @{$this};
	$pdl =~ /T([BSULFD]+)/ or confess("Macroaccess wrong: $pdl\n");
	my @lst = split ',',$inds;
	my @ilst = split '',$1;
	if($#lst != $#ilst) {confess("Macroaccess: different nos of args $pdl $inds\n");}
	croak "generic type access outside a generic loop"
	  unless defined $parent->{Gencurtype}->[-1];
	my $type = $parent->{Gencurtype}->[-1];
	croak "unknown Type" unless $type =~ /^PDL_([BSULFD])/;
	$type = $1;
	for (0..$#lst) {
	  return "$lst[$_]" if $ilst[$_] =~ /$type/;
	}
	# return join ' ',map {
	#	"THISIS_$ilst[$_]($lst[$_])"
	# } (0..$#lst) ;
}


###########################
#
# Encapsulate a SizeAccess

package PDL::PP::SizeAccess;
use Carp;

sub new { my($type,$pdl,$inds) = @_; bless [$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
	croak "can't get SIZE of undefined dimension $this->[0]"
	  unless defined($parent->{IndObjs}{$this->[0]});
	$parent->{IndObjs}{$this->[0]}->get_size();
}

###########################
#
# Encapsulate a ReSizeAccess

package PDL::PP::ReSizeAccess;
use Carp;

sub new { my($type,$pdl,$inds) = @_; bless [$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
	$this->[0] =~ /^([^,]+),([^,]+)$/ or
		croak "Can't interpret resize str $this->[0]";
	croak "can't RESIZE undefined dimension $1"
	  unless defined($parent->{IndObjs}{$1});

	my $s = $parent->{IndObjs}{$1}->get_size();

# XXX NOTE: All piddles must be output piddles, there must not be
# a loop over this var (at all!) etc. Should check for these,
# this is why not yet documented.
# FURTHER NOTE: RESIZE DOESN'T COPY DATA PROPERLY!

	my($ord,$pdls) = $parent->get_pdls();
	my @p;

	for(@$ord) {
		push @p, $_
			if $pdls->{$_}->has_dim($1);
	}
	print "RESIZEACC: $1 $2, (",(join ',',@p),")\n";
	warn "RESIZE USED: DO YOU KNOW WHAT YOU ARE DOING???\n";

	return "$s = $2; ".(join '',map {$pdls->{$_}->do_resize($1,$2)} @p);
}


###########################
#
# Encapsulate a GentypeAccess

package PDL::PP::GentypeAccess;
use Carp;

sub new { my($type,$pdl,$inds) = @_; bless [$inds],$type; }

sub get_str {my($this,$parent,$context) = @_;
	     croak "generic type access outside a generic loop"
	       unless defined $parent->{Gencurtype}->[-1];
	     my $type = $parent->{Gencurtype}->[-1];
	     if ($this->[0]) {
	       croak "not a defined name"
		 unless defined($parent->{ParObjs}{$this->[0]});
	       $type = $parent->{ParObjs}{$this->[0]}->ctype($type);
	     }
	     return $type;
}

########################
#
# Type coercion
#
# Now, if TYPES:F given and double arguments, will coerce.

package PDL::PP::TypeConv;

sub print_xscoerce { my($this) = @_;
	$this->printxs("\t__priv->datatype=PDL_B;\n");
# First, go through all the types, selecting the most general.
	for(@{$this->{PdlOrder}}) {
		$this->printxs($this->{Pdls}{$_}->get_xsdatatypetest());
	}
# See which types we are allowed to use.
	$this->printxs("\tif(0) {}\n");
	for(@{$this->get_generictypes()}) {
		$this->printxs("\telse if(__priv->datatype <= $_->[2]) __priv->datatype = $_->[2];\n");
	}
	$this->{Types} =~ /F/ and (
		$this->printxs("\telse if(__priv->datatype == PDL_D) {__priv->datatype = PDL_F; /* Cast double to float */}\n"));
	$this->printxs("\telse {croak(\"Too high type %d given!\\n\",__priv->datatype);}");
# Then, coerce everything to this type.
	for(@{$this->{PdlOrder}}) {
		$this->printxs($this->{Pdls}{$_}->get_xscoerce());
	}
}
# XXX Should use PDL::Core::Dev;

no strict 'vars';

# STATIC!
sub PDL::PP::get_generictyperecs { my($types) = @_;
	my $foo;
	return [map {$foo = $_;
		( grep {/$foo->[0]/} (@$types) ) ?
		  ["PDL_".($_->[0]eq"U"?"US":$_->[0]),$_->[1],$_->[2],$_->[0]]
		  : ()
	}
	       (["B","PDL_Byte",$PDL_B],
		["S","PDL_Short",$PDL_S],
		["U","PDL_Ushort",$PDL_US],
		["L","PDL_Long",$PDL_L],
		["F","PDL_Float",$PDL_F],
		["D","PDL_Double",$PDL_D])];
}

sub xxx_get_generictypes { my($this) = @_;
	return [map {
		$this->{Types} =~ /$_->[0]/ ? ["PDL_".($_->[0]eq"U"?"US":$_->[0]),$_->[1],$_->[2],$_->[0]] : ()
	}
	       (["B","PDL_Byte",$PDL_B],
		["S","PDL_Short",$PDL_S],
		["U","PDL_Ushort",$PDL_US],
		["L","PDL_Long",$PDL_L],
		["F","PDL_Float",$PDL_F],
		["D","PDL_Double",$PDL_D])];
}
