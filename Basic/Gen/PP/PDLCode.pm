# This file provides a class that parses the Code -member
# of the PDL::PP code.
#
# This is what makes the nice loops go around etc.
#

package PDL::PP::Code;
use Carp;

use strict;

# check for bad value support
#
use PDL::Config;
#use vars qw ( $bvalflag $usenan );
my $bvalflag = $PDL::Config{WITH_BADVAL} || 0;
my $usenan   = $PDL::Config{BADVAL_USENAN} || 0;

sub get_pdls {my($this) = @_; return ($this->{ParNames},$this->{ParObjs});}

# we define the method separate_code() at the end of this
# file, so that it can call the constructors from the classes
# defined in this file. ugly...

# Do the appropriate substitutions in the code.
sub new { 
    my($type,$code,$badcode,$parnames,$parobjs,$indobjs,$generictypes,
       $extrageneric,$havethreading,$name,
       $dont_add_thrloop, $nogeneric_loop) = @_;

    # simple way of handling bad code check
    $badcode = undef unless $bvalflag;
    my $handlebad = defined($badcode);

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
	print "HandleBad: $handlebad\n";
    }
    my $this = bless {
	IndObjs => $indobjs,
	ParNames => $parnames,
	ParObjs => $parobjs,
	Gencurtype => [], # stack to hold GenType in generic loops
	types => 0,  # hack for PDL::PP::Types/GenericLoop
	pars => {},  # hack for PDL::PP::NaNSupport/GenericLoop
        Generictypes => $generictypes,   # so that MacroAccess can check it
        Name => $name,
    }, $type;
    
    my $inccode = join '',map {$_->get_incregisters();} (values %{$this->{ParObjs}});

    # First, separate the code into an array of C fragments (strings),
    # variable references (strings starting with $) and
    # loops (array references, 1. item = variable.
    #
    my ( $threadloops, $coderef, $sizeprivs ) = 
	$this->separate_code( "{$inccode\n$code\n}" );

    # Now, if there is no explicit threadlooping in the code,
    # enclose everything into it.
    if(!$threadloops && !$dont_add_thrloop && $havethreading) {
	print "Adding threadloop...\n" if $::PP_VERBOSE;
	my $nc = $coderef;
	$coderef = new PDL::PP::ThreadLoop();
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
	    $this->separate_code( "{$inccode\n$badcode\n}" );

	if(!$bad_threadloops && !$dont_add_thrloop && $havethreading) {
	    print "Adding 'bad' threadloop...\n" if $::PP_VERBOSE;
	    my $nc = $bad_coderef;
	    $bad_coderef = new PDL::PP::ThreadLoop();
	    push @{$bad_coderef},$nc;
	}

	my $good_coderef = $coderef;
	$coderef = new PDL::PP::BadSwitch( $good_coderef, $bad_coderef );

	# amalgamate sizeprivs from Code/BadCode segments
	# (sizeprivs is a simple hash, with each element 
	# containing a string - see PDL::PP::Loop)
	while ( my ( $bad_key, $bad_str ) = each %$bad_sizeprivs ) {
	    my $str = $$sizeprivs{$bad_key}; 
	    if ( defined $str ) {
		die "ERROR: sizeprivs problem in PP/PDLCode.pm (BadVal stuff)\n"
		    unless $str eq $bad_str;
	    }
	    $$sizeprivs{$bad_key} = $bad_str;  # copy over
	}

    } # if: $handlebad

    print "SIZEPRIVSX: ",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;

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

} # new()

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
      $str .= (join '',map {ref $_ ? $_->get_str($parent,$context) : $_}
	       @{$this}[$this->myoffs()..$#{$this}]);
      $nth++;
  }
    return $str;
} # get_str_int()

###########################
#
# Deal with bad code
# - ie create something like
#   if ( badflag ) { badcode } else { goodcode }
#
package PDL::PP::BadSwitch;
@PDL::PP::BadSwitch::ISA = "PDL::PP::Block";

sub new { 
    my($type,$good,$bad) = @_;
    return bless [$good,$bad], $type;
}

sub get_str {
    my ($this,$parent,$context) = @_;

    my $good = $this->[0];
    my $bad  = $this->[1];

    my $str = "if ( \$PRIV(bvalflag) ) { /*** do 'bad' Code ***/\n";
    $str .= $bad->get_str($parent,$context);
    $str .= "} else { /*** else do 'good' Code ***/\n";
    $str .= $good->get_str($parent,$context);
    $str .= "}\n";

    return $str;
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
#
# we use the value of $parent->{types} [set by a PDL::PP::Types object]
# to determine whether to define/undefine the THISISxxx macros
# (makes the xs code easier to read)
#
package PDL::PP::GenericLoop;
@PDL::PP::GenericLoop::ISA = "PDL::PP::Block";

# Types: BSULFD
use PDL::Types ':All';
sub new { 
    my($type,$types,$name,$varnames,$whattype) = @_;
    bless [(PDL::PP::get_generictyperecs($types)),$name,$varnames,
	   $whattype],$type;
}

sub myoffs {4}

sub myprelude { 
    my($this,$parent,$context) = @_;
    push @{$parent->{Gencurtype}},'PDL_undef'; # so that $GENERIC can get at it

    # horrible hack for PDL::PP::NaNSupport
    if ( $this->[1] ne "" ) {
	my ( @test ) = keys %{$parent->{pars}};
	die "ERROR: need to rethink NaNSupport in GenericLoop\n"
	    if $#test != -1;
	$parent->{pars} = {};
    }

    my $thisis_loop = '';
    if ( $parent->{types} ) {
	$thisis_loop = join '',
	map { 
	    "#undef THISIS$this->[1]_$_\n#define THISIS$this->[1]_$_(a)\n" 
	    }
	(ppdefs);
    }

    return "/* Start generic loop */\n$thisis_loop\n" .
	"\tswitch($this->[3]) { case -42: /* Warning eater */ {1;\n";
}

sub myitem { 
    my($this,$parent,$nth) = @_;
#	print "GENERICITEM\n";
    my $item = $this->[0]->[$nth];
    if(!$item) {return "";}
    $parent->{Gencurtype}->[-1] = $item->[1];

    # horrible hack for PDL::PP::NaNSupport
    if ( $this->[1] ne "" ) {
	foreach my $parname ( @{$this->[2]} ) {
	    $parent->{pars}{$parname} = $item->[1];
	}
    }

    my $thisis_loop = '';
    if ( $parent->{types} ) {
	$thisis_loop = (
			join '',
			map {
			    "#undef THISIS$this->[1]_$_\n#define THISIS$this->[1]_$_(a)\n";
			}
			(ppdefs) 
			) . 
			    "#undef THISIS$this->[1]_$item->[3]\n" . 
				"#define THISIS$this->[1]_$item->[3](a) a\n";
    }
	
    return "\t} break; case $item->[0]: {\n".
	$thisis_loop . 
	    (join '',map{
		# print "DAPAT: '$_'\n";
		$parent->{ParObjs}{$_}->get_xsdatapdecl($item->[1]);
	    } (@{$this->[2]})) ;
}

sub mypostlude { 
    my($this,$parent,$context) = @_;
    pop @{$parent->{Gencurtype}};  # and clean up the Gentype stack

    # horrible hack for PDL::PP::NaNSupport
    if ( $this->[1] ne "" ) { $parent->{pars} = {}; }
    
    return "\tbreak;}
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
sub myprelude {
    my($this,$parent,$context) = @_;
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
#
# horrible hack:
#  set $parent->{types} if we create this object so that
#  PDL::PP::GenericLoop knows to define the THISIS ... macros
#
package PDL::PP::Types;
use Carp;
use PDL::Types ':All';
@PDL::PP::Types::ISA = "PDL::PP::Block";

sub new { 
    my($type,$ts,$parent) = @_;
    my $types = join '', ppdefs; # BSUL....
    $ts =~ /[$types]+/ or confess "Invalid type access with '$ts'!";
    $parent->{types} = 1; # hack for PDL::PP::GenericLoop
    bless [$ts],$type; }
sub myoffs { return 1; }
sub myprelude {
    my($this,$parent,$context) = @_;
    return "\n#if ". (join '||',map {"(THISIS_$_(1)+0)"} split '',$this->[0])."\n";
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
	$str =~ /^\$([a-zA-Z_]\w*)\s*\(([^)]*)\)/ or
		confess ("Access wrong: '$str'\n");
	my($pdl,$inds) = ($1,$2);
	if($pdl =~ /^T/) {new PDL::PP::MacroAccess($pdl,$inds,
						   $parent->{Generictypes},$parent->{Name});}
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
# used by BadAccess code to know when to use NaN support
# - the output depends on the value of the
#   BADVAL_USENAN option in perldl.conf
#   == 1 then we use NaN's
#      0             PDL.bvals.Float/Double
#
# note the *horrible hack* for piddles whose type have been 
# specified using the FType option - see GenericLoop. 
# There MUST be a better way than this...
#
package PDL::PP::NaNSupport;
use PDL::Types ':All'; # typefld et al.

# need to be lower-case because of FlagTyped stuff
#
# need to be able to handle signatures with fixed types
# which means parameters like 'int mask()',
# which means the hack to add 'int' to %use_nan
#
my %use_nan =
    map {(typefld($_,'convertfunc') => typefld($_,'usenan')*$usenan)} typesrtkeys;
$use_nan{int} = 0;

# original try
##my %use_nan =
##  map {(typefld($_,'convertfunc') => typefld($_,'usenan')*$usenan)} typesrtkeys;

# Was the following, before new Type "interface"
#     ( byte => 0, short => 0, ushort => 0, long => 0,
#       int => 0,  longlong => 0, # necessary for fixed-type piddles (or something)
#       float => $usenan, 
#       double => $usenan 
#       );

my %set_nan = 
    (
     float  => 'PDL->bvals.Float',  PDL_Float  => 'PDL->bvals.Float',
     double => 'PDL->bvals.Double', PDL_Double => 'PDL->bvals.Double',
     );

sub use_nan ($) {
    my $type = shift;

    $type =~ s/^PDL_//;
    $type = lc $type;
    die "ERROR: Unknown type [$type] used in a 'Bad' macro."
	unless exists $use_nan{$type};
    return $use_nan{$type};
}

sub convert ($$$$$) {
    my ( $parent, $name, $lhs, $rhs, $opcode ) = @_;

    my $type = $parent->{Gencurtype}[-1];
    die "ERROR: unable to find type info for $opcode access"
	unless defined $type;

    # note: gentype may not be sensible because the
    # actual piddle could have a 'fixed' type
    die "ERROR: unable to find piddle $name in parent!"
	unless exists $parent->{ParObjs}{$name};
    my $pobj = $parent->{ParObjs}{$name};

    # based on code from from PdlParObj::ctype()
    # - want to handle FlagTplus case
    # - may not be correct
    # - extended to include hack to GenericLoop 
    #
    if ( exists $parent->{pars}{$name} ) {
	$type = $parent->{pars}{$name};
	print "#DBG: hacked <$name> to type <$type>\n" if $::PP_VERBOSE;
    } elsif ( exists $pobj->{FlagTyped} and $pobj->{FlagTyped} ) {
	$type = $pobj->{Type};

	# this should use Dev.pm - fortunately only worried about double/float here
	# XXX - do I really know what I'm doing ?
	if ( $pobj->{FlagTplus} ) {
	    my $gtype = $parent->{Gencurtype}[-1];
	    if ( $gtype eq "PDL_Double" ) {
		$type = $gtype if $type ne "double";
	    } elsif ( $gtype eq "PDL_Float" ) {
		$type = $gtype if $type !~ /^(float|double)$/;  # note: ignore doubles
	    }
	}
    }
	
    if ( use_nan($type) ) {
	if ( $opcode eq "SETBAD" ) {
#	    $rhs = "(0.0/0.0)";
	    $rhs = $set_nan{$type};
	} else {
	    $rhs = "0";
	    $lhs = "finite($lhs)";
	}
    }
    
    return ( $lhs, $rhs );
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
#   $ISBAD($a(n))  -> finite($a(n)) == 0
#   $ISGOOD($a())     finite($a())  != 0
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

sub new { 
    my ( $type, $opcode, $pdl_name, $inds, $parent ) = @_;

    # trying to avoid auto creation of hash elements
    my $check = $parent->{ParObjs};
    die "\nIt looks like you have tried a \$${opcode}() macro on an\n" .
	"  unknown piddle <$pdl_name($inds)>\n"
	unless exists($check->{$pdl_name}) and defined($check->{$pdl_name});

    return bless [$opcode, $pdl_name, $inds], $type;
}

my %ops = ( ISBAD => '==', ISGOOD => '!=', SETBAD => '=' );

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

    my $lhs = $obj->do_access($inds,$context);
    my $rhs = "${name}_badval";

    ( $lhs, $rhs ) = 
      PDL::PP::NaNSupport::convert( $parent, $name, $lhs, $rhs, $opcode );

    print "DBG:  [$lhs $op $rhs]\n" if $::PP_VERBOSE;
    return "$lhs $op $rhs";
}


###########################
#
# Encapsulate a check on whether a value is good or bad
# handles both checking (good/bad) and setting (bad)
#
# Integer types (BSUL) + floating point when no NaN (FD)
#   $ISBADVAR(foo,a)  -> foo == a_badval
#   $ISGOODVAR(foo,a)    foo != a_badval
#   $SETBADVAR(foo,a)    foo  = a_badval
#
# floating point with NaN
#   $ISBADVAR(foo,a)  -> finite(foo) == 0
#   $ISGOODVAR(foo,a)    finite(foo) != 0
#   $SETBADVAR(foo,a)    foo          = PDL->bvals.Float (or .Double)
#

package PDL::PP::BadVarAccess;
use Carp;

sub new { 
    my ( $type, $opcode, $var_name, $pdl_name, $parent ) = @_;

    # trying to avoid auto creation of hash elements
    my $check = $parent->{ParObjs};
    die "\nIt looks like you have tried a \$${opcode}() macro on an\n" .
	"  unknown piddle <$pdl_name>\n"
	unless exists($check->{$pdl_name}) and defined($check->{$pdl_name});

    bless [$opcode, $var_name, $pdl_name], $type;
}

my %ops = ( ISBAD => '==', ISGOOD => '!=', SETBAD => '=' );

sub get_str {
    my($this,$parent,$context) = @_;

    my $opcode   = $this->[0];
    my $var_name = $this->[1];
    my $pdl_name = $this->[2];

    print "PDL::PP::BadVarAccess sent [$opcode] [$var_name] [$pdl_name]\n" if $::PP_VERBOSE;

    my $op = $ops{$opcode};
    die "ERROR: unknown check <$opcode> sent to PDL::PP::BadVarAccess\n"
	unless defined $op;

    my $obj = $parent->{ParObjs}{$pdl_name};
    die "ERROR: something screwy in PDL::PP::BadVarAccess (PP/PDLCode.pm)\n"
	unless defined( $obj );

    my $lhs = $var_name;
    my $rhs = "${pdl_name}_badval";

    ( $lhs, $rhs ) = 
      PDL::PP::NaNSupport::convert( $parent, $pdl_name, $lhs, $rhs, $opcode );

    print "DBG:  [$lhs $op $rhs]\n" if $::PP_VERBOSE;
    return "$lhs $op $rhs";
}


###########################
#
# Encapsulate a check on whether a value is good or bad using PP
# handles both checking (good/bad) and setting (bad)

# this is only an initial attempt - it will, almost certainly,
# need more work as more code is converted to handle bad values
#
# currently it can only handle cases like
#  $PPISBAD(PARENT,[i])  -> PARENT_physdatap[i] == PARENT_badval
#  etc
#
# if we use NaN's, then
#  $PPISBAD(PARENT,[i])   -> finite(PARENT_physdatap[i]) == 0
#  $PPISGOOD(PARENT,[i])  -> finite(PARENT_physdatap[i]) != 0
#  $PPSETBAD(PARENT,[i])  -> PARENT_physdatap[i]          = PDL->bvals.Float (or .Double)
#

package PDL::PP::PPBadAccess; 
use Carp;

sub new { 
    my ( $type, $opcode, $pdl_name, $inds, $parent ) = @_;

    $opcode =~ s/^PP//;
    bless [$opcode, $pdl_name, $inds], $type;
}

# PP is stripped in new()
my %ops = ( ISBAD => '==', ISGOOD => '!=', SETBAD => '=' );

sub get_str {
    my($this,$parent,$context) = @_;

    my $opcode = $this->[0];
    my $name   = $this->[1];
    my $inds   = $this->[2];

    print "PDL::PP::PPBadAccess sent [$opcode] [$name] [$inds]\n" if $::PP_VERBOSE;

    my $op = $ops{$opcode};
    die "\nERROR: unknown check <$opcode> sent to PDL::PP::PPBadAccess\n"
	unless defined $op;

    my $obj = $parent->{ParObjs}{$name};
    die "\nERROR: ParObjs does not seem to exist for <$name> = problem in PDL::PP::PPBadAccess\n"
	unless defined $obj;

    my $lhs = $obj->do_physpointeraccess() . "$inds";
    my $rhs = "${name}_badval";

    ( $lhs, $rhs ) = 
      PDL::PP::NaNSupport::convert( $parent, $name, $lhs, $rhs, $opcode );

    print "DBG:  [$lhs $op $rhs]\n" if $::PP_VERBOSE;
    return "$lhs $op $rhs";
}


###########################
#
# Encapsulate a check on whether the state flag of a piddle
# is set/change this state
#
# $PDLSTATEISBAD(a)    ->  ($PDL(a)->state & PDL_BADVAL) > 0
# $PDLSTATEISGOOD(a)   ->  ($PDL(a)->state & PDL_BADVAL) == 0
# 
# $PDLSTATESETBAD(a)   ->  ($PDL(a)->state |= PDL_BADVAL)
# $PDLSTATESETGOOD(a)  ->  ($PDL(a)->state &= ~PDL_BADVAL)
# 

package PDL::PP::PDLStateBadAccess;
use Carp;

sub new { 
    my ( $type, $op, $val, $pdl_name, $parent ) = @_;

    # $op  is one of: IS SET
    # $val is one of: GOOD BAD

    # trying to avoid auto creation of hash elements
    my $check = $parent->{ParObjs};
    die "\nIt looks like you have tried a \$PDLSTATE${op}${val}() macro on an\n" .
	"  unknown piddle <$pdl_name>\n"
	unless exists($check->{$pdl_name}) and defined($check->{$pdl_name});

    bless [$op, $val, $pdl_name], $type;
}

my %ops  = ( 
	     IS  => { GOOD => '== 0', BAD => '> 0' },
	     SET => { GOOD => '&= ~', BAD => '|= ' },
	     );

sub get_str {
    my($this,$parent,$context) = @_;

    my $op   = $this->[0];
    my $val  = $this->[1];
    my $name = $this->[2];

    print "PDL::PP::PDLStateBadAccess sent [$op] [$val] [$name]\n" if $::PP_VERBOSE;

    my $opcode = $ops{$op}{$val};
    my $type = $op . $val;
    die "ERROR: unknown check <$type> sent to PDL::PP::PDLStateBadAccess\n"
	unless defined $opcode;

    my $obj = $parent->{ParObjs}{$name};
    die "\nERROR: ParObjs does not seem to exist for <$name> = problem in PDL::PP::PDLStateBadAccess\n"
	unless defined $obj;

    my $state = $obj->do_pdlaccess() . "->state";

    my $str;
    if ( $op eq 'IS' ) {
	$str = "($state & PDL_BADVAL) $opcode";
    } elsif ( $op eq 'SET' ) {
	$str = "$state ${opcode}PDL_BADVAL";
    }

    print "DBG:  [$str]\n" if $::PP_VERBOSE;
    return $str;
}


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
use PDL::Types ':All';
my $types = join '',ppdefs;

sub new { my($type,$pdl,$inds,$gentypes,$name) = @_; 
	  $pdl =~ /^\s*T([A-Z]+)\s*$/ or confess("Macroaccess wrong: $pdl\n");
	  my @ilst = split '',$1;
	  for my $gt (@$gentypes) {
	    warn "$name has no Macro for generic type $gt (has $pdl)\n"
	      unless grep {$gt eq $_} @ilst }
	  for my $mtype (@ilst) {
	    warn "Macro for unsupported generic type identifier $mtype".
	      " (probably harmless)\n"
	      unless grep {$mtype eq $_} @$gentypes;
	  }
	  return bless [$pdl,$inds,$name],
	    $type; }

sub get_str {my($this,$parent,$context) = @_;
	my ($pdl,$inds,$name) = @{$this};
	$pdl =~ /^\s*T([A-Z]+)\s*$/ 
	  or confess("Macroaccess wrong in $name (allowed types $types): was '$pdl'\n");
	my @lst = split ',',$inds;
	my @ilst = split '',$1;
	if($#lst != $#ilst) {confess("Macroaccess: different nos of args $pdl $inds\n");}
	croak "generic type access outside a generic loop in $name"
	  unless defined $parent->{Gencurtype}->[-1];
	my $type = mapfld $parent->{Gencurtype}->[-1], 'ctype' => 'ppsym';
	#     print "Type access: $type\n";
	croak "unknown Type in $name (generic type currently $parent->{Gencurtype}->[-1]"
	  unless defined $type;
	for (0..$#lst) {
	  return "$lst[$_]" if $ilst[$_] =~ /$type/;
	}
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

# make the typetable from info in PDL::Types
use PDL::Types ':All';
my @typetable = map {[$typehash{$_}->{ppsym},
		  $typehash{$_}->{ctype},
		  $typehash{$_}->{numval},
		 ]} typesrtkeys;

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
		  [mapfld($_->[0],'ppsym'=>'sym'),$_->[1],$_->[2],$_->[0]]
		  : ()
	}
	       @typetable];
}

sub xxx_get_generictypes { my($this) = @_;
	return [map {
		$this->{Types} =~ /$_->[0]/ ? [mapfld($_->[0],'ppsym'=>'sym'),$_->[1],$_->[2],$_->[0]] : ()
	}
	       @typetable];
}


package PDL::PP::Code;

# my ( $threadloops, $coderef, $sizeprivs ) = $this->separate_code( $code );
#
# umm, can't call classes defined later on in code ...
# hence moved to end of file
# (rather ugly...)
#
# separates the code into an array of C fragments (strings),
# variable references (strings starting with $) and
# loops (array references, 1. item = variable.
#
sub separate_code {
    my ( $this, $code ) = @_;

    my $coderef = new PDL::PP::Block;
 
    my @stack = ($coderef);
    my $threadloops = 0;
    my $sizeprivs = {};

    $_ = $code;
##    print "Code to parse = [$_]\n" if $::PP_VERBOSE; 
    while($_) {
	# Parse next statement
	
	# I'm not convinced that having the checks twice is a good thing,
	# since it makes it easy (for me at least) to forget to update one
	# of them

	s/^(.*?) # First, some noise is allowed. This may be bad.
	    ( \$(ISBAD|ISGOOD|SETBAD)\s*\(\s*\$?[a-zA-Z_]+\s*\([^)]*\)\s*\)   # $ISBAD($a(..)), ditto for ISGOOD and SETBAD
                |\$PP(ISBAD|ISGOOD|SETBAD)\s*\(\s*[a-zA-Z_]+\s*,\s*[^)]*\s*\)   # $PPISBAD(CHILD,[1]) etc
###                |\$STATE(IS|SET)(BAD|GOOD)\s*\(\s*[^)]*\s*\)      # $STATEISBAD(a) etc
                |\$PDLSTATE(IS|SET)(BAD|GOOD)\s*\(\s*[^)]*\s*\)   # $PDLSTATEISBAD(a) etc
	        |\$[a-zA-Z_]\w*\s*\([^)]*\)  # $a(...): access
		|\bloop\s*\([^)]+\)\s*%{   # loop(..) %{
		|\btypes\s*\([^)]+\)\s*%{  # types(..) %{
		|\bthreadloop\s*%{         # threadloop %{
		|%}                        # %}
		|$)//xs
		    or confess("Invalid program $_");
	my $control = $2;
	# Store the user code.
	# Some day we shall parse everything.
	push @{$stack[-1]},$1;

if ( $control =~ /^\$STATE/ ) { print "\nDBG: - got [$control]\n\n"; }

	# Then, our control.
	if($control) {
	    if($control =~ /^loop\s*\(([^)]+)\)\s*%{/) {
		my $ob = new PDL::PP::Loop([split ',',$1],
					   $sizeprivs,$this);
		print "SIZEPRIVSXX: $sizeprivs,",(join ',',%$sizeprivs),"\n" if $::PP_VERBOSE;
		push @{$stack[-1]},$ob;
		push @stack,$ob;
	    } elsif($control =~ /^types\s*\(([^)]+)\)\s*%{/) {
		my $ob = new PDL::PP::Types($1,$this);
		push @{$stack[-1]},$ob;
		push @stack,$ob;
	    } elsif($control =~ /^threadloop\s*%{/) {
		my $ob = new PDL::PP::ThreadLoop();
		push @{$stack[-1]},$ob;
		push @stack,$ob;
		$threadloops ++;
	    } elsif($control =~ /^\$PP(ISBAD|ISGOOD|SETBAD)\s*\(\s*([a-zA-Z_]+)\s*,\s*([^)]*)\s*\)/) {
		push @{$stack[-1]},new PDL::PP::PPBadAccess($1,$2,$3,$this);
	    } elsif($control =~ /^\$(ISBAD|ISGOOD|SETBAD)VAR\s*\(\s*([^)]*)\s*,\s*([^)]*)\s*\)/) {
		push @{$stack[-1]},new PDL::PP::BadVarAccess($1,$2,$3,$this);
	    } elsif($control =~ /^\$(ISBAD|ISGOOD|SETBAD)\s*\(\s*\$?([a-zA-Z_]+)\s*\(([^)]*)\)\s*\)/) {
		push @{$stack[-1]},new PDL::PP::BadAccess($1,$2,$3,$this);
#	    } elsif($control =~ /^\$STATE(IS|SET)(BAD|GOOD)\s*\(\s*([^)]*)\s*\)/) {
#		push @{$stack[-1]},new PDL::PP::StateBadAccess($1,$2,$3,$this);
	    } elsif($control =~ /^\$PDLSTATE(IS|SET)(BAD|GOOD)\s*\(\s*([^)]*)\s*\)/) {
		push @{$stack[-1]},new PDL::PP::PDLStateBadAccess($1,$2,$3,$this);
	    } elsif($control =~ /^\$[a-zA-Z_]\w*\s*\([^)]*\)/) {
		push @{$stack[-1]},new PDL::PP::Access($control,$this);
	    } elsif($control =~ /^%}/) {
	        pop @stack;
	    } else {
		confess("Invalid control: $control\n");
	    }
	} else {
	    print("No \$2!\n") if $::PP_VERBOSE;
	}
    } # while: $_

    return ( $threadloops, $coderef, $sizeprivs );

} # sub: separate_code()


# return true
1;
