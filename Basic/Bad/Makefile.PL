
# Makefile.PL for PDL::Bad module.

# we create a Bad.pm whatever the value of
# PDL_CONFIG - it's just the contents that will be
# different...
#
use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

# bad value support?
use vars qw( $bvalflag $usenan );
use File::Spec;
require File::Spec->catfile( File::Spec->updir, "Core", "badsupport.p" );

# print a banner to the screen
print "\n\t";
if ( $bvalflag ) {
    print "Congratulations - building PDL with bad value support (WITH_BADVAL=1)";
    print "\n\t";
    if ( $usenan ) {
 	print "+ using NaN for floating-point bad value";
    } else {
 	print "+ using -FLT_MAX/-DBL_MAX for floating-point bad values";
    }
} else {
    print "building PDL without bad value support (WITH_BADVAL!=1)";
}
print "\n\n";

@pack = (["bad.pd",Bad,PDL::Bad]);

%hash = pdlpp_stdargs_int(@::pack);

#$hash{LIBS} = ['-lm'];

WriteMakefile(%hash);

# we add ../Core/badsupport.p to the dependencies of Bad.pm
# - not very elegant
#
sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule

