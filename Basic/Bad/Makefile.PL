
# Makefile.PL for PDL::Bad module.

# we create a Bad.pm whatever the value of
# WITH_CONFIG - it's just the contents that will be
# different...
#
use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

# print a banner to the screen
use File::Spec;
require File::Spec->catfile( File::Spec->curdir, "perldl.conf" );
my $bvalflag = $PDL_CONFIG{WITH_BADVAL} || 0;
print "\n\t";
if ( $bvalflag ) {
    print "Congratulations - building PDL with bad value support (WITH_BADVAL=1)";
} else {
    print "building PDL without bad value support (WITH_BADVAL!=1)";
}
print "\n\n";

@pack = (["bad.pd",Bad,PDL::Bad]);

%hash = pdlpp_stdargs_int(@::pack);

#$hash{LIBS} = ['-lm'];

WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule

