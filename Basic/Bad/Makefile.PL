use strict;
use warnings;
use ExtUtils::MakeMaker;

# we create a Bad.pm whatever the value of
# PDL_CONFIG - it's just the contents that will be
# different...

# bad value support?
use vars qw( $bvalflag $usenan $bvalPerPdl );
use File::Spec;
require File::Spec->catfile( File::Spec->updir, "Core", "badsupport.p" );

# print a banner to the screen
if ( $bvalflag ) {
    print "Congratulations - building PDL with bad value support (WITH_BADVAL=1)\n\t";

    if ( $usenan ) {
 	print "+ using NaN for floating-point bad value";
    } else {
 	print "+ using -FLT_MAX/-DBL_MAX for floating-point bad values";
    }
    print "+ with per-piddle bad values (EXPERIMENTAL FEATURE)"
      if $bvalPerPdl;
} else {
    print "building PDL without bad value support (WITH_BADVAL!=1)";
}
print "\n";

my @pack = (["bad.pd",qw(Bad PDL::Bad)]);

my %hash = pdlpp_stdargs_int(@pack);

#$hash{LIBS} = ['-lm'];

# we add ../Core/badsupport.p to the dependencies of Bad.pm
undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
