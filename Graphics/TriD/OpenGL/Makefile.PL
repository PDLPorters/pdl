# Makefile.PL for a package defined by PP code.

use PDL::Core::Dev;            # Pick up development utilities
use ExtUtils::MakeMaker;
require '../../../perldl.conf';
@pack = (["opengl.pd",OpenGL,PDL::Graphics::OpenGL]);

%hash = pdlpp_stdargs(@::pack);

push @{$hash{LIBS}}, $PDL_CONFIG{OPENGL_LIBS};
$hash{INC} .= $PDL_CONFIG{OPENGL_INC};
$hash{DEFINE} .= $PDL_CONFIG{OPENGL_DEFINE};

#use Data::Dumper;
#my $out = Dumper(\%hash);
#print $out;

WriteMakefile(%hash);

# add a dependancy for the perldl.conf file

sub MY::postamble {
  my $amb = pdlpp_postamble_int(@::pack);
  $amb =~ s/pm_to_blib$/pm_to_blib \.\.\/\.\.\/\.\.\/perldl.conf/m;
  $amb;
}  # Add genpp rule

