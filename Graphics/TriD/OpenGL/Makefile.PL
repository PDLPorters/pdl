# Makefile.PL for a package defined by PP code.

#use PDL::Core::Dev;            # Pick up development utilities
use ExtUtils::MakeMaker;


@pack = (["opengl.pd",OpenGL,PDL::Graphics::OpenGL]);

%hash = pdlpp_stdargs(@::pack);

push @{$hash{LIBS}}, $PDL_CONFIG{OPENGL_LIBS};
$hash{INC} .= $PDL_CONFIG{OPENGL_INC};
$hash{DEFINE} .= $PDL_CONFIG{OPENGL_DEFINE};
$hash{clean}{FILES} .= ' tmp_*.h ppcode.out';

#use Data::Dumper;
#my $out = Dumper(\%hash);
#print $out;

WriteMakefile(%hash);

# add a dependancy for the perldl.conf file

sub MY::postamble {
  my $amb = pdlpp_postamble_int(@::pack);
#
# Perls GetOpt::Long routine needs to have a space after the option name
#
  my $inc = $PDL_CONFIG{OPENGL_INC};
  $inc =~ s/-I(\S)/-I $1/g;
  my $def = $PDL_CONFIG{OPENGL_DEFINE};
  $def =~ s/-D(\S)/-D $1/g;

  $amb =~ s/opengl.pd\s*$/opengl.pd $inc $def\n/m;
  $amb;
}  # Add genpp rule

