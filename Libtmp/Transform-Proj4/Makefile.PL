use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;

eval { require Alien::proj };
if ($@) {
  write_dummy_make("Will skip build of PDL::Transform::Proj4 on this system - no Alien::proj");
  return;
}

my @pd_srcs;
undef &MY::init_PM; # suppress warning
*MY::init_PM = sub {
  package MY; # so that "SUPER" works right
  my ($self) = @_;
  $self->SUPER::init_PM;
  @pd_srcs = ::pdlpp_eumm_update_deep($self);
};

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pd_srcs); };

WriteMakefile(
  NAME  => 'PDL',
  VERSION_FROM => 'lib/PDL/Transform/Proj4.pd',
  MIN_PERL_VERSION => '5.014',
  XSMULTI => 1,
  INC => PDL::Core::Dev::PDL_INCLUDE() . ' ' . Alien::proj->cflags,
  TYPEMAPS => [PDL::Core::Dev::PDL_TYPEMAP()],
  LIBS => [Alien::proj->libs],
  clean => { FILES => join ' ', qw(MANIFEST.bak) },
  NO_MYMETA => 1,
);
