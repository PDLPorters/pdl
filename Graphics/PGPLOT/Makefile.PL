use strict;
use warnings;
use ExtUtils::MakeMaker;

eval {
  local @INC = grep $_ ne '.', @INC;
  require PGPLOT;
};
if ($@) {
    write_dummy_make("Not building PDL::Graphics::PGPLOT, PGPLOT not installed");
    return;
}

WriteMakefile(
	      'NAME'  	=> 'PDL::Graphics::PGPLOT',
	      'VERSION_FROM' => '../../Basic/PDL.pm',
	      'DIR' => ['Window'],
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
