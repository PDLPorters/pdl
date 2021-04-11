use strict;
use warnings;

# files ending in .fits will end up in
# PDL/Graphics/LUT/ramps/
use ExtUtils::MakeMaker;

my @tables = glob( "*.fits" );

WriteMakefile(
  'NAME'         => 'PDL::Graphics::LUT::ramps::DATA',   
  'VERSION_FROM' => '../../../Basic/PDL.pm',
  'PM' => {
      (map {($_,'$(INST_LIBDIR)/'.$_)} @tables)
   },
   (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
