#
# files ending in .fits will end up in
# PDL/Graphics/LUT/tables/
#
# WARNING: this all looks rather UNIX specific
# (unless MakeMaker is clever enough to convert
# UNIX file names to the system it's running on)
#
use ExtUtils::MakeMaker;

my @tables = glob( "*.fits" );

WriteMakefile(
  'NAME'         => 'PDL::Graphics::LUT::tables::DATA',   
  'VERSION_FROM' => '../../../Basic/Core/Version.pm',
  'PM' => {
      (map {($_,'$(INST_LIBDIR)/'.$_)} @tables)
   }
);

