# we have to create the .pm file by appending the 
# colour table data to the end of the file
# This is handled by LUT.pm.PL

use ExtUtils::MakeMaker;

WriteMakefile(
 'NAME'         => 'PDL::Graphics::LUT',
 'VERSION_FROM' => '../../Basic/Core/Version.pm',
);
