
# Build the whole PDL distribtuion

use ExtUtils::MakeMaker;

WriteMakefile(
 'NAME' => 'PDL',
 'VERSION_FROM'     => 'PDL/Core/Core.pm',  
 'EXE_FILES' => ['perldl'],
 'linkext'  => {LINKTYPE => ''},  # No linking required in this directory
  'dist'      => { COMPRESS=>"gzip", SUFFIX=>"gz" }
);

