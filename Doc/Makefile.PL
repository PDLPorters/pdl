use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'         => 'PDL::Doc',
    'VERSION_FROM' => '../Basic/Core/Version.pm',
    'LIBS'         => [''],   # e.g., '-lm'
    'DEFINE'       => '',     # e.g., '-DHAVE_SOMETHING'
    'INC'          => '',     # e.g., '-I/usr/include/other'
    'dist'         => { SUFFIX => "gz", COMPRESS => "gzip -f"},
    'PM'           => { 'Doc.pm' =>
		      '$(INST_LIBDIR)/Doc.pm',
		        'Doc/Config.pm' => '$(INST_LIBDIR)/Doc/Config.pm',
		        'Doc/Perldl.pm' => '$(INST_LIBDIR)/Doc/Perldl.pm',
			},
    'PL_FILES'     => {q[Doc/Config.pm.PL]=>q[Doc/Config.pm]},
     'clean'       => { 'FILES' => q[Doc/Config.pm] },
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
