
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

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
#	 map {s/[.]PL$//; m/^Makefile$/ ? () : ($_.".PL" => $_)}
#	 	<*.PL>, <Doc/*.PL>
#	},
     'clean'       => { 'FILES' => q[Doc/Config.pm] },
# map {s/[.]PL$//; m/^Makefile$/ ?
#					  () : $_}
#			<*.PL>, <Doc/*.PL> },
);
