PDL::Core::Dev->import();

use ExtUtils::MakeMaker;

WriteMakefile(
	      'NAME' => 'PDL::IO::IDL',
	      'VERSION_FROM' => '../../Basic/Core/Version.pm',
	      'PM' => {'IDL.pm' => '$(INST_LIB)/PDL/IO/IDL.pm' }
	      );
