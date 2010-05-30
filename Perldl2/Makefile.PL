use ExtUtils::MakeMaker;

if (!defined $PDL::Config{WITH_DEVEL_REPL}) {
   if (eval 'require Devel::REPL;') {
      # Only generate Makefile if we have Devel::REPL
      WriteMakefile(
         'NAME' => 'PDL::Perldl2',
         'VERSION' => '2.000',
         'EXE_FILES' => [ 'pdl2' ],
         'DIR' => [ qw(Profile Plugin) ],
      );
   } else {
      # Devel::REPL was not found
      $PDL::Config{WITH_DEVEL_REPL} = 0;
      print STDERR "Devel::REPL is not installed, will not build Perldl2 shell\n";
   }
}
