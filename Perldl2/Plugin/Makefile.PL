use ExtUtils::MakeMaker;

WriteMakefile(
   'NAME' => 'PDL::Perldl2::Plugin',
   'VERSION' => '2.001',
   'PM' => {
      'CleanErrors.pm' =>  '$(INST_LIBDIR)/Plugin/CleanErrors.pm',
      'NiceSlice.pm' => '$(INST_LIBDIR)/Plugin/NiceSlice.pm',
      'PDLCommands.pm' => '$(INST_LIBDIR)/Plugin/PDLCommands.pm',
      'PrintControl.pm' => '$(INST_LIBDIR)/Plugin/PrintControl.pm',
   },
);
