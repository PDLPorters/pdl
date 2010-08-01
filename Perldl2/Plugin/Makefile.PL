use ExtUtils::MakeMaker;

WriteMakefile(
   'NAME' => 'PDL::Perldl2::Plugin',
   'VERSION' => '2.000',
   'PM' => {
      'NiceSlice.pm' => '$(INST_LIBDIR)/Plugin/NiceSlice.pm',
      'PDLCommands.pm' => '$(INST_LIBDIR)/Plugin/PDLCommands.pm',
      'PrintControl.pm' => '$(INST_LIBDIR)/Plugin/PrintControl.pm',
   },
);
