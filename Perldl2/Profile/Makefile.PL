use ExtUtils::MakeMaker;

WriteMakefile(
   'NAME' => 'PDL::Perldl2::Profile',
   'VERSION' => '2.000',
   'PM' => { 'Perldl2.pm' => '$(INST_LIBDIR)/Profile/Perldl2.pm' },
   (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
