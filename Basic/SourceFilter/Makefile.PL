
use ExtUtils::MakeMaker;
WriteMakefile(
   NAME	=> 'PDL::NiceSlice',
   VERSION_FROM => 'NiceSlice.pm',
   PM => {
      'NiceSlice.pm' => '$(INST_LIBDIR)/NiceSlice.pm',
      'FilterUtilCall.pm' => '$(INST_LIBDIR)/NiceSlice/FilterUtilCall.pm',
      'FilterSimple.pm' => '$(INST_LIBDIR)/NiceSlice/FilterSimple.pm',
      'ModuleCompile.pm' => '$(INST_LIBDIR)/NiceSlice/ModuleCompile.pm',
   },
   PREREQ_PM => {
      Filter::Util::Call => 0,
      Text::Balanced => 0,
      PDL => 2.3,
   },
);

sub MY::postamble {
"
README: README.head NiceSlice.pm
\tcp README.head README
\tpod2text NiceSlice.pm >> README

";
}
