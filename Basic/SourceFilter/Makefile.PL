use strict;
use warnings;
use ExtUtils::MakeMaker;

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
"
README: README.head NiceSlice.pm
\tcp README.head README
\tpod2text NiceSlice.pm >> README

";
};

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
      'Filter::Util::Call' => 0,
      'Text::Balanced' => 0,
#      'Module::Compile' => '0.23',
   },
   NO_MYMETA => 1,
);
