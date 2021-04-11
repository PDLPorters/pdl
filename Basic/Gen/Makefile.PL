use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;

my @pms = map {($_ => '$(INST_LIBDIR)/'.$_)}
  (<*.pm>, <PP/*.pm>, 'PP/Dump.pm');
push @pms, ('pptemplate.pod' => '$(INST_LIBDIR)/pptemplate.pod');

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
  <<'EOF';
PP/Dump.pm: PP/dump.pp
	$(PERL) PP$(DFSEP)dump.pp > PP$(DFSEP)Dump.pm.tmp
	$(MV) PP$(DFSEP)Dump.pm.tmp PP$(DFSEP)Dump.pm

pptemplate.pod: pptemplate
	$(PERLRUN) -MPod::Select -e "podselect('pptemplate');" > pptemplate.pod

EOF
};

WriteMakefile(NAME => "PDL::PP",
	PM => {@pms},
	'EXE_FILES' => ['pptemplate'],
	clean => {FILES => "PP/Dump.pm PP/Dump.pm.tmp pptemplate.pod"},
	(eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
