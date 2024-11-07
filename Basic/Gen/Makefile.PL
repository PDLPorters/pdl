use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;

my @pms = map {($_ => '$(INST_LIBDIR)/'.$_)}
  (<*.pm>, <PP/*.pm>, 'PP/Dump.pm');

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
  <<'EOF';
PP/Dump.pm: PP/dump.pp
	$(PERL) PP$(DFSEP)dump.pp > PP$(DFSEP)Dump.pm.tmp
	$(MV) PP$(DFSEP)Dump.pm.tmp PP$(DFSEP)Dump.pm

EOF
};

WriteMakefile(NAME => "PDL::PP",
	PM => {@pms},
	clean => {FILES => "PP/Dump.pm PP/Dump.pm.tmp pptemplate.pod"},
	NO_MYMETA => 1,
);
