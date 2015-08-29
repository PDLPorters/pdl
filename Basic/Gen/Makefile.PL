use strict;
use warnings;
use ExtUtils::MakeMaker;
use Config;

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

#EU::MM's processPL() is continually broken on Win32 ... hence:
## Fix appears to be necessary on all MM platforms now, to avoid circular references :-( -- CED 9-July-2008
undef &MY::processPL; *MY::processPL = sub {
    package MY;
    my ($self) = @_;
    return $self->SUPER::processPL if 0; ## unless $^O =~ /MSWin32/i && $Config{make} =~ /\b[dn]make/i;
    return "" unless $self->{PL_FILES};
    my @m;
    my $alltarget = 'all';
    my $colon = $Config::Config{make} =~ /\bdmake/i ? ':' : '::';
    foreach my $plfile (sort keys %{$self->{PL_FILES}}) {
        my $list = ref($self->{PL_FILES}->{$plfile})
                ? $self->{PL_FILES}->{$plfile}
                : [$self->{PL_FILES}->{$plfile}];
        foreach my $target (@$list) {
            push @m,
                "\n$alltarget :: $target\n\t\$(NOECHO) \$(NOOP)\n",
                "\n$target $colon\n\t\$(PERLRUNINST) $plfile $target\n";
        }
    }
    join "", @m;
};

WriteMakefile(NAME => "PDL::PP",
	PM => {@pms},
	'EXE_FILES' => ['pptemplate'],
	clean => {FILES => "PP/Dump.pm PP/Dump.pm.tmp pptemplate.pod"},
	(eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
