use ExtUtils::MakeMaker;
WriteMakefile(NAME => "PDL::PP",
	PM => {
	 map {($_ => '$(INST_LIBDIR)/'.$_)}
	 	<*.pm>, <PP/*.pm>, 'PP/Dump.pm'
	},
	clean => {FILES => "PP/Dump.pm PP/Dump.pm.tmp"}
);

sub MY::postamble {
'PP/Dump.pm: PP/dump.pp
	$(PERL) PP/dump.pp > PP/Dump.pm.tmp
'.($^O =~ /win32/i ? '	move PP\Dump.pm.tmp PP\Dump.pm' :
'	mv PP/Dump.pm.tmp PP/Dump.pm')."\n";
}
