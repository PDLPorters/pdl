BEGIN{if ($^O =~ /MSWin/) {
  print "getting dosglob...\n";
  require File::DosGlob;
  File::DosGlob->import('glob');}
}

@pms = map {($_ => '$(INST_LIBDIR)/'.$_)}
  (<*.pm>, <PP/*.pm>, 'PP/Dump.pm');
push @pms, ('pptemplate.pod' => '$(INST_LIBDIR)/pptemplate.pod')
  if $] >= 5.006;

use ExtUtils::MakeMaker;
WriteMakefile(NAME => "PDL::PP",
	PM => {@pms},
	'EXE_FILES' => ['pptemplate'],
	clean => {FILES => "PP/Dump.pm PP/Dump.pm.tmp pptemplate"},
);

sub MY::postamble {
  my $text =
'PP/Dump.pm: PP/dump.pp
	$(PERL) PP/dump.pp > PP/Dump.pm.tmp
'.($^O =~ /win32/i ? '	move PP\Dump.pm.tmp PP\Dump.pm' :
'	mv PP/Dump.pm.tmp PP/Dump.pm')."\n";

  if ( $] >= 5.006 ) {
    $text .= << 'EOT';

pptemplate.pod: pptemplate
	podselect pptemplate > pptemplate.pod

EOT
  }
  $text;
}
