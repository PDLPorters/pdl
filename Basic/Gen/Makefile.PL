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
	clean => {FILES => "PP/Dump.pm PP/Dump.pm.tmp pptemplate pptemplate.pod"},
);

sub MY::postamble {
  my $text =
'PP/Dump.pm: PP/dump.pp
	$(PERL) PP/dump.pp > PP/Dump.pm.tmp
'.($^O =~ /win32/i ? '	move PP\Dump.pm.tmp PP\Dump.pm' :
'	mv PP/Dump.pm.tmp PP/Dump.pm')."\n";

  if ( $] >= 5.006 ) {
     $text .= << "EOPS" ;

pptemplate.pod: pptemplate
\t\$(PERLRUN) -MPod::Select -e "podselect('pptemplate');" > pptemplate.pod

EOPS

  }
  $text;
}

#EU::MM's processPL() is continually broken on Win32 ... hence:
sub MY::processPL {
    ## Fix appears to be necessary on all MM platforms now, to avoid circular references :-( -- CED 9-July-2008
    if(1) { ## if($^O =~ /MSWin32/i && ($Config{make} =~ /\bdmake/i || $Config{make} =~ /\bnmake/i)) {
    my($self) = shift;
    return "" unless $self->{PL_FILES};
    my(@m, $plfile);
    foreach $plfile (sort keys %{$self->{PL_FILES}}) {
        my $list = ref($self->{PL_FILES}->{$plfile})
                ? $self->{PL_FILES}->{$plfile}
                : [$self->{PL_FILES}->{$plfile}];
        my $target;
        if($Config{make} =~ /\bdmake/i) {
          foreach $target (@$list) {
          push @m, "
all :: $target
	\$(NOECHO) \$(NOOP)

$target :
	\$(PERLRUNINST) $plfile $target
";
          } # close foreach
        }
        else {
          foreach $target (@$list) {
          push @m, "
all :: $target
	\$(NOECHO) \$(NOOP)

$target ::
	\$(PERLRUNINST) $plfile $target
";
          } # close foreach
        }  
    }
    join "", @m;
    }
   else {
    package MY;
    my $self = shift;
    return $self->SUPER::processPL;
    }
}

