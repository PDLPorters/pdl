BEGIN{if ($^O =~ /MSWin/) {
  print "getting dosglob...\n";
  require File::DosGlob;
  File::DosGlob->import('glob');}
}
use ExtUtils::MakeMaker;

@pods = map { $_=~s/.pod//; $_ } glob("*.pod");

# do we want to create PP-Inline?
eval 'require Pod::Select';
if (!$@ && $] >= 5.005 ) {
    push @pods, 'PP-Inline' unless grep {/PP-Inline/} @pods;
}

# DISABLED
# push @pods, 'Export';


my @man1 = map { $_.".pod", '$(INST_MAN1DIR)/PDL::' . $_ . '.$(MAN1EXT)' } @pods;
push @man1,  'Intro.pod', '$(INST_MAN1DIR)/pdl'.'.$(MAN1EXT)';

my @pms  = map { $_.".pod", '$(INST_LIBDIR)/' . $_ .".pod"} @pods;

WriteMakefile(
 'NAME' =>      'PDL::pod',

 'MAN1PODS' =>  { @man1 },
 'MAN3PODS' =>  { },
 'PM'    =>     { @pms },
 'clean' => {FILES => ""},
 'linkext' =>   {LINKTYPE => ''},  # No linking required in this directory
);

sub MY::postamble {
  my $text = '';
  eval 'require Pod::Select';
  if (!$@ && $] >= 5.005 ) {
    $text .= << 'EOT';

PP-Inline.pod: ../Gen/Inline/Pdlpp.pm
	podselect ../Gen/Inline/Pdlpp.pm > PP-Inline.pod

EOT
  }
  $text;
}

