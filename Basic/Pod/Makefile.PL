BEGIN{if ($^O =~ /MSWin/) {
  print "getting dosglob...\n";
  require File::DosGlob;
  File::DosGlob->import('glob');}
}
use ExtUtils::MakeMaker;

@pods = map { $_=~s/.pod//; $_ } glob("*.pod");

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


