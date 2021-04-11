use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pods = map { $_=~s/.pod//; $_ } grep { ! m/Index.pod/ } glob("*.pod");

# do we want to create PP-Inline?
eval 'require Pod::Select';
if (!$@) {
    push @pods, 'PP-Inline' unless grep {/PP-Inline/} @pods;
}

my @man1 = map { $_.".pod", '$(INST_MAN1DIR)/PDL::' . $_ . '.$(MAN1EXT)' } @pods;
my @pms  = map { $_.".pod", '$(INST_LIBDIR)/' . $_ .".pod"} @pods;

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
  my $text = '';
  eval 'require Pod::Select';
  if (!$@) {
     $text .= << "EOPS" ;

PP-Inline.pod: ../Gen/Inline/Pdlpp.pm
\t\$(PERLRUN) -MPod::Select -e "podselect('../Gen/Inline/Pdlpp.pm');" > PP-Inline.pod

EOPS

  }
  $text;
};

WriteMakefile(
 'NAME' =>      'PDL::pod',
 'MAN1PODS' =>  { @man1 },
 'MAN3PODS' =>  { },
 'PM'    =>     { @pms },
 'clean' => {FILES => "PP-Inline.pod"},
 NO_MYMETA => 1,
);
