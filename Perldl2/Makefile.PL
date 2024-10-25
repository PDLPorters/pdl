use strict;
use warnings;
use ExtUtils::MakeMaker;

if (!eval { require Devel::REPL; 1 }) {
  # Devel::REPL was not found
  my $msg = "Devel::REPL is not installed, will not build Perldl2 shell";
  write_dummy_make($msg);
  return;
}

# Extra build target to build the doc database
undef &MY::postamble; # suppress warning
*MY::postamble = sub {
   <<"EOPS";

pdl2.pod : pdl2
\t\$(PERLRUN) -MPod::Select -e "podselect('pdl2');" > pdl2.pod

EOPS
};

my @podpms = map { $_.".pod", '$(INST_LIBDIR)/' . $_ .".pod"} qw/pdl2/;
WriteMakefile(
  NAME => 'PDL::Perldl2',
  VERSION => '2.000',
  EXE_FILES => [ 'pdl2' ],
  PM => { 'Script.pm' => '$(INST_LIBDIR)/Perldl2/Script.pm', @podpms },
  NO_MYMETA => 1,
);
