use strict;
use warnings;
use Test::More;
use Config;
use Cwd qw(getcwd);
use File::Spec;
use File::Temp qw(tempdir);
use PDL::Core::Dev;

subtest 'functional probes (trylink, isbigendian, get_maths_libs)' => sub {
  ok defined isbigendian(), 'isbigendian returns defined boolean';
  ok defined get_maths_libs(), 'get_maths_libs returns defined string';

  ok trylink(
    'trivial C program',
    '#include <stdio.h>',
    'int x = 1; (void)x;',
    '',
  ), 'trylink succeeds for valid C program';

  ok !trylink(
    'invalid C program',
    '',
    'this_is_not_valid_c_syntax(;',
    '',
  ), 'trylink returns false for invalid C program';
};

subtest 'functional pptemplate build and test' => sub {
  my $orig_dir = getcwd();
  my $blib_lib  = File::Spec->catdir($orig_dir, 'blib', 'lib');
  my $blib_arch = File::Spec->catdir($orig_dir, 'blib', 'arch');
  my $pptemplate = File::Spec->catfile($orig_dir, 'blib', 'script', 'pptemplate');
  plan skip_all => 'pptemplate script not built in blib/script'
    unless -f $pptemplate;

  my $make = $Config{make} || 'make';
  my $dir = tempdir(CLEANUP => 1);

  local $ENV{PERL5LIB} = join($Config{path_sep},
    $blib_lib, $blib_arch, ($ENV{PERL5LIB} ? $ENV{PERL5LIB} : ()));

  chdir $dir or die "chdir $dir: $!";
  eval {
    is system($^X, $pptemplate, 'PDL::TestPP'), 0, 'pptemplate generated PDL::TestPP';
    chdir 'PDL-TestPP' or die "chdir PDL-TestPP: $!";
    ok -f 'Makefile.PL', 'pptemplate created Makefile.PL';

    is system($^X, 'Makefile.PL'), 0, 'perl Makefile.PL succeeded';
    is system($make), 0, "$make build succeeded";
    is system($make, 'test'), 0, "$make test passed";
  };
  my $err = $@;
  chdir $orig_dir or die "chdir $orig_dir: $!";
  die $err if $err;
};

done_testing;
