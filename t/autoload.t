# Test PDL::AutoLoader
use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::NiceSlice;
use Test::PDL;

plan skip_all => 'This test must be run from t/..' if !-f 't/autoload_func.pdl';

use_ok('PDL::AutoLoader');

#$PDL::debug = 1;

our @PDLLIB = ("./t"); # this means you have to run the test from t/..

my $x = long(2 + ones(2,2));

is_pdl autoload_func($x), pdl('29 29; 29 29'), 'autoloaded func worked';
{ no warnings 'once';
is $::GLOBAL_VAR, '$'.'COMP(max_it)', "NiceSlice didn't mangle text";
}

#check that tilde expansion works (not applicable on MS Windows)
SKIP: {
   skip "Inapplicable to MS Windows", 1 if $^O =~ /MSWin/i;
   my $tilde = (PDL::AutoLoader::expand_path('~'))[0];
   my $get = $ENV{'HOME'} || (getpwnam( getlogin || getpwuid($<) ))[7];
   my $glob = glob q(~);
   if ($glob !~ /^~/) {
      is($tilde, $glob, "Check tilde expansion (Got '$get' from (getpwnam(getpwuid(\$<)))[7] )");
   } else {
      is($tilde, $get, "Check tilde expansion (Got '$glob' from glob ~");
   }
}

subtest 'recursive +dir PDLLIB and reloader rescan' => sub {
  require File::Spec;
  require File::Temp;
  my $dir = File::Temp::tempdir(CLEANUP => 1);
  my $sub = File::Spec->catdir($dir, 'nested');
  mkdir($sub) or die "mkdir $sub: $!";

  my $f1 = File::Spec->catfile($dir, 'al_add_one.pdl');
  open my $fh1, '>', $f1 or die "open $f1: $!";
  print {$fh1} "sub al_add_one { return \$_[0] + 1; }\n1;\n";
  close $fh1;

  my $f2 = File::Spec->catfile($sub, 'al_double_val.pdl');
  open my $fh2, '>', $f2 or die "open $f2: $!";
  print {$fh2} "sub al_double_val { return \$_[0] * 2; }\n1;\n";
  close $fh2;

  local @PDLLIB = (PDL::AutoLoader::expand_path("+$dir"));
  is al_add_one(41), 42, 'autoloaded top-level .pdl function';
  is al_double_val(21), 42, 'autoloaded nested +dir .pdl function';

  eval { al_nonexistent_fn() };
  ok $@, 'calling nonexistent autoloaded function dies';

  local $PDL::AutoLoader::Rescan = 0;
  PDL::AutoLoader::reloader();

  $PDL::AutoLoader::FileInfo{al_add_one} = [ $f1, (stat($f1))[9] - 10 ];
  open my $fh, '>', $f1 or die "open $f1: $!";
  print {$fh} "sub al_add_one { return \$_[0] + 100; }\n1;\n";
  close $fh;

  local $PDL::AutoLoader::Rescan = 1;
  PDL::AutoLoader::reloader();
  is al_add_one(5), 105, 'reloader picked up modified .pdl file';
};

done_testing;
