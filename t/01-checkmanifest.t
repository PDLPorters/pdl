#!perl -w

use Test::More tests => 1;
use ExtUtils::Manifest qw(manicheck);

my @missing_files = do {
  local $SIG{__WARN__} = sub { }; # suppress "No such file:" messages
  grep { $_ ne 'Changes' } manicheck; # dev-mode doesn't have
};

is_deeply \@missing_files, [], 'missing files from MANIFEST'
  or map diag("$_\n"), @missing_files;
