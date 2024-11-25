use strict;
use warnings;
use Test::More;
use Test::Exception;

use PDL::LiteF;
use PDL::Dbg;

my @lv_subs = map [$_], qw(
  dice flat indexND indexNDb broadcast nslice_if_pdl px range reorder reshape
  sever slice indexNDb mslice
);
push @lv_subs, map [$_, 1], qw(clump dummy index unbroadcast);
push @lv_subs, map [$_, pdl 1], qw(where whereND);
push @lv_subs, map [$_, 0, 1], qw(diagonal);
push @lv_subs, map [$_, 0, 0], qw(dice_axis index2d mv xchg);
push @lv_subs, map [$_, pdl([0]), undef, undef], qw(rangeb);

my $pa = sequence 3,3;
for (@lv_subs) {
  my ($name, @args) = @$_;
  no warnings 'uninitialized';
  lives_ok { $pa->$name(@args) .= 0 } "lvalue @$_ ran OK";
}

is($pa->max, 0, "lvalue slice modified values");

done_testing;
