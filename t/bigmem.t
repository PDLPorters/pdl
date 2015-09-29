#!/usr/bin/perl
#
# Created on: Thu 17 Sep 2015 09:21:15 AM 
# Last saved: Thu 17 Sep 2015 02:35:49 PM 
#
# This is a start at tests to verify PDL operation with pdls having
# large numbers of elements (more than than 32bits binary)
#
use Test::More;

BEGIN {
   if ($ENV{AUTOMATED_TESTING} or $ENV{CI_TESTING}) {
      plan skip_all => 'bigmem tests skipped to avoid OOM fails';
   } else {
      plan tests => 2;
   }
}

use PDL;
$PDL::BIGPDL = 1;  # should this be the defaults for 64bit index support?

# Tests for PDL::Core
#
# NOTE: pdl and PDL->new can't really be tested since the corresponding
#       perl data may be 10-100x bigger than the end pdl element sizes.
#
# A lot of these PDL::Core tests simply check that these routines don't
# break for large pdls as a sanity check.  Ideally, more thorough tests
# could be done but code inspection/review might be more efficient.
#
# nelem
# dims
# shape

my $bigbyte = ones( byte, 5*1024*1024*1024+17 );
ok( $bigbyte->shape->sclr == $bigbyte->nelem, "shape handles indx dims > 4GiB");

$bigbyte = ones(byte, 2**30, 4);
my $aaa = $bigbyte->slice("3:-10");
my $bbb = $aaa->slice(":,3");
ok( $bbb->sum == $bbb->nelem, "slices of slices of giant PDLs seem to work right");

# ndims
# getndims
# dim
# getdim
# get_dataref
# upd_data
# doflow
# flows
# copy
# unwind
# make_physical
# dummy
# clump
# thread_define
# thread
# diagonal
# thread[123I]
# sever
# info
# mslice
# inplace, is_inplace, set_inplace, new_or_inplace
# new_from_specification
# zeros
# ones
# reshape
# squeeze
# flat
# convert
# byte|short|ushort|long|indx|longlong|float|double
# set, at, sclr
# cat, dog
# set_autopthread_targ, get_autopthread_targ, get_autopthread_actual, set_autopthread_size, get_autopthread_size

done_testing();  # tests done
