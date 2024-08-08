# Test Script for the PDL interface to the GSL library
#  This tests only that the interface is working, i.e. that the
#   functions can be called. The actual return values are not
#   checked. 
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::GSL::RNG;

my $image = zeroes(10,10);
my $ndim = 2;
my $name = '';
my $sigma = 1;

# new() function Test: 
my $rng = PDL::GSL::RNG->new('taus');

pass('new() function');

# set_seed(); function Test: 
$rng->set_seed(666);

pass('set_seed(); function');

my $rng2 = PDL::GSL::RNG->new('taus')->set_seed(666);
is(ref $rng2, 'PDL::GSL::RNG', 'PDL::GSL::RNG->new(..)->set_seed(..)');

# min() function Test: 
my $min = $rng->min(); my $max = $rng->max();

pass('min() function');

# rmax() function Test: 
$min = $rng->min(); $max = $rng->max();

pass('rmax() function');

# name() function Test: 
$name = $rng->name();

pass('name() function');

# get_uniform() function Test: 
my $x = zeroes 5,6; $max=100;
my $o = $rng->get_uniform(10,10); $rng->get_uniform($x);

pass('get_uniform() function');

# get_uniform_pos() function Test: 
$x = zeroes 5,6;

$o = $rng->get_uniform_pos(10,10); $rng->get_uniform_pos($x);

pass('get_uniform_pos() function');

# get() function Test: 
$x = zeroes 5,6;

$o = $rng->get(10,10); $rng->get($x);

pass('get() function');

# get_int() function Test: 
$x = zeroes 5,6; $max=100;

$o = $rng->get(10,10); $rng->get($x);

pass('get_int() function');

# ran_gaussian() function Test: 
$o = $rng->ran_gaussian($sigma,10,10);

$rng->ran_gaussian($sigma,$x);


pass('ran_gaussian() function');

# $rng->ran_gaussian_var() function Test: 
my $sigma_pdl = rvals zeroes 11,11; $o = $rng->ran_gaussian_var($sigma_pdl);

pass('ran_gaussian_var() method');

# ran_additive_gaussian() function Test: 
$rng->ran_additive_gaussian(1,$image);

pass('ran_additive_gaussian() method');

# ran_additive_poisson() function Test: 
$rng->ran_additive_poisson(1,$image);

pass('ran_additive_poisson() method');

# ran_feed_poisson() function Test: 
$rng->ran_feed_poisson($image);

pass('ran_feed_poisson() method');

# ran_bivariate_gaussian() function Test: 
$o = $rng->ran_bivariate_gaussian(1,2,0.5,1000);

pass('ran_bivariate_gaussian() method');

# ran_dir() function Test: 
$o = $rng->ran_dir($ndim,12);

pass('ran_dir() method');

# ran_discrete_preproc() function Test: 
my $prob = pdl [0.1,0.3,0.6];

my $discrete_dist_handle = $rng->ran_discrete_preproc($prob);

$o = $rng->ran_discrete($discrete_dist_handle,100);

pass('ran_discrete_preproc() method');

# ran_discrete() function Test: 
$prob = pdl [0.1,0.3,0.6];

$discrete_dist_handle = $rng->ran_discrete_preproc($prob);

$o = $rng->ran_discrete($discrete_dist_handle,100);

pass('ran_discrete() method');

my $vec2d = sequence(10,10);
$rng->ran_shuffle($_) for $vec2d->dog;
ok any($vec2d != sequence(10,10)), 'ran_shuffle() method';

$vec2d = sequence(10,10);
$rng->ran_shuffle_1d($vec2d);
ok any($vec2d != sequence(10,10)), 'ran_shuffle_1d() method';

done_testing;
