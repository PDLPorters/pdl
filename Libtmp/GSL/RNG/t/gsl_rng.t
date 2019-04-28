# Test Script for the PDL interface to the GSL library
#  This tests only that the interface is working, i.e. that the
#   functions can be called. The actual return values are not
#   checked. 
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use Test::More;
use PDL;
use PDL::GSL::RNG;

$image = zeroes(10,10);
$ndim = 2;
$name = '';
$sigma = 1;

# new() function Test: 
$rng = PDL::GSL::RNG->new('taus');

ok(1,'new() function');

# set_seed(); function Test: 
$rng->set_seed(666);

ok(1,'set_seed(); function');

my $rng2 = PDL::GSL::RNG->new('taus')->set_seed(666);
is(ref $rng2, 'PDL::GSL::RNG', 'PDL::GSL::RNG->new(..)->set_seed(..)');

# min() function Test: 
$min = $rng->min(); $max = $rng->max();

ok(1,'min() function');

# rmax() function Test: 
$min = $rng->min(); $max = $rng->max();

ok(1,'rmax() function');

# name() function Test: 
$name = $rng->name();

ok(1,'name() function');

# get_uniform() function Test: 
$x = zeroes 5,6; $max=100;

$o = $rng->get_uniform(10,10); $rng->get_uniform($x);

ok(1,'get_uniform() function');

# get_uniform_pos() function Test: 
$x = zeroes 5,6;

$o = $rng->get_uniform_pos(10,10); $rng->get_uniform_pos($x);

ok(1,'get_uniform_pos() function');

# get() function Test: 
$x = zeroes 5,6;

$o = $rng->get(10,10); $rng->get($x);

ok(1,'get() function');

# get_int() function Test: 
$x = zeroes 5,6; $max=100;

$o = $rng->get(10,10); $rng->get($x);

ok(1,'get_int() function');

# ran_gaussian() function Test: 
$o = $rng->ran_gaussian($sigma,10,10);

$rng->ran_gaussian($sigma,$x);


ok(1,'ran_gaussian() function');

# $rng->ran_gaussian_var() function Test: 
$sigma_pdl = rvals zeroes 11,11; $o = $rng->ran_gaussian_var($sigma_pdl);


ok(1,'ran_gaussian_var() method');

# ran_additive_gaussian() function Test: 
$rng->ran_additive_gaussian(1,$image);

ok(1,'ran_additive_gaussian() method');

# ran_additive_poisson() function Test: 
$rng->ran_additive_poisson(1,$image);

ok(1,'ran_additive_poisson() method');

# ran_feed_poisson() function Test: 
$rng->ran_feed_poisson($image);

ok(1,'ran_feed_poisson() method');

# ran_bivariate_gaussian() function Test: 
$o = $rng->ran_bivariate_gaussian(1,2,0.5,1000);

ok(1,'ran_bivariate_gaussian() method');

# ran_dir() function Test: 
$o = $rng->ran_dir($ndim,12);

ok(1,'ran_dir() method');

# ran_discrete_preproc() function Test: 
$prob = pdl [0.1,0.3,0.6];

$discrete_dist_handle = $rng->ran_discrete_preproc($prob);

$o = $rng->ran_discrete($discrete_dist_handle,100);

ok(1,'ran_discrete_preproc() method');

# ran_discrete() function Test: 
$prob = pdl [0.1,0.3,0.6];

$discrete_dist_handle = $rng->ran_discrete_preproc($prob);

$o = $rng->ran_discrete($discrete_dist_handle,100);

ok(1,'ran_discrete() method');

done_testing;
