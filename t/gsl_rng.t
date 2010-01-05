

# Test Script for the PDL interface to the GSL library
#  This tests only that the interface is working, i.e. that the
#   functions can be called. The actual return values are not
#   checked. 
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use PDL;

BEGIN {
        eval " use PDL::GSL::RNG; ";
        $loaded = ($@ ? 0 : 1);
}

print "1..18\n";
unless ($loaded) {
	#print STDERR "PDL::Slatec not installed. All tests are skipped.\n";
	for (1..18) {
		print "ok $_ # Skipped: PDL::GSL::RNG not available.\n";
	}
	exit;
}


$n = 1;
$image = zeroes(10,10);
$ndim = 2;
$name = '';
$sigma = 1;
# new() function Test: 
$rng = PDL::GSL::RNG->new('taus');

print "ok ".$n++."\n";

# set_seed(); function Test: 
$rng->set_seed(666);

print "ok ".$n++."\n";

# min() function Test: 
$min = $rng->min(); $max = $rng->max();

print "ok ".$n++."\n";

# rmax() function Test: 
$min = $rng->min(); $max = $rng->max();

print "ok ".$n++."\n";

# name() function Test: 
$name = $rng->name();

print "ok ".$n++."\n";

# get_uniform() function Test: 
$a = zeroes 5,6; $max=100;

$o = $rng->get_uniform(10,10); $rng->get_uniform($a);

print "ok ".$n++."\n";

# get_uniform_pos() function Test: 
$a = zeroes 5,6;

$o = $rng->get_uniform_pos(10,10); $rng->get_uniform_pos($a);

print "ok ".$n++."\n";

# get() function Test: 
$a = zeroes 5,6;

$o = $rng->get(10,10); $rng->get($a);

print "ok ".$n++."\n";

# get_int() function Test: 
$a = zeroes 5,6; $max=100;

$o = $rng->get(10,10); $rng->get($a);

print "ok ".$n++."\n";

# ran_gaussian() function Test: 
$o = $rng->ran_gaussian($sigma,10,10);

$rng->ran_gaussian($sigma,$a);


print "ok ".$n++."\n";

# $rng->ran_gaussian_var() function Test: 
$sigma_pdl = rvals zeroes 11,11; $o = $rng->ran_gaussian_var($sigma_pdl);


print "ok ".$n++."\n";

# ran_additive_gaussian() function Test: 
$rng->ran_additive_gaussian(1,$image);

print "ok ".$n++."\n";

# ran_additive_poisson() function Test: 
$rng->ran_additive_poisson(1,$image);

print "ok ".$n++."\n";

# ran_feed_poisson() function Test: 
$rng->ran_feed_poisson($image);

print "ok ".$n++."\n";

# ran_bivariate_gaussian() function Test: 
$o = $rng->ran_bivariate_gaussian(1,2,0.5,1000);

print "ok ".$n++."\n";

# ran_dir() function Test: 
$o = $rng->ran_dir($ndim,$n);

print "ok ".$n++."\n";

# ran_discrete_preproc() function Test: 
$prob = pdl [0.1,0.3,0.6];

$discrete_dist_handle = $rng->ran_discrete_preproc($prob);

$o = $rng->ran_discrete($discrete_dist_handle,100);

print "ok ".$n++."\n";

# ran_discrete() function Test: 
$prob = pdl [0.1,0.3,0.6];

$discrete_dist_handle = $rng->ran_discrete_preproc($prob);

$o = $rng->ran_discrete($discrete_dist_handle,100);

print "ok ".$n++."\n";

