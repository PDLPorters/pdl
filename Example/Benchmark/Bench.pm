# Old results: approx. 1.91_03: 34 secs (512, 10 iter)
# With simply folded-out threading: 3.4 secs (10fold!)

# For 512,30iter:

package PDL::Bench;

use strict;
use warnings;
require Exporter;
require DynaLoader;

our @ISA = qw(Exporter DynaLoader);
our @EXPORT = qw(
	do_benchmark
);

bootstrap PDL::Bench;

use Benchmark;

sub do_benchmark {
	$size = 512;
	$niter = 10000;
	$ndarray = (PDL->zeroes($size,$size));
	$dref = ${$ndarray->get_dataref()};
	timethese($niter, {
#		'With double ndarray' => 'for($i=0; $i<100; $i++) {$ndarray++}',
		'With double ndarray' => '$ndarray++;',
		'C using ++' => 'c_use_pp($dref)',
		'C using foo = bar + baz' => 'c_use_add($dref,$dref,$dref)',
		'C using incrs and foo = bar + baz' => 'c_use_add_incr($dref,$dref,$dref,1,1,1)'
	});
}

1;
