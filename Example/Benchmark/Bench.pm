# Old results: approx. 1.91_03: 34 secs (512, 10 iter)
# With simply folded-out threading: 3.4 secs (10fold!)

# For 512,30iter:

package PDL::Bench;

use vars qw(@ISA @EXPORT $AUTOLOAD);

require Exporter;
require DynaLoader;

@ISA = qw(Exporter DynaLoader);
# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.
@EXPORT = qw(
	do_benchmark
);

bootstrap PDL::Bench;

use Benchmark;

sub do_benchmark {
	$size = 512;
	$niter = 80;
	$piddle = (PDL->zeroes($size,$size));
	$dref = ${$piddle->get_dataref()};
	timethese($niter, {
#		'With double piddle' => 'for($i=0; $i<100; $i++) {$piddle++}',
		'With double piddle' => '$piddle++;',
		'C using ++' => 'c_use_pp($dref)',
		'C using foo = bar + baz' => 'c_use_add($dref,$dref,$dref)',
		'C using incrs and foo = bar + baz' => 'c_use_add_incr($dref,$dref,$dref,1,1,1)'
	});
}

1;
