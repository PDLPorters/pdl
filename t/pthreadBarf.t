
# These tests check for proper deferred handling of barf and warn messages when pthreading.
#   

use PDL::LiteF;
use PDL::Image2D;

use strict;


if (PDL::Core::pthreads_enabled) {

	print "1..2\n";

	## Check Handling of barf messages when pthreading ###

	# These statements will cause pthread to happen in two pthreads
	set_autopthread_targ(2);
	set_autopthread_size(0);

	# Because of the duplicate 8's interpolates barf (in the PPcode) will get
	#  called. This should not cause a segfault
	my $x = float( [1, 2, 3, 4, 5, 8, 9, 10], [1, 2, 3, 4, 5, 8, 8, 8] );
	my $y = ($x * 3) * ($x - 2);

	# Setup to catch warning messages
	local $SIG{__WARN__} = sub { die $_[0] }; 
	eval{
	
		my ( $ans, $err ) = interpolate(8.5, $x, $y );
	};
	if(  $@ ){
		# Error message should  be "identical abscissas"
		unless( $@ =~ /identical abscissas/ ){
			print "not ";
		}
		print "ok 1\n";
	}
	else{
		# Test didn't die. It should have
		print "not ok 1\n";
	}
	
	
	
	## Now Check Warning Messages with pthreading ###
	
	# Create an array of 2 bogus polygon indexes (bogus due to negative indexes)
	#  Thes will make polyfill emit a warning message.

	# Single polygon
	my $poly = pdl([-1,1],
                	[0,0]
		       );
	$poly = $poly->reorder(1,0);

	# make second polygon have same indexes
	my $poly2 = $poly->copy;
	$poly = cat $poly, $poly2;

	my $mask = zeroes(5,5);


	#kill 'INT',$$;
	# Because of the negative indexes, a warning message
	#   will be printed, which will cause segfault wheen pthreaded, if messages not deferred
	#    properly
	
	# Setup to catch warning messages
	local $SIG{__WARN__} = sub { die $_[0] }; 



	eval{
		polyfill($mask, $poly, 1);
	};
	if(  $@ ){
		# Error message should  be "errors during polygonfilling"
		unless( $@ =~ /errors during polygonfilling/ ){
			print "not ";
		}
		print "ok 2\n";
	}
	else{
		# Test didn't die. It should have
		print "not ok 2\n";
	}
	
} else {
  print "1..1\n";
  print "ok 1\n";
}
	

	


