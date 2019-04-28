use strict;
use warnings;
use Test::More;
use PDL;
use PDL::Graphics::Limits;

#####################################################################
# test user override limits.  only need to worry about how they affect
# the bounding algorithms.  they shouldn't be affected by errors,
# so don't toss them in the test.

my $x1 = pdl( 1, 2, 3 );
my $x2 = pdl( 2, 3, 4 );

my $y1 = pdl( 10, 8, 3 );
my $y2 = pdl( 0, 2, 4 );

my @udsets_arr  = ( [ $x1, $y1 ], 
	         [ $x2, $y2 ] );

my @udsets_hash = ( [ { x => $x1, y => $y1 },
	           { x => $x2, y => $y2 } ] );

#############################################################3
#### Bounds => MinMax

my %attr = ( Bounds => 'MinMax', Clean => 'None' );

# array based

my @tests = ( 
	  [ 'none 0', 
	    [ ],
	    [ 1, 4, 0, 10 ]
	  ],

	  [ 'none 1', 
	    [ [ ] ],
	    [ 1, 4, 0, 10 ]
	  ],

	  [ 'none 2', 
	    [ [ ], [ ] ],
	    [ 1, 4, 0, 10 ]
	  ],

	  [ 'xmin', 
	    [ [ -20 ] ],
	    [ -20, 4, 0, 10 ]
	  ],

	  [ 'xmax',
	    [ [ undef, 20 ] ],
	    [ 1, 20, 0, 10 ] 
	  ],

	  [ 'xmin+xmax', 
	    [ [ -20, 20 ] ],
	    [ -20, 20, 0, 10 ]
	  ],

	  [ 'ymin', 
	    [ [ ], [ -20 ] ],
	    [ 1, 4, -20, 10 ]
	  ],

	  [ 'ymax',
	    [ [ ], [ undef, 20 ] ],
	    [ 1, 4, 0, 20 ] 
	  ],

	  [ 'ymin+ymax', 
	    [ [], [ -20, 20 ] ],
	    [ 1, 4, -20, 20 ]
	  ],

	  [ 'xmin+xmax+ymin+ymax', 
	    [ [-20, 10], [ -20, 20 ] ],
	    [ -20, 10, -20, 20 ]
	  ],

	);

foreach my $test ( @tests ) {
  my ( $msg, $limits, $exp ) = @$test;
  my $range = pdl limits( @udsets_arr, { %attr, Limits => $limits } );
  ok( all(approx $range, pdl $exp), "array: $msg" );
}

@tests = ( 
	  [ 'none', 
	    { },
	    [ 1, 4, 0, 10 ]
	  ],

	  [ 'xmin', 
	    { x => { min =>  -20 }  },
	    [ -20, 4, 0, 10 ]
	  ],

	  [ 'xmax',
	    { x => { max =>  20 }  },
	    [ 1, 20, 0, 10 ] 
	  ],

	  [ 'xmin+xmax', 
	    { x => { min => -20, max => 20 }  },
	    [ -20, 20, 0, 10 ]
	  ],

	  [ 'ymin', 
	    { y => { min =>  -20 } },
	    [ 1, 4, -20, 10 ]
	  ],

	  [ 'ymax',
	    { y => { max => 20 } },
	    [ 1, 4, 0, 20 ] 
	  ],

	  [ 'ymin+ymax', 
	    { y => { min => -20, max => 20 } },
	    [ 1, 4, -20, 20 ]
	  ],

	  [ 'xmin+xmax+ymin+ymax', 
	    { x => { min => -20, max => 10 }, y => { min => -20, max => 20 } },
	    [ -20, 10, -20, 20 ]
	  ],

	);

foreach my $test ( @tests ) {
  my ( $msg, $limits, $exp ) = @$test;
  my $range = pdl limits( @udsets_hash, { %attr, Limits => $limits,  VecKeys => [ qw/ x y / ] } );
  ok( all(approx $range, pdl $exp), "hash: $msg" );
}

#############################################################
#### Bounds => ZScale

# just use arrays here; tests above suffice to ensure hashes are ok

%attr = ( Bounds => 'Zscale', Clean => 'None' );

@tests = ( 
	  [ 'none', 
	    [ ],
	    [ 1, 4, 0, 10.5 ]
	  ],

	  [ 'xmin', 
	    [ [ -20 ] ],
	    [ -20, 4, 0, 10.5 ]
	  ],

	  [ 'xmax',
	    [ [ undef, 20 ] ],
	    [ 1, 20, 0, 10.5 ] 
	  ],

	  [ 'xmin+xmax', 
	    [ [ -20, 20 ] ],
	    [ -20, 20, 0, 10.5 ]
	  ],

	  [ 'ymin', 
	    [ [ ], [ -20 ] ],
	    [ 1, 4, -20, 10.5 ]
	  ],

	  [ 'ymax',
	    [ [ ], [ undef, 20 ] ],
	    [ 1, 4, 0, 20 ] 
	  ],

	  [ 'ymin+ymax', 
	    [ [], [ -20, 20 ] ],
	    [ 1, 4, -20, 20 ]
	  ],

	  [ 'xmin+xmax+ymin+ymax', 
	    [ [-20, 10], [ -20, 20 ] ],
	    [ -20, 10, -20, 20 ]
	  ],

	);

foreach my $test ( @tests ) {
  my ( $msg, $limits, $exp ) = @$test;
  my $range = pdl limits( @udsets_arr, { %attr, Limits => $limits } );
  ok( all(approx $range, pdl $exp), "array: $msg" ) or diag "($range), [@$exp]\n";
}

done_testing;
