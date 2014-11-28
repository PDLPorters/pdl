#!perl

use strict;
use warnings;

use Test::More;

use PDL::LiteF;

use PDL::Primitive;


# Some of these tests are based upon those in Chapter 5 of Programming
# Pearls, by J. Bentley

# choose a non-factor of two odd number for the length
my $N = 723;

my $ones = ones( $N );
my $idx  = sequence( $N );
my $x    = $idx * 10;

# create ordered duplicates so can test insertion points. This creates
# 7 sequential duplicates of the values 0-99
my $ndup = 7;
my $xdup = double long sequence( $ndup * 100 ) / $ndup;

# get insertion points and values
my ( $xdup_idx_insert_left, $xdup_idx_insert_right, $xdup_values ) = do {

    my ( $counts, $values ) = do { my @q = $xdup->rle; where( @q, $q[0] > 0 ) };

    ( $counts->cumusumover - $counts->at( 0 ), $counts->cumusumover, $values );

};

# The tests are table driven, with appropriate inputs and outputs for
# forward and reverse sorted arrays.  The tests sort the input array
# against itself, so we have a very good idea of which indices should
# be returned.  Most of the tests use that.  There are also specific
# tests for the endpoints as specified in the documentation, which
# may be easier for humans to parse and validate.

my %search = (

    vsearch => {

        all_the_same_element => $N - 1,    # finds right-most element

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx,
            nequal_m => $idx,
            nequal_p =>
              do { my $t = $idx + 1; $t->set( -1, $t->at( -1 ) - 1 ); $t },
            xdup => {
                set    => $xdup,
                idx    => $xdup_idx_insert_left,
                values => $xdup_values,
            },
	    #<<< noperltidy
            docs => [
                '          V <= xs[0] : i = 0                      ' => [ (  0, -1, 0 ),
									  (  0,  0, 0 ),
									],
                'xs[0]  < V <= xs[-1] : i s.t. xs[i-1] < V <= xs[i]' => [ (  0,  1,  1 ),
									  (  1,  0,  1 ),
									  ( -1,  0, $N-1 ),
									],
                'xs[-1] < V           : i = $xs->nelem -1          ' => [ ( -1,  0, $N-1 ),
									  ( -1,  1, $N-1 ),
									],
            ],
	   #>>> noperltidy
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx,
            nequal_m => $idx,
            nequal_p => do { my $t = $idx - 1; $t->set( 0, 0 ); $t },
            xdup     => {
                set => $xdup->slice( [ -1, 0 ] ),
                idx => $xdup->nelem - 1 - $xdup_idx_insert_left,
                values => $xdup_values,
            },
	    #<<< noperltidy
            docs => [
		'          V > xs[0]  : i = 0                      ' => [(0,  1, 0) ],
		'xs[0]  >= V > xs[-1] : i s.t. xs[i] >= V > xs[i+1]' => [(0,  0, 0),
									 (0, -1, 0),
									 (1,  0, 1),
									],
		'xs[-1] >= V          : i = $xs->nelem - 1         ' => [(-1,  0, $N-1),
									 (-1, -1, $N-1),
									],
            ],
	   #>>> noperltidy

       }

    },

    vsearch_insert_leftmost => {

        all_the_same_element => 0,

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx,
            nequal_m => $idx,
            nequal_p => $idx + 1,
            xdup     => {
                set    => $xdup,
                idx    => $xdup_idx_insert_left,
                values => $xdup_values,
            },
	    #<<< noperltidy
	    docs => [
		'         V <= xs[0]  : i = 0                      ' => [ ( 0, -1, 0 ),
									  ( 0,  0,  0)
									],
		'xs[0]  < V <= xs[-1] : i s.t. xs[i-1] < V <= xs[i]' => [ ( 0, 1, 1 ),
									  ( 1, 0, 1 ),
									  ( -1, 0, $N-1 ),
									],
		'xs[-1] < V           : i = $xs->nelem             ' => [
									 ( -1, 1, $N ),
									],

	    ],
	   #>>> noperltidy

        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx,
            nequal_m => $idx,
            nequal_p => $idx - 1,
            xdup     => {
                set => $xdup->mslice( [ -1, 0 ] ),
                idx => $xdup->nelem - 1 - $xdup_idx_insert_left,
                values => $xdup_values,
            },

	    #<<< noperltidy
           docs => [
	       '          V >  xs[0]  : i = -1                     ' => [ ( 0,   1, -1 ), ],
	       'xs[0]  >= V >= xs[-1] : i s.t. xs[i] >= V > xs[i+1]' => [ ( 0,   0,  0 ),
									  ( 0,  -1,  0 ),
									],
	       'xs[-1] >= V           : i = $xs->nelem -1          ' => [ ( -1,  0, $N-1 ),
									  ( -1, -1, $N-1 ),
									],

           ],
	   #>>> noperltidy

        },
    },

    vsearch_insert_rightmost => {

        all_the_same_element => $N,

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx + 1,
            nequal_m => $idx,
            nequal_p => $idx + 1,
            xdup     => {
                set    => $xdup,
                idx    => $xdup_idx_insert_right,
                values => $xdup_values,
                idx_offset => -1,   # returns index of element *after* the value
            },
	    #<<< noperltidy
	    docs => [
		'          V < xs[0]  : i = 0                      ' => [ ( 0, -1, 0 ) ],
		'xs[0]  <= V < xs[-1] : i s.t. xs[i-1] <= V < xs[i]' => [ ( 0, 0, 1 ),
									  ( 0, 1, 1 ),
									  ( 1, 0, 2 ),
									],
		'xs[-1] <= V          : i = $xs->nelem             ' => [ ( -1, 0, $N ),
									  ( -1, 1, $N ),
									],
            ],
	   #>>> noperltidy
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx - 1,
            nequal_m => $idx,
            nequal_p => $idx - 1,
            xdup     => {
                set => $xdup->mslice( [ -1, 0 ] ),
                idx => $xdup->nelem - 1 - $xdup_idx_insert_right,
                values => $xdup_values,
                idx_offset => +1,   # returns index of element *after* the value
            },
	    #<<< noperltidy
	    docs => [
		'         V >= xs[0]  : i = -1                     ' => [ ( 0,   1, -1 ),
									  ( 0,   0, -1 ),
									],
		'xs[0]  > V >= xs[-1] : i s.t. xs[i] >= V > xs[i+1]' => [ ( 0,  -1, 0 ),
									  ( -1,  1, $N-2 ),
									  ( -1,  0, $N-2 ),
									],
		'xs[-1] > V           : i = $xs->nelem -1          ' => [ ( -1,  -1, $N-1 ) ]
            ],
	   #>>> noperltidy
        },
    },

    vsearch_match => {

        all_the_same_element => ( $N ) >> 1,

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx,
            nequal_m => -( $idx + 1 ),
            nequal_p => -( $idx + 1 + 1 ),
            xdup     => {
                set    => $xdup,
                values => $xdup_values,
            },
	    #<<< noperltidy
	    docs => [
		'V < xs[0]  : i = -1' => [ ( 0,   -1, -1 ), ],
		'V == xs[n] : i = n' => [ ( 0,  0, 0 ),
					  ( -1, 0, $N-1 ) ],
		'xs[0] > V > xs[-1], V != xs[n] : -(i+1) s.t. xs[i] > V > xs[i+1]' => [ ( 0,   1, -( 1 + 1)  ),
											( 1,  -1, -( 1 + 1 ) ),
											( 1,   1, -( 2 + 1 ) ),
											( -1,  -1, -( $N - 1 + 1 ) ),
										      ],
		' V > xs[-1] : -($xs->nelem - 1 + 1)' => [ ( -1,   1, -( $N + 1)  ), ]
            ],
	   #>>> noperltidy
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx,
            nequal_m => -( $idx + 1 ),
            nequal_p => -( $idx + 1 - 1 ),
            xdup     => {
                set => $xdup->mslice( [ -1, 0 ] ),
                values => $xdup_values,
            },
	    #<<< noperltidy
	    docs => [
		'V > xs[0]  : i = 0' => [ ( 0,  1, 0 ), ],
		'V == xs[n] : i = n' => [ ( 0,  0, 0 ),
					  ( -1, 0, $N-1 ) ],
		'xs[0] < V < xs[-1], V != xs[n] : -(i+1) s.t. xs[i-1] > V > xs[i]' => [ ( 0,  -1, -( 0 + 1)  ),
											( 1,   1, -( 0 + 1 ) ),
											( 1,  -1, -( 1 + 1 ) ),
											( -1,  -1, -( $N - 1 + 1 ) ),
										      ],
		' xs[-1] > V: -($xs->nelem - 1 + 1)' => [ ( -1,   -1, -( $N - 1 + 1)  ), ]
            ],
	   #>>> noperltidy
        },
    },

    vsearch_bin_inclusive => {

        all_the_same_element => $N - 1,

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx,
            nequal_m => $idx - 1,
            nequal_p => $idx,
            xdup     => {
                set    => $xdup,
                idx    => $xdup_idx_insert_left + $ndup - 1,
                values => $xdup_values,
            },
	    #<<< noperltidy
	    docs => [
		'          V < xs[0]  : i = -1                     ' => [ ( 0, -1, -1 ), ],
		'xs[0]  <= V < xs[-1] : i s.t. xs[i] <= V < xs[i+1]' => [ ( 0,  0,  0 ),
									  ( 0,  1,  0 ),
									  ( 1, -1,  0 ),
									  ( 1,  0,  1 ),
									  ( -1, -1, $N-2 ),
									],
		'xs[-1] <= V          : i = $xs->nelem - 1         ' => [
									  ( -1, 0,  $N-1 ),
									  ( -1, 1,  $N-1 ),
									]
            ],
	   #>>> noperltidy
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx,
            nequal_m => $idx + 1,
            nequal_p => $idx,
            xdup     => {
                set => $xdup->mslice( [ -1, 0 ] ),
                idx => $xdup->nelem - ( 1 + $xdup_idx_insert_left + $ndup - 1 ),
                values => $xdup_values,
            },
	    #<<< noperltidy
	    docs => [
		'          V >= xs[0]  : i = 0                        ' => [ (0, 1, 0 ),
									     (0, 0, 0 )
									 ],
		'xs[0]  >  V >= xs[-1] : i s.t. xs[i+1] > V >= xs[i]' => [ ( 0, -1, 1 ),
									   ( 1,  1, 1 ),
									   ( 1,  0, 1 ),
									   ( 1, -1, 2 ),
									   ( -1, 0, $N-1 ),
									 ],
		'xs[-1] >  V           : i = $xs->nelem -1          ' => [ ( -1, -1, $N ) ],
            ],
	   #>>> noperltidy
        },
    },

    vsearch_bin_exclusive => {

        all_the_same_element => -1,

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx - 1,
            nequal_m => $idx - 1,
            nequal_p => $idx,
            xdup     => {
                set        => $xdup,
                idx        => $xdup_idx_insert_left - 1,
                values     => $xdup_values,
                idx_offset => 1,
            },
	    #<<< noperltidy
	    docs => [
		'          V <= xs[0]  : i = -1                     ' => [ ( 0, -1, -1 ),
									   ( 0,  0, -1 ),
									 ],
		'xs[0]  <  V <= xs[-1] : i s.t. xs[i] < V <= xs[i+1]' => [ ( 0,  1, 0 ),
									   ( 1, -1, 0 ),
									   ( 1,  0, 0 ),
									   ( 1,  1, 1 ),
									   ( -1, -1, $N-2 ),
									   ( -1, 0, $N-2 ),
									],
		'xs[-1] <  V           : i = $xs->nelem - 1         ' => [
									  ( -1, 1, $N-1 ),
									 ],
            ],
	    #>>> noperltidy
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx + 1,
            nequal_m => $idx + 1,
            nequal_p => $idx,
            xdup     => {
                set => $xdup->mslice( [ -1, 0 ] ),
                idx => $xdup->nelem - ( 1 + $xdup_idx_insert_left - 1 ),
                values     => $xdup_values,
                idx_offset => -1,
            },
	    #<<< noperltidy
	    docs => [
		'          V >  xs[0]  : i = 0                      ' => [ ( 0,  1, 0 ), ],
		'xs[0]  >  V >  xs[-1] : i s.t. xs[i-1] >= V > xs[i]' => [ ( 0,  0, 1 ),
									   ( 0, -1, 1 ),
									   ( -1, 1, $N-1 ),
									 ],
		'xs[-1] >= V           : i = $xs->nelem -1          ' => [ ( -1, 0, $N ),
									   ( -1, -1, $N ),
									 ],
	    ],
	    #>>> noperltidy
        },
    },

);

for my $fname (
    keys %search
  )
{

    my $data   = $search{$fname};

    subtest $fname => sub {

        my ( $got, $exp );

        my $func = do {
            no strict 'refs';
            \&$fname;
        };

	#<<< no perltidy
        for my $sort_direction ( qw[ forward reverse ] ) {

            subtest $sort_direction => sub {

		my $so = $data->{$sort_direction}
		  or plan( skip_all => "not testing $sort_direction!\n" );

                ok(
                    all(
                        ( $got = $func->( $so->{x}, $so->{x} ) )
			==
			( $exp = $so->{equal} )
                    ),
                    'equal elements'
                ) or diag "got     : $got\nexpected: $exp\n";

                ok(
                    all(
                        ( $got = $func->( $so->{x} - 5, $so->{x} ) )
                        ==
			( $exp = $so->{nequal_m} )
                    ),
                    'non-equal elements x[i] < xs[i] (check lower bound)'
                ) or diag "got     : $got\nexpected: $exp\n";

                ok(
                    all(
                        ( $got = $func->( $so->{x} + 5, $so->{x} ) )
                        ==
			( $exp = $so->{nequal_p} )
                    ),
                    'non-equal elements x[i] > xs[i] (check upper bound)'
                ) or diag "got     : $got\nexpected: $exp\n";


		# duplicate testing.

		# check for values. note that the rightmost routine returns
		# the index of the element *after* the last duplicate
		# value, so we need an offset
		ok(
		    all(
			( $got = $so->{xdup}{set}->index( $func ->( $so->{xdup}{values}, $so->{xdup}{set} )
							                 + ($so->{xdup}{idx_offset} || 0) ) )
			==
			( $exp = $so->{xdup}{values} )
		    ),
		    'duplicates values'
		) or diag "got     : $got\nexpected: $exp\n";

		# if there are guarantees about which duplicates are returned, test it
		if ( exists $so->{xdup}{idx} ) {

		    ok(
			all(
			    ( $got = $func ->( $so->{xdup}{values}, $so->{xdup}{set} ) )
			    ==
			    ( $exp = $so->{xdup}{idx} )
			),
			'duplicate indices'
		    ) or diag "got     : $got\nexpected: $exp\n";

		}

		if ( exists $so->{docs} ) {

		    while( my ($label, $inputs ) = splice( @{$so->{docs}}, 0, 2 )  ) {

			while( @$inputs ) {

			    my ( $idx, $offset, $exp ) = splice( $inputs, 0, 3 );
			    my $value = $so->{x}->at($idx) + $offset;

			    is ( $got = ( $func->( $value, $so->{x} )->sclr), $exp, "$label: ($idx, $offset)" );

			}
		    }
		}


            };
        }

        ok(
            all(
                ( $got = $func->( $ones, $ones ) )
                ==
                ( $exp = $data->{all_the_same_element} )
            ),
            'all the same element'
        ) or diag "got     : $got\nexpected: $exp\n";

	#>>> no perltidy

    };

}


done_testing;
