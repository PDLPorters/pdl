use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL::LiteF;
use Test::PDL;

subtest 'where' => sub {
    subtest 'where' => sub {
        my $y = PDL->pdl( 4, 3, 1, 0, 0, 0, 0, 5, 2, 0, 3, 6 );
        my $c = ( $y->xvals ) + 10;
        is_pdl $y->where( $y > 4 ), PDL->pdl( 5, 6 ), ">";
        is_pdl $c->where($y), PDL->pdl( 10, 11, 12, 17, 18, 20, 21 ), "mask";
    };

    subtest 'where_both' => sub {
        my $y = sequence(10) + 2;
        my ( $big, $small ) = where_both( $y, $y > 5 );
        $big += 2, $small -= 1;
        is_pdl $big,   pdl('[8 9 10 11 12 13]'), 'big + 2 is right';
        is_pdl $small, pdl('[1 2 3 4]'),         'small - 2 is right';
        is_pdl $y,     pdl('[1 2 3 4 8 9 10 11 12 13]'),
          'dataflow affected orig';
    };

    subtest 'whereND_both' => sub {
      my ( $t, $f ) = whereND_both(sequence(2,2,2), pdl(0,1));
      is_pdl $t, pdl('[1;3] [5;7]'), 'nonzero vals';
      is_pdl $f, pdl('[0;2] [4;6]'), 'zero vals';
    };

    subtest 'whereND' => sub {
        is_deeply( [ zeroes( 2, 3, 1 )->whereND( pdl '0 0' )->dims ], [ 0, 3, 1 ] );

        is_deeply( [ zeroes( 2, 0 )->whereND( pdl '1 1' )->dims ], [ 2, 0 ] );

        subtest '1D' => sub {
            my $x = sequence( 4, 3, 2 );
            my $y = pdl( 0, 1, 1, 0 );
            my $c = whereND( $x, $y );
            is_pdl $c, pdl(q[[[1 2] [5 6] [9 10]] [[13 14] [17 18] [21 22]]]);
        };

        subtest 'nD' => sub {
            my $x = sequence( 4, 3, 2 );
            my $y = pdl q[ 0 0 1 1 ; 0 1 0 0 ; 1 0 0 0 ];
            my $c = whereND( $x, $y );
            is_pdl $c, pdl(q[2  3  5  8 ; 14 15 17 20]), "[4,3]";
        };

        subtest 'vs where' => sub {
            my $x = sequence( 2, 2, 2 );
            for my $y (
              pdl('[[[0 0][0 0]][[0 0][0 0]]]'),
              pdl('[[[0 0][0 0]][[0 0][0 1]]]'),
              pdl('[[[0 0][0 0]][[0 1][0 1]]]'),
            ) {
                my $c = whereND( $x, $y );
                my $where = where( $x, $y );
                is_pdl $c->flat, $where, "vs where";
            }
        };

        subtest 'lvalue' => sub {

            # Make sure whereND functions as an lvalue:
            my $x = sequence( 4, 3 );
            my $y = pdl( 0, 1, 1, 1 );
            lives_ok { $x->whereND($y) *= -1 } 'lvalue multiply';
            ok( all( $x->slice("1:-1") < 0 ),    'works' );
        };

        subtest 'sf.net bug 3415115' => sub {
            # sf.net bug #3415115, whereND fails to handle all zero mask case
            my $x = sequence( 4, 3 );
            my $y = zeros(4);
            my $c = whereND( $x, $y );
            ok( $c->isempty, 'all-zeros mask' );
        };
    };
};

subtest 'which' => sub {
  subtest 'which' => sub {
    subtest 'heterogenous values' => sub {
      my $y = PDL->pdl( 4, 3, 1, 0, 0, 0, 0, 5, 2, 0, 3, 6 );
      is_pdl $y->which, PDL->pdl(indx, 0, 1, 2, 7, 8, 10, 11),
        "heterogenous values";
    };
    ok zeroes(3)->which->isempty, 'all zeroes returns empty';

    # Test bad handling in selector
    subtest 'bad value' => sub {
      my $y = xvals(3);
      is_pdl $y->which, indx( 1, 2 ), "only good";
      setbadat $y, 1;
      is_pdl $y->which, indx( [2] ), "good & bad";
      setbadat $y, 0;
      setbadat $y, 2;
      is( $y->which->nelem, 0, "only bad" );
    };

    my $empty = zeroes(indx, 0);
    is_pdl which(ones(4) > 2), $empty, 'which 1D -> 1D, empty, type indx';
    is_pdl which(ones(4,3) > 2), $empty, 'which 2D -> 1D';
  };

  subtest 'which_both' => sub {
    my $which_both_test = pdl( 1, 4, -2, 0, 5, 0, 1 );
    my ( $nonzero, $zero ) = which_both($which_both_test);
    is_pdl $nonzero, pdl(indx, 0, 1, 2, 4, 6 ), 'nonzero indices';
    is_pdl $zero, pdl(indx, 3, 5), 'zero indices';
  };

  subtest 'whichND_both' => sub {
    my ( $nonzero, $zero ) = whichND_both(PDL::MatrixOps::identity(2));
    is_pdl $nonzero, indx('0 0; 1 1'), 'nonzero indices';
    is_pdl $zero, indx('1 0; 0 1'), 'zero indices';
  };

  subtest 'whichover' => sub {
    my $a = pdl q[3 4 6 3 2 3 5 6 1 7];
    my $b = $a->uniq;
    my $c = +($a->dummy(0) == $b)->transpose;
    my $expected = pdl q[
     [ 8 -1 -1 -1 -1 -1 -1 -1 -1 -1]
     [ 4 -1 -1 -1 -1 -1 -1 -1 -1 -1]
     [ 0  3  5 -1 -1 -1 -1 -1 -1 -1]
     [ 1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
     [ 6 -1 -1 -1 -1 -1 -1 -1 -1 -1]
     [ 2  7 -1 -1 -1 -1 -1 -1 -1 -1]
     [ 9 -1 -1 -1 -1 -1 -1 -1 -1 -1]
    ];
    my $got = $c->whichover;
    is_pdl $got, $expected, 'whichover';
    $c->inplace->whichover;
    is_pdl $c, $expected, 'whichover inplace';
  };

  subtest 'whichND' => sub {
    subtest 'Nontrivial case gives correct coordinates' => sub {
      my $r = xvals( 10, 10 ) + 10 * yvals( 10, 10 );
      my $x = whichND( $r % 12 == 0 );

      my $got;
      is_deeply(
        $got = $x->unpdl,
        [
          [ 0, 0 ], [ 2, 1 ], [ 4, 2 ], [ 6, 3 ],
          [ 8, 4 ], [ 0, 6 ], [ 2, 7 ], [ 4, 8 ],
          [ 6, 9 ]
        ]
      ) or diag 'got: ', explain $got;
      is $x->type, 'indx', 'returns indx-type';
    };

    subtest 'Empty case gives matching Empty' => sub {
      my $r = xvals( 10, 10 ) + 10 * yvals( 10, 10 );
      my $x = whichND( $r * 0 );
      is $x->nelem, 0, "whichND( 0*\$r ) gives an Empty PDL";
      is_deeply( [ $x->dims ], [ 2, 0 ], "whichND( 0*\$r ) is 2x0" );
      is $x->type, 'indx', "whichND( 0*\$r) type is indx";
    };

    subtest 'Scalar PDLs are treated as 1-PDLs' => sub {
      my $x = whichND( pdl(5) );
      is $x->nelem, 1,   "whichND scalar PDL";
      is $x,    0,   "whichND scalar PDL";
      is $x->type, 'indx', "returns indx ndarray for scalar ndarray mask";
    };

    subtest 'Scalar empty case returns a 1-D vector of size 0' => sub {
      my $x = whichND( pdl(0) );
      is $x->nelem,  0,   "whichND of 0 scalar is empty";
      is_deeply [ $x->dims ], [0], "whichND of 0 scalar: return 0 dim size is 0";
      is $x->type, 'indx',
        "returns indx-type ndarray for scalar empty case";
    };

    subtest 'Empty case returns Empty' => sub {
      my $y = whichND( which( pdl(0) ) );
      is $y->nelem, 0,    "whichND of Empty mask";
      is $y->type,  'indx', "returns indx-type ndarray for empty case";
    };

    subtest 'whichND(Empty[2x0x2]) should return Empty[3x0]' => sub {
      my $y = whichND( zeroes( 2, 0, 2 ) );
      is_deeply [ $y->dims ], [ 3, 0 ];
    };

    subtest 'regression' => sub {
      my $r = zeroes( 7, 7 );
      $r->set( 3, 4, 1 );
      is_deeply( $r->whichND->unpdl, [ [ 3, 4 ] ], 'was failing on 32-bit' );
    };

    subtest 'torture test' => sub {
      is_pdl scalar whichND(PDL->sequence(10,10,3,4) == 203), pdl(indx,[[3,0,2,0]]);
    };
  };
};

subtest 'uniq' => sub {
    is_pdl sequence(4)->uniq,    sequence(4), 'heterogeneous';
    is_pdl ones(4)->uniq,        ones(1),     'repeated homogenous';
    is_pdl empty()->uniq,        empty(),     'empty';
    is_pdl pdl( [ [1] ] )->uniq, ones(1),
      '2-deep uniq flattens';    # Data::Frame relies
};

subtest 'uniqind' => sub {

    my $x = pdl( [ 0, 1, 2, 2, 0, 1 ] );
    my $y = $x->uniqind;
    is_deeply( $y->unpdl, [ 0, 1, 3 ] );
    is $y->ndims, 1, "uniqind";

    subtest 'SF bug 3076570' => sub {
        my $y = pdl( 1, 1, 1, 1, 1 )->uniqind;    # SF bug 3076570
        ok( !$y->isempty );
        ok all( $y == pdl( [0] ) ), 'uniqind';
        is $y->ndims, 1, 'ndims';
    };

};

done_testing;
