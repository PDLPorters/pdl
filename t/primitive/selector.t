#! perl

use strict;
use warnings;
use Test2::V0 '!float';

use PDL::LiteF;

use Test::Lib;
use My::Test::Primitive;

subtest 'where' => sub {

    subtest 'where' => sub {
        my $y = PDL->pdl( 4, 3, 1, 0, 0, 0, 0, 5, 2, 0, 3, 6 );
        my $c = ( $y->xvals ) + 10;
        ok( tapprox( $y->where( $y > 4 ), PDL->pdl( 5, 6 ) ), ">" );
        ok( tapprox( $c->where($y), PDL->pdl( 10, 11, 12, 17, 18, 20, 21 ) ),
            "mask" );
    };

    subtest 'where_both' => sub {
        my $y = sequence(10) + 2;
        my ( $big, $small ) = where_both( $y, $y > 5 );
        $big += 2, $small -= 1;
        ok tapprox( $big,   pdl('[8 9 10 11 12 13]') ), 'big + 2 is right';
        ok tapprox( $small, pdl('[1 2 3 4]') ),         'small - 2 is right';
        ok tapprox( $y,     pdl('[1 2 3 4 8 9 10 11 12 13]') ),
          'dataflow affected orig';
    };

    subtest 'whereND' => sub {

        is( [ zeroes( 2, 3, 1 )->whereND( pdl '0 0' )->dims ], [ 0, 3, 1 ] );

        is( [ zeroes( 2, 0 )->whereND( pdl '1 1' )->dims ], [ 2, 0 ] );

        subtest '1D' => sub {
            my $x = sequence( 4, 3, 2 );
            my $y = pdl( 0, 1, 1, 0 );
            my $c = whereND( $x, $y );
            is( [ $c->dims ], [ 2, 3, 2 ] );
            ok tapprox(
                $c, pdl q[[[1 2] [5 6] [9 10]] [[13 14] [17 18] [21 22]]]
              ),
              "[4]";
        };

        subtest 'nD' => sub {
            my $x = sequence( 4, 3, 2 );
            my $y = pdl q[ 0 0 1 1 ; 0 1 0 0 ; 1 0 0 0 ];

            my $c = whereND( $x, $y );
            is( [ $c->dims ], [ 4, 2 ] );
            ok tapprox( $c, pdl q[ 2  3  5  8 ; 14 15 17 20 ] ), "[4,3]";
        };

        subtest 'vs where' => sub {
            my $x = sequence( 4, 3, 2 );
            my $y = ( random($x) < 0.3 );
            my $c = whereND( $x, $y );
            ok tapprox( $c->squeeze, where( $x, $y ) ), "vs where";
        };

        subtest 'lvalue' => sub {

            # Make sure whereND functions as an lvalue:
            my $x = sequence( 4, 3 );
            my $y = pdl( 0, 1, 1, 1 );
            ok( lives { $x->whereND($y) *= -1 }, 'lvalue multiply' );
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
            ok( tapprox( $y->which, PDL->pdl( 0, 1, 2, 7, 8, 10, 11 ) ),
                "heterogenous values" );
        };
        ok zeroes(3)->which->isempty, 'all zeroes returns empty';

        # Test bad handling in selector
        subtest 'bad value' => sub {
            my $y = xvals(3);
            ok( tapprox( $y->which, PDL->pdl( 1, 2 ) ), "only good" );
            setbadat $y, 1;
            ok( tapprox( $y->which, PDL->pdl( [2] ) ), "good & bad" );
            setbadat $y, 0;
            setbadat $y, 2;
            is( $y->which->nelem, 0, "only bad" );
        };
    };

    subtest 'which_both' => sub {
        my $which_both_test = pdl( 1, 4, -2, 0, 5, 0, 1 );
        my ( $nonzero, $zero ) = which_both($which_both_test);
        ok tapprox( $nonzero, pdl( 0, 1, 2, 4, 6 ) ), 'nonzero indices';
        ok tapprox( $zero, pdl( 3, 5 ) ), 'zero indices';

    };

    subtest 'whichND' => sub {

        subtest 'Nontrivial case gives correct coordinates' => sub {
            my $r = xvals( 10, 10 ) + 10 * yvals( 10, 10 );
            my $x = whichND( $r % 12 == 0 );

            is(
                $x->unpdl,
                [
                    [ 0, 0 ], [ 2, 1 ], [ 4, 2 ], [ 6, 3 ],
                    [ 8, 4 ], [ 0, 6 ], [ 2, 7 ], [ 4, 8 ],
                    [ 6, 9 ]
                ]
            );
            is $x->type, 'indx', 'returns indx-type';
        };

        subtest 'Empty case gives matching Empty' => sub {
            my $r = xvals( 10, 10 ) + 10 * yvals( 10, 10 );
            my $x = whichND( $r * 0 );
            is $x->nelem, 0, "whichND( 0*\$r ) gives an Empty PDL";
            is( [ $x->dims ], [ 2, 0 ], "whichND( 0*\$r ) is 2x0" );
            is $x->type, 'indx', "whichND( 0*\$r) type is indx";
        };

        subtest 'Scalar PDLs are treated as 1-PDLs' => sub {
            my $x = whichND( pdl(5) );
            is $x->nelem, 1,     "whichND scalar PDL";
            is $x,        0,     "whichND scalar PDL";
            is $x->type, 'indx', "returns indx ndarray for scalar ndarray mask";
        };

        subtest 'Scalar empty case returns a 1-D vector of size 0' => sub {
            my $x = whichND( pdl(0) );
            is $x->nelem,    0,   "whichND of 0 scalar is empty";
            is [ $x->dims ], [0], "whichND of 0 scalar: return 0 dim size is 0";
            is $x->type, 'indx',
              "returns indx-type ndarray for scalar empty case";
        };

        subtest 'Empty case returns Empty' => sub {
            my $y = whichND( which( pdl(0) ) );
            is $y->nelem, 0,      "whichND of Empty mask";
            is $y->type,  'indx', "returns indx-type ndarray for empty case";
        };

        subtest 'whichND(Empty[2x0x2]) should return Empty[3x0]' => sub {
            my $y = whichND( zeroes( 2, 0, 2 ) );
            is [ $y->dims ], [ 3, 0 ];
        };

        subtest 'regression' => sub {
            my $r = zeroes( 7, 7 );
            $r->set( 3, 4, 1 );
            is( $r->whichND->unpdl, [ [ 3, 4 ] ], 'was failing on 32-bit' );
        };

        subtest 'torture test' => sub {
            my $a1 = PDL->sequence( 10, 10, 3, 4 );
            my ( $x, $y, $z, $w ) = whichND( $a1 == 203 )->mv( 0, -1 )->dog;
            ok( $a1->at( $x->list, $y->list, $z->list, $w->list ) == 203,
                "whichND" );
        };
    };

};

subtest 'uniq' => sub {
    ok tapprox( sequence(4)->uniq,    sequence(4) ), 'heterogeneous';
    ok tapprox( ones(4)->uniq,        ones(1) ),     'repeated homogenous';
    ok tapprox( empty()->uniq,        empty() ),     'empty';
    ok tapprox( pdl( [ [1] ] )->uniq, ones(1) ),
      '2-deep uniq flattens';    # Data::Frame relies
};

subtest 'uniqind' => sub {

    my $x = pdl( [ 0, 1, 2, 2, 0, 1 ] );
    my $y = $x->uniqind;
    is( $y->unpdl, [ 0, 1, 3 ] );
    is $y->ndims, 1, "uniqind";

    subtest 'SF bug 3076570' => sub {
        my $y = pdl( 1, 1, 1, 1, 1 )->uniqind;    # SF bug 3076570
        ok( !$y->isempty );
        ok all( $y == pdl( [0] ) ), 'uniqind';
        is $y->ndims, 1, 'ndims';
    };

};

done_testing;
