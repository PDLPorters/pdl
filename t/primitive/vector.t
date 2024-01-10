use strict;
use warnings;

use Test2::V0 '!float';

use PDL::LiteF;
use lib 't/lib';
use My::Test::Primitive;

subtest 'cmpvec' => sub {
    ok tapprox( pdl( 1, 2, 3 )->cmpvec( pdl( 3, 2, 1 ) ), -1 ), 'less';
    ok tapprox( pdl( 3, 2, 1 )->cmpvec( pdl( 1, 2, 3 ) ), 1 ),  'more';
    ok tapprox( pdl( 3, 2, 1 )->cmpvec( pdl( 3, 2, 1 ) ), 0 ),  'same';

    is pdl('[1 BAD]')->cmpvec( pdl( 3, 2 ) )->unpdl, [-1],    'bad before';
    is pdl('[BAD 1]')->cmpvec( pdl( 3, 2 ) )->unpdl, ['BAD'], 'bad';

    my $vdim = 4;
    my $v1   = zeroes($vdim);
    my $v2   = pdl($v1);
    $v2->set( -1, 1 );

    ok $v1->cmpvec($v2) < 0, "1d:<";
    ok $v2->cmpvec($v1) > 0, "1d:>";
    is $v1->cmpvec($v1)->sclr, 0, "1d:==";
};

subtest 'eqvec' => sub {
    ok tapprox( pdl( 3, 2, 1 )->eqvec( pdl( 1, 2, 3 ) ), 0 ), 'diff';
    ok tapprox( pdl( 3, 2, 1 )->eqvec( pdl( 3, 2, 1 ) ), 1 ), 'same';
    is pdl('[2 1 BAD]')->eqvec( pdl( 1, 3, 2 ) )->unpdl, ['BAD'], 'bad before';
    is pdl('[2 BAD 1]')->eqvec( pdl( 2, 3, 2 ) )->unpdl, ['BAD'], 'bad';
};

subtest 'uniqvec' => sub {

    is pdl( [ [ 0, 1 ], [ 2, 2 ], [ 0, 1 ] ] )->uniqvec->unpdl,
        [ [ 0, 1 ], [ 2, 2 ] ], '2x3';

    is pdl( [ [ 0, 1 ] ] )->uniqvec->unpdl, [ [ 0, 1 ] ], '1x2';

    is pdl( [ [ 0, 1, 2 ], [ 0, 1, 2 ], [ 0, 1, 2 ], ] )->uniqvec->unpdl,
      [ [ 0, 1, 2 ] ], '3x3';
};

subtest 'qsortvec' => sub {
    my $p2d = pdl( [ [ 1, 2 ], [ 3, 4 ], [ 1, 3 ], [ 1, 2 ], [ 3, 3 ] ] );
    ok tapprox( $p2d->qsortvec,
        pdl( long, [ [ 1, 2 ], [ 1, 2 ], [ 1, 3 ], [ 3, 3 ], [ 3, 4 ] ] ) ),
      "qsortvec";
    ok tapprox( $p2d->dice_axis( 1, $p2d->qsortveci ), $p2d->qsortvec ),
      "qsortveci";
};

subtest 'vsearchvec' => sub {

    my $which = pdl(
        long,
        [
            [ 0, 0 ], [ 0, 0 ], [ 0, 1 ], [ 0, 1 ],
            [ 1, 0 ], [ 1, 0 ], [ 1, 1 ], [ 1, 1 ]
        ]
    );
    my $find = $which->slice(",0:-1:2");

    ok tapprox( $find->vsearchvec($which), pdl( long, [ 0, 2, 4, 6 ] ) ),
      "match";
    ok tapprox( pdl( [ -1, -1 ] )->vsearchvec($which), 0 ),                "<<";
    ok tapprox( pdl( [ 2, 2 ] )->vsearchvec($which), $which->dim(1) - 1 ), ">>";

};

subtest 'unionvec' => sub {

    my $vtype    = long;
    my $universe = pdl( $vtype, [ [ 0, 0 ], [ 0, 1 ], [ 1, 0 ], [ 1, 1 ] ] );
    my $v1       = $universe->dice_axis( 1, pdl( [ 0, 1, 2 ] ) );
    my $v2       = $universe->dice_axis( 1, pdl( [ 1, 2, 3 ] ) );

    my ( $c, $nc ) = $v1->unionvec($v2);
    ok tapprox(
        $c,
        pdl(
            $vtype,
            [ [ 0, 0 ], [ 0, 1 ], [ 1, 0 ], [ 1, 1 ], [ 0, 0 ], [ 0, 0 ] ]
        )
      ),
      "list:c";
    is $nc, $universe->dim(1), "list:nc";
    my $cc = $v1->unionvec($v2);
    ok tapprox( $cc, $universe ), "scalar";
};

subtest 'intersectvec' => sub {

    my $vtype    = long;
    my $universe = pdl( $vtype, [ [ 0, 0 ], [ 0, 1 ], [ 1, 0 ], [ 1, 1 ] ] );
    my $v1       = $universe->dice_axis( 1, pdl( [ 0, 1, 2 ] ) );
    my $v2       = $universe->dice_axis( 1, pdl( [ 1, 2, 3 ] ) );

    my ( $c, $nc ) = $v1->intersectvec($v2);
    ok tapprox( $c, pdl( $vtype, [ [ 0, 1 ], [ 1, 0 ], [ 0, 0 ] ] ) ), "list:c";
    is $nc->sclr, 2, "list:nc";
    my $cc = $v1->intersectvec($v2);
    ok tapprox( $cc, $universe->slice(",1:2") ), "scalar";
};

subtest 'setdiffvec' => sub {

    my $vtype    = long;
    my $universe = pdl( $vtype, [ [ 0, 0 ], [ 0, 1 ], [ 1, 0 ], [ 1, 1 ] ] );
    my $v1       = $universe->dice_axis( 1, pdl( [ 0, 1, 2 ] ) );
    my $v2       = $universe->dice_axis( 1, pdl( [ 1, 2, 3 ] ) );

    my ( $c, $nc ) = $v1->setdiffvec($v2);
    ok tapprox( $c, pdl( $vtype, [ [ 0, 0 ], [ 0, 0 ], [ 0, 0 ] ] ) ), "list:c";
    is $nc, 1, "list:nc";
    my $cc = $v1->setdiffvec($v2);
    ok tapprox( $cc, pdl( $vtype, [ [ 0, 0 ] ] ) ), "scalar";

};

subtest '*_sorted' => sub {

    my $all   = sequence(20);
    my $amask = ( $all % 2 ) == 0;
    my $bmask = ( $all % 3 ) == 0;
    my $alpha = $all->where($amask);
    my $beta  = $all->where($bmask);

    ok tapprox(
        scalar( $alpha->union_sorted($beta) ),
        $all->where( $amask | $bmask )
      ),
      "union_sorted";
    ok tapprox(
        scalar( $alpha->intersect_sorted($beta) ),
        $all->where( $amask & $bmask )
      ),
      "intersect_sorted";
    ok tapprox(
        scalar( $alpha->setdiff_sorted($beta) ),
        $all->where( $amask & $bmask->not )
      ),
      "setdiff_sorted";
};

##--------------------------------------------------------------
## dim-checks and implicit broadcast dimensions
##  + see https://github.com/moocow-the-bovine/PDL-VectorValued/issues/4

subtest 'broadcast_dimensions' => sub {
    ##-- unionvec
    my $empty = zeroes( 3, 0 );
    my $uw    = pdl( [ [ -3, -2, -1 ], [ 1, 2, 3 ] ] );
    my $wx    = pdl( [ [ 1,  2,  3 ],  [ 4, 5, 6 ] ] );
    my $xy    = pdl( [ [ 4,  5,  6 ],  [ 7, 8, 9 ] ] );

    # unionvec: basic
    ok tapprox( scalar( $uw->unionvec($wx) ),
        pdl( [ [ -3, -2, -1 ], [ 1, 2, 3 ], [ 4, 5, 6 ] ] ) ),
      "unionvec - broadcast dims - uw+wx";
    ok tapprox( scalar( $uw->unionvec($xy) ),
        pdl( [ [ -3, -2, -1 ], [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ] ) ),
      "unionvec - broadcast dims - uw+xy";
    ok tapprox( scalar( $empty->unionvec($wx) ), $wx ),
      "unionvec - broadcast dims - 0+wx";
    ok tapprox( scalar( $wx->unionvec($empty) ), $wx ),
      "unionvec - broadcast dims - wx+0";
    ok tapprox( scalar( $empty->unionvec($empty) ), $empty ),
      "unionvec - broadcast dims - 0+0";

    # unionvec: broadcasting
    my $k      = 2;
    my $kempty = $empty->slice(",,*$k");
    my $kuw    = $uw->slice(",,*$k");
    my $kwx    = $wx->slice(",,*$k");
    my $kxy    = $xy->slice(",,*$k");
    ok tapprox( scalar( $kuw->unionvec($wx) ),
        pdl( [ [ -3, -2, -1 ], [ 1, 2, 3 ], [ 4, 5, 6 ] ] )->slice(",,*$k") ),
      "unionvec - broadcast dims - uw(*k)+wx";
    ok tapprox(
        scalar( $kuw->unionvec($xy) ),
        pdl( [ [ -3, -2, -1 ], [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ] )
          ->slice(",,*$k")
      ),
      "unionvec - broadcast dims - uw(*k)+xy";
    ok tapprox( scalar( $kempty->unionvec($wx) ), $kwx ),
      "unionvec - broadcast dims - 0(*k)+wx";
    ok tapprox( scalar( $kwx->unionvec($empty) ), $kwx ),
      "unionvec - broadcast dims - wx(*k)+0";
    ok tapprox( scalar( $kempty->unionvec($empty) ), $kempty ),
      "unionvec - broadcast dims - 0(*k)+0";

    ##-- intersectvec

    my $needle0 = pdl( [ [ -3, -2, -1 ] ] );
    my $needle1 = pdl( [ [ 1,  2,  3 ] ] );
    my $needles = pdl( [ [ -3, -2, -1 ], [ 1, 2, 3 ] ] );
    my $haystack =
      pdl( [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ], [ 10, 11, 12 ] ] );

    # intersectvec: basic
    ok tapprox( scalar( $needle0->intersectvec($haystack) ), $empty ),
      "intersectvec - broadcast dims - needle0&haystack";
    ok tapprox( scalar( $needle1->intersectvec($haystack) ), $needle1 ),
      "intersectvec - broadcast dims - needle1&haystack";
    ok tapprox( scalar( $needles->intersectvec($haystack) ), $needle1 ),
      "intersectvec - broadcast dims - needles&haystack";
    ok tapprox( scalar( $haystack->intersectvec($haystack) ), $haystack ),
      "intersectvec - broadcast dims - haystack&haystack";
    ok tapprox( scalar( $haystack->intersectvec($empty) ), $empty ),
      "intersectvec - broadcast dims - haystack&empty";
    ok tapprox( scalar( $empty->intersectvec($haystack) ), $empty ),
      "intersectvec - broadcast dims - empty&haystack";

    # intersectvec: broadcasting
    my $kneedle0  = $needle0->slice(",,*$k");
    my $kneedle1  = $needle1->slice(",,*$k");
    my $kneedles  = pdl( [ [ [ -3, -2, -1 ] ], [ [ 1, 2, 3 ] ] ] );
    my $khaystack = $haystack->slice(",,*$k");
    ok tapprox( scalar( $kneedle0->intersectvec($haystack) ), $kempty ),
      "intersectvec - broadcast dims - needle0(*k)&haystack";
    ok tapprox( scalar( $kneedle1->intersectvec($haystack) ), $kneedle1 ),
      "intersectvec - broadcast dims - needle1(*k)&haystack";
    ok tapprox(
        scalar( $kneedles->intersectvec($haystack) ),
        pdl( [ [ [ 0, 0, 0 ] ], [ [ 1, 2, 3 ] ] ] )
      ),
      "intersectvec - broadcast dims - needles(*k)&haystack";
    ok tapprox( scalar( $khaystack->intersectvec($haystack) ), $khaystack ),
      "intersectvec - broadcast dims - haystack(*k)&haystack";
    ok tapprox( scalar( $khaystack->intersectvec($empty) ), $kempty ),
      "intersectvec - broadcast dims - haystack(*k)&empty";
    ok tapprox( scalar( $kempty->intersectvec($haystack) ), $kempty ),
      "intersectvec - broadcast dims - empty(*k)&haystack";

    ##-- setdiffvec

    # setdiffvec: basic
    ok tapprox( scalar( $haystack->setdiffvec($needle0) ), $haystack ),
      "setdiffvec - broadcast dims - haystack-needle0";
    ok tapprox(
        scalar( $haystack->setdiffvec($needle1) ),
        $haystack->slice(",1:-1")
      ),
      "setdiffvec - broadcast dims - haystack-needle1";
    ok tapprox(
        scalar( $haystack->setdiffvec($needles) ),
        $haystack->slice(",1:-1")
      ),
      "setdiffvec - broadcast dims - haystack-needles";
    ok tapprox( scalar( $haystack->setdiffvec($haystack) ), $empty ),
      "setdiffvec - broadcast dims - haystack-haystack";
    ok tapprox( scalar( $haystack->setdiffvec($empty) ), $haystack ),
      "setdiffvec - broadcast dims - haystack-empty";
    ok tapprox( scalar( $empty->setdiffvec($haystack) ), $empty ),
      "setdiffvec - broadcast dims - empty-haystack";

    # setdiffvec: broadcasting
    ok tapprox( scalar( $khaystack->setdiffvec($needle0) ), $khaystack ),
      "setdiffvec - broadcast dims - haystack(*k)-needle0";
    ok tapprox( scalar( $khaystack->setdiffvec($needle1) ),
        $khaystack->slice(",1:-1,") ),
      "setdiffvec - broadcast dims - haystack(*k)-needle1";
    ok tapprox( scalar( $khaystack->setdiffvec($needles) ),
        $khaystack->slice(",1:-1,") ),
      "setdiffvec - broadcast dims - haystack(*k)-needles";
    ok tapprox( scalar( $khaystack->setdiffvec($haystack) ), $kempty ),
      "setdiffvec - broadcast dims - haystack(*k)-haystack";
    ok tapprox( scalar( $khaystack->setdiffvec($empty) ), $khaystack ),
      "setdiffvec - broadcast dims - haystack(*k)-empty";
    ok tapprox( scalar( $kempty->setdiffvec($haystack) ), $kempty ),
      "setdiffvec - broadcast dims - empty(*k)-haystack";
};

## intersectvec tests as suggested by ETJ/mowhawk2
##  + see https://github.com/moocow-the-bovine/PDL-VectorValued/issues/4
subtest intersect_implicit_dims => sub {

# intersectvec: from ETJ/mowhawk2 a la https://stackoverflow.com/a/71446817/3857002
    my $toto  = pdl( [ 1, 2, 3 ], [ 4, 5, 6 ] );
    my $titi  = pdl( 1, 2, 3 );
    my $notin = pdl( 7, 8, 9 );
    my ($c);

    ok tapprox( $c = intersectvec( $titi, $toto ), [ [ 1, 2, 3 ] ] ),
      'intersectvec - implicit dims - titi&toto';
    ok tapprox( $c = intersectvec( $notin, $toto ), zeroes( 3, 0 ) ),
      'intersectvec - implicit dims - notin&toto';
    ok tapprox( $c = intersectvec( $titi->dummy(1), $toto ), [ [ 1, 2, 3 ] ] ),
      'intersectvec - implicit dims - titi(*1)&toto';
    ok tapprox( $c = intersectvec( $notin->dummy(1), $toto ), zeroes( 3, 0 ) ),
      'intersectvec - implicit dims - notin(*1)&toto';

    my $needle0_in    = pdl( [ 1, 2, 3 ] );                  # 3
    my $needle0_notin = pdl( [ 9, 9, 9 ] );                  # 3
    my $needle_in     = $needle0_in->dummy(1);               # 3x1: [[1 2 3]]
    my $needle_notin  = $needle0_notin->dummy(1);            # 3x1: [[-3 -2 -1]]
    my $needles       = pdl( [ [ 1, 2, 3 ], [ 9, 9, 9 ] ] )
      ;    # 3x2: $needle0_in->cat($needle0_notin)
    my $haystack = pdl( [ [ 1, 2, 3 ], [ 4, 5, 6 ] ] );    # 3x2

    sub intersect_ok {
        my ( $label, $a, $b, $c_want, $nc_want, $c_sclr_want ) = @_;
        my ( $c, $nc ) = intersectvec( $a, $b );
        my $c_sclr = intersectvec( $a, $b );
        ok tapprox( $c,      $c_want ),      "$label - result";
        ok tapprox( $nc,     $nc_want ),     "$label - counts";
        ok tapprox( $c_sclr, $c_sclr_want ), "$label - scalar";
    }

    intersect_ok(
        'intersectvec - implicit dims - needle0_in&haystack',
        $needle0_in, $haystack, [ [ 1, 2, 3 ] ],
        1, [ [ 1, 2, 3 ] ]
    );
    intersect_ok(
        'intersectvec - implicit dims - needle_in&haystack',
        $needle_in, $haystack, [ [ 1, 2, 3 ] ],
        1, [ [ 1, 2, 3 ] ]
    );

    intersect_ok(
        'intersectvec - implicit dims - needle0_notin&haystack',
        $needle0_notin, $haystack, [ [ 0, 0, 0 ] ],
        0, zeroes( 3, 0 )
    );
    intersect_ok(
        'intersectvec - implicit dims - needle_notin&haystack',
        $needle_notin, $haystack, [ [ 0, 0, 0 ] ],
        0, zeroes( 3, 0 )
    );

    intersect_ok(
        'intersectvec - implicit dims - needles&haystack',
        $needles, $haystack, [ [ 1, 2, 3 ], [ 0, 0, 0 ] ],
        1, [ [ 1, 2, 3 ] ]
    );

    # now we want to know whether each needle is "in" one by one, not really
    # a normal intersect, so we insert a dummy in haystack in order to broadcast
    # the "nc" needs to come back as a 4x2
    my $needles8 = pdl(
        [
            [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 8, 8, 8 ], [ 8, 8, 8 ] ],
            [ [ 4, 5, 6 ], [ 9, 9, 9 ], [ 1, 2, 3 ], [ 9, 9, 9 ] ]
        ]
    );    # 3x4x2

# need to manipulate above into suitable inputs for intersect to get right output
# + dummy dim here also ensures singleton query-vector-sets are (trivially) sorted
    my $needles8x =
      $needles8->slice(",*1,,");   # 3x*x4x2 # dummy of size 1 inserted in dim 1

# haystack: no changes needed; don't need same number of dims, broadcast engine will add dummy/1s at top
    my $haystack8 = $haystack;
    my $c_want8   = [
        [ [ [ 1, 2, 3 ] ], [ [ 4, 5, 6 ] ], [ [ 0, 0, 0 ] ], [ [ 0, 0, 0 ] ] ],
        [ [ [ 4, 5, 6 ] ], [ [ 0, 0, 0 ] ], [ [ 1, 2, 3 ] ], [ [ 0, 0, 0 ] ] ],
    ];
    my $nc_want8 = [ [ 1, 1, 0, 0 ], [ 1, 0, 1, 0 ] ];

    intersect_ok( 'intersectvec - implicit dims - needles8x&haystack8',
        $needles8x, $haystack8, $c_want8, $nc_want8, $c_want8 );
};

## dim-checks and implicit broadcast dimensions
##  + analogous to https://github.com/moocow-the-bovine/PDL-VectorValued/issues/4
subtest v_broadcast_dimensions => sub {

    # data: basic
    my $empty = zeroes(0);
    my $v1_2  = pdl( [ 1, 2 ] );
    my $v3_4  = pdl( [ 3, 4 ] );
    my $v1_4  = $v1_2->cat($v3_4)->flat;

    # data: broadcasting
    my $k      = 2;
    my $kempty = $empty->slice(",*$k");
    my $kv1_2  = $v1_2->slice(",*$k");
    my $kv3_4  = $v3_4->slice(",*$k");
    my $kv1_4  = $v1_4->slice(",*$k");

    #-- union_sorted
    ok tapprox( scalar( $v1_2->union_sorted($v3_4) ), $v1_4 ),
      "union_sorted - broadcast dims - 12+34";
    ok tapprox( scalar( $v3_4->union_sorted($v1_4) ), $v1_4 ),
      "union_sorted - broadcast dims - 34+1234";
    ok tapprox( scalar( $empty->union_sorted($v1_4) ), $v1_4 ),
      "union_sorted - broadcast dims - 0+1234";
    ok tapprox( scalar( $v1_4->union_sorted($empty) ), $v1_4 ),
      "union_sorted - broadcast dims - 1234+0";
    ok tapprox( scalar( $empty->union_sorted($empty) ), $empty ),
      "union_sorted - broadcast dims - 0+0";
    #
    ok tapprox( scalar( $kv1_2->union_sorted($v3_4) ), $kv1_4 ),
      "union_sorted - broadcast dims - 12(*k)+34";
    ok tapprox( scalar( $kv3_4->union_sorted($v1_4) ), $kv1_4 ),
      "union_sorted - broadcast dims - 34(*k)+1234";
    ok tapprox( scalar( $kempty->union_sorted($v1_4) ), $kv1_4 ),
      "union_sorted - broadcast dims - 0(*k)+1234";
    ok tapprox( scalar( $kv1_4->union_sorted($empty) ), $kv1_4 ),
      "union_sorted - broadcast dims - 1234(*k)+0";
    ok tapprox( scalar( $kempty->union_sorted($empty) ), $kempty ),
      "union_sorted - broadcast dims - 0(*k)+0";

    #-- intersect_sorted
    ok tapprox( scalar( $v1_2->intersect_sorted($v3_4) ), $empty ),
      "intersect_sorted - broadcast dims - 12&34";
    ok tapprox( scalar( $v3_4->intersect_sorted($v1_4) ), $v3_4 ),
      "intersect_sorted - broadcast dims - 34&1234";
    ok tapprox( scalar( $empty->intersect_sorted($v1_4) ), $empty ),
      "intersect_sorted - broadcast dims - 0&1234";
    ok tapprox( scalar( $v1_4->intersect_sorted($empty) ), $empty ),
      "intersect_sorted - broadcast dims - 1234&0";
    ok tapprox( scalar( $empty->intersect_sorted($empty) ), $empty ),
      "intersect_sorted - broadcast dims - 0&0";
    #
    ok tapprox( scalar( $kv1_2->intersect_sorted($v3_4) ), $kempty ),
      "intersect_sorted - broadcast dims - 12(*k)&34";
    ok tapprox( scalar( $kv3_4->intersect_sorted($v1_4) ), $kv3_4 ),
      "intersect_sorted - broadcast dims - 34(*k)&1234";
    ok tapprox( scalar( $kempty->intersect_sorted($v1_4) ), $kempty ),
      "intersect_sorted - broadcast dims - 0(*k)&1234";
    ok tapprox( scalar( $kv1_4->intersect_sorted($empty) ), $kempty ),
      "intersect_sorted - broadcast dims - 1234(*k)&0";
    ok tapprox( scalar( $kempty->intersect_sorted($empty) ), $kempty ),
      "intersect_sorted - broadcast dims - 0(*k)&0";

    #-- setdiff_sorted
    ok tapprox( scalar( $v1_2->setdiff_sorted($v3_4) ), $v1_2 ),
      "setdiff_sorted - broadcast dims - 12-34";
    ok tapprox( scalar( $v3_4->setdiff_sorted($v1_4) ), $empty ),
      "setdiff_sorted - broadcast dims - 34-1234";
    ok tapprox( scalar( $v1_4->setdiff_sorted($empty) ), $v1_4 ),
      "setdiff_sorted - broadcast dims - 1234-0";
    ok tapprox( scalar( $empty->setdiff_sorted($v1_4) ), $empty ),
      "setdiff_sorted - broadcast dims - 0-1234";
    ok tapprox( scalar( $empty->setdiff_sorted($empty) ), $empty ),
      "setdiff_sorted - broadcast dims - 0-0";
    #
    ok tapprox( scalar( $kv1_2->setdiff_sorted($v3_4) ), $kv1_2 ),
      "setdiff_sorted - broadcast dims - 12(*k)-34";
    ok tapprox( scalar( $kv3_4->setdiff_sorted($v1_4) ), $kempty ),
      "setdiff_sorted - broadcast dims - 34(*k)-1234";
    ok tapprox( scalar( $kv1_4->setdiff_sorted($empty) ), $kv1_4 ),
      "setdiff_sorted - broadcast dims - 1234(*k)-0";
    ok tapprox( scalar( $kempty->setdiff_sorted($v1_4) ), $kempty ),
      "setdiff_sorted - broadcast dims - 0(*k)-1234";
    ok tapprox( scalar( $kempty->setdiff_sorted($empty) ), $kempty ),
      "setdiff_sorted - broadcast dims - 0(*k)-0";
};

done_testing;
