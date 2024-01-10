use Test2::V0 '!float';
use PDL::LiteF;
use Test::Lib;
use My::Test::Primitive;

sub IM {
    PDL->new(
        [
            [ 1,  2,  3,  3,  5 ],
            [ 2,  3,  4,  5,  6 ],
            [ 13, 13, 13, 13, 13 ],
            [ 1,  3,  1,  3,  1 ],
            [ 10, 10, 2,  2,  2, ]
        ]
    );

}

is(
    IM->hclip(5)->unpdl,
    [
        [ 1, 2, 3, 3, 5 ],
        [ 2, 3, 4, 5, 5 ],
        [ 5, 5, 5, 5, 5 ],
        [ 1, 3, 1, 3, 1 ],
        [ 5, 5, 2, 2, 2, ]
    ],
    'hclip'
);

is(
    IM->lclip(5)->unpdl,
    [
        [ 5,  5,  5,  5,  5 ],
        [ 5,  5,  5,  5,  6 ],
        [ 13, 13, 13, 13, 13 ],
        [ 5,  5,  5,  5,  5 ],
        [ 10, 10, 5,  5,  5, ]
    ],
    'lclip'
);

is(
    IM->clip( 5, 7 )->unpdl,
    [
        [ 5, 5, 5, 5, 5 ],
        [ 5, 5, 5, 5, 6 ],
        [ 7, 7, 7, 7, 7 ],
        [ 5, 5, 5, 5, 5 ],
        [ 7, 7, 5, 5, 5, ]
    ],
    'clip'
);

subtest 'with NaN badvalue' => sub {
    my $im = sequence(3);
    $im->badvalue( nan() );
    $im->badflag(1);
    $im->set( 1, nan() );
    my $clipped = $im->lclip(0);
    is $clipped->unpdl, [0, 'BAD', 2], 'ISBAD() works when badvalue is NaN';
};

done_testing;
