use strict;
use warnings;
use Test::More;
use PDL::LiteF;

is_deeply(
    append( zeroes( 2, 0 ), zeroes( 3, 0 ) )->shape->unpdl,
    [ 5, 0 ],
    'multi-dim empty shape'
);

is_deeply( append( pdl( 1, 2, 3, 4 ), 2 )->unpdl, [ 1, 2, 3, 4, 2 ], '[4], [1]' );

subtest '$output = append (null,null) ' => sub {
    my $output = append( null, null );
    ok !$output->isnull, 'returns non-null';
    ok $output->isempty, 'returns empty';
};

subtest 'append(null, null, $output)' => sub {
    my $output = zeroes(1);
    append( null, null, $output );
    is_deeply( $output->unpdl, [0], q{user's ndarray is unchanged} );
};

subtest types => sub {

    is( append( zeroes( float, 2, 0 ), zeroes( 3, 0 ) )->type,
        'float', 'float + double = float' );

    my $b1 = indx( 1, 2 );
    is $b1->type, 'indx', '$indx_pdl is an indx pdl';

    $b1 = $b1->append(-1);
    is $b1->type, 'indx',     'append($indx_pdl, -1) returns an indx pdl';
    is $b1. '',   '[1 2 -1]', 'append($indx_pdl, -1) correct content';
};

done_testing;
