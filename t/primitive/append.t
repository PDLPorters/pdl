use strict;
use warnings;
use Test2::V0 '!float';

use PDL::LiteF;
use Test::Lib;

is(
    append( zeroes( 2, 0 ), zeroes( 3, 0 ) )->shape->unpdl,
    [ 5, 0 ],
    'multi-dim empty shape'
);

is( append( pdl( 1, 2, 3, 4 ), 2 )->unpdl, [ 1, 2, 3, 4, 2 ], '[4], [1]' );

subtest '$output = append (null,null) ' => sub {
    my $output = append( null, null );
    ok !$output->isnull, 'returns non-null';
    ok $output->isempty, 'returns empty';
};

subtest 'append(null, null, $output)' => sub {
    my $output = zeroes(1);
    append( null, null, $output );
    is( $output->unpdl, [0], q{user's ndarray is unchanged} );
};

subtest 'output ndarray has different shape' => sub {

    todo 'output => [1]; required [2].  SHOULD FAIL, output too small' => sub {
        my $output = zeroes(1);
        like ( dies { append( pdl(1), pdl(2), $output ) },
               qr/dim has size 1/ );
    };

    subtest 'output => [3,1]; required [2]' => sub {
        my $output = zeroes(3,1);
        like ( dies { append( pdl(1), pdl(2), $output ) },
               qr/dim has size 3/ );
    };

    subtest 'output => null; required [2]' => sub {
        my $output = null;
        append( pdl(1), pdl(2), $output );
        is( $output->unpdl, [ 1, 2 ], q{full append } );
    };

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
