use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use Test::PDL;

subtest 'setops' => sub {
    my $temp = sequence(10);
    my $x    = which( ( $temp % 2 ) == 0 );
    my $y    = which( ( $temp % 3 ) == 0 );
    is_pdl setops( $x, 'AND', $y ), my $exp = indx( [ 0, 6 ] ), "setops AND";
    is_pdl intersect( $x, $y ), $exp, "intersect same as setops AND";
    is_pdl setops( $x, 'OR', $y ), indx([0, 2, 3, 4, 6, 8, 9]), "setops OR";
    is_pdl setops( $x, 'XOR', $y ), indx( [ 2, 3, 4, 8, 9 ] ), "setops XOR";
};

subtest 'intersect' => sub {
    my $intersect_test = intersect( pdl( 1, -5, 4, 0 ), pdl( 0, 3, -5, 2 ) );
    is_pdl $intersect_test, pdl( -5, 0 ), 'Intersect test values';
};

subtest 'AND' => sub {
    # based on cases supplied by @jo-37
    my @cases = (
        [ pdl(1),      empty(), empty() ],
        [ ones(1),     empty(), empty() ],
        [ ones(4),     empty(), empty() ],
        [ sequence(4), empty(), empty() ],
        [ pdl(1),      ones(2), ones(1) ],
        [ ones(1),     ones(2), ones(1) ],
        [ ones(4),     ones(2), ones(1) ],
        [ sequence(4), ones(2), ones(1) ],
    );
    is_pdl setops( $_->[0], 'AND', $_->[1] ), $_->[2],
      "$_->[0] AND $_->[1]"
      for @cases;
};

done_testing;
