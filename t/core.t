# -*-perl-*-
#
# test some PDL core routines
#

use strict;
use Test;


plan tests => 4;

use PDL::LiteF;
$| = 1;

sub tapprox {
    my ( $a, $b ) = @_;
    my $d = abs( $a - $b );
    print "diff = [$d]\n";
    return $d <= 0.0001;
}

my $a = sequence 10;
my $b = $a->slice('5');
my $c = $a->slice('4:7');

# test 'sclr' method
ok tapprox $b->sclr, 5;
ok tapprox $c->sclr, 4;

# switch multiel check on
ok(PDL->sclr({Check=>'barf'}),2);
eval '$c->sclr';
ok $@ =~ /multielement piddle in 'sclr' call/;
