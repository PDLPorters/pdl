# -*-perl-*-
#
# test some PDL core routines
#

use strict;
use Test;


plan tests => 12;

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

# switch multielement check on
ok(PDL->sclr({Check=>'barf'}),2);
eval '$c->sclr';
ok $@ =~ /multielement piddle in 'sclr' call/;

# test reshape barfing with negative args

eval 'my $d = $a->reshape(0,-3);';
ok $@ =~ /invalid dim size/;

# test reshape with no args
$a = ones 3,1,4;
$b = $a->reshape;
ok join(',',$b->dims) eq '3,4';

# test reshape(-1) and squeeze
$a = ones 3,1,4;
$b = $a->reshape(-1);
$c = $a->squeeze;
ok join(',',$b->dims) eq '3,4';
ok all $b == $c;

$c++; # check dataflow
print "a: $a\nb: $b\nc: $c\n";
ok all $b == $c; # should flow back to b
ok all $a == 2;


# test topdl

ok UNIVERSAL::isa(PDL->topdl(1),'PDL');
ok UNIVERSAL::isa(PDL->topdl([1,2,3]),'PDL');
