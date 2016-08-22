# Test conversions. This is not yet good enough: we need
# nasty test cases,

# 1.9901 - converted to new type semantics + extra test

use Test::More tests => 7;

use PDL::LiteF;
use PDL::Types;
use PDL::Constants qw(PI);
use strict;
use warnings;

my $A = pdl 42.4;
note "A is $A\n";

is($A->get_datatype,$PDL_D, "A is double");

my $B = byte $A;
note "B (byte $A) is $B\n";

is($B->get_datatype,$PDL_B, "B is byte");
is($B->at(),42, 'byte value is 42');

my $C = $B * 3;
is($C->get_datatype, $PDL_B, "C also byte");
note "C ($B * 3) is $C\n";

my $D = $B * 600.0;
is($D->get_datatype, $PDL_F, "D promoted to float");
note "D ($B * 600) is $D\n";

my $E = $B * PI;
is($E->get_datatype, $PDL_D, "E promoted to double");
note "E ($B * PI) is $E\n";

my $F = $B * "-2.2";
is($F->get_datatype, $PDL_D, "F check string handling");
note "F ($B * string(-2.2)) is $F\n";

