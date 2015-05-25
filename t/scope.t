# Test if we can still do scopes ok - multiple uses etc..
# Also see that PDL loaders get the correct symbols.
use strict;
use warnings;
use Test::More tests => 10;

package A;
our $pa;
# note "A: ",%A::,"\n";
use PDL;

# $pa = zeroes 5,5;

# note "A: ",%A::,"\n";

$pa = zeroes 5,5;

# note "A: %A::\n";

# note "AC: ",(bless {},A)->can("zeroes"),"\n";
::ok((bless {},'A')->can("zeroes"));

package B;
use PDL;

#note "B: ",%B::,"\n";
#note "B: ",%B::,"\n";
# $pb = zeroes 5,5;
# note "BC: ",(bless {},B)->can("zeroes"),"\n";
::ok((bless {},'B')->can("zeroes"));

package C;
use PDL::Lite;
::ok(!((bless {},'C')->can("zeroes")));

package D;
use PDL::Lite;
::ok(!((bless {},'D')->can("zeroes")));

package E;
use PDL::LiteF;
::ok((bless {},'E')->can("zeroes"));

package F;
use PDL::LiteF;
::ok((bless {},'F')->can("zeroes"));

::ok(!((bless {},'C')->can("imag")));
::ok(!((bless {},'D')->can("imag")));
::ok(!((bless {},'E')->can("imag")));
::ok(!((bless {},'F')->can("imag")));
