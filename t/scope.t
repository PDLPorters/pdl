# Test if we can still do scopes ok - multiple uses etc..
# Also see that PDL loaders get the correct symbols.


print "1..10\n";

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

package A;
use vars qw/$a/;
# print "A: ",%A::,"\n";
use PDL;

# $a = zeroes 5,5;

# print "A: ",%A::,"\n";

$a = zeroes 5,5;

# print "A: %A::\n";



# print "AC: ",(bless {},A)->can("zeroes"),"\n";
::ok(1,(bless {},A)->can("zeroes"));

package B;
use PDL;

#print "B: ",%B::,"\n";
#print "B: ",%B::,"\n";
# $b = zeroes 5,5;
# print "BC: ",(bless {},B)->can("zeroes"),"\n";
::ok(2,(bless {},B)->can("zeroes"));

package C;
use PDL::Lite;
::ok(3,!((bless {},C)->can("zeroes")));

package D;
use PDL::Lite;
::ok(4,!((bless {},D)->can("zeroes")));

package E;
use PDL::LiteF;
::ok(5,(bless {},E)->can("zeroes"));

package F;
use PDL::LiteF;
::ok(6,(bless {},F)->can("zeroes"));

::ok(7,!((bless {},C)->can("imag")));
::ok(8,!((bless {},D)->can("imag")));
::ok(9,!((bless {},E)->can("imag")));
::ok(10,!((bless {},F)->can("imag")));
