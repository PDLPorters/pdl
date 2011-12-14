use PDL;
use PDL::ImageND;
use PDL::NiceSlice;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

print "1..7\n";

# Right answer

my $ans = pdl(
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);

my $a = xvals zeroes 10,3;

my $b = pdl [1,2],[2,1];

my $c=convolve ($a, $b);

ok( 1, int(at(sum($c-$ans)))==0 );


$a = zeroes(6,6); 

my $ta;

($ta = $a(4,:)) .= 1;
($ta = $a(5,:)) .= 1;
($ta = $a(1,2)) .= 1;
($ta = $a(0,4)) .= 1;
($ta = $a(2,0)) .= 1;

$b = pdl( [-1,0],[0,1] );


my $ans_e = pdl(
	     [ 0,  0,  1, -1,  0,  0],
	     [-1,  0,  0, -1,  0,  0],
	     [ 0,  1,  0, -1,  0,  0],
	     [ 0,  0,  0, -1,  0,  0],
	     [ 1,  0,  0, -1,  0,  0],
	     [ 0,  0,  0, -1,  0,  0]
	);
$c = convolveND($a,$b,{m=>'d',b=>'e'});
ok( 2, all( abs($c - $ans_e) < 1e-15 ) );

$c = convolveND($a,$b,{m=>'f',b=>'e'});
ok( 3, all( abs($c - $ans_e) < 1e-15 ) );

$ans_p = pdl(
	     [ 0,  0,  1, -1,  0,  1],
	     [-1,  0,  0, -1,  0,  1],
	     [ 0,  1,  0, -1,  0,  1],
	     [ 0,  0,  0, -1,  0,  0],
	     [ 1,  0,  0, -1,  0,  1],
	     [ 0, -1,  0, -1,  0,  1]
	);
$c = convolveND($a,$b,{m=>'d',b=>'p'});
ok( 4, all( abs($c - $ans_p) < 1e-15 ) );

$c = convolveND($a,$b,{m=>'f',b=>'p'});
ok( 5, all( abs($c - $ans_p) < 1e-15 ) );


$ans_t = pdl(
	     [ 0,  0,  1, -1,  0,  1],
	     [-1,  0,  0, -1,  0,  1],
	     [ 0,  1,  0, -1,  0,  1],
	     [ 0,  0,  0, -1,  0,  1],
	     [ 1,  0,  0, -1,  0,  1],
	     [ 0,  0,  0,  0,  1,  1]
	);
$c = convolveND($a,$b,{m=>'d',b=>'t'});
ok( 6, all( abs($c - $ans_t) < 1e-15 ) );

$c = convolveND($a,$b,{m=>'f',b=>'t'});
ok( 7, all( abs($c - $ans_t) < 1e-15 ) );

		

