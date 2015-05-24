#!/bin/perl -w

## Test of PDL::Char subclass -- treating byte PDLs as matrices of fixed strings

use Test::More tests => 6;
use PDL;
use PDL::Char;
use strict;
use warnings;

{
my $pa = PDL::Char->new ([[['abc', 'def', 'ghi'],['jkl', 'mno', 'qrs']],
		    [['tuv', 'wxy', 'zzz'],['aaa', 'bbb', 'ccc']]]);

my $stringized = $pa->string;
my $comp = 
qq{[
 [
  [ 'abc' 'def' 'ghi'   ] 
  [ 'jkl' 'mno' 'qrs'   ] 
 ] 
 [
  [ 'tuv' 'wxy' 'zzz'   ] 
  [ 'aaa' 'bbb' 'ccc'   ] 
 ] 
] 
};

is( $stringized, $comp);
$pa->setstr(0,0,1, 'foo');
is( $pa->atstr(0,0,1), 'foo');
$pa->setstr(2,0,0, 'barfoo');
is( $pa->atstr(2,0,0), 'bar');
$pa->setstr(0,0,1, 'f');
is( $pa->atstr(0,0,1), "f");
my $pb = sequence (byte, 4, 5) + 99;
$pb = PDL::Char->new($pb);
$stringized = $pb->string;
$comp = "[ 'cdef' 'ghij' 'klmn' 'opqr' 'stuv' ] \n";
is($stringized, $comp);
}

{
# Variable-length string test
my $varstr = PDL::Char->new( [ ["longstring", "def", "ghi"],["jkl", "mno", 'pqr'] ] );
 
# Variable Length Strings: Expected Results
my $comp2 = 
"[
 [ 'longstring' 'def' 'ghi'  ] 
 [ 'jkl' 'mno' 'pqr'  ] 
] 
";

is("$varstr", $comp2);
}
