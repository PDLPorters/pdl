#!/bin/perl -w

## Test of PDL::Char subclass -- treating byte PDLs as matrices of fixed strings

use Test::More tests => 6;
use PDL;
use PDL::Char;
use strict;

my $a = PDL::Char->new ([[['abc', 'def', 'ghi'],['jkl', 'mno', 'qrs']],
		    [['tuv', 'wxy', 'zzz'],['aaa', 'bbb', 'ccc']]]);

my $stringized = $a->string;
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

ok( ($stringized eq $comp));
$a->setstr(0,0,1, 'foo');
ok( ($a->atstr(0,0,1) eq 'foo'));
$a->setstr(2,0,0, 'barfoo');
ok( ($a->atstr(2,0,0) eq 'bar'));
$a->setstr(0,0,1, 'f');
ok( ($a->atstr(0,0,1) eq "f"));
$b = sequence (byte, 4, 5) + 99;
$b = PDL::Char->new($b);
$stringized = $b->string;
$comp = "[ 'cdef' 'ghij' 'klmn' 'opqr' 'stuv' ] \n";
ok( ($stringized eq $comp));

# Variable-length string test
my $varstr = PDL::Char->new( [ ["longstring", "def", "ghi"],["jkl", "mno", 'pqr'] ] );
 
# Variable Length Strings: Expected Results
my $comp2 = 
"[
 [ 'longstring' 'def' 'ghi'  ] 
 [ 'jkl' 'mno' 'pqr'  ] 
] 
";

ok( ("$varstr" eq $comp2));
