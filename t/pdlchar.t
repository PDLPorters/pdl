#!/bin/perl -w

#
## Test of PDL::Char subclass -- treating byte PDLs as matrices of fixed strings
#

use PDL;
use PDL::Char;
use strict;

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

print "1..5\n";

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

ok(1, ($stringized eq $comp));
$a->setstr(0,0,1, 'foo');
ok(2, ($a->atstr(0,0,1) eq 'foo'));
$a->setstr(2,0,0, 'barfoo');
ok(3, ($a->atstr(2,0,0) eq 'bar'));
$a->setstr(0,0,1, 'f');
ok(4, ($a->atstr(0,0,1) eq "f\0\0"));
$b = sequence (byte, 4, 5) + 99;
$b = PDL::Char->new($b);
$stringized = $b->string;
$comp = "[ 'cdef' 'ghij' 'klmn' 'opqr' 'stuv' ] \n";
ok(5, ($stringized eq $comp));

