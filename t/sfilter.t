use strict;
use Test;

use PDL::LiteF;

BEGIN { 
    if ( require Filter::Simple && require Text::Balanced ) {
	plan tests => 7;
    } else {
	plan tests => 1;
	print "ok 1 # Skipped: no sourcefilter support\n";
	exit;
    }
} 

$| = 1;
eval 'require PDL::SFilter';
sub PDL::SFilter::findslice;

ok (!$@);

my $a = sequence 10; # shut up -w
eval PDL::SFilter::findslice '$b = $a((5));';

ok (!$@);
ok($b->at == 5);

eval PDL::SFilter::findslice '$b = $a->((5));';
ok (!$@);
ok($b->at == 5);

my $c = PDL->pdl(7,6);
my $txt = '$b = $a(($c(1)->at(0)));';
my $etxt = PDL::SFilter::findslice '$b = $a(($c(1)->at(0)));';
eval $etxt;
ok (!$@);
ok(all $b == 6);

print "$txt -> $etxt\n";
