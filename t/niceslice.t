use strict;
use Test;

use PDL::LiteF;

BEGIN { 
    eval 'require PDL::NiceSlice';
    unless ($@) {
	plan tests => 25;
    } else {
	plan tests => 1;
	print "ok 1 # Skipped: no sourcefilter support\n";
	exit;
    }
} 

$| = 1;
sub PDL::NiceSlice::findslice;
sub translate_and_show {
  my ($txt) = @_;
  my $etxt = PDL::NiceSlice::findslice $txt;
  print "$txt -> \n\t$etxt\n";
  return $etxt;
}


ok (!$@);

my $a = sequence 10; # shut up -w
my $b = pdl(1);
eval translate_and_show '$b = $a((5));';

ok (!$@);
ok($b->at == 5);

eval translate_and_show '$b = $a->((5));';
ok (!$@);
ok($b->at == 5);

my $c = PDL->pdl(7,6);
eval translate_and_show '$b = $a(($c(1)->at(0)));';
ok (!$@);
ok($b->getndims == 0 && all $b == 6);

# the latest versions should do the 'at' automatically
eval translate_and_show '$b = $a(($c(1)));';
ok (!$@);
ok($b->getndims == 0 && all $b == 6);

eval translate_and_show '$c = $a(:);';
ok (!$@);
print $@ if $@;
ok ($c->getdim(0) == 10 && all $c == $a);

my $idx = pdl 1,4,5;

eval translate_and_show '$b = $a($idx);';
ok (!$@);
ok(all $b == $idx);

# use 1-el piddles as indices
my $rg = pdl(2,7,2);
my $cmp = pdl(2,4,6);
eval translate_and_show '$b = $a($rg(0):$rg(1):$rg(2));';
ok (!$@);
ok(all $b == $cmp);

# mix ranges and index piddles
my $twod = sequence 5,5;
$idx = pdl 2,3,0;
$cmp = $twod->slice('-1:0')->dice_axis(1,$idx);
eval translate_and_show '$b = $twod(-1:0,$idx);';
ok (!$@);
ok(all $b == $cmp);

# modifiers

$a = sequence 10;
eval translate_and_show '$b = $a($a<3;?)';
ok (!$@);
ok(all $b == pdl(0,1,2));

$a = sequence 3,3;
eval translate_and_show '$b = $a(0:-2;_);';
ok (!$@);
ok(all $b == sequence 8);

# foreach/for blocking

$a = '';
eval translate_and_show "foreach \n" . ' $b(1,2,3,4) {$a .= $b;}';
ok(!$@ and $a eq '1234');

$a = '';
eval translate_and_show 'for    $b(1,2,3,4) {$a .= $b;}';
ok(!$@ and $a eq '1234');

$a = '';
eval translate_and_show 'for  my  $b(1,2,3,4) {$a .= $b;}';
ok(!$@ and $a eq '1234');

$a = '';
eval translate_and_show 'for  our $b(1,2,3,4) {$a .= $b;}';
ok(!$@ and $a eq '1234');
