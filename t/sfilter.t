use strict;
use Test;

use PDL::LiteF;

BEGIN { 
    eval 'require PDL::SFilter';
    unless ($@) {
	plan tests => 13;
    } else {
	plan tests => 1;
	print "ok 1 # Skipped: no sourcefilter support\n";
	exit;
    }
} 

$| = 1;
sub PDL::SFilter::findslice;
sub translate_and_show {
  my ($txt) = @_;
  my $etxt = PDL::SFilter::findslice $txt;
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
