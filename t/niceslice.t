use strict;
use Test;

use PDL::LiteF;

BEGIN { 
    eval 'require PDL::NiceSlice';
    unless ($@) {
	plan tests => 41,
	# todo => [37..40],
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

#
# modifiers
#

$a = sequence 10;
eval translate_and_show '$b = $a($a<3;?)' ;
ok (!$@);
ok(all $b == pdl(0,1,2));

# flat modifier
$a = sequence 3,3;
eval translate_and_show '$b = $a(0:-2;_);';
ok (!$@);
ok(all $b == sequence 8);

# where modifier cannot be mixed with other modifiers
$a = sequence 10;
eval { translate_and_show '$b = $a($a<3;?_)' };
ok ($@ =~ 'more than 1');

# more than one identifier
$a = sequence 3,3;
eval translate_and_show '$b = $a(0;-|)';
print "Error was: $@\n" if $@;
ok (!$@);
eval {$b++};
print "\$b = $b\n";
ok($b->dim(0) == 3 && all $b == 3*sequence(3)+1);
ok($a->at(0,0) == 0);

# do we ignore whitspace correctly?
eval translate_and_show '$c = $a(0; - | )';
print "Error was: $@\n" if $@;
ok (!$@);
ok (all $c == $b-1);

# empty modifier block
$a = sequence 10;
eval translate_and_show '$b = $a(0;   )';
ok (!$@);
ok ($b == $a->at(0));

# modifiers repeated
eval 'translate_and_show "\$b = \$a(0;-||)"';
print "Error was: $@\n" if $@;
ok ($@ =~ 'twice or more');

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

$a = ''; # foreach and whitespace
eval translate_and_show 'foreach  my $b (1,2,3,4) {$a .= $b;}';
ok(!$@ and $a eq '1234');

# block method access translation

$a = pdl(5,3,2);
my $method = 'dim';
eval translate_and_show '$c = $a->$method(0)';
print "c: $c\n";
ok(!$@ && $c == $a->dim(0));

#
# todo ones
#

# whitespace tolerance

$a= sequence 10;
eval translate_and_show '$c = $a (0)';
ok(!$@ && $c == $a->at(0));

# comment tolerance

eval translate_and_show << 'EOT';

$c = $a-> # comment
	 (0);
EOT

ok(!$@ && $c == $a->at(0));

eval translate_and_show << 'EOT';

$c = $a-> # comment
          # comment line 2
	 (0);
EOT

ok(!$@ && $c == $a->at(0));

$a = ''; # foreach and whitespace + comments
eval translate_and_show << 'EOT';

foreach  my $b # a random comment thrown in

(1,2,3,4) {$a .= $b;}

EOT

ok(!$@ and $a eq '1234');

# test for correct header propagation
$a = ones(10,10);
my $h = {NAXIS=>2,
	 NAXIS1=>100,
	 NAXIS=>100,
	 COMMENT=>"Sample FITS-style header"};
$a->sethdr($h);
$a->hdrcpy(1);
eval translate_and_show '$b = $a(1:2,pdl(0,2));';

# Old hdrcpy test (for copy-by-reference); this is obsolete
# with quasi-deep copying.  --CED 11-Apr-2003
#   ok (!$@ and $b->gethdr() == $h);
ok(!$@ and join("",%{$b->gethdr}) eq join("",%{$h}));
