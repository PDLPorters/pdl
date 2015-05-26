use strict;
use Test::More;

use PDL::LiteF;

BEGIN {
	eval {
		require PDL::NiceSlice;
		1;
	} or plan skip_all => "no sourcefilter support: $@";
}

plan tests => 43;

$| = 1;
sub PDL::NiceSlice::findslice;
sub translate_and_show {
  my ($txt) = @_;
  my $etxt = PDL::NiceSlice::findslice $txt;
  note "$txt -> \n\t$etxt\n";
  return $etxt;
}

my $pa = sequence 10; # shut up -w
my $pb = pdl(1);
eval translate_and_show '$pb = $pa((5));';

ok (!$@);
ok($pb->at == 5);

eval translate_and_show '$pb = $pa->((5));';
ok (!$@);
ok($pb->at == 5);

my $c = PDL->pdl(7,6);
eval translate_and_show '$pb = $pa(($c(1)->at(0)));';
ok (!$@);
ok($pb->getndims == 0 && all $pb == 6);

# the latest versions should do the 'at' automatically
eval translate_and_show '$pb = $pa(($c(1)));';
ok (!$@);
note "ERROR is $@\n" if($@);
ok($pb->getndims == 0 && all $pb == 6);

eval translate_and_show '$c = $pa(:);';
ok (!$@);
note $@ if $@;
ok ($c->getdim(0) == 10 && all $c == $pa);

my $idx = pdl 1,4,5;

eval translate_and_show '$pb = $pa($idx);';
ok (!$@);
ok(all $pb == $idx);

# use 1-el piddles as indices
my $rg = pdl(2,7,2);
my $cmp = pdl(2,4,6);
eval translate_and_show '$pb = $pa($rg(0):$rg(1):$rg(2));';
ok (!$@);
ok(all $pb == $cmp);

# mix ranges and index piddles
my $twod = sequence 5,5;
$idx = pdl 2,3,0;
$cmp = $twod->slice('-1:0')->dice_axis(1,$idx);
eval translate_and_show '$pb = $twod(-1:0,$idx);';
ok (!$@);
ok(all $pb == $cmp);

#
# modifiers
#

$pa = sequence 10;
eval translate_and_show '$pb = $pa($pa<3;?)' ;
ok (!$@);
ok(all $pb == pdl(0,1,2));

# flat modifier
$pa = sequence 3,3;
eval translate_and_show '$pb = $pa(0:-2;_);';
ok (!$@);
ok(all $pb == sequence 8);

# where modifier cannot be mixed with other modifiers
$pa = sequence 10;
eval { translate_and_show '$pb = $pa($pa<3;?_)' };
ok ($@ =~ 'more than 1');

# more than one identifier
$pa = sequence 3,3;
eval translate_and_show '$pb = $pa(0;-|)';
note "Error was: $@\n" if $@;
ok (!$@);
eval {$pb++};
note "\$pb = $pb\n";
ok($pb->dim(0) == 3 && all $pb == 3*sequence(3)+1);
ok($pa->at(0,0) == 0);

# do we ignore whitspace correctly?
eval translate_and_show '$c = $pa(0; - | )';
note "Error was: $@\n" if $@;
ok (!$@);
ok (all $c == $pb-1);

# empty modifier block
$pa = sequence 10;
eval translate_and_show '$pb = $pa(0;   )';
ok (!$@);
ok ($pb == $pa->at(0));

# modifiers repeated
eval 'translate_and_show "\$pb = \$pa(0;-||)"';
note "Error was: $@\n" if $@;
ok ($@ =~ 'twice or more');

# foreach/for blocking

$pa = '';
eval translate_and_show "foreach \n" . ' $pb(1,2,3,4) {$pa .= $pb;}';
ok(!$@ and $pa eq '1234');

$pa = '';
eval translate_and_show 'for    $pb(1,2,3,4) {$pa .= $pb;}';
ok(!$@ and $pa eq '1234');

$pa = '';
eval translate_and_show 'for  my  $pb(1,2,3,4) {$pa .= $pb;}';
ok(!$@ and $pa eq '1234');

$pa = '';
eval translate_and_show 'for  our $pb(1,2,3,4) {$pa .= $pb;}';
ok(!$@ and $pa eq '1234');

$pa = ''; # foreach and whitespace
eval translate_and_show 'foreach  my $pb (1,2,3,4) {$pa .= $pb;}';
ok(!$@ and $pa eq '1234');

$pa = ''; my $t = ones 10; # foreach and imbedded expression
eval translate_and_show 'foreach my $type ( $t(0)->list ) { $pa .= $type }';
ok(!$@ and $pa eq '1');

# block method access translation

$pa = pdl(5,3,2);
my $method = 'dim';
eval translate_and_show '$c = $pa->$method(0)';
note "c: $c\n";
ok(!$@ && $c == $pa->dim(0));

#
# todo ones
#

# whitespace tolerance

$pa= sequence 10;
eval translate_and_show '$c = $pa (0)';
ok(!$@ && $c == $pa->at(0));

# comment tolerance

eval translate_and_show << 'EOT';

$c = $pa-> # comment
	 (0);
EOT

ok(!$@ && $c == $pa->at(0));

eval translate_and_show << 'EOT';

$c = $pa-> # comment
          # comment line 2
	 (0);
EOT

ok(!$@ && $c == $pa->at(0));

$pa = ''; # foreach and whitespace + comments
eval translate_and_show << 'EOT';

foreach  my $pb # a random comment thrown in

(1,2,3,4) {$pa .= $pb;}

EOT

ok(!$@ and $pa eq '1234');

# test for correct header propagation
$pa = ones(10,10);
my $h = {NAXIS=>2,
	 NAXIS1=>100,
	 NAXIS=>100,
	 COMMENT=>"Sample FITS-style header"};
$pa->sethdr($h);
$pa->hdrcpy(1);
eval translate_and_show '$pb = $pa(1:2,pdl(0,2));';

# Old hdrcpy test (for copy-by-reference); this is obsolete
# with quasi-deep copying.  --CED 11-Apr-2003
#   ok (!$@ and $pb->gethdr() == $h);
if ( ok(!$@) ) {
   my %bh = %{$pb->gethdr};
   my (@bhkeys) = sort keys %bh;
   my %hh = %{$h};
   my (@hhkeys) =  sort keys %hh;
   ok(join("",@bh{@bhkeys}) eq join("",@hh{@hhkeys}));
}

$pa = ones(10);
my $i = which $pa < 0;
my $ai;
eval translate_and_show '$ai = $pa($i);';
ok(isempty $ai );
