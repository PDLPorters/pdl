use strict;
use warnings;
use Test::More;
use PDL::LiteF;
require PDL::NiceSlice;

# these are accessible inside sub
my $pa = sequence 10;
my $pb = pdl(1);
my $c = PDL->pdl(7,6);
my $idx = pdl 1,4,5;
my $rg = pdl(2,7,2);

sub translate_and_run {
  local $Test::Builder::Level = $Test::Builder::Level + 1;
  my ($txt, $expected_error) = @_;
  $expected_error ||= qr/^$/;
  my $retval = eval {
    my $etxt = PDL::NiceSlice::findslice($txt);
    note "$txt -> \n\t$etxt\n";
    eval $etxt;
  };
  like $@, $expected_error;
  $retval;
}

$pb = translate_and_run '$pa((5));';
cmp_ok($pb->at, '==', 5);

$pb = translate_and_run '$pa->((5));';
cmp_ok($pb->at, '==', 5);

$pb = translate_and_run '$pa(($c(1)->at(0)));';
is $pb->getndims, 0;
ok(all $pb == 6);

# the latest versions should do the 'at' automatically
$pb = translate_and_run '$pa(($c(1)));';
is $pb->getndims, 0;
ok(all $pb == 6);

$c = translate_and_run '$pa(:);';
ok ($c->getdim(0) == 10 && all $c == $pa);

$pb = translate_and_run '$pa($idx);';
ok(all $pb == $idx);

# use 1-el ndarrays as indices
my $cmp = pdl(2,4,6);
$pb = translate_and_run '$pa($rg(0):$rg(1):$rg(2));';
ok(all $pb == $cmp);

# mix ranges and index ndarrays
$pa = sequence 5,5;
$idx = pdl 2,3,0;
$cmp = $pa->slice('-1:0')->dice_axis(1,$idx);
translate_and_run '$pb = $pa(-1:0,$idx);';
ok(all $pb == $cmp);

#
# modifiers
#

$pa = sequence 10;
$pb = translate_and_run '$pa($pa<3;?)' ;
ok(all $pb == pdl(0,1,2));

# flat modifier
$pa = sequence 3,3;
$pb = translate_and_run '$pa(0:-2;_);';
ok(all $pb == sequence 8);

# where modifier cannot be mixed with other modifiers
$pa = sequence 10;
$pb = translate_and_run '$pa($pa<3;?_)', qr/more than 1/;

# more than one identifier
$pa = sequence 3,3;
$pb = translate_and_run '$pa(0;-|)';
eval {$pb++};
ok($pb->dim(0) == 3 && all $pb == 3*sequence(3)+1) or diag $pb;
ok($pa->at(0,0) == 0) or diag $pa;

# do we ignore whitspace correctly?
$c = translate_and_run '$pa(0; - | )';
ok (all $c == $pb-1);

# empty modifier block
$pa = sequence 10;
$pb = translate_and_run '$pa(0;   )';
ok ($pb == $pa->at(0));

# modifiers repeated
$pb = translate_and_run '$pa(0;-||)', qr/twice or more/;

# foreach/for blocking

$pa = '';
translate_and_run "foreach \n" . ' $pb(1,2,3,4) {$pa .= $pb;}';
is($pa, '1234');

$pa = '';
translate_and_run 'for    $pb(1,2,3,4) {$pa .= $pb;}';
is($pa, '1234');

$pa = '';
translate_and_run 'for  my  $pb(1,2,3,4) {$pa .= $pb;}';
is($pa, '1234');

$pa = '';
translate_and_run 'for  our $pb(1,2,3,4) {$pa .= $pb;}';
is($pa, '1234');

$pa = '';
# foreach and whitespace
translate_and_run 'foreach  my $pb (1,2,3,4) {$pa .= $pb;}';
is($pa, '1234');

# foreach and embedded expression
$pa = '';
translate_and_run 'my $t = ones 10; foreach my $type ( $t(0)->list ) { $pa .= $type }';
is($pa, '1');

# block method access translation

$pa = pdl(5,3,2);
$c = translate_and_run 'my $method = "dim"; $pa->$method(0)';
is($c, $pa->dim(0));

#
# todo ones
#

# whitespace tolerance

$pa= sequence 10;
translate_and_run '$c = $pa (0)';
is($c, $pa->at(0));

# comment tolerance

translate_and_run << 'EOT';

$c = $pa-> # comment
	 (0);
EOT

is($c, $pa->at(0));

translate_and_run << 'EOT';

$c = $pa-> # comment
          # comment line 2
	 (0);
EOT

is($c, $pa->at(0));

$pa = ''; # foreach and whitespace + comments
translate_and_run << 'EOT';

foreach  my $pb # a random comment thrown in

(1,2,3,4) {$pa .= $pb;}

EOT

is($pa, '1234');

# test for correct header propagation
$pa = ones(10,10);
my $h = {NAXIS=>2,
	 NAXIS1=>100,
	 NAXIS=>100,
	 COMMENT=>"Sample FITS-style header"};
$pa->sethdr($h);
$pa->hdrcpy(1);
translate_and_run '$pb = $pa(1:2,pdl(0,2));';
if ( !$@ ) {
   my %bh = %{$pb->gethdr};
   my (@bhkeys) = sort keys %bh;
   my %hh = %{$h};
   my (@hhkeys) =  sort keys %hh;
   ok(join("",@bh{@bhkeys}) eq join("",@hh{@hhkeys}));
}

$pa = ones(10);
my $ai = translate_and_run 'my $i = which $pa < 0; $pa($i);';
ok(isempty $ai );

{
use PDL::NiceSlice;

my $data = join '', <DATA>;
like $data, qr/we've got data/, "we've got data";
}

done_testing;

__DATA__

we've got data
