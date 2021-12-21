use strict;
use Test::More;
use PDL::LiteF;
use Config;
use PDL::Types;
use Math::Complex ();

sub tapprox ($$) {
    my ( $x, $y ) = @_;
    my $d = abs( $x - $y );
    return $d <= 0.0001;
}

my $a_long = sequence long, 10;
my $a_dbl  = sequence 10;

my $b_long = $a_long->slice('5');
my $b_dbl  = $a_dbl->slice('5');

my $c_long = $a_long->slice('4:7');
my $c_dbl  = $a_dbl->slice('4:7');

# test 'sclr' method
#
is $b_long->sclr, 5, "sclr test of 1-elem pdl (long)";
is $c_long->sclr, 4, "sclr test of 3-elem pdl (long)";

ok tapprox( $b_dbl->sclr, 5 ), "sclr test of 1-elem pdl (dbl)";
ok tapprox( $c_dbl->sclr, 4 ), "sclr test of 3-elem pdl (dbl)";

# switch multielement check on
is( PDL->sclr({Check=>'barf'}), 2, "changed error mode of sclr" );

eval '$c_long->sclr';
like $@, qr/multielement ndarray in 'sclr' call/, "sclr failed on multi-element ndarray (long)";

eval '$c_dbl->sclr';
like $@, qr/multielement ndarray in 'sclr' call/, "sclr failed on multi-element ndarray (dbl)";

# test reshape barfing with negative args
#
eval 'my $d_long = $a_long->reshape(0,-3);';
like $@, qr/invalid dim size/, "reshape() failed with negative args (long)";

eval 'my $d_dbl = $a_dbl->reshape(0,-3);';
like $@, qr/invalid dim size/, "reshape() failed with negative args (dbl)";

# test reshape with no args
my ( $x, $y, $c );

$x = ones 3,1,4;
$y = $x->reshape;
ok eq_array( [ $y->dims ], [3,4] ), "reshape()";

# test reshape(-1) and squeeze
$x = ones 3,1,4;
$y = $x->reshape(-1);
$c = $x->squeeze;
ok eq_array( [ $y->dims ], [3,4] ), "reshape(-1)";
ok all( $y == $c ), "squeeze";

$c++; # check dataflow in reshaped PDL
ok all( $y == $c ), "dataflow"; # should flow back to y
ok all( $x == 2 ), "dataflow";

our $d = pdl(5); # zero dim ndarray and reshape/squeeze
ok $d->reshape(-1)->ndims==0, "reshape(-1) on 0-dim PDL gives 0-dim PDL";
ok $d->reshape(1)->ndims==1, "reshape(1) on 0-dim PDL gives 1-dim PDL";
ok $d->reshape(1)->reshape(-1)->ndims==0, "reshape(-1) on 1-dim, 1-element PDL gives 0-dim PDL";

# reshape test related to bug SF#398 "$pdl->hdr items are lost after $pdl->reshape"
$c = ones(25);
$c->hdr->{demo} = "yes";
is($c->hdr->{demo}, "yes", "hdr before reshape");
$c->reshape(5,5);
is($c->hdr->{demo}, "yes", "hdr after reshape");

# test topdl

isa_ok( PDL->topdl(1),       "PDL", "topdl(1) returns an ndarray" );
isa_ok( PDL->topdl([1,2,3]), "PDL", "topdl([1,2,3]) returns an ndarray" );
isa_ok( PDL->topdl(1,2,3),   "PDL", "topdl(1,2,3) returns an ndarray" );
$x=PDL->topdl(1,2,3);
ok (($x->nelem == 3  and  all($x == pdl(1,2,3))), "topdl(1,2,3) returns a 3-ndarray containing (1,2,3)");


# test $PDL::undefval support in pdl (bug #886263)
#
is $PDL::undefval, 0, "default value of \$PDL::undefval is 0";

$x = [ [ 2, undef ], [3, 4 ] ];
$y = pdl( $x );
$c = pdl( [ 2, 0, 3, 4 ] )->reshape(2,2);
ok all( $y == $c ), "undef converted to 0 (dbl)";
ok eq_array( $x, [[2,undef],[3,4]] ), "pdl() has not changed input array";

$y = pdl( long, $x );
$c = pdl( long, [ 2, 0, 3, 4 ] )->reshape(2,2);
ok all( $y == $c ), "undef converted to 0 (long)";

do {
    local($PDL::undefval) = -999;
    $x = [ [ 2, undef ], [3, 4 ] ];
    $y = pdl( $x );
    $c = pdl( [ 2, -999, 3, 4 ] )->reshape(2,2);
    ok all( $y == $c ), "undef converted to -999 (dbl)";

    $y = pdl( long, $x );
    $c = pdl( long, [ 2, -999, 3, 4 ] )->reshape(2,2);
    ok all( $y == $c ), "undef converted to -999 (long)";
} while(0);

##############
# Funky constructor cases

# pdl of a pdl
$x = pdl(pdl(5));
ok all( $x== pdl(5)), "pdl() can piddlify an ndarray";

# pdl of mixed-dim pdls: pad within a dimension
$x = pdl( zeroes(5), ones(3) );
ok all($x == pdl([0,0,0,0,0],[1,1,1,0,0])),"Piddlifying two ndarrays concatenates them and pads to length" or diag("x=$x\n");

# pdl of mixed-dim pdls: pad a whole dimension
$x = pdl( [[9,9],[8,8]], xvals(3)+1 );
ok all($x == pdl([[[9,9],[8,8],[0,0]] , [[1,0],[2,0],[3,0]] ])),"can concatenate mixed-dim ndarrays" or diag("x=$x\n");

# pdl of mixed-dim pdls: a hairier case
$c = pdl [1], pdl[2,3,4], pdl[5];
ok all($c == pdl([[[1,0,0],[0,0,0]],[[2,3,4],[5,0,0]]])),"Can concatenate mixed-dim ndarrays: hairy case" or diag("c=$c\n");

# same thing, with undefval set differently
do {
    local($PDL::undefval) = 99;
    $c = pdl undef;
    ok all($c == pdl(99)), "explicit, undefval of 99 works" or diag("c=$c\n");
    $c = pdl [1], pdl[2,3,4], pdl[5];
    ok all($c == pdl([[[1,99,99],[99,99,99]],[[2,3,4],[5,99,99]]])), "implicit, undefval works for padding" or diag("c=$c\n");
    $PDL::undefval = undef;
    $c = pdl undef;
    ok all($c == pdl(0)), "explicit, undefval of undef falls back to 0" or diag("c=$c\n");
    $c = pdl [1], [2,3,4];
    ok all($c == pdl([1,0,0],[2,3,4])), "implicit, undefval of undef falls back to 0" or diag("c=$c\n");
    $PDL::undefval = inf;
    $c = pdl undef;
    ok all($c == inf), "explicit, undefval of PDL scalar works" or diag("c=$c\n");
    $c = pdl [1], [2,3,4];
    ok all($c == pdl([1,inf,inf],[2,3,4])), "implicit, undefval of a PDL scalar works" or diag("c=$c\n");
} while(0);

# empty pdl cases
eval {$x = zeroes(2,0,1);};
ok(!$@,"zeroes accepts empty PDL specification");

eval { $y = pdl($x,sequence(2,0,1)); };
ok((!$@ and all(pdl($y->dims) == pdl(2,0,1,2))), "concatenating two empties gives an empty");

eval { $y = pdl($x,sequence(2,1,1)); };
ok((!$@ and all(pdl($y->dims) == pdl(2,1,1,2))), "concatenating an empty and a nonempty treats the empty as a filler");

eval { $y = pdl($x,5) };
ok((!$@ and all(pdl($y->dims)==pdl(2,1,1,2))), "concatenating an empty and a scalar on the right works");
ok( all($y==pdl([[[0,0]]],[[[5,0]]])), "concatenating an empty and a scalar on the right gives the right answer");

eval { $y = pdl(5,$x) };
ok((!$@ and all(pdl($y->dims)==pdl(2,1,1,2))), "concatenating an empty and a scalar on the left works");
ok( all($y==pdl([[[5,0]]],[[[0,0]]])), "concatenating an empty and a scalar on the left gives the right answer");

# end

# cat problems
eval {cat(1, pdl(1,2,3), {}, 6)};
ok ($@ ne '', 'cat barfs on non-ndarray arguments');
like ($@, qr/Arguments 0, 2 and 3 are not ndarrays/, 'cat correctly identifies non-ndarray arguments');
$@ = '';
eval {cat(1, pdl(1,2,3))};
like($@, qr/Argument 0 is not an ndarray/, 'cat uses good grammar when discussing non-ndarrays');
$@ = '';

my $two_dim_array = cat(pdl(1,2), pdl(1,2));
eval {cat(pdl(1,2,3,4,5), $two_dim_array, pdl(1,2,3,4,5), pdl(1,2,3))};
ok ($@ ne '', 'cat barfs on mismatched ndarrays');
like($@, qr/The dimensions of arguments 1 and 3 do not match/
	, 'cat identifies all ndarrays with differing dimensions');
like ($@, qr/\(argument 0\)/, 'cat identifies the first actual ndarray in the arg list');
$@ = '';
eval {cat(pdl(1,2,3), pdl(1,2))};
like($@, qr/The dimensions of argument 1 do not match/
	, 'cat uses good grammar when discussing ndarray dimension mismatches');
$@ = '';
eval {cat(1, pdl(1,2,3), $two_dim_array, 4, {}, pdl(4,5,6), pdl(7))};
ok ($@ ne '', 'cat barfs combined screw-ups');
like($@, qr/Arguments 0, 3 and 4 are not ndarrays/
	, 'cat properly identifies non-ndarrays in combined screw-ups');
like($@, qr/arguments 2 and 6 do not match/
	, 'cat properly identifies ndarrays with mismatched dimensions in combined screw-ups');
like($@, qr/\(argument 1\)/,
	'cat properly identifies the first actual ndarray in combined screw-ups');
$@ = '';

eval {$x = cat(pdl(1),pdl(2,3));};
ok(!$@, 'cat(pdl(1),pdl(2,3)) succeeds');
ok( ($x->ndims==2 and $x->dim(0)==2 and $x->dim(1)==2), 'weird cat case has the right shape');
ok( all( $x == pdl([1,1],[2,3]) ), "cat does the right thing with catting a 0-pdl and 2-pdl together");
$@='';

my $by=xvals(byte,5)+253;
my $so=xvals(short,5)+32766;
my $lo=xvals(long,5)+32766;
my $fl=float(xvals(5)+0.2);
my @list = ($lo,$so,$fl,$by);
my $c2 = cat(@list);
is($c2->type,'float','concatentating different datatypes returns the highest type');
my $i=0;
map{ ok(all($_==$list[$i]),"cat/dog symmetry for values ($i)"); $i++; }$c2->dog;

$x = sequence(byte,5);

$x->inplace;
ok($x->is_inplace,"original item inplace-d true inplace flag");
$y = $x->copy;
ok($x->is_inplace,"original item true inplace flag after copy");
ok(!$y->is_inplace,"copy has false inplace flag");
$y++;
ok(all($y!=sequence(byte,5)),"copy returns severed copy of the original thing if inplace is set");
ok($x->is_inplace,"original item still true inplace flag");
ok(!$y->is_inplace,"copy still false inplace flag");
ok(all($x==sequence(byte,5)),"copy really is severed");

# new_or_inplace
$y = $x->new_or_inplace;
ok( all($y==$x) && ($y->get_datatype ==  $x->get_datatype), "new_or_inplace with no pref returns something like the orig.");

$y++;
ok(all($y!=$x),"new_or_inplace with no inplace flag returns something disconnected from the orig.");

$y = $x->new_or_inplace("float,long");
ok($y->type eq 'float',"new_or_inplace returns the first type in case of no match");

$y = $x->inplace->new_or_inplace;
$y++;
ok(all($y==$x),"new_or_inplace returns the original thing if inplace is set");
ok(!($y->is_inplace),"new_or_inplace clears the inplace flag");

# check reshape and dims.  While we're at it, check null & empty creation too.
my $null = null;
my $empty = zeroes(0);
ok($empty->nelem==0,"you can make an empty PDL with zeroes(0)");
ok("$empty" =~ m/Empty/, "an empty PDL prints 'Empty'");

is $null->info, 'PDL->null', "null ndarray's info is 'PDL->null'";
my $mt_info = $empty->info;
$mt_info =~m/\[([\d,]+)\]/;
my $mt_info_dims = pdl("$1");
ok(any($mt_info_dims==0), "empty ndarray's info contains a 0 dimension");
ok($null->isnull, "a null ndarray is null");
ok($null->isempty, "a null ndarray is empty") or diag $null->info;
ok(!$empty->isnull, "an empty ndarray is not null");
ok($empty->isempty, "an empty ndarray is empty");

$x = short pdl(3,4,5,6);
eval { $x->reshape(2,2);};
ok(!$@,"reshape succeeded in the normal case");
ok( ( $x->ndims==2 and $x->dim(0)==2 and $x->dim(1)==2 ), "reshape did the right thing");
ok(all($x == short pdl([[3,4],[5,6]])), "reshape moved the elements to the right place");

$y = $x->slice(":,:");
eval { $y->reshape(4); };
ok( $@ !~ m/Can\'t/, "reshape doesn't fail on a PDL with a parent" );

{
my $pb = double ones(2,3);
my $ind = 1;
is(($pb->dims)[0], 2);
is(($pb->dims)[1], 3);
note $pb;
is($pb->at(1,1), 1);
is($pb->at(1,2), 1);
}

my $array = [
 [[1,2],
  [3,4]],
 [[5,6],
  [7,8]],
 [[9,10],
  [11,12]]
];
my $pdl = pdl $array;
is_deeply( unpdl($pdl), $array, "back convert 3d");
SKIP: {
  skip("your perl hasn't 64bit int support", 6) if $Config{ivsize} < 8;
  my $input = [
      -9223372036854775808, #min int64
      -9000000000000000001,
      -9000000000000000002,
      -9000000000000000003,
      -9000000000000000004,
      -9000000000000000005,
      -8999999999999999999,
      -8999999999999999998,
      -8999999999999999997,
      -8999999999999999996,
      -1000000000000000001,
               -2147483648, #min int32
                2147483647, #max int32
                4294967295, #max uint32
       1000000000000000001,
       9000000000000000001,
       9000000000000000002,
       9000000000000000003,
       9000000000000000004,
       9000000000000000005,
       8999999999999999999,
       8999999999999999998,
       8999999999999999997,
       8999999999999999996,
       9223372036854775807, #max int64
  ];
  is_deeply(longlong($input)->unpdl, $input, 'back convert of 64bit integers');
  my $small_pdl = longlong([ -9000000000000000001, 9000000000000000001 ]);
  is($small_pdl->at(0), -9000000000000000001, 'at/1');
  is(PDL::Core::at_c($small_pdl, [1]),  9000000000000000001, 'at_c back-compat');
  is(PDL::Core::at_bad_c($small_pdl, [1]),  9000000000000000001, 'at_bad_c/1');
  $small_pdl->set(0, -8888888888888888888);
  PDL::Core::set_c($small_pdl, [1], 8888888888888888888);
  is($small_pdl->at(0), -8888888888888888888, 'at/2');
  is(PDL::Core::at_bad_c($small_pdl, [1]),  8888888888888888888, 'at_bad_c/2');
  is_deeply($small_pdl->unpdl, [ -8888888888888888888, 8888888888888888888 ], 'unpdl/small_pdl');
}

my $big_ushort = ushort(65535);
is $big_ushort->badflag, 0, 'max ushort value badflag';
is PDL::Core::at_bad_c($big_ushort, []), 65535, 'max ushort value not "BAD" per se';

{
my $x = cdouble(2, 3);
PDL::Core::set_c($x, [1], i);
is $x.'', '[2 i]', 'set_c can take ndarray value';
}

{
my $x = cdouble(2, Math::Complex::i());
is $x.'', '[2 i]', 'type constructor can take Math::Complex value';
$x = pdl(Math::Complex::cplx(2, 0), Math::Complex::i());
is $x.'', '[2 i]', 'pdl defaults to cdouble if Math::Complex values';
$x = pdl([Math::Complex::cplx(2, 0), Math::Complex::i()]);
is $x.'', '[2 i]', 'pdl defaults to cdouble if Math::Complex values in arrayref';
}

sub hdr_test {
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    my ($pb, $hdr, $method) = @_;
    $method ||= 'gethdr';
    note "pb: ", explain my $pbh=$pb->$method;
    is_deeply($pbh,$hdr);
}

{
my $pa = zeroes(20);
$pa->hdrcpy(1);
my $hdr = {Field1=>'arg1', Field2=>'arg2'};
$pa->sethdr($hdr);
note "pa: ", explain $pa->gethdr();
ok($pa->hdrcpy);
hdr_test($pa+1, $hdr);
hdr_test(ones(20) + $pa, $hdr);
hdr_test($pa->slice('0:5'), $hdr);
hdr_test($pa->copy, $hdr);
$pa->hdrcpy(0);
hdr_test($pa->slice('3'), {}, 'hdr');
hdr_test($pa->slice('3'), undef);
}

{
my $pa = pdl 42.4;
note "A is $pa";

is($pa->get_datatype,$PDL_D, "A is double");

my $pb = byte $pa;
note "B (byte $pa) is $pb";

is($pb->get_datatype,$PDL_B, "B is byte");
is($pb->at(),42, 'byte value is 42');

my $pc = $pb * 3;
is($pc->get_datatype, $PDL_B, "C also byte");
note "C ($pb * 3) is $pc";

my $pd = $pb * 600.0;
is($pd->get_datatype, $PDL_F, "D promoted to float");
note "D ($pb * 600) is $pd";

my $pi = 4*atan2(1,1);

my $pe = $pb * $pi;
is($pe->get_datatype, $PDL_D, "E promoted to double (needed to represent result)");
note "E ($pb * PI) is $pe";

my $pf = $pb * "-2.2";
is($pf->get_datatype, $PDL_D, "F check string handling");
note "F ($pb * string(-2.2)) is $pf";
}

{
my @types = (
	{ typefunc => *byte  , size => 1 },
	{ typefunc => *short , size => 2 },
	{ typefunc => *ushort, size => 2 },
	{ typefunc => *long  , size => 4 },
	{ typefunc => *float , size => 4 },
	{ typefunc => *double, size => 8 },
);
for my $type (@types) {
	my $pdl = $type->{typefunc}(42); # build a PDL with datatype $type->{type}
	is( PDL::Core::howbig( $pdl->get_datatype ), $type->{size} );
}
}

for (['ones', 1], ['zeroes', 0], ['nan', 'NaN'], ['inf', 'Inf'], ['i', 'i', 'cdouble']) {
  my ($name, $val, $type) = @$_;
  no strict 'refs';
  my $g = eval { $name->() };
  is $@, '', "$name works with no args";
  is_deeply [$g->dims], [], 'no args -> no dims';
  ok !$g->isnull, 'no args -> not null';
  ok !$g->isempty, 'no args -> not empty';
  like $g.'', qr/^$val/i, "$name() gives back right value";
  my $g1 = eval { $name->(2) };
  is $@, '', "$name works with 1 args";
  is_deeply [$g1->dims], [2], 'right dims';

  # from PDL::Core docs of zeroes
  my (@dims, $w) = (1..3);
  $w = $name->(byte, @dims); is_deeply [$w->dims], \@dims; is $w->type, $type || 'byte';
  $w = $name->(@dims); is_deeply [$w->dims], \@dims; is $w->type, $type || 'double';
  $w = PDL->$name(byte, @dims); is_deeply [$w->dims], \@dims; is $w->type, $type || 'byte';
  $w = PDL->$name(@dims); is_deeply [$w->dims], \@dims; is $w->type, $type || 'double';
  my $pdl = ones(float, 4, 5);
  $w = $pdl->$name(byte, @dims); is_deeply [$w->dims], \@dims; is $w->type, $type || 'byte';
  # usage type (ii):
  my $y = ones(@dims);
  $w = $name->($y); is_deeply [$w->dims], \@dims;
  $w = $y->$name; is_deeply [$w->dims], \@dims;
  next if $val =~ /\D/;
  $w = $y->copy; $name->(inplace $w); ok all tapprox $w, pdl($val) or diag "$name got:$w";
  $w = $y->copy; $w->inplace->$name; ok all tapprox $w, pdl($val);
}

is short(1)->zeroes->type, 'short', '$existing->zeroes right type';

eval { PDL->is_inplace }; # shouldn't infinite-loop
isnt $@, '', 'is_inplace as class method throws exception';

is sequence(3)->get_trans, undef, 'get_trans without trans undef';
isnt sequence(3)->slice()->get_trans, undef, 'get_trans with trans defined';

done_testing;
