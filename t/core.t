use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL::LiteF;
use PDL::Math; # for polyroots with [phys] params, for dim compat tests
use PDL::MatrixOps; # for simq with [phys] params, for dim compat tests
use Config;
use PDL::Types;
use Math::Complex ();
use Devel::Peek;

sub tapprox ($$) {
    my ( $x, $y ) = @_;
    my $d = abs( $x - $y );
    return $d <= 0.0001;
}

for my $type (PDL::Types::types()) {
   ok defined pdl($type, 0), "constructing PDL of type $type";
}

{
my $p = sequence(100); # big enough to not fit in "value" field
my $ref = $p->get_dataref;
$p->reshape(3); # small enough now
$p->upd_data;
}

{
  my $pa = pdl 2,3,4;
  $pa->flowing;
  my $pb = $pa + $pa;
  is "$pb", '[4 6 8]';
  $pa->set(0,50);
  is "$pb", '[100 6 8]';
  eval {$pa->set_datatype(PDL::float()->enum)};
  like $@, qr/ndarray has child/, 'set_datatype if has child dies';
  $pb->set_datatype(PDL::float()->enum);
  $pa->set(0,60);
  is "$pb", '[100 6 8]', 'dataflow broken by set_datatype';
}

eval {PDL->inplace};
like $@, qr/called object method/, 'error on PDL->obj_method';

{
my $p = sequence(3);
my $p2 = sequence(2);
eval {$p->set(1,$p2)};
isnt $@, '', 'set(..., $multi_elt) should error';
}

{
my $p = sequence(5);
is Devel::Peek::SvREFCNT($p), 1, 'right refcnt blessed ref';
is Devel::Peek::SvREFCNT($$p), 1, 'right refcnt pointer SV';
}

for (@PDL::Core::EXPORT_OK) {
  next if $_ eq 'mslice'; # bizarrely, this is callable but not "defined"
  no strict 'refs';
  ok defined &{"PDL::Core::$_"}, "PDL::Core-exported $_ exists";
}

is sequence(3,2)->dup(0, 2).'', '
[
 [0 1 2 0 1 2]
 [3 4 5 3 4 5]
]
', 'dup';

is sequence(3,2)->dupN(2, 3).'', '
[
 [0 1 2 0 1 2]
 [3 4 5 3 4 5]
 [0 1 2 0 1 2]
 [3 4 5 3 4 5]
 [0 1 2 0 1 2]
 [3 4 5 3 4 5]
]
', 'dupN';

is sequence(3,2)->inflateN(2, 3).'', '
[
 [0 0 1 1 2 2]
 [0 0 1 1 2 2]
 [0 0 1 1 2 2]
 [3 3 4 4 5 5]
 [3 3 4 4 5 5]
 [3 3 4 4 5 5]
]
', 'inflateN';

my $a_long = sequence long, 10;
my $a_dbl  = sequence 10;

my $b_long = $a_long->slice('5');
my $b_dbl  = $a_dbl->slice('5');

my $c_long = $a_long->slice('4:7');
my $c_dbl  = $a_dbl->slice('4:7');

# test 'sclr' method
#
is $b_long->sclr, 5, "sclr test of 1-elem pdl (long)";

ok tapprox( $b_dbl->sclr, 5 ), "sclr test of 1-elem pdl (dbl)";

eval { $c_long->sclr };
like $@, qr/multielement ndarray in 'sclr' call/, "sclr failed on multi-element ndarray (long)";

eval { $c_dbl->sclr };
like $@, qr/multielement ndarray in 'sclr' call/, "sclr failed on multi-element ndarray (dbl)";

eval { zeroes(0)->max ? 1 : 0 };
like $@, qr/bad.*conditional/, 'badvalue as boolean is error';

# test reshape barfing with negative args
#
eval { my $d_long = $a_long->reshape(0,-3) };
like $@, qr/invalid dim size/, "reshape() failed with negative args (long)";

eval { my $d_dbl = $a_dbl->reshape(0,-3) };
like $@, qr/invalid dim size/, "reshape() failed with negative args (dbl)";

eval { my $y = zeroes(1,3); $y .= sequence(2,3); };
isnt $@, '', 'scaling-up of output dim 1 throws error';
eval { my $y = zeroes(1); $y .= zeroes(0) + 1; };
isnt $@, '', 'scaling-down of output dim 1 throws error';

# test reshape with no args
my $x = ones 3,1,4;
my $y = $x->reshape;
ok eq_array( [ $y->dims ], [3,4] ), "reshape()";

# test reshape(-1) and squeeze
$x = ones 3,1,4;
$y = $x->reshape(-1);
my $c = $x->squeeze;
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

eval {empty->squeeze->dims};
is $@, '', 'can "squeeze" an empty';
eval {empty->copy->make_physical};
is $@, '', 'can physicalise the copy of an empty';

# capture ancient pptest.t test for Solaris segfault
ok all(tapprox(norm(pdl 3,4), pdl(0.6,0.8))), 'vector quasi-copy works';
# pptest for null input
eval {(my $tmp=null) .= null}; like $@, qr/input.*null/;
# pptest for OtherPars=>named dim
ok all(tapprox((5*sequence(5))->maximum_n_ind(3), pdl(4,3,2))), 'named dim';
# pptest for dim with fixed value
ok all(tapprox(crossp([1..3],[4..6]), pdl(-3,6,-3))), 'named dim=3';

subtest 'dim compatibility' => sub {
  for (
    # non-phys params
    [\&append, [zeroes(1), zeroes(1), zeroes(1)], 2, qr/dim has size 1/, 'output=[1]; required [2]. output too small'],
    [\&append, [pdl(1), pdl(2), null], 2, [ 1, 2 ], 'output=null; required [2]'],
    [\&append, [pdl(1), pdl(2), zeroes(2)], 2, [ 1, 2 ], 'output=[2]; required [2]'],
    [\&append, [zeroes(1), zeroes(1), zeroes(3)], 2, qr/dim has size 3/, 'output=[3]; required [2]. output too large'],
    [\&append, [zeroes(1), zeroes(0), zeroes()], 2, [0], 'output=scalar; required [1]'],
    [\&append, [zeroes(1), zeroes(1), zeroes()], 2, qr/can't broadcast/, 'output=scalar; required [2]. output too small'],
    [\&append, [zeroes(1), zeroes(1), zeroes(1,1)], 2, qr/dim has size 1/, 'output=[1,1]; required [2]. output too small'],
    [\&append, [pdl(1),    pdl(2),    zeroes(2,1)], 2, [[ 1, 2 ]], 'output=[2,1]; required [2]'],
    [\&append, [zeroes(1), zeroes(1), zeroes(3,1)], 2, qr/dim has size 3/, 'output=[3,1]; required [2]. output too large'],
    [\&append, [zeroes(1), zeroes(1), zeroes(1,2)], 2, qr/dim has size 1/, 'output=[1,2]; required [2]. output too small'],
    [\&append, [zeroes(1), zeroes(1), zeroes(2,2)], 2, [[ 0, 0 ], [ 0, 0 ]], 'output=[2,2]; required [2]. input without that dim broadcasted up'],
    [\&append, [zeroes(1,2), zeroes(1), zeroes(2,2)], 2, [[ 0, 0 ], [ 0, 0 ]], 'output=[2,2]; required [2]. one input without that dim broadcasted up'],
    [\&append, [zeroes(1,3), zeroes(1), zeroes(2,2)], 2, qr/Mismatch/, 'input=[1,3] output=[2,2]. input with mismatched broadcast dim'],
    [\&append, [zeroes(1,2), zeroes(1), zeroes(2,1)], 2, qr/implicit dim/, 'output=[2,1]; required [2,2]. output too small in broadcast dim'],
    [\&append, [zeroes(1,2), zeroes(1), zeroes(2)], 2, qr/implicit dim/, 'output=[2,1]; required [2,2]. output too small in broadcast implicit dim'],
    [\&append, [zeroes(1,2), zeroes(1,2), zeroes(2,1)], 2, qr/implicit dim/, 'output=[2,1]; required [2,2]. output too small in broadcast dim'],
    [\&append, [zeroes(1,2), zeroes(1,2), zeroes(2)->dummy(1,2)], 2, qr/implicit dim/, 'output=[2,*2]; required [2,2]. output into dummy implicit dim'],
    [\&append, [zeroes(1,2), zeroes(1,2), zeroes(2)->dummy(1,2)->make_physical], 2, qr/implicit dim/, 'output=[2,*2](phys); required [2,2]. output into dummy implicit dim'],
    [\&append, [zeroes(1,2), zeroes(1,2), zeroes(2)->dummy(0,2)], 2, qr/over dummy dim/, 'output=[*2,2]; required [2,2]. output into dummy active dim'],
    [\&append, [zeroes(1,2), zeroes(1,2), zeroes(2)->dummy(0,2)->make_physical], 2, qr/over dummy dim/, 'output=[*2,2](phys); required [2,2]. output into dummy active dim'],
    # phys params
    [\&polyroots, [ones(2), zeroes(2), zeroes(1), zeroes(1)], 2, [-1], '[phys] output=[1]'],
    [\&polyroots, [ones(2), zeroes(1), zeroes(), zeroes(1)], 2, qr/dim has size 1/, '[phys] output=[2] mismatch'],
    [\&polyroots, [ones(2), zeroes(1), zeroes(1), zeroes(1)], 2, qr/dim has size 1/, '[phys] output=[2] mismatch'],
    [\&polyroots, [ones(2), zeroes(2), zeroes(2), zeroes(2)], 2, qr/dim has size 2/, '[phys] output=[2] mismatch'],
    [\&polyroots, [ones(2), zeroes(2), zeroes(1,2), zeroes(1)], 2, qr/implicit dim/, '[phys] one outputs=[1,2],[1] no promote output implicit dims'],
    [\&polyroots, [ones(2), zeroes(2,2), zeroes(1,2), zeroes(1,2)], 2, [[-1],[-1]], '[phys] output=[1,2] ok broadcast over input'],
    [\&polyroots, [ones(2), zeroes(2,2), zeroes(1), zeroes(1,2)], 2, qr/implicit dim/, '[phys] output=[1,2] not ok broadcast over output implicit dim'],
    [\&polyroots, [ones(2), zeroes(2,2), zeroes(1,1), zeroes(1,2)], 2, qr/implicit dim/, '[phys] outputs=[1,1],[1,2] not ok broadcast over output explicit dim'],
    # phys params with (n,n)
    [\&simq, [identity(3)+1, sequence(3,1), null, null, 0], 2, [[-0.75,0.25,1.25]], '[phys] output=[3,3]'],
    [\&simq, [[[2,1,1]], sequence(3,1), null, null, 0], 2, qr/dim has size/, '[phys] input=[3,1] output=[3,3] no expand input phys multi-used dim of 1'],
    [\&simq, [identity(3)+1, sequence(3,2), null, null, 0], 2, qr/implicit dim/, '[phys] inputs:n,n=[3,3],n=[3,2] no broadcast over [io]'],
  ) {
    my ($func, $args, $exp_index, $exp, $label) = @$_;
    if (ref $exp eq 'Regexp') {
      throws_ok { $func->( @$args ) } $exp, $label;
    } else {
      $func->( @$args );
      my $got = $args->[$exp_index];
      ok all(tapprox $got, pdl($exp)), $label or diag $got;
    }
  }
};

# test topdl

{ package # hide from PAUSE
  PDL::Trivial;
our @ISA = qw(PDL);
sub new {bless {PDL=>PDL->SUPER::new(@_[1..$#_])}} # like PDL::DateTime
}
my $subobj = PDL::Trivial->new(6);
isa_ok $subobj, 'PDL::Trivial';
isa_ok +PDL->topdl($subobj), 'PDL::Trivial';
isa_ok $subobj->inplace, 'PDL::Trivial';
isa_ok( PDL->topdl(1),       "PDL", "topdl(1) returns an ndarray" );
isa_ok( PDL->topdl([1,2,3]), "PDL", "topdl([1,2,3]) returns an ndarray" );
isa_ok( PDL->topdl(1,2,3),   "PDL", "topdl(1,2,3) returns an ndarray" );
$x=PDL->topdl(1,2,3);
ok (($x->nelem == 3  and  all($x == pdl(1,2,3))), "topdl(1,2,3) returns a 3-ndarray containing (1,2,3)");
eval {PDL->topdl({})};
isnt $@, '', 'topdl({}) no segfault';

# stringification
{
my $x = sequence( 3 + 1e7 );
my $x_indx = which( $x > 1e7 - 4 );
is $x_indx.'', "[9999997 9999998 9999999 10000000 10000001 10000002]";
my $x_indx_bad = $x_indx->copy;
$x_indx_bad->setbadat($_) for 1, 4;
is $x_indx_bad.'', "[9999997 BAD 9999999 10000000 BAD 10000002]";
is +($x_indx - 10).'', "[9999987 9999988 9999989 9999990 9999991 9999992]";
is +($x_indx)->splitdim(0,3).'', "\n[\n [     9999997      9999998      9999999]\n [    10000000     10000001     10000002]\n]\n";
is +($x_indx - 10)->splitdim(0,3).'', "\n[\n [9999987 9999988 9999989]\n [9999990 9999991 9999992]\n]\n";
is +($x_indx_bad)->splitdim(0,3).'', "\n[\n [     9999997          BAD      9999999]\n [    10000000          BAD     10000002]\n]\n";
is +($x_indx_bad - 10)->splitdim(0,3).'', "\n[\n [9999987     BAD 9999989]\n [9999990     BAD 9999992]\n]\n";
my $x_double = where( $x, $x > 1e7 - 4 );
is $x_double.'', "[9999997 9999998 9999999 10000000 10000001 10000002]";
is +($x_double - 10).'', "[9999987 9999988 9999989 9999990 9999991 9999992]";
is +($x_double)->splitdim(0,3).'', "\n[\n [   9999997    9999998    9999999]\n [  10000000   10000001   10000002]\n]\n";
is +($x_double - 10)->splitdim(0,3).'', "\n[\n [9999987 9999988 9999989]\n [9999990 9999991 9999992]\n]\n";
my $x_long = where( long($x), $x > 1e7 - 4 );
is $x_long.'', "[9999997 9999998 9999999 10000000 10000001 10000002]";
is +($x_long - 10).'', "[9999987 9999988 9999989 9999990 9999991 9999992]";
is +($x_long)->splitdim(0,3).'', "\n[\n [ 9999997  9999998  9999999]\n [10000000 10000001 10000002]\n]\n";
is +($x_long - 10)->splitdim(0,3).'', "\n[\n [9999987 9999988 9999989]\n [9999990 9999991 9999992]\n]\n";
my $fracs = sequence(9) / 16;
is $PDL::doubleformat, "%10.8g";
is $fracs.'', "[0 0.0625 0.125 0.1875 0.25 0.3125 0.375 0.4375 0.5]";
is $fracs->string($PDL::doubleformat).'', "[         0     0.0625      0.125     0.1875       0.25     0.3125      0.375     0.4375        0.5]";
{
local $PDL::doubleformat = '%8.2g';
is $fracs.'', "[0 0.0625 0.125 0.1875 0.25 0.3125 0.375 0.4375 0.5]";
is $fracs->string($PDL::doubleformat).'', "[       0    0.062     0.12     0.19     0.25     0.31     0.38     0.44      0.5]";
}

# from Data::Frame
{
  my $_pdl_stringify_temp = PDL::Core::pdl([[0]]);
  my $_pdl_stringify_temp_single = PDL::Core::pdl(0);
  sub element_stringify {
    my ($self, $element) = @_;
    return $_pdl_stringify_temp_single->set(0, $element)->string if $self->ndims == 0;
    # otherwise
    ( $_pdl_stringify_temp->set(0,0, $element)->string =~ /\[(.*)\]/ )[0];
  }
}
sub element_stringify_max_width {
  my ($self) = @_;
  my @vals = @{ $self->uniq->unpdl };
  my @lens = map { length element_stringify($self, $_) } @vals;
  max( pdl @lens )->sclr;
}
for (1.23456789, 1.2345678901, 1.23456789012) {
  my $ndim = length( pdl([ $_ ])->string ) - 2;
  is element_stringify_max_width(pdl([ $_ ])), $ndim, "length right for [$_]";
  is element_stringify_max_width(pdl([[ $_ ]])), $ndim, "length right for [[$_]]";
}
}

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

$x = pdl(null);
ok $x->isnull, 'pdl(null) gives null' or diag "x(", $x->info, ")";

$x = pdl(null, null);
is_deeply [$x->dims], [0,2], 'pdl(null, null) gives empty' or diag "x(", $x->info, ")";
ok !$x->isnull, 'pdl(null, null) gives non-null' or diag "x(", $x->info, ")";

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
is($@, '', "zeroes accepts empty PDL specification");

eval { $y = pdl($x,sequence(2,0,1)); };
is $@, '';
ok all(pdl($y->dims) == pdl(2,0,1,2)), "concatenating two empties gives an empty";

eval { $y = pdl($x,sequence(2,1,1)); };
is $@, '';
ok all(pdl($y->dims) == pdl(2,1,1,2)), "concatenating an empty and a nonempty treats the empty as a filler";

eval { $y = pdl($x,5) };
is $@, '';
ok all(pdl($y->dims)==pdl(2,1,1,2)), "concatenating an empty and a scalar on the right works";

eval { $y = pdl(5,$x) };
is $@, '';
ok all(pdl($y->dims)==pdl(2,1,1,2)), "concatenating an empty and a scalar on the left works";
ok( all($y==pdl([[[5,0]]],[[[0,0]]])), "concatenating an empty and a scalar on the left gives the right answer");

# cat problems
eval {cat(1, pdl(1,2,3), {}, 6)};
isnt($@, '', 'cat barfs on non-ndarray arguments');
like ($@, qr/Arguments 0, 2 and 3 are not ndarrays/, 'cat correctly identifies non-ndarray arguments');
eval {cat(1, pdl(1,2,3))};
like($@, qr/Argument 0 is not an ndarray/, 'cat uses good grammar when discussing non-ndarrays');

my $two_dim_array = cat(pdl(1,2), pdl(1,2));
eval {cat(pdl(1,2,3,4,5), $two_dim_array, pdl(1,2,3,4,5), pdl(1,2,3))};
isnt($@, '', 'cat barfs on mismatched ndarrays');
like($@, qr/The dimensions of arguments 1 and 3 do not match/
	, 'cat identifies all ndarrays with differing dimensions');
like ($@, qr/\(argument 0\)/, 'cat identifies the first actual ndarray in the arg list');
eval {cat(pdl(1,2,3), pdl(1,2))};
like($@, qr/The dimensions of argument 1 do not match/
	, 'cat uses good grammar when discussing ndarray dimension mismatches');
eval {cat(1, pdl(1,2,3), $two_dim_array, 4, {}, pdl(4,5,6), pdl(7))};
isnt($@, '', 'cat barfs combined screw-ups');
like($@, qr/Arguments 0, 3 and 4 are not ndarrays/
	, 'cat properly identifies non-ndarrays in combined screw-ups');
like($@, qr/arguments 2 and 6 do not match/
	, 'cat properly identifies ndarrays with mismatched dimensions in combined screw-ups');
like($@, qr/\(argument 1\)/,
	'cat properly identifies the first actual ndarray in combined screw-ups');

eval {$x = cat(pdl(1),pdl(2,3));};
is($@, '', 'cat(pdl(1),pdl(2,3)) succeeds');
is_deeply [$x->dims], [2,2], 'weird cat case has the right shape';
ok( all( $x == pdl([1,1],[2,3]) ), "cat does the right thing with catting a 0-pdl and 2-pdl together");

my $lo=sequence(long,5)+32766;
my $so=sequence(short,5)+32766;
my $fl=sequence(float,5)+float(0.2); # 0.2 is an NV so now a double
my $by=sequence(byte,5)+253;
my @list = ($lo,$so,$fl,$by);
my $c2 = cat(@list);
is($c2->type,'float','concatenating different datatypes returns the highest type');
ok(all($_==shift @list),"cat/dog symmetry for values") for $c2->dog;
my ($dogcopy) = $c2->dog({Break=>1});
$dogcopy++;
ok all($dogcopy != $c2->slice(':,(0)')), 'Break means copy'; # not lo as cat no flow
my ($dogslice) = $c2->dog;
$dogslice->dump;
$lo->dump;
$dogslice++;
ok all($dogslice == $c2->slice(':,(0)')), 'no Break means dataflow' or diag "got=$dogslice\nexpected=$lo";

$x = sequence(byte,5);

$x->inplace;
ok($x->is_inplace,"original item inplace-d true inplace flag");
eval { $x->inplace(1) };
is $@, '', 'passing spurious extra args no error';
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
my $empty = empty();
is $empty->type->enum, 0, 'empty() gives lowest-numbered type';
is empty(float)->type, 'float', 'empty(float) works';
ok($empty->nelem==0,"you can make an empty PDL with zeroes(0)");
ok("$empty" =~ m/Empty/, "an empty PDL prints 'Empty'");

my $null = null;
is $null->nbytes, 0, 'a null has 0 nbytes';
is $null->info, 'PDL->null', "null ndarray's info is 'PDL->null'";
my $mt_info = $empty->info;
$mt_info =~m/\[([\d,]+)\]/;
my $mt_info_dims = pdl("$1");
ok(any($mt_info_dims==0), "empty ndarray's info contains a 0 dimension");
{
    is($PDL::infoformat, "%C: %T %D", "check default info format");
    local $PDL::infoformat = "default info format for %C";
    is(pdl(2, 3)->info, "default info format for PDL",
        "use default info format");
}
ok($null->isnull, "a null ndarray is null");
ok($null->isempty, "a null ndarray is empty") or diag $null->info;
ok(!$empty->isnull, "an empty ndarray is not null");
ok($empty->isempty, "an empty ndarray is empty");
eval { $null->long };
like $@, qr/null/, 'null->long gives right error';

$x = short pdl(3,4,5,6);
eval { $x->reshape(2,2);};
is($@, '', "reshape succeeded in the normal case");
ok( ( $x->ndims==2 and $x->dim(0)==2 and $x->dim(1)==2 ), "reshape did the right thing");
ok(all($x == short pdl([[3,4],[5,6]])), "reshape moved the elements to the right place");

$y = $x->slice(":,:");
eval { $y->reshape(4); };
ok( $@ !~ m/Can\'t/, "reshape doesn't fail on a PDL with a parent" );

{
my $pb = sequence(2,3);
is(($pb->dims)[0], 2);
is(($pb->dims)[1], 3);
note $pb;
is $pb->at(1,1), 3;
is $pb->at(1,2), 5;
eval {$pb->at(2,1)};
like $@, qr/Position 2 at dimension 0 out of range/;
is $pb->at(-1,2), 5;
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
  {
  my $neg = -684394069604;
  my $straight_pdl = pdl($neg);
  my $multed = pdl(1) * $neg;
  ok $straight_pdl == $multed, 'upgrade of large negative SV to ndarray'
    or diag "straight=$straight_pdl mult=$multed\n",
      "straight:", $straight_pdl->info, " mult:", $multed->info;
  }
  {
  my $fromuv_r = pdl('10223372036854775507');
  ok $fromuv_r > 0, 'UV real > 0';
  my $fromuv_c = pdl('10223372036854775507i');
  ok $fromuv_c->im > 0, 'UV complex->real > 0'
    or diag "fromuv_c=$fromuv_c\nfromuv_c->im=", $fromuv_c->im,
      "\nfromuv_r=$fromuv_r";
  $fromuv_c = pdl('2+10223372036854775507i');
  ok $fromuv_c->im > 0, 'UV complex->real > 0 with some real'
    or diag "fromuv_c=$fromuv_c\nfromuv_c->im=", $fromuv_c->im,
      "\nfromuv_r=$fromuv_r";
  }
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
is($pd->get_datatype, $PDL_D, "pdl-ed NV is double, D promoted to double");
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
for my $type (
  { typefunc => *byte  , size => 1 },
  { typefunc => *short , size => 2 },
  { typefunc => *ushort, size => 2 },
  { typefunc => *long  , size => 4 },
  { typefunc => *float , size => 4 },
  { typefunc => *double, size => 8 },
) {
  my $pdl = $type->{typefunc}(42); # build a PDL with datatype $type->{type}
  is( PDL::Core::howbig( $pdl->get_datatype ), $type->{size} );
  is $pdl->type, $type->{typefunc}->().'', 'pdl has right type';
  is $pdl->convert(longlong).'', 42, 'converted to longlong same value';
  $pdl->inplace->convert(longlong);
  is $pdl->type, 'longlong', 'pdl has new right type, inplace convert worked';
}
}

for (['ones', 1], ['zeroes', 0], ['nan', '.*NaN'], ['inf', '.*Inf'], ['i', 'i', 'cdouble']) {
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
  ok $w->allocated, "$name(type, dims) is allocated";
  $w = $name->(@dims); is_deeply [$w->dims], \@dims; is $w->type, $type || 'double';
  ok $w->allocated, "$name(dims) is allocated";
  $w = PDL->$name(byte, @dims); is_deeply [$w->dims], \@dims; is $w->type, $type || 'byte';
  ok $w->allocated, "PDL->$name(type, dims) is allocated";
  $w = PDL->$name(@dims); is_deeply [$w->dims], \@dims; is $w->type, $type || 'double';
  ok $w->allocated, "PDL->$name(dims) is allocated";
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

my $s = sequence(3);
is $s->trans_parent, undef, 'trans_parent without trans undef';
my $slice = $s->slice;
isnt +(my $tp=$slice->trans_parent), undef, 'trans_parent with trans defined';
is ${($s->trans_children)[0]}, $$tp, 'correct trans_children';
my @parents = $tp->parents;
is ${$parents[0]}, $s->address, 'correct parent ndarray';
my @children = $tp->children;
is ${$children[0]}, $slice->address, 'correct child ndarray';
my $vtable = $tp->vtable;
isnt $vtable->name, undef, 'trans vtable has a name';
isnt PDL::Core::pdump($slice), undef, 'pdump works';
isnt PDL::Core::pdump_trans($tp), undef, 'pdump_trans works';
isnt PDL::Core::pdumphash($slice), undef, 'pdumphash works with ndarray';
isnt PDL::Core::pdumphash($tp), undef, 'pdumphash works with trans';
my @pn = $vtable->par_names;
is 0+@pn, 2, 'par_names returned 2 things';
my $roots = pdl '[1 2i 3i 4i 5i]';
eval {PDL::Core::pdump($roots)}; # gave "panic: attempt to copy freed scalar"
is $@, '';
{
my $x = sequence 3; my $y = $x->flowing + 1;
isnt $y->trans_parent, undef, '$y has parent';
isnt PDL::Core::pdumphash($x), undef, 'pdumphash works';
isnt $y->trans_parent, undef, '$y still has parent after pdumphash';
$x += 3;
is "$x", "[3 4 5]", '$x right value';
is "$y", "[4 5 6]", '$y right value';
ok !$y->fflows, 'y not "flowing"';
$y->flowing;
ok $y->fflows, 'y "flowing" on';
my $z = $y + 1;
isnt $z->trans_parent, undef, 'z has trans_parent';
ok !$y->fflows, 'y "flowing" off again';
eval {$y += 4};
isnt $@, '', 'error on assigning to ndarray with inward but no outward flow';
my $oneway_slice = $y->slice('0:1');
is "$oneway_slice", '[4 5]';
eval {$oneway_slice .= 11};
isnt $@, '', 'error on assigning into one-way slice';
}

my $notouch = sequence(4);
$notouch->set_donttouchdata(4 * PDL::Core::howbig($notouch->get_datatype));
eval { $notouch->setdims([2,2]); $notouch->make_physical; };
is $@, '', 'setdims to same total size of set_donttouchdata should be fine';
eval { $notouch->setdims([3,2]); $notouch->make_physical; };
isnt $@, '', 'setdims/make_physical to different size of set_donttouchdata should fail';
my $sliced = sequence(4)->slice('');
eval { $sliced->setdims([3,2]) };
like $@, qr/but has trans_parent/, 'setdims on pdl with trans_parent is error';

eval { pdl(3)->getbroadcastid($_) }, isnt $@, '', "getbroadcastid($_) out of range gives error" for -2, 5;

done_testing;
