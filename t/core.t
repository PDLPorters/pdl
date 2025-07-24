use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL::LiteF;
use Test::PDL;
use PDL::Math; # for polyroots with [phys] params, for dim compat tests
use PDL::MatrixOps; # for simq with [phys] params, for dim compat tests
use Config;
use PDL::Types;
use Math::Complex ();
use Devel::Peek;

for my $type (PDL::Types::types()) {
   ok defined pdl($type, 0), "constructing PDL of type $type";
   ok $type->howbig, "$type has size";
}

{
my $p = sequence(100); # big enough to not fit in "value" field
my $ref = $p->get_dataref;
$p->reshape(3); # small enough now
$p->upd_data;
is_pdl $p, pdl('0 1 2');
my $other_numbers = (sequence(3)+6)->get_dataref;
$p->update_data_from($$other_numbers);
is_pdl $p, pdl('6 7 8');
$other_numbers = sequence(4)->get_dataref;
eval {$p->update_data_from($$other_numbers)};
like $@, qr/but sv length/, 'error if update_data_from wrong size';
}

{
my $p = sequence(100); # big enough to not fit in "value" field
is $p->datasv_refcount, 1;
my $ref = $p->get_dataref;
$ref = $p->get_dataref;
is $p->datasv_refcount, 2;
eval {PDL->new_around_datasv(0+$ref, -1)};
like $@, qr/negative/, 'no negative offset';
eval {PDL->new_around_datasv(0+$ref, 2000)};
like $@, qr/>=/, 'no too-big offset';
my $p2 = PDL->new_around_datasv(0+$ref);
ok $p2->allocated;
is $p2->nbytes, $p->type->howbig * $p->nelem;
$p2->set_datatype($p->type->enum);
$p2->setdims([$p->dims]);
$p2->set_donttouchdata;
is $p->datasv_refcount, 3;
is $p2->datasv_refcount, 3;
undef $p2;
is $p->datasv_refcount, 2;
undef $ref;
is $p->datasv_refcount, 1;
my $datasv_ref = \(' ' x 50);
my $p3 = PDL->new_around_datasv(0+$datasv_ref, 3);
is $p3->nbytes, 47;
$p3 = PDL->new_around_datasv(0+$datasv_ref);
ok $p3->allocated;
$p3->set_datatype(byte->enum);
$p3->setdims([50]);
$p3->set_donttouchdata;
my $refcount = $p3->datasv_refcount; # varies on some Perls
is $p3->nbytes, 50;
undef $datasv_ref;
is $p3->datasv_refcount, $refcount - 1;
}

{
eval {PDL->new_around_pointer(0, 10)};
like $@, qr/NULL pointer/;
my $p1 = sequence(5); # too big for value
my $p2 = PDL->new_around_pointer($p1->address_data, $p1->nbytes);
$p2->set_datatype($p1->type->enum);
$p2->setdims([5]);
is_pdl $p2, sequence(5), 'new_around_pointer worked';
undef $p2; # make very sure this goes first
}

{
  my $pa = pdl 2,3,4;
  $pa->flowing;
  my $pb = $pa + $pa;
  is_pdl $pb, pdl '[4 6 8]';
  $pa->set(0,50);
  is_pdl $pb, pdl '[100 6 8]';
  ${$pa->get_dataref} = ${pdl(51,3,4)->get_dataref};
  $pa->upd_data;
  is_pdl $pb, pdl('[102 6 8]'), 'after upd_data, change reflected';
  $pa->update_data_from(${pdl(50,3,4)->get_dataref});
  is_pdl $pb, pdl('[100 6 8]'), 'after update_data_from, change reflected';
  eval {$pa->set_datatype(PDL::float()->enum)};
  like $@, qr/ndarray has child/, 'set_datatype if has child dies';
  $pb->set_datatype(PDL::float()->enum);
  $pa->set(0,60);
  is_pdl $pb, float('[100 6 8]'), 'dataflow broken by set_datatype';
}

eval {PDL->inplace};
like $@, qr/called object method/, 'error on PDL->obj_method';

{
  isa_ok sequence(3)->readonly, 'PDL', 'returns object';
  my $x = sequence(3);
  ok !$x->is_readonly, 'not readonly';
  $x->readonly;
  ok $x->is_readonly, 'now is readonly';
  is_pdl $x + 1, pdl '1 2 3';
  eval {$x .= 5};
  like $@, qr/is read-only/, 'assgn causes error';
  eval {$x += 5};
  like $@, qr/is read-only/, 'inplace causes error';
}

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

eval { zeroes(0)->max ? 1 : 0 };
like $@, qr/bad.*conditional/, 'badvalue as boolean is error';

{
my $a_long = sequence long, 10;
my $a_dbl  = sequence 10;
my $b_long = $a_long->slice('5');
my $b_dbl  = $a_dbl->slice('5');
my $c_long = $a_long->slice('4:7');
my $c_dbl  = $a_dbl->slice('4:7');
is $b_long->sclr, 5, "sclr test of 1-elem pdl (long)";
is $b_dbl->sclr, 5, "sclr test of 1-elem pdl (dbl)";
eval { $c_long->sclr };
like $@, qr/multielement ndarray in 'sclr' call/, "sclr failed on multi-element ndarray (long)";
eval { $c_dbl->sclr };
like $@, qr/multielement ndarray in 'sclr' call/, "sclr failed on multi-element ndarray (dbl)";
eval { my $d_long = $a_long->reshape(0,-3) };
like $@, qr/invalid dim size/, "reshape() failed with negative args (long)";
eval { my $d_dbl = $a_dbl->reshape(0,-3) };
like $@, qr/invalid dim size/, "reshape() failed with negative args (dbl)";
}

eval { my $y = zeroes(1,3); $y .= sequence(2,3); };
isnt $@, '', 'scaling-up of output dim 1 throws error';
eval { my $y = zeroes(1); $y .= zeroes(0) + 1; };
isnt $@, '', 'scaling-down of output dim 1 throws error';

{
# test reshape with no args
my $x = ones 3,1,4;
my $y = $x->reshape;
ok eq_array( [ $y->dims ], [3,4] ), "reshape()";
}

{
# test reshape(-1) and squeeze
my $x = ones 3,1,4;
my $y = $x->reshape(-1);
my $c = $x->squeeze;
is_pdl $y->shape, indx([3,4]), "reshape(-1)";
is_pdl $y, $c, "squeeze";
$c++; # check dataflow in reshaped PDL
is_pdl $y, $c, "dataflow"; # should flow back to y
is_pdl $x, pdl(2)->slice('*3,*1,*4'), "dataflow";
}

{
my $d = pdl(5); # zero dim ndarray and reshape/squeeze
is_pdl $d->reshape(-1)->shape, empty(indx), "reshape(-1) on 0-dim PDL gives 0-dim PDL";
is_pdl $d->reshape(1)->shape, indx([1]), "reshape(1) on 0-dim PDL gives 1-dim PDL";
is_pdl $d->reshape(1)->reshape(-1)->shape, empty(indx), "reshape(-1) on 1-dim, 1-element PDL gives 0-dim PDL";
}

{
# reshape test related to bug SF#398 "$pdl->hdr items are lost after $pdl->reshape"
my $c = ones(25);
$c->hdr->{demo} = "yes";
is($c->hdr->{demo}, "yes", "hdr before reshape");
$c->reshape(5,5);
is($c->hdr->{demo}, "yes", "hdr after reshape");
}

eval {zeroes(0,-2)};
like $@, qr/non-negative/, 'negative dim to zeroes gives clear error';
eval {empty->squeeze->dims};
is $@, '', 'can "squeeze" an empty';
eval {empty->copy->make_physical};
is $@, '', 'can physicalise the copy of an empty';

# capture ancient pptest.t test for Solaris segfault
is_pdl norm(pdl 3,4), pdl(0.6,0.8), 'vector quasi-copy works';
# pptest for null input
eval {(my $tmp=null) .= null}; like $@, qr/input.*null/;
# pptest for OtherPars=>named dim
is_pdl +(5*sequence(5))->maximum_n_ind(3), indx(4,3,2), 'named dim';
# pptest for dim with fixed value
is_pdl crossp([1..3],[4..6]), longlong(-3,6,-3), {test_name=>'named dim=3', require_equal_types=>0};

eval {simq(null, zeroes(3), 0)};
like $@, qr/io.*null/;

subtest 'dim compatibility' => sub {
  for (
    # non-phys params
    [\&append, [zeroes(1), zeroes(1), zeroes(1)], 2, qr/dim has size 1/, 'output=[1]; required [2]. output too small'],
    [\&append, [pdl(1), pdl(2), null], 2, [ 1, 2 ], 'output=null; required [2]'],
    [\&append, [pdl(1), pdl(2), zeroes(2)], 2, [ 1, 2 ], 'output=[2]; required [2]'],
    [\&append, [zeroes(1), zeroes(1), zeroes(3)], 2, qr/dim has size 3/, 'output=[3]; required [2]. output too large'],
    [\&append, [zeroes(1), zeroes(0), zeroes()], 2, 0, 'output=scalar; required [1]'],
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
    [\&simq, [pdl([[2,1,1]]), sequence(3,1), null, null, 0], 2, qr/dim has size/, '[phys] input=[3,1] output=[3,3] no expand input phys multi-used dim of 1'],
    [\&simq, [identity(3)+1, sequence(3,2), null, null, 0], 2, qr/implicit dim/, '[phys] inputs:n,n=[3,3],n=[3,2] no broadcast over [io]'],
  ) {
    my ($func, $args, $exp_index, $exp, $label) = @$_;
    if (ref $exp eq 'Regexp') {
      throws_ok { $func->( @$args ) } $exp, $label;
    } else {
      $func->( @$args );
      my $got = $args->[$exp_index];
      is_pdl $got, pdl($exp), $label;
    }
  }
};

{
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
isa_ok +PDL->topdl(1),       "PDL", "topdl(1) returns an ndarray";
isa_ok +PDL->topdl([1,2,3]), "PDL", "topdl([1,2,3]) returns an ndarray";
isa_ok +PDL->topdl(1,2,3),   "PDL", "topdl(1,2,3) returns an ndarray";
is_pdl +PDL->topdl(1,2,3), pdl(1,2,3), "topdl(1,2,3) returns a 3-ndarray containing (1,2,3)";
eval {PDL->topdl({})};
isnt $@, '', 'topdl({}) no segfault';
}

is_pdl pdl(1)->tocomplex, cdouble(1), 'tocomplex';
is_pdl cdouble(1)->tocomplex, cdouble(1), 'tocomplex already complex';
is_pdl float(1)->tocomplex, cfloat(1), 'tocomplex float';
is_pdl cfloat(1)->tocomplex, cfloat(1), 'tocomplex float already complex';

# stringification
{
my $x = pdl( -3..2 ) + 1e7;
my $x_indx = $x->indx;
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
is $PDL::undefval, 0, "default value of \$PDL::undefval is 0";

{
my $x = [ [ 2, undef ], [3, 4 ] ];
my $y = pdl($x);
my $c = pdl([[2, 0],[3, 4]]);
is_pdl $y, $c, "undef converted to 0 (dbl)";
is_deeply $x, [[2,undef],[3,4]], "pdl() has not changed input array";
is_pdl long($x), long($c), "undef converted to 0 (long)";
}

{
local($PDL::undefval) = -999;
my $x = [ [ 2, undef ], [3, 4 ] ];
my $y = pdl($x);
my $c = pdl('2 -999; 3 4');
is_pdl $y, $c, "undef converted to -999 (dbl)";
is_pdl long($x), long($c), "undef converted to -999 (long)";
};

{
# Funky constructor cases
# pdl of a pdl
is_pdl pdl(pdl(5)), pdl(5), "pdl() can piddlify an ndarray";
is_pdl pdl(null), null, 'pdl(null) gives null';

is_pdl pdl(null, null), zeroes(0,2), 'pdl(null, null) gives empty';

# pdl of mixed-dim pdls: pad within a dimension
is_pdl pdl( zeroes(5), ones(3) ), pdl([0,0,0,0,0],[1,1,1,0,0]),"Piddlifying two ndarrays concatenates them and pads to length";

# pdl of mixed-dim pdls: pad a whole dimension
is_pdl pdl( [[9,9],[8,8]], xvals(3)+1 ), pdl([[[9,9],[8,8],[0,0]] , [[1,0],[2,0],[3,0]] ]),"can concatenate mixed-dim ndarrays";

# pdl of mixed-dim pdls: a hairier case
is_pdl pdl([1], pdl[2,3,4], pdl[5]), pdl([[[1,0,0],[0,0,0]],[[2,3,4],[5,0,0]]]),"Can concatenate mixed-dim ndarrays: hairy case";
}

# same thing, with undefval set differently
{
    local($PDL::undefval) = 99;
    my $c = pdl undef;
    is_pdl $c, pdl(99), "explicit, undefval of 99 works";
    $c = pdl [1], pdl[2,3,4], pdl[5];
    is_pdl $c, pdl([[[1,99,99],[99,99,99]],[[2,3,4],[5,99,99]]]), "implicit, undefval works for padding";
    $PDL::undefval = undef;
    $c = pdl undef;
    is_pdl $c, pdl(0), "explicit, undefval of undef falls back to 0";
    $c = pdl [1], [2,3,4];
    is_pdl $c, pdl([1,0,0],[2,3,4]), "implicit, undefval of undef falls back to 0";
    $PDL::undefval = inf;
    $c = pdl undef;
    is_pdl $c, inf, "explicit, undefval of PDL scalar works";
    $c = pdl [1], [2,3,4];
    is_pdl $c, pdl([1,inf,inf],[2,3,4]), {rtol=>0, test_name=>"implicit, undefval of a PDL scalar works"};
}

{
# empty pdl cases
my $x = eval {zeroes(2,0,1);};
is($@, '', "zeroes accepts empty PDL specification");

my $y = pdl($x,sequence(2,0,1));
is_pdl $y->shape, indx(2,0,1,2), "concatenating two empties gives an empty";

$y = pdl($x,sequence(2,1,1));
is_pdl $y->shape, indx(2,1,1,2), "concatenating an empty and a nonempty treats the empty as a filler";

$y = pdl($x,5);
is_pdl $y->shape, indx(2,1,1,2), "concatenating an empty and a scalar on the right works";

$y = pdl(5,$x);
is_pdl $y, pdl([[[5,0]]],[[[0,0]]]), "concatenating an empty and a scalar on the left gives the right answer";
}

is_pdl pdl(Math::Complex->make(1,2)), pdl('1+2i'), 'pdl(Math::Complex obj)';

# cat problems
eval {cat(1, pdl(1,2,3), {}, 6)};
isnt $@, '', 'cat barfs on non-ndarray arguments';
like $@, qr/Arguments 0, 2 and 3 are not ndarrays/, 'cat correctly identifies non-ndarray arguments';
eval {cat(1, pdl(1,2,3))};
like $@, qr/Argument 0 is not an ndarray/, 'cat uses good grammar when discussing non-ndarrays';

{
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
}

is_pdl cat(pdl(1),pdl(2,3)), pdl([1,1],[2,3]), "cat does the right thing with catting a 0-pdl and 2-pdl together";

{
my $lo=sequence(long,5)+32766;
my $so=sequence(short,5)+32766;
my $fl=sequence(float,5)+float(0.2); # 0.2 is an NV so now a double
my $by=sequence(byte,5)+253;
my @list = ($lo,$so,$fl,$by);
my $c2 = cat(@list);
is($c2->type,'float','concatenating different datatypes returns the highest type');
is_pdl $_, shift @list, {require_equal_types=>0, test_name=>"cat/dog symmetry for values"} for $c2->dog;
my ($dogcopy) = $c2->dog({Break=>1});
$dogcopy++;
is_pdl $dogcopy, $c2->slice(':,(0)')+1, 'Break means copy'; # not lo as cat no flow
my ($dogslice) = $c2->dog;
$dogslice++;
is_pdl $dogslice, $c2->slice(':,(0)'), 'no Break means dataflow';
eval {pdl([3])->dog(5)};
like $@, qr/Usage/, "error if excess args";
for ([[], qr/at least/], [[5]], [[4,5]]) {
  my ($dims, $err) = @$_;
  my @d = eval {zeroes(@$dims)->dog};
  like($@, $err, "right error (@$dims)"), next if $err;
  is 0+@d, $dims->[-1], "works (@$dims)";
}
@list = pdl('[3;0;2;0]')->mv(0,-1)->dog;
is 0+@list, 1, "dog on pure-vaff works";
}

zeroes(1,1000)->dog; # no segfault please

{
my $x = sequence(byte,5);
$x->inplace;
ok $x->is_inplace,"original item inplace-d true inplace flag";
eval { $x->inplace(1) };
is $@, '', 'passing spurious extra args no error';
my $y = $x->copy;
ok $x->is_inplace,"original item true inplace flag after copy";
ok !$y->is_inplace,"copy has false inplace flag";
$y++;
is_pdl $y, sequence(byte,5)+1,"copy returns severed copy of the original thing if inplace is set";
ok $x->is_inplace,"original item still true inplace flag";
ok !$y->is_inplace,"copy still false inplace flag";
is_pdl $x, sequence(byte,5),"copy really is severed";
}

{
# new_or_inplace
my $x = sequence(byte,5);
my $y = $x->new_or_inplace;
is_pdl $y, $x, "new_or_inplace with no pref returns something like the orig.";
$y++;
is_pdl $y, $x+1, "new_or_inplace with no inplace flag returns something disconnected from the orig.";

$y = $x->new_or_inplace("float,long");
is $y->type, 'float',"new_or_inplace returns first type in case of no match";

$y = $x->inplace->new_or_inplace;
$y++;
is_pdl $y, $x, "new_or_inplace returns the original thing if inplace is set";
ok !$y->is_inplace,"new_or_inplace clears the inplace flag";
}

{
# check empty creation
is empty(float)->type, 'float', 'empty(float) works';
my $empty = empty();
is $empty->type->enum, 0, 'empty() gives lowest-numbered type';
is $empty->nelem, 0, "you can make an empty PDL with zeroes(0)";
like "$empty", qr/Empty/, "an empty PDL prints 'Empty'";
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
}

{
my $x = short(3,4,5,6);
eval { $x->reshape(2,2);};
is($@, '', "reshape succeeded in the normal case");
is_pdl $x, short([[3,4],[5,6]]), "reshape moved the elements to the right place";
my $y = $x->slice(":,:");
eval { $y->reshape(4); };
unlike $@, qr/Can't/, "reshape doesn't fail on a PDL with a parent";
my $nzai = zeroes(indx,6)->slice('');
eval {$nzai = $nzai->reshape(30)};
is $@, '', 'no reshape error';
}

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

for my $array (
  1,
  [],
  [1..3],
  [[[1,2], [3,4]], [[5,6], [7,8]], [[9,10], [11,12]]],
) {
  my ($expected, $got) = ref $array ? $array : [$array]; # scalar not round-tripped right but back-compat
  is_deeply $got = pdl($array)->unpdl, $expected,
    "back convert ".join('', explain $array)
    or diag explain $got;
}
SKIP: {
  skip("your perl hasn't 64bit int support", 6) if $Config{ivsize} < 8;
  {
  my $neg = -684394069604;
  my $straight_pdl = pdl($neg);
  my $multed = pdl(1) * $neg;
  is $straight_pdl, $multed, 'upgrade of large negative SV to ndarray'
    or diag "straight:", $straight_pdl->info, " mult:", $multed->info;
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

{
my $big_ushort = ushort(65535);
is $big_ushort->badflag, 0, 'max ushort value badflag';
is PDL::Core::at_bad_c($big_ushort, []), 65535, 'max ushort value not "BAD" per se';
}

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
  is $type->{typefunc}()->howbig, $type->{size}, 'howbig method works';
  my $pdl = $type->{typefunc}(42); # build a PDL with datatype $type->{type}
  is( PDL::Core::howbig( $pdl->get_datatype ), $type->{size} );
  is $pdl->type, $type->{typefunc}->().'', 'pdl has right type';
  is_pdl $pdl->convert(longlong), longlong(42), 'converted to longlong same value';
  $pdl->inplace->convert(longlong);
  is_pdl $pdl, longlong(42), 'inplace convert worked';
}
}

{
is_pdl pdl(-3)->ushort, ushort(-3), 'convert negative to ushort right';
ok pdl(-3)->ushort, 'convert negative to ushort non-zero';
my $p = pdl(-3);
$p->inplace->convert(ushort);
is_pdl $p, ushort(-3), 'inplace convert negative to ushort right';
ok $p, 'inplace convert negative to ushort non-zero';

my $from = pdl(5);
my $to = $from->convert_flowing(float);
is $to->type, 'float';
is_pdl $to, float(5);
$to++;
is_pdl $to, float(6);
is_pdl $from, pdl(6);
$from++;
is_pdl $to, float(7);
is_pdl $from, pdl(7);
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
  my $exp = pdl($val)->slice('*1,*2,*3');
  $w = $y->copy; $name->(inplace $w); is_pdl $w, $exp, $name;
  $w = $y->copy; $w->inplace->$name; is_pdl $w, $exp, $name;
}

is short(1)->zeroes->type, 'short', '$existing->zeroes right type';

eval { PDL->is_inplace }; # shouldn't infinite-loop
isnt $@, '', 'is_inplace as class method throws exception';

{
my $s = sequence(3);
is $s->trans_parent, undef, 'trans_parent without trans undef';
my $slice = $s->slice;
isnt +(my $tp=$slice->trans_parent), undef, 'trans_parent with trans defined';
is 0+$s->trans_children, 1, 'scalar trans_children';
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
}

{
my $x = sequence 3; my $y = $x->flowing + 1;
isnt $y->trans_parent, undef, '$y has parent';
isnt PDL::Core::pdumphash($x), undef, 'pdumphash works';
isnt $y->trans_parent, undef, '$y still has parent after pdumphash';
$x += 3;
is_pdl $x, pdl("[3 4 5]"), '$x right value';
is_pdl $y, pdl("[4 5 6]"), '$y right value';
ok !$y->fflows, 'y not "flowing"';
$y->flowing;
ok $y->fflows, 'y "flowing" on';
my $z = $y + 1;
isnt $z->trans_parent, undef, 'z has trans_parent';
ok !$y->fflows, 'y "flowing" off again';
eval {$y += 4};
isnt $@, '', 'error on assigning to ndarray with inward but no outward flow';
my $oneway_slice = $y->slice('0:1');
is_pdl $oneway_slice, pdl '[4 5]';
eval {$oneway_slice .= 11};
isnt $@, '', 'error on assigning into one-way slice';
ok $y->flowing->_convert_int(cdouble->enum)->fflows, 'flowing -> converted has "flowing" on';
}

{
my $x = sequence 3;
my $c = $x->_convert_int(cdouble->enum);
isnt $c->trans_parent, undef, 'converted has trans_parent';
$c++;
is_pdl $x, pdl(1..3);
my $cf = $c->_convert_int(cfloat->enum);
isnt $cf->trans_parent, undef, 'converted2 has trans_parent';
$cf++;
is_pdl $x, pdl(2..4);
}

{
my $o_float = PDL::plus(float(1), float(2));
is_pdl $o_float, float(3);
# 3-arg is (a,b,swap); 4-arg is (a,b,output,swap)
PDL::plus(float(1), float(2), $o_float = null, 0);
is_pdl $o_float, float(3);
PDL::plus(float(1), float(2), my $o_double = double(0), 0);
is_pdl $o_double, pdl(3);
is 0+$o_double->trans_children, 0, 'no trans_children from non-flowing';

PDL::plus(my $i_float = float(1)->flowing, float(2), $o_double = double(0), 0);
is +($i_float->trans_children)[0]->vtable->name, 'converttypei_new', 'input trans_children[0] is convert output type > inputs';
is_pdl $o_double, pdl(3);
is $o_double->trans_parent->vtable->name, 'PDL::Ops::plus', 'right trans_parent from flowing output type > inputs';
is 0+$o_double->trans_children, 0, '0 trans_children on output from flowing';

PDL::plus(my $i_double = double(1)->flowing, double(2), $o_float = float(0), 0);
is +($i_double->trans_children)[0]->vtable->name, 'converttypei_new', 'input.trans_children[0] IS convert from flowing output type < inputs';
is_pdl $o_float, float(3), 'output right from flowing output type < inputs';
is $o_float->trans_parent->vtable->name, 'PDL::Ops::plus', 'trans_parent of output is plus from flowing output type < inputs';
is 0+$o_float->trans_children, 0, '0 trans_children on output from flowing output type < inputs';

is_pdl +(sequence(2,2)->simq(pdl('4 9'),0))[0], pdl('-1.5 4'), 'unconverted [io] works';
is_pdl +(sequence(2,2)->float->simq(pdl('4 9'),0))[0], pdl('-1.5 4'), 'converted [io] works';

my $double_mask = double('0 0 1 1 1 1 1');
$double_mask   &= double('1 1 1 1 1 0 0');
is_pdl $double_mask, double('0 0 1 1 1 0 0');

eval {PDL::eqvec(double([1,2]), double([1,2]), float(0)->slice(''))};
like $@, qr/cannot convert/, "error when flowing xform given non-available-typed output with parent";

PDL::eqvec(double([1,2])->flowing, double([1,2]), $o_float = float(0));
is 0+$o_float->trans_children, 0, 'converted output of flowing xform has no trans_children';
is $o_float->trans_parent->vtable->name, 'converttypei_new', 'converted output of flowing xform has convert trans_parent';
is_pdl $o_float, float(1), 'converted output of flowing xform has right value';

PDL::eqvec(double([1,2],3,4)->flowing, double([1,2],3,5), my $o_byte = byte([0,0,0]));
is 0+$o_byte->trans_children, 0, 'converted output of flowing xform has no trans_children';
is $o_byte->trans_parent->vtable->name, 'converttypei_new', 'converted output of flowing xform has convert trans_parent';
is_pdl $o_byte, byte([1,1,0]), 'converted output of flowing xform has right value';

{
  my $in = byte('1 2 3 4 5 6 7 8 9 10');
  my $got = $in->zeroes;
  my $exp = $in->copy;
  my $tmp = $exp->where( ! ($in % 2) );
  $tmp .= 0;
  PDL::acosh( $in, $got );
  is_pdl $got, byte('0 1 1 2 2 2 2 2 2 2'), "convert of thing with trans_children no NULL data";
}

is_pdl PDL::and2(byte(3), byte(1)), byte(1), 'both input available-typed';
is_pdl PDL::and2(double(3), double(1)), longlong(1), 'both input non-available-typed';
is_pdl PDL::and2(byte(3), double(1)), longlong(1), 'inputs one avail, one non-available-typed -> last-given type';

for ([\&float,\&cfloat,\&cdouble], [\&double,\&cdouble,\&cfloat], [\&ldouble,\&cldouble]) {
  my ($rt, $ct, $other_ct) = @$_;
  my $o_cmplx = czip($rt->(3), $rt->(2));
  is_pdl $o_cmplx, $ct->('3+2i'), 'right answer from no supplied output '.$rt->();
  czip($rt->(3), $rt->(2), $o_cmplx = $ct->(0));
  is_pdl $o_cmplx, $ct->('3+2i'), 'right answer from supplied output '.$rt->();
  $o_cmplx = czip($rt->(3)->flowing, $rt->(2));
  is_pdl $o_cmplx, $ct->('3+2i'), 'right answer from flowing, no supplied output '.$rt->();
  czip($rt->(3)->flowing, $rt->(2), $o_cmplx = $ct->(0));
  is_pdl $o_cmplx, $ct->('3+2i'), 'right answer from flowing, supplied output '.$rt->();
  eval {czip($rt->(3)->flowing, $rt->(2), $ct->(0)->slice(''))};
  is $@, '', 'no error when supply right-typed output with parent to flowing '.$rt->();
  next if !$other_ct;
  czip($rt->(3)->flowing, $rt->(2), $o_cmplx = $other_ct->(0));
  is_pdl $o_cmplx, $other_ct->('3+2i'), 'right answer from flowing, input '.$rt->().', supplied output '.$other_ct->();
}
}

my $notouch = sequence(4);
$notouch->set_donttouchdata;
eval { $notouch->setdims([2,2]); $notouch->make_physical; };
is $@, '', 'setdims to same total size of set_donttouchdata should be fine';
eval { $notouch->setdims([3,2]); $notouch->make_physical; };
isnt $@, '', 'setdims/make_physical to different size of set_donttouchdata should fail';
my $sliced = sequence(4)->slice('');
eval { $sliced->setdims([3,2]) };
like $@, qr/but has trans_parent/, 'setdims on pdl with trans_parent is error';

eval { pdl(3)->getbroadcastid($_) }, isnt $@, '', "getbroadcastid($_) out of range gives error" for -2, 5;

done_testing;
