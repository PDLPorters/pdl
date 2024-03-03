use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Dbg;

# PDL::Core::set_debugging(1);

# Useful for debugging. Removed by DJB whilst cleaning up the
# tests
#
#sub kill_if_debug () {
#    kill INT,$$  if $ENV{UNDER_DEBUGGER};
#}

sub tapprox ($$) {
    my $x = shift;
    my $y = shift;
    return 1 if $x->isempty and $y->isempty;
    my $maxdiff = abs($x-$y)->max;
    return $maxdiff < 0.01;
}

my $x = (1+(xvals zeroes 4,5) + 10*(yvals zeroes 4,5));

is($x->at(2,2), 23, "x location (2,2) is 23");

my $y = $x->slice('1:3:2,2:4:2');
ok(tapprox($y,pdl([[22,24],[42,44]])));

$y .= 0.5;
ok(tapprox($y,pdl([[0.5,0.5],[0.5,0.5]])));
is($x->at(1,2), 0.5);
is($x->at(2,2), 23); # Check that nothing happened to other elems

# test stringify
$x = zeroes(3,3);
my $line = $x->slice(':,(0)');
$x++;
is("$line", '[1 1 1]', 'right value after collapsing slice (0)');

my $im = byte [[0,1,255],[0,0,0],[1,1,1]];
(my $im1 = null) .= $im->dummy(0,3);
my $im2 = $im1->clump(2)->slice(':,0:2');
ok(!tapprox(ones(byte,9,3),$im2));

# here we encounter the problem
$im2 = $im1->clump(2)->slice(':,-1:0');
ok(!tapprox(ones(byte,9,3),$im2));

$x = xvals( zeroes 10,10) + 0.1*yvals(zeroes 10,10);
ok(tapprox($x->mslice('X',[6,7]),
	   pdl([
		[0.6, 1.6, 2.6, 3.6, 4.6, 5.6, 6.6, 7.6, 8.6, 9.6],
		[0.7, 1.7, 2.7, 3.7, 4.7, 5.7, 6.7, 7.7, 8.7, 9.7]
	       ])));

my $lut = pdl [[1,0],[0,1]];
$im = pdl [1];
my $in = $lut->transpose->index($im->dummy(0));

is("$in", "
[
 [0 1]
]
");

$in .= pdl 1;

is("$in", "
[
 [1 1]
]
");
ok(tapprox($lut,pdl([[1,0],[1,1]])));

# Test of dice and dice_axis
$x = sequence(10,4);
is($x->dice([1,2],[0,3])->sum, 66, "dice");
is($x->dice([0,1],'X')->sum, 124, "dice 'X'");

# Test of dice clump compatability
my $xxx = PDL->new([[[0,0]],[[1,1]],[[2,2]]]);
is_deeply($xxx->where($xxx == 0)->unpdl,[0,0],"dice clump base zero");
my $dice = $xxx->dice("X","X",[1,0]);
is_deeply($dice->clump(-1)->unpdl,[1,1,0,0],"dice clump correct");
is_deeply($dice->where($dice == 0)->unpdl,[0,0],"dice clump where zero");

$x = sequence(5,3,2);
my @newDimOrder = (2,1,0);
$y = $x->reorder(@newDimOrder);
my $got = [$y->dims];
is_deeply($got, [2,3,5], "Test of reorder") or diag explain $got;

$x = zeroes(3,4);
$y = $x->dummy(-1,2);
is(join(',',$y->dims), '3,4,2');

$x = pdl([1,1,1,3,3,4,4,1,1,2]);
for my $in (
  $x,
  $x->cat(map $x->rotate($_), 1..4)
) {
  rle($in,my $y=null,my $z=null);
  ok(tapprox(rld($y,$z), $in),"rle with null input");
  ($y,$z) = rle($in);
  ok(tapprox(rld($y,$z), $in),"rle with return vals");
}

$y = $x->mslice(0.5);
ok(tapprox($y, 1), "mslice 1");
$y = mslice($x, 0.5);
ok(tapprox($y, 1), "func mslice 1");
$y = $x->mslice([0.5,2.11]);
is("$y", "[1 1 1]", "mslice 2");

$x = zeroes(3,3);
$y = $x->splitdim(3,3);
eval { $y->make_physdims };
like($@, qr/splitdim:nthdim/, "make_physdim: Splitdim");
$y = $x->splitdim(-1,1);
is_deeply [$y->dims], [3,1,3], 'splitdims negative nthdim works' or diag explain [$y->dims];
$y = $x->splitdim(1,1);
is_deeply [$y->dims], [3,1,3], 'splitdims works' or diag explain [$y->dims];
$y = $x->splitdim(1,2);
eval { $y->make_physdims };
like($@, qr/non-divisible/, "splitdims error non-divisible");

$x = sequence 5,5;
$y = $x->diagonal(0,1);
is("$y", "[0 6 12 18 24]", "diagonal");

$x = sequence 10;
eval { $y = $x->lags(1,1,1)->make_physdims };
like($@, qr/lags:\s*dim out of range/, "make_physdim: out of range");

eval { $y = $x->lags(0,-1,1)->make_physdims };
like($@, qr/lags:\s*step must be positive/, "make_physdim: negative step");

eval { $y = $x->lags(0,1,11)->make_physdims };
like($@, qr/too large/, "make_physdim: too large");

$x = sequence(10);
my $x1 = pdl(1,2);
my $x2 = xvals(5,5)+10*yvals(5,5);
my $x3 = xvals 20,20;
my $x4 = zeroes(5,3,3);
for (
  [$x, "", $x, "Empty slice"],
  [$x, "5", pdl([5]), "simple slice"],
  [$x, "(5)", pdl(5), "single squish"],
  [$x, ":5", pdl(0,1,2,3,4,5), "empty first specifier"],
  [$x, "5:", pdl(5,6,7,8,9), "empty second specifier"],
  [$x, " 4:", pdl(4,5,6,7,8,9), "slice with whitespace 1"],
  [$x, " :4", pdl(0,1,2,3,4), "slice with whitespace 2"],
  [$x, " 3: 4 ", pdl(3,4), "slice with whitespace 3"],
  [$x, "0:-10", pdl([0]), "slice 0:-n picks first element"],
  [$x, "0:-14", qr/slice ends out of bounds/, "out of bounds"],
  [$x, [[pdl(7,6)->slice(1),0,0]], pdl(6), "slice did 'at'"],
  [$x, [[pdl([2]),pdl([7]),pdl([2])]], pdl(2,4,6), "slice did 'at' 2"],
  [$x1, "*3,", pdl([[1,1,1],[2,2,2]]), "dummy 0"],
  [$x1, ",*3", pdl([[1,2],[1,2],[1,2]]), "dummy 1"],
  [$x2, "1,2,(0)", [1,1], "squished 0th of non-existent dim"],
  [$x2, "1,2,(1)", qr/too many dims/i, "squished 1th of non-existent dim"],
  [$x2, "0:1,2:3,0", [2,2,1], "0th of non-existent dim"],
  [$x3, ["1:18:2,:",":,1:18:2","3:5,:",":,(0)"], pdl(7,9,11), "multiple slices"],
  [$x3, ["1:18:2,:",":,1:18:2","3:5,:",":,(1)"], pdl(7,9,11), "multiple slices 2"],
  [$x4, ":,:,1", [5,3,1], "single-coord slice"],
  [$x4, [":,:,1",":,:,2"], qr/out of bounds/, "slice bounds"],
  [PDL->null, "", qr/is null/, "null->slice"],
  [pdl([1]), [pdl([])], pdl([]), "slice 1-elt ndarray with empty"],
  [$x1, [pdl([])], pdl([]), "slice 2-elt ndarray with empty"],
  [$x1, [pdl(1)], pdl([2]), "slice 2-elt ndarray with length-1 ndarray"],
  [zeroes(2,1,0), [\[[],[0,0,0],[]]], zeroes(2,0), "squeeze empty"],
  [zeroes(2,0), ",0:-1", zeroes(2,0), "slice empty string syntax"],
) {
  my ($src, $sl, $exp, $label) = @$_;
  my $y = $src;
  $y = eval { $y->slice(ref($_) eq 'REF' ? @$$_ : $_)->make_physical } for ref $sl ? @$sl : $sl;
  like($@, $exp, "$label right error"), next if ref($exp) eq 'Regexp';
  is $@, '', "$label works";
  is_deeply([$y->dims], ref($exp) eq 'ARRAY' ? $exp : [$exp->dims], "$label dims right") or diag explain [$y->dims];
  next if ref($exp) eq 'ARRAY';
  is $y->nelem, $exp->nelem, "$label works right";
  ok tapprox($y, $exp), "$label works right";
}

my $d = eval { $x2->slice("0:1,2:3,0")->xchg(0,2)->make_physical };
is $@, '', "slice->xchg";
is_deeply([$d->dims], [1,2,2], "permissive slice xchg dims right");

my $e = eval { $x2->dummy(6,2)->make_physical };
is $@, '', "dummy";
is_deeply([$e->dims], [5,5,1,1,1,1,2], "dummy dims right");

##############################
# Tests of indexND (Nowadays this is just another call to range)

# Basic indexND operation
my $source = 10*xvals(10,10) + yvals(10,10);
my $index  = pdl([[2,3],[4,5]],[[6,7],[8,9]]);
eval { $x = $source->indexND( $index ) };
is $@, '';
ok(tapprox($x, pdl([23,45],[67,89])));

# Broadcast indexND operation
$source = 100*xvals(10,10,2)+10*yvals(10,10,2)+zvals(10,10,2);
$index  = pdl([[2,3],[4,5]],[[6,7],[8,9]]);
eval { $x = $source->indexND($index) };
is $@, '';
ok(tapprox($x, pdl([[230,450],[670,890]],[[231,451],[671,891]])));

# Tests of range operator
$source = 10*xvals(10,10) + yvals(10,10);
my $source3 = 10*xvals(3,3) + yvals(3,3);
$index = pdl([[2,3],[4,5]],[[6,7],[8,9]]);
my $mt = which(pdl(0));
my $dex = pdl(5,4,3);
for (
  [$source, [$index], [2,2], pdl([23,45],[67,89]), "simple"],
  [$source, [$index,3], [2,2,3,3], qr/out-of-bounds/, "out of bounds with scalar size"],
  [$source, [$index,3,"t"], [2,2,3,3], pdl([[89,99,0],[0,0,0],[0,0,0]]), "truncate size 3", sub {shift->slice("(1),(1)")}],
  [$source, [$index,3,"tp"], [2,2,3,3], pdl([[89,99,0],[80,90,0],[81,91,0]]), "truncate+periodic size 3", sub {shift->slice("(1),(1)")}],
  [$source3, [[-1,-1],[2,2],"p"], [2,2], pdl([[22,2],[20,0]]), "periodic size [2 2]", undef, sub {$_[0] .= 6}, [[6,10,6],[1,11,21],[6,12,6]]],
  [$source, [$index,3,["e","p"]], [2,2,3,3], pdl([[89,99,99],[80,90,90],[81,91,91]]), "extension+periodic list syntax size 3", sub {shift->slice("(1),(1)")}],
  [$dex, [$mt], [0], pdl([]), "scalar Empty[0] indices"],
  [$dex, [zeroes(1,0)], [0], pdl([]), "Empty[1,0] indices"],
  [$mt, [$dex,undef,'e'], [], pdl(0), "empty source"],
  [$mt, [$mt], [0], pdl([]), "empty source and index"],
  [pdl(5,5,5,5), [$mt], [0], pdl([]), "non-empty source, empty index", sub {$_[0] .= 2}],
) {
  my ($src, $args, $exp_dims, $exp, $label, $exp_mod, $mutate, $mutate_exp) = @$_;
  $_ = $src->copy for $src, my $src_copy;
  my $y = eval { $src->range(@$args) };
  is $@, '', "$label works";
  is_deeply([$y->dims], $exp_dims, "$label dims right") or diag explain [$y->dims];
  eval { $y->make_physical };
  like($@, $exp, "$label right error"), next if ref($exp) eq 'Regexp';
  is $@, '', "$label works 2";
  $y = $exp_mod->($y) if $exp_mod;
  is $y->nelem, $exp->nelem, "$label nelem right";
  ok tapprox($y, $exp), "$label right data";
  ok tapprox($src, $src_copy), "$label source not mutated";
  next if !$mutate;
  $mutate->($y);
  ok tapprox($src, $mutate_exp), "$label src right data after mutation" or diag "got=$src";
}

# range on higher-dimensional
for (4..6) {
  my @dims = (5) x $_;
  my $src = sequence @dims;
  my $idx = ndcoords indx, $src;
  (my $out = $src->range($idx, 2, 't'))->make_physdims;
  my $expected = [@dims, (2) x $_];
  is_deeply [$out->dims], $expected or diag explain [$out->dims];
}

for my $start (0, 4, -4, 20, -20) {
	for my $stop (0, 4, -4, 20, -20) {
		# Generate a simple data ndarray and a bad slice of that ndarray
		my $data = sequence(10);
		my $slice = $data->slice("$start:$stop");

		pass('Slice operation for properly formed slice does not croak');

		# Calculate the expected dimension size:
		my $expected_dim_size;
		my $real_start = $start;
		$real_start += 10 if $start < 0;
		my $real_stop = $stop;
		$real_stop += 10 if $stop < 0;
		$expected_dim_size = abs($real_stop - $real_start) + 1
			if 0 <= $real_stop and $real_stop < 10
				and 0 <= $real_start and $real_start < 10;

		my $expected_outcome_description
			= defined $expected_dim_size ? 'is fine' : 'croaks';

		my $dim1;
		# Should croak when we ask about the dimension:
		eval { $dim1 = $slice->dim(0) };
		is($dim1, $expected_dim_size, "Requesting dim(0) on slice($start:$stop) $expected_outcome_description");

		# Should *STILL* croak when we ask about the dimension:
		eval { $dim1 = $slice->dim(0) };
		is($dim1, $expected_dim_size, "Requesting dim(0) a second time on slice($start:$stop) $expected_outcome_description");

		# Calculate the expected value
		my $expected_value;
		$expected_value = $data->at($real_start) if defined $expected_dim_size;

		# Should croak when we ask about data
		my $value;
		eval { $value = $slice->at(0) };
		is($value, $expected_value, "Requesting first element on slice($start:$stop) $expected_outcome_description");
	}
}

{
my @METHODS = qw(datachgd allocated vaffine);
sub vafftest {
  my ($all, $exp, $elabel) = @_[0..2];
  local $Test::Builder::Level = $Test::Builder::Level + 1;
  for (0..$#$all) {
    my ($x, $name, $xexp) = (@{$all->[$_]}[0,1], $exp->[$_]);
    is $x->${\$METHODS[$_]}, $xexp->[$_], "$elabel: $name $METHODS[$_]"
      for 0..$#METHODS;
  }
}
# Test vaffine optimisation
my $root = zeroes(100,100);
my $vaff = $root->slice('10:90,10:90');
my $vaff2 = $vaff->slice('5:8,5:8');
my $clumped = $vaff2->clump(-1);
my $all = [[$vaff,'vaff'], [$vaff2,'vaff2'], [$clumped,'clumped']];
vafftest($all, [[0,0,0],[0,0,0],[1,0,0]], "start");
$vaff++;
vafftest($all, [[0,0,1],[0,0,0],[1,0,0]], "vaff mutated");
$vaff2->make_physvaffine;
vafftest($all, [[0,0,1],[0,0,1],[1,0,0]], "vaff2 vaffed");
$vaff->make_physical;
vafftest($all, [[0,1,1],[0,0,1],[1,0,0]], "vaff physicalised");
$vaff2 += 1;
vafftest($all, [[1,1,1],[0,0,1],[1,0,0]], "vaff2 mutated");
$vaff->make_physvaffine;
vafftest($all, [[0,1,1],[0,0,1],[1,0,0]], "vaff physvaffined");
$clumped++;
vafftest($all, [[1,1,1],[1,1,1],[1,1,0]], "clumped mutated");
$root->set(0,0,7);
vafftest($all, [[1,1,1],[1,1,1],[1,1,0]], "root set()ed");
$vaff->make_physvaffine;
vafftest($all, [[0,1,1],[1,1,1],[1,1,0]], "vaff physvaffined");
$vaff2->make_physvaffine;
vafftest($all, [[0,1,1],[0,1,1],[1,1,0]], "vaff2 physvaffined");
$clumped->make_physvaffine;
vafftest($all, [[0,1,1],[0,1,1],[0,1,0]], "clumped physvaffined");
push @$all, [my $latevaff=$vaff2->slice(''), 'latevaff'];
vafftest($all, [[0,1,1],[0,1,1],[0,1,0],[0,0,0]], "latevaff created");
$latevaff->make_physvaffine;
vafftest($all, [[0,1,1],[0,1,1],[0,1,0],[0,0,1]], "latevaff physvaffined");
is $latevaff->vaffine_from, $root->address, 'latevaff vaffine_from root';

# capturing GH#461
$root = zeroes 2,2,2;
my $clumped1 = $root->clump( 0,1 );
my $clumped2 = $clumped1->clump( 0,1 );
$all = [[$root,'root'], [$clumped1,'clumped1'], [$clumped2,'clumped2']];
vafftest($all, [[0,1,0],[1,0,0],[1,0,0]], "start");
$clumped2->make_physvaffine;
vafftest($all, [[0,1,0],[0,1,0],[0,1,0]], "clumped2 physvaff 1");
$root .= 3;
vafftest($all, [[0,1,0],[1,1,0],[1,1,0]], "root assigned to");
$clumped2->make_physvaffine;
vafftest($all, [[0,1,0],[0,1,0],[0,1,0]], "clumped2 physvaff 2");
is "@{$clumped2->unpdl}", "3 3 3 3 3 3 3 3";

# Make sure that vaffining is properly working:
my $y = xvals(5,6,2) + 0.1 * yvals(5,6,2) + 0.01 * zvals(5,6,2);
my $c = $y->copy->slice("2:3");
ok tapprox $c, $c->copy;
for ([0,1], [1,0], [1,1]) {
  my ($mv, $mult) = @$_;
  my $x_orig = pdl [1..4];
  my $x_mv = $mv ? $x_orig->mv(-1,0) : $x_orig;
  my $x_slice = $x_mv->slice("0:2");
  $x_slice->make_physvaffine;
  $x_slice *= 100 if $mult;
  my $y = PDL::_clump_int($x_slice,-1);
  $y->make_physvaffine;
  my $got = [$x_slice->firstvals_nophys];
  my $exp = [map $_*($mult ? 100 : 1), 1..3];
  is_deeply $got, $exp, "mv=$mv mult=$mult firstvals_nophys" or diag explain $got;
  $got = $y->unpdl;
  is_deeply $got, $exp, "mv=$mv mult=$mult clump" or diag explain $got;
}
# test the bug alluded to in the comments in pdl_changed (pdlapi.c)
# used to segfault
my $xx=ones(double,3,4);
my $sl1 = $xx->slice('(0)');
my $sl11 = $sl1->slice('');
my $sl2 = $xx->slice('(1)');
my $sl22 = $sl2->slice('');
my $roots = pdl '[1 -2396-2796i -778800+5024412i 2652376792-1643494392i -684394069604-217389559200i]'; # gives 4 roots of 599+699i
PDL::polyroots($roots->re, $roots->im, $sl11, $sl22);
my $got;
ok all(approx $got=$xx->slice('(0)'), 599), "col=0" or diag "got=$got";
ok all(approx $got=$xx->slice('(1)'), 699), "col=1" or diag "got=$got";
}

# captured from https://www.perlmonks.org/?node_id=11153348
for ([0,0], [0,1], [1,0], [1,1]) {
  my ($phys_clump, $mutate_orig) = @$_;
  my $orig = zeroes 3,2,1;
  my $clump = $orig->clump(1,2);
  $clump->make_physvaffine if $phys_clump;
  ($mutate_orig ? $orig : $clump) .= 3;
  my $got = $orig->unpdl;
  is_deeply $got, [[[(3)x3],[(3)x3]]], "phys_clump=$phys_clump mutate_orig=$mutate_orig orig" or diag explain $got;
  $got = $clump->unpdl;
  is_deeply $got, [[(3)x3],[(3)x3]], "phys_clump=$phys_clump mutate_orig=$mutate_orig clump" or diag explain $got;
  $got = $clump->uniqvec->unpdl;
  is_deeply $got, [[(3)x3]], "phys_clump=$phys_clump mutate_orig=$mutate_orig uniqvec" or diag explain $got;
}

my $pa = zeroes(7, 7); $pa->set(3, 4, 1);
my $indices = $pa->which->dummy(0,$pa->getndims)->make_physical;
my $s = $indices->index(0);
$s %= 7;
is $indices.'', <<EOF, 'mutate indexed slice affects only right column';
\n[\n [ 3 31]\n]
EOF

## rlevec(), rldvec(): 2d ONLY
my $p = pdl([[1,2],[1,2],[1,2],[3,4],[3,4],[5,6]]);
my ($pf,$pv)  = rlevec($p);
my $pf_expect = pdl(long,[3,2,1,0,0,0]);
my $pv_expect = pdl([[1,2],[3,4],[5,6],[0,0],[0,0],[0,0]]);
ok all(approx($pf, $pf_expect)), "rlevec():counts";
ok all(approx($pv, $pv_expect)), "rlevec():elts";

my $pd = rldvec($pf,$pv);
ok all(approx($pd, $p)), "rldvec()";

my $pk = enumvec($p);
ok all(approx($pk, pdl(long,[0,1,2,0,1,0]))), "enumvec()";

$pk = enumvecg($p);
ok all(approx($pk, pdl(long,[0,0,0,1,1,2]))), "enumvecg()";

## 6..7: test rleND(): 2d
($pf,$pv) = rleND($p);
ok all(approx($pf, $pf_expect)), "rleND():2d:counts";
ok all(approx($pv, $pv_expect)), "rleND():2d:elts";

## 8..8: test rldND(): 2d
$pd = rldND($pf,$pv);
ok all(approx($pd, $p)), "rldND():2d";

## rleND, rldND: Nd
my $pnd1 = (1  *(sequence(long, 2,3  )+1))->slice(",,*3");
my $pnd2 = (10 *(sequence(long, 2,3  )+1))->slice(",,*2");
my $pnd3 = (100*(sequence(long, 2,3,2)+1));
my $p_nd = $pnd1->mv(-1,0)->append($pnd2->mv(-1,0))->append($pnd3->mv(-1,0))->mv(0,-1);

my $pf_expect_nd = pdl(long,[3,2,1,1,0,0,0]);
my $pv_expect_nd = zeroes($p_nd->type, $p_nd->dims);
(my $tmp=$pv_expect_nd->slice(",,0:3")) .= $p_nd->dice_axis(-1,[0,3,5,6]);

## 9..10: test rleND(): Nd
my ($pf_nd,$pv_nd) = rleND($p_nd);
ok all(approx($pf_nd, $pf_expect_nd)), "rleND():Nd:counts";
ok all(approx($pv_nd, $pv_expect_nd)), "rleND():Nd:elts";

## 11..11: test rldND(): Nd
my $pd_nd = rldND($pf_nd,$pv_nd);
ok all(approx($pd_nd, $p_nd)), "rldND():Nd";

## 12..12: test enumvec(): nd
my $v_nd = $p_nd->clump(2);
my $k_nd = $v_nd->enumvec();
ok all(approx($k_nd, pdl(long,[0,1,2,0,1,0,0]))), "enumvec():Nd";

## 13..17: test rldseq(), rleseq()
my $lens = pdl(long,[qw(3 0 1 4 2)]);
my $offs = (($lens->xvals+1)*100)->short;
my $seqs = zeroes(short, 0);
$seqs  = $seqs->append(sequence(short,$_)) foreach ($lens->list);
$seqs += $lens->rld($offs);

my $seqs_got = $lens->rldseq($offs);
is $seqs_got->type, $seqs->type, "rldseq():type";
ok all(approx($seqs_got, $seqs)), "rldseq():data";

my ($len_got,$off_got) = $seqs->rleseq();
is $off_got->type, $seqs->type, "rleseq():type";
ok all(approx($len_got->where($len_got), $lens->where($lens))), "rleseq():lens";
ok all(approx($off_got->where($len_got), $offs->where($lens))), "rleseq():offs";

done_testing;
