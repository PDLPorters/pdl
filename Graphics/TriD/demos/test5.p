use strict;
use warnings;
use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Graph;

my $size = 30;
my $y = PDL->zeroes(3,$size,$size);
axisvalues($y->slice("(0)")->inplace);
axisvalues($y->slice("(1)")->transpose->inplace);
$y /= $size;
random($y->slice("(2)")->inplace);
(my $tmp = $y->slice("(2)")) /= 5;
my $c = PDL->zeroes(3,$size,$size);
random($c->inplace);

my @objs = (
  ['Lattice'],
  ['SCLattice'],
  ['SLattice'],
  ['SLattice_S', {Smooth=>0}],
  ['SLattice_S'],
);
my $i = 0;
@objs = map [$i++, @$_], @objs;
my ($below_obj, $above_obj) = map [$_, 'Lines'], -1, 0+@objs;

sub mk_trid { "PDL::Graphics::TriD::$_[1]"->new($y+pdl(0,0,$_[0]),$c,$_[2]) }

my $win = PDL::Graphics::TriD::get_current_window();
my $g = PDL::Graphics::TriD::Graph->new;
my @all = [map mk_trid(@$_), $below_obj, @objs, $above_obj];
push @all, map [map mk_trid(@$_), $below_obj, $_, $above_obj], @objs;
for my $these (@all) {
  $g->clear_data;
  $win->clear_viewport;
  $g->default_axes;
  $g->add_dataseries($_) for @$these;
  $g->scalethings;
  $win->clear_objects;
  $win->add_object($g);
  $win->twiddle;
}
