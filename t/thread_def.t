use Test::More;
use PDL::LiteF;
use Test::Exception;

use strict;
use warnings;

my $debug = 0;
$PDL::debug = $debug;

my $pa = sequence(3,4);
my $pb = yvals(zeroes(4,3)) + sequence(4);
my $pc = $pa->transpose->slice(':,-1:0');

# make sure compat alias works
thread_define 'tline(a(n);b(n))', over {
    $_[0] .= $_[1];
};

# not very useful examples but simple and test the essentials
broadcast_define 'tassgn(a(n,m);[o] b())', over {
    # sumover($_[0],$_[1]);
    $_[1] .= $_[0]->sum;
};

broadcast_define 'ttext(a(n=3)), NOtherPars => 1', over {
    ${$_[1]} .= sprintf("%.3f %.3f %.3f,\n",$_[0]->list);
  #join(' ',$_[0]->list) . ",\n";
};

broadcast_define 'tprint(a(n);b(n)), NOtherPars => 1', over {
	${$_[2]} .= "$_[1]";
};

PDL::Core::set_debugging(1) if $debug;
tline($pc,$pb);

note $pa;
note $pb;

ok(all approx($pc,$pb));

$pc = ones(5); # produce an error
throws_ok {
	tline($pa,$pc);
} qr/conflicting/;

$pa = ones(2,3,4)*sequence(4)->slice('*,*,:');
note $pa;
tassgn($pa,($pb=null));
note "$pb\n";
$pb->dump;
ok(all approx($pb,6*sequence(4)));

# test if setting named dim with '=' raises error
# correctly at runtime
$pa = sequence(4,4);
throws_ok {
	ttext($pa, \my $text);
} qr/conflicting/;

# test if dim=1 -> broadcastdim
note "testing tprint\n";
$pa = sequence(3);
$pb = pdl [1];
my $text = "";
tprint($pa, $pb, \$text);
is $text, '[1 1 1]';

# cut down from PDL::Apply which got broken by 2.057_01
thread_define '_apply_slice_ND(data(n);sl(2,m);[o]output(m)),NOtherPars=>2', over {
  _apply_slice_1D($_[1], ones($_[0]->type), my $output = null, @_[0,3,4]);
  $_[2] .= $output;
};
thread_define '_apply_slice_1D(slices(n);dummy();[o]output()),NOtherPars=>3', over {
  my $func = $_[4];
  my $args = $_[5];
  my $data = slice($_[3], $_[0]->unpdl);
  $_[2] .= PDL::Core::topdl($data->$func(@$args));
};
my $x = sequence(5,3,2);
my $slices = indx([0,2], [1,3], [2,4]);
my $y = null;
lives_ok { _apply_slice_ND($x, $slices, $y, 'sum', []) };

done_testing;
