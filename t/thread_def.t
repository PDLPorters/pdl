use Test::More tests => 5;
use PDL::LiteF;
use Test::Exception;

use strict;
use warnings;

my $debug = 0;
$PDL::debug = 1;

my $pa = sequence(3,4);
my $pb = yvals(zeroes(4,3)) + sequence(4);
my $pc = $pa->xchg(0,1)->slice(':,-1:0');

# not very useful examples but simple and test the essentials
thread_define 'tline(a(n);b(n))', over {
    $_[0] .= $_[1];
};

thread_define 'tassgn(a(n,m);[o] b())', over {
    # sumover($_[0],$_[1]);
    $_[1] .= $_[0]->sum;
};

thread_define 'ttext(a(n=3)), NOtherPars => 1', over {
    ${$_[1]} .= sprintf("%.3f %.3f %.3f,\n",$_[0]->list);
  #join(' ',$_[0]->list) . ",\n";
};

thread_define 'tprint(a(n);b(n)), NOtherPars => 1', over {
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

# test if dim=1 -> threaddim
note "testing tprint\n";
$pa = sequence(3);
$pb = pdl [1];
my $text = "";
tprint($pa, $pb, \$text);
is $text, '[1 1 1]';
