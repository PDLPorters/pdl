use PDL::LiteF;

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b) = @_;
	my($c,$d);
	$c = abs($a-$b);
	$d = max($c);
	$d < 0.01;
}

$debug = $debug = 0;
$PDL::debug = 1;
print "1..5\n";

$a = sequence(3,4);
$b = yvals(zeroes(4,3)) + sequence(4);
$c = $a->xchg(0,1)->slice(':,-1:0');

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
tline($c,$b);

print $a;
print $b;

ok(1,tapprox($c,$b));

$c = ones(5); # produce an error
eval('tline($a,$c)');
print "Error was : $@\n";
ok(2,$@ =~ /conflicting/);

$a = ones(2,3,4)*sequence(4)->slice('*,*,:');
print $a;
tassgn($a,($b=null));
print "$b\n";
$b->dump;
ok(3,tapprox($b,6*sequence(4)));

# test if setting named dim with '=' raises error
# correctly at runtime
$a = sequence(4,4);
$text="";
eval('ttext($a, \$text)');
print "Error was : $@\n";
ok(4,$@ =~ /conflicting/);

# test if dim=1 -> threaddim
print "testing tprint\n";
$a = sequence(3);
$b = pdl [1];
$text = "";
tprint($a, $b, \$text);
ok(5,$text eq '[1 1 1]');
