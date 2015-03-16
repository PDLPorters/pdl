use Test::More tests => 4;
use PDL::LiteF;

if($^O !~ /mswin32/i) {$SIG{BUS} = \&not_ok}
$SIG{SEGV} = \&not_ok;

sub not_ok {
	print STDERR "\ngot fatal signal\n";
	exit;
}

# PDL::Core::set_debugging(1);
$b = pdl [[1,1,1],[2,2,2]];

# we are using more dims than are available
$i = 1;
eval {$c = $b->slice(':,:,:,(1)'); $c->make_physical();};
like $@, qr/too many dims/i;

$i++;
# now see if we survive the destruction of this invalid trans
$b = zeroes(5,3,3);
$c = $b->slice(":,:,1");
ok 1;  # if we're here we survived

$i++;
$b = pdl [[1,1,1],[2,2,2]];
eval {$c = $b->dummy(5,1); $c->make_physical();};
is $@, '';

$i++;
$b = zeroes(5,3,3);
$c = $b->slice(":,:,1");
ok(1);

# if we're here we survived


