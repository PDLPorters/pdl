use Test::More tests => 5;
use PDL::LiteF;
use PDL::Reduce;

$a = sequence 5,5;
$b = $a->reduce('add',0);

ok(all $b == $a->sumover);
ok(all $a->reduce('add',1) == $a->mv(1,0)->sumover);
ok(all $a->reduce('mult',1) == $a->mv(1,0)->prodover);
# test the new reduce features
ok($a->reduce('+',0,1) == sum $a); # reduce over list of dims
ok(all $a->reduce(\&PDL::sumover) == $a->sumover); # use code refs
