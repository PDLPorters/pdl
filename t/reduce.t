use Test::More tests => 5;
use PDL::LiteF;
use PDL::Reduce;

use strict;
use warnings;

my $pa = sequence 5,5;
my $pb = $pa->reduce('add',0);

ok(all $pb == $pa->sumover);
ok(all $pa->reduce('add',1) == $pa->mv(1,0)->sumover);
ok(all $pa->reduce('mult',1) == $pa->mv(1,0)->prodover);
# test the new reduce features
ok($pa->reduce('+',0,1) == sum $pa); # reduce over list of dims
ok(all $pa->reduce(\&PDL::sumover) == $pa->sumover); # use code refs
