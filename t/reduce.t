use strict;
use warnings;

use Test::More;
use Test::PDL;
use PDL::LiteF;
use PDL::Reduce;

my $pa = sequence 5,5;
is_pdl $pa->reduce('add',0), $pa->sumover;
is_pdl $pa->reduce('add',1), $pa->mv(1,0)->sumover;
is_pdl $pa->reduce('mult',1), $pa->mv(1,0)->prodover;
# test the new reduce features
is_pdl $pa->reduce('+',0,1), $pa->sum; # reduce over list of dims
is_pdl $pa->reduce(\&PDL::sumover), $pa->sumover; # use code refs

done_testing;
