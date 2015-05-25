use Test::More tests => 7;

use PDL::LiteF;
use strict;
use warnings;

$|=1;
kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

{
my $pa = pdl [2,3,4],[5,6,7];
$pa->doflow;

#print $pa;
# $pa->jdump;

my $a2 = pdl 1;

my $pb = $pa + $a2;
ok("$pb" eq <<END);

[
 [3 4 5]
 [6 7 8]
]
END

my $pc = $pb * 2; # This should stay the same flowed structure.

ok("$pc" eq <<END);

[
 [ 6  8 10]
 [12 14 16]
]
END
}

{
# Then, the more difficult ways: explicit threading.

# Dims: 3,3,2
my $pa = pdl [[0,1,2],[3,4,5],[6,7,8]],[[10,11,12],[13,14,15],[16,17,18]];
# print $pa;

my $pb = zeroes(3,3);
my $pc = $pb->thread(0,1);
$pb->make_physical();
$pc->make_physical();

# print "B:\n"; $pb->dump(); print "C:\n";$pc->dump();

maximum($pa->thread(0,1),$pc);
# print "B:\n"; $pb->dump(); print "C:\n";$pc->dump();
# print $pb;

ok($pb->at(0,0) == 10);
ok($pb->at(1,1) == 14);

# print "B:\n"; $pb->dump(); print "C:\n";$pc->dump();
minimum($pa->thread(0,1),$pb->thread(0,1));
# print $pb;

ok($pb->at(0,0) == 0);
ok($pb->at(1,1) == 4);
}


{
# Now, test 'unthread'.
my $pa = zeroes(4,5,6);
my $pb = $pa->thread(1);
my $pc = $pb->unthread(2);

ok((join ',',$pc->dims) eq "4,6,5");

# $pb->jdump; $pc->jdump;
}

{
#### Now, test whether the Perl-accessible thread works:

my $pa = pdl [[0,1,2],[3,4,5],[6,7,8]],[[10,11,12],[13,14,15],[16,17,18]];
my $pb = pdl [2,3,4];

PDL::threadover_n($pa,$pb,sub {print "ROUND: @_\n"});

# As well as with virtuals...

PDL::threadover_n($pa->slice("-1:0,-1:0"),$pb,sub {print "ROUND: @_\n"});
}

done_testing;
