use strict;
use warnings;
use PDL::IO::Dumper;
use Test::More;
use Test::PDL;
use Config;
use PDL::LiteF;

########### Dump several items and make sure we get 'em back...
# a: trivial
# b: 0-d
# c: inline
# d: advanced expr

my $s;

#  Need a value greater than the uuencode dump threshold.
#  Currently 25 but may change in future.
my $big_size = int (5 + sqrt $PDL::IO::Dumper::med_thresh);
my @big_dims = ($big_size, $big_size);

#  Small thresh is currently 8
my $med_size = int (2 + sqrt $PDL::IO::Dumper::small_thresh);
my @med_dims = ($med_size, $med_size);

eval { $s = sdump({a=>3,b=>pdl(4),c=>xvals(3,3),d=>xvals(@med_dims)}) };
is $@, '', 'Call sdump()'
   or diag("Call sdump() output string:\n$s\n");
my $x = eval $s;
is $@, '', 'Can eval dumped data code' or diag("The output string was '$s'\n");
isa_ok $x, 'HASH', 'HASH was restored';
cmp_ok $x->{a}, '==', 3, 'SCALAR value restored ok';
is_pdl $x->{b}, pdl(4), '0-d PDL restored ok';
is_pdl $x->{c}, xvals(3,3), '3x3 PDL restored ok';
is_pdl $x->{d}, xvals(@med_dims), '"medium" sized PDL restored ok';

########## Dump a uuencoded expr and try to get it back...
# e: uuencoded expr
eval { $s = sdump({e=>xvals(@big_dims)}) };
is $@, '', 'sdump() of "big" PDL to test uuencode dumps';
$x = eval $s;
is $@, '', 'Can eval dumped "big" PDL' or diag 'string: ', $s;
ok((ref $x eq 'HASH'), 'HASH structure for uuencoded "big" PDL restored');
is_pdl $x->{e}, xvals(@big_dims), 'Verify "big" PDL restored data';

########## Check header dumping...
my $y;
eval {
    $x = xvals(2,2);
    $x->sethdr({ok=>1});
    $x->hdrcpy(1);
    $y = xvals(@big_dims);
    $y->sethdr({ok=>2});
    $y->hdrcpy(0);
    $s = sdump([$x,$y,yvals(@big_dims)]);
};
is $@, '', 'Check header dumping';

$x = eval $s;
is $@, '', 'ARRAY can restore';
is ref($x), 'ARRAY' or diag explain $s;
ok eval { $x->[0]->hdrcpy() == 1 && $x->[1]->hdrcpy() == 0 }, 'Check hdrcpy() persist';
ok eval { ($x->[0]->gethdr()->{ok}==1) && ($x->[1]->gethdr()->{ok}==2) }, 'Check gethdr() values persist';

#  GH508
{
    #  need 10 vals to trigger GH508
    my $x = xvals(10);
    my $y1 = $x;
    my $y2 = 2*$x;
    my $y3 = $x*$x;

    my %plots = (
        'x1'=>$x, 'y1'=>$y1,
        'x2'=>$x, 'y2'=>$y2,
        'x3'=>$x, 'y3'=>$y3,
    );

    my $as_string = sdump \%plots;

    my $restored = eval $as_string;

    my @nulls = grep {!defined $restored->{$_}} sort keys %$restored;
    is_deeply \@nulls, [], 'none of the restored items are undef';

    #  test a dump with uuencoded content
    my $u = xvals(@big_dims);
    my @ndarrays = ($u, $u);
    $as_string = sdump \@ndarrays;
    $restored = eval $as_string;
    @nulls = grep {!defined $_} @$restored;
    is_deeply \@nulls, [], 'none of the restored uuencoded items are undef';
}

done_testing;
