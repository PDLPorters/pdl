use strict;
use warnings;
use Test::More;
use Storable qw/freeze thaw retrieve/;
use PDL::LiteF;
use PDL::Dbg;
use PDL::IO::Storable;

my $x = sequence(2,2);
# $x->dump;

my $serialized = freeze $x;

my $oldx = thaw $serialized;
# $oldx->dump;

is sum(abs($x-$oldx)), 0, 'PDL freeze/thaw';

$x = double '1';
$serialized = freeze $x;
my $dthaw = thaw $serialized;
is $dthaw, $x, 'PDL freeze/thaw of PDL scalar';

# $oldb = thaw $serialized;
# $oldc = thaw $serialized;
# 
# $PDL::Dbg::Infostr = "%T %D %S %A";
# PDL->px;
# 
# undef $oldb;
# print $oldc;

undef $x;

my $data = {
   key1 => 1,
   key2 => sequence(3),
   key3 => 'hallo',
};

my $dfreeze = freeze $data;
$dthaw = thaw $dfreeze;

isa_ok($dthaw, 'HASH'); # we got a HASH back

ok(all($data->{key2} == $dthaw->{key2}), 'PDL in structure');

my $phash = bless {PDL => sequence 3}, 'PDL';
can_ok($phash, 'freeze');

my $pfreeze = $phash->freeze;
my $phthaw = thaw $pfreeze;

ok(all($phthaw == $phash), 'PDL has-a works with freeze/thaw');
isa_ok($phthaw,'HASH', 'PDL is a hash');

# Test that freeze + thaw results in new object
my $seq1 = sequence(3);
my $seq1_tf = thaw(freeze($seq1));
$seq1->slice('1') .= 9;
ok(! all($seq1 == $seq1_tf), 'Initialization from seraialized object') or
    diag($seq1, $seq1_tf);

# Test that dclone results in a new object
# i.e. that dclone(.) == thaw(freeze(.))
my $seq2 = sequence(4);
my $seq2_dc = Storable::dclone($seq2);
$seq2->slice('2') .= 8;
ok(! all($seq2 == $seq2_dc), 'Initialization from dclone object') or
    diag($seq2, $seq2_dc);

{
  my @w;
  local $SIG{__WARN__} = sub { push @w, @_ };
  thaw( freeze pdl([]) );
  is "@w", '', 'no warnings';
}

# Now test reading from files
testLoad($_) foreach( qw(t/storable_new_amd64.dat t/storable_old_amd64.dat) );

done_testing;

# tests loading some files made on different architectures. All these files were
# made with this:
#
#   use PDL;
#   use PDL::IO::Storable;
#   use Storable qw(nstore);
#   my $x = sequence(3,3)->byte * sequence(3)->byte;
#   my $y = 50 + sequence(7)->double;
#   nstore [$x, 'abcd', $y], "/tmp/tst.dat";
#
# I make sure these all were read correctly
sub testLoad
{
  my $filename = shift;

  # if we're on a big endian machine, the old-style data will be bogus so I skip
  # the tests in that case
SKIP:
  {
    if ( $filename =~ /_old_/ )
    {
      my ($byte0) = unpack( 'C*', pack( 'l', 1 ));
      if ( $byte0 == 0 )
      {
        skip "On a big endian machine the old stored files will be bogus", 7;
      }
    }

    my $x = retrieve $filename;
    ok( defined $x, "Reading from file '$filename'" );
    ok( @$x == 3, "Reading an array-ref of size 3 from file '$filename'" );
    ok( $x->[1] eq 'abcd', "Reading a correct string from file '$filename'" );
    isa_ok( $x->[0], 'PDL', "Reading an ndarray from file '$filename'" );
    isa_ok( $x->[2], 'PDL', "Reading another ndarray from file '$filename'" );

    my $diff0 = $x->[0] - pdl[[0,1,4],
                              [0,4,10],
                              [0,7,16]];
    my $diff2 = $x->[2] - (50 + sequence(7));

    ok( $diff0->max == 0, "Read correct data from file '$filename'" );
    ok( $diff2->max == 0, "Read more correct data from file '$filename'" );
  }
}
