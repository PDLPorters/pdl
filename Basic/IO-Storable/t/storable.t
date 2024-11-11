use strict;
use warnings;
use Test::More;
use Test::PDL;
use Storable qw/freeze thaw retrieve/;
use PDL::LiteF;
use PDL::Dbg;
use PDL::IO::Storable;

my $x = sequence(2,2);
my $serialized = freeze $x;
my $oldx = thaw $serialized;
is_pdl $x, $oldx, 'PDL freeze/thaw';

$x = double(1);
$serialized = freeze $x;
my $dthaw = thaw $serialized;
is_pdl $dthaw, $x, 'PDL freeze/thaw of PDL scalar';

my $data = {
   key1 => 1,
   key2 => sequence(3),
   key3 => 'hallo',
};

my $dfreeze = freeze $data;
$dthaw = thaw $dfreeze;

isa_ok($dthaw, 'HASH'); # we got a HASH back
is_pdl $dthaw->{key2}, $data->{key2}, 'PDL in structure';

my $phash = bless {PDL => sequence 3}, 'PDL';
can_ok($phash, 'freeze');
my $pfreeze = $phash->freeze;
my $phthaw = thaw $pfreeze;

is_pdl $phthaw, $phash, 'PDL has-a works with freeze/thaw';
isa_ok $phthaw, 'HASH', 'PDL is a hash';

my $seq1 = sequence(3);
my $seq1_tf = thaw(freeze($seq1));
$seq1->slice('1') .= 9;
is_pdl $seq1, pdl(0,9,2);
is_pdl $seq1_tf, sequence(3), 'mutate orig no change thawed object';

# Test that dclone results in a new object
# i.e. that dclone(.) == thaw(freeze(.))
my $seq2 = sequence(4);
my $seq2_dc = Storable::dclone($seq2);
$seq2->slice('2') .= 8;
is_pdl $seq2, pdl(0,1,8,3);
is_pdl $seq2_dc, sequence(4), 'mutate orig no change dcloned object';

{
  my @w;
  local $SIG{__WARN__} = sub { push @w, @_ };
  thaw( freeze pdl([]) );
  is "@w", '', 'no warnings';
}

# Now test reading from files
testLoad($_) foreach( qw(t/storable_new_amd64.dat t/storable_old_amd64.dat) );

{
my $pdl = sequence(5);
my $native_frozen = freeze $pdl;
my $f2 = $native_frozen;
is_pdl thaw($native_frozen), sequence(5), "thawed native";
my $one = substr($native_frozen, -60, 4); # count from back as Storable uses platform data sizes
PDL::swapEndian($one, 4);
my $data = substr($native_frozen, -40, 40);
PDL::swapEndian($data, 8);
substr $f2, -60, 4, $one;
substr $f2, -40, 40, $data;
is_pdl thaw($f2), sequence(5), "thawed byte-swapped";
}

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
    is 0+@$x, 3, "Reading an array-ref of size 3 from file '$filename'";
    is_pdl $x->[0], byte [[0,1,4], [0,4,10], [0,7,16]];
    is $x->[1], 'abcd', "Reading a correct string from file '$filename'";
    is_pdl $x->[2], 50 + sequence(7);
  }
}
