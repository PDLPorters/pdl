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
my $pdl = sequence(long,5);
my $native_frozen = freeze $pdl;
my $f2 = $native_frozen;
is_pdl thaw($native_frozen), sequence(long,5), "thawed native";
my $one = substr($native_frozen, -40, 4); # count from back as Storable uses platform data sizes
$one = pack "N", unpack "V", $one;
my $data = substr($native_frozen, -20, 20);
$data = pack "N*", unpack "V*", $data;
substr $f2, -40, 4, $one;
substr $f2, -20, 20, $data;
is_pdl thaw($f2), sequence(long,5), "thawed byte-swapped";
}

#  packages supporting Types::Serialiser protocol
{
  #  add to if needed
  my @possibles = qw/Sereal CBOR::XS JSON::MaybeXS/;

  my @serialisers;
  for my $module (@possibles) {
    if (eval "require $module") {
      push @serialisers, $module;
    }
    else {
      note "package $module not available for serialisation, not testing it";
    }
  }

  if (!@serialisers) {
    diag "No serialisation modules installed that support the Types::Serialiser protocol, skipping those tests";
  }

  my @ndarrays = (
      [ xvals => xvals(2, 2) ],
      [ cdouble => pdl(cdouble, 2, 3) ],
      [ cdouble2 => xvals(cdouble, 3, 5) + 10 - 2 * xvals(3, 5) * i ],
      [ indx => pdl(indx, 2, 3) ],
      [ ldouble => pdl(ldouble, 2, 3) ],
  );

  my ($encoder, $decoder);

  foreach my $serialiser (@serialisers) {
    if ($serialiser eq 'Sereal') {
      $encoder = Sereal::Encoder->new({ freeze_callbacks => 1 });
      $decoder = Sereal::Decoder->new({ freeze_callbacks => 1 });
    }
    elsif ($serialiser eq 'CBOR::XS') {
      $encoder = CBOR::XS->new;
      $decoder = CBOR::XS->new;
    }
    elsif ($serialiser eq 'JSON::MaybeXS') {
      $encoder = JSON::MaybeXS->new(allow_tags => 1);
      $decoder = JSON::MaybeXS->new(allow_tags => 1);
    }

    foreach my $pair (@ndarrays) {
      my ($name, $ndarray) = @$pair;

      my $frozen = $encoder->encode($ndarray);
      my $thawed = $decoder->decode($frozen);

      is_pdl($thawed, $ndarray, "$name thawed correctly using $serialiser");
    }
  }
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
