# -*- cperl -*-

use strict;
use Test::More;

BEGIN { 
   eval 'use Storable 1.03';
   unless ($@) {
      eval 'require 5.010';
      unless ($@) {
         plan tests => 23;
      } else {
         plan skip_all => "PDL::IO::Storable requires perl 5.10 or greater\n";
      }
   } else {
      plan skip_all => "Storable >= 1.03 not installed\n";
   }
   use Storable qw/freeze thaw retrieve/;
}

BEGIN { 
   use PDL::LiteF;
   use PDL::Dbg;
   use_ok('PDL::IO::Storable');
}

my ($data,$dfreeze,$dthaw,$olda,$pfreeze,$phash,$phthaw,$seq1,$seq1_tf,$seq2,$seq2_dc,$serialized);

$a = sequence(2,2);
# $a->dump;

$serialized = freeze $a;

$olda = thaw $serialized;
# $olda->dump;

ok(sum(abs($a-$olda))==0, 'PDL freeze/thaw');

# $oldb = thaw $serialized;
# $oldc = thaw $serialized;
# 
# $PDL::Dbg::Infostr = "%T %D %S %A";
# PDL->px;
# 
# undef $oldb;
# print $oldc;

undef $a;

$data = {
   key1 => 1,
   key2 => sequence(3),
   key3 => 'hallo',
};

$dfreeze = freeze $data;
$dthaw = thaw $dfreeze;

isa_ok($dthaw, 'HASH'); # we got a HASH back

ok(all($data->{key2} == $dthaw->{key2}), 'PDL in structure');

$phash = bless {PDL => sequence 3}, 'PDL';
can_ok($phash, 'freeze');

$pfreeze = $phash->freeze;
$phthaw = thaw $pfreeze;

ok(all($phthaw == $phash), 'PDL has-a works with freeze/thaw');
ok(UNIVERSAL::isa($phthaw,'HASH'), 'PDL is a hash');

# Test that freeze + thaw results in new object
$seq1 = sequence(3);
$seq1_tf = thaw(freeze($seq1));
$seq1->slice('1') .= 9;
ok(! all($seq1 == $seq1_tf), 'Initialization from seraialized object') or
    diag($seq1, $seq1_tf);

# Test that dclone results in a new object
# i.e. that dclone(.) == thaw(freeze(.))
$seq2 = sequence(4);
$seq2_dc = Storable::dclone($seq2);
$seq2->slice('2') .= 8;
ok(! all($seq2 == $seq2_dc), 'Initialization from dclone object') or
    diag($seq2, $seq2_dc);


# Now test reading from files
testLoad($_) foreach( qw(t/storable_new_amd64.dat t/storable_old_amd64.dat) );




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
    isa_ok( $x->[0], 'PDL', "Reading a piddle from file '$filename'" );
    isa_ok( $x->[2], 'PDL', "Reading another piddle from file '$filename'" );

    my $diff0 = $x->[0] - pdl[[0,1,4],
                              [0,4,10],
                              [0,7,16]];
    my $diff2 = $x->[2] - (50 + sequence(7));

    ok( $diff0->max == 0, "Read correct data from file '$filename'" );
    ok( $diff2->max == 0, "Read more correct data from file '$filename'" );
  }
}
