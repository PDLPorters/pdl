# Created on: Fri 14 Dec 2007 07:22:09 PM 
# Last saved: Fri 14 Dec 2007 08:11:13 PM 
#
# This tests the 16-bit image capabilities of the rpic() and wpic()
# commands.  The new code works with PNM output files and PNG format
# too.

# Our new default testing framework
use Test::More;

use PDL;
use PDL::NiceSlice;

BEGIN {
  eval "use PDL::IO::Pic;";
  if ( !$@ ) {
    plan tests => 5;
  } else {
    plan skip_all => 'PDL::IO::Pic not available';
  }
  use_ok('PDL::IO::Pic');
};

# test save/restore of 8-bit image
my $a = sequence(16,16);
$a->wpic('tbyte_a.pnm');
my $a_pnm = rpic('tbyte_a.pnm');
ok(sum(abs($a-$a_pnm)) == 0, 'pnm byte image save+restore');
unlink 'tbyte_a.pnm';

$a->wpic('tbyte_a.png');
my $a_png = rpic('tbyte_a.png');
ok(sum(abs($a-$a_png)) == 0, 'png byte image save+restore');
unlink 'tbyte_a.png';

# test save/restore of 16-bit image
my $a16 = sequence(256,255)->ushort * 231;
$a16->wpic('tushort_a16.pnm');
my $a16_pnm = rpic('tushort_a16.pnm');
ok(sum(abs($a16-$a16_pnm)) == 0, 'pnm ushort image save+restore');
unlink 'tushort_a16.pnm';

$a16->wpic('tushort_a16.png');
my $a16_png = rpic('tushort_a16.png');
ok(sum(abs($a16-$a16_png)) == 0, 'png ushort image save+restore');
unlink 'tushort_a16.png';

# end
