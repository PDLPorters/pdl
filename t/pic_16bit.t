# Created on: Fri 14 Dec 2007 07:22:09 PM 
# Last saved: Fri 02 Dec 2011 12:55:49 PM 
#
# This tests the 16-bit image capabilities of the rpic() and wpic()
# commands.  The new code works with PNM output files and PNG format
# too.

# Our new default testing framework
use strict;
no warnings qw(misc);
use Test::More;

use PDL;
use PDL::NiceSlice;

my ($test_pnmtopng);

BEGIN {
   eval "use PDL::IO::Pic;";
   if ( !$@ ) {
      $test_pnmtopng = 1;
      plan tests => 5;
      if($^O =~ /MSWin32/i) {
         $test_pnmtopng = `pnmtopng --help 2>&1`;
         $test_pnmtopng = $test_pnmtopng =~ /^pnmtopng:/ ? 1 : 0;
      } elsif ( !defined( scalar( qx(pnmtopng --help 2>&1) ) ) ) {
         $test_pnmtopng = 0;
      } 
   } else {
      plan skip_all => 'PDL::IO::Pic not available'
   }
   use_ok('PDL::IO::Pic');
};

$PDL::IO::Pic::debug=20;

# test save/restore of 8-bit image
my $a = sequence(16, 16);
$a->wpic('tbyte_a.pnm');
my $a_pnm = rpic('tbyte_a.pnm');
ok(sum(abs($a-$a_pnm)) == 0, 'pnm byte image save+restore');
unlink 'tbyte_a.pnm';

SKIP: {
  skip ": pnmtopng not found, is NetPBM installed?", 1 unless $test_pnmtopng; 
  $a->wpic('tbyte_a.png');
  my $a_png;
  unless($^O =~ /MSWin32/i) {$a_png = rpic('tbyte_a.png')}
  else {$a_png = rpic('tbyte_a.png', {FORMAT => 'PNG'})}
  ok(sum(abs($a-$a_png)) == 0, 'png byte image save+restore'); #test 3
  unlink 'tbyte_a.png';
};

# test save/restore of 16-bit image
my $a16 = sequence(256, 255)->ushort * 231;
$a16->wpic('tushort_a16.pnm');
my $a16_pnm = rpic('tushort_a16.pnm');
ok(sum(abs($a16-$a16_pnm)) == 0, 'pnm ushort image save+restore'); # test 4
unlink 'tushort_a16.pnm';

SKIP : {
  skip ": pnmtopng not found, is NetPBM installed?", 1 unless $test_pnmtopng;
  $a16->wpic('tushort_a16.png');
  my $a16_png;
  unless($^O =~ /MSWin32/i) {$a16_png = rpic('tushort_a16.png')}
  else {$a16_png = rpic('tushort_a16.png', {FORMAT => 'PNG'})} 
  ok(sum(abs($a16-$a16_png)) == 0, 'png ushort image save+restore'); # test 5 (fails on Win32 if not skipped)
  unlink 'tushort_a16.png';
  };

# end
