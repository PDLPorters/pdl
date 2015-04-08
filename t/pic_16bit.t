# Created on: Fri 14 Dec 2007 07:22:09 PM 
# Last saved: Fri 09 Dec 2011 01:57:56 PM 
#
# This tests the 16-bit image capabilities of the rpic() and wpic()
# commands.  The new code works with PNM output files and PNG format
# too.

# Our new default testing framework
use strict;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;

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
}

$PDL::IO::Pic::debug=20;

# test save/restore of 8-bit image
my $a = sequence(16, 16);
my $tmpdir = tempdir( CLEANUP => 1 );
my $filestub = File::Spec->catfile($tmpdir, 'tbyte_a');
$a->wpic("$filestub.pnm");
my $a_pnm = rpic("$filestub.pnm");
ok(sum(abs($a-$a_pnm)) == 0, 'pnm byte image save+restore');
unlink "$filestub.pnm";

SKIP: {
  skip ": pnmtopng not found, is NetPBM installed?", 1 unless $test_pnmtopng; 
  $a->wpic("$filestub.png");
  my $a_png;
  unless ($^O =~ /MSWin32/i) { $a_png = rpic("$filestub.png") }
  else { $a_png = rpic("$filestub.png", {FORMAT => 'PNG'}) }
  ok(sum(abs($a-$a_png)) == 0, 'png byte image save+restore'); #test 3
  unlink "$filestub.png";
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
