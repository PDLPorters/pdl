# we need tests with index shuffling once vaffines are fixed

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub approx {
	my($a,$b,$mdiff) = @_;
	$mdiff = 0.01 unless defined($mdiff);
	$c = abs($a-$b);
	$d = max($c);
	$d < $mdiff;
}

sub rpic_unlink {
  my $file = shift;
  my $pdl = rpic($file);
  unlink $file;
  return $pdl;
}


use PDL;
use PDL::IO::Pic;
use PDL::Lib::Util;
use PDL::Dbg;

# private fix
$ENV{PATH} .= ":$ENV{HOME}/perl/netpbm/bin" if `hostname` =~ /mbcsg1/;
$PDL::debug = 0;
$iform = 'PNMRAW'; # change to PNMASCII to use ASCII PNM intermediate
                   # output format

#              [FORMAT, extension, ushort-divisor,
#               only RGB/no RGB/any (1/-1/0), mxdiff]
#  no test of PCX format because seems to be severely brain damaged
@formats = (['PNM','pnm',1,0,0.01],['GIF','gif',256,0,1.01],
	       ['TIFF','tif',1,0,0.01],['Sun Raster','rast',256,0,1.01],
	       ['IFF','iff',256,1,0.01],['SGI','rgb',1,0,0.01]);

$ntests = 3 * @formats - 1 ;  # -1 due to TIFF converter
print("1..$ntests\n");

$im1 = pdl([[0,65535,0], [256,256,256], [65535,256,65535]])->ushort;
$im2 = byte($im1/256);

# make the resulting file at least 12 byte long
# otherwise we run into a problem when reading the magic (Fix!)
$im3 = byte [[0,0,255,255,12,13],[1,4,5,6,11,124],
	     [100,0,0,0,10,10],[2,1,0,1,0,14],[2,1,0,1,0,14],
	     [2,1,0,1,0,14]];

if ($PDL::debug) {
  print $im1;
  $im1->px;
  print $im2;
  $im2->px;
  print $im3>0;
  $im3->px;
}

# for some reason the pnmtotiff converter coredumps when trying
# to do the conversion for the ushort data, haven't yet tried to
# figure out why
$n = 1;
foreach $form (@formats) {
    print " ** testing $form->[0] format **\n";

    wpic ($im1,"tushort.$form->[1]",{IFORM => "$iform"})
      unless $form->[0] eq 'TIFF';
    wpic ($im2,"tbyte.$form->[1]",{IFORM => "$iform"});
    wpic ($im3,"tbin.$form->[1]",{COLOR => 'bw', IFORM => "$iform"});
    $in1 = rpic_unlink("tushort.$form->[1]") unless $form->[0] eq 'TIFF';
    $in2 = rpic_unlink("tbyte.$form->[1]");
    $in3 = rpic_unlink("tbin.$form->[1]");

    if ($form->[0] ne 'TIFF') {
      $scale = ($form->[3] ? $im1->dummy(0,3) : $im1);
      $comp = $scale / $form->[2];
      ok($n++,approx($comp,$in1,$form->[4]));
    }
    $comp = ($form->[3] ? $im2->dummy(0,3) : $im2);
    ok($n++,approx($comp,$in2));
    $comp = ($form->[3] ? ($im3->dummy(0,3)>0)*255 : ($im3 > 0));
    $comp = $comp->ushort*65535 if $form->[0] eq 'SGI'; # yet another format quirk
    ok($n++,approx($comp,$in3));

    if ($PDL::debug) {
      print $in1->px unless $form->[0] eq 'TIFF';
      print $in2->px;
      print $in3->px;
    }
}
