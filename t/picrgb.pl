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
use PDL::Dbg;

# private fix
$ENV{PATH} .= ":$ENV{HOME}/perl/netpbm/bin" if `hostname` =~ /mbcsg1/;
$PDL::debug = 0;
$PDL::Debug = 0;
$iform = 'PNMRAW'; # change to PNMASCII to use ASCII PNM intermediate
                   # output format

#              [FORMAT, extension, ushort-divisor,
#               only RGB/no RGB/any (1/-1/0), mxdiff]
#  no test of PCX format because seems to be severely brain damaged
@formats = (['PNM','pnm',1,0,0.01],['GIF','gif',256,0,1.01],
	       ['TIFF','tif',1,0,0.01],['Sun Raster','rast',256,0,0.01],
	       ['IFF','iff',256,1,0.01],['SGI','rgb',1,1,0.01]);

$ntests = 2 * @formats;
print("1..$ntests\n");

$im1 = ushort pdl [[[0,0,0],[256,65535,256],[0,0,0]],
		   [[256,256,256],[256,256,256],[256,256,256]],
		   [[2560,65535,2560],[256,2560,2560],[65535,65534,65535]]];
$im2 = byte ($im1/256);

if ($PDL::debug){
   print $im1;
   print $im2;
}

$n = 1;
foreach $form (@formats) {
    print " ** testing $form->[0] format **\n";

    wpic ($im1,"tushort.$form->[1]",{IFORM => $iform});
    wpic ($im2,"tbyte.$form->[1]",{IFORM => $iform});

    $in1 = rpic_unlink("tushort.$form->[1]");
    $in2 = rpic_unlink("tbyte.$form->[1]");

    $comp = $im1 / $form->[2];
    ok($n++,approx($comp,$in1,$form->[4]));
    ok($n++,approx($im2,$in2));

    if ($PDL::debug) {
      print $in1->px;
      print $in2->px;
    }
}
