# -*-perl-*-
#
# we need tests with index shuffling once vaffines are fixed

use Test;
use strict;

#sub ok {
#	my $no = shift ;
#	my $result = shift ;
#	print "not " unless $result ;
#	print "ok $no\n" ;
#}

sub approx {
	my($a,$b,$mdiff) = @_;
	$mdiff = 0.01 unless defined($mdiff);
	my $d = max(abs($a-$b));
	$d < $mdiff;
}

sub rpic_unlink {
  my $file = shift;
  my $pdl = PDL->rpic($file);
  unlink $file;
  return $pdl;
}

sub check {
  my ($err,$i) = @_;
  if ($err =~ /maxval is too large/) {
    print STDERR
       "(to test recompile pbmplus with PGM_BIGGRAYS!)\n"
  } else {
    print STDERR "(would test but unknownm error: $err)\n"
  }
}

sub rgb { $_[0]->getndims == 3 and $_[0]->getdim(0) == 3 }

use PDL::LiteF;
use PDL::IO::Pic;
use PDL::ImageRGB;
use PDL::Dbg;

$PDL::debug = 1;

# change to PNMASCII to use ASCII PNM intermediate output format 
my $iform = 'PNMRAW'; 

#              [FORMAT, extension, ushort-divisor,
#               only RGB/no RGB/any (1/-1/0), mxdiff]
#  no test of PCX format because seems to be severely brain damaged
my %formats = ('PNM'  => ['pnm',1,0,0.01],
	       'GIF'  => ['gif',256,0,1.01],
	       'TIFF' => ['tif',1,0,0.01],
	       'RAST' => ['rast',256,0,1.01],
	       'IFF'  => ['iff',256,1,0.01],
	       'SGI'  => ['rgb',1,0,0.01],
	       );

my @allowed = ();
for (PDL->wpiccan) { 
    push @allowed, $_
	if PDL->rpiccan($_) and defined $formats{$_} 
}

my $ntests = 3 * @allowed;  # -1 due to TIFF converter
$ntests-- if grep /^TIFF$/, @allowed;
if ($ntests < 1) {
    print("1..1\nok 1\n"); # dummy
    exit;
}

#print("1..$ntests\n");
plan tests => $ntests;

print "Testable formats on this platform:\n  ".join(',',@allowed)."\n";

my $im1 = pdl([[0,65535,0], [256,256,256], [65535,256,65535]])->ushort;
my $im2 = byte $im1/256;

# make the resulting file at least 12 byte long
# otherwise we run into a problem when reading the magic (Fix!)
my $im3 = PDL::byte [[0,0,255,255,12,13],[1,4,5,6,11,124],
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
my $n = 1;
my $usherr=0;
my $in1;
foreach my $format (sort @allowed) {
    print " ** testing $format format **\n";
    my $form = $formats{$format};

    eval '$im1->wpic("tushort.$form->[0]",{IFORM => "$iform"})'
      unless $format eq 'TIFF';
    if ($format ne 'TIFF' and $@) { check($@,$n); $usherr = 1; } else {$usherr=0;}
    $im2->wpic("tbyte.$form->[0]",{IFORM => "$iform"});
    $im3->wpic ("tbin.$form->[0]",{COLOR => 'bw', IFORM => "$iform"});

    $in1 = rpic_unlink("tushort.$form->[0]") unless
        $usherr or $format eq 'TIFF';
    my $in2 = rpic_unlink("tbyte.$form->[0]");
    my $in3 = rpic_unlink("tbin.$form->[0]");

    my $comp;
    if ($format ne 'TIFF') {
      my $scale = ($form->[2] or rgb($in1) ? $im1->dummy(0,3) : $im1);
      $comp = $scale / PDL::ushort($form->[1]);
      skip($usherr, approx($comp,$in1,$form->[3]));
    }
    $comp = ($form->[2] or rgb($in2) ? $im2->dummy(0,3) : $im2);
    ok(approx($comp,$in2));
    $comp = ($form->[2] or rgb($in3) ? ($im3->dummy(0,3)>0)*255 : ($im3 > 0));
    $comp = $comp->ushort*$in3->max if $format eq 'SGI' and $in3->max > 0;
    ok(approx($comp,$in3));

    if ($PDL::debug) {
      print $in1->px unless $format eq 'TIFF';
      print $in2->px;
      print $in3->px;
    }
}
