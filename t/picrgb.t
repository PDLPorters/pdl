# we need tests with index shuffling once vaffines are fixed

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub approx {
	my($a,$b,$mdiff) = @_;
        return 0 if $a->getdim(0) != $b->getdim(0) ||
                    $a->getdim(1) != $b->getdim(1);
	$mdiff = 0.01 unless defined($mdiff);
	my $c = abs($a-$b);
	my $d = max($c);
	$d < $mdiff;
}

sub rpic_unlink {
  my $file = shift;
  my $pdl = PDL->rpic($file);
  unlink $file;
  return $pdl;
}

sub depends_on {
  print "ushort is ok with $_[0]\n"
	if $PDL::IO::Pic::converter{$_[0]}->{ushortok};
  return 1 if $PDL::IO::Pic::converter{$_[0]}->{ushortok};
  return 256;
}

sub mmax { return $_[0] > $_[1] ? $_[0] : $_[1] }

$::warned = 0;
sub tifftest {
  my ($form) = @_;
  return 0 unless $form eq 'TIFF';
  warn "WARNING: you are probably using buggy tiff converters.
     Check IO/Pnm/converters for patched source files\n" unless $::warned;
  $warned = 1;
  return 1;
}

use PDL;
use PDL::IO::Pic;
use PDL::ImageRGB;
use PDL::Dbg;

$PDL::debug = 0;
$iform=$iform = 'PNMRAW'; # change to PNMASCII to use ASCII PNM intermediate
                   # output format

#              [FORMAT, extension, ushort-divisor,
#               only RGB/no RGB/any (1/-1/0), mxdiff]
#  no test of PCX format because seems to be severely brain damaged
%formats = ('PNM'  => ['pnm',1,0,0.01],
	    'GIF'  => ['gif',256,0,1.01],
	    'TIFF' => ['tif',1,0,0.01],
  	    'RAST' => ['rast',256,0,0.01],
	    'IFF'  => ['iff',256,1,0.01],
	    'SGI'  => ['rgb',1,1,0.01],
	   );

@allowed = ();
for (PDL->wpiccan) { push @allowed, $_
	if PDL->rpiccan($_) && defined $formats{$_} }

$ntests = 2 * (@allowed);
if ($ntests < 1) {
  print("1..1\nok 1\n"); # dummy
  exit;
}

print("1..$ntests\n");

print "Testable formats on this platform:\n  ".join(',',@allowed)."\n";


$im1 = ushort pdl [[[0,0,0],[256,65535,256],[0,0,0]],
		   [[256,256,256],[256,256,256],[256,256,256]],
		   [[2560,65535,2560],[256,2560,2560],[65535,65534,65535]]];
$im2 = byte ($im1/256);

if ($PDL::debug){
   print $im1;
   print $im2;
}

$n = 1;
foreach $form (sort @allowed) {
    print " ** testing $form format **\n";

    $arr = $formats{$form};
eval <<'EOD';
    $im1->wpic("tushort.$arr->[0]",{IFORM => $iform});
    $im2->wpic("tbyte.$arr->[0]",{IFORM => $iform});

    $in1 = rpic_unlink("tushort.$arr->[0]");
    $in2 = rpic_unlink("tbyte.$arr->[0]");

    $comp = $im1 / PDL::ushort(mmax(depends_on($form),$arr->[1]));
EOD
    print "Comparison arr: $comp" if $PDL::debug;
    ok($n++,approx($comp,$in1,$arr->[3]) || tifftest($form));
    ok($n++,approx($im2,$in2) || tifftest($form));

    if ($PDL::debug) {
      print $in1->px;
      print $in2->px;
    }
}
