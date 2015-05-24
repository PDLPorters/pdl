use PDL::LiteF;
use PDL::IO::Pic;
use PDL::ImageRGB;
use PDL::Dbg;
use File::Temp qw(tempdir);
use File::Spec;

use strict;
use warnings;

# we need tests with index shuffling once vaffines are fixed
use Test::More;

sub tapprox {
	my($pa,$pb,$mdiff) = @_;
	all approx($pa, $pb,$mdiff || 0.01);
}

sub rpic_unlink {
  my $file = shift;
  my $pdl = PDL->rpic($file);
  unlink $file;
  return $pdl;
}

sub rgb { $_[0]->getndims == 3 && $_[0]->getdim(0) == 3 }

$PDL::debug = 1;
my $iform = 'PNMRAW'; # change to PNMASCII to use ASCII PNM intermediate
                      # output format

#              [FORMAT, extension, ushort-divisor,
#               only RGB/no RGB/any (1/-1/0), mxdiff]
#  no test of PCX format because seems to be severely brain damaged
my %formats = ('PNM'  => ['pnm',1,0,0.01],
	       'GIF'  => ['gif',256,0,1.01],
	       'TIFF' => ['tif',1,0,0.01],
#	       'RAST' => ['rast',256,0,1.01],
#	       'SGI'  => ['rgb',1,0,0.01],
	      );

# only test PNM format
# netpbm has too many bugs on various platforms
my @allowed = ();
for ('PNM') { push @allowed, $_
	if PDL->rpiccan($_) && defined $formats{$_} }

my $ntests = 3 * @allowed;  # -1 due to TIFF converter
$ntests-- if grep /^TIFF$/, @allowed;
if ($ntests < 1) {
  plan skip_all => 'No tests';
}

plan tests => $ntests;
note "Testable formats on this platform:\n  ".join(',',@allowed)."\n";

my $im1 = pdl([[0,65535,0], [256,256,256], [65535,256,65535]])->ushort;
my $im2 = byte $im1/256;

# make the resulting file at least 12 byte long
# otherwise we run into a problem when reading the magic (Fix!)
# FIXME
my $im3 = PDL::byte [[0,0,255,255,12,13],[1,4,5,6,11,124],
	     [100,0,0,0,10,10],[2,1,0,1,0,14],[2,1,0,1,0,14],
	     [2,1,0,1,0,14]];

if ($PDL::debug) {
  note $im1;
  $im1->px;
  note $im2;
  $im2->px;
  note $im3>0;
  $im3->px;
}

# for some reason the pnmtotiff converter coredumps when trying
# to do the conversion for the ushort data, haven't yet tried to
# figure out why
my $usherr = 0;
my $tmpdir = tempdir( CLEANUP => 1 );
sub tmpfile { File::Spec->catfile($tmpdir, $_[0]); }
foreach my $format (sort @allowed) {
    note " ** testing $format format **\n";
    my $form = $formats{$format};

    my $tushort = tmpfile("tushort.$form->[0]");
    my $tbyte = tmpfile("tbyte.$form->[0]");
    my $tbin = tmpfile("tbin.$form->[0]");
    eval {
        $im1->wpic($tushort,{IFORM => "$iform"})
    } unless $format eq 'TIFF';
    SKIP: {
        my $additional = '';
        if ($format ne 'TIFF' && $@ =~ /maxval is too large/) {
            $additional = ' (recompile pbmplus with PGM_BIGGRAYS!)';
        }
        skip "Error: '$@'$additional", 2 if $@;
        $im2->wpic($tbyte,{IFORM => "$iform"});
        $im3->wpic($tbin,{COLOR => 'bw', IFORM => "$iform"});
        my $in1 = rpic_unlink($tushort) unless
            $usherr || $format eq 'TIFF';
        my $in2 = rpic_unlink($tbyte);
        my $in3 = rpic_unlink($tbin);

        if ($format ne 'TIFF') {
          my $scale = ($form->[2] || rgb($in1) ? $im1->dummy(0,3) : $im1);
          my $comp = $scale / PDL::ushort($form->[1]);
          ok($usherr || tapprox($comp,$in1,$form->[3]));
        }
        {
        my $comp = ($form->[2] || rgb($in2) ? $im2->dummy(0,3) : $im2);
        ok(tapprox($comp,$in2));
        }
        {
        my $comp = ($form->[2] || rgb($in3) ? ($im3->dummy(0,3)>0)*255 : ($im3 > 0));
        $comp = $comp->ushort*$in3->max if $format eq 'SGI' && $in3->max > 0;
        ok(tapprox($comp,$in3));
        }

        if ($PDL::debug) {
          note $in1->px unless $format eq 'TIFF';
          note $in2->px;
          note $in3->px;
        }
    }
}

use Data::Dumper;
note "Dumping diagnostic PDL::IO::Pic converter data...\n";
note Dumper(\%PDL::IO::Pic::converter);
