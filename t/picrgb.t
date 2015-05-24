use PDL;
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

sub depends_on {
  note "ushort is ok with $_[0]\n"
	if $PDL::IO::Pic::converter{$_[0]}->{ushortok};
  return 1 if $PDL::IO::Pic::converter{$_[0]}->{ushortok};
  return 256;
}

sub mmax { return $_[0] > $_[1] ? $_[0] : $_[1] }

my $warned = 0;
sub tifftest {
  my ($form) = @_;
  return 0 unless $form eq 'TIFF';
  warn "WARNING: you are probably using buggy tiff converters.
     Check IO/Pnm/converters for patched source files\n" unless $warned;
  $warned = 1;
  return 1;
}

$PDL::debug = 0;
$PDL::IO::Pic::debug = 0;
my $iform = 'PNMRAW'; # change to PNMASCII to use ASCII PNM intermediate
                      # output format

#              [FORMAT, extension, ushort-divisor,
#               only RGB/no RGB/any (1/-1/0), mxdiff]
#  no test of PCX format because seems to be severely brain damaged
my %formats = ('PNM'  => ['pnm',1,0,0.01],
	       'GIF'  => ['gif',256,0,1.01],
	       'TIFF' => ['tif',1,0,0.01],
	       'RAST' => ['rast',256,0,0.01],
#	       'SGI'  => ['rgb',1,1,0.01],
 	       'PNG'  => ['png',1,1,0.01],
	      );

# only test PNM format
# netpbm has too many bugs on various platforms
my @allowed = ();
## for ('PNM') { push @allowed, $_
for (keys %formats) {
   if (PDL->rpiccan($_) && PDL->wpiccan($_) && defined $formats{$_}) {
      push @allowed, $_;
   }
}

my $ntests = 2 * (@allowed);
if ($ntests < 1) {
  plan skip_all => "No tests";
}

plan tests => $ntests;

note "Testable formats on this platform:\n".join(',',@allowed)."\n";

my $im1 = ushort pdl [[[0,0,0],[256,65535,256],[0,0,0]],
		     [[256,256,256],[256,256,256],[256,256,256]],
		     [[2560,65535,2560],[256,2560,2560],[65535,65534,65535]]];
my $im2 = byte ($im1/256);

if ($PDL::debug){
   note $im1;
   note $im2;
}

my $usherr = 0;
my $tmpdir = tempdir( CLEANUP => 1 );
sub tmpfile { File::Spec->catfile($tmpdir, $_[0]); }
foreach my $form (sort @allowed) {
    note "** testing $form format **\n";

    my $arr = $formats{$form};
    my $tushort = tmpfile("tushort.$arr->[0]");
    my $tbyte = tmpfile("tbyte.$arr->[0]");
    eval {
        $im1->wpic($tushort,{IFORM => $iform});
    };
    SKIP: {
        my $additional = '';
        if ($@ =~ /maxval is too large/) {
            $additional = ' (recompile pbmplus with PGM_BIGGRAYS!)';
        }
        skip "Error: '$@'$additional", 2 if $@;
        $im2->wpic($tbyte,{IFORM => $iform});

        my $in1 = rpic_unlink($tushort) unless $usherr;
        my $in2 = rpic_unlink($tbyte);

        my $comp = $im1 / PDL::ushort(mmax(depends_on($form),$arr->[1]));
        note "Comparison arr: $comp" if $PDL::debug;
        ok($usherr || tapprox($comp,$in1,$arr->[3]) || tifftest($form));
        ok(tapprox($im2,$in2) || tifftest($form));

        if ($PDL::debug) {
          note $in1->px;
          note $in2->px;
        }
    }
}

use Data::Dumper;
note "PDL::IO::Pic converter data:\n";
note Dumper(\%PDL::IO::Pic::converter);
