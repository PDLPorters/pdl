
# Test routine for PDL::IO::Misc module

use strict; 

use PDL::LiteF;
use PDL::IO::Misc;

use PDL::Core ':Internal'; # For howbig()
use PDL::Config;

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use Test;
BEGIN { plan tests => 17; }

sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = max($c);
        $d < 0.0001;
}

require File::Spec;
my $fs = 'File::Spec';
sub cdir { return $fs->catdir(@_)}
sub cfile { return $fs->catfile(@_)}

my $tempd = $PDL::Config{TEMPDIR} or
  die "TEMPDIR not found in %PDL::Config";
my $file = cfile $tempd, "iotest$$";

############# Test rcols with filename and pattern #############

open(OUT, ">$file") || die "Can not open $file for writing\n";
print OUT <<EOD;
1 2
2 33 FOO
3 7
4 9  FOO
5 66
EOD
close(OUT);

($a,$b) = rcols $file,0,1;
$a = long($a); $b=long($b);

ok( (sum($a)==15 && max($b)==66 && $b->getdim(0)==5), 1, "rcols with filename" );

($a,$b) = rcols $file, "/FOO/",0,1;
$a = long($a);
$b=long($b);

ok( (sum($a)==6 && max($b)==33 && $b->getdim(0)==2), 1, "rcols with filename + pattern" );

############### Test rgrep with FILEHANDLE #####################

open(OUT, ">$file") || die "Can not open $file for writing\n";
print OUT <<EOD;
foo"1" -2-
foo"2"  Test -33-
foo"3" jvjtvbjktrbv -7-
foo"4" -9-
fjrhfiurhe foo"5" jjjj -66-
EOD
close(OUT);

open(OUT, $file) || die "Can not open $file for reading\n";
($a,$b) = rgrep {/foo"(.*)".*-(.*)-/} *OUT;
$a = long($a); $b=long($b);
close(OUT);

ok( (sum($a)==15 && max($b)==66 && $b->getdim(0)==5), 1, "rgrep" );

########### Explicit test of byte swapping #################

$a = short(3); $b = long(3); # $c=long([3,3]);
bswap2($a); bswap4($b);
ok(sum($a)==768 && sum($b)==50331648,1,"bswap2");

############# Test rasc  #############

open(OUT, ">$file") || die "Can not open $file for writing\n";
print OUT <<EOD;
0.231862613
0.20324005
0.067813045
0.040103501
0.438047631
0.283293628
0.375427346
0.195821617
0.189897617
0.035941205
0.339051483
0.096540854
0.25047197
0.579782013
0.236164184
0.221568561
0.009776015
0.290377604
0.785569601
0.260724391

EOD
close(OUT);

$a = PDL->null;
$a->rasc($file,20);
ok( abs($a->sum - 5.13147) < .01, 1, "rasc on null piddle" );
 
$b = zeroes(float,20,2);
$b->rasc($file);
ok( abs($b->sum - 5.13147) < .01, 1, "rasc on existing piddle" );

eval '$b->rasc("file_that_does_not_exist")';
ok( $@, qr/Can't open/, "rasc on non-existant file" );

unlink $file; # clean up

#######################################################
# Tests of rcols() options
#   EXCLUDE/INCLUDE/LINES/DEFTYPE/TYPES

open(OUT, ">$file") || die "Can not open $file for writing\n";
print OUT <<EOD;
1 2
# comment line
3 4
-5 6
7 8
EOD
close(OUT);

($a,$b) = rcols $file,0,1;
ok( $a->nelem==4 && sum($a)==6 && sum($b)==20, 1,
    "rcols: default" );

($a,$b) = rcols \*DATA,0,1;
ok( $a->nelem==4 && sum($a)==6 && sum($b)==20, 1,
    "rcols: pipe" );

($a,$b) = rcols $file,0,1, { INCLUDE => '/^-/' };
ok( $a->nelem==1 && $a->at(0)==-5 && $b->at(0)==6, 1,
    "rcols: include pattern" );

($a,$b) = rcols $file,0,1, { LINES => '-2:0' };
ok( $a->nelem==3 && tapprox($a,pdl(-5,3,1)) && tapprox($b,pdl(6,4,2)), 1,
    "rcols: lines option" );

use PDL::Types;
($a,$b) = rcols $file, { DEFTYPE => long };
ok( $a->nelem==4 && $a->get_datatype==$PDL_L && $b->get_datatype==$PDL_L, 1,
    "rcols: deftype option" );

($a,$b) = rcols $file, { TYPES => [ ushort ] };
ok( $a->nelem==4 && $a->get_datatype==$PDL_US && $b->get_datatype==$PDL_D, 1,
    "rcols: types option" );

ok( UNIVERSAL::isa($PDL::IO::Misc::deftype,"PDL::Type"), 1,
    "PDL::IO::Misc::deftype is a PDL::Type object" );
ok( $PDL::IO::Misc::deftype->[0], double->[0],
    "PDL::IO::Misc::deftype check" );

$PDL::IO::Misc::deftype = short;
($a,$b) = rcols $file;
ok( $a->get_datatype, short->[0], "rcols: can read in as 'short'" );

unlink $file;

eval { wcols $a, $b };
ok(!$@,1, "wcols" );

1;

__DATA__
1 2
# comment line
3 4
-5 6
7 8
