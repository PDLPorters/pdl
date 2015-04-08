
# Test routine for PDL::IO::Misc module

use strict; 

use PDL::LiteF;
use PDL::IO::Misc;

use PDL::Core ':Internal'; # For howbig()
use PDL::Config;

use File::Temp qw( tempfile tempdir );

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use Test::More tests => 19;

sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = max($c);
        $d < 0.0001;
}

my $tempd = tempdir( CLEANUP => 1 ) or die "Couldn't get tempdir\n";
my ($fileh,$file) = tempfile( DIR => $tempd );

############# Test rcols with colsep and missing fields ###################

print $fileh <<EOD;
1,6,11
2,7,
3,8,13
4,,14
5,10,15
EOD
close($fileh);

{
   local $PDL::undefval = -1;
   $a = rcols $file, [], { colsep=>',' };
}

is( (sum($a<0)==2 && $a->getdim(0)==5 && $a->getdim(1)==3), 1, "rcols with undefval and missing cols" );
unlink $file;

############# Test rcols with filename and pattern #############

($fileh,$file) = tempfile( DIR => $tempd );
print $fileh <<EOD;
1 2
2 33 FOO
3 7
4 9  FOO
5 66
EOD
close($fileh);

($a,$b) = rcols $file,0,1;
$a = long($a); $b=long($b);

is( (sum($a)==15 && max($b)==66 && $b->getdim(0)==5), 1, "rcols with filename" );

($a,$b) = rcols $file, "/FOO/",0,1;
$a = long($a);
$b=long($b);

is( (sum($a)==6 && max($b)==33 && $b->getdim(0)==2), 1, "rcols with filename + pattern" );

############# Test rcols with file handle with nothing left #############

open my $fh, '<', $file;
# Pull in everything:
my @slurp = <$fh>;
# Now apply rcols:
$@ = '';
$a = eval { rcols $fh };
is($@, '', 'rcols does not die on a used file handle');
close $fh;

############### Test rgrep with FILEHANDLE #####################

($fileh,$file) = tempfile( DIR => $tempd );
print $fileh <<EOD;
foo"1" -2-
foo"2"  Test -33-
foo"3" jvjtvbjktrbv -7-
foo"4" -9-
fjrhfiurhe foo"5" jjjj -66-
EOD
close($fileh);

open(OUT, $file) || die "Can not open $file for reading\n";
($a,$b) = rgrep {/foo"(.*)".*-(.*)-/} *OUT;
$a = long($a); $b=long($b);
close(OUT);

is( (sum($a)==15 && max($b)==66 && $b->getdim(0)==5), 1, "rgrep" );

########### Explicit test of byte swapping #################

$a = short(3); $b = long(3); # $c=long([3,3]);
bswap2($a); bswap4($b);
is(sum($a)==768 && sum($b)==50331648,1,"bswap2");

############# Test rasc  #############

($fileh,$file) = tempfile( DIR => $tempd );
print $fileh <<EOD;
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
close($fileh);

$a = PDL->null;
$a->rasc($file,20);
is( abs($a->sum - 5.13147) < .01, 1, "rasc on null piddle" );
 
$b = zeroes(float,20,2);
$b->rasc($file);
is( abs($b->sum - 5.13147) < .01, 1, "rasc on existing piddle" );

eval '$b->rasc("file_that_does_not_exist")';
like( $@, qr/Can't open/, "rasc on non-existant file" );

unlink $file; # clean up

#######################################################
# Tests of rcols() options
#   EXCLUDE/INCLUDE/LINES/DEFTYPE/TYPES

($fileh,$file) = tempfile( DIR => $tempd );
print $fileh <<EOD;
1 2
# comment line
3 4
-5 6
7 8
EOD
close($fileh);

($a,$b) = rcols $file,0,1;
is( $a->nelem==4 && sum($a)==6 && sum($b)==20, 1,
    "rcols: default" );

($a,$b) = rcols \*DATA,0,1;
is( $a->nelem==4 && sum($a)==6 && sum($b)==20, 1,
    "rcols: pipe" );

($a,$b) = rcols $file,0,1, { INCLUDE => '/^-/' };
is( $a->nelem==1 && $a->at(0)==-5 && $b->at(0)==6, 1,
    "rcols: include pattern" );

($a,$b) = rcols $file,0,1, { LINES => '-2:0' };
is( $a->nelem==3 && tapprox($a,pdl(-5,3,1)) && tapprox($b,pdl(6,4,2)), 1,
    "rcols: lines option" );

use PDL::Types;
($a,$b) = rcols $file, { DEFTYPE => long };
is( $a->nelem==4 && $a->get_datatype==$PDL_L && $b->get_datatype==$PDL_L, 1,
    "rcols: deftype option" );

($a,$b) = rcols $file, { TYPES => [ ushort ] };
is( $a->nelem==4 && $a->get_datatype==$PDL_US && $b->get_datatype==$PDL_D, 1,
    "rcols: types option" );

is( UNIVERSAL::isa($PDL::IO::Misc::deftype,"PDL::Type"), 1,
    "PDL::IO::Misc::deftype is a PDL::Type object" );
is( $PDL::IO::Misc::deftype->[0], double->[0],
    "PDL::IO::Misc::deftype check" );

$PDL::IO::Misc::deftype = short;
($a,$b) = rcols $file;
is( $a->get_datatype, short->[0], "rcols: can read in as 'short'" );

unlink $file;

($fileh,$file) = tempfile( DIR => $tempd );
eval { wcols $a, $b, $fileh };
is(!$@,1, "wcols" );
unlink $fileh;

1;

__DATA__
1 2
# comment line
3 4
-5 6
7 8
