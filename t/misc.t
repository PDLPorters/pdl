
# Test routine for PDL::IO::Misc module

use strict; 

use PDL::LiteF;
use PDL::IO::Misc;

use File::Temp qw( tempfile tempdir );

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use Test::More tests => 23;

sub tapprox {
        my($x,$b) = @_;
        my $c = abs($x-$b);
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

my $x = do {
   local $PDL::undefval = -1;
   rcols $file, [], { colsep=>',' };
};

is( (sum($x<0)==2 && $x->getdim(0)==5 && $x->getdim(1)==3), 1, "rcols with undefval and missing cols" );
unlink $file || warn "Could not unlink $file: $!";

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

($x,$b) = rcols $file,0,1;
$x = long($x); $b=long($b);

is( (sum($x)==15 && max($b)==66 && $b->getdim(0)==5), 1, "rcols with filename" );

($x,$b) = rcols $file, "/FOO/",0,1;
$x = long($x);
$b=long($b);

is( (sum($x)==6 && max($b)==33 && $b->getdim(0)==2), 1, "rcols with filename + pattern" );

############# Test rcols with file handle with nothing left #############

open my $fh, '<', $file;
# Pull in everything:
my @slurp = <$fh>;
# Now apply rcols:
$@ = '';
$x = eval { rcols $fh };
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
($x,$b) = rgrep {/foo"(.*)".*-(.*)-/} *OUT;
$x = long($x); $b=long($b);
close(OUT);

is( (sum($x)==15 && max($b)==66 && $b->getdim(0)==5), 1, "rgrep" );

########### Explicit test of byte swapping #################

$x = short(3); $b = long(3); # $c=long([3,3]);
bswap2($x); bswap4($b);
is(sum($x)==768 && sum($b)==50331648,1,"bswap2");

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

$x = PDL->null;
$x->rasc($file,20);
is( abs($x->sum - 5.13147) < .01, 1, "rasc on null piddle" );
 
$b = zeroes(float,20,2);
$b->rasc($file);
is( abs($b->sum - 5.13147) < .01, 1, "rasc on existing piddle" );

eval '$b->rasc("file_that_does_not_exist")';
like( $@, qr/Can't open/, "rasc on non-existant file" );

unlink $file || warn "Could not unlink $file: $!"; # clean up

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

($x,$b) = rcols $file,0,1;
is( $x->nelem==4 && sum($x)==6 && sum($b)==20, 1,
    "rcols: default" );

($x,$b) = rcols \*DATA,0,1;
is( $x->nelem==4 && sum($x)==6 && sum($b)==20, 1,
    "rcols: pipe" );

($x,$b) = rcols $file,0,1, { INCLUDE => '/^-/' };
is( $x->nelem==1 && $x->at(0)==-5 && $b->at(0)==6, 1,
    "rcols: include pattern" );

($x,$b) = rcols $file,0,1, { LINES => '-2:0' };
is( $x->nelem==3 && tapprox($x,pdl(-5,3,1)) && tapprox($b,pdl(6,4,2)), 1,
    "rcols: lines option" );

use PDL::Types;
($x,$b) = rcols $file, { DEFTYPE => long };
is( $x->nelem==4 && $x->get_datatype==$PDL_L && $b->get_datatype==$PDL_L, 1,
    "rcols: deftype option" );

($x,$b) = rcols $file, { TYPES => [ ushort ] };
is( $x->nelem==4 && $x->get_datatype==$PDL_US && $b->get_datatype==$PDL_D, 1,
    "rcols: types option" );

is( UNIVERSAL::isa($PDL::IO::Misc::deftype,"PDL::Type"), 1,
    "PDL::IO::Misc::deftype is a PDL::Type object" );
is( $PDL::IO::Misc::deftype->[0], double->[0],
    "PDL::IO::Misc::deftype check" );

$PDL::IO::Misc::deftype = short;
($x,$b) = rcols $file;
is( $x->get_datatype, short->[0], "rcols: can read in as 'short'" );

unlink $file || warn "Could not unlink $file: $!";

($fileh,$file) = tempfile( DIR => $tempd );
eval { wcols $x, $b, $fileh };
is(!$@,1, "wcols" );
unlink $file || warn "Could not unlink $file: $!";

($fileh,$file) = tempfile( DIR => $tempd );
eval { wcols $x, $b, $fileh, {FORMAT=>"%0.3d %0.3d"}};
is(!$@,1, "wcols FORMAT option");
unlink $file || warn "Could not unlink $file: $!";

($fileh,$file) = tempfile( DIR => $tempd );
eval { wcols "%d %d", $x, $b, $fileh;};
is(!$@,1, "wcols format_string");
unlink $file || warn "Could not unlink $file: $!";

($fileh,$file) = tempfile( DIR => $tempd );
eval { wcols "arg %d %d", $x, $b, $fileh, {FORMAT=>"option %d %d"};};
is(!$@,1, "wcols format_string override");

open($fileh,"<",$file) or warn "Can't open $file: $!";
readline(*$fileh); # dump first line
like(readline($fileh),qr/^arg/, "wcols format_string obeyed");
unlink $file || warn "Could not unlink $file: $!";

1;

__DATA__
1 2
# comment line
3 4
-5 6
7 8
