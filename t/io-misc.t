use strict; 
use warnings;
use PDL::LiteF;
use PDL::IO::Misc;
use File::Temp qw( tempfile tempdir );
use Test::More;
use Test::PDL;

my $tempd = tempdir( CLEANUP => 1 ) or die "Couldn't get tempdir\n";
my ($fileh,$file) = tempfile( DIR => $tempd );

for my $type (PDL::Types::types()) {
   ok $type->bswap, "$type has bswap";
}

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
is_pdl $x, pdl('1 2 3 4 5; 6 7 8 -1 10; 11 -1 13 14 15'),
  "rcols with undefval and missing cols";
unlink($file) || warn "Could not unlink $file: $!";

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
my $y;
($x,$y) = rcols $file,0,1;
is_pdl $x, pdl('1 2 3 4 5'), "rcols with filename";
is_pdl $y, pdl('2 33 7 9 66'), "rcols with filename";

($x,$y) = rcols $file, "/FOO/",0,1;
is_pdl $x, pdl('2 4'), "rcols with filename + pattern";
is_pdl $y, pdl('33 9'), "rcols with filename + pattern";

############# Test rcols with file handle with nothing left #############

open my $fh, '<', $file;
# Pull in everything:
my @slurp = <$fh>;
# Now apply rcols:
$x = eval { rcols $fh };
is $@, '', 'rcols does not die on a used file handle';
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

open OUT, $file or die "Can not open $file for reading\n";
($x,$y) = rgrep {/foo"(.*)".*-(.*)-/} *OUT;
close OUT;
is_pdl $x, pdl('1 2 3 4 5'), "rgrep";
is_pdl $y, pdl('2 33 7 9 66'), "rgrep";

########### Explicit test of byte swapping #################

$x = short(3); $y = long(3); # $c=long([3,3]);
bswap2($x); bswap4($y);
is_pdl $x, short(768), "bswap2";
is_pdl $y, long(50331648), "bswap4";
$x = short(3);
$x->type->bswap->($x);
is_pdl $x, short(768), "bswap Type method";
eval {$x->bswap8};
like $@, qr/Tried to/, 'bswap of greater than ndarray type-size gives error';

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
is_pdl $x, pdl('0.231862613 0.20324005 0.067813045 0.040103501 0.438047631 0.283293628 0.375427346 0.195821617 0.189897617 0.035941205 0.339051483 0.096540854 0.25047197 0.579782013 0.236164184 0.221568561 0.009776015 0.290377604 0.785569601 0.260724391'), "rasc on null ndarray";
 
$y = zeroes(float,20,2);
$y->rasc($file);
is_pdl $y, float('0.231862613 0.20324005 0.067813045 0.040103501 0.438047631 0.283293628 0.375427346 0.195821617 0.189897617 0.035941205 0.339051483 0.096540854 0.25047197 0.579782013 0.236164184 0.221568561 0.009776015 0.290377604 0.785569601 0.260724391; 0 0 0 0 0 0 0 0 0 0'), "rasc on existing ndarray";

eval { $y->rasc("file_that_does_not_exist") };
like $@, qr/Can't open/, "rasc on non-existant file";

unlink($file) || warn "Could not unlink $file: $!"; # clean up

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

($x,$y) = rcols $file,0,1;
is_pdl $x, pdl(1,3,-5,7), "rcols: default";
is_pdl $y, pdl(2,4,6,8), "rcols: default";

($x,$y) = rcols \*DATA,0,1;
is_pdl $x, pdl(1,3,-5,7), "rcols: fh";
is_pdl $y, pdl(2,4,6,8), "rcols: fh";

($x,$y) = rcols $file,0,1, { INCLUDE => '/^-/' };
is_pdl $x, pdl([-5]), "rcols: include pattern";
is_pdl $y, pdl([6]), "rcols: include pattern";

($x,$y) = rcols $file,0,1, { LINES => '-2:0' };
is_pdl $x, pdl(-5,3,1), "rcols: lines option";
is_pdl $y, pdl(6,4,2), "rcols: lines option";

use PDL::Types;
($x,$y) = rcols $file, { DEFTYPE => long };
is_pdl $x, long(1,3,-5,7), "rcols: deftype option";
is_pdl $y, long(2,4,6,8), "rcols: deftype option";

($x,$y) = rcols $file, { TYPES => [ ushort ] };
is_pdl $x, ushort(1,3,-5,7), "rcols: types option";
is_pdl $y, double(2,4,6,8), "rcols: types option";

# capturing problem in PDL::CCS
my $ix = PDL->rcols($file, [0,1], { TYPES => [ indx ], IGNORE => qr/^\s*#/ });
is_pdl $ix, indx('1 3 -5 7; 2 4 6 8'), "rcols: types option";

isa_ok $PDL::IO::Misc::deftype, "PDL::Type", "PDL::IO::Misc::deftype";
is $PDL::IO::Misc::deftype, 'double', "PDL::IO::Misc::deftype check";

$PDL::IO::Misc::deftype = short;
($x,$y) = rcols $file;
is( $x->get_datatype, short->enum, "rcols: can read in as 'short'" );

unlink($file) or warn "Could not unlink $file: $!";

($fileh,$file) = tempfile( DIR => $tempd );
eval { wcols $x, $y, $fileh };
is $@, '', "wcols";
close $fileh; unlink($file) or warn "Could not unlink $file: $!";

($fileh,$file) = tempfile( DIR => $tempd );
eval { wcols $x, $y, $fileh, {FORMAT=>"%0.3d %0.3d"}};
is $@, '', "wcols FORMAT option";
close $fileh; unlink($file) or warn "Could not unlink $file: $!";

($fileh,$file) = tempfile( DIR => $tempd );
eval { wcols "%d %d", $x, $y, $fileh;};
is $@, '', "wcols format_string";
close $fileh; unlink($file) or warn "Could not unlink $file: $!";

($fileh,$file) = tempfile( DIR => $tempd );
eval { wcols "arg %d %d", $x, $y, $fileh, {FORMAT=>"option %d %d"};};
is $@, '', "wcols format_string override";

open $fileh,"<",$file or warn "Can't open $file: $!";
readline *$fileh; # dump first line
like readline($fileh), qr/^arg/, "wcols format_string obeyed";
close $fileh; unlink($file) or warn "Could not unlink $file: $!";

subtest 'rcols options, swcols, bswap round-trips, and rgrep' => sub {
    local $PDL::IO::Misc::deftype = double;
    my $cx = pdl(1, 2, 3, 4, 5);
    my $cy = pdl(10.5, 20.5, 30.5, 40.5, 50.5);
    my $cfile = "$tempd/cols.txt";
    wcols($cx, $cy, $cfile, { HEADER => '# x y', COLSEP => ',' });

    my ($rx, $ry) = rcols($cfile, { COLSEP => ',', EXCLUDE => '/^#/' });
    is_pdl $rx, $cx, 'rcols COLSEP/EXCLUDE col 0';
    is_pdl $ry, $cy, 'rcols COLSEP/EXCLUDE col 1';

    my ($lx, $ly) = rcols($cfile, { COLSEP => ',', EXCLUDE => '/^#/', LINES => '1:3' });
    is_pdl $lx, pdl(1, 2, 3), 'rcols LINES=>"1:3" values';

    my ($fx, $fy) = rcols($cfile, { COLSEP => ',', EXCLUDE => '/^#/', DEFTYPE => float });
    is $fx->type, float, 'rcols DEFTYPE=>float';

    my $a = pdl(1, 2, 3);
    my $b = pdl(4.5, 5.5, 6.5);
    my $lines = swcols('%d %.1f', $a, $b);
    is ref($lines), 'ARRAY', 'swcols returns arrayref in scalar context';
    like $lines->[0], qr/1 4\.5/, 'swcols formats row 0';
    my @list = swcols('%s:%d', [ qw(a b c) ], $a);
    is $list[1], 'b:2', 'swcols formats arrayref + pdl';
    my @nofmt = swcols($a, $b);
    ok scalar(@nofmt) > 0, 'swcols works without explicit format';

    for my $pair ([ short(0x0102, 0x0304), \&bswap2 ], [ long(0x01020304, 0x05060708), \&bswap4 ], [ double(1.5, -2.25), \&bswap8 ]) {
      my ($orig, $fn) = @$pair;
      my $copy = $orig->copy;
      $fn->($copy);
      $fn->($copy);
      is_pdl $copy, $orig, { atol => 0, test_name => 'double bswap is identity' };
    }

    my $gfile = "$tempd/grep.txt";
    open my $gfh, '>', $gfile or die "open $gfile: $!";
    print {$gfh} "noise\nval=10 count=2\nval=20 count=4\nother\n";
    close $gfh;
    my ($v, $c) = rgrep { /val=(\d+)\s+count=(\d+)/ } $gfile;
    is_pdl $v, pdl(10, 20), { atol => 0, test_name => 'rgrep capture 1' };
    is_pdl $c, pdl(2, 4), { atol => 0, test_name => 'rgrep capture 2' };
};

done_testing;

__DATA__
1 2
# comment line
3 4
-5 6
7 8
