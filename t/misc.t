
# Test routine for PDL::IO module

use PDL::LiteF;
use PDL::IO::Misc;

use PDL::Core ':Internal'; # For howbig()

print "1..39\n";

kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

$count=1;
sub ok {
        my $no = $count++ ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = max($c);
        $d < 0.0001;
}

require File::Spec;
$fs = 'File::Spec';
sub cdir { return $fs->catdir(@_)}
sub cfile { return $fs->catfile(@_)}
$td = $^O =~ /MSWin/ ? 'TEMP' : 'tmp';
$tempd = defined $ENV{TEMP} ? $ENV{TEMP} :
            defined $ENV{TMP} ? $ENV{TMP} :
                           cdir($fs->rootdir,$td);
$file = cfile $tempd, "iotest$$";

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

($a,$b) = rcols $file,0,1; $a = long($a); $b=long($b);

ok( (sum($a)==15 && max($b)==66 && $b->getdim(0)==5) );

($a,$b) = rcols $file, "/FOO/",0,1; $a = long($a); $b=long($b);

ok( (sum($a)==6 && max($b)==33 && $b->getdim(0)==2) );

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

ok( (sum($a)==15 && max($b)==66 && $b->getdim(0)==5) );

################ Test rfits/wfits ########################

$t = long xvals(zeroes(11,20))-5;

# note: keywords are converted to uppercase
%hdr = ('Foo'=>'foo', 'Bar'=>42, 'NUM'=>'0123',NUMSTR=>['0123']);
$t->sethdr(\%hdr);

wfits($t, $file);
print "#file is $file\n";
$t2 = rfits $file;

ok( (sum($t->slice('0:4,:')) == -sum($t2->slice('5:-1,:')) ));

$h = $t2->gethdr;
ok($$h{'FOO'} eq "foo" && $$h{'BAR'} == 42);
print "# test 5: foo='".$$h{'FOO'}."'; bar=='".$$h{'BAR'}."'\n";

ok($$h{'NUM'}+1 == 124 && $$h{'NUMSTR'} eq '0123');

unlink $file;

########### Explicit test of byte swapping #################

$a = short(3); $b = long(3); # $c=long([3,3]);
bswap2($a); bswap4($b);
ok(sum($a)==768 && sum($b)==50331648);

########### Check if r/wfits bugs are fixed ################

# test 7 - 16
{
    local $| = 1;
    my $a1 =  [1,2];
    my $a2 = [[1,2],[1,2]];
    my $p;
    my $q;
    for my $cref ( \(&byte, &short, &long, &float, &double) ) {
        for my $a ($a1,$a2) {
            $p = &$cref($a);
            $p->wfits('x.fits');
            $q = PDL->rfits('x.fits');
            if ( ${$p->get_dataref} eq ${$q->get_dataref} ) {
                ok(1);
            } else {
	        { local $, = " ";
		  print "\tnelem=",$p->nelem,"datatype=",$p->get_datatype,"\n";
                  print "\tp:", unpack("c" x ($p->nelem*howbig($p->get_datatype)), ${$p->get_dataref}),"\n";
                  print "\tq:", unpack("c" x ($q->nelem*howbig($q->get_datatype)), ${$q->get_dataref}),"\n";
		}
                ok(0);
            }
        }
    }
    unlink 'x.fits';
}

# test 17 - 26
{
    local $| = 1;
    my $p1= pdl  [1,2];
    my $p2= pdl [[1,2],[1,2]];
    my $q;
    my @s;
    for my $i (8,16,32,-32,-64) {
    for my $p ($p2, $p1) {
        $p->wfits('x.fits',$i);
        $q = PDL->rfits('x.fits');
        @s = $q->stats;
        if ($s[0] == 1.5 and $s[1] == 0.5) {
           ok(1);
        } else {
           print "\tBITPIX=$i, nelem=", $p->nelem, "\n";
           print "\tbug: $s[0] == 1.5 and $s[1] == 0.5\n";
	   { local $, = " ";
	     print "\tp:", unpack("c8" x         $p->nelem,  ${$p->get_dataref}),"\n";
	     print "\tq:", unpack("c" x abs($i/8*$q->nelem), ${$q->get_dataref}),"\n";
           }
           ok(0);
        }
    }
    }
    unlink 'x.fits';
};

############# Test rasc  #############

# test 27 - 28
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
ok( abs($a->sum - 5.13147) < .01 );
 
$b = zeroes(float,20,2);
$b->rasc($file);
ok( abs($b->sum - 5.13147) < .01 );

eval '$b->rasc("file_that_does_not_exist")';
ok( $@ =~ /^Can't open/ );

unlink $file; # clean up

#######################################################
# Tests of rcols() options: 29 to 36
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
ok(  $a->nelem==4 && sum($a)==6 && sum($b)==20 );  # test: 29

($a,$b) = rcols $file,0,1, { INCLUDE => '/^-/' };
ok( $a->nelem==1 && $a->at(0)==-5 && $b->at(0)==6 );  # test: 30

($a,$b) = rcols $file,0,1, { LINES => '-2:0' };
ok( $a->nelem==3 && tapprox($a,pdl(-5,3,1)) && tapprox($b,pdl(6,4,2)) ); # test: 31

use PDL::Types;
($a,$b) = rcols $file, { DEFTYPE => long };
ok( $a->nelem==4 && $a->get_datatype==$PDL_L && $b->get_datatype==$PDL_L ); # test: 32

($a,$b) = rcols $file, { TYPES => [ ushort ] };
ok( $a->nelem==4 && $a->get_datatype==$PDL_US && $b->get_datatype==$PDL_D ); # test: 33

ok( UNIVERSAL::isa($PDL::IO::Misc::deftype,"PDL::Type") ); # test: 34
ok( $PDL::IO::Misc::deftype->[0] == double->[0] ); # test: 35

$PDL::IO::Misc::deftype = short;
($a,$b) = rcols $file;
ok( $a->get_datatype == short->[0] ); # test: 36

unlink $file;

eval { wcols $a, $b };
ok(!$@);

1;
