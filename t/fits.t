
# Test routine for PDL::IO::Fits module

use strict;

use PDL::LiteF;
use PDL::IO::FITS;

use PDL::Core ':Internal'; # For howbig()
use PDL::Config;

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use Test;
BEGIN { plan tests => 23; }

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

################ Test rfits/wfits ########################

my $t = long xvals(zeroes(11,20))-5;

# note: keywords are converted to uppercase
my %hdr = ('Foo'=>'foo', 'Bar'=>42, 'NUM'=>'0123',NUMSTR=>['0123']);
$t->sethdr(\%hdr);

wfits($t, $file);
print "#file is $file\n";
my $t2 = rfits $file;

ok( sum($t->slice('0:4,:')), -sum($t2->slice('5:-1,:')),
    "r/wfits: slice check" );

my $h = $t2->gethdr;
ok( $$h{'FOO'} eq "foo" && $$h{'BAR'} == 42, 1,
    "header check on FOO/BAR" );

ok( $$h{'NUM'}+1 == 124 && $$h{'NUMSTR'} eq '0123', 1,
    "header check on NUM/NUMSTR" );

unlink $file;

########### Check if r/wfits bugs are fixed ################

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
	    my $flag = 1;
            if ( ${$p->get_dataref} ne ${$q->get_dataref} ) {
	        $flag = 0;
	        { local $, = " ";
		  print "\tnelem=",$p->nelem,"datatype=",$p->get_datatype,"\n";
                  print "\tp:", unpack("c" x ($p->nelem*howbig($p->get_datatype)), ${$p->get_dataref}),"\n";
                  print "\tq:", unpack("c" x ($q->nelem*howbig($q->get_datatype)), ${$q->get_dataref}),"\n";
		}
            }
	    ok($flag,1,"hash reference - type check: " . &$cref );
        }
    }
    unlink 'x.fits';
}

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
	my $flag;
        if ($s[0] == 1.5 and $s[1] < 0.7072 and $s[1]>0.577) {
           $flag = 1;
        } else {
           $flag = 0;
           print "\tBITPIX=$i, nelem=", $p->nelem, "\n";
           print "\tbug: $s[0] == 1.5 and $s[1] == 0.5\n";
	   { local $, = " ";
	     print "\tp:", unpack("c8" x         $p->nelem,  ${$p->get_dataref}),"\n";
	     print "\tq:", unpack("c" x abs($i/8*$q->nelem), ${$q->get_dataref}),"\n";
           }
        }
	ok($flag,1,"piddle - bitpix=$i" );
    }
    }
    unlink 'x.fits';
};

1;
