
# Test routine for PDL::IO::FITS module

use strict;

use PDL::LiteF;

use PDL::Core ':Internal'; # For howbig()
use PDL::Config;

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use Test::More tests => 53;

BEGIN {
      use_ok( "PDL::IO::FITS" );
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

is( sum($t->slice('0:4,:')), -sum($t2->slice('5:-1,:')),
    "r/wfits: slice check" );

my $h = $t2->gethdr;
ok( $$h{'FOO'} eq "foo" && $$h{'BAR'} == 42,
    "header check on FOO/BAR" );

ok( $$h{'NUM'}+1 == 124 && $$h{'NUMSTR'} eq '0123',
    "header check on NUM/NUMSTR" );

unlink $file;

########### Rudimentary table tests ################

sub compare_piddles ($$$) {
    my $orig  = shift;
    my $new   = shift;
    my $label = shift;

    is( $new->type->symbol, $orig->type->symbol, "$label has the correct type" );
    is( $new->nelem, $orig->nelem, "  and the right number of elements" );
    is( $new->ndims, $orig->ndims, "  and the right number of dimensions" );

    my $flag;
    if ( $orig->type() < float() ) {
	$flag = all( $new == $orig );
    } else {
	$flag = all( approx( $orig, $new ) );
    }
    ok( $flag, "  and all the values agree" );
}

my $a = long( 1, 4, 9, 32 );
my $b = double( 2.3, 4.3, -999.0, 42 );
my $table = { COLA => $a, COLB => $b };
wfits $table, $file;

my $table2 = rfits $file;
unlink $file;

ok( defined $table2, "Read of table returned something" );
is( ref($table2), "HASH", "which is a hash reference" );
is( $$table2{tbl}, "binary", "and appears to be a binary TABLE" );

ok( exists $$table2{COLA} && exists $$table2{COLB}, "columns COLA and COLB exist" );
is( $$table2{hdr}{TTYPE1}, "COLA", "column #1 is COLA" );
is( $$table2{hdr}{TFORM1}, "1J", "  stored as 1J" );
is( $$table2{hdr}{TTYPE2}, "COLB", "column #2 is COLB" );
is( $$table2{hdr}{TFORM2}, "1D", "  stored as 1D" );

compare_piddles $a, $$table2{COLA}, "COLA";
compare_piddles $b, $$table2{COLB}, "COLB";

$table = { BAR => $a, FOO => $b,
	   hdr => { TTYPE1 => 'FOO', TTYPE2 => 'BAR' } };
$table2 = {};

wfits $table, $file;
$table2 = rfits $file;

ok( defined $table2 && ref($table2) eq "HASH" && $$table2{tbl} eq "binary",
    "Read in the second binary table" );
is( $$table2{hdr}{TTYPE1}, "FOO", "column #1 is FOO" );
is( $$table2{hdr}{TFORM1}, "1D", "  stored as 1D" );
is( $$table2{hdr}{TTYPE2}, "BAR", "column #2 is BAR" );
is( $$table2{hdr}{TFORM2}, "1J", "  stored as 1J" );

compare_piddles $a, $$table2{BAR}, "BAR";
compare_piddles $b, $$table2{FOO}, "FOO";

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
	    ok($flag,"hash reference - type check: " . &$cref );
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
	print "s=@s\n";
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
	ok($flag,"piddle - bitpix=$i" );
    }
    }
    unlink 'x.fits';
};

1;
