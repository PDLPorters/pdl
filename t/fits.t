
# Test routine for PDL::IO::FITS module

use strict;

use PDL::LiteF;

use PDL::Core ':Internal'; # For howbig()
use PDL::Config;

##use PDL::Complex;  # currently not supported

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use Test::More tests => 83;

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

END {
  unlink $file if defined $file and -e $file;
}

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

# note:
#   the tests do not directly test the output file,
#   instead they write out a file, read it back in, and
#   compare to the data used to create the file.
#   So it is more of a "self consistent" test.
#
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

unless($PDL::Astro_FITS_Header) {
 # Astro::FITS::Header is not present, ignore table tests
 for(1..59){ok(1,"Test skipped (no binary table support without Astro::FITS::Header)");}
} else { # Astro::FITS::Header exists

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
	
	# try out more "exotic" data types
	
	$a = byte(12,45,23,0);
	$b = short(-99,100,0,32767);
	my $c = ushort(99,32768,65535,0);
	my $d = [ "A string", "b", "", "The last string" ];
	my $e = float(-999.0,0,0,12.3);
	##my $f = float(1,0,-1,2) + i * float( 0,1,2,-1 );
	$table = {
	       ACOL => $a, BCOL => $b, CCOL => $c, DCOL => $d, ECOL => $e,
	##	  FCOL => $f,
	};
	$table2 = {};
	
	wfits $table, $file;
	$table2 = rfits $file;
	#unlink $file;
	
	ok( defined $table2 && ref($table2) eq "HASH" && $$table2{tbl} eq "binary",
	    "Read in the third binary table" );
	my @elem = sort keys %$table2;
	##my @expected = sort( qw( ACOL BCOL CCOL DCOL ECOL FCOL hdr tbl ) );
	##is ( $#elem+1, 8, "hash contains 8 elements" );
	my @expected = sort( qw( ACOL BCOL CCOL DCOL ECOL hdr tbl ) );
	is ( $#elem+1, 7, "hash contains 7 elements" );
	ok( eq_array( \@elem, \@expected ), "hash contains expected keys" );
	
	# convert the string array so that each element has the same length
	# (and calculate the maximum length to use in the check below)
	#
	my $dlen = 0;
	foreach my $str ( @$d ) {
	  my $len = length($str);
	  $dlen = $len > $dlen ? $len : $dlen;
	}
	foreach my $str ( @$d ) {
	  $str .= ' ' x ($dlen-length($str));
	}
	
	# note that, for now, ushort data is written out as a long (Int4)
	# instead of being written out as an Int2 using TSCALE/TZERO
	#
	my $i = 1;
	foreach my $colinfo ( ( ["ACOL","1B",$a],
				["BCOL","1I",$b],
				["CCOL","1J",$c->long],
				["DCOL","${dlen}A",$d],
				["ECOL","1E",$e],
	##			["FCOL","1M",$f]
			      ) ) {
	  is( $$table2{hdr}{"TTYPE$i"}, $$colinfo[0], "column $i is $$colinfo[0]" );
	  is( $$table2{hdr}{"TFORM$i"}, $$colinfo[1], "  and is stored as $$colinfo[1]" );
	  my $col = $$table2{$$colinfo[0]};
	  if ( UNIVERSAL::isa($col,"PDL") ) {
	    compare_piddles $col, $$colinfo[2], $$colinfo[0];
	  } else {
	    # Need to somehow handle the arrays since the data read in from the
	    # file all have 15-character length strings (or whatever the length is)
	    #
	    ok( eq_array($col, $$colinfo[2]),
		"  $$colinfo[0] values agree (as an array reference)" );
	  }
	  $i++;
	}
}
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
