use strict;
use warnings;
use File::Basename;
use PDL::LiteF;
use PDL::Core ':Internal'; # For howbig()
use Test::More;
use Test::Exception;
use PDL::IO::FITS;
require File::Spec;
require File::Temp;

my $fs = 'File::Spec';
sub cfile { return $fs->catfile(@_)}

my %tmp_opts = (TMPDIR => 1, UNLINK => 1);
my $file = File::Temp::tempfile(%tmp_opts);

################ Test rfits/wfits ########################

my $t = long xvals(zeroes(11,20))-5;

# note: keywords are converted to uppercase
my %hdr = ('Foo'=>'foo', 'Bar'=>42, 'NUM'=>'0123',NUMSTR=>['0123']);
$t->sethdr(\%hdr);

wfits($t, $file);
my $t2 = rfits $file;

is( sum($t->slice('0:4,:')), -sum($t2->slice('5:-1,:')),
    "r/wfits: slice check" );				#2

my $h = $t2->gethdr;
ok( $$h{'FOO'} eq "foo" && $$h{'BAR'} == 42,
    "header check on FOO/BAR" );			#3

ok( $$h{'NUM'}+1 == 124 && $$h{'NUMSTR'} eq '0123',
    "header check on NUM/NUMSTR" );			#4

unlink $file;

SKIP: {
   eval { require Astro::FITS::Header };

   skip "Astro::FITS::Header not installed", 79 if $@;

########### Rudimentary table tests ################

# note:
#   the tests do not directly test the output file,
#   instead they write out a file, read it back in, and
#   compare to the data used to create the file.
#   So it is more of a "self consistent" test.
#
sub compare_ndarrays ($$$) {
    my $orig  = shift;
    my $new   = shift;
    my $label = shift;

    TODO: {
       local $TODO = "Need to fix alias between PDL_IND and PDL_L or PDL_LL";

       is( $new->type->symbol, $orig->type->symbol, "$label has the correct type" );
    }
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

	my $x = long( 1, 4, 9, 32 );
	my $y = double( 2.3, 4.3, -999.0, 42 );
	my $table = { COLA => $x, COLB => $y };
	wfits $table, $file;

	my $table2 = rfits $file;
	unlink $file;

	ok( defined $table2, "Read of table returned something" );	#5
	is( ref($table2), "HASH", "which is a hash reference" );	#6
	is( $$table2{tbl}, "binary", "and appears to be a binary TABLE" );#7

	ok( exists $$table2{COLA} && exists $$table2{COLB}, "columns COLA and COLB exist" ); #8
	is( $$table2{hdr}{TTYPE1}, "COLA", "column #1 is COLA" );	  #9
	is( $$table2{hdr}{TFORM1}, "1J", "  stored as 1J" );		  #10
	is( $$table2{hdr}{TTYPE2}, "COLB", "column #2 is COLB" );	  #11
	is( $$table2{hdr}{TFORM2}, "1D", "  stored as 1D" );		  #12

	compare_ndarrays $x, $$table2{COLA}, "COLA";			#13-16
	compare_ndarrays $y, $$table2{COLB}, "COLB";			#17-20

	$table = { BAR => $x, FOO => $y,
		   hdr => { TTYPE1 => 'FOO', TTYPE2 => 'BAR' } };
	$table2 = {};

	wfits $table, $file;
	$table2 = rfits $file;

	ok( defined $table2 && ref($table2) eq "HASH" && $$table2{tbl} eq "binary",
	    "Read in the second binary table" );		       #21
	is( $$table2{hdr}{TTYPE1}, "FOO", "column #1 is FOO" );	       #22
	is( $$table2{hdr}{TFORM1}, "1D", "  stored as 1D" );	       #23
	is( $$table2{hdr}{TTYPE2}, "BAR", "column #2 is BAR" );	       #24
	is( $$table2{hdr}{TFORM2}, "1J", "  stored as 1J" );	       #25

	compare_ndarrays $x, $$table2{BAR}, "BAR";			#26-29
	compare_ndarrays $y, $$table2{FOO}, "FOO";			#30-33

	# try out more "exotic" data types

	$x = byte(12,45,23,0);
	$y = short(-99,100,0,32767);
	my $c = ushort(99,32768,65535,0);
	my $d = [ "A string", "b", "", "The last string" ];
	my $e = float(-999.0,0,0,12.3);
	##my $f = float(1,0,-1,2) + i * float( 0,1,2,-1 );
	$table = {
	       ACOL => $x, BCOL => $y, CCOL => $c, DCOL => $d, ECOL => $e,
	##	  FCOL => $f,
	};
	$table2 = {};

	wfits $table, $file;
	$table2 = rfits $file;

	ok( defined $table2 && ref($table2) eq "HASH" && $$table2{tbl} eq "binary",
	    "Read in the third binary table" );			       #34
	my @elem = sort keys %$table2;
	##my @expected = sort( qw( ACOL BCOL CCOL DCOL ECOL FCOL hdr tbl ) );
	##is ( $#elem+1, 8, "hash contains 8 elements" );
	my @expected = sort( qw( ACOL BCOL CCOL DCOL ECOL hdr tbl ) );
	is ( $#elem+1, 7, "hash contains 7 elements" );			#35
	ok( eq_array( \@elem, \@expected ), "hash contains expected
	    keys" );							#36

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
	foreach my $colinfo ( ( ["ACOL","1B",$x],
				["BCOL","1I",$y],
				["CCOL","1J",$c->long],
				["DCOL","${dlen}A",$d],
				["ECOL","1E",$e],
	##			["FCOL","1M",$f]
			      ) ) {
	  is( $$table2{hdr}{"TTYPE$i"}, $$colinfo[0], "column $i is $$colinfo[0]" ); #37,43,49,55,58
	  is( $$table2{hdr}{"TFORM$i"}, $$colinfo[1], "  and is stored as $$colinfo[1]" ); #38,44,50,56,59
	  my $col = $$table2{$$colinfo[0]};
	  if ( UNIVERSAL::isa($col,"PDL") ) {
	    compare_ndarrays $col, $$colinfo[2], $$colinfo[0]; #39-42,45-48,51-54,60-63
	  } else {
	    # Need to somehow handle the arrays since the data read in from the
	    # file all have 15-character length strings (or whatever the length is)
	    #
	    ok( eq_array($col, $$colinfo[2]),
		"  $$colinfo[0] values agree (as an array reference)" );#57
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
    my @target_bitpix = (8,16,32,-32,-64);
    my $bp_i = 0;
    for my $cref ( \(&byte, &short, &long, &float, &double) ) {
        for my $x ($a1,$a2) {
            $p = &$cref($x);
            unlink $file;
            $p->wfits($file);
            $q = PDL->rfits($file);
	    my $flag = 1;
            if ( ${$p->get_dataref} ne ${$q->get_dataref} ) {
	        $flag = 0;
	        diag "\tnelem=",$p->nelem,"datatype=",$p->get_datatype;
	        diag "\tp:", unpack("c" x ($p->nelem*howbig($p->get_datatype)), ${$p->get_dataref});
	        diag "\tq:", unpack("c" x ($q->nelem*howbig($q->get_datatype)), ${$q->get_dataref});
            }
	    is($q->hdr->{BITPIX},$target_bitpix[$bp_i],"BITPIX implicitly set to " . $target_bitpix[$bp_i]);
	    ok($flag,"hash reference - type check: " . &$cref ); #64-73
        }
	$bp_i++;
    }
}

{
    local $| = 1;
    my $p1= pdl  [1,2];
    my $p2= pdl [[1,2],[1,2]];
    my $q;
    my @s;
    for my $i (8,16,32,-32,-64) {
    for my $p ($p2, $p1) {
        unlink $file;
        $p->wfits($file,$i);
        $q = PDL->rfits($file);
        @s = $q->stats;
	my $flag;
        if ($s[0] == 1.5 and $s[1] < 0.7072 and $s[1]>0.577) {
           $flag = 1;
        } else {
           $flag = 0;
           diag "s=@s\n";
           diag "\tBITPIX=$i, nelem=", $p->nelem;
           diag "\tbug: $s[0] == 1.5 and $s[1] == 0.5";
           diag "\tp:", unpack("c8" x         $p->nelem,  ${$p->get_dataref});
           diag "\tq:", unpack("c" x abs($i/8*$q->nelem), ${$q->get_dataref});
        }
	is($q->hdr->{BITPIX},$i,"BITPIX explicitly set to $i works");
	ok($flag,"ndarray - bitpix=$i" ); #74-83
    }
    }
};

}; # end of SKIP block

#### Check that discontinuous data (e.g. from fftnd) get written correctly.
#### (Sourceforge bug 3299611) it is possible to store data in a PDL non-contiguously
#### through the C API, by manipulating dimincs; fft uses this technique, which
#### used to hose up fits output.

SKIP:{
    eval "use PDL::FFT";
    skip "PDL::FFT not installed", 79 if $@;

    my $ar = sequence(10,10,10);
    my $ai = zeroes($ar);
    fftnd($ar,$ai);
    unlink $file;
    wfits($ar,$file);
    my $y = rfits($file);
    ok(all($ar==$y),"fftnd output (non-contiguous in memory) is written correctly");
    unlink $file;
}

##############################
# Check multi-HDU read/write

my $x = sequence(5,5);
my $y = rvals(5,5);

our @aa;

lives_ok { wfits([$x,$y],$file) } "wfits with multiple HDUs didn't fail";

lives_ok { @aa = rfits($file) } "rfits in list context didn't fail";

ok( $aa[0]->ndims == $x->ndims && all($aa[0]->shape == $x->shape), "first element has right shape");
ok( all($aa[0] == $x), "first element reproduces written one");

ok( $aa[1]->ndims == $y->ndims && all($aa[1]->shape == $y->shape), "second element has right shape");
ok( all($aa[1] == $y), "Second element reproduces written one");

unlink $file;

##############################
# Rudimentary check for longlong support
SKIP:{
	eval "use PDL::Types";
	our $PDL_LL;
    	skip "Longlong not supported",5   unless ($PDL_LL//0);

	$x = rvals(longlong,7,7);
	eval { wfits($x, $file); };
	is $@, '', "writing a longlong image succeeded";
	eval { $y = rfits($file); };
	is $@, '', "Reading the longlong image succeeded";
	ok(ref($y->hdr) eq "HASH", "Reading the longlong image produced a PDL with a hash header");
	ok($y->hdr->{BITPIX} == 64, "BITPIX value was correct");
	ok(all($y==$x),"The new image matches the old one (longlong)");
	unlink $file;
}

###############################
# Check that tilde expansion works

my $tildefile = cfile('~',"PDL-IO-FITS-test_$$.fits");

# Only read/write the tildefile if the directory is writable.
# Some build environments, like the Debian pbuilder chroots, use a non-existent $HOME.
# See: https://github.com/PDLPorters/pdl/issues/238
if(-w dirname($tildefile)) {
	lives_ok { sequence(3,5,2)->wfits($tildefile) } "wfits tilde expansion didn't fail";
	lives_ok { rfits($tildefile) } "rfits tilde expansion didn't fail";
	$tildefile =~ s/^(~)/glob($1)/e; #use the same trick as in FITS.pm to resolve this filename.
	unlink($tildefile) or warn "Could not delete $tildefile: $!\n"; #clean up.
}

# test bad with r/wfits
{
(undef, my $fname) = File::Temp::tempfile( 'delmeXXXXX', SUFFIX => '.fits', %tmp_opts );
my $x = sequence(10)->setbadat(0);
#diag "Writing to fits: $x  type = (", $x->get_datatype, ")\n";
$x->wfits($fname);
my $y = rfits($fname);
#diag "Read from fits:  $y  type = (", $y->get_datatype, ")\n";
ok( $y->slice('0:0')->isbad, "rfits/wfits propagated bad flag" );
ok( sum(abs($x-$y)) < 1.0e-5, "  and values" );
# now force to integer
$x->wfits($fname,16);
$y = rfits($fname);
my $got = $y->slice('0:0');
ok( $got->isbad, "wfits coerced bad flag with integer datatype" ) or diag "got: $got (from $y)";
ok( sum(abs(convert($x,short)-$y)) < 1.0e-5, "  and the values" );
}

{
my $m51 = rfits('t/m51.fits.fz');
is_deeply [$m51->dims], [384,384], 'right dims from compressed FITS file';
(undef, my $fname) = File::Temp::tempfile( 'delmeXXXXX', SUFFIX => '.fits', %tmp_opts );
my $m51_2;
if ($PDL::Astro_FITS_Header) {
my $m51_tbl = rfits('t/m51.fits.fz',{expand=>0});
wfits($m51_tbl, $fname);
$m51_2 = rfits($fname);
ok all(approx $m51, $m51_2), 'read back written-out bintable FITS file' or diag "got:", $m51_2->info;
$m51->wfits($fname, {compress=>1});
$m51_2 = rfits($fname);
ok all(approx $m51, $m51_2), 'read back written-out compressed FITS file' or diag "got:", $m51_2->info;
}
}

#multi-line HISTORY header writing
{   my $f_out;
    my $hstr = join("\n",'A'..'G');

    $f_out = 'long_history.fits';
    my $x = xvals(10);
    $x->hdr->{'HISTORY'} = $hstr;
    $x->wfits($f_out);
    my $xr = rfits($f_out);
    is($xr->hdr->{'HISTORY'}, $hstr, 'multi-line HISTORY correct with fresh header');
    unlink($f_out) or die "couldn't delete $f_out";

    $f_out = 'm51_longhist.fits';
    my $m51 = rfits('t/m51.fits.fz');
    $m51->hdr->{HISTORY} = $hstr;
    $m51->wfits($f_out);
    my $m51r = rfits($f_out);
    is($m51r->hdr->{'HISTORY'}, $hstr, 'multi-line HISTORY correct with pre-existing header');
    unlink($f_out) or die "couldn't delete $f_out";
}

done_testing();
