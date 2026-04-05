use strict;
use warnings;
use File::Basename;
use PDL::LiteF;
use PDL::Core ':Internal';    # For howbig()
use Test::More;
use Test::PDL;
use Test::Exception;
use PDL::IO::FITS;
require File::Spec;
require File::Temp;
use List::Util;

sub tfile {
    my $fh = File::Temp->new(@_);
    return ( $fh, $fh->filename );
}

################ Test rfits/wfits ########################

subtest 'header check' => sub {

    my $t = long xvals( zeroes( 11, 20 ) ) - 5;
    my ( $fh, $file ) = tfile;
    wfits( $t, $file );    # without a header
    my $t2 = rfits $file;
    unlike $t2->hdr->{COMMENT} // '', qr/HASH/,
      'no "HASH" garbage in written header';

    # note: keywords are converted to uppercase
    my %hdr =
      ( 'Foo' => 'foo', 'Bar' => 42, 'NUM' => '0123', NUMSTR => ['0123'] );
    $t->sethdr( \%hdr );
    wfits( $t, $file );
    $t2 = rfits $file;
    is_pdl $t2, $t, 'w/rfits round-trip';
    my $h = $t2->gethdr;
    subtest 'header items' => sub {
        is $$h{FOO},       "foo",  "FOO";
        is $$h{BAR},       42,     "BAR";
        is $$h{'NUM'} + 1, 124,    "NUM";
        is $$h{'NUMSTR'},  '0123', "NUMSTR";
    };
};

########### Rudimentary table tests ################
subtest 'Astro::FITS::Header' => sub {
  SKIP: {

        skip "Astro::FITS::Header not installed", 79
          if !$PDL::Astro_FITS_Header;

        # note:
        #   the tests do not directly test the output file,
        #   instead they write out a file, read it back in, and
        #   compare to the data used to create the file.
        #   So it is more of a "self consistent" test.

        subtest 'auto column names, auto column types' => sub {

            my $x      = long( 1, 4, 9, 32 );
            my $y      = double( 2.3, 4.3, -999.0, 42 );
            my $table2 = do {
                my $table = { COLA => $x, COLB => $y };
                my ( $fh, $file ) = tfile;
                wfits $table, $file;
                rfits $file;
            };

            ok( defined $table2, "Read of table returned something" );
            is( ref($table2),  "HASH",   "which is a hash reference" );
            is( $$table2{tbl}, "binary", "and appears to be a binary TABLE" );

            ok( exists $$table2{COLA} && exists $$table2{COLB},
                "columns COLA and COLB exist" );
            is( $$table2{hdr}{TTYPE1}, "COLA", "column #1 is COLA" );
            is( $$table2{hdr}{TFORM1}, "1J",   "  stored as 1J" );
            is( $$table2{hdr}{TTYPE2}, "COLB", "column #2 is COLB" );
            is( $$table2{hdr}{TFORM2}, "1D",   "  stored as 1D" );

            is_pdl $$table2{COLA}, $x, "COLA";
            is_pdl $$table2{COLB}, $y, "COLB";

        };

        subtest 'explicit column name, auto column type' => sub {

            my $x = long( 1, 4, 9, 32 );
            my $y = double( 2.3, 4.3, -999.0, 42 );

            my $table2 = do {
                my $table = {
                    BAR => $x,
                    FOO => $y,
                    hdr => { TTYPE1 => 'FOO', TTYPE2 => 'BAR' }
                };
                my ( $fh, $file ) = tfile;
                wfits $table, $file;
                rfits $file;
            };

            ok( defined $table2, "Read of table returned something" );
            is( ref($table2),  "HASH",   "which is a hash reference" );
            is( $$table2{tbl}, "binary", "and appears to be a binary TABLE" );

            is( $$table2{hdr}{TTYPE1}, "FOO", "column #1 is FOO" );
            is( $$table2{hdr}{TFORM1}, "1D",  "  stored as 1D" );
            is( $$table2{hdr}{TTYPE2}, "BAR", "column #2 is BAR" );
            is( $$table2{hdr}{TFORM2}, "1J",  "  stored as 1J" );

            is_pdl $$table2{BAR}, $x, "BAR";
            is_pdl $$table2{FOO}, $y, "FOO";
        };

        # try out more "exotic" data types

        subtest 'exotic data types' => sub {
            my $x = byte( 12, 45, 23, 0 );
            my $y = short( -99, 100, 0, 32767 );
            my $c = ushort( 99, 32768, 65535, 0 );
            my $d = [ "A string", "b", "", "The last string" ];
            my $e = float( -999.0, 0, 0, 12.3 );
            ##my $f = float(1,0,-1,2) + i * float( 0,1,2,-1 );

            my $table2 = do {
                my ( $fh, $file ) = tfile;
                wfits {
                    ACOL => $x,
                    BCOL => $y,
                    CCOL => $c,
                    DCOL => $d,
                    ECOL => $e,
                  },
                  $file;
                rfits $file;
            };

            ok( defined $table2, "Read of table returned something" );
            is( ref($table2),  "HASH",   "which is a hash reference" );
            is( $$table2{tbl}, "binary", "and appears to be a binary TABLE" );

            my @elem     = sort keys %$table2;
            my @expected = sort(qw( ACOL BCOL CCOL DCOL ECOL hdr tbl ));
            is_deeply \@elem, \@expected, "hash contains expected keys";

            # convert the string array so that each element has the same length
            # (and calculate the maximum length to use in the check below)
            #
            my $dlen = 0;
            foreach my $str (@$d) {
                my $len = length($str);
                $dlen = $len > $dlen ? $len : $dlen;
            }
            foreach my $str (@$d) {
                $str .= ' ' x ( $dlen - length($str) );
            }

            # note that, for now, ushort data is written out as a long (Int4)
            # instead of being written out as an Int2 using TSCALE/TZERO
            #
            my $i = 1;
            foreach my $colinfo (
                (
                    [ "ACOL", "1B",       $x ],
                    [ "BCOL", "1I",       $y ],
                    [ "CCOL", "1J",       $c->long ],
                    [ "DCOL", "${dlen}A", $d ],
                    [ "ECOL", "1E",       $e ],
                    ##			["FCOL","1M",$f]
                )
              )
            {
                is( $$table2{hdr}{"TTYPE$i"},
                    $$colinfo[0], "column $i is $$colinfo[0]" );
                is( $$table2{hdr}{"TFORM$i"},
                    $$colinfo[1], "  and is stored as $$colinfo[1]" );
                my $col = $$table2{ $$colinfo[0] };
                if ( UNIVERSAL::isa( $col, "PDL" ) ) {
                    is_pdl $col, $$colinfo[2], $$colinfo[0];
                }
                else {
         # Need to somehow handle the arrays since the data read in from the
         # file all have 15-character length strings (or whatever the length is)
         #
                    is_deeply $col, $$colinfo[2],
                      "$$colinfo[0] values agree (as an array reference)";
                }
                $i++;
            }
        };
    }    # end of SKIP block
};

########### Check if r/wfits bugs are fixed ################

subtest 'Internal FITS Header regression tests' => sub {

    subtest 'r/wfits bug #1' => sub {
        local $| = 1;
        my $a1 = [ 1, 2 ];
        my $a2 = [ [ 1, 2 ], [ 1, 2 ] ];
        my $p;
        my $q;
        my @target_bitpix = ( 8, 16, 32, -32, -64 );
        my $bp_i          = 0;
        for my $cref ( \( &byte, &short, &long, &float, &double ) ) {
            for my $x ( $a1, $a2 ) {
                $p = $cref->($x);
                my ( $fh, $file ) = tfile;
                $p->wfits($file);
                $q = PDL->rfits($file);
                my $flag = 1;
                is_pdl( $p, $q, 'round-trip data' );
                is( $q->hdr->{BITPIX}, $target_bitpix[$bp_i],
                    "BITPIX implicitly set to " . $target_bitpix[$bp_i] );
                ok( $flag, "hash reference - type check: " . &$cref );
            }
            $bp_i++;
        }
    };

    subtest 'r/wfits bug #1' => sub {
        local $| = 1;
        my $p1 = pdl [ 1, 2 ];
        my $p2 = pdl [ [ 1, 2 ], [ 1, 2 ] ];
        my $q;
        my @s;
        for my $i ( 8, 16, 32, -32, -64 ) {
            for my $p ( $p2, $p1 ) {
                my ( $fh, $file ) = tfile;
                $p->wfits( $file, $i );
                $q = PDL->rfits($file);
                @s = $q->stats;
                my $flag;
                if ( $s[0] == 1.5 and $s[1] < 0.7072 and $s[1] > 0.577 ) {
                    $flag = 1;
                }
                else {
                    $flag = 0;
                    diag "s=@s\n";
                    diag "\tBITPIX=$i, nelem=", $p->nelem;
                    diag "\tbug: $s[0] == 1.5 and $s[1] == 0.5";
                    diag "\tp:",
                      unpack( "c8" x $p->nelem, ${ $p->get_dataref } );
                    diag "\tq:",
                      unpack( "c" x abs( $i / 8 * $q->nelem ),
                        ${ $q->get_dataref } );
                }
                is( $q->hdr->{BITPIX}, $i,
                    "BITPIX explicitly set to $i works" );
                ok( $flag, "ndarray - bitpix=$i" );
            }
        }
    };

};

#### Check that discontinuous data (e.g. from fftnd) get written correctly.
#### (Sourceforge bug 3299611) it is possible to store data in a PDL non-contiguously
#### through the C API, by manipulating dimincs; fft uses this technique, which
#### used to hose up fits output.

subtest 'PDL::FFT' => sub {
  SKIP: {
        skip "PDL::FFT not installed", 79
          if !eval 'use PDL::FFT; 1;';

        my $ar = sequence( 10, 10, 10 );
        my $ai = zeroes($ar);
        fftnd( $ar, $ai );
        my ( $fh, $file ) = tfile;
        wfits( $ar, $file );
        my $y = rfits($file);
        is_pdl $ar, $y,
          "fftnd output (non-contiguous in memory) is written correctly";
    }
};

##############################
# Check multi-HDU read/write

subtest 'multi-HDU read/write' => sub {
    my $x = sequence( 5, 5 );
    my $y = rvals( 5, 5 );

    my @aa;

    my ( $fh, $file ) = tfile;
    lives_ok { wfits( [ $x, $y ], $file ) }
    "wfits with multiple HDUs didn't fail";

    lives_ok { @aa = rfits($file) } "rfits in list context didn't fail";

    is_pdl $aa[0], $x, "first element reproduces written one";
    is_pdl $aa[1], $y, "Second element reproduces written one";
};

##############################
# Rudimentary check for longlong support
subtest 'longlong image' => sub {
    my ( $fh, $file ) = tfile;

    my $x = rvals( longlong, 7, 7 );
    lives_ok { wfits( $x, $file ) } 'write';
    my $y;
    lives_ok { $y = rfits($file); } 'read';
    isa_ok $y->hdr, "HASH", "header is hash";
    is $y->hdr->{BITPIX}, 64, "BITPIX value was correct";
    is_pdl $y, $x, "The new image matches the old one";

};

###############################
# Check that tilde expansion works

subtest 'tilde path expansion' => sub {

# Only read/write the tildefile if the directory is writable.
# Some build environments, like the Debian pbuilder chroots, use a non-existent $HOME.
# See: https://github.com/PDLPorters/pdl/issues/238
  SKIP: {
        my $tilde_dir = <~>;

        skip '~ directory is not writeable', 4
          unless -w $tilde_dir;

        my ( $fh, $file ) = tfile( DIR => $tilde_dir, SUFFIX => '.fits' );
        ok( -z $file, "output file is empty" );
        my $tildefile = File::Spec->catfile( '~', basename($file) );
        lives_ok { sequence( 3, 5, 2 )->wfits($tildefile) } "write succeeded";
        ok( -s $file, "output file is not empty" );
        lives_ok { rfits($tildefile) } "rfits succeeded.";
    }
};

###############################
# test bad with r/wfits
subtest 'bad with r/wfits' => sub {
    my ( $fh, $fname ) = tfile;
    my $x = sequence(10)->setbadat(0);
    $x->wfits($fname);
    my $y = rfits($fname);
    is_pdl $y, $x, "wfits/rfits propagated bad flag and values";

    # now force to integer
    $x->wfits( $fname, 16 );
    $y = rfits($fname);
    is_pdl $y, $x->short, "integer wfits/rfits propagated bad flag and values";
};

###############################
subtest 'compressed fits file' => sub {
    my $m51 = rfits('t/m51.fits.fz');
    is_pdl $m51->shape, indx( [ 384, 384 ] ),
      'right dims from compressed FITS file';
    my ( $fh, $fname ) = tfile;
    if ($PDL::Astro_FITS_Header) {
        my $m51_tbl = rfits( 't/m51.fits.fz', { expand => 0 } );
        wfits( $m51_tbl, $fname );
        my $m51_2 = rfits($fname);
        is_pdl $m51_2, $m51, 'read back written-out bintable FITS file';
        $m51->wfits( $fname, { compress => 1 } );
        $m51_2 = rfits($fname);
        is_pdl $m51_2, $m51, 'read back written-out compressed FITS file';
        $m51_2->hdrcpy(1);
        $m51_2                = $m51_2->dummy( 2, 3 )->sever;
        $m51_2->hdr->{NAXIS}  = 3;
        $m51_2->hdr->{NAXIS3} = 3;
        $m51_2->wfits( $fname, { compress => 1 } );
        my $m51_3 = rfits($fname);
        is_pdl $m51_3, $m51_2, 'read back written-out compressed RGB FITS file';
    }
};

###############################
subtest 'multi-line HISTORY' => sub {

    my $hstr = join( "\n", 'A' .. 'G', '' );    # must end in newline

    subtest 'fresh header' => sub {
        my ( $fh, $file ) = tfile;
        my $x = xvals(10);
        $x->hdr->{'HISTORY'} = $hstr;
        $x->wfits($file);
        my $xr   = rfits($file);
        my $hist = $xr->hdr->{'HISTORY'};
        $hist = join '', map "$_\n", @$hist if ref $hist eq 'ARRAY';
        $hist =~ s/ +$//gm;
        is( $hist, $hstr, 'correct with fresh header' );
    };

    subtest 'pre-existing header' => sub {
        my ( $fh, $file ) = tfile;
        my $m51 = rfits('t/m51.fits.fz');
        $m51->hdr->{HISTORY} = $hstr;
        $m51->wfits($file);
        my $m51r = rfits($file);
        my $hist = $m51r->hdr->{'HISTORY'};
        $hist = join '', map "$_\n", @$hist if ref $hist eq 'ARRAY';
        $hist =~ s/ +$//gm;
        is( $hist, $hstr, 'correct with pre-existing header' );
    };

};

###############################
subtest 'write null hdu with correct module' => sub {

    my ( $fh, $file ) = tfile;
    wfits [ pdl(3) ], $file;

    my ($hdr) = rfitshdr( $file . '[0]' );

    my $found;
    if ( my $comments = $hdr->{COMMENT} ) {
        $comments = [ split( /\n/, $comments ) ]
          if $PDL::Astro_FITS_Header;

        $comments = [$comments]
          unless ref($comments) eq 'ARRAY';

        $found = !!List::Util::first { /legacy code/ } @{$comments};
    }

    if ($PDL::Astro_FITS_Header) {
        ok( !$found, "used Astro::FITS::Header" );
    }
    else {
        ok( $found, "used internal legacy code" );
    }

};

###############################
subtest 'accept scalar (0D) ndarrays' => sub {

    subtest 'bintable' => sub {
      SKIP: {
            skip 'require Astro::FITS::Header for bintables', 2
              unless $PDL::Astro_FITS_Header;
            my ( $fh, $file ) = tfile;
            lives_ok { wfits { x => pdl(3) }, $file } 'write column';
            my $got = rfits($file);
            is_pdl( $got->{X}, pdl( [3] ), "got data" );
        }
    };

    subtest 'image' => sub {
        my ( $fh, $file ) = tfile;
        lives_ok { wfits pdl(3), $file } 'write image';
        my $got;
        lives_ok { $got = rfits($file) } 'read image';
        is_pdl( $got, pdl( [3] ), "got data" );

    };

};

###############################
subtest 'handle multiple COMMENT and HISTORY cards' => sub {

    my $cards = join q{}, map { sprintf( '%-80s', $_ ) }
      q{SIMPLE  =                    T},
      q{BITPIX  =                  -32},
      q{NAXIS   =                    1},
      q{NAXIS1  =                    0},
      q{COMMENT comment1},
      q{COMMENT comment2},
      q{HISTORY history1},
      q{HISTORY history2},
      q{END};

    my $fits = sprintf( '%-2880s', $cards );

    my ( $fh, $file ) = tfile;
    $fh->print($fits);
    $fh->flush;

    my $hdr = rfitshdr($file);

    for my $keyword ( 'COMMENT', 'HISTORY' ) {
        ok( defined $hdr->{$keyword}, "got $keyword 'card'" )
          or next;

        my $arr = $hdr->{$keyword};

        if ($PDL::Astro_FITS_Header) {
            $arr = [ split /\n/, $arr ];
        }
        else {
            is( ref($arr), 'ARRAY', 'is array' )
              or next;
        }
        s/\s+$// for @$arr;

        is( $arr->[0], lc($keyword) . '1', 'first card' );
        is( $arr->[1], lc($keyword) . '2', 'second card' );
    }

};

done_testing();
