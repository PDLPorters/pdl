=head1 NAME

PDL::IO::FITS -- Simple FITS support for PDL

=head1 SYNOPSIS

 use PDL;
 use PDL::IO::FITS;

 $a = rfits('foo.fits');          # read a FITS file

 $a->wfits('bar.fits');           # write a FITS file

=head1 DESCRIPTION

This module provides basic FITS support for PDL, in the sense of
reading and writing whole FITS files.  (For more complex operations,
such as prefiltering rows out of tables or performing operations on
the FITS file in-place on disk), you can use the Astro::FITS::CFITSIO
module that is available on CPAN.

Basic FITS image files are supported, along with BINTABLE and IMAGE extensions.

=head1 AUTHOR

Copyright (C) Karl Glazebrook and Craig DeForest, 1997-2003.
There is no warranty.  You are allowed to redistribute and/or modify
this software under certain conditions.  For details, see the file
COPYING in the PDL distribution.  If this file is separated from the 
PDL distribution, the copyright notice should be pasted into in this file.

=head1 FUNCTIONS

=cut

BEGIN {

package PDL::IO::FITS;
@EXPORT_OK = qw( rfits rfitshdr wfits );
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);
@ISA = ('PDL::Exporter');

use PDL::Core;
use PDL::IO::Misc;
use PDL::Exporter;
use PDL::Primitive;
use PDL::Types;
use PDL::Options;
use PDL::Bad;
use Carp;
use strict;

##############################
#
# Check if there's Astro::FITS::Header support, and set flag.
# Kludgy but it only has to run once, on first load.  --CED
#
  eval "use Astro::FITS::Header;";
  $PDL::Astro_FITS_Header = (defined $Astro::FITS::Header::VERSION);
  if($PDL::Astro_FITS_Header) {
    my($a) = $Astro::FITS::Header::VERSION;
    $a =~ s/[^0-9\.].*//;
    $PDL::Astro_FITS_Header = 0 if($a < 1.12);
  }

}


package PDL::IO::FITS;

=head2 rfits()

=for ref

Simple piddle FITS reader.

=for example

  $pdl = rfits('file.fits');   # Read a simple FITS image

Suffix magic:

  $pdl = rfits('file.fits.gz'); # Read a file with gunzip(1)
  $pdl = rfits('file.fits.Z');  # Read a file with uncompress(1)

  $pdl = rfits('file.fits[2]');    # Read 2nd extension
  $pdl = rfits('file.fits.gz[3]'); # Read 3rd extension
  @pdls = rfits('file.fits');      # Read primary data and extensions

In list context, C<rfits> reads the primary image and all possible
extensions, returning them in the same order that they occurred in the
file.  In scalar context, the default is to read the primary HDU. One
can read other HDU's by using the [n] syntax, the second one is [1].
Currently recognized extensions are IMAGE and BINTABLE.  (See the
addendum on EXTENSIONS for details).

FITS image headers are stored in the output PDL and can be retrieved
with L<hdr|PDL::Core/hdr> or L<gethdr|PDL::Core/gethdr>.  The
L<hdrcpy|PDL::Core/hdrcpy> flag of the PDL is set so that the header
is copied to derived piddles by default.

The header is a hash whose keys are the keywords in the FITS header.
If you have the "Astro::FITS::Header" module installed, the header is
actually a tied hash to a FITS header object, which can give you
more control over card order, comment fields, and variable types.
(see L<Astro::FITS::Header> for details).

The header keywords are converted to I<uppercase> per the FITS
standard.  Access is case-insensitive on the perl side, provided that
Astro::FITS::Header is installed.

Keyword-associated comments in the headers are stored as under the
hash key C<E<lt>keywordE<gt>_COMMENT>.  All HISTORY cards in the 
header are collected into a single multiline string stored in the 
C<HISTORY> key.  All COMMENT cards are similarly collected under
the C<COMMENT> key.

BSCALE/BZERO

If the BSCALE and/or BZERO keywords are set, they are applied to the
image before it is returned.  The returned PDL is promoted as
necessary to contain the multiplied values, and the BSCALE and BZERO
keywords are deleted from the header for clarity.  If you don't want
this type of processing, set 'bscale=>0' in the options hash.

BINTABLE EXTENSIONS

Binary tables are handled.  

The return value for a binary table is a hash ref containing the names
of the columns in the table (in UPPER CASE as per the FITS standard).
Each element of the hash contains a PDL (for numerical values) or a
perl list (for string values).  The PDL's 0th dimension runs across
rows; the 1st dimension runs across the repeat index within the row
(for rows with more than one value).  

Thus, if your table contains a column named C<FOO> with type C<5D>,
the expression

  $a->{FOO}->((2))

returns a 5-element double-precision PDL containing the values of FOO
from the third row of the table.

The header of the table itself is parsed as with a normal FITS HDU,
and is returned in the element 'hdr' of the returned hash.  You can
use that to presere the original column order, if you like.

Because different columns in the table might have identical names, the
binary table reader practices collision avoidance.  If you have
multiple columns named "FOO", then the first one encountered
(numerically) gets the name "FOO", the next one gets "FOO_1", and the
next "FOO_2", etc.  The appropriate TTYPEn fields in the header are
changed to match the renamed column fields.

Scaling and zero-point adjustment are performed as with BSCALE/BZERO:
the appropriate keywords are deleted from the as-returned header.  To avoid
this behavior, set 'bscale=>0' in the options hash.

=for bad

If a FITS file contains the C<BLANK> keyword (and has C<BITPIX E<gt> 0>), 
the piddle will have its bad flag set, and those elements which equal the
C<BLANK> value will be set bad.  For C<BITPIX E<lt> 0>, any NaN's are
converted to bad (if necessary).


=head2 rfitshdr()

=for ref

Read only the header of a FITS file or an extension within it.

This is syntactic sugar for the C<data=>0> option to L<rfits|/rfits()>.

See L<rfits|/rfits()> for details on header handling.  rfitshdr() runs 
the same code to read the header, but returns it rather than 
reading in a data structure as well.

=cut

# rfits (finally)...


$PDL::FITS::rfits_options = new PDL::Options( {
  bscale=>1
 ,data=>1
  }
					      );


sub PDL::rfits {

  my $class = shift;
  
  barf 'Usage: $a = rfits($file)  -or-   $a = PDL->rfits($file)' if (@_ < 1 || @_ > 2);
  
  my $file = shift; 
  
  my $u_opt = ifhref(shift);
  
  my $opt = new PDL::Options( {
    data=>1,
    bscale=>1
    }
			      );
  
  $opt = $opt->options($u_opt);
  
  
  my($nbytes, $line, $name, $rest, $size, $i, $bscale, $bzero);
  my $extnum;
  
  if ($file =~ s/\[(\d+)\]$//) {
    $extnum = $1;
  }else{
    $extnum = 0;
  }
  
  $file = "gunzip -c $file |" if $file =~ /\.gz$/;    # Handle compression
  $file = "uncompress -c $file |" if $file =~ /\.Z$/;
  
  open(FITS, $file) || barf "FITS file $file not found";
  binmode FITS;
  $nbytes = 0; # Number of bytes read so far
  my @extensions;  # This accumulates the list in list context...
  my $currentext=0;
  my $pdl;

 list:do {                     # Runs over extensions, in list context
     my $ext_type;      # Gets the type of XTENSION if one is detected.
     my $foo={};       # To go in pdl
     my @history=();
     my @cards = ();


     $pdl = $class->new;


   head:while( !eof(FITS)) {     # This while() runs over header lines only
       
       
       read(FITS,$line,80);
       barf "file $file is not in FITS-format:\n$line\n"
	   if( $nbytes==0 && ($line !~ /^SIMPLE  = +T/));
       
       if($line =~ /^XTENSION= \'(\w+)\s*\'/) {
	   $ext_type = $1;
       }
       
       $nbytes += 80;
       push(@cards,$line)  if($PDL::Astro_FITS_Header);

       #
       # If we're in scalar context, skip to the desired extension
       # number.  [This implementation is really slow since we have
       # to read the whole file.  Someone Really Ought To rework it to
       # read individual headers and skip forward an extension at a
       # a time with seek() calls.  
       #     --CD
       #
       
       if(!wantarray and $currentext != $extnum) {
	 skipper: while(1) {
	     # Move to next record
	     read(FITS,$line,2880-80);
	     barf "Unexpected end of FITS file\n" if eof(FITS);
	     # Read start of next record
	     read(FITS,$line,80);
	     barf "Unexpected end of FITS file\n" if eof(FITS);
	     # Check if we have found the new extension
	     # if not move on
	     $currentext++ if  $line =~ /^XTENSION/;
	     if ($currentext == $extnum) {
		 barf "Bad FITS extension $currentext doesn't start with 'XTENSION'\n"
		     unless ( $line =~ /^XTENSION= \'(\w+)\s*\'/ );
		 $ext_type = $1;
		 last skipper;
	     }   
	 }
       } # End of skipping to desired extension
       
       
       $name = (split(' ',substr($line,0,8)))[0]; 
       $rest = substr($line,8);
       
       #
       # Snarf up the found header, and parse it if Astro::FITS::Header
       # doesn't exist.
       # 
       
       if(!($PDL::Astro_FITS_Header)) { 
	   # (No FITS header library -- do legacy parsing)
	   
	   # skip if the first eight characters are ' '
	   # - as seen in headers from the DSS at STScI
	   next if substr($line,0,8) eq " " x 8;
	   
	   $name = (split(' ',substr($line,0,8)))[0]; 
	   $rest = substr($line,8);
	   
	   if ($name =~ m/^HISTORY/) {
	       push @history, $rest;
	   } else {
	       $$foo{$name} = "";
	       
	       $$foo{$name}=$1 if $rest =~ m|^= +([^\/\' ][^\/ ]*) *( +/(.*))?$| ;
	       $$foo{$name}=$1 if $rest =~ m|^= \'(.*)\' *( +/(.*))?$| ;
	       $$foo{COMMENT}{$name} = $3 if defined($3);
	   }
       } # End of legacy parsing
       
       last head if ((defined $name) && $name eq "END");
       
   }
     
     # Clean up HISTORY card
     # (This line only runs if KGB parsing has happened)
     $$foo{"HISTORY"} = \@history if $#history >= 0;
     
     # Step to end of header block in file
     my $skip = 2879 - ($nbytes-1)%2880;
     read(FITS, my $dummy, $skip) if $skip; 
     $nbytes += $skip;
     
     
     # Do header parsing and tying if Astro::FITS::Header is enabled
     if($PDL::Astro_FITS_Header) {
	 my($hdr) = new Astro::FITS::Header(Cards => \@cards);
	 my(%hdrhash);
	 tie %hdrhash,"Astro::FITS::Header",$hdr;
	 $foo = \%hdrhash;
     }
     
     if( ! $opt->{data} ) {
	 $pdl = $foo;
     } else {

	 # Switch based on extension type -- default is IMAGE.
	 
	 #
	 # Check if we're reading an XTENSION block.  If so, then jump
	 # into the appropriate sub in the FITS_Extension patch table.  
	 # Otherwise, just carry on and read the image. 
	 #
	 
	 if ($ext_type && !defined($PDL::IO::FITS::_Extension->{$ext_type})) {
	     
	     print STDERR "rfits: Ignoring unknown extension $ext_type...";
	     $pdl = undef;
	     
	 } elsif($ext_type && ref $PDL::IO::FITS::_Extension->{$ext_type} ) {
	     
	     # Pass $pdl into the extension reader for easier use -- but
	     # it just gets overwritten (and disappears) if ignored.
	     
	     $pdl = &{$PDL::IO::FITS::_Extension->{$ext_type}}($foo,$opt,$pdl);
	     
	 } else {

	     #
	     # This is the default code -- if the current block is NOT 
	     # an XTENSION, or if it is an XTENSION of type IMAGE, then 
	     # we read it.  Other types of XTENSION should either have
	     # code refs in $FITS_Extension that point to the correct
	     # reader, or else should have caused an error to be thrown 
	     # in the header-reading part.
	     #
	     
	     # Setup piddle structure
	     
	     $pdl->set_datatype($PDL_B)    if $$foo{"BITPIX"} ==   8;
	     $pdl->set_datatype($PDL_S)    if $$foo{"BITPIX"} ==  16;
	     $pdl->set_datatype($PDL_L)    if $$foo{"BITPIX"} ==  32;
	     $pdl->set_datatype($PDL_F)    if $$foo{"BITPIX"} == -32;
	     $pdl->set_datatype($PDL_D)    if $$foo{"BITPIX"} == -64;
	     
	     my @dims; # Store the dimenions 1..N, compute total number of pixels
	     $size = 1;  $i=1;
	     while(defined( $$foo{"NAXIS$i"} )) {
		 $size *= $$foo{"NAXIS$i"};
		 push @dims, $$foo{"NAXIS$i"} ; $i++;
	     }
	     $pdl->setdims([@dims]);
	     
	     my $dref = $pdl->get_dataref();
	     
	     print "BITPIX = ",$$foo{"BITPIX"}," size = $size pixels \n"
		 if $PDL::verbose;
	     
	     # Slurp the FITS binary data
	     
	     print "Reading ",$size*PDL::Core::howbig($pdl->get_datatype) , " bytes\n" 
		 if $PDL::verbose;
	     
	     # Read the data and pad to the next HDU
	     my $rdct = $size * PDL::Core::howbig($pdl->get_datatype);
	     read( FITS, $$dref, $rdct );
	     read( FITS, my $dummy, 2880 - (($rdct-1) % 2880) - 1 );
	     $pdl->upd_data();
	 
	     
	     
	     if (!isbigendian() ) { # Need to byte swap on little endian machines
		 bswap2($pdl) if $pdl->get_datatype == $PDL_S;
		 bswap4($pdl) if $pdl->get_datatype == $PDL_L || 
		     $pdl->get_datatype == $PDL_F;
		 bswap8($pdl) if $pdl->get_datatype == $PDL_D;
	     }
	     
	     if($opt->{bscale}) {
		 if ( $PDL::Bad::Status ) {
		     # do we have bad values? - needs to be done before BSCALE/BZERO
		     # (at least for integers)
		     #
		     if ( $$foo{BITPIX} > 0 and exists $$foo{BLANK} ) {
			 # integer, so bad value == BLANK keyword
			 my $blank = $foo->{BLANK};
			 # do we have to do any conversion?
			 if ( $blank == $pdl->badvalue() ) {
			     $pdl->badflag(1);
			 } else {
			     # we change all BLANK values to the current bad value
			     # (would not be needed with a per-piddle bad value)
			     $pdl->inplace->setvaltobad( $blank );
			 }
		     } elsif ( $foo->{BITPIX} < 0 ) {
			 # bad values are stored as NaN's in FITS
			 # let setnanbad decide if we need to change anything
			 $pdl->inplace->setnantobad();
		     }
		     print "FITS file may contain bad values.\n"
			 if $pdl->badflag() and $PDL::verbose;
		 } # if: PDL::Bad::Status
		 
		 $bscale = $$foo{"BSCALE"}; $bzero = $$foo{"BZERO"};
		 print "BSCALE = $bscale &&  BZERO = $bzero\n" if $PDL::verbose;
		 $bscale = 1 if (!defined($bscale) || $bscale eq "");
		 $bzero  = 0 if (!defined($bzero)  || $bzero  eq "");
		 
		 # Be clever and work out the final datatype before eating
		 # memory
		 #
		 # ensure we pick an element that is not equal to the bad value
		 # (is this OTT?)
		 my $tmp;
		 if ( $pdl->badflag() == 0 ) {
		     $tmp = $pdl->flat()->slice("0:0");
		 } elsif ( $pdl->ngood > 0 ) {
		     my $index = which( $pdl->flat()->isbad() == 0 )->at(0);
		     $tmp = $pdl->flat()->slice("${index}:${index}");
		 } else {
		     # all bad, so ignore the type conversion and return
		     # -- too lazy to include this check in the code below,
		     #    so just copy the header clean up stuff
		     print "All elements are bad.\n" if $PDL::verbose;
		     
		     delete $$foo{"BSCALE"}; delete $$foo{"BZERO"};
		 }  #end of BSCALE section (whew!)
		 
		 
		 $tmp = $tmp*$bscale if $bscale != 1; # Dummy run on one element
		 $tmp = $tmp+$bzero  if $bzero  != 0;
		 
		 $pdl = $pdl->convert($tmp->type) if $tmp->get_datatype != $pdl->get_datatype;
		 
		 $pdl *= $bscale if $bscale != 1;
		 $pdl += $bzero  if $bzero  != 0;
		 
		 delete $$foo{"BSCALE"}; delete $$foo{"BZERO"};
	     }
	     
	     # Header
	     
	     $pdl->sethdr($foo);
	     $pdl->hdrcpy(1);
	 } #  End of image-reading code (default non-extension code)
	 
	 #
	 # Note -- $pdl isn't necessarily a PDL if this block was an XTENSION!
	 # But it's definitely a scalar or ref, so we can just return it.
	 #
	 push(@extensions,$pdl)  if(wantarray);
     }
     $currentext++;
 } while( wantarray && !eof(FITS) );
  
  close FITS;
  
  return @extensions if(wantarray);
  $pdl;
}
  
sub rfits { PDL->rfits(@_); }
  
sub rfitshdr { 
   my($file,$opt) = shift; 
   $opt->{data} =0; 
   PDL->rfitshdr($file,$opt); 
}
  
  
##############################
#
# FITS extensions patch-table links extension name to the supported reader.
# IMAGE extensions are a special case that gets read just like a normal
# FITS file.   
# 
  
  $PDL::IO::FITS::_Extension = {
    IMAGE    => 1
      , BINTABLE => \&_rfits_bintable
    };
  

##########
# 
# bintable_handlers -- helper table for bintable_row, below.
#
# Each element of the table is named by the appropriate type letter
# from the FITS specification.  The value is a list containing the 
# reading and writing methods.
#
# This probably ought to be a separate class, but instead it's a tawdry
# imitation.  Too bad -- except that the ersatz really does run faster than
# genuine.
#
# 0: either a data type or a constructor.
# 1: either a length per element or a read method. 
# 2: either a length per element or a write method.
# 3: 'finish' contains finishing-up code or a byte-count to swap.
#
# Main bintable type handler table.  
# Elements: (constructor or type,  reader or nbytes, writer or nbytes, 
# finisher or nbytes).  The finisher should convert the internal reading
# format into the final output format, e.g. by swapping (which is done 
# automatically in the basic case).  Output has row in the 0th dim.
#
# If present, the constructor should
# accept ($rowlen $extra, $nrows, $$size), where $rowlen is the repeat 
# specifier in the TFORM field, $extra is the extra characters if any 
# (for added flavor later, if desired), and $nrows is the number of rows in 
# the table.  $$size points to a scalar value that should be incremented by the
# size (in bytes) of a single row of the data, for accounting purposes.
#
# If a read  method is specified, it should accept:
#   ($thing, $rownum, $strptr, $rpt, $extra)
# where $rpt is the repeat count and $extra is the extra characters in the 
# specifier; and it should cut the used characters off the front of the string.
#
# If a writer is specified it should accept:
#   ($thing, $row, $rpt, $extra)
# and return the generated binary string.
#
# The finisher just takes the data itself.  It should:
#    * Byteswap
#    * Condition the data to final dimensional form (if necessary)
#    * Apply TSCAL/TZERO keywords (as necessary)
#
# The magic numbers in the table (1,2,4,8, etc.) are kludgey -- they break
# the isolation of PDL size and local code -- but they are a part of the FITS
# standard.  The code will break anyway (damn) on machines that have other 
# sizes for these datatypes.
# 

$PDL::IO::FITS_bintable_handlers = {
  'X' => [ byte                              # Packed bit field
           , sub { 
               my( $pdl, $row, $strptr ) = @_;  # (ignore repeat and extra)
               my $n = $pdl->dim(0);
               my $s =  unpack( "B".$n,  substr( $$strptr, 0, int(($n+7)/8)));
               $s =~ tr/[01]/[\000\001]/;
               substr(${$pdl->get_dataref},  $n * $row,  length($s)) = $s;
             }
           , sub { 
               my( $pdl, $row ) = @_;  # Ignore extra and rpt
               my $n = $pdl->dim(0);
               my $p2 = byte(($pdl->(($row)) != 0));
               my $s = ${$p2->get_dataref};
               $s =~ tr/[\000\001]/[01]/;
               pack(  "B".$pdl->dim(0), $s );
             }
          , 1 
    ]
  ,'A' => [  sub { # constructor               # String  - handle as perl list
               my($rowlen, $extra, $nrows, $szptr) = @_;
               my($i,@a);
               if(ref $szptr eq 'SCALAR') {$$szptr += $rowlen;}
               for $i(1..$nrows) { push(@a,' 'x$rowlen); }
               \@a;
            }
          , sub { # reader 
              my( $list, $row, $strptr, $rpt ) = @_;
              $list->[$row] = substr(${$strptr},0,$rpt,'');
            }
          , sub { # writer
              my($strs, $row, $rpt ) = @_;
              my $s = substr($strs->[$row],0,$rpt);
              $s . ' 'x($rpt - length $s);
            } 
          , undef # no finisher needed
         ]             
  ,'B' => [ byte,    1,     1,     1  ] # byte
  ,'L' => [ byte,    1,     1,     1  ] # logical - treat as byte
  ,'I' => [ short,   2,     2,     2  ] # short (no unsigned shorts?)
  ,'J' => [ long,    4,     4,     4  ] # long
  ,'E' => [ float,   4,     4,     4  ] # single-precision
  ,'D' => [ double,  8,     8,     8  ] # double-precision
  ,'C' => [ sub { _nucomplx(float,  @_) }, sub { _rdcomplx(float,  @_) },
	    sub { _wrcomplx(float,  @_) }, sub { _fncomplx(float,  @_) } 
      ]
  ,'M' => [ sub { _nucomplx(double, @_) }, sub { _rdcomplx(double, @_) },
	    sub { _wrcomplx(double, @_) }, sub { _fncomplx(double, @_) } 
      ]
# 'P' - array descriptor -- not supported.
};


##############################
# Helpers for complex numbers (construct/read/write/finish)
sub _nucomplx { # complex-number constructor
  my($type, $rowlen, $extra, $nrows, $szptr) = @_;
  $szptr += PDL::Core::howbig($type) * $nrows * 2;
  return PDL->new_from_specification($type,2,$rowlen,$nrows);
}
sub _rdcomplx { # complex-number reader
  my( $type, $pdl, $row, $strptr, $rpt ) = @_;  # ignore extra
  my $s = $pdl->get_dataref;
  my $rlen = 2 * PDL::Core::howbig($type) * $rpt;
  substr($$s, $row*$rlen, $rlen) = substr($strptr, 0, $rlen, '');
}
sub _wrcomplx { # complex-number writer
  my( $type, $pdl, $row, $rpt ) = @_; # ignore extra
  my $rlen = 2 * PDL::Core::howbig($type) * $rpt;
  substr( ${$pdl->get_dataref}, $rlen * $row, $rlen );
}
sub _fncomplx { # complex-number finisher-upper
  my( $type, $pdl, $n, $hdr, $opt)  = shift;
  eval 'bswap'.(PDL::Core::howbig($type)).'($pdl)';
  $pdl->reorder(2,1,0);
  print STDERR "Ignoring poorly-defined TSCAL/TZERO for complex data in col. $n (".$hdr->{"TTYPE$n"}.").\n" 
    if( length($hdr->{"TSCAL$n"}) or length($hdr->{"TZERO$n"}) );
}


##############################
#
# _rfits_bintable -- snarf up a binary table, returning the named columns
# in a hash ref, each element of which is a PDL or list ref according to the
# header.
# 

sub _rfits_bintable {
  my $hdr = shift;
  my $opt = shift;
  ##shift;  ### (ignore $pdl argument)

  
  print STDERR "Warning: BINTABLE extension should have BITPIX=8, found ".$hdr->{BITPIX}.".  Winging it...\n" unless($hdr->{BITPIX} == 8);
  
  
  
  ### Allocate the main table hash
  my $tbl = {};    # Table is indexed by name
  $tbl->{hdr} = $hdr;
  my $tmp = [];    # Temporary space is indexed by col. no.
  
  
  ### Allocate all the columns of the table, checking for consistency
  ### and name duplication.
  
  barf "Binary extension has no fields (TFIELDS=0)" unless($hdr->{TFIELDS});
  my $rowlen = 0;
  
  for my $i(1..$hdr->{TFIELDS}) {
    my $iter;
    my $name = $tmp->[$i]->{name} = $hdr->{"TTYPE$i"};
    
    ### Allocate some temp space for dealing with this column
    my $tmpcol = $tmp->[$i] = {};
    
    ### Check for duplicate name and change accordingly...
    while( defined(  $tbl->{ $name } ) ) {
      $iter++;
      $name = $hdr->{"TTYPE$i"}."_$iter";
    }
    
    # (Check avoids scrozzling comment fields unnecessarily)
    $hdr->{"TTYPE$i"} = $name unless($hdr->{"TTYPE$i"} eq $name);
    $tmpcol->{name} = $name;
    
    if( ($hdr->{"TFORM$i"}) =~ m/(\d*)(.)(.*)/ ) {
      ($tmpcol->{rpt},  $tmpcol->{type},  $tmpcol->{extra}) = ($1,$2,$3);
    } else {
      barf "Couldn't parse BINTABLE form'"
        . $hdr->{TFORM$i}
      . "' for column $i ("
        . $hdr->{TTYPE$i}
      . ")\n";
    }
    
    $tmpcol->{handler} =  # sic - assignment
      $PDL::IO::FITS_bintable_handlers->{ $tmpcol->{type} }
    or 
      barf "Unknown type ".$hdr->{"TFORM$i"}." in BINTABLE column $i ("
      . $hdr->{"TTYPE$i"}
    . ")\n  That invalidates the byte count, so I give up.\n" ;
    
    
    ### Allocate the actual data space and increment the row length
    
    my $foo = $tmpcol->{handler}->[0];
    if( ref ($foo) eq 'CODE' ) {
      $tmpcol->{data} = $tbl->{$name} = 
        &{$foo}( $tmpcol->{rpt}
                 , $tmpcol->{extra}
                 , $hdr->{NAXIS2}
                 , \$rowlen );
    } else {
      $tmpcol->{data} = $tbl->{$name} = 
        PDL->new_from_specification(
                                    $foo 
                                    , $tmpcol->{rpt}, 
                                    , $hdr->{NAXIS2}
                                    );
      $rowlen += PDL::Core::howbig($foo) * $tmpcol->{rpt};
    }
    
    print "Prefrobnicated col. $i (".$hdr->{"TTYPE$i"}.")\ttype is ".$hdr->{"TFORM$i"}."\t length is now $rowlen\n" if($PDL::debug);
    
    
  }  ### End of prefrobnication loop...
  
  barf "Calculated row length is $rowlen, hdr claims ".$hdr->{NAXIS1}
  .".  Giving up.  (Set \$PDL::debug for more detailed info)\n"
    if($rowlen != $hdr->{NAXIS1});
  
  
  
  ### Snarf up the whole extension, and pad to 2880 bytes...
  my ($rawtable, $n);
  $n = $hdr->{NAXIS1} * $hdr->{NAXIS2} + $hdr->{PCOUNT};
  $n = ($n-1)+2880 - (($n-1) % 2880);
  read(FITS, $rawtable, $n);
  
  ### Frobnicate the rows, one at a time.
  for my $row(0..$hdr->{NAXIS2}-1) {
    my $prelen = length($rawtable);
    for my $i(1..$hdr->{TFIELDS}) {
      my $tmpcol = $tmp->[$i];
      my $reader = $tmpcol->{handler}->[1];
      if(ref $reader eq 'CODE') {
        &{$reader}( $tmpcol->{data}
                    , $row
                    , \$rawtable
                    , $tmpcol->{rpt}
                    , $tmpcol->{extra}
                    );
      } elsif(ref $tmpcol->{data} eq 'PDL') {
        my $rlen = $reader * $tmpcol->{rpt};

        substr( ${$tmpcol->{data}->get_dataref()}, $rlen * $row, $rlen ) = 
          substr( $rawtable, 0, $rlen, '');
        $tmpcol->{data}->upd_data;

    } else {
      barf("Bug detected: inconsistent types in BINTABLE reader\n");
    }
  }
  if(length($rawtable) ne $prelen - $hdr->{NAXIS1}) {
    barf "Something got screwed up -- expected a length of $prelen - $hdr->{NAXIS1}, got ".length($rawtable).".  Giving up.\n";
  }
}

### Postfrobnicate the columns.
for my $i(1..$hdr->{TFIELDS}) {
  my $tmpcol = $tmp->[$i];
  my $post = $tmpcol->{handler}->[3];
  
  if(ref $post eq 'CODE') {
    
    $tbl->{$tmpcol->{name}} = &$post($tmpcol->{data}, $i, $hdr, $opt);
    
  } elsif( (ref ($tmpcol->{data})) eq 'PDL' ) {
    $tmpcol->{data}->upd_data;

    unless( isbigendian() ) {
      if(    $post == 2 ) { bswap2($tmpcol->{data}); }
      elsif( $post == 4 ) { bswap4($tmpcol->{data}); }
      elsif( $post == 8 ) { bswap8($tmpcol->{data}); }
      elsif( $post != 1 ) {
        print STDERR "Unknown swapsize $post for column $i ("
          . $tmpcol->{name} . ")!  This is a bug.  Winging it.\n";
      }
    }
    
    # Apply scaling and badval keywords, which are illegal for A, L, and X
    # types but legal for anyone else.
    
    if($opt->{bscale}) {
      my $tzero = $hdr->{"TZERO$i"};
      my $tscal = $hdr->{"TSCAL$i"};
      if( (length($tzero) || length($tscal)) && $tmpcol->{type} =~ m/[ALX]/i ){
	print STDERR "Ignoring illegal TSCAL/TZERO keywords for col $i (" .
	  $tmpcol->{name} . "); type is $tmpcol->{type})\n" 
	  if(defined($tzero) or defined $tscal);
	
      } else {
	unless((length($tscal)==0) || $tscal==1) {
	  
	  # Use PDL's cleverness to work out the final datatype...
	  
	  my $tmp;
	  my $pdl = $tmpcol->{data};
	  if($pdl->badflag() == 0) {
	    $tmp = $pdl->flat()->slice("0:0");
	  } elsif($pdl->ngood > 0) {
	    my $index = which( $pdl->flat()->isbad()==0 )->at(0);
	    $tmp = $pdl->flat()->slice("${index}:${index}");
	  } else { # Do nothing if it's all bad....
	    $tmp = $pdl;
	  }
	  
	  $tmp *= $tscal; 
	  $tmp += $tzero;
	  
	  $tmpcol->{data} = $pdl->convert($tmp->type) 
	    if($tmp->get_datatype != $pdl->get_datatype);
	}
	
	delete $hdr->{"TZERO$i"};
	delete $hdr->{"TSCAL$i"};
    }
    } # end of scaling section.


    $tbl->{$tmpcol->{name}} = 
	  ( $tmpcol->{data}->dim(0) == 1 ) 
	? $tmpcol->{data}->slice("(0)") 
	: $tmpcol->{data}->xchg(0,1);
    
  } elsif(defined $post) {
    print STDERR "Postfrobnication bug detected in column $i ("
      . $tmpcol->{name}. ").  Winging it.\n";
  }
}


### Done!
return $tbl;

}



=head2 wfits()
  
=for ref

Simple PDL FITS writer

=for example

  wfits $pdl, 'filename.fits', [$BITPIX];
  $pdl->wfits('foo.fits',-32);

Suffix magic:

  # Automatically compress through pipe to gzip
  wfits $pdl, 'filename.fits.gz';
  # Automatically compress through pipe to compress 
  wfits $pdl, 'filename.fits.Z';  

$BITPIX is optional and coerces the output format.

Header handling:

If C<$pdl> has a FITS header attached to it (actually, any hash that
contains a SIMPLE=>T keyword), then that FITS header is written out to the
file.  The image dimension tags are adjusted to the actual dataset.
If there's a mismatch between the dimensions of the data and the 
dimensions in the FITS header, then the header gets corrected and a 
warning is printed.

=for bad

For integer types (ie C<BITPIX E<gt> 0>), the C<BLANK> keyword is set
to the bad value.  For floating-point types, the bad value is
converted to NaN (if necessary) before writing.

=cut

*wfits = \&PDL::wfits;

BEGIN {
  @PDL::IO::FITS::wfits_keyword_order = 
    ('SIMPLE','BITPIX','NAXIS','NAXIS1','BUNIT','BSCALE','BZERO');
}

# Until we do a rewrite these have to be file global since they
# are used by the wheader routine
my (%hdr, $nbytes);

sub PDL::wfits { # Write a PDL to a FITS format file
  barf 'Usage: wfits($pdl,$file,[$BITPIX])' if $#_<1 || $#_>2;
  
  my ($pdl,$file,$BITPIX) = @_;
  my ($k, $buff, $off, $ndims, $sz);
  
  if ($file =~ /\.gz$/) {            # Handle compression
    $file = "|gzip -9 > $file";
  }
  elsif ($file =~ /\.Z$/) {
    $file = "|compress > $file";
  }
  else{
    $file = ">$file";
  }
  
  # Figure output type
  
  $BITPIX = "" unless defined $BITPIX;
  if ($BITPIX eq "") {
    $BITPIX =   8 if $pdl->get_datatype == $PDL_B;
    $BITPIX =  16 if $pdl->get_datatype == $PDL_S || $pdl->get_datatype == $PDL_US;
    $BITPIX =  32 if $pdl->get_datatype == $PDL_L;
    $BITPIX = -32 if $pdl->get_datatype == $PDL_F;
    $BITPIX = -64 if $pdl->get_datatype == $PDL_D;
  }
  my $convert = sub { return $_[0] };# Default - do nothing
    $convert = sub {byte($_[0])}   if $BITPIX ==   8;
  $convert = sub {short($_[0])}  if $BITPIX ==  16;
  $convert = sub {long($_[0])}   if $BITPIX ==  32;
  $convert = sub {float($_[0])}  if $BITPIX == -32;
  $convert = sub {double($_[0])} if $BITPIX == -64;
  
  # Automatically figure output scaling
  
  my $bzero = 0; my $bscale = 1;
  if ($BITPIX>0) {
    my $min = $pdl->min;
    my $max = $pdl->max;
    my ($dmin,$dmax) = (0, 2**8-1)     if $BITPIX == 8;
    ($dmin,$dmax) = (-2**15, 2**15-1)  if $BITPIX == 16;
    ($dmin,$dmax) = (-2**31, 2**31-1)  if $BITPIX == 32;
    
    if ($min<$dmin || $max>$dmax) {
      $bzero = $min - $dmin;
      $max -= $bzero;
      $bscale = $max/$dmax if $max>$dmax;
    }
    print "BSCALE = $bscale &&  BZERO = $bzero\n" if $PDL::verbose;
  }
  
  
  ## Open file & prepare to write binary info
  open(FITS, "$file") || barf "Unable to create FITS file $file\n";
  binmode FITS;
  
  ##############################
  ## Check header and prepare to write it out
  
  my($h) = $pdl->gethdr();
  if(defined($h) && 
     ( (defined (tied %$h)) && 
       (UNIVERSAL::isa(tied %$h,"Astro::FITS::Header")))
     ){
    my $k;
    
    ##n############################
    ## Tied-hash code -- I'm too lazy to incorporate this into KGB's
    ## direct hash handler below, so I've more or less just copied and
    ## pasted with some translation.  --CED
    ##
    my $hdr = tied %$h;
    
    #
    # Put advertising comment in the SIMPLE field
    #n
    $h->{SIMPLE} = 'T';
    my(@a) = $hdr->itembyname('SIMPLE');
    $a[0]->comment('Created with PDL (http://pdl.perl.org)');
    
    # and register it as a LOGICAL rather than a string
    $a[0]->type('LOGICAL');
    
    
    #
    # Use tied interface to set all the keywords.  Note that this
    # preserves existing per-line comments, only changing the values.
    #
    $h->{BITPIX} = $BITPIX;
    $h->{NAXIS} = $pdl->getndims;
    my $correction = 0;
    for $k(1..$h->{NAXIS}) { 
      $correction |= (exists $h->{"NAXIS$k"} and 
                      $h->{"NAXIS$k"} != $pdl->dim($k-1)
                      );
      $h->{"NAXIS$k"} = $pdl->getdim($k-1); 
    }
    carp("Warning: wfits corrected dimensions of FITS header") 
      if($correction);
    
    $h->{BUNIT} = "Data Value" unless exists $h->{BUNIT};
    $h->{BSCALE} = $bscale if($bscale != 1);
    $h->{BZERO}  = $bzero  if($bzero  != 0);
    
    
    if ( $pdl->badflag() ) {
      if ( $BITPIX > 0 ) { my $a = &$convert(pdl(0.0)); $h->{BLANK} = $a->badvalue(); }
      else               { delete $h->{BLANK}; }
    }
    
    #
    # Use object interface to sort the lines. This is complicated by
    # the need for an arbitrary number of NAXIS<n> lines in the middle
    # of the sorting.  Keywords with a trailing '1' in the sorted-order
    # list get looped over.
    my($kk) = 0; 
    for $k(0..$#PDL::IO::FITS::wfits_keyword_order) {
      my($kn) = 0;
      
      my @index;
      do {            # Loop over numericised keywords (e.g. NAXIS1)
        
        my $kw = $PDL::IO::FITS::wfits_keyword_order[$k]; # $kw get keyword
        $kw .= (++$kn) if( $kw =~ s/\d$//);               # NAXIS1 -> NAXIS<n>
        @index = $hdr->index($kw);
        
        if(defined $index[0]) {
          $hdr->insert($kk, $hdr->remove($index[0])) 
            unless ($index[0] == $kk) ;
          $kk++;
        }
      } while((defined $index[0]) && $kn);
    }
    
    #
    # Delete the END card if necessary (for later addition at the end)
    #
    $hdr->removebyname('END');
    
    #
    # Make sure that the HISTORY lines all come at the end
    # 
    my @hindex = $hdr->index('HISTORY');
    for $k(0..$#hindex) {
      $hdr->insert(-1-$k, $hdr->remove($hindex[-1-$k]));
    }
    
    #
    # Make sure the last card is an END
    #
    $hdr->insert(scalar($hdr->cards),
                 new Astro::FITS::Header::Item(Keyword=>'END'));
    
    #
    # Write out all the cards, and note how many bytes for later padding.
    #
    my $s = join("",$hdr->cards);
    
    print FITS $s;
    $nbytes = length $s;
  } else {
    #
    # Legacy emitter (note different advertisement in the SIMPLE
    # comment, for debugging!)
    #
    
    printf FITS "%-80s", "SIMPLE  =                    T / PDL::IO::FITS::wfits (http://pdl.perl.org)";
    
    $nbytes = 80; # Number of bytes written so far
    
    # Write FITS header
    
    %hdr = ();
    if (defined($h)) {
      for (keys %$h) { $hdr{uc $_} = $$h{$_} } # Copy (ensuring keynames are uppercase)
    }
    
    delete $hdr{SIMPLE}; delete $hdr{'END'};
    
    $hdr{BITPIX} =  $BITPIX;
    $hdr{BUNIT} = "Data Value" unless exists $hdr{BUNIT};
    wheader('BITPIX');
    
    $ndims = $pdl->getndims; # Dimensions of data array
    $hdr{NAXIS}  = $ndims;
    wheader('NAXIS');
    for $k (1..$ndims) { $hdr{"NAXIS$k"} = $pdl->getdim($k-1) }
    for $k (1..$ndims) { wheader("NAXIS$k") }
    
    if ($bscale != 1 || $bzero  != 0) {
      $hdr{BSCALE} =  $bscale;
      $hdr{BZERO}  =  $bzero;
      wheader('BSCALE');
      wheader('BZERO');
    }
    wheader('BUNIT');
    
    # IF badflag is set
    #   and BITPIX > 0 - ensure the header contains the BLANK keyword
    #                    (make sure it's for the correct type)
    #   otherwise      - make sure the BLANK keyword is removed
    if ( $pdl->badflag() ) {
      if ( $BITPIX > 0 ) { my $a = &$convert(pdl(0.0)); $hdr{BLANK} = $a->badvalue(); }
      else               { delete $hdr{BLANK}; }
    }
    
    for $k (sort keys %hdr) { wheader($k) unless $k =~ m/HISTORY/}
    wheader('HISTORY'); # Make sure that HISTORY entries come last.
    printf FITS "%-80s", "END"; $nbytes += 80;
  }
  
  
  #
  # Pad the header to a legal value and write the rest of the FITS file.
  #
  $nbytes %= 2880;
  print FITS " "x(2880-$nbytes) if $nbytes != 0; # Fill up HDU
  
  # Decide how to byte swap - note does not quite work yet. Needs hack
  # to IO.xs
  
  my $bswap = sub {};     # Null routine
  if ( !isbigendian() ) { # Need to set a byte swap routine
    $bswap = \&bswap2 if $BITPIX==16;
    $bswap = \&bswap4 if $BITPIX==32 || $BITPIX==-32;
    $bswap = \&bswap8 if $BITPIX==-64;
  }
  
  # Write FITS data
  
  my $p1d = $pdl->clump(-1); # Data as 1D stream
  
  $off = 0;
  $sz  = PDL::Core::howbig(&$convert($p1d->slice('0:0'))->get_datatype);
  
  $nbytes = $p1d->getdim(0) * $sz;
  
  # Transfer data in blocks (because might need to byte swap)
  # Buffer is also type converted on the fly
  
  my $BUFFSZ = 360*2880; # = ~1Mb - must be multiple of 2880
  my $tmp;
  
  if ( $pdl->badflag() and $BITPIX < 0 and $PDL::Bad::UseNaN == 0 ) {
    # just print up a message - conversion is actually done in the loop
    print "Converting PDL bad value to NaN\n" if $PDL::verbose;
  }
  
  while ($nbytes - $off > $BUFFSZ) {
    
    # Data to be transferred
    
    $buff = &$convert( ($p1d->slice( ($off/$sz).":". (($off+$BUFFSZ)/$sz-1))
                        -$bzero)/$bscale );
    
    # if there are bad values present, and output type is floating-point,
    # convert the bad values to NaN's.  We can ignore integer types, since
    # we have set the BLANK keyword
    #
    if ( $pdl->badflag() and $BITPIX < 0 and $PDL::Bad::UseNaN == 0 ) {
      $buff->inplace->setbadtonan();
    }
    
    &$bswap($buff); print FITS ${$buff->get_dataref};
  $off += $BUFFSZ;
}
$buff = &$convert( ($p1d->slice($off/$sz.":-1") - $bzero)/$bscale );

if ( $pdl->badflag() and $BITPIX < 0 and $PDL::Bad::UseNaN == 0 ) {
  $buff->inplace->setbadtonan();
}

&$bswap($buff); print  FITS ${$buff->get_dataref};
print FITS " "x(($BUFFSZ - $buff->getdim(0) * $sz)%2880);  # Fill HDU

close(FITS);

1;}

sub wheader {     # Local utility routine of wfits()
  my $k = shift;
  
  if ($k =~ m/HISTORY/) {
    return unless ref($hdr{$k}) eq 'ARRAY';
    foreach my $line (@{$hdr{$k}}) {
      printf FITS "HISTORY %-72s", substr($line,0,72);
      $nbytes += 80;
    }
    delete $hdr{$k};
  } else {
    # Check that we are dealing with a scalar value in the header
    # Need to make sure that the header does not include PDLs or
    # other structures. Return unless $hdr{$k} is a scalar.
    my($hdrk) = $hdr{$k};
    
    if(ref $hdrk eq 'ARRAY') {
      $hdrk = join("\n",@$hdrk);
    }
    
    return unless not ref($hdrk);
    
    if ($hdrk eq "") {
      printf FITS "%-80s", substr($k,0,8);
    } else {
      printf FITS "%-8s= ", substr($k,0,8);
      
      my $com = ( ref $hdr{COMMENT} eq 'HASH' ) ? $hdr{COMMENT}{$k} : undef;
      
      
      if ($hdrk =~ /^ *([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))? *$/) { # Number?
        my $cl=60-($com ? 2 : 0);
        my $end=' ' x $cl;
        $end =' /'. $com if($com);
        printf FITS "%20s%-50s", substr($hdrk,0,20),
        substr($end, 0, 50);
      } elsif ($hdrk eq 'F' or $hdrk eq 'T') {
        # Logical flags ?
        printf FITS "%20s", $hdrk;
        my $end=' ' x 50;
        $end =' /'.$com if($com);
        printf FITS "%-50s", $end;
      } else {
        # Handle strings, truncating as necessary
        # (doesn't do multicard strings like Astro::FITS::Header does)
        
        # Convert single quotes to doubled single quotes 
        # (per FITS standard)
        my($st) = $hdrk;
        $st =~ s/\'/\'\'/g;
        
        my $sl=length($st)+2;
        my $cl=70-$sl-($com ? 2 : 0);
        print FITS "'$st'";
        
        if (defined $com) {
          printf FITS " /%-$ {cl}s", substr($com, 0, $cl);
        } else {
          printf FITS "%-$ {cl}s", ' ' x $cl;
        }
      }
    }
    $nbytes += 80; delete $hdr{$k};
  }
  delete $hdr{COMMENT}{$k} if(ref $hdr{COMMENT} eq 'HASH');
  1;
}

1;
