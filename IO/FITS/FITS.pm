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
ASCII Table support is planned, as are the HEASARC bintable extensions that
are recommended in the 1999 FITS standard.

Table support is based on hashes and named columns, rather than the
less convenient (but slightly more congruent) technique of perl lists
of numbered columns.

The principle interface routines are C<rfits> and C<wfits>, for
reading and writing respectively.  FITS headers are returned as perl
hashes or (if the module is present) Astro::FITS::Header objects that
are tied to perl hashes.  Astro::FITS::Header objects provide
convenient access through the tied hash interface, but also allow you
to control the card structure in more detail using a separate method
interface; see the L<Astro::FITS::Header|Astro::FITS::Header>
documentation for details.

=head1 AUTHOR

Copyright (C) Karl Glazebrook, Craig DeForest, and Doug Burke, 1997-2004.
There is no warranty.  You are allowed to redistribute and/or modify
this software under certain conditions.  For details, see the file
COPYING in the PDL distribution.  If this file is separated from the 
PDL distribution, the copyright notice should be pasted into in this file.

=head1 FUNCTIONS

=cut

use strict;

BEGIN {

  package PDL::IO::FITS;

  $PDL::IO::FITS::VERSION = 0.9; # Will be 1.0 when ascii table read/write works.

  our @EXPORT_OK = qw( rfits rfitshdr wfits );
  our %EXPORT_TAGS = (Func=>[@EXPORT_OK]);
  our @ISA = ('PDL::Exporter');

  use PDL::Core;
  use PDL::Config;
  use PDL::IO::Misc;
  use PDL::Exporter;
  use PDL::Primitive;
  use PDL::Types;
  use PDL::Options;
  use PDL::Bad;
  use PDL::NiceSlice;
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

  unless($PDL::Astro_FITS_Header) {
    unless($ENV{"PDL_FITS_LEGACY"} || $PDL::Config{FITS_LEGACY}) {
      print(STDERR "\n\nWARNING: Can't find the Astro::FITS::Header module, limiting FITS support.\n\n  PDL will use the deprecated legacy perl hash handling code but will not\n  properly support tables, FITS extensions, or COMMENT cards. You really\n  ought to install the Astro::FITS::Header module, available from\n  'http://www.cpan.org'.  (You can also get rid of this message by setting\n  the environment variable 'PDL_FITS_LEGACY' or the global PDL config value (in perldl.conf)\n  \$PDL::Config{FITS_LEGACY} to 1.\n\n");
    }
  }
}

package PDL::IO::FITS;

## declare subroutines 

sub _wfits_nullhdu ($);
sub _wfits_table ($$$);

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

  $hdr = rfits('file.fits',{data=>0});  # Options hash changes behavior

In list context, C<rfits> reads the primary image and all possible
extensions, returning them in the same order that they occurred in the
file.  In scalar context, the default is to read the primary HDU. One
can read other HDU's by using the [n] syntax, the second one is [1].
Currently recognized extensions are IMAGE and BINTABLE.  (See the
addendum on EXTENSIONS for details).

C<rfits> accepts several options that may be passed in as a hash ref
if desired:

=over 3

=item bscale (default=1)

Determines whether the data are linearly scaled using the BSCALE/BZERO keywords
in the FITS header.  To read in the exact data values in the file, set this
to 0. 

=item data (default=1)

Determines whether to read the data, or just the header.  If you set this to 
0, you will get back the FITS header rather than the data themselves.  (Note
that the header is normally returned as the C<hdr> field of the returned PDL;
this causes it to be returned as a hash ref directly.)

=item hdrcpy (default=0)

Determines whether the L<hdrcpy|PDL::Core/hdrcpy> flag is set in the returned
PDL.  Setting the flag will cause an explicit deep copy of the header whenever
you use the returned PDL in an arithmetic or slicing operation.  That is useful
in many circumstances but also causes a hit in speed.  

=back

FITS image headers are stored in the output PDL and can be retrieved
with L<hdr|PDL::Core/hdr> or L<gethdr|PDL::Core/gethdr>.  The
L<hdrcpy|PDL::Core/hdrcpy> flag of the PDL is set so that the header
is copied to derived piddles by default.  (This is inefficient if you
are planning to do lots of small operations on the data; clear
the flag with "->hcpy(0)" or via the options hash if that's the case.)

The header is a hash whose keys are the keywords in the FITS header.
If you have the "Astro::FITS::Header" module installed, the header is
actually a tied hash to a FITS header object, which can give you
more control over card order, comment fields, and variable types.
(see L<Astro::FITS::Header> for details).

The header keywords are converted to I<uppercase> per the FITS
standard.  Access is case-insensitive on the perl side, provided that
Astro::FITS::Header is installed. 

If Astro::FITS::Header is not installed, then a built-in legacy parser
is used to generate the header hash.  Keyword-associated comments in
the headers are stored under the hash key
C<< <keyword>_COMMENT> >>.  All HISTORY cards in the header are
collected into a single multiline string stored in the C<HISTORY> key.
All COMMENT cards are similarly collected under the C<COMMENT> key.

=head3 BSCALE/BZERO

If the BSCALE and/or BZERO keywords are set, they are applied to the
image before it is returned.  The returned PDL is promoted as
necessary to contain the multiplied values, and the BSCALE and BZERO
keywords are deleted from the header for clarity.  If you don't want
this type of processing, set 'bscale=>0' in the options hash.

=head3 EXTENSIONS

Sometimes a FITS file contains only extensions and a stub header in
the first header/data unit ("primary HDU").  In scalar context, you
normally only get back the primary HDU -- but in this special case,
you get back the first extension HDU.  You can force a read of the
primary HDU by adding a '[0]' suffix to the file name.

=head3 BINTABLE EXTENSIONS

Binary tables are handled. Currently only the following PDL
datatypes are supported: byte, short, ushort, long, float, and
double. At present ushort() data is written as a long rather than
as a short with TSCAL/ZERO; this may change.

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
use that to preserve the original column order or access the table at a low
level, if you like. 

Scaling and zero-point adjustment are performed as with BSCALE/BZERO:
the appropriate keywords are deleted from the as-returned header.  To avoid
this behavior, set 'bscale=>0' in the options hash.  

As appropriate, TSCAL/ZERO and TUNIT are copied into each column-PDL's
header as BSCALE/BZERO and BUNIT.  

The main hash also contains the element 'tbl', which is set
to 'binary' to distinguish it from an ASCII table.

Because different columns in the table might have identical names in a
FITS file, the binary table reader practices collision avoidance.  If
you have multiple columns named "FOO", then the first one encountered
(numerically) gets the name "FOO", the next one gets "FOO_1", and the
next "FOO_2", etc.  The appropriate TTYPEn fields in the header are
changed to match the renamed column fields.

Columns with no name are assigned the name "COL_<n>", where <n> starts
at 1 and increments for each no-name column found.


=for bad

If a FITS file contains the C<BLANK> keyword (and has C<BITPIX E<gt> 0>), 
the piddle will have its bad flag set, and those elements which equal the
C<BLANK> value will be set bad.  For C<BITPIX E<lt> 0>, any NaN's are
converted to bad (if necessary).


=head2 rfitshdr()

=for ref

Read only the header of a FITS file or an extension within it.

This is syntactic sugar for the C<data=E<gt>0> option to L<rfits|/rfits()>.

See L<rfits|/rfits()> for details on header handling.  rfitshdr() runs 
the same code to read the header, but returns it rather than 
reading in a data structure as well.

=cut

our $rfits_options = new PDL::Options( { bscale=>1, data=>1, hdrcpy=>0 } );

sub PDL::rfitshdr {
  my $class = shift;
  my $file = shift;
  my $u_opt = ifhref(shift);
  $u_opt->{data} = 0;
  PDL::rfits($class,$file,$u_opt);
}

sub PDL::rfits {
  
  my $class = shift;
  
  barf 'Usage: $a = rfits($file)  -or-   $a = PDL->rfits($file)' if (@_ < 1 || @_ > 2);
  
  my $file = shift; 
  
  my $u_opt = ifhref(shift);
  my $opt = $rfits_options->options($u_opt);
  
  my($nbytes, $line, $name, $rest, $size, $i, $bscale, $bzero, $extnum);

  $nbytes = 0;

  # Modification 02/04/2005 - JB. Earlier version stripped the extension
  # indicator which cancelled the check for empty primary data array at the end.
  my $explicit_extension = ($file =~ m/\[\d+\]$/ ? 1 : 0);
  $extnum = ( ($file =~ s/\[(\d+)\]$//) ? $1 : 0 );
  
  $file = "gunzip -c $file |" if $file =~ /\.gz$/;    # Handle compression
  $file = "uncompress -c $file |" if $file =~ /\.Z$/;
  
  my $fh = IO::File->new( $file )
      or barf "FITS file $file not found";
  binmode $fh;

  my @extensions;  # This accumulates the list in list context...
  my $currentext=0;
  my $pdl;

 hdu:{do {                     # Runs over extensions, in list context
   my $ext_type = 'IMAGE';     # Gets the type of XTENSION if one is detected.
   my $foo={};       # To go in pdl
   my @history=();
   my @cards = ();
   
   $pdl = $class->new;
 

   # If $opt->{data} is false, then the reading routines leave the
   # file alone, so the file pointer is left at the end of the last
   # header.  Skip over the unread data to the next extension...
   
   if( wantarray and !$opt->{data} and @extensions) {
       while( $fh->read($line,80) && ($line !~ /^XTENSION=/) && !$fh->eof() ) {
	   $fh->read($line,2880-80);
       };
       
       return @extensions 
	   if($fh->eof());

   } else {
       my $ct = $fh->read($line,80);
       barf "file $file is not in FITS-format:\n$line\n"
	   if( $nbytes==0 && ($line !~ /^SIMPLE  = +T/));
       last hdu if($fh->eof() || !$ct);
   }

   $nbytes = 80; # Number of bytes read from this extension (1 line so far)
 
   if($line =~ /^XTENSION= \'(\w+)\s*\'/) {
     $ext_type = $1;
   } elsif( @extensions ) {

     print "Warning: expected XTENSION, found '$line'.  Exiting.\n"
       if($PDL::verbose);
     last hdu;
   }

   push(@cards,$line)  if($PDL::Astro_FITS_Header);
   
   #
   # If we are in scalar context, skip to the desired extension
   # number.  [This implementation is really slow since we have
   # to read the whole file.  Someone Really Ought To rework it to
   # read individual headers and skip forward an extension at a
   # a time with seek() calls. ]
   #     --CD
   #
 
   if(!wantarray and $currentext != $extnum) {

    skipper: while(1) {
      # Move to next record
      $nbytes += $fh->read($line,2880-80);
      barf "Unexpected end of FITS file\n" if $fh->eof();
      # Read start of next record
      $nbytes += $fh->read($line,80);
      barf "Unexpected end of FITS file\n" if $fh->eof();
      # Check if we have found the new extension
      # if not move on

      $currentext++ if  $line =~ /^XTENSION\= \'(\w+)\s*\'/;
      if ($currentext == $extnum) {
	$ext_type = $1;
	last skipper;
      }
    }
   } # End of skipping to desired extension
   
   #
   # Snarf up the found header, and parse it if Astro::FITS::Header
   # does not exist.
   # 

   if($PDL::Astro_FITS_Header) { 

     ## Astro::FITS::Header parsing.  Snarf lines to the END card,
     ## and pass them to Astro::FITS::Header.

     do {
       $nbytes += $fh->read($line, 80);
       push(@cards,$line);
     } while(!$fh->eof() && $line !~ m/^END(\s|\000)/);

     $nbytes += $fh->read(my $dummy, 2879 - ($nbytes-1)%2880);

     my($hdr) = Astro::FITS::Header->new(Cards => \@cards);
     my(%hdrhash);
     tie %hdrhash,"Astro::FITS::Header",$hdr;
     $foo = \%hdrhash;
   
   } else {
     
     ## Legacy (straight header-to-hash-ref) parsing.  
     ## Deprecated but preserved.
     
     hdr_legacy: { do {
       no strict 'refs';
       # skip if the first eight characters are ' '
       # - as seen in headers from the DSS at STScI
       if (substr($line,0,8) ne " " x 8) { # If non-blank
       
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
       } # non-blank
       last hdr_legacy if ((defined $name) && $name eq "END");
       $nbytes += $fh->read($line, 80);
     } while(!$fh->eof()); }

     # Clean up HISTORY card
     $$foo{"HISTORY"} = \@history if $#history >= 0;
   
     # Step to end of header block in file
     my $skip = 2879 - ($nbytes-1)%2880;
     $fh->read(my $dummy, $skip) if $skip; 
     $nbytes += $skip;

   } # End of legacy header parsing
 

   
   ##############################
   # Special case: if the file only contains 
   # extensions then read the first extension in scalar context,
   # instead of the null zeroth extension.
   #
   if( !(defined $foo->{XTENSION})  # Primary header
       and $foo->{NAXIS} == 0       # No data
       and !wantarray               # Scalar context
       and !$explicit_extension     # No HDU specifier
       ) {
     print "rfits: Skipping null primary HDU (use [0] to force read of primary)...\n" 
       if($PDL::verbose);
     return PDL::rfits($class,$file.'[1]',$opt);
   }
 

   ##########
   # If we don't want data, return the header from the HDU.  Likewise, 
   # if NAXIS is 0 then there are no data, so return the header instead.
   if( ! $opt->{data} || $foo->{NAXIS}==0 ) {
     # If we're NOT reading data, then return the header instead of the
     # image.

     $pdl = $foo;

   } else {
     
     ##########
     # Switch based on extension type to do the dirty work of reading
     # the data.  Handlers are listed in the _Extension patch-panel.
     
     if (ref $PDL::IO::FITS::_Extension->{$ext_type} ) {
       
       # Pass $pdl into the extension reader for easier use -- but
       # it just gets overwritten (and disappears) if ignored.
       
       $pdl = &{$PDL::IO::FITS::_Extension->{$ext_type}}($fh,$foo,$opt,$pdl);
       
     } else {

       print STDERR "rfits: Ignoring unknown extension '$ext_type'...\n"
	 if($PDL::verbose || $PDL::debug);
       
       $pdl = undef;

     }
   }

   #
   # Note -- $pdl isn't necessarily a PDL.  It's only a $pdl if
   # the extension was an IMAGE.
   #
   push(@extensions,$pdl) if(wantarray);
   $currentext++;
 
  } while( wantarray && !$fh->eof() );}  # Repeat if we are in list context
   
 $fh->close;
  
 return @extensions if(wantarray);
 return $pdl;
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
      IMAGE    => \&_rfits_image
    , BINTABLE => \&_rfits_bintable
  };


       
##############################
#
# IMAGE extension -- this is also the default reader.

sub _rfits_image($$$$) {
  print "Reading IMAGE data...\n" if($PDL::verbose);
  my $fh  = shift; # file handle to read from
  my $foo = shift;  # $foo contains the pre-read header
  my $opt = shift;  # $opt contains the option hash
  my $pdl = shift;  # $pdl contains a pre-blessed virgin PDL

  # Setup piddle structure
  
  $pdl->set_datatype($PDL_B)    if $$foo{"BITPIX"} ==   8;
  $pdl->set_datatype($PDL_S)    if $$foo{"BITPIX"} ==  16;
  $pdl->set_datatype($PDL_L)    if $$foo{"BITPIX"} ==  32;
  $pdl->set_datatype($PDL_F)    if $$foo{"BITPIX"} == -32;
  $pdl->set_datatype($PDL_D)    if $$foo{"BITPIX"} == -64;
  
  my @dims; # Store the dimenions 1..N, compute total number of pixels
  my $i = 1;
  my $size = 1; 
  my $bscale;
  my $bzero;

##second part of the conditional guards against a poorly-written hdr.
  while(defined( $$foo{"NAXIS$i"} ) && $i <= $$foo{"NAXIS"}) {
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
  $fh->read( $$dref, $rdct );
  $fh->read( my $dummy, 2880 - (($rdct-1) % 2880) - 1 );
  $pdl->upd_data();

  if (!isbigendian() ) {
    # Need to byte swap on little endian machines
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
      $tmp = $pdl;
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

  $pdl->hdrcpy($opt->{hdrcpy});

  return $pdl;
} 

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
	     my $s =  unpack( "B".$n,  substr(${$strptr}, 0, int(($n+7)/8),''));
	   $s =~ tr/[01]/[\000\001]/;
	   substr( ${$pdl->get_dataref},  $n * $row,  length($s)) = $s;
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
               $$szptr += $rowlen;
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

sub _rfits_bintable ($$$$) {
  my $fh  = shift;
  my $hdr = shift;
  my $opt = shift;
  ##shift;  ### (ignore $pdl argument)

  $tmp::hdr = $hdr;
  
  print STDERR "Warning: BINTABLE extension should have BITPIX=8, found ".$hdr->{BITPIX}.".  Winging it...\n" unless($hdr->{BITPIX} == 8);
  
  
  
  ### Allocate the main table hash
  my $tbl = {};    # Table is indexed by name
  $tbl->{hdr} = $hdr;
  $tbl->{tbl} = 'binary';

  my $tmp = [];    # Temporary space is indexed by col. no.
  
  
  ### Allocate all the columns of the table, checking for consistency
  ### and name duplication.
  
  barf "Binary extension has no fields (TFIELDS=0)" unless($hdr->{TFIELDS});
  my $rowlen = 0;
  
  for my $i(1..$hdr->{TFIELDS}) {
    my $iter;
    my $name = $tmp->[$i]->{name} = $hdr->{"TTYPE$i"} || "COL";
    
    ### Allocate some temp space for dealing with this column
    my $tmpcol = $tmp->[$i] = {};
    
    ### Check for duplicate name and change accordingly...
    while( defined(  $tbl->{ $name } ) || ($name eq "COL") ) {
      $iter++;
      $name = ($hdr->{"TTYPE$i"} )."_$iter";
    }
    
    # (Check avoids scrozzling comment fields unnecessarily)
    $hdr->{"TTYPE$i"} = $name unless($hdr->{"TTYPE$i"} eq $name);
    $tmpcol->{name} = $name;

    if( ($hdr->{"TFORM$i"}) =~ m/(\d*)(.)(.*)/ ) {
      ($tmpcol->{rpt},  $tmpcol->{type},  $tmpcol->{extra}) = ($1,$2,$3);
      # added by DJB 03.18/04 - works for my data file but is it correct?
      $tmpcol->{rpt} ||= 1;
    } else {
      barf "Couldn't parse BINTABLE form '"
        . $hdr->{"TFORM$i"}
      . "' for column $i ("
        . $hdr->{"TTYPE$i"}
      . ")\n" if($hdr->{"TFORM$i"});
      barf "BINTABLE header is missing a crucial field, TFORM$i.  I give up.\n";
    }

    # "A bit array consists of an integral number of bytes with trailing bits zero"
    $tmpcol->{rpt} = PDL::ceil($tmpcol->{rpt}/8) if ($tmpcol->{type} eq 'X');

    $tmpcol->{handler} =  # sic - assignment
      $PDL::IO::FITS_bintable_handlers->{ $tmpcol->{type} }
    or 
      barf "Unknown type ".$hdr->{"TFORM$i"}." in BINTABLE column $i "."("
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
                                    , $hdr->{NAXIS2}||1
                                    );
      $rowlen += PDL::Core::howbig($foo) * $tmpcol->{rpt};
    }
    
    print "Prefrobnicated col. $i (".$hdr->{"TTYPE$i"}.")\ttype is ".$hdr->{"TFORM$i"}."\t length is now $rowlen\n" if($PDL::debug);
    
    
  }  ### End of prefrobnication loop...
  

  barf "Calculated row length is $rowlen, hdr claims ".$hdr->{NAXIS1}
       . ".  Giving up.  (Set \$PDL::debug for more detailed info)\n"
    if($rowlen != $hdr->{NAXIS1});
  
  ### Snarf up the whole extension, and pad to 2880 bytes...
  my ($rawtable, $n);
  $n = $hdr->{NAXIS1} * $hdr->{NAXIS2} + $hdr->{PCOUNT};
  $n = ($n-1)+2880 - (($n-1) % 2880);
  print "Reading $n bytes of table data....\n"
    if($PDL::verbose);
  $fh->read($rawtable, $n);

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

    } # End of TFIELDS loop

    if(length($rawtable) ne $prelen - $hdr->{NAXIS1}) {
      barf "Something got screwed up -- expected a length of $prelen - $hdr->{NAXIS1}, got ".length($rawtable).".  Giving up.\n";
    }
  } # End of NAXIS2 loop

 #
 # Note: the above code tickles a bug in most versions of the emacs 
 # prettyprinter.  The following "for my $i..." should be indented
 # two spaces.
 #

  ### Postfrobnicate the columns.
  for my $i(1..$hdr->{TFIELDS}) { # Postfrobnication loop
    my $tmpcol = $tmp->[$i];
    my $post = $tmpcol->{handler}->[3];
    
    if(ref $post eq 'CODE') {
      # Do postprocessing on all special types
      
      $tbl->{$tmpcol->{name}} = &$post($tmpcol->{data}, $i, $hdr, $opt);
      
    } elsif( (ref ($tmpcol->{data})) eq 'PDL' ) {
      # Do standard PDL-type postprocessing
      
      ## Is this call to upd_data necessary?
      ## I think not. (reinstate if there are bugs)
      # $tmpcol->{data}->upd_data;
      
      
      # Do swapping as necessary
      unless( isbigendian() ) {
	if(    $post == 2 ) { bswap2($tmpcol->{data}); }
	elsif( $post == 4 ) { bswap4($tmpcol->{data}); }
	elsif( $post == 8 ) { bswap8($tmpcol->{data}); }
	elsif( $post != 1 ) {
	  print STDERR "Unknown swapsize $post for column $i ("
	    . $tmpcol->{name} . ")!  This is a bug.  Winging it.\n";
	}
      }
    
      # Apply scaling and badval keys, which are illegal for A, L, and X
      # types but legal for anyone else.  (A shouldn't be here, L and X 
      # might be)
      
      if($opt->{bscale}) {
	my $tzero = $hdr->{"TZERO$i"} || 0.0;
	my $tscal = $hdr->{"TSCAL$i"} || 1.0;
	
	# The $valid_<foo> flags let us avoid unnecessary arithmetic.
	my $valid_tzero = ($tzero != 0.0);
	my $valid_tscal = ($tscal != 1.0);
	
	if ( $valid_tzero or $valid_tscal ) {
	  if ( $tmpcol->{type} =~ m/[ALX]/i ) {
	    
	    print STDERR "Ignoring illegal TSCAL/TZERO keywords for col $i (" .
	      $tmpcol->{name} . "); type is $tmpcol->{type})\n";
	    
	  } else { # Not an illegal type -- do the scaling
	    
	    # (Normal execution path) 
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
	    
	    # Figure out the type by scaling the single element.
	    $tmp = ($tmp - $tzero) * $tscal;
	    
	    # Convert the whole PDL as necessary for the scaling.
	    $tmpcol->{data} = $pdl->convert($tmp->type) 
	      if($tmp->get_datatype != $pdl->get_datatype);
	    
	    # Do the scaling.
	    $tmpcol->{data} -= $tzero;
	    $tmpcol->{data} *= $tscal;

	  } # End of legal-type conditional  
	} # End of valid_<foo> conditional

	delete $hdr->{"TZERO$i"};
	delete $hdr->{"TSCAL$i"};
	
      } else { # $opt->{bscale} is zero; don't scale.
	       # Instead, copy factors into individual column headers.
	my %foo = ("TZERO$i"=>"BZERO", 
		   "TSCAL$i"=>"BSCALE", 
		   "TUNIT$i"=>"BUNIT");
	for my $a(keys %foo) {
	  $tmpcol->{data}->hdr->{$foo{$a}} = $hdr->{$a}
	  if( defined($hdr->{$a}) );
	}
      } # End of bscale checking...

      # Copy the PDL out to the table itself.
      $tmpcol->{data}->hdrcpy(1);
      if($hdr->{NAXIS2} > 0 && $tmpcol->{rpt}>0) {
	$tbl->{$tmpcol->{name}} = 
	  ( ( $tmpcol->{data}->dim(0) == 1 ) 
	    ? $tmpcol->{data}->slice("(0)") 
	    : $tmpcol->{data}->xchg(0,1)
	    );
      }
      
      # End of PDL postfrobnication case
    } elsif(defined $post) {
      
      print STDERR "Postfrobnication bug detected in column $i ("
	. $tmpcol->{name}. ").  Winging it.\n";
      
    }
  } # End of postfrobnication loop over columns

  ### Done!
  return $tbl;
}

=head2 wfits()

=for ref

Simple PDL FITS writer

=for example

  wfits $pdl, 'filename.fits', [$BITPIX];
  wfits $hash, 'filename.fits', [$OPTIONS];
  $pdl->wfits('foo.fits',-32);

Suffix magic:

  # Automatically compress through pipe to gzip
  wfits $pdl, 'filename.fits.gz';
  # Automatically compress through pipe to compress 
  wfits $pdl, 'filename.fits.Z';  

=over 3

=item * Ordinary (PDL) data handling: 

If the first argument is a PDL, then the PDL is written out as an
ordinary FITS file with a single Header/Data Unit of data.

$BITPIX is then optional and coerces the output data type according to 
the standard FITS convention for the BITPIX field (with positive 
values representing integer types and negative values representing
floating-point types).

If C<$pdl> has a FITS header attached to it (actually, any hash that
contains a C<< SIMPLE=>T >> keyword), then that FITS header is written
out to the file.  The image dimension tags are adjusted to the actual
dataset.  If there's a mismatch between the dimensions of the data and
the dimensions in the FITS header, then the header gets corrected and
a warning is printed.

If C<$pdl> is a slice of another PDL with a FITS header already
present (and header copying enabled), then you must be careful.
C<wfits> will remove any extraneous C<NAXISn> keywords (per the FITS
standard), and also remove the other keywords associated with that
axis: C<CTYPEn>, C<CRPIXn>, C<CRVALn>, C<CDELTn>, and C<CROTAn>.  This
may cause confusion if the slice is NOT out of the last dimension:
C<wfits($a(:,(0),:),'file.fits');> and you would be best off adjusting
the header yourself before calling C<wfits>.

=item * Table handling:

If you feed in a hash ref instead of a PDL, then the hash ref is
written out as a binary table extension.  The hash ref keys are
treated as column names, and their values are treated as the data to
be put in each column.

For numeric information, the hash values should contain PDLs.  The 0th
dim of the PDL runs across rows, and higher dims are written as
multi-value entries in the table (e.g. a 7x5 PDL will yield a single
named column with 7 rows and 5 numerical entries per row, in a binary
table).  ASCII tables only allow one entry per column in each row, so
if you plan to write an ASCII table then all of the values of C<$hash>
should have at most one dim.

All of the column dims must agree in the threading sense. That is to
say, the 0th dimension of all of the values of C<$hash> should be the
same (indicating that all columns have the same number of rows).  As
an exception, if the 0th dim of any of the values is 1, or if that
value is a PDL scalar (with 0 dims), then that value is "threaded"
over -- copied into all rows.

Data dimensions higher than 2 are not possible in ordinary FITS
tables, so dims higher than 1 are clumped.  (e.g. a 7x5x3 PDL will
yield a single named column with 7 rows and 15 numerical entries per
row).  If C<$PDL::Verbose> is set, this condition causes a warning message
to be printed.  [There is a multidim extension that is not yet
implemented but should be].

Non-PDL Perl scalars are treated as strings, even if they contain
numerical values.  For example, a list ref containing 7 values is
treated as 7 rows containing one string each.  There is no such thing
as a multi-string column in FITS tables, so any nonscalar values in
the list are stringified before being written.  For example, if you
pass in a perl list of 7 PDLs, each PDL will be stringified before
being written, just as if you printed it to the screen.  This is
probably not what you want -- you should use L<glue|glue> to connect 
the separate PDLs into a single one.  (e.g. C<$a-E<gt>glue(1,$b,$c)-E<gt>mv(1,0)>)

The column names are case-insensitive, but by convention the keys of
C<$hash> should normally be ALL CAPS, containing only digits, capital
letters, hyphens, and underscores.  If you include other characters,
then case is smashed to ALL CAPS, whitespace is converted to
underscores, and unrecognized characters are ignored -- so if you
include the key "Au Purity (%)", it will be written to the file as a
column that is named "AU_PURITY".  Since this is not guaranteed to 
produce unique column names, subsequent columns by the same name are
disambiguated by the addition of numbers.

Two special keys, 'hdr' and 'tbl', can contain meta-information about
the type of table you want to write.  You may override them by
including an C<$OPTIONS> hash with a 'hdr' and/or 'tbl' key. 

The 'tbl' key, if it exists, must contain either 'ASCII' or 'binary'
(case-insensitive), indicating whether to write an ascii or binary
table.  The default is binary. [ASCII table writing is planned but
does not yet exist].

You can specify the format of the table quite specifically with the
'hdr' key or option field.  If it exists, then the 'hdr' key should
contain fields appropriate to the table extension being used.  Any
field information that you don't specify will be filled in
automatically, so (for example) you can specify that a particular
column name goes in a particular position, but allow C<wfits> to
arrange the other columns in the usual alphabetical order into any
unused slots that you leave behind.  The C<TFORMn>, C<TFIELDS>,
C<PCOUNT>, C<GCOUNT>, C<NAXIS>, and C<NAXISn> keywords are ignored:
their values are calculated based on the hash that you supply.  Any
other fields are passed into the final FITS header verbatim.

As an example, the following

  $a = long(1,2,4);
  $b = double(1,2,4);
  wfits { 'COLA'=>$a, 'COLB'=>$b }, "table1.fits";

will create a binary FITS table called F<table1.fits> which
contains two columns called C<COLA> and C<COLB>. The order
of the columns is controlled by setting the C<TTYPEn>
keywords in the header array, so 

  $h = { 'TTYPE1'=>'Y', 'TTYPE2'=>'X' };
  wfits { 'X'=>$a, 'Y'=>$b, hdr=>$h }, "table2.fits";

creates F<table2.fits> where the first column is
called C<Y> and the second column is C<X>.

=item * multi-value handling

If you feed in a perl list rather than a PDL or a hash, then 
each element is written out as a separate HDU in the FITS file.  
Each element of the list must be a PDL or a hash. [This is not implemented
yet but should be soon!]

=item * DEVEL NOTES

ASCII tables are not yet handled but should be.

Binary tables currently only handle one vector (up to 1-D array) 
per table entry; the standard allows more, and should be fully implemented.
This means that PDL::Complex piddles currently can not be written to
disk.

Handling multidim arrays implies that perl multidim lists should also be
handled.

=back

=for bad

For integer types (ie C<BITPIX E<gt> 0>), the C<BLANK> keyword is set
to the bad value.  For floating-point types, the bad value is
converted to NaN (if necessary) before writing.

=cut

*wfits = \&PDL::wfits;

BEGIN {
  @PDL::IO::FITS::wfits_keyword_order = 
    ('SIMPLE','BITPIX','NAXIS','NAXIS1','BUNIT','BSCALE','BZERO');
  @PDL::IO::FITS::wfits_numbered_keywords =
    ('CTYPE','CRPIX','CRVAL','CDELT','CROTA');
}

# Until we do a rewrite these have to be file global since they
# are used by the wheader routine
my (%hdr, $nbytes);

# Local utility routine of wfits()
sub wheader ($$) {
    my $fh = shift;
    my $k = shift;
  
    if ($k =~ m/HISTORY/) {
	return unless ref($hdr{$k}) eq 'ARRAY';
	foreach my $line (@{$hdr{$k}}) {
	    $fh->printf( "HISTORY %-72s", substr($line,0,72) );
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
	    $fh->printf( "%-80s", substr($k,0,8) );
	} else {
	    $fh->printf( "%-8s= ", substr($k,0,8) );
      
	    my $com = ( ref $hdr{COMMENT} eq 'HASH' ) ?
		$hdr{COMMENT}{$k} : undef;
      
	    if ($hdrk =~ /^ *([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))? *$/) { # Number?
		my $cl=60-($com ? 2 : 0);
		my $end=' ' x $cl;
		$end =' /'. $com if($com);
		$fh->printf( "%20s%-50s", substr($hdrk,0,20),
			     substr($end, 0, 50) );
             } elsif ($hdrk eq 'F' or $hdrk eq 'T') {
		 # Logical flags ?
		 $fh->printf( "%20s", $hdrk );
		 my $end=' ' x 50;
		 $end =' /'.$com if($com);
		 $fh->printf( "%-50s", $end );
	     } else {
		 # Handle strings, truncating as necessary
		 # (doesn't do multicard strings like Astro::FITS::Header does)
        
		 # Convert single quotes to doubled single quotes 
		 # (per FITS standard)
		 my($st) = $hdrk;
		 $st =~ s/\'/\'\'/g;
        
		 my $sl=length($st)+2;
		 my $cl=70-$sl-($com ? 2 : 0);
		 $fh->print( "'$st'" );
        
		 if (defined $com) {
		     $fh->printf( " /%-$ {cl}s", substr($com, 0, $cl) );
		 } else {
		     $fh->printf( "%-$ {cl}s", ' ' x $cl );
		 }
	     }
	}
	$nbytes += 80; delete $hdr{$k};
    }
    delete $hdr{COMMENT}{$k} if ref $hdr{COMMENT} eq 'HASH';
    1;
}

# Write a PDL to a FITS format file
#
sub PDL::wfits {
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
  
  #### Figure output type

  unless( UNIVERSAL::isa($pdl,'PDL') ) {
      my $ref = ref($pdl) || "";
      if($ref eq 'HASH') {
	  my $fh = IO::File->new( $file )
	      or barf "Could not open $file\n";
	  _wfits_nullhdu($fh);
	  # default to binary table if none specified
	  my $table_type = exists $pdl->{tbl} ?
	      ($pdl->{tbl} =~ m/^a/i ? 'ascii' : 'binary') :
	      "binary";
	  _wfits_table($fh,$pdl,$table_type);
	  $fh->close;
	return;
      } else {
	  barf('wfits: multiple extensions not yet supported\n')
      }
  }

  ### Regular image writing.
  
  $BITPIX = "" unless defined $BITPIX;
  if ($BITPIX eq "") {
    $BITPIX =   8 if $pdl->get_datatype == $PDL_B;
    $BITPIX =  16 if $pdl->get_datatype == $PDL_S || $pdl->get_datatype == $PDL_US;
    $BITPIX =  32 if $pdl->get_datatype == $PDL_L;
    $BITPIX = -32 if $pdl->get_datatype == $PDL_F;
    $BITPIX = -64 if $pdl->get_datatype == $PDL_D;
  }
  my $convert = sub { return $_[0] }; # Default - do nothing
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
  my $fh = IO::File->new( $file )
      or barf "Unable to create FITS file $file\n";
  binmode $fh;
  
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
      if ( $BITPIX > 0 ) { my $a = &$convert(pdl(0.0));
			   $h->{BLANK} = $a->badvalue(); }
      else               { delete $h->{BLANK}; }
    }
    
    #
    # Use object interface to sort the lines. This is complicated by
    # the need for an arbitrary number of NAXIS<n> lines in the middle
    # of the sorting.  Keywords with a trailing '1' in the sorted-order
    # list get looped over.
    my($kk) = 0; 
    my(@removed_naxis) = ();
    for $k(0..$#PDL::IO::FITS::wfits_keyword_order) {
      my($kn) = 0;
      
      my @index;
      do {            # Loop over numericised keywords (e.g. NAXIS1)
        
        my $kw = $PDL::IO::FITS::wfits_keyword_order[$k]; # $kw get keyword
        $kw .= (++$kn) if( $kw =~ s/\d$//);               # NAXIS1 -> NAXIS<n>
        @index = $hdr->index($kw);
        
        if(defined $index[0]) {
	    if($kn <= $pdl->getndims){
		$hdr->insert($kk, $hdr->remove($index[0])) 
		    unless ($index[0] == $kk) ;
		$kk++;
	    } 
	    else{ #remove e.g. NAXIS3 from hdr if NAXIS==2
		$hdr->removebyname($kw);
		push(@removed_naxis,$kw);
	    }
	}
      } while((defined $index[0]) && $kn);
    }

    foreach my $naxis(@removed_naxis){
	$naxis =~ m/(\d)$/;
	my $n = $1;
	foreach my $kw(@PDL::IO::FITS::wfits_numbered_keywords){
	    $hdr->removebyname($kw . $n);
	}
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
                 Astro::FITS::Header::Item->new(Keyword=>'END'));
    
    #
    # Write out all the cards, and note how many bytes for later padding.
    #
    my $s = join("",$hdr->cards);
    
    $fh->print( $s );
    $nbytes = length $s;
  } else {
    ##
    ## Legacy emitter (note different advertisement in the SIMPLE
    ## comment, for debugging!)
    ##
    
    $fh->printf( "%-80s", "SIMPLE  =                    T / PDL::IO::FITS::wfits (http://pdl.perl.org)" );
    
    $nbytes = 80; # Number of bytes written so far
    
    # Write FITS header
    
    %hdr = ();
    if (defined($h)) {
      for (keys %$h) { $hdr{uc $_} = $$h{$_} } # Copy (ensuring keynames are uppercase)
    }
    
    delete $hdr{SIMPLE}; delete $hdr{'END'};
    
    $hdr{BITPIX} =  $BITPIX;
    $hdr{BUNIT} = "Data Value" unless exists $hdr{BUNIT};
    wheader($fh, 'BITPIX');
    
    $ndims = $pdl->getndims; # Dimensions of data array
    $hdr{NAXIS}  = $ndims;
    wheader($fh, 'NAXIS');
    for $k (1..$ndims) { $hdr{"NAXIS$k"} = $pdl->getdim($k-1) }
    for $k (1..$ndims) { wheader($fh,"NAXIS$k") }
    
    if ($bscale != 1 || $bzero  != 0) {
      $hdr{BSCALE} =  $bscale;
      $hdr{BZERO}  =  $bzero;
      wheader($fh,'BSCALE');
      wheader($fh,'BZERO');
    }
    wheader($fh,'BUNIT');
    
    # IF badflag is set
    #   and BITPIX > 0 - ensure the header contains the BLANK keyword
    #                    (make sure it's for the correct type)
    #   otherwise      - make sure the BLANK keyword is removed
    if ( $pdl->badflag() ) {
      if ( $BITPIX > 0 ) { my $a = &$convert(pdl(0.0)); $hdr{BLANK} = $a->badvalue(); }
      else               { delete $hdr{BLANK}; }
    }
    
    for $k (sort fits_field_cmp keys %hdr) { 
	wheader($fh,$k) unless $k =~ m/HISTORY/;
    }
    wheader($fh, 'HISTORY'); # Make sure that HISTORY entries come last.
    $fh->printf( "%-80s", "END" );
    $nbytes += 80;
  }
  
  #
  # Pad the header to a legal value and write the rest of the FITS file.
  #
  $nbytes %= 2880;
  $fh->print( " "x(2880-$nbytes) )
      if $nbytes != 0; # Fill up HDU
  
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
    
    &$bswap($buff);
    $fh->print( ${$buff->get_dataref} );
    $off += $BUFFSZ;
}
$buff = &$convert( ($p1d->slice($off/$sz.":-1") - $bzero)/$bscale );

if ( $pdl->badflag() and $BITPIX < 0 and $PDL::Bad::UseNaN == 0 ) {
  $buff->inplace->setbadtonan();
}

  &$bswap($buff);
  $fh->print( ${$buff->get_dataref} );
  # Fill HDU and close
  # note that for the data space the fill character is \0 not " "
  #
  $fh->print( "\0"x(($BUFFSZ - $buff->getdim(0) * $sz)%2880) );
  $fh->close();
  1;
}

######################################################################
######################################################################

# Compare FITS headers in a sensible manner.

=head2 fits_field_cmp

=for ref

fits_field_cmp

Sorting comparison routine that makes proper sense of the digits at the end
of some FITS header fields.  Sort your hash keys using "fits_field_cmp" and 
you will get (e.g.) your "TTYPE" fields in the correct order even if there
are 140 of them.

This is a standard kludgey perl comparison sub -- it uses the magical
$a and $b variables, rather than normal argument passing.

=cut

sub fits_field_cmp {
  if( $a=~m/^(.*[^\d])(\d+)$/ ) { 
    my ($an,$ad) = ($1,$2);
    if( $b=~m/^(.*[^\d])(\d+)$/ ) {
      my($bn,$bd) = ($1,$2);
    
      if($an eq $bn) {
	return $ad<=>$bd;
      } 
    }
  }
  $a cmp $b;
}

=head2 _rows()

=for ref

Return the number of rows in a variable for table entry

You feed in a PDL or a list ref, and you get back the 0th dimension.

=cut

sub _rows {
  my $var = shift;

  return $var->dim(0) if( UNIVERSAL::isa($var,'PDL') );
  return 1+$#$var if(ref $var eq 'ARRAY');
  return 1 unless(ref $var);
  
  print STDERR "Warning: _rows found an unacceptable ref. ".ref $var.". Ignoring...\n"
    if($PDL::verbose);
  
  return undef;
}



=head2 _prep_table()

=for ref 

Accept a hash ref containing a table, and return a header describing the table
and a string to be written out as the table, or barf.

You can indicate whether the table should be binary or ascii.  The default
is binary; it can be overridden by the "tbl" field of the hash (if present)
or by parameter.

=cut

our %bintable_types = (
  'byte'=>['B',1],
  'short'=>['I',2],
  'ushort'=>['J',4, sub {return long shift;}],
  'long'=>['J',4],
  'longlong'=>['D', 8, sub {return double shift;}],
  'float'=>['E',4],
  'double'=>['D',8],
#  'complex'=>['M',8]  # Complex doubles are supported (actually, they aren't at the moment)
);


sub _prep_table {
  my ($hash,$tbl,$nosquish) = @_;
  
  my $ohash;

  my $hdr = $hash->{hdr};

  # Make a local copy of the header.
  my $h = {};
  if(defined $hdr) {
    local $_;
    for (keys %$hdr) {$h->{$_} = $hdr->{$_}};
  }
  $hdr = $h;

  $tbl = $hash->{tbl} unless defined($tbl);

  barf "_prep_table called without a HASH reference as the first argument"
    unless ref $hash eq 'HASH';

  #####
  # Figure out how many columns are in the table
  my @colkeys = grep( ( !m/^(hdr|tbl)$/ and defined $hash->{$_}), 
		      sort fits_field_cmp keys %$hash 
		      );
  my $cols = @colkeys;

  print "Table seems to have $cols columns...\n"
    if($PDL::verbose);
  
  #####
  # Figure out how many rows are in the table, and store counts...
  # 
  my $rows;
  my $rkey;
  for my $key(@colkeys) {
    my $r = _rows($hash->{$key});
    ($rows,$rkey) = ($r,$key) unless(defined($rows) && $rows != 1);
    if($r != $rows && $r != 1) {
      barf "_prep_table: inconsistent number of rows ($rkey: $rows vs. $key: $r)\n";
    }
  }
  
  print "Table seems to have $rows rows...\n"
    if($PDL::verbose);

  #####
  # Squish and disambiguate column names 
  #
  my %keysbyname;
  my %namesbykey;

  print "Renaming hash keys...\n"
    if($PDL::debug);

  for my $key(@colkeys) {
    my $name = $key;

    $name =~ tr/[a-z]/[A-Z]/;   # Uppercaseify (required by FITS standard)
    $name =~ s/\s+/_/g;         # Remove whitespace (required by FITS standard)
    
    unless($nosquish) {     
      $name =~ s/[^A-Z0-9_-]//g;  # Squish (recommended by FITS standard)
    }
    
    ### Disambiguate...
    if(defined $ohash->{$name}) {
      my $iter = 1;
      my $name2;
      do { $name2 = $name."_".($iter++); }
           while(defined $ohash->{$name2});
      $name = $name2;
    }

    $ohash->{$name} = $hash->{$key};
    $keysbyname{$name} = $key;
    $namesbykey{$key} = $name;

    print "\tkey '$key'\t-->\tname '$name'\n"
      if($PDL::debug || (($name ne $key) and $PDL::verbose));
  }


  # The first element of colnames is ignored (since FITS starts
  # count at 1)
  #
  my @colnames;  # Names by number
  my %colnums;   # Numbers by name

  ### Allocate any table columns that are already in the header...
  local $_;
  map { for my $a(1) { # [Shenanigans to make next work right]
    next unless m/^TTYPE(\d*)$/;

    my $num = $1;
    
    if($num > $cols || $num < 1) {
      print "Ignoring illegal column number $num ( should be in range 1..$cols )\n"
	if($PDL::verbose);
      delete $hdr->{$_};
      next;
    }

    my $key = $hdr->{$_};

    my $name;
    unless( $name = $namesbykey{$key}) { # assignment
      $name = $key;
      unless( $key = $keysbyname{$key}) {
	print "Ignoring column $num in existing header (unknown name '$key')\n"
	if($PDL::verbose);
	next;
      }
    }

    $colnames[$num] = $name;
    $colnums{$name} = $num;
  } } sort fits_field_cmp keys %$hdr;

  ### Allocate all other table columns in alphabetical order...
  my $i = 0;
  for my $k (@colkeys) {
    my $name = $namesbykey{$k};

    unless($colnums{$name}) {
      while($colnames[++$i]) { }
      $colnames[$i] = $name;
      $colnums{$name} = $i;
    } else { $i++; }
  }
  
  print "Assertion failed:  i ($i) != colnums ($cols)\n"
    if($PDL::debug && $i != $cols);

  print "colnames: " .
      join(",", map { $colnames[$_]; } (1..$cols) ) ."\n"
	  if($PDL::debug);

  ######## 
  # OK, now the columns are allocated -- spew out a header.

  my @converters = ();   # Will fill up with conversion routines for each column
  my @field_len = ();    # Will fill up with field lengths for each column
  my @internaltype = (); # Gets flag for PDLhood
  my @fieldvars = ();    # Gets refs to all the fields of the hash.

  if($tbl eq 'binary') {
    $hdr->{XTENSION} = 'BINTABLE';
    $hdr->{BITPIX} = 8;
    $hdr->{NAXIS} = 2;
    #$hdr->{NAXIS1} = undef; # Calculated below; inserted here as placeholder.
    $hdr->{NAXIS2} = $rows;
    $hdr->{PCOUNT} = 0; # Change this is variable-arrays are adopted
    $hdr->{GCOUNT} = 1;
    $hdr->{TFIELDS} = $cols;

    # Figure out data types, and accumulate a row length at the same time.

    my $rowlen = 0;

    # NOTE:
    #  the conversion from ushort to long below is a hack to work
    #  around the issue that otherwise perl treats it as a 2-byte
    #  NOT 4-byte string on writing out, which leads to data corruption
    #  Really ushort arrays should be written out using SCALE/ZERO
    #  so that it can be written as an Int2 rather than Int4
    #
    for my $i(1..$cols) {
      $fieldvars[$i] = $hash->{$keysbyname{$colnames[$i]}};
      my $var = $fieldvars[$i];

      $hdr->{"TTYPE$i"} = $colnames[$i];
      my $tform;
      
      my $tstr;
      my $rpt;
      my $bytes;
      
      if( UNIVERSAL::isa($var,'PDL') ) {

	$internaltype[$i] = 'P';

	my $dims = pdl($var->dims); 
	$dims->(0) .= 1;
	$rpt = $dims->prod;
	
	my $t;

=begin WHENCOMPLEXVALUESWORK
	
	if( UNIVERSAL::isa($var,'PDL::Complex') ) {
	  $rpt = $var->dim(1);
	  $t = 'complex'
	} else { 
	  $t = type $var;
	}

=end WHENCOMPLEXVALUESWORK

=cut

	barf "Error: wfits() currently can not handle PDL::Complex arrays (column $colnames[$i])\n"
	  if UNIVERSAL::isa($var,'PDL::Complex');
	$t = $var->type;

	$t = $bintable_types{$t};
	
	unless(defined($t)) {
	  print "Warning: converting unknown type $t (column $colnames[$i]) to double...\n"
	    if($PDL::verbose);
	  $t = $bintable_types{'double'};
	}

	($tstr, $bytes, $converters[$i]) = @$t;

      } elsif( ref $var eq 'ARRAY' ) {

	$internaltype[$i] = 'A';
	$bytes = 1;
	
	# Got an array (of strings) -- find the longest element 
	$rpt = 0;
	for(@$var) {
	  my $l = length($_);
	  $rpt = $l if($l>$rpt);
	}
	($tstr, $bytes, $converters[$i]) = ('A',1,undef);

      } elsif( ref $var ) {
	barf "You seem to be writing out a ".(ref $var)." as a table column.  I\ndon't know how to do that (yet).\n";
	
      } else {   # Scalar
	$internaltype[$i] = 'A';
	($tstr, $bytes, $converters[$i])  = ('A',1,undef);
	$rpt = length($var);
      }
      
      $hdr->{"TFORM$i"} = "$rpt$tstr";
      $rowlen += ($field_len[$i] = $rpt * $bytes);
    }
      
    $hdr->{NAXIS1} = $rowlen;
    
    ## Now accumulate the binary table

    my $table = "";
    
    for my $r(0..$rows-1) {
      my $row = "";
      for my $c(1..$cols) {
	my $tmp;
	my $a = $fieldvars[$c];
       
	if($internaltype[$c] eq 'P') {  # PDL handling
	  $tmp = $converters[$c]
	    ? &{$converters[$c]}($a->($r)->flat->sever) 
	      : $a->($r)->flat->sever ;

	  ## This would go faster if moved outside the loop but I'm too
	  ## lazy to do it Right just now.  Perhaps after it actually works.
	  ##
	  if(!isbigendian()) {
	    bswap2($tmp) if($tmp->get_datatype == $PDL_S);
	    bswap4($tmp) if($tmp->get_datatype == $PDL_L ||
			    $tmp->get_datatype == $PDL_F);
	    bswap8($tmp) if($tmp->get_datatype == $PDL_D);
	  }

	  my $t = $tmp->get_dataref;  
	  $tmp = $$t;
	} else {                          # Only other case is ASCII just now...
	  $tmp = ( ref $a eq 'ARRAY' ) ?          # Switch on array or string
	    ( $#$a == 0 ? $a->[0] : $a->[$r] )  # Thread arrays as needed
	      : $a;
	 
	  $tmp .= " " x ($field_len[$c] - length($tmp));
	}
       
	# Now $tmp contains the bytes to be written out...
	#
	$row .= substr($tmp,0,$field_len[$c]);
      } # for: $c
      $table .= $row;
    } # for: $r

    my $table_size = $rowlen * $rows;
    if( (length $table) != $table_size ) {
      print "Warning: Table length is ".(length $table)."; expected $table_size\n";
    }

    return ($hdr,$table);
      
  } elsif($tbl eq 'ascii') {
    barf "ASCII tables not yet supported...\n";
  } else {
    barf "unknown table type '$tbl' -- giving up.";
  }
}

# the header fill value can be blanks, but the data fill value must
# be zeroes in non-ASCII tables
#
sub _print_to_fits ($$$) {
    my $fh = shift;
    my $data = shift;
    my $blank = shift;

    my $len = ((length $data) - 1) % 2880 + 1;
    $fh->print( $data . ($blank x (2880-$len)) );
}
  
{
    my $ctr = 0;

    sub reset_hdr_ctr() { $ctr = 0; }

    sub add_hdr_item ($$$$;$) {
	my ( $hdr, $key, $value, $type, $comment ) = @_;
	$type = uc($type) if defined  $type;
	my $item = Astro::FITS::Header::Item->new( Keyword=>$key,
						   Value=>$value,
						   Type=>$type );
	$item->comment( $comment ) if defined $comment;
	$hdr->replace( $ctr++, $item );
    };
}

##############################
#
# _wfits_table -- given a hash ref, try to write it out as a 
# table extension.  The file FITS should be open when you call it.
# Most of the work of creating the extension header, and all of
# the work of creating the table, is handled by _prep_table().
#
# NOTE:
#   can not think of a sensible name for the extension so calling
#   it TABLE for now
#
sub _wfits_table ($$$) {
  my $fh = shift;
  my $hash = shift;
  my $tbl = shift;
  
  barf "FITS BINTABLES are not supported without the Astro::FITS::Header module.\nGet it from www.cpan.org.\n"
    unless($PDL::Astro_FITS_Header);

  my ($hdr,$table) = _prep_table($hash,$tbl,0);

  # Copy the prepared fields into the extension header.
  tie my %newhdr,'Astro::FITS::Header',my $h = Astro::FITS::Header->new;
  
  reset_hdr_ctr();
  add_hdr_item $h, "XTENSION", ($tbl eq 'ascii'?"TABLE":"BINTABLE"), 'string', "from perl hash";
  add_hdr_item $h, "BITPIX", $hdr->{BITPIX}, 'int';
  add_hdr_item $h, "NAXIS", 2, 'int';
  add_hdr_item $h, "NAXIS1", $hdr->{NAXIS1}, 'int', 'Bytes per row';
  add_hdr_item $h, "NAXIS2", $hdr->{NAXIS2}, 'int', 'Number of rows';
  add_hdr_item $h, "PCOUNT", 0, 'int', ($tbl eq 'ascii' ? undef : "No heap") ;
  add_hdr_item $h, "GCOUNT", 1, 'int';
  add_hdr_item $h, "TFIELDS", $hdr->{TFIELDS},'int';
  add_hdr_item $h, "HDUNAME", "TABLE", 'string';

  for my $field( sort fits_field_cmp keys %$hdr ) {
    next if( defined $newhdr{$field} or $field =~ m/^end|simple|xtension$/i);
    my $type = 	(UNIVERSAL::isa($hdr->{field},'PDL') ? 
		   $hdr->{$field}->type : 
		   ((($hdr->{$field})=~ m/^[tf]$/i) ? 
		    'logical' : 
		    undef ## 'string' seems to have a bug - 'undef' works OK
		    ));

    add_hdr_item $h, $field, $hdr->{$field}, $type, $hdr->{"${field}_COMMENT"};
  }

  add_hdr_item $h, "END", undef, 'undef';

  $hdr = join("",$h->cards);
  _print_to_fits( $fh, $hdr, " " );
  _print_to_fits( $fh, $table, "\0" ); # use " " if it is an ASCII table
}

sub _wfits_nullhdu ($) {
  my $fh = shift;
  if($Astro::FITS::Header) {
    my $h = Astro::FITS::Header->new();
    
    reset_hdr_ctr();
    add_hdr_item $h, "SIMPLE", "T", 'logical', "Null HDU (no data, only extensions)";
    add_hdr_item $h, "BITPIX", -32, 'int', "Needed to make fverify happy";
    add_hdr_item $h, "NAXIS", 0, 'int';
    add_hdr_item $h, "EXTEND", "T", 'logical', "File contains extensions";
    add_hdr_item $h, "COMMENT", "", "comment",
    "  File written by perl (PDL::IO::FITS::wfits)";
    #
    # The following seems to cause a problem so removing for now (I don't
    # believe it is required, but may be useful for people who aren't
    # FITS connoisseurs). It could also be down to a version issue in
    # Astro::FITS::Header since it worked on linux with a newer version
    # than on Solaris with an older version of the header module)
    #
    ##  add_hdr_item $h, "COMMENT", "", "comment",
    ##    "  FITS (Flexible Image Transport System) format is defined in 'Astronomy";
    ##  add_hdr_item $h, "COMMENT", "", "comment",
    ##    "  and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H";
    add_hdr_item $h, "HDUNAME", "PRIMARY", 'string';
    add_hdr_item $h, "END", undef, 'undef';
    
    my $hdr = join("",$h->cards);
    _print_to_fits( $fh, $hdr, " " );
  } else {
    _print_to_fits( $fh, 
		    q+SIMPLE  =                    T / Null HDU (no data, only extensions)            BITPIX  =                  -32 / Needed to make fverify happy                   NAXIS   =                    0                                                  EXTEND  =                    T / File contains extensions                       COMMENT   Written by perl (PDL::IO::FITS::wfits) legacy code.                   COMMENT   For best results, install Astro::FITS::Header.                        HDUNAME = 'PRIMARY '                                                            END                                                                             +,
		    " ");
  }
}

    
1;

