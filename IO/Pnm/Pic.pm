=head1 NAME

PDL::IO::Pic -- image I/O for PDL

=head1 DESCRIPTION

=head2 Image I/O for PDL based on the netpbm package.

This package implements I/O for a number of popular image formats
by exploiting the xxxtopnm and pnmtoxxx converters from the netpbm package
(which is based on the original pbmplus by Jef Poskanzer).

Netpbm is available at
ftp://wuarchive.wustl.edu/graphics/graphics/packages/NetPBM/
Pbmplus (on which netpbm is based) might work as well, I haven't tried it.
If you want to read/write JPEG images you additionally need the two
converters cjpeg/djpeg which come with the libjpeg distribution (the
"official" archive site for this software is L<ftp://ftp.uu.net/graphics/jpeg>).

Image I/O for all formats is established by reading and writing only
the PNM format directly while the netpbm standalone apps take care of
the necessary conversions. In accordance with netpbm parlance PNM stands
here for 'portable any map' meaning any of the PBM/PGM/PPM formats.

As it appeared to be a reasonable place this package also contains the
routine wmpeg to write mpeg movies from PDLs representing image
stacks (the image stack is first written as a sequence of PPM images into some
temporary directory). For this to work you need the program ffmpeg also.

=cut

package PDL::IO::Pic;


@EXPORT_OK = qw( wmpeg rim wim rpic wpic rpiccan wpiccan );

%EXPORT_TAGS = (Func => [@EXPORT_OK]);
use PDL::Core;
use PDL::Exporter;
use PDL::Types;
use PDL::ImageRGB;
use PDL::IO::Pnm;
use PDL::Options;
use PDL::Config;
use File::Basename;
use SelfLoader;
use File::Spec;

use strict;
use vars qw( $Dflags @ISA %converter );

@ISA    = qw( PDL::Exporter );


=head2 Configuration

The executables from the netpbm package are assumed to be in your path.
Problems in finding the executables may show up as PNM format
errors when calling wpic/rpic. If you run into this kind of problem run
your program with perl C<-w> so that perl prints a message if it can't find
the filter when trying to open the pipe. [']

=cut


# list of converters by type
# might get more fields in the future to provide a generic representation
# of common flags like COMPRESSION, LUT, etc which would hold the correct
# flags for the particular converter or NA if not supported
# conventions:
#    NONE             we need no converter (directly supported format)
#    NA               feature not available
#    'whatevertopnm'  name of the executable
# The 'FLAGS' key must be used if the converter needs other flags than
# the default flags ($Dflags)
#
#
# The "referral" field, if present, contains a within-perl referral
# to other methods for reading/writing the PDL as that type of file.  The
# methods must have the same syntax as wpic/rpic (e.g. wfits/rfits).
#

$PDL::IO::Pic::debug = $PDL::IO::Pic::debug || 0;
&init_converter_table();

# setup functions

sub init_converter_table {
  # default flag to be used with any converter unless overridden with FLAGS
  $Dflags = '';
  %converter = ();

  # Pbmplus systems have cjpeg/djpeg; netpbm systems have pnmtojpeg and
  # jpegtopnm.

  my $jpeg_conv='';

  {
      my @path = File::Spec->path();
      my $ext = $^O =~ /MSWin/i ? '.exe' : '';
      local $_;
      my $pbmplus;

      for (@path) {
	  $jpeg_conv="cjpeg" if ( -x "$_/cjpeg" . $ext );
	  $jpeg_conv="pnmtojpeg" if (  -x "$_/pnmtojpeg" . $ext );
      }
  }

  my @normal = qw/TIFF SGI RAST PCX PNG/;
  push(@normal,"JPEG") if($jpeg_conv eq 'pnmtojpeg');

  for (@normal)
    { my $conv = lc; $converter{$_} = {put => "pnmto$conv",
				       get => "$conv".'topnm'} }

  my @special = (['PNM','NONE','NONE'],
		 ['PS','pnmtops -dpi=100',
		  'pstopnm -stdout -xborder=0 -yborder=0 -quiet -dpi=100'],
		 ['GIF','ppmtogif','giftopnm'],
		 ['IFF','ppmtoilbm','ilbmtoppm']
		 );
  push(@special,['JPEG', 'cjpeg' ,'djpeg'])
      if($jpeg_conv eq 'cjpeg');

   for(@special) {
    $converter{$_->[0]} = {put => $_->[1],
			   get => $_->[2]}
  }

  $converter{'FITS'}={ 'referral' => {'put' => \&PDL::wfits, 'get' => \&PDL::rfits} };

  # these converters do not understand pbmplus flags:
  $converter{'JPEG'}->{FLAGS} = '';
  $converter{'GIF'}->{Prefilt} = 'ppmquant 256 |';


  my $key;
  for $key (keys %converter) {

    $converter{$key}->{Rok} = inpath($converter{$key}->{'get'})
      if defined($converter{$key}->{'get'});

    $converter{$key}->{Wok} = inpath($converter{$key}->{'put'})
      if defined($converter{$key}->{'put'});

    if (defined $converter{$key}->{Prefilt}) {
      my $filt = $1 if $converter{$key}->{Prefilt} =~ /^\s*(\S+)\s+/;
      $converter{$key}->{Wok} = inpath($filt) if $converter{$key}->{Wok};
    }
  }

  $PDL::IO::Pic::biggrays = &hasbiggrays();
  print "using big grays\n" if $PDL::IO::Pic::debug &&
    $PDL::IO::Pic::biggrays;

  for (keys %converter) {
    $converter{$_}->{ushortok} = $PDL::IO::Pic::biggrays ?
      (m/GIF/ ? 0 : 1) : (m/GIF|RAST|IFF/ ? 0 : 1);
  }
}

sub inpath {
  my ($prog) = @_;
  my $pathsep = $^O =~ /win32/i ? ';' : ':';
  my $exe = $^O =~ /win32/i ? '.exe' : '';
  for(split $pathsep,$ENV{PATH}){return 1 if -x "$_/$prog$exe" || $prog =~ /^NONE$/}
  return 0;
}


sub hasbiggrays {
  my ($checked,$form) = (0,'');
  require IO::File;
  for (&rpiccan()) { next if /^PNM$/; $form = $_; $checked=1; last }
  unless ($checked) {
    warn "PDL::IO::Pic - couldn't find any pbm converter"
      if $PDL::IO::Pic::debug;
    return 0;
  }
  *SAVEERR = *SAVEERR;  # stupid fix to shut up -w (AKA pain-in-the-...-flag)
  open(SAVEERR, ">&STDERR");
  my $tmp = new_tmpfile IO::File or barf "couldn't open tmpfile";
  my $pos = $tmp->getpos;
  my $txt;
  { local *IN;
    *IN = *$tmp;  # doesn't seem to work otherwise
    open(STDERR,">&IN") or barf "couldn't redirect stdder";

    system("$converter{$form}->{get} -version");
    open(STDERR, ">&PDL::IO::Pic::SAVEERR");
    $tmp->setpos($pos);  # rewind
    $txt = join '',<IN>;
    close IN; undef $tmp;
  }
  return ($txt =~ /PGM_BIGGRAYS/);
}

=head1 FUNCTIONS

=head2 rpiccan, wpiccan

=for ref

Test which image formats can be read/written

=for example

   $im = PDL->rpic('PDL.jpg') if PDL->rpiccan('JPEG');
   @wformats = PDL->wpiccan();

finds out if PDL::IO::Pic can read/write certain image formats.
When called without arguments returns a list of supported
formats. When called with an argument returns true if format
is supported on your computer (requires appropriate filters in
your path), false otherwise.

=cut

sub rpiccan {return PDL->rpiccan(@_)}
sub wpiccan {return PDL->wpiccan(@_)}
sub PDL::rpiccan {splice @_,1,0,'R';
		  return PDL::IO::Pic::piccan(@_)}
sub PDL::wpiccan {splice @_,1,0,'W';
		  return PDL::IO::Pic::piccan(@_)}


=head2 rpic

=for ref

Read images in many formats with automatic format detection.

=for example

    $im = rpic $file;
    $im = PDL->rpic 'PDL.jpg' if PDL->rpiccan('JPEG');

I<Options>

=for opt

    FORMAT  =>  'JPEG'   # explicitly read this format
    XTRAFLAGS => '-nolut'  # additional flags for converter

Reads image files in most of the formats supported by netpbm. You can
explicitly specify a supported format by additionally passing a hash
containing the FORMAT key as in

    $im = rpic ($file, {FORMAT => 'GIF'});

This is especially useful if the particular format isn't identified by
a magic number and doesn't have the 'typical' extension or you want to
avoid the check of the magic number if your data comes in from a pipe.
The function returns a pdl of the appropriate type upon completion.
Option parsing uses the L<PDL::Options|PDL::Options> module and
therefore supports minimal options matching.

You can also read directly into an existing pdl that has to have the
right size(!). This can come in handy when you want to read a sequence
of images into a datacube, e.g.

  $stack = zeroes(byte,3,500,300,4);
  rpic $stack->slice(':,:,:,(0)'),"PDL.jpg";

reads an rgb image (that had better be of size (500,300)) into the
first plane of a 3D RGB datacube (=4D pdl datacube). You can also do
transpose/inversion upon read that way.

=cut

my $rpicopts = {
               FORMAT => undef,
               XTRAFLAGS => undef,
              };

sub rpic {PDL->rpic(@_)}

sub PDL::rpic {
    barf 'Usage: $im = rpic($file[,hints]) or $im = PDL->rpic($file[,hints])'
       if $#_<0;
    my ($class,$file,$hints,$maybe) = @_;
    my ($type, $pdl);

    if (ref($file)) { # $file is really a pdl in this case
	$pdl = $file;
	$file = $hints;
	$hints = $maybe;
    } else {
        $pdl = $class->initialize;
    }

    $hints = { iparse $rpicopts, $hints } if ref $hints;
    if (defined($$hints{'FORMAT'})) {
	$type = $$hints{'FORMAT'};
        barf "unsupported (input) image format"
	    unless (exists($converter{$type}) &&
		    $converter{$type}->{'get'} !~ /NA/);
      }
    else {
	$type = chkform($file);
	barf "can't figure out file type, specify explicitly"
	    if $type =~ /UNKNOWN/; }

    my($converter) = $PDL::IO::Pic::converter;
    if (defined($converter{$type}->{referral})) {
      if(ref ($converter{$type}->{referral}->{'get'}) eq 'CODE') {
	return &{$converter{$type}->{referral}->{'get'}}(@_);
      } else {
	barf "rpic: internal error with referral (format is $type)\n";
      }
    }

    my $flags = $converter{$type}->{FLAGS};
    $flags = "$Dflags" unless defined($flags);
    $flags .= " $$hints{XTRAFLAGS}" if defined($$hints{XTRAFLAGS});
    my $cmd = qq{$converter{$type}->{get} $flags "$file" |};
    $cmd = $file if $converter{$type}->{'get'} =~ /^NONE/;

    print("conversion by '$cmd'\n") if $PDL::IO::Pic::debug > 10;

    return rpnm($pdl,$cmd);
}

=head2 wpic

=for ref

Write images in many formats with automatic format selection.

=for usage

   Usage: wpic($pdl,$filename[,{ options... }])

=for example

    wpic $pdl, $file;
    $im->wpic('web.gif',{LUT => $lut});
    for (@images) {
      $_->wpic($name[0],{CONVERTER => 'ppmtogif'})
    }


Write out an image file. Function will try to guess correct image
format from the filename extension, e.g.

    $pdl->wpic("image.gif")

will write a gif file. The data written out will be scaled to byte if
input is of type float/double. Input data that is of a signed integer
type and contains negative numbers will be rejected (assuming the user
should have the desired conversion to an unsigned type already). A number
of options can be specified (as a hash reference) to get more direct control of
the image format that is being written. Valid options are (key
=> example_value):

=for options

   CONVERTER  => 'ppmtogif',   # explicitly specify pbm converter
   FLAGS      => '-interlaced -transparent 0',  # flags for converter
   IFORM      => 'PGM',        # explicitly specify intermediate format
   XTRAFLAGS  => '-imagename iris', # additional flags to defaultflags
   FORMAT     => 'PCX',        # explicitly specify output image format
   COLOR      => 'bw',         # specify color conversion
   LUT        => $lut,         # use color table information

Option parsing uses the L<PDL::Options|PDL::Options> module and
therefore supports minimal options matching. A detailed explanation of
supported options follows.

=over 7

=item CONVERTER

directly specify the converter,
you had better know what you are doing, e.g.

  CONVERTER  => 'ppmtogif',

=item FLAGS

flags to use with the converter;
ignored if !defined($$hints{CONVERTER}), e.g. with the gif format

  FLAGS      => '-interlaced -transparent 0',

=item IFORM

intermediate PNM/PPM/PGM/PBM format to use;
you can append the strings 'RAW' or 'ASCII'
to enforce those modes, eg IFORMAT=>'PGMRAW' or

  IFORM    => 'PGM',

=item XTRAFLAGS

additional flags to use with an automatically chosen
converter, this example works when you write SGI
files (but will give an error otherwise)

  XTRAFLAGS => '-imagename iris',

=item FORMAT

explicitly select the format you want to use. Required if wpic cannot
figure out the desired format from the file name extension. Supported
types are currently TIFF,GIF,SGI,PNM,JPEG,PS,RAST(Sun Raster),IFF,PCX,
e.g.

   FORMAT     => 'PCX',

=item COLOR

you want black and white (value B<bw>), other possible value is
B<bwdither> which will write a dithered black&white
image from the input data, data conversion will be done appropriately,
e.g.

   COLOR      => 'bw',

=item LUT

This is a palette image and the value of this key should be a
pdl containing an RGB lookup table (3,x), e.g.

   LUT        => $lut,

=back

Using the CONVERTER hint you can also build a pipe and perform
several netpbm operations to get the special result you like. Using it
this way the first converter/filecommand in the pipe should be
specified with the CONVERTER hint and subsequent converters + flags in
the FLAGS hint. This is because wpic tries to figure out the required
format to be written by wpnm based on the first converter. Be careful when
using the PBMBIN var as it will only be prepended to the converter. If more
converters are in the FLAGS part specify the full path unless they are in
your PATH anyway.

Example:

   $im->wpic('test.ps',{CONVERTER  => 'pgmtopbm',
		    FLAGS => "-dither8 | pnmtops" })

Some of the options may appear silly at the moment and probably
are. The situation will hopefully improve as people use the code and
the need for different/modified options becomes clear. The general
idea is to make the function perl compliant: easy things should be
easy, complicated tasks possible.

=cut

my %wpicopts = map {($_ => undef)}
               qw/IFORM CONVERTER FLAGS FORMAT
               XTRAFLAGS COLOR LUT/;
my $wpicopts = \%wpicopts;

*wpic = \&PDL::wpic;

sub PDL::wpic {
    barf 'Usage: wpic($pdl,$filename[,$hints]) ' .
	   'or $pdl->wpic($filename,[,$hints])' if $#_<1;

    my ($pdl,$file,$hints) = @_;
    my ($type, $cmd, $form,$iform,$iraw);

    $hints = {iparse($wpicopts, $hints)} if ref $hints;
    # figure out the right converter
    my ($conv, $flags, $format, $referral) = getconv($pdl,$file,$hints);

    if(defined($referral)) {
      if(ref ($referral->{'put'}) eq 'CODE') {
	return &{$referral->{'put'}}(@_);
      } else {
	barf "wpic: internal error with referral (format is $format)\n";
      }
    }

    print "Using the command $conv with the flags $flags\n"
       if $PDL::IO::Pic::debug>10;

    if (defined($$hints{IFORM})) {
	$iform = $$hints{IFORM}; }
    else {  # check if converter requires a particular intermediate format
	$iform = 'PPM' if $conv =~ /^\s*(ppm)|(cjpeg)/;
	$iform = 'PGM' if $conv =~ /^\s*pgm/;
	$iform = 'PBM' if $conv =~ /^\s*pbm/;
	$iform = 'PNM' if $conv =~ /^\s*(pnm)|(NONE)/; }
    # get final values for $iform and $pdl (check conversions, consistency,etc)
    ($pdl,$iform) = chkpdl($pdl,$iform,$hints,$format);
    print "using intermediate format $iform\n" if $PDL::IO::Pic::debug>10;

    $cmd = "|"  . qq{$conv $flags >"$file"};
    $cmd = ">" . $file if $conv =~ /^NONE/;
    print "built the command $cmd to write image\n" if $PDL::IO::Pic::debug>10;

    $iraw = 1 if (defined($$hints{IFORM}) && $$hints{IFORM} =~ /RAW/);
    $iraw = 0 if (defined($$hints{IFORM}) && $$hints{IFORM} =~ /ASCII/);

    local $SIG{PIPE}= sub {}; # Prevent crashing if converter dies

    wpnm($pdl, $cmd, $iform , $iraw);
}

=head2 rim

=for usage

 Usage: $x = rim($file);
 or       rim($x,$file);

=for ref

Read images in most formats, with improved RGB handling.

You specify a filename and get back a PDL with the image data in it.
Any PNM handled format or FITS will work. In the second form, $x is an
existing PDL that gets loaded with the image data.

If the image is in one of the standard RGB formats, then you get back
data in (<X>,<Y>,<RGB-index>) format -- that is to say, the third dim
contains the color information.  That allows you to do simple indexing
into the image without knowing whether it is color or not -- if present,
the RGB information is silently threaded over.  (Contrast L<rpic|rpic>, which
munges the information by putting the RGB index in the 0th dim, screwing
up subsequent threading operations).

If the image is in FITS format, then you get the data back in exactly
the same order as in the file itself.

Images with a ".Z" or ".gz" extension are assumed to be compressed with
UNIX L<"compress"|compress> or L<"gzip"|gzip>, respecetively, and are
automatically uncompressed before reading.

OPTIONS

The same as L<rpic|rpic>, which is used as an engine:

=over 3

=item FORMAT

If you don't specify this then formats are autodetected.  If you do specify
it then only the specified interpreter is tried.  For example,

  $x = rim("foo.gif",{FORMAT=>"JPEG"})

forces JPEG interpretation.

=item XTRAFLAGS

Contains extra command line flags for the pnm interpreter.  For example,

  $x = rim("foo.jpg",{XTRAFLAGS=>"-nolut"})

prevents use of a lookup table in JPEG images.

=back

=cut

use PDL::IO::Pic;

sub rim {
  my(@args) = @_;

  my $out;

  ## Handle dest-PDL-first case
  if(@args >= 2 and (UNIVERSAL::isa($args[0],'PDL'))) {
      my $dest = shift @args;
      my $rpa = PDL->null;
      $out = rpic(@args);

      if($out->ndims == 3 && $out->dim(0) == 3 &&
	 !( defined($out->gethdr) && $out->gethdr->{SIMPLE} )
	  ) {
	  $out =  $out->reorder(1,2,0);
      }
      
      $dest .= $out;
      return $out;
  }

  # Handle no-first-PDL case
  $out = rpic(@args);

  if($out->ndims == 3 && $out->dim(0) == 3 &&
     !( defined($out->gethdr) && $out->gethdr->{SIMPLE} )
     ) {
    return $out->reorder(1,2,0);
  }

  $out;
}



=head2 wim

=for ref

Write a pdl to an image file with selected type (or using filename extensions)

=for usage

  wim $pdl,$file;
  $pdl->wim("foo.gif",{LUT=>$lut});

Write out an image file.  You can specify the format explicitly as an
option, or the function will try to guess the correct image
format from the filename extension, e.g.

  $pdl->wim("image.gif");
  $pdl->wim("image.fits");

will write a gif and a FITS file.  The data written out will be scaled
to byte if the input if of type float/double.  Input data that is of a
signed integer type and contains negative numbers will be rejected.

If you append C<.gz> or C<.Z> to the end of the file name, the final
file will be automatically compresed with L<"gzip"|gzip> |
L<"compress"|compress>, respectively.

OPTIONS

You can pass in a hash ref whose keys are options.  The code uses the
PDL::Options module so unique abbreviations are accepted.  Accepted
keys are the same as for L<wpic|wpic>, which is used as an engine:

=over 3

=item CONVERTER

Names the converter program to be used by pbmplus (e.g. "ppmtogif" to
output a gif file)

=item FLAGS

Flags that should be passed to the converter (replacing any default flag list)
e.g. "-interlaced" to make an interlaced GIF

=item IFORM

Explicitly specifies the intermediate format (e.g. PGM, PPM, or PNM).

=item XTRAFLAGS

Flags that should be passed to the converter (in addition to any default
flag list).

=item FORMAT

Explicitly specifies the output image format (allowing pbmplus to pick an
output converter)

=item COLOR

Specifies color conversion (e.g. 'bw' converts to black-and-white; see
L<pbmplus> for details).

=item LUT

Use color-table information

=back

=cut

*wim = \&PDL::wim;

sub PDL::wim {
  my(@args) = @_;

  my($im) = $args[0];

  $args[0] = $im->reorder(2,0,1)
    if(    $im->ndims == 3
       and $im->dim(2)==3
       and !(
	      ( $args[1] =~ m/\.fits$/i )
	      or
	      ( ref $args[2] eq 'HASH' and $args[2]->{FORMAT} =~ m/fits/i )
	    )
       );

  wpic(@args);
}

=head2 wmpeg

=for ref

Write an image sequence (a (3,x,y,n) byte pdl) as an animation.

=for usage

  $piddle->wmpeg('movie.mpg'); # $piddle is (3,x,y,nframes) byte

Writes a stack of RGB images as a movie.  While the
format generated is nominally MPEG, the file extension
is used to determine the video encoder type.

  E.g.:
    .mpg for MPEG-1 encoding
    .mp4 for MPEG-4 encoding
   
  And even:
    .gif for GIF animation (uncompressed)

C<wmpeg> requires a 4-D pdl of type B<byte> as
input.  The first dim B<has> to be of size 3 since
it will be interpreted as RGB pixel data.
C<wmpeg> returns 1 on success and undef on failure.

=for example

  $anim->wmpeg("GreatAnimation.mpg")
      or die "can't create mpeg1 output";

  $anim->wmpeg("GreatAnimation.mp4")
      or die "can't create mpeg4 output";

Some of the input data restrictions will have to
be relaxed in the future but routine serves as
a proof of principle at the moment. It uses the
program ffmpeg to encode the frames into video.
The arguments and parameters used for ffmpeg have
not been tuned. This is a first implementation
replacing mpeg_encode by ffmpeg. Currently, wmpeg
doesn't allow modification of the parameters
written through its calling interface. This will
change in the future as needed.

In the future it might be much nicer to implement
a movie perl object that supplies methods for
manipulating the image stack (insert, cut, append
commands) and a final movie->make() call would
invoke ffmpeg on the picture stack (which will
only be held on disk). This should get around the
problem of having to hold a huge amount of data
in memory to be passed into wmpeg (when you are,
e.g. writing a large animation from PDL3D rendered
fly-throughs).

Having said that, the actual storage requirements
might not be so big in the future any more if
you could pass 'virtual' transform pdls into
wmpeg that will only be actually calculated when
accessed by the wpic routines, you know what I
mean...


=cut

*wmpeg = \&PDL::wmpeg;

sub PDL::wmpeg {
   barf 'Usage: wmpeg($pdl,$filename) ' .
   'or $pdl->wmpeg($filename)' if $#_ != 1;

   my ($pdl,$file) = @_;

   # return undef if no ffmpeg in path
   if (! inpath('ffmpeg')) {
      warn("wmpeg: ffmpeg not found in PATH");
      return;
   }

   my @Dims = $pdl->dims;
   # too strict in general but alright for the moment
   # especially restriction to byte will have to be relaxed
   barf "input must be byte (3,x,y,z)" if (@Dims != 4) || ($Dims[0] != 3)
   || ($pdl->get_datatype != $PDL_B);
   my $nims = $Dims[3];
   my $tmp = gettmpdir();

   # get tmpdir for parameter file
   # see PDL-2.4.6 version for original code

   # check the pdl for correct dimensionality

   # write all the images as ppms and write the appropriate parameter file
   my ($i,$fname);
   # add blank cells to each image to fit with 16N x 16N mpeg standard
   # $frame is full frame, insert each image in as $inset
   my (@MDims) = (3,map(16*int(($_+15)/16),@Dims[1..2]));
   my ($frame) = zeroes(byte,@MDims);
   my ($inset) = $frame->slice(join(',',
         map(int(($MDims[$_]-$Dims[$_])/2).':'.
            int(($MDims[$_]+$Dims[$_])/2-1),0..2)));
   my $range = sprintf "[%d-%d]",0,$nims-1;
   local $SIG{PIPE} = 'IGNORE';
   open MPEG, "| ffmpeg -f image2pipe -vcodec ppm -i -  $file"
      or barf "spawning ffmpeg failed: $?";
   binmode MPEG;
   # select ((select (MPEG), $| = 1)[0]);  # may need for win32
   my (@slices) = $pdl->dog;
   for ($i=0; $i<$nims; $i++) {
      local $PDL::debug = 1;
      print STDERR "Writing frame $i, " . $frame->slice(':,:,-1:0')->clump(2)->info . "\n";
      $inset .= $slices[$i];
      print MPEG "P6\n$MDims[1] $MDims[2]\n255\n";
      pnmout($frame->slice(':,:,-1:0')->clump(2), 1, 0, 'PDL::IO::Pic::MPEG');
   }
   # clean up
   close MPEG;

   # rm tmpdir and files if needed
   return 1;
}



1; # Return OK status

__DATA__

# SelfLoaded code

sub piccan {
  my $class = shift;
  my $rw = (shift =~ /r/i) ? 'Rok' : 'Wok';
  if ($#_ > -1) {
    my $format = shift;
    barf 'unknown format' unless defined($converter{$format});
    return $converter{$format}->{$rw};
  } else {
    my @formats = ();
    for (keys %converter) {push @formats, $_ if $converter{$_}->{$rw}}
    return @formats;
  }
}

sub getext {
# changed to a more os independent way
    my $file = shift;
    my ($base,$dir,$ext) = fileparse($file,'\.[^.]*');
    $ext = $1 if $ext =~ /^.([^;]*)/;  # chop off VMS version numbers
    return $ext;
}

# try to figure out the format of a supposed image file from the extension
# a couple of extensions are only checked when the optional parameter
# $wmode is set (because those should have been identified by magic numbers
# when reading)
#    todo: check completeness
sub chkext {
    my ($ext,$wmode) = @_;
    $wmode = 0 unless defined $wmode;

    # there are not yet file formats which wouldn't have been identified
    # by magic no's if in reading mode

    if ($wmode) {
	return 'PNM'  if $ext =~ /^(pbm)|(pgm)|(ppm)|(pnm)$/;
	return 'JPEG' if $ext =~ /^(jpg)|(jpeg)$/;
	return 'TIFF' if $ext =~ /^(tiff)|(tif)$/;
	return 'PCX'  if $ext =~ /^pcx$/;
	return 'SGI'  if $ext =~ /^rgb$/;
	return 'GIF'  if $ext =~ /^gif$/;
	return 'RAST' if $ext =~ /^(r)|(rast)$/;
	return 'IFF'  if $ext =~ /^(iff)|(ilbm)$/;
	return 'PS'   if $ext =~ /^ps/;
	return 'FITS' if $ext =~ /^f(i?ts|it)$/;
	return 'PNG'  if $ext =~ /^png$/i;
    }


    return 'UNKNOWN';
}

# try to figure out the format of a supposed image file
# from the magic numbers (numbers taken from magic in netpbm and
# the file format routines in xv)
# if no magics match try extension for non-magic file types
#     todo: make more complete
sub chkform {
    my $file = shift;
    my ($format, $magic, $len, $ext) = ("","",0,"");

    open(IMG, $file) or barf "Can't open image file";
    binmode IMG;
    # should first check if file is long enough
    $len = read(IMG, $magic,12);
    if (!defined($len) ||$len != 12) {
	barf "end of file when checking magic number";
	close IMG;
	return 'UNKNOWN';
    }
    close IMG;

    return 'PNM'  if $magic =~ /^P[1-6]/;
    return 'GIF'  if $magic =~ /(^GIF87a)|(^GIF89a)/;
    return 'TIFF' if $magic =~ /(^MM)|(^II)/;
    return 'JPEG' if $magic =~ /^(\377\330\377)/;
    return 'SGI'  if $magic =~ /^(\001\332)|(\332\001)/;
    return 'RAST' if $magic =~ /^\131\246\152\225/;
    return 'IFF'  if $magic =~ /ILBM$/;
    return 'PCX'  if $magic =~ /^\012[\000-\005]/;
    return 'PS'   if $magic =~ /%!\s*PS/;
    return 'FITS' if $magic =~ /^SIMPLE  \=/;
    return 'PNG'  if $magic =~ /^.PNG\r/;


    return chkext(getext($file));    # then try extensions
}


# helper proc for wpic
# process hints for direct converter control and try to guess from extension
# otherwise
sub getconv {
    my ($pdl,$file,$hints) = @_;

    return ($$hints{CONVERTER},$$hints{FLAGS})
	if defined($$hints{CONVERTER});   # somebody knows what he is doing

    my $type = "";
    if (defined($$hints{'FORMAT'})) {
	$type = $$hints{'FORMAT'};
        barf "unsupported (output) image format"
	    unless (exists($converter{$type})
	      && $converter{$type}->{'put'} !~ /NA/);
      }
    else {
	$type = chkext(getext($file),1);
	if ($type =~ /UNKNOWN/) {
	    barf "can't figure out desired file type, using PNM" ;
	    $type = 'PNM';
	  }
      }

    my $conv = $converter{$type}->{'put'};

    # the datatype check is only a dirty fix for the ppmquant problem with
    # types > byte
    # a ppmquant is anyway only warranted when $isrgb!!!
    $conv = $converter{$type}->{Prefilt}.$conv
      if defined($converter{$type}->{Prefilt});

    my $flags = $converter{$type}->{FLAGS};
    $flags = "$Dflags" unless defined($flags);
    $flags .= " $$hints{XTRAFLAGS}" if defined($$hints{XTRAFLAGS});
    if (defined($$hints{'COLOR'}) && $$hints{'COLOR'} =~ /bwdither/) {
	$flags = " | $conv $flags";
	$conv =  "pgmtopbm -floyd"; }

    my($referral) = $converter{$type}->{referral};

    return ($conv, $flags, $type, $referral);
}

# helper proc for wpic
# if a certain type of pnm is required check data and make compliant if possible
# else if intermediate format is pnm or ppm figure out the appropriate format
# from the pdl
sub chkpdl {
    my ($pdl, $iform, $hints, $format) = @_;

    if ($pdl->get_datatype >= $PDL_L ||
	$pdl->get_datatype == $PDL_S ||
	(!$converter{$format}->{ushortok} && $pdl->get_datatype == $PDL_US)) {
	print "scaling data to type byte...\n" if $PDL::IO::Pic::debug;
	$pdl = bytescl($pdl,-255);
    }

    my ($isrgb,$form) = (0,"");
    my @Dims = $pdl->dims;
    $isrgb = 1 if ($#Dims >= 2) && ($Dims[0] == 3);
    barf "expecting 2D or 3D-RGB-interlaced data as input"
	unless ($isrgb || ($#Dims == 1));

    $$hints{'COLOR'} = "" unless defined($$hints{'COLOR'});
    if ($iform =~ /P[NP]M/) {  # figure out the format from the data
	$form = 'PPM' if $isrgb;
	$form = 'PGM' if ($#Dims == 1) || ($$hints{'COLOR'} =~ /bwdither/i);
	$form = 'PBM' if ($$hints{'COLOR'} =~ /bw/i);
        $iform = $form; }
    # this is the place for data conversions
    if ($isrgb && ($iform =~ 'P[B,G]M')) {
	print "wpic: converting to grayscale...\n";
	$pdl = rgbtogr($pdl); # colour to grayscale
    }
    if (defined $$hints{LUT}) {  # make LUT images into RGB
	barf "luts only with non RGB data" if $isrgb;
       print "starting palette->RGB conversion...\n" if $PDL::IO::Pic::debug;
	$pdl = interlrgb($pdl,$$hints{LUT});
	$iform = 'PPM';  # and tell everyone we are now RGB
       print "finished conversion\n" if $PDL::IO::Pic::debug;
	}
    return ($pdl, $iform);
}

# delegate setting the temporary directory to the config file
# (so that it can either be OS-independent or at least
#  easily controlled by the user).
#
sub gettmpdir {
    my $tmpdir = $PDL::Config{TEMPDIR} ||
      die "TEMPDIR not found in %PDL::Config";
    barf "can't locate a temp dir called $tmpdir" unless -d $tmpdir;
    return $tmpdir;
}


=head1 BUGS

Currently only a random selection of converters/formats provided by
pbmplus/netpbm is supported. It is hoped that the more important formats
are covered. Other formats can be added as needed. Please send patches to
the author.

=head1 AUTHOR

Copyright (C) 1996,1997 Christian Soeller <c.soeller@auckland.ac.nz>
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut
