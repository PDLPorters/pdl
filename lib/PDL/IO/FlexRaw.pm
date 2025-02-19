=head1 NAME

PDL::IO::FlexRaw -- A flexible binary I/O format for PerlDL

=head1 SYNOPSIS

    use PDL;
    use PDL::IO::FlexRaw;

    # To obtain the header for reading (if multiple files use the
    # same header, for example):
    #
    $hdr = PDL::IO::FlexRaw::_read_flexhdr("filename.hdr")

    ($x,$y,...) = readflex("filename" [, $hdr])
    ($x,$y,...) = mapflex("filename" [, $hdr] [, $opts])

    $hdr = writeflex($file, $pdl1, $pdl2,...)
    writeflexhdr($file, $hdr)

    # if $PDL::IO::FlexRaw::writeflexhdr is true and
    #    $file is a filename, writeflexhdr() is called automatically
    #
    $hdr = writeflex($file, $pdl1, $pdl2,...)  # need $hdr for something
    writeflex($file, $pdl1, $pdl2,...)         # ..if $hdr not needed

=head1 DESCRIPTION

FlexRaw is a generic method for the input and output of `raw' data
arrays.  In particular, it is designed to read output from FORTRAN 77
UNFORMATTED files and the low-level C write function, even if the
files are compressed or gzipped.  As in FastRaw, the data file is
supplemented by a header file (although this can be replaced by the
optional C<$hdr> argument).  More information can be included in the
header file than for FastRaw -- the description can be extended to
several data objects within a single input file.

For example, to read the output of a FORTRAN program

    real*4 a(4,600,600)
    open (8,file='banana',status='new',form='unformatted')
    write (8) a
    close (8)

the header file (`banana.hdr') could look like

    # FlexRaw file header
    # Header word for F77 form=unformatted
    Byte 1 4
    # Data
    Float 3            # this is ignored
             4 600 600
    Byte 1 4           As is this, as we've got all dims

The data can then be input using

    $x = (readflex('banana'))[1];

The format of the hdr file is an extension of that used by FastRaw.
Comment lines (starting with #) are allowed, as are descriptive names
(as elsewhere: byte, short, ushort, long, float, double) for the data
types -- note that case is ignored by FlexRaw.  After the type, one
integer specifies the number of dimensions of the data `chunk', and
subsequent integers the size of each dimension.  So the specifier
above (`Float 3 4 600 600') describes our FORTRAN array.  A scalar can
be described as `float 0' (or `float 1 1', or `float 2 1 1', etc.).

When all the dimensions are read -- or a # appears after whitespace --
the rest of the current input line is ignored, I<unless> badvalues
are being read or written.  In that case, the next token will be the
string C<badvalue> followed by the bad value used, if needed.

What about the extra 4 bytes at the head and tail, which we just threw
away?  These are added by FORTRAN (at least on Suns, Alphas and
Linux), and specify the number of bytes written by each WRITE -- the
same number is put at the start and the end of each chunk of data.
You I<may> need to know all this in some cases.  In general, FlexRaw
tries to handle it itself, if you simply add a line saying `f77' to
the header file, I<before> any data specifiers:

    # FlexRaw file header for F77 form=unformatted
    F77
    # Data
    Float 3
    4 600 600

-- the redundancy in FORTRAN data files even allows FlexRaw to
automatically deal with files written on other machines which use
back-to-front byte ordering.  This won't always work -- it's a 1 in 4
billion chance it won't, even if you regularly read 4Gb files!  Also,
it currently doesn't work for compressed files, so you can say `swap'
(again before any data specifiers) to make certain the byte order is
swapped.

The optional C<$hdr> argument allows the use of an anonymous array to
give header information, rather than using a .hdr file.  For example,

    $header = [
        {Type => 'f77'},
        {Type => 'float', NDims => 3, Dims => [ 4,600,600 ] }
    ];
    @a = readflex('banana',$header);

reads our example file again.  As a special case, when NDims is 1, Dims
may be given as a scalar.

The highest dimension can be given as C<undef>, which will read as many
frames as possible of the given size (but only if only one hash-ref is given):

  $video = readflex('frames.raw', [
    { Type=>'byte', NDims=>4, Dims=>[4,640,480,undef] },
  ]);

Within PDL, readflex and writeflex can be used to write several pdls
to a single file -- e.g.

    use PDL;
    use PDL::IO::FlexRaw;

    @pdls = ($pdl1, $pdl2, ...);
    $hdr = writeflex("fname",@pdls);
    @pdl2 = readflex("fname",$hdr);

    writeflexhdr("fname",$hdr);  # not needed if $PDL::IO::FlexRaw::writeflexhdr is set
    @pdl3 = readflex("fname");

-- C<writeflex> produces the data file and returns the file header as an
anonymous hash, which can be written to a .hdr file using
C<writeflexhdr>.

If the package variable C<$PDL::IO::FlexRaw::writeflexhdr>
is true, and the C<writeflex> call was with a I<filename> and not
a handle, C<writeflexhdr> will be called automatically (as done by
C<writefraw>.

The reading of compressed data is switched on automatically if the
filename requested ends in .gz or .Z, or if the originally specified
filename does not exist, but one of these compressed forms does.

If C<writeflex> and C<readflex> are given a reference to a
file handle as a first parameter instead of a filename, then
the data is read or written to the open filehandle.  This
gives an easy way to read an arbitrary slice in a big data
volume, as in the following example:

    use PDL;
    use PDL::IO::FastRaw;

    open(DATA, "raw3d.dat");
    binmode(DATA);

    # assume we know the data size from an external source
    ($width, $height, $data_size) = (256,256, 4);

    my $slice_num = 64;   # slice to look at
    # Seek to slice
    seek(DATA, $width*$height*$data_size * $slice_num, 0);
    $pdl = readflex \*DATA, [{Dims=>[$width, $height], Type=>'long'}];

WARNING: In later versions of perl (5.8 and up) you must
be sure that your file is in "raw" mode (see the perlfunc
man page entry for "binmode", for details).  Both readflex
and writeflex automagically switch the file to raw mode for
you -- but in code like the snippet above, you could end up
seeking the wrong byte if you forget to make the binmode() call.

C<mapflex> memory maps, rather than reads, the data files.  Its interface
is similar to C<readflex>.  Extra options specify if the data is to be
loaded `ReadOnly', if the data file is to be `Creat'-ed anew on the
basis of the header information or `Trunc'-ated to the length of the
data read.  The extra speed of access brings with it some limitations:
C<mapflex> won't read compressed data, auto-detect f77 files, or read f77
files written by more than a single unformatted write statement.  More
seriously, data alignment constraints mean that C<mapflex> cannot read
some files, depending on the requirements of the host OS (it may also
vary depending on the setting of the `uac' flag on any given machine).
You may have run into similar problems with common blocks in FORTRAN.

For instance, floating point numbers may have to align on 4 byte
boundaries -- if the data file consists of 3 bytes then a float, it
cannot be read.  C<mapflex> will warn about this problem when it occurs,
and return the PDLs mapped before the problem arose.  This can be
dealt with either by reorganizing the data file (large types first
helps, as a rule-of-thumb), or more simply by using C<readflex>.

=head2 Fortran code to create data

Until PDL 2.099, the test file F<t/flexraw_fortran.t> compiled a
Fortran program, ran it, then byte-swapped its output, to test this
module's ability to do that. Version 2.099 has dropped external
dependencies, including the use of Fortran. The code it used is
shown here for historical curiosity:

  c Program to test i/o of F77 unformatted files
        program rawtest
        implicit none
        integer i
        $f77type a($ndata)
        do i = 1, $ndata
          a(i) = $val
        enddo
        open(8,file=
       \$'$data'
       \$,status='new',form='unformatted')
        i = $ndata
        write (8) i
        write (8) a
        close(8)
        end

with this FlexRaw header:

  # FlexRaw file header
  f77
  long 1 1
  # Data
  $pdltype 1 $ndata

C<$ndata> was set to 10, C<$val> was C<100.*sin(0.01* i)>, C<$data>
was a filename. C<$f77type> was set to C<real*4> and C<real*8>.

There was also a more complex program:

  c Program to test i/o of F77 unformatted files
        program rawtest
        implicit none
        character a
        integer*2 i
        integer*4 l
        real*4    f
        real*8    d
        d = 4*atan(1.)
        f = d
        l = 10**d
        i = l
        a = ' '
        open(8,file=
       \$'$data'
       \$,status='new',form='unformatted')
  c Choose bad boundaries...
        write (8) a,i,l,f,d
        close(8)
        end

with this FlexRaw header:

  # FlexRaw file header
  byte 1 4
  byte 0
  short 0
  long 0
  float 0
  double 0
  byte 1 4

=head1 FUNCTIONS

=cut

package PDL::IO::FlexRaw;
use strict;
use warnings;
use PDL;
use Exporter;
use PDL::Types ':All';
use PDL::IO::Misc qw(bswap4);

our @ISA = qw/Exporter/;
our @EXPORT = qw/writeflex writeflexhdr readflex mapflex glueflex/;

# Cast type numbers in concrete, for external file's sake...
my %flexnames = map +($_->enum => $_->ioname), types();
my %flextypes = map +($_->ioname => $_->enum,
  $_->enum => $_->enum,
  $_->ppsym => $_->enum,
), types();
my %flexswap = map {
  my $nb = PDL::Core::howbig(my $val = $_->enum);
  $nb > 1 ? ($val =>  "bswap$nb") : ()
} types();

our $verbose = 0;
our $writeflexhdr //= 0;

sub _read_flexhdr {
  my ($hname) = @_;
  open my $hfile, $hname or barf "Couldn't open '$hname' for reading: $!";
  binmode $hfile;
  my ($newfile, $tid, @str, @ret) = 1;
  # check for ENVI files and bail (for now)
  my $line1 = scalar <$hfile>;
  barf "This is an ENVI format file, please use readenvi()\n" if $line1 =~ /^ENVI\r?$/;
  seek $hfile, 0, 0;  # reset file pointer to beginning
  ITEM: while (!eof($hfile)) {
    my ($ndims, $mode, @dims) = (-1, -2);
    my ($have_badvalue) = undef;
    my ($badvalue) = undef;
    LINE: while (<$hfile>) {
      next LINE if /^#/ or /^\s*$/;
      chop;
      tr/A-Z/a-z/;
      @str = split;
      my $numtokens = scalar @str;
      TOKEN: foreach my $token (@str) {
        next LINE if $token =~ /^#/;
        if ($mode == -2) { # type
          if ($newfile) {
             if ($token eq 'f77' || $token eq 'swap') {
                push @ret, {
                   Type => $token
                };
                $numtokens--;
                next ITEM;
             }
          }
          barf("Bad typename '$token' in readflex") if (!exists($flextypes{$token}));
          $tid = $flextypes{$token};
          $numtokens--;
          $newfile = 0;
          $mode++;
        } elsif ($mode == -1) { #ndims
          barf("Not number for ndims in readflex") if $token !~ /^\d*$/;
          $ndims = $token;
          barf("Bad ndims in readflex") if ($ndims < 0);
          $numtokens--;
          $mode++;
          last LINE if $mode == $ndims and $numtokens == 0;
        } elsif ($mode < $ndims) { # get dims
          barf("Not number for dimension in readflex")
          if $token !~ /^\d*$/;
          push(@dims,$token);
          $numtokens--;
          $mode++;
          last LINE if $mode == $ndims and $numtokens == 0;
        } elsif ($mode == $ndims and ! $have_badvalue) {  # check for badvalue info
          if ($token =~ /^badvalue$/ ) {
             $have_badvalue = 1;
             $numtokens--;
             last LINE if $numtokens==0;  # using default bad value
          } else {
             last LINE;
          }
        } elsif ($mode == $ndims and $have_badvalue and $numtokens > 0) {
          $badvalue = $token;
          last LINE;
        }
      }
    }
    last ITEM if $mode == -2;
    barf "Bad format in readflex header file ($ndims, $mode)"
      if $ndims < 0 || $mode != $ndims;
    push @ret, {
      Type => $tid,
      Dims => \@dims,
      NDims => $ndims,
      BadFlag => (($have_badvalue) ? 1 : 0),
      BadValue => $badvalue,
    };
  }
  return \@ret;
}

sub readchunk {
  my ($d, $pdl, $len, $name, $offset) = @_;
  my ($nread);
  print "Reading $len at $offset from $name\n"
    if $verbose;
  ($nread = read($d, ${$pdl->get_dataref}, $len)) == $len
      or barf "Couldn't read $len bytes at offset $offset from '$name', got $nread";
  $pdl->upd_data();
  $len;
}

our $flexmapok;
sub myhandler {
  $flexmapok = 0;
  barf "Data out of alignment, can't map further\n";
}
sub mapchunk {
  my ($svref, $type, $dims, $offset, $len) = @_;
  my $pdl = PDL->new_around_datasv(0+$svref, $offset);
  $pdl->set_datatype($type);
  $pdl->setdims(ref $dims ? $dims : [$dims]);
  $pdl->set_donttouchdata($len);
  local $flexmapok=1;
  local $SIG{BUS} = \&myhandler unless $^O =~ /MSWin32/i;
  local $SIG{FPE} = \&myhandler;
  eval {$pdl->at((0) x $pdl->ndims)}; # "->flat" allocs copy of whole array
  $flexmapok ? $pdl : undef;
}

=head2 glueflex

=for ref

Append a single data item to an existing binary file written by
L</writeflex>.  Must be to the last data item in that file. Error if
dims not compatible with existing data.

=for usage

    $hdr = glueflex($file, $pdl[, $hdr]); # or
    $hdr = glueflex(FILEHANDLE, $pdl[, $hdr]);
    # now you must call writeflexhdr()
    writeflexhdr($file, $hdr);

or

    $PDL::IO::FlexRaw::writeflexhdr = 1; # set so we don't have to call writeflexhdr
    $hdr = glueflex($file, $pdl[, $hdr])  # remember, $file must be filename
    glueflex($file, $pdl[, $hdr])         # remember, $file must be filename

=cut

sub glueflex {
  my $usage = 'Usage $hdr = glueflex("filename"|FILEHANDLE,$pdl[,$hdr])';
  my ($name,$pdl,$hdr) = @_;
  barf $usage if @_ < 2 or @_ > 3 or !UNIVERSAL::isa($pdl, 'PDL');
  my $isname = 0;
  my $d;
  # Test if $name is a file handle
  if (defined fileno($name)) {
    $d = $name;
  } else {
    barf $usage if ref $name;
    barf "'$name' must be real filename: $!" if !-f $name;
    $isname = 1;
    open $d, '>>', $name or barf "Couldn't open '$name' for appending: $!";
  }
  binmode $d;
  $hdr ||= _read_flexhdr("$name.hdr");
  my $hash = $hdr->[-1] || barf "glueflex: need valid header-hash";
  barf "glueflex: ndarray has type '@{[$pdl->type]}' but last hash has type '$hash->{Type}'"
    if $pdl->type != PDL::Type->new($hash->{Type});
  my @dims = ref $hash->{Dims} ? @{$hash->{Dims}} : $hash->{Dims};
  barf "glueflex: header dims needs at least 2 dims, got (@dims)" if @dims < 2;
  my @ldims = @dims[0..$#dims-1];
  barf "glueflex: incompatible lower dims, ndarray (@{[$pdl->dims]}) vs header (@ldims)"
    if !all($pdl->shape == pdl(@ldims));
  print $d ${$pdl->get_dataref};
  $dims[-1]++;
  $hash->{Dims} = \@dims;
  if (defined wantarray) {
    # list or scalar context
    writeflexhdr($name, $hdr) if $isname and $writeflexhdr;
    return $hdr;
  } else {
    # void context so write header file
    writeflexhdr($name, $hdr) if $isname;
    return;
  }
}

=head2 readflex

=for ref

Read a binary file with flexible format specification

=for usage

    Usage:

    ($x,$y,...) = readflex("filename" [, $hdr])
    ($x,$y,...) = readflex(FILEHANDLE [, $hdr])

=cut

sub readflex {
  barf 'Usage ($x,$y,...) = readflex("filename"|FILEHANDLE [, \@hdr])'
    if @_ > 2;
  my ($name,$h) = @_;
  my ($hdr, $pdl, $len, @out, $chunk, $chunkread, $data);
  my $offset = 0;
  my ($newfile, $swapbyte, $f77mode, $zipt) = (1,0,0,0);
  my $d;
  # Test if $name is a file handle
  if (defined fileno($name)) {
    $d = $name;
  } else {
    $name =~ s/\.(gz|Z)$//; # strip any trailing compression suffix
    $data = $name;
    if (! -e $name ) {  # If it's still not found, then...
      suffix: for my $suffix (grep -e "$name.$_", 'gz','Z') {
        ## This little fillip detects gzip if we need it, and caches
        ## the version in a package-global variable.  The return string
        ## is undefined if there is no gzip in the path.
        our $gzip_version;
        unless (defined($gzip_version)) {
          # Try running gzip -V to get the version.  Redirect STDERR to STDOUT since
          # Apple'z gzip writes its version to STDERR.
          $gzip_version = `gzip -V 2>&1`;
          unless(defined($gzip_version)) {
            # That may or may not work on Microsoft Windows, so if it doesn't,
            # try running gzip again without the redirect.
            $gzip_version = `gzip -V`;
          }
          barf "FlexRaw: couldn't find the external gzip utility (to parse $name.$suffix)!" unless(defined($gzip_version));
        }
        if($gzip_version =~ m/^Apple/) {
          # Apple gzip requires a suffix
          $data = "gzip -dcq $name.$suffix |";
        } else {
          # Other gzips apparently don't require a suffix - they find it automagically.
          $data = "gzip -dcq $name |";
        }
        $zipt = 1;
        last suffix;
      }
    }
    my $size = (stat $name)[7];
    open $d, $data
        or barf "Couldn't open '$data' for reading: $!";
    $h ||= _read_flexhdr("$name.hdr");
  }
  binmode $d;
  barf "Last dim given as undef but >1 header-hash given"
    if ref $h->[0]{Dims} and @{$h->[0]{Dims}} and !defined $h->[0]{Dims}[-1] and @$h > 1;
  # Go through headers which reconfigure
  foreach $hdr (@$h) {
    my ($type) = $hdr->{Type};
    if ($type eq 'swap') {
      $swapbyte = 1;
    } elsif ($type ne 'f77') {
      last;
    }
  }
  READ: foreach $hdr (@$h) {
    my ($type) = $hdr->{Type};
    # Case convert when we have user data
    $type =~ tr/A-Z/a-z/ if @_ == 2;
    if ($newfile) {
      if ($type eq 'f77') {
        $hdr = { Type => $PDL_L, Dims => [ ], NDims => 0 };
        $type = $PDL_L;
        $f77mode = 1;
      } elsif ($type eq 'swap') {
        next READ;
      } else {
        $newfile = 0;
      }
    }
    if (@_ == 2) {
      barf "Bad typename '$type' in readflex" if !defined $flextypes{$type};
      $type = $flextypes{$type};
    }
    my @dims = ref $hdr->{Dims} ? @{$hdr->{Dims}} : $hdr->{Dims};
    my @rdims = @dims[0..($#dims - (defined $dims[-1] ? 0 : 1))];
    $len = pdl(PDL::Core::howbig($type), @rdims)->prodover->sclr;
    if (@dims and !defined $dims[-1]) {
      my ($count, @pdls) = 0;
      while (!eof $d) {
        push @pdls, PDL->zeroes(PDL::Type->new($type), @rdims);
        $offset += readchunk($d,$pdls[-1],$len,$name, $offset);
        $count++;
      }
      $pdl = pdl(@pdls);
      $len *= $count;
    } else {
      $pdl = PDL->zeroes(PDL::Type->new($type), @dims);
      $offset += readchunk($d,$pdl,$len,$name, $offset);
    }
    $chunkread += $len;
    if ($swapbyte) {
      my $method = $flexswap{$type};
      $pdl->$method if $method;
    }
    if ($newfile && $f77mode) {
      if ($zipt || $swapbyte) {
        $chunk = $pdl->copy;
        $chunkread = 0;
        next READ;
      } else {
        SWAP: foreach (0,1) {
          seek($d,4,0);
          $swapbyte = $_;
          bswap4($pdl) if $swapbyte;
          $chunk = $pdl->copy;
          next SWAP if !seek($d,$pdl->at,1);
          next SWAP if read($d,${$chunk->get_dataref},$len) != $len;
          $chunk->upd_data;
          bswap4($chunk) if $swapbyte;
          next SWAP if $pdl->at != $chunk->at;
          $chunkread = 0;
          barf "Error can't rewind" if !seek($d,4,0);
          next READ;
        }
        barf "Error: Doesn't look like f77 file (even swapped)";
      }
    }
    if ($hdr->{BadFlag}) {  # set badflag and badvalue if needed
      $pdl->badflag($hdr->{BadFlag});
      $pdl->badvalue($hdr->{BadValue}) if defined $hdr->{BadValue};
    }
    push @out, $pdl;
    if ($f77mode && $chunk->at == $chunkread) {
      $chunkread = 0;
      my ($check) = $chunk->copy;
      $offset += readchunk($d,$check,4,$name,$offset);
      bswap4($check) if $swapbyte;
      if ($check->at ne $chunk->at) {
        barf "F77 file format error for $check cf $chunk";
        last READ;
      }
      last READ if eof $d;
      $offset += readchunk($d,$chunk,4,$name,$offset);
      bswap4($chunk) if $swapbyte;
    }
  }
  wantarray ? @out : $out[0];
}

=head2 mapflex

=for ref

Memory map a binary file with flexible format specification

=for usage

    Usage:

    ($x,$y,...) = mapflex("filename" [, $hdr] [, $opts])

=for options

    All of these options default to false unless set true:

    ReadOnly - Data should be readonly
    Creat    - Create file if it doesn't exist
    Trunc    - File should be truncated to a length that conforms
               with the header

=cut

sub mapflex {
  my $name = shift;
  # reference to header array
  my ($h, $size);
  # reference to options array, with defaults
  my %opts = ( 'ReadOnly' => 0, 'Creat' => 0, 'Trunc' => 0 );
  my (@out, $chunk, $chunkread);
  my $offset = 0;
  my ($newfile, $swapbyte, $f77mode, $zipt) = (1,0,0,0);
  foreach (@_) {
    if (ref($_) eq "ARRAY") {
      $h = $_;
    } elsif (ref($_) eq "HASH") {
      %opts = (%opts,%$_);
    } else {
      warn 'Usage ($x,$y,...) = mapflex("filename" [, \@hdr] [,\%opts])';
    }
  }
  barf "Can't map compressed file"
    if $name =~ s/\.gz$// || $name =~ s/\.Z$// ||
      (!-e $name && (-e $name.'.gz' || -e $name.'.Z'));
  $h = _read_flexhdr("$name.hdr") if !defined $h;
  # Go through headers which reconfigure
  for my $hdr (@$h) {
    my $type = $hdr->{Type};
    barf "Can't map byte swapped file" if $type eq 'swap';
    if ($type eq 'f77') {
      $f77mode = 1;
    } else {
      barf "Bad typename '$type' in mapflex" if !defined $flextypes{$type};
      $type = $flextypes{$type};
      $size += _data_size_in_bytes($type, $hdr->{Dims});
    }
  }
  # $s now contains estimated size of data in header --
  # setting $f77mode means that it will be 8 x n bigger in reality
  $size += 8 if $f77mode;
  if (!$opts{Creat}) {
    my $s = $size;
    $size = (stat $name)[7];
    barf "File looks too small ($size cf header $s)" if $size < $s;
  }
  my $mapped_sv; do {
    my $writable = $opts{ReadOnly}?0:1;
    my $creat = $opts{Creat}?1:0;
    my $trunc = $opts{Creat} || $opts{Trunc} ? 1:0;
    my $fh = PDL::Core::_file_map_open($name,$size,1,$writable,$creat,0644,$trunc);
    PDL::Core::_file_map_sv(\$mapped_sv,$fh,$size,1,$writable);
  };
  READ: for my $hdr (@$h) {
    my ($type) = $hdr->{Type};
    # Case convert when we have user data
    $type =~ tr/A-Z/a-z/ if @_ == 2;
    if ($newfile) {
      if ($type eq 'f77') {
        $hdr = { Type => $PDL_L, Dims => [], NDims => 0 };
        $type = $PDL_L;
      } else {
        $newfile = 0;
      }
    }
    if (@_ == 2) {
      barf "Bad typename '$type' in mapflex" if !defined $flextypes{$type};
      $type = $flextypes{$type};
    }
    my $len = _data_size_in_bytes($type, $hdr->{Dims});
    my $pdl = mapchunk(\$mapped_sv,$type,$hdr->{Dims},$offset,$len);
    last READ if !defined $pdl;
    $offset += $len;
    $chunkread += $len;
    if ($newfile && $f77mode) {
      if ($opts{Creat}) {
        $pdl->set(0,$size - 8);
      } else {
        $chunk = $pdl->copy;
      }
      $chunkread = 0;
      next READ;
    }
    if ($hdr->{BadFlag}) {  # set badflag and badvalue if needed
      $pdl->badflag($hdr->{BadFlag});
      $pdl->badvalue($hdr->{BadValue}) if defined $hdr->{BadValue};
    }
    push @out, $pdl;
    if ($f77mode && $chunk->at == $chunkread) {
      $chunkread = 0;
      my $check = mapchunk(\$mapped_sv,$type,$hdr->{Dims},$offset,4);
      last READ if !defined $check;
      $offset += 4;
      if ($opts{Creat}) {
        $check->set(0,$size-8);
      } else {
        barf "F77 file format error for $check cf $chunk"
          if $check->at ne $chunk->at;
      }
      barf "Will only map first f77 data statement" if $offset < $size;
      last READ;
    }
  }
  wantarray ? @out : $out[0];
}

sub _data_size_in_bytes {
  my ($type, $dims) = @_;
  my $si = 1;
  $si *= $_ for ref $dims ? @$dims : $dims;
  $si * PDL::Core::howbig($type);
}

=head2 writeflex

=for ref

Write a binary file with flexible format specification

=for usage

    Usage:

    $hdr = writeflex($file, $pdl1, $pdl2,...) # or
    $hdr = writeflex(FILEHANDLE, $pdl1, $pdl2,...)
    # now you must call writeflexhdr()
    writeflexhdr($file, $hdr)

or

    $PDL::IO::FlexRaw::writeflexhdr = 1;  # set so we don't have to call writeflexhdr

    $hdr = writeflex($file, $pdl1, $pdl2,...)  # remember, $file must be filename
    writeflex($file, $pdl1, $pdl2,...)         # remember, $file must be filename

=cut

sub writeflex {
  my $usage = 'Usage $hdr = writeflex("filename"|FILEHANDLE,$pdl,...)';
  barf $usage if !@_;
  my $name = shift;
  my $isname = 0;
  my $hdr;
  my $fh;
  # Test if $name is a file handle
  if (defined fileno($name)) {
    $fh = $name;
  } else {
    barf $usage if ref $name;
    $isname = 1;
    my $modename = ($name =~ /^[+]?[><|]/) ? $name : ">$name";
    open $fh, $modename or barf "Couldn't open '$name' for writing: $!";
  }
  binmode $fh;
  foreach my $pdl (@_) {
    barf $usage if !ref $pdl;
    push @$hdr, {
      Type => $flexnames{$pdl->get_datatype},
      Dims => [ $pdl->dims ],
      NDims => $pdl->getndims,
      BadFlag => $pdl->badflag,
      BadValue => (($pdl->badvalue == $pdl->orig_badvalue) ? undef : $pdl->badvalue),
    };
    print $fh ${$pdl->get_dataref};
  }
  if (defined wantarray) {
    # list or scalar context
    writeflexhdr($name, $hdr) if $isname and $writeflexhdr;
    return $hdr;
  } else {
    # void context so write header file
    writeflexhdr($name, $hdr) if $isname;
    return;
  }
}

=head2 writeflexhdr

=for ref

Write the header file corresponding to a previous writeflex call

=for usage

    Usage:

    writeflexhdr($file, $hdr)

    $file or "filename" is the filename used in a previous writeflex
    If $file is actually a "filename" then writeflexhdr() will be
    called automatically if $PDL::IO::FlexRaw::writeflexhdr is true.
    If writeflex() was to a FILEHANDLE, you will need to call
    writeflexhdr() yourself since the filename cannot be determined
    (at least easily).

=cut

sub writeflexhdr {
  barf 'Usage writeflex("filename", $hdr)' if @_!=2 || !ref $_[1];
  my($name, $hdr) = @_;
  my $hname = "$name.hdr";
  open my $h, '>', $hname or barf "Couldn't open '$hname' for writing: $!";
  binmode $h;
  print $h "# Output from PDL::IO::writeflex, data in $name\n";
  foreach (@$hdr) {
    my $type = $_->{Type};
    barf "Writeflexhdr: will only print data elements, not $type"
      if !exists $flextypes{$type};
    print $h join("\n",$type, $_->{NDims},
      (join ' ',ref $_->{Dims} ? @{$_->{Dims}} : $_->{Dims}) . (($_->{BadFlag}) ? " badvalue $_->{BadValue}" : '')),
      "\n\n";
  }
}

=head1 BAD VALUE SUPPORT

As of PDL-2.4.8, L<PDL::IO::FlexRaw> has support for reading and writing
pdls with L<bad|PDL::Bad> values in them.

On C<writeflex>, an ndarray
argument with C<< $pdl->badflag == 1 >> will have the keyword/token "badvalue"
added to the header file after the dimension list and an additional token
with the bad value for that pdl if C<< $pdl->badvalue != $pdl->orig_badvalue >>.

On C<readflex>, a pdl with the "badvalue" token in the header will
automatically have its L<badflag|PDL::Bad/#badflag> set and its
L<badvalue|PDL::Bad/#badvalue> as well if it is not the standard default for that type.

=for example

The new badvalue support required some additions to the header
structure.  However, the interface is still being finalized.  For
reference the current C<$hdr> looks like this:

    $hdr = {
             Type => 'byte',    # data type
             NDims => 2,        # number of dimensions
             Dims => [640,480], # dims
             BadFlag => 1,      # is set/set badflag
             BadValue => undef, # undef==default
           };

    $badpdl = readflex('badpdl', [$hdr]);

If you use bad values and try the new L<PDL::IO::FlexRaw> bad value
support, please let us know via the perldl mailing list.
Suggestions and feedback are also welcome.

=head1 BUGS

The test on two dimensional byte arrays fail using g77 2.7.2, but not
Sun f77.  I hope this isn't my problem!

Assumes gzip is on the PATH.

Can't auto-swap compressed files, because it can't seek on them.

The header format may not agree with that used elsewhere.

Should it handle handles?

Mapflex should warn and fallback to reading on SEGV?  Would have to
make sure that the data was written back after it was `destroyed'.

=head1 AUTHOR

Copyright (C) Robin Williams <rjrw@ast.leeds.ac.uk> 1997.
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

Documentation contributions copyright (C) David Mertens, 2010.

=cut

1;
