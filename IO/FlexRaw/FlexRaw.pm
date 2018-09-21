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
you -- but in code like the snipped above, you could end up
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

=head1 BUGS

The test on two dimensional byte arrays fail using g77 2.7.2, but not
Sun f77.  I hope this isn't my problem!

Assumes gzip is on the PATH.

Can't auto-swap compressed files, because it can't seek on them.

The header format may not agree with that used elsewhere.

Should it handle handles?

Mapflex should warn and fallback to reading on SEGV?  Would have to
make sure that the data was written back after it was `destroyed'.

=head1 FUNCTIONS

=head2 readflex

=for ref

Read a binary file with flexible format specification

=for usage

    Usage:

    ($x,$y,...) = readflex("filename" [, $hdr])
    ($x,$y,...) = readflex(FILEHANDLE [, $hdr])


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

=head2 _read_flexhdr

Read a FlexRaw header file and return a header structure.

=for usage

    Usage:

    $hdr = PDL::IO::FlexRaw::_read_flexhdr($file)

Note that C<_read_flexhdr> is supposed to be an internal function.  It
was not originally documented and it is not tested.  However, there
appeared to be no other method for obtaining a header structure from
a file, so I figured I would write a small bit of documentation on it.

=head1 Bad Value Support

As of PDL-2.4.8, L<PDL::IO::FlexRaw|PDL::IO::FlexRaw> has support for reading and writing
pdls with L<bad|PDL::Bad> values in them.

On C<writeflex>, a piddle
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

If you use bad values and try the new L<PDL::IO::FlexRaw|PDL::IO::FlexRaw> bad value
support, please let us know via the perldl mailing list.
Suggestions and feedback are also welcome.


=head1 AUTHOR

Copyright (C) Robin Williams <rjrw@ast.leeds.ac.uk> 1997.
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

Documentation contributions copyright (C) David Mertens, 2010.

=cut

package PDL::IO::FlexRaw;

BEGIN {
   our $have_file_map = 0;

   eval "use File::Map 0.57 qw(map_file)";
   $have_file_map = 1 unless $@;
}

use PDL;
use Exporter;
use FileHandle;
use PDL::Types ':All';
use PDL::IO::Misc qw(bswap2 bswap4 bswap8);

@PDL::IO::FlexRaw::ISA = qw/Exporter/;

@EXPORT = qw/writeflex writeflexhdr readflex mapflex/;

# Cast type numbers in concrete, for external file's sake...
%flexnames = ( map {(typefld($_,'numval') => typefld($_,'ioname'))}
	       typesrtkeys());
%flextypes = ( map {(typefld($_,'ioname') => typefld($_,'numval'),
		     typefld($_,'numval') => typefld($_,'numval'),
		     lc typefld($_,'ppsym') =>  typefld($_,'numval'),
		    )}
	       typesrtkeys());
%flexswap = ( map {my $val = typefld($_,'numval');
		   my $nb = PDL::Core::howbig($val);
		   ($val =>  $nb > 1 ? "bswap$nb" : undef)}
	      typesrtkeys());

# use Data::Dumper;
# print Dumper \%flexnames;
# print Dumper \%flextypes;
# print Dumper \%flexswap;

# %flexnames = (
#    $PDL_B => 'byte', $PDL_S => 'short',
#    $PDL_US => 'ushort', $PDL_L => 'long',
#    $PDL_F => 'float', $PDL_D => 'double');

# %flextypes = (
# 'byte'   => $PDL_B, '0' => $PDL_B, 'b' => $PDL_B, 'c' => $PDL_B,
# 'short'  => $PDL_S, '1' => $PDL_S, 's' => $PDL_S,
# 'ushort' => $PDL_US,'2' => $PDL_US,'u' => $PDL_US,
# 'long'   => $PDL_L, '3' => $PDL_L, 'l' => $PDL_L,
# 'float'  => $PDL_F, '4' => $PDL_F, 'f' => $PDL_F,
# 'double' => $PDL_D, '5' => $PDL_D, 'd' => $PDL_D
# );

$PDL::IO::FlexRaw::verbose = 0;
$PDL::IO::FlexRaw::writeflexhdr = defined($PDL::FlexRaw::IO::writeflexhdr) ? $PDL::FlexRaw::IO::writeflexhdr : 0;

sub _read_flexhdr {
    my ($hname) = @_;
    my $hfile = new FileHandle "$hname"
	or barf "Couldn't open '$hname' for reading";
    binmode $hfile;
    my ($newfile) = 1;
    my ($tid, @str);
    my (@ret);
    # check for ENVI files and bail (for now)
    my $line1 = scalar <$hfile>;
    barf "This is an ENVI format file, please use readenvi()\n" if $line1 =~ /^ENVI\r?$/;
    seek $hfile, 0, 0;  # reset file pointer to beginning
 ITEM:
 while (!eof($hfile)) {
    my (@dims) = (); my ($ndims) = -1, ($mode) = -2;
    my ($have_badvalue) = undef;
    my ($badvalue) = undef;
    LINE:
    while (<$hfile>) {
       ### print STDERR "processing line '$_'\n";
       next LINE if /^#/ or /^\s*$/;
       chop;
       tr/A-Z/a-z/;
       @str = split;
       TOKEN:
       ### print STDERR "Got tokens: " . join(',',@str) . "\n";
       my $numtokens = scalar @str;
       foreach my $token (@str) {
          next LINE if $token =~ /^#/;
          if ($mode == -2) { # type
             ### print STDERR "  \$mode == -2:  #tokens=$numtokens, '$token'\n";
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
             ### print STDERR "  \$mode == -1:  #tokens=$numtokens, '$token'\n";
             barf("Not number for ndims in readflex") if $token !~ /^\d*$/;
             $ndims = $token;
             barf("Bad ndims in readflex") if ($ndims < 0);
             $numtokens--;
             $mode++;
             if ($mode == $ndims and $numtokens == 0) {
                last LINE;
             }
          } elsif ($mode < $ndims) { # get dims
             ### print STDERR "  # get dims:  #tokens=$numtokens, '$token'\n";
             barf("Not number for dimension in readflex")
             if $token !~ /^\d*$/;
             push(@dims,$token);
             $numtokens--;
             $mode++;
             if ($mode == $ndims and $numtokens == 0) {
                last LINE;
             }
          } elsif ($mode == $ndims and ! $have_badvalue) {  # check for badvalue info
             ### print STDERR "  # ! \$have_badvalue:  #tokens=$numtokens, '$token'\n";
             if ($token =~ /^badvalue$/ ) {
                $have_badvalue = 1;
                $numtokens--;
                last LINE if $numtokens==0;  # using default bad value
             } else {
                last LINE;
             }
          } elsif ($mode == $ndims and $have_badvalue and $numtokens > 0) {
             ### print STDERR "  #   \$have_badvalue:  #tokens = $numtokens, '$token'\n";
             $badvalue = $token;
             last LINE;
          }
       }
    }
    last ITEM if $mode == -2;
    barf("Bad format in readflex header file ($ndims, $mode)") if ($ndims < 0 || $mode != $ndims);
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
    my ($d, $pdl, $len, $name) = @_;
    my ($nread);
    print "Reading $len at $offset from $name\n"
      if $PDL::IO::FlexRaw::verbose;
    ($nread = read($d, ${$pdl->get_dataref}, $len)) == $len
	or barf "Couldn't read $len bytes at offset $offset from '$name', got $nread";
    $pdl->upd_data();
    $offset += $len;
    return 1;
}

sub myhandler {
    $flexmapok = 0;
    barf "Data out of alignment, can't map further\n";
    die;
}

sub mapchunk {
    my ($orig, $pdl, $len, $name) = @_;
    # link $len at $offset from $orig to $pdl.
    # print "linking $len bytes from $offset\n";
    $pdl->set_data_by_offset($orig,$offset);
    local ($flexmapok)=1;
    local $SIG{BUS} = \&myhandler unless $^O =~ /MSWin32/i;
    local $SIG{FPE} = \&myhandler;
    eval {$pdl->clump(-1)->at(0)};
    $offset += $len;
    $flexmapok;
}

sub readflex {
    barf 'Usage ($x,$y,...) = readflex("filename"|FILEHANDLE [, \@hdr])'
		if $#_ > 1;
    my ($name,$h) = @_;
    my ($hdr, $pdl, $len, @out, $chunk, $chunkread, $data);
    local ($offset) = 0;
    my ($newfile, $swapbyte, $f77mode, $zipt) = (1,0,0,0);
    my $d;
    # print("readflex: name is $name\n");
    # Test if $name is a file handle
    if (defined fileno($name)) {
		$d = $name;
		binmode($d);
    }
    else {
	$name =~ s/\.(gz|Z)$//; # strip any trailing compression suffix
	$data = $name;
	if(! -e $name ) {  # If it's still not found, then...
	  suffix: for my $suffix('gz','Z') {
	      if( -e "$name.$suffix" ) {

		  ## This little fillip detects gzip if we need it, and caches
		  ## the version in a package-global variable.  The return string
		  ## is undefined if there is no gzip in the path.
                  our $gzip_version;
		  unless(defined($gzip_version)) {
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
	}
	my ($size) = (stat $name)[7];
	$d = new FileHandle $data
	    or barf "Couldn't open '$data' for reading";
	binmode $d;
	$h = _read_flexhdr("$name.hdr")
	    unless $h;
    }

# Go through headers which reconfigure
    foreach $hdr (@$h) {
		my ($type) = $hdr->{Type};
		if ($type eq 'swap') {
			$swapbyte = 1;
		} elsif ($type ne 'f77') {
			last;
		}
    }

READ:
    foreach $hdr (@$h) {
	my ($type) = $hdr->{Type};
	# Case convert when we have user data
	$type =~ tr/A-Z/a-z/ if $#_ == 1;
	if ($newfile) {
	    if ($type eq 'f77') {
		$hdr = {
		    Type => $PDL_L,
		    Dims => [ ],
		    NDims => 0
		    };
		$type = $PDL_L;
		$f77mode = 1;
	    } elsif ($type eq 'swap') {
		next READ;
	    } else {
		$newfile = 0;
	    }
	}
	if ($#_ == 1) {
	    barf("Bad typename '$type' in readflex")
		if (!defined($flextypes{$type}));
	    $type = $flextypes{$type};
	}
	$pdl = PDL->zeroes ((new PDL::Type($type)),
			    ref $hdr->{Dims} ? @{$hdr->{Dims}} : $hdr->{Dims});
	$len = length $ {$pdl->get_dataref};

	&readchunk($d,$pdl,$len,$name) or last READ;
	$chunkread += $len;
	if ($swapbyte) {
	  my $method = $flexswap{$type};
	  $pdl->$method if $method;
# 	    bswap2($pdl) if $pdl->get_datatype == $PDL_S;
# 	    bswap4($pdl) if $pdl->get_datatype == $PDL_L
# 		|| $pdl->get_datatype == $PDL_F;
# 	    bswap8($pdl) if $pdl->get_datatype == $PDL_D;
	}
	if ($newfile && $f77mode) {
	    if ($zipt || $swapbyte) {
		$chunk = $pdl->copy;
		$chunkread = 0;
		next READ;
	    } else {
	      SWAP:
		foreach (0,1) {
		    seek($d,4,0);
		    $swapbyte = $_;
		    bswap4($pdl) if $swapbyte;
		    $chunk = $pdl->copy;
		    next SWAP if ! seek($d,$pdl->at,1);
		    next SWAP if
			read($d,$ {$chunk->get_dataref},$len) != $len;
		    $chunk->upd_data;
		    bswap4($chunk) if $swapbyte;
		    next SWAP if ($pdl->at != $chunk->at);
		    $chunkread = 0;
		    barf "Error can't rewind" if !seek($d,4,0);
		    # print "OK".($swapbyte?", swapped":""),"\n";
		    next READ;
		}
		barf "Error: Doesn't look like f77 file (even swapped)";
	    }
	}

        if ($hdr->{BadFlag}) {  # set badflag and badvalue if needed
           $pdl->badflag($hdr->{BadFlag});
           $pdl->badvalue($hdr->{BadValue}) if defined $hdr->{BadValue};
        }
        push (@out,$pdl);

	if ($f77mode && $chunk->at == $chunkread) {
	    $chunkread = 0;
	    my ($check) = $chunk->copy;
	    &readchunk($d,$check,4,$name) or last READ;
	    bswap4($check) if $swapbyte;
	    if ($check->at ne $chunk->at) {
		barf "F77 file format error for $check cf $chunk";
		last READ;
	    }
	    if (!eof($d)) {
		&readchunk($d,$chunk,4,$name) or last READ;
		bswap4($chunk) if $swapbyte;
	    } else {
		last READ;
	    }
	}
    }
    wantarray ? @out : $out[0];
}

sub mapflex {
    my ($usage)
		= 'Usage ($x,$y,...) = mapflex("filename" [, \@hdr] [,\%opts])';
    my $name = shift;
    # reference to header array
    my ($h, $size);
    # reference to options array, with defaults
    my (%opts) = ( 'ReadOnly' => 0, 'Creat' => 0, 'Trunc' => 0 );

    my ($hdr, $d, $pdl, $len, @out, $chunk, $chunkread);
    local ($offset) = 0;
    my ($newfile, $swapbyte, $f77mode, $zipt) = (1,0,0,0);

    foreach (@_) {
		if (ref($_) eq "ARRAY") {
			$h = $_;
		} elsif (ref($_) eq "HASH") {
			%opts = (%opts,%$_);
		} else {
			warn $usage;
		}
    }

    if ($name =~ s/\.gz$// || $name =~ s/\.Z$// ||
	(! -e $name && (-e $name.'.gz' || -e $name.'.Z'))) {
		barf "Can't map compressed file";
    }

    if (!defined $h) {
		$h = _read_flexhdr("$name.hdr");
    }

# Go through headers which reconfigure
    foreach $hdr (@$h) {
		my ($type) = $hdr->{Type};
		if ($type eq 'swap') {
			barf "Can't map byte swapped file";
		} elsif ($type eq 'f77') {
			$f77mode = 1;
		} else {
			my($si) = 1;
			foreach (ref $hdr->{Dims} ? @{$hdr->{Dims}} : $hdr->{Dims}) {
			$si *= $_;
			}
			barf("Bad typename '$type' in mapflex")
				unless defined $flextypes{$type};
			$type = $flextypes{$type};
			$size += $si * PDL::Core::howbig ($type);
		}
    }
# $s now contains estimated size of data in header --
# setting $f77mode means that it will be 8 x n bigger in reality
    $size += 8 if ($f77mode);
    if (!($opts{Creat})) {
		my ($s) = $size;
		$size = (stat $name)[7];
		barf "File looks too small ($size cf header $s)" if $size < $s;
    }
    # print "Size $size f77mode $f77mode\n";

    $d = PDL->zeroes(byte());

    # print "Mapping total size $size\n";
    # use Data::Dumper;
    # print "Options: ", Dumper(\%opts), "\n";
    if ($have_file_map and not defined($PDL::force_use_mmap_code) ) {
       $d->set_data_by_file_map($name,
                            $size,
                            1,
                            ($opts{ReadOnly}?0:1),
                            ($opts{Creat}?1:0),
                            (0644),
                            ($opts{Creat} || $opts{Trunc} ? 1:0)
                         );
    } else {
       warn "mapflex: direct mmap support being deprecated, please install File::Map\n";
       $d->set_data_by_mmap($name,
                            $size,
                            1,
                            ($opts{ReadOnly}?0:1),
                            ($opts{Creat}?1:0),
                            (0644),
                            ($opts{Creat} || $opts{Trunc} ? 1:0)
                         );
    }
READ:
    foreach $hdr (@$h) {
		my ($type) = $hdr->{Type};
		# Case convert when we have user data
		$type =~ tr/A-Z/a-z/ if $#_ == 1;
		if ($newfile) {
			if ($type eq 'f77') {
			$hdr = {
				Type => $PDL_L,
				Dims => [ ],
				NDims => 0
				};
			$type = $PDL_L;
			} else {
			$newfile = 0;
			}
		}
		if ($#_ == 1) {
			barf("Bad typename '$type' in mapflex")
				unless defined $flextypes{$type};
			$type = $flextypes{$type};
		}
		my $pdl = PDL->zeroes ((new PDL::Type($type)),
					ref $hdr->{Dims} ? @{$hdr->{Dims}} : $hdr->{Dims});
		$len = length $ {$pdl->get_dataref};

		&mapchunk($d,$pdl,$len,$name) or last READ;
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
			push (@out,$pdl);

		if ($f77mode && $chunk->at == $chunkread) {
			$chunkread = 0;
			my ($check) = $chunk->copy;
			&mapchunk($d,$check,4,$name) or last READ;
			if ($opts{Creat}) {
				$check->set(0,$size-8);
				} else {
				if ($check->at ne $chunk->at) {
					barf "F77 file format error for $check cf $chunk";
					last READ;
				}
			}
			barf "Will only map first f77 data statement" if ($offset < $size);
			last READ;
		}
    }
    wantarray ? @out : $out[0];
}

sub writeflex {
   my $usage = 'Usage $hdr = writeflex("filename"|FILEHANDLE,$pdl,...)';
   barf $usage if $#_<0;
   my($name) = shift;
   my $isname = 0;
   my $hdr;
   my $d;

   # Test if $name is a file handle
   if (defined fileno($name)) {
      $d = $name;
      binmode $d;
   }
   else {
      barf $usage if ref $name;
      $isname = 1;
      my $modename = ($name =~ /^[+]?[><|]/) ? $name : ">$name";
      $d = new FileHandle $modename
         or barf "Couldn't open '$name' for writing";
      binmode $d;
   }
   foreach $pdl (@_) {
      barf $usage if ! ref $pdl;
      # print join(' ',$pdl->getndims,$pdl->dims),"\n";
      push @{$hdr}, {
         Type => $flexnames{$pdl->get_datatype},
         Dims => [ $pdl->dims ],
         NDims => $pdl->getndims,
         BadFlag => $pdl->badflag,
         BadValue => (($pdl->badvalue == $pdl->orig_badvalue) ? undef : $pdl->badvalue),
      };
      print $d $ {$pdl->get_dataref};
   }
   if (defined wantarray) {
      # list or scalar context
      writeflexhdr($name, $hdr) if $isname and $PDL::IO::FlexRaw::writeflexhdr;
      return $hdr;
   } else {
      # void context so write header file
      writeflexhdr($name, $hdr) if $isname;
      return;
   }
}

sub writeflexhdr {
    barf 'Usage writeflex("filename", $hdr)' if $#_!=1 || !ref $_[1];
    my($name) = shift; my ($hdr) = shift;
    my $hname = "$name.hdr";
    my $h = new FileHandle ">$hname"
	or barf "Couldn't open '$hname' for writing";
    binmode $h;
    print $h
	"# Output from PDL::IO::writeflex, data in $name\n";
    foreach (@$hdr) {
	my ($type) = $_->{Type};
	if (! exists $flextypes{$type}) {
	    barf "Writeflexhdr: will only print data elements, not $type";
	    next;
	}
	print $h join("\n",$_->{Type},
		      $_->{NDims},
		      (join ' ',ref $_->{Dims} ? @{$_->{Dims}} : $_->{Dims}) . (($_->{BadFlag}) ? " badvalue $_->{BadValue}" : '')),
	"\n\n";
    }
}

1;


