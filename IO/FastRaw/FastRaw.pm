=head1 NAME

PDL::IO::FastRaw -- A simple, fast and convenient io format for PerlDL.

=head1 SYNOPSIS

 use PDL;
 use PDL::IO::FastRaw;

 writefraw($pdl,"fname");         # write a raw file

 $pdl2 = readfraw("fname");       # read a raw file
 $pdl2 = PDL->readfraw("fname");

 gluefraw($pdlx, "fname");        # append to existing file
 $pdlx->gluefraw("fname");

 $pdl3 = mapfraw("fname2",{ReadOnly => 1}); # mmap a file, don't read yet

 $pdl4 = maptextfraw("fname3",{...}); # map a text file into a 1-D pdl.


=head1 DESCRIPTION

This is a very simple and fast io format for PerlDL.
The disk data consists of two files, a header metadata file
in ASCII and a binary file consisting simply of consecutive
bytes, shorts or whatever.

It is hoped that this will not only make for a simple PerlDL module
for saving and retrieving these files but also make it easy
for other programs to use these files.

The format of the ASCII header is simply

	<typeid>
	<ndims>
	<dim0> <dim1> ...

You should probably stick with the default header name.  You may want
to specify your own header, however, such as when you have a large
collection of data files with identical dimensions and data types.
Under these circumstances, simply specify the C<Header> option in the
options hash.

The binary files are in general
NOT interchangeable between different architectures since the binary
file is simply dumped from the memory region of the ndarray.
This is what makes the approach efficient.

It is also possible to mmap the file which can give a large
speedup in certain situations as well as save a lot of memory
by using a disk file as virtual memory. When a file is mapped,
parts of it are read only as they are accessed in the memory
(or as the kernel decides: if you are reading the pages in order,
it may well preread some for you).

Note that memory savings and copy-on-write are operating-system
dependent - see Core.xs and your operating system documentation
for exact semantics of whatever. Basically, if you write to a
mmapped file without C<ReadOnly>, the change will be reflected
in the file immediately. C<ReadOnly> doesn't really make it impossible
to write to the ndarray but maps the memory privately so the file
will not be changed when you change the ndarray. Be aware though
that mmapping a 40Mb file without C<ReadOnly> spends no virtual
memory but with C<ReadOnly> it does reserve 40Mb.

=head2 Example: Converting ASCII to raw

You have a whole slew of data files in ASCII from an experiment
that you ran in your lab.  You're still tweaking the analysis
and plots, so you'd like if your data could load as fast as
possible.  Eventually you'll read the data into your scripts
using C<readfraw>, but the first thing you might do is create
a script that converts all the data files to raw files:

 #!/usr/bin/perl
 # Assumes that the data files end with a .asc or .dat extension
 # and saves the raw file output with a .bdat extension.
 # call with
 #  >./convert_to_raw.pl file1.dat file2.dat ...
 # or
 #  >./convert_to_raw.pl *.dat
 
 use PDL;
 use PDL::IO::FastRaw;	# for saving raw files
 use PDL::IO::Misc;		# for reading ASCII files with rcols
 while(shift) {			# run through the entire supplied list of file names
	 ($newName = $_) =~ s/\.(asc|dat)/.bdat/;
	 print "Saving contents of $_ to $newName\n";
	 $data = rcols($_);
	 writefraw($data, $newName);
 }


=head2 Example: readfraw

Now that you've gotten your data into a raw file format, you can
start working on your analysis scripts.  If you scripts used C<rcols>
in the past, the reading portion of the script should go much,
much faster now:

 #!/usr/bin/perl
 # My plotting script.
 # Assume I've specified the files to plot on the command line like
 #  >./plot_script.pl file1.bdat file2.bdat ...
 # or
 #  >./plot_script.pl *.bdat
 
 use PDL;
 use PDL::IO::FastRaw;
 while(shift) {			# run through the entire supplied list of file names
	 $data = readfraw($_);
	 my_plot_func($data);
 }

=head2 Example: Custom headers

In the first example, I allow C<writefraw> to use the standard header
file name, which would be C<file.bdat.hdr>.  However, I often measure
time series that have identical length, so all of those header files
are redundant.  To fix that, I simply pass the Header option to the
C<writefraw> command.  A modified script would look like this:

 #!/usr/bin/perl
 # Assumes that the data files end with a .asc or .dat extension
 # and saves the raw file output with a .bdat extension.
 # call with
 #  >./convert_to_raw.pl [-hHeaderFile] <fileglob> [-hHeaderFile] <fileglob> ...
 
 use PDL;
 use PDL::IO::FastRaw;	# for saving raw files
 use PDL::IO::Misc;		# for reading ASCII files with rcols
 my $header_file = undef;
 CL_OPTION: while($_ = shift @ARGV) {	# run through the entire list of command-line options
 	 if(/-h(.*)/) {
		 $header_file = $1;
		 next CL_OPTION;
	 }
	 ($newName = $_) =~ s/\.(asc|dat)/.bdat/;
	 print "Saving contents of $_ to $newName\n";
	 $data = rcols($_);
	 writefraw($data, $newName, {Header => $header_file});
 }

Modifying the read script is left as an exercise for the reader.  :]


=head2 Example: Using mapfraw

Sometimes you'll want to use C<mapfraw> rather than the read/write
functions.  In fact, the original author of the module doesn't
use the read/write functions anymore, prefering to always use
C<mapfraw>.  How would you go about doing this?

Assuming you've already saved your data into the raw format, the
only change you would have to make to the script in example 2 would
be to change the call to C<readfraw> to C<mapfraw>.  That's it.
You will probably see differences in performance, though I (David
Mertens) couldn't tell you about them because I haven't played
around with C<mapfraw> much myself.

What if you eschew the use of C<writefraw> and prefer to only use
C<mapfraw>?  How would you save your data to a raw format?  In that
case, you would have to create a C<mapfraw> ndarray with the correct
dimensions first using 

 $ndarray_on_hd = mapfraw('fname', {Creat => 1, Dims => [dim1, dim2, ...]});

Note that you must specify the dimensions and you must tell
C<mapfraw> to create the new ndarray for you by setting the
C<Creat> option to a true value, not C<Create> (note the missing
final 'e').

=head1 FUNCTIONS

=cut

package PDL::IO::FastRaw;
use strict;
use warnings;

our $VERSION = '0.000003';
$VERSION = eval $VERSION;

require Exporter;
use PDL::Core '';
use PDL::Exporter;

our @ISA = qw/PDL::Exporter/;
our @EXPORT_OK = qw/writefraw readfraw mapfraw maptextfraw gluefraw/;
our %EXPORT_TAGS = (Func=>\@EXPORT_OK);

# Exported functions

*writefraw = \&PDL::writefraw;
*gluefraw = \&PDL::gluefraw;
sub readfraw {PDL->readfraw(@_)}
sub mapfraw  {PDL->mapfraw(@_)}
sub maptextfraw  {PDL->maptextfraw(@_)}

sub _read_frawhdr {
	my($name,$opts) = @_;
	my $hname = $opts->{Header} || "$name.hdr";
	open my $h, '<', $hname
	 or barf "Couldn't open '$hname' for reading: $!";
	chomp(my $tid = <$h>);
	chomp(my $ndims = <$h>);
	chomp(my $str = <$h>); if(!defined $str) {barf("Format error in '$hname'");}
	my @dims = split ' ',$str;
	if($#dims != $ndims-1) {
		barf("Format error reading fraw header file '$hname'");
	}
	return {
		Type => $tid,
		Dims => \@dims,
		NDims => $ndims
	};
}

sub _writefrawhdr {
	my($pdl,$name,$opts) = @_;
	my $hname = $opts->{Header} || "$name.hdr";
	open my $h, '>', $hname
	 or barf "Couldn't open '$hname' for writing: $!";
	print $h map "$_\n", $pdl->get_datatype,
		$opts->{NDims} // $pdl->getndims,
		join(' ', $opts->{Dims} ? @{$opts->{Dims}} : $pdl->dims);
}

=head2 writefraw

=for ref

Write a raw format binary file

=for usage

 writefraw($pdl,"fname");
 writefraw($pdl,"fname", {Header => 'headerfname'});

=for options

The C<writefraw> command
supports the following option:

=over 8

=item Header

Specify the header file name.

=back

=cut

sub PDL::writefraw {
	my($pdl,$name,$opts) = @_;
	_writefrawhdr($pdl,$name,$opts);
	open my $d, '>', $name
	 or barf "Couldn't open '$name' for writing: $!";
	binmode $d;
	print $d ${$pdl->get_dataref};
}

=head2 readfraw

=for ref

Read a raw format binary file

=for usage

 $pdl2 = readfraw("fname");
 $pdl2 = PDL->readfraw("fname");
 $pdl2 = readfraw("fname", {Header => 'headerfname'});

=for options

The C<readfraw> command
supports the following option:

=over 8

=item Header

Specify the header file name.

=back

=cut

sub PDL::readfraw {
  my $class = shift;
  my($name,$opts) = @_;
  open my $d, '<', $name or barf "Couldn't open '$name' for reading: $!";
  binmode $d;
  my $hdr = _read_frawhdr($name,$opts);
  my $pdl = $class->zeroes(PDL::Type->new($hdr->{Type}), @{$hdr->{Dims}});
  my $len = length ${$pdl->get_dataref};
  my $index = 0;
  my $data;
  my $retlen;
  while (($retlen = sysread $d, $data, $len) != 0) {
    substr(${$pdl->get_dataref},$index,$len) = $data;
    $index += $retlen;
    $len -= $retlen;
  }
  $pdl->upd_data();
  return $pdl;
}

=head2 gluefraw

=for ref

Append a single data item to an existing binary file written by
L</writefraw>. Error if dims not compatible with existing data.

=for usage

  gluefraw($file, $pdl[, $opts]);

=cut

sub PDL::gluefraw {
  my $usage = 'Usage: gluefraw($pdl,"filename"[,$opts])';
  my ($pdl,$name,$opts) = @_;
  barf $usage if @_ < 2 or @_ > 3 or !UNIVERSAL::isa($pdl, 'PDL') or ref $name;
  barf "'$name' must be real filename: $!" if !-f $name;
  $opts ||= {};
  my $hdr = _read_frawhdr($name,$opts);
  barf "gluefraw: ndarray has type '@{[$pdl->type]}' but file has type '$hdr->{Type}'"
    if $pdl->type != PDL::Type->new($hdr->{Type});
  my @dims = ref $hdr->{Dims} ? @{$hdr->{Dims}} : $hdr->{Dims};
  barf "gluefraw: header dims needs at least 2 dims, got (@dims)" if @dims < 2;
  my @ldims = @dims[0..$#dims-1];
  barf "gluefraw: incompatible lower dims, ndarray (@{[$pdl->dims]}) vs header (@ldims)"
    if !PDL::all($pdl->shape == pdl(@ldims));
  open my $d, '>>', $name or barf "Couldn't open '$name' for appending: $!";
  binmode $d;
  print $d ${$pdl->get_dataref};
  $dims[-1]++;
  $hdr->{Dims} = \@dims;
  _writefrawhdr($pdl, $name, { %$opts, %$hdr });
}

=head2 mapfraw

=for ref

Memory map a raw format binary file (see the module docs also)

=for usage

 $pdl3 = mapfraw("fname2",{ReadOnly => 1});

=for options

The C<mapfraw> command
supports the following options (not all combinations make sense):

=over 8

=item Dims, Datatype

If creating a new file or if you want to specify your own header
data for the file, you can give an array reference and a scalar,
respectively.

=item Creat

Create the file. Also writes out a header for the file.

=item Trunc

Set the file size. Automatically enabled with C<Creat>. NOTE: This also
clears the file to all zeroes.

=item ReadOnly

Disallow writing to the file.

=item Header

Specify the header file name.

=back

=cut

sub PDL::mapfraw {
        my $class = shift;
	my($name,$opts) = @_;
	my $hdr;
	if($opts->{Dims}) {
		$hdr->{Type} = $opts->{Datatype} // double->enum;
		$hdr->{Dims} = $opts->{Dims};
		$hdr->{NDims} = scalar(@{$opts->{Dims}});
	} else {
		$hdr = _read_frawhdr($name,$opts);
	}
	my $s = PDL::Core::howbig($hdr->{Type});
	for(@{$hdr->{Dims}}) {
		$s *= $_;
	}
	my $pdl = $class->zeroes(PDL::Type->new($hdr->{Type}));
        $pdl->set_data_by_file_map(
            $name,
            $s,
            1,
            ($opts->{ReadOnly}?0:1),
            ($opts->{Creat}?1:0),
            (0644),
            ($opts->{Creat} || $opts->{Trunc} ? 1:0)
        );
	$pdl->setdims($hdr->{Dims});
	_writefrawhdr($pdl,$name,$opts) if $opts->{Creat};
	$pdl;
}

=head2 maptextfraw

=for ref

Memory map a text file (see the module docs also).

Note that this function maps the raw format so if you are
using an operating system which does strange things to e.g.
line delimiters upon reading a text file, you get the raw (binary)
representation.

The file doesn't really need to be text but it is just mapped
as one large binary chunk.

This function is just a convenience wrapper which firsts C<stat>s
the file and sets the dimensions and datatype.

=for usage

 $pdl4 = maptextfraw("fname", {options}

=for options

The options other than Dims, Datatype of C<mapfraw> are
supported.

=cut

sub PDL::maptextfraw {
	my($class, $name, $opts) = @_;
	$opts = {%$opts}; # Copy just in case
	my @s = stat $name;
	$opts->{Dims} = [$s[7]];
	$opts->{Datatype} = &PDL::byte;
	return PDL::mapfraw($class, $name, $opts);
}

=head1 BUGS

Should be documented better. C<writefraw> and C<readfraw> should
also have options (the author nowadays only uses C<mapfraw> ;)

=head1 AUTHOR

Copyright (C) Tuomas J. Lukka 1997.
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

=cut

1;
