=head1 NAME

PDL::IO::FastRaw -- A simple, fast and convenient io format for PerlDL.

=head1 VERSION

This documentation refers to PDL::IO::FastRaw version 0.0.2, I guess.

=head1 SYNOPSIS

 use PDL;
 use PDL::IO::FastRaw;

 writefraw($pdl,"fname");         # write a raw file

 $pdl2 = readfraw("fname");       # read a raw file
 $pdl2 = PDL->readfraw("fname");

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
file is simply dumped from the memory region of the piddle.
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
to write to the piddle but maps the memory privately so the file
will not be changed when you change the piddle. Be aware though
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
case, you would have to create a C<mapfraw> piddle with the correct
dimensions first using 

 $piddle_on_hd = mapfraw('fname', {Creat => 1, Dims => [dim1, dim2, ...]});

Note that you must specify the dimensions and you must tell
C<mapfraw> to create the new piddle for you by setting the
C<Creat> option to a true value, not C<Create> (note the missing
final 'e').


=head1 FUNCTIONS

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

package PDL::IO::FastRaw;

## use version; our $VERSION = qv('0.0.3');
our $VERSION = '0.000003';
$VERSION = eval $VERSION;

BEGIN {
   our $have_file_map = 0;

   eval "use File::Map 0.57 qw(:all)";
   $have_file_map = 1 unless $@;
}

require Exporter;
use PDL::Core '';
use PDL::Exporter;
use FileHandle;

@PDL::IO::FastRaw::ISA = qw/PDL::Exporter/;

@EXPORT_OK = qw/writefraw readfraw mapfraw maptextfraw/;
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

# Exported functions

*writefraw = \&PDL::writefraw;
sub readfraw {PDL->readfraw(@_)}
sub mapfraw  {PDL->mapfraw(@_)}
sub maptextfraw  {PDL->maptextfraw(@_)}

sub _read_frawhdr {
	my($name,$opts) = @_;
	my $hname = $opts->{Header} || "$name.hdr";
	my $h = new FileHandle "$hname"
	 or barf "Couldn't open '$hname' for reading";
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
	my $h = new FileHandle ">$hname"
	 or barf "Couldn't open '$hname' for writing";
	print $h map {"$_\n"} ($pdl->get_datatype,
		$pdl->getndims, (join ' ',$pdl->dims));
}

sub PDL::writefraw {
	my($pdl,$name,$opts) = @_;
	_writefrawhdr($pdl,$name,$opts);
	my $d = new FileHandle ">$name"
	 or barf "Couldn't open '$name' for writing";
	binmode $d;
	print $d ${$pdl->get_dataref};
}

sub PDL::readfraw {
        my $class = shift;
	my($name,$opts) = @_;
	my $d = new FileHandle "$name"
	 or barf "Couldn't open '$name' for reading";
	binmode $d;
	my $hdr = _read_frawhdr($name,$opts);
	my $pdl = $class->zeroes ((new PDL::Type($hdr->{Type})), @{$hdr->{Dims}});
	my $len = length ${$pdl->get_dataref};
# wrong.
#       $d->sysread(${$pdl->get_dataref},$len) == $len
#         or barf "Couldn't read enough data from '$name'";
        my $index = 0;
        my $data;
        my $retlen;
        while (($retlen = $d->sysread($data, $len)) != 0) {
                substr(${$pdl->get_dataref},$index,$len) = $data;
                $index += $retlen;
               $len -= $retlen;
        }
	$pdl->upd_data();
	return $pdl;
}

sub PDL::mapfraw {
        my $class = shift;
	my($name,$opts) = @_;
	my $hdr;
	if($opts->{Dims}) {
		my $datatype = $opts->{Datatype};
		if(!defined $datatype) {$datatype = $PDL_D;}
		$hdr->{Type} = $datatype;
		$hdr->{Dims} = $opts->{Dims};
		$hdr->{NDims} = scalar(@{$opts->{Dims}});
	} else {
		$hdr = _read_frawhdr($name,$opts);
	}
	$s = PDL::Core::howbig($hdr->{Type});
	for(@{$hdr->{Dims}}) {
		$s *= $_;
	}
	my $pdl = $class->zeroes(new PDL::Type($hdr->{Type}));
	$pdl->setdims($hdr->{Dims});

        if ($have_file_map and not defined($PDL::force_use_mmap_code) ) {
           $pdl->set_data_by_file_map(
              $name,
              $s,
              1,
              ($opts->{ReadOnly}?0:1),
              ($opts->{Creat}?1:0),
              (0644),
              ($opts->{Creat} || $opts->{Trunc} ? 1:0)
           );
        } else {
           warn "mapfraw: direct mmap support will be deprecated, please install File::Map\n";
           $pdl->set_data_by_mmap(
              $name,
              $s,
              1,
              ($opts->{ReadOnly}?0:1),
              ($opts->{Creat}?1:0),
              (0644),
              ($opts->{Creat} || $opts->{Trunc} ? 1:0)
           );
        }

	if($opts->{Creat}) {
		_writefrawhdr($pdl,$name,$opts);
	}
	return $pdl;
}

sub PDL::maptextfraw {
	my($class, $name, $opts) = @_;
	$opts = {%$opts}; # Copy just in case
	my @s = stat $name;
	$opts->{Dims} = [$s[7]];
	$opts->{Datatype} = &PDL::byte;
	return PDL::mapfraw($class, $name, $opts);
}

1;
