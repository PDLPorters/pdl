=head1 NAME

PDL::IO::FastRaw -- A simple, fast and convenient io format for PerlDL.

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

=head1 FUNCTIONS

=head2 readfraw

=for ref

Read a raw format binary file

=for usage

 $pdl2 = readfraw("fname");
 $pdl2 = PDL->readfraw("fname");


=head2 writefraw

=for ref

Write a raw format binary file

=for usage

 writefraw($pdl,"fname");


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
	my($name) = @_;
	my $hname = "$name.hdr";
	my $h = new FileHandle "$hname"
	 or barf "Couldn't open '$hname' for reading";
	my $tid = <$h>;
	my $ndims = <$h>;
	my $str = <$h>; if(!defined $str) {barf("Format error in '$hname'");}
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
	my($pdl,$name) = @_;
	my $hname = "$name.hdr";
	my $h = new FileHandle ">$hname"
	 or barf "Couldn't open '$hname' for writing";
	print $h map {"$_\n"} ($pdl->get_datatype,
		$pdl->getndims, (join ' ',$pdl->dims));
}

sub PDL::writefraw {
	my($pdl,$name,$opts) = @_;
	_writefrawhdr($pdl,$name);
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
	my $hdr = _read_frawhdr($name);
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
		$hdr = _read_frawhdr($name);
	}
	$s = PDL::Core::howbig($hdr->{Type});
	for(@{$hdr->{Dims}}) {
		$s *= $_;
	}
	my $pdl = $class->zeroes(new PDL::Type($hdr->{Type}));
#	$pdl->dump();
	$pdl->setdims($hdr->{Dims});
#	$pdl->dump();
	$pdl->set_data_by_mmap($name,$s,1,($opts->{ReadOnly}?0:1),
		($opts->{Creat}?1:0),
		(0644),
		($opts->{Creat} || $opts->{Trunc} ? 1:0));
#	$pdl->dump();
	if($opts->{Creat}) {
		_writefrawhdr($pdl,$name);
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
