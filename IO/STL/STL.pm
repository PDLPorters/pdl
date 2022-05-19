package PDL::IO::STL;

use strict;
use warnings;

our $VERSION = '0.001';
our @EXPORT_OK = qw( rstl wstl );
our %EXPORT_TAGS = (Func=>[@EXPORT_OK]);
our @ISA = ('PDL::Exporter');

use PDL::LiteF;
use PDL::Options;
use PDL::Exporter;
use PDL::IO::Misc; # for little/big-endian

=head1 NAME

PDL::IO::STL - read/write 3D stereolithography files

=head1 SYNOPSIS

 use PDL;
 use PDL::IO::STL;

 ($vertices, $faceidx, $colours) = rstl('owl.stl'); # read an STL file
 wstl('file.stl', $vertices, $faceidx, $colours); # write an STL file

=head1 DESCRIPTION

Normal-vector information is currently ignored.
The "attribute byte count", used sometimes to store colour information,
is currently ignored.

This module is based on L<CAD::Format::STL>, but with C<binmode> on
opened filehandles and little-endian (i.e. network) order forced on the
binary format.

=head1 FUNCTIONS

=head2 rstl

=for ref

Read an STL file (ASCII or binary), returning vertices and face-indices.

=for example

 ($vertices, $faceidx, $colours) = rstl('owl.stl'); # read an STL file

=cut

sub rstl { PDL->rstl(@_); }
sub PDL::rstl {
  my $class = shift;
  barf 'Usage: $x = rstl($file) -or- $x = PDL->rstl($file)' if @_ < 1 || @_ > 2;
  my $file = shift;
  # allow filehandle
  unless((ref($file) || '') eq 'GLOB') {
    open(my $fh, '<', $file) or
      barf "cannot open '$file' for reading $!";
    binmode $fh;
    $file = $fh;
  }
  barf('must have seekable filehandle') if !seek($file, 0,0);
  my $mode = _detect($file);
  seek($file, 0, 0) or barf "cannot reset filehandle";
  my $func = $mode eq 'ascii' ? \&_read_ascii : \&_read_binary;
  $func->($file);
}

sub _detect {
  my $fh = shift;
  my $location = tell $fh;
  my $buf; read($fh, $buf, 5) or barf $@;
  seek($fh, $location, 0), return 'ascii' if $buf eq 'solid';
  seek($fh, $location + 80, 0);
  my $count = eval {
    my $buf; read($fh, $buf, 4) or barf $@;
    unpack('L<', $buf);
  };
  $@ and seek($fh, $location, 0), return 'ascii'; # if we hit eof, not binary
  $count or barf "detection failed - no facets?";
  my $size = (stat($fh))[7];
  barf "failed to stat '$fh'" if !defined $size;
  # calculate the expected file size
  my $expect =
    + 80 # header
    +  4 # count
    + $count * (
      + 4 # normal, pt,pt,pt (vectors)
      * 3 # values per vector
      * 4 # bytes per value
      + 2 # the trailing 'short'
    );
  return ($size - $location >= $expect) ? 'binary' : 'ascii';
}

my $p_re = qr/([^ ]+)\s+([^ ]+)\s+([^ ]+)$/;
sub _read_ascii {
  my ($fh) = @_;
  my $getline = sub {
    while(my $line = <$fh>) {
      $line =~ s/\s*$//; # allow any eol
      length($line) or next;
      return($line);
    }
    return;
  };
  my (@tri, $part);
  while(my $line = $getline->()) {
    if($line =~ m/^\s*solid (.*)/) {
      $part = $1;
      next;
    }
    elsif($line =~ m/^\s*endsolid (.*)/) {
      my $name = $1;
      barf "invalid 'endsolid' entry with no current part" if !defined $part;
      barf "end of part '$name' should have been '$part'" if $name ne $part;
      $part = undef;
      last;
    }
    barf "what? ($line)" if !defined $part;
    my @n = ($line =~ m/^\s*facet\s+normal\s+$p_re/) or
      barf "how did that happen? ($line)";
    my $next = $getline->();
    unless($next and ($next =~ m/^\s*outer\s+loop$/)) {
      barf "facet doesn't start with 'outer loop' ($next)";
    }
    my @this_tri;
    while(my $line = $getline->()) {
      ($line =~ m/^\s*endloop$/) and last;
      if($line =~ m/^\s*vertex\s+$p_re/) {
        push(@this_tri, [$1, $2, $3]);
      }
    }
    barf "need three vertices per facet (not @{[ 0+@this_tri ]})" if @this_tri != 3;
    my $end = $getline->();
    ($end and ($end =~ m/^\s*endfacet/)) or
      barf "bad endfacet $line";
    push @tri, \@this_tri;
  }
  barf "part '$part' was left open" if defined $part;
  _as_ndarray(pdl PDL::float(), \@tri);
}

sub _as_ndarray {
  my ($pdl) = @_;
  my $uniqv = $pdl->uniqvec;
  ($uniqv, $pdl->vsearchvec($uniqv), undef);
}

sub _read_binary {
  my ($fh) = @_;
  barf "bigfloat" unless(length(pack("f", 1)) == 4);
  # TODO try to read part name from header (up to \0)
  seek($fh, 80, 0);
  my $buf; read($fh, $buf, 4) or warn "EOF?"; my $triangles = unpack('L<', $buf);
  my $bytes = 50 * $triangles; # norm+3vertices * 3float + short with length of extra
  my $bytespdl = zeroes PDL::byte(), 50, $triangles;
  my $bytesread = read($fh, ${$bytespdl->get_dataref}, $bytes);
  barf "Tried to read $bytes but only got $bytesread" if $bytesread != $bytes;
  $bytespdl->upd_data;
  my $floatpdl = zeroes PDL::float(), 3, 4, $triangles;
  ${$floatpdl->get_dataref} = ${$bytespdl->slice('0:47')->get_dataref};
  $floatpdl->upd_data;
  $floatpdl->type->bswap->($floatpdl) if isbigendian();
  # TODO check that the unit normal is within a thousandth of a radian
  # (0.001 rad is ~0.06deg)
  _as_ndarray($floatpdl->slice(':,1:3'));
}

=head2 wstl

=for ref

Simple PDL FITS writer

=for example

  wstl 'file.stl', $vertices, $faceidx;
  wstl 'file.stl', $vertices, $faceidx, \%OPTIONS;
  wstl $fh, $vertices, $faceidx, \%OPTIONS;

Passing a file-handle is supported, so multiple parts can be written to
an ASCII file with several calls.

C<wstl> accepts several options that may be passed in as a hash ref
if desired:

=over 3

=item mode (default='binary')

Whether to write out the file as ASCII or binary.

=item name (default='part')

The part name to use.

=back

=cut

our $wstl_options = PDL::Options->new( { mode=>'binary', name=>'part' } );
my %valid_mode = map +($_=>1), qw(ascii binary);
sub wstl { PDL->wstl(@_); }
sub PDL::wstl {
  barf 'Usage: wstl($file,$vertices,$faceidx,[$colours],[{options}])' if @_<3 || @_>5;
  my (undef, $file, $v, $f, $c) = @_;
  my $u_opt = ifhref($_[-1]);
  my $opt = $wstl_options->options($u_opt);
  my $mode = $opt->{mode};
  barf "invalid write mode '$mode'" if !$valid_mode{$mode};
  # allow filehandle
  unless((ref($file) || '') eq 'GLOB') {
    open(my $fh, '>', $file) or
      barf "cannot open '$file' for writing $!";
    binmode $fh;
    $file = $fh;
  }
  my $func = $mode eq 'ascii' ? \&_write_ascii : \&_write_binary;
  $func->($file, $v, $f, $c, $opt->{name});
  1;
}

sub _write_binary {
  my ($fh, $v, $f, $c, $name) = @_;
  print $fh $name, "\0" x (80 - do {use bytes; length($name)});
  print $fh pack 'L<', $f->dim(1);
  foreach my $facet (@{ $v->dice_axis(1, $f->flat)->splitdim(1,3)->unpdl }) {
    print $fh map {map pack('f<', $_), @$_} [0,0,0], @$facet;
    print $fh "\0" x 2;
  }
}

sub _write_ascii {
  my ($fh, $v, $f, $c, $name) = @_;
  my $spaces = '';
  my $print = sub {print $fh $spaces . join(' ', @_) . "\n"};
  $print->('solid', $name);
  $spaces = ' 'x2;
  foreach my $facet (@{ $v->dice_axis(1, $f->flat)->splitdim(1,3)->unpdl }) {
    my ($n, @pts) = ([0,0,0], @$facet);
    $print->('facet normal', @$n);
    $spaces = ' 'x4;
    $print->('outer loop');
    $spaces = ' 'x6;
    (@pts == 3) or barf "invalid facet";
    foreach my $pt (@pts) {
      $print->('vertex', @$pt);
    }
    $spaces = ' 'x4;
    $print->('endloop');
    $spaces = ' 'x2;
    $print->('endfacet');
  }
  $spaces = '';
  $print->('endsolid', $name);
}

=head1 AUTHOR

Ed J, based on Eric Wilhelm's code in L<CAD::Format::STL>.

=cut

1;
