=encoding utf8

=head1 NAME

PDL::Basic -- Basic utility functions for PDL

=head1 DESCRIPTION

This module contains basic utility functions for
creating and manipulating ndarrays. Most of these functions
are simplified interfaces to the more flexible functions in
the modules
L<PDL::Primitive>
and
L<PDL::Slices>.

=head1 SYNOPSIS

 use PDL::Basic;

=head1 FUNCTIONS

=cut

package PDL::Basic;
use strict;
use warnings;
use PDL::Core '';
use PDL::Types;
use PDL::Exporter;
use PDL::Options;

our @ISA=qw/PDL::Exporter/;
our @EXPORT_OK = qw(
  sec ins hist whist similar_assign transpose
  allaxisvals ndcoords sequence rvals
  axisvals xvals yvals zvals
  axislinvals xlinvals ylinvals zlinvals
  xlogvals ylogvals zlogvals
);
our %EXPORT_TAGS = (Func=>[@EXPORT_OK]);

# Exportable functions
for (@EXPORT_OK) {
  no strict 'refs';
  *$_ = \&{ $PDL::{$_} };
}

=head2 axisvals, xvals, yvals, zvals

=for ref

Fills an ndarray with index values on X, Y, Z, or Nth dimension

Uses similar specifications to L<zeroes|PDL::Core/zeroes> and
L<new_from_specification|PDL::Core/new_from_specification>.
B<CAVEAT>: If you use the single argument ndarray form (top row
in the usage table) the output will have the same type as the input,
except that between 2.064 and 2.100, the returned ndarray will
default to at least type C<double>; as of 2.101 this upgrade is
relaxed to C<float>.
As of 2.085, this will respect a given type as in the second
or third form below.

=for usage

 $x = xvals($somearray); # at least type float
 $x = xvals([OPTIONAL TYPE],$nx,$ny,$nz...);
 $x = xvals([OPTIONAL TYPE], $somarray->dims);
 $y = yvals($somearray); yvals(inplace($somearray));
 $y = yvals([OPTIONAL TYPE],$nx,$ny,$nz...);
 $z = zvals($somearray); zvals(inplace($somearray));
 $z = zvals([OPTIONAL TYPE],$nx,$ny,$nz...);
 $axisv = axisvals ($ndarray, $nth);
 $axisv = $ndarray->axisvals($nth);
 $axisv = axisvals ([type,] $nth, $dim0, $dim1, ...); # new in PDL 2.101
 $axisv = axisvals ($nth, [type,] $dim0, $dim1, ...); # new in PDL 2.101

See also L</allaxisvals>, which generates all axis values
simultaneously in a form useful for L</range>, L</interpND>,
L</indexND>, etc.

=for example

  pdl> print xvals zeroes(5,2)
  [
   [0 1 2 3 4]
   [0 1 2 3 4]
  ]
  pdl> print yvals zeroes(5,2)
  [
   [0 0 0 0 0]
   [1 1 1 1 1]
  ]
  pdl> print zvals zeroes(3,2,2)
  [
   [
    [0 0 0]
    [0 0 0]
   ]
   [
    [1 1 1]
    [1 1 1]
   ]
  ]

=cut

sub PDL::axisvals {
  my $type_given = grep +(ref($_[$_])||'') eq 'PDL::Type', 0..2;
  my ($first_non_ref) = grep !ref $_[$_], 0..$#_;
  my ($nth) = splice @_, $first_non_ref, 1;
  axisvals2(&PDL::Core::_construct,$nth,$type_given);
}

# We need this version for xvals etc to work in place
sub axisvals2 {
  my($dummy,$nth,$keep_type) = @_;
  $dummy = PDL::Core::float($dummy)
    if !$keep_type && $dummy->get_datatype < PDL::Core::float()->enum;
  return $dummy .= 0 if $dummy->getndims <= $nth; # 'kind of' consistency
  (0==$nth ? $dummy : $dummy->xchg(0,$nth))->inplace->axisvalues;
  $dummy;
}

# Conveniently named interfaces to axisvals()
sub PDL::xvals { unshift @_, 0; goto &axisvals; }
sub PDL::yvals { unshift @_, 1; goto &axisvals; }
sub PDL::zvals { unshift @_, 2; goto &axisvals; }

=head2 axislinvals, xlinvals, ylinvals, zlinvals

=for ref

Axis values linearly spaced between endpoints (see L</xvals>).

=for usage

 $w = zeroes(100,100);
 $x = $w->xlinvals(0.5,1.5);
 $y = $w->ylinvals(-2,-1);
 $z = f($x,$y); # calculate Z for X from 0.5 to 1.5, Y from -2 to -1
 # alternatively (new in PDL 2.101):
 $x = xlinvals(0.5,1.5,100);
 $y = xlinvals(-2,-1,100); # x = along 0-th dim
 $z = f(meshgrid($x,$y));  # should go faster as meshgrid makes better locality
 $pdl = xlinvals(float,0.5,1.5,100);        # can specify type
 $x = $w->axislinvals(0,0.5,1.5);           # same as xlinvals
 $x = axislinvals(0,0.5,1.5,100,100);       # same as xlinvals
 $x = axislinvals(float,0,0.5,1.5,100,100); # same as xlinvals
 $x = axislinvals(0,float,0.5,1.5,100,100); # same as xlinvals

C<xlinvals>, C<ylinvals> and C<zlinvals> return an ndarray with the same shape
as their first argument if an ndarray, and linearly scaled values between the two other
arguments along the given axis.
Works with dim-length of one as of 2.093, giving the starting point.
As of 2.101, instead of giving an ndarray you can give an optional
type at the start, and dimensions after the two mandatory arguments.

=cut

sub _dimcheck {
  my ($pdl, $whichdim, $name) = @_;
  barf "Given non-PDL '$pdl'" if !UNIVERSAL::isa($pdl, 'PDL');
  my $dimlength = $pdl->getdim($whichdim);
  barf "Must have at least one element in dimension for $name" if $dimlength < 1;
  $dimlength;
}
sub _linvals {
  my ($name) = splice @_, 0, 1;
  my ($first_non_ref) = grep !ref $_[$_], 0..$#_;
  my $whichdim = $_[$first_non_ref];
  (undef, $first_non_ref) = grep !ref $_[$_], 0..$#_;
  my ($v1, $v2) = splice @_, $first_non_ref, 2;
  my $pdl = &axisvals;
  my $dimlength = _dimcheck($pdl, $whichdim, $name);
  $pdl *= (($v2 - $v1) / ($dimlength > 1 ? ($dimlength-1) : 1));
  $pdl += $v1;
}
sub PDL::axislinvals { unshift @_, 'axislinvals'; goto &_linvals; }
sub PDL::xlinvals { unshift @_, 'xlinvals', 0; goto &_linvals; }
sub PDL::ylinvals { unshift @_, 'ylinvals', 1; goto &_linvals; }
sub PDL::zlinvals { unshift @_, 'zlinvals', 2; goto &_linvals; }

=head2 xlogvals, ylogvals, zlogvals

=for ref

Axis values logarithmically spaced between endpoints (see L</xvals>).

=for usage

 $w = zeroes(100,100);
 $x = $w->xlogvals(1e-6,1e-3);
 $y = $w->ylogvals(1e-4,1e3);
 $z = f($x,$y); # calculate Z for X from 1e-6 to 1e-3, Y from 1e-4 to 1e3
 # alternatively (new in PDL 2.101):
 $x = xlogvals(1e-6,1e-3,100);
 $y = xlogvals(1e-4,1e3,100); # x = along 0-th dim
 $z = f(meshgrid($x,$y));  # should go faster as meshgrid makes better locality
 $pdl = xlogvals(float,1e-6,1e-3,100); # can specify type

C<xlogvals>, C<ylogvals> and C<zlogvals> return an ndarray with the same shape
as their first argument and logarithmically scaled values between the two other
arguments along the given axis.
Works with dim-length of one as of 2.093, giving the starting point.
As of 2.101, instead of giving an ndarray you can give an optional
type at the start, and dimensions after the two mandatory arguments.

=cut

sub _logvals {
  my ($name) = splice @_, 0, 1;
  my ($whichdim) = @_;
  my ($first_non_ref) = grep !ref $_[$_], 1..$#_;
  my ($min, $max) = splice @_, $first_non_ref, 2;
  barf "min and max must be positive" if $min <= 0 || $max <= 0;
  my ($lmin,$lmax) = map log($_), $min, $max;
  my $pdl = &axisvals;
  my $dimlength = _dimcheck($pdl, $whichdim, $name);
  $pdl .= exp($pdl * (($lmax - $lmin) / ($dimlength > 1 ? ($dimlength-1) : 1)) + $lmin);
}
sub PDL::xlogvals {
  unshift @_, 'xlogvals', 0;
  goto &_logvals;
}
sub PDL::ylogvals {
  unshift @_, 'ylogvals', 1;
  goto &_logvals;
}
sub PDL::zlogvals {
  unshift @_, 'zlogvals', 2;
  goto &_logvals;
}

=head2 ndcoords, allaxisvals

=for ref

Enumerate pixel coordinates for an N-D ndarray

Returns an enumerated list of coordinates suitable for use in
L<indexND|PDL::Slices/indexND>, L<range|PDL::Slices/range>, or
L<interpND|PDL::Primitive/interpND>: you feed
in a dimension list and get out an ndarray whose 0th dimension runs over
dimension index and whose 1st through Nth dimensions are the
dimensions given in the input.  If you feed in an ndarray instead of a
perl list, then the dimension list is used, as in L</xvals> etc.

Unlike L</xvals> etc., if you supply an ndarray input, you get
out an ndarray of the default ndarray type: double.   This causes less
surprises than the previous default of keeping the data type of
the input ndarray since that rarely made sense in most usages.

=for usage

  $indices = ndcoords($pdl);
  $indices = ndcoords(@dimlist);
  $indices = ndcoords($type,@dimlist);
  $indices = allaxisvals($pdl);
  $indices = allaxisvals(@dimlist);
  $indices = allaxisvals($type,@dimlist);

=for example

  pdl> print ndcoords(2,3)
  [
   [
    [0 0]
    [1 0]
   ]
   [
    [0 1]
    [1 1]
   ]
   [
    [0 2]
    [1 2]
   ]
  ]
  pdl> $w = zeroes(byte,2,3);        # $w is a 2x3 byte ndarray
  pdl> $y = ndcoords($w);            # $y inherits $w's type
  pdl> $c = ndcoords(long,$w->dims); # $c is a long ndarray, same dims as $y
  pdl> help $y;
  This variable is   Byte D [2,2,3]              P            0.01Kb
  pdl> help $c;
  This variable is   Long D [2,2,3]              P            0.05Kb

=cut

sub PDL::ndcoords {
  my $type = ref $_[0] eq 'PDL::Type' ? shift : undef;
  my @dims = ref($_[0]) ? shift->dims : @_;
  my $out = PDL->zeroes(defined($type) ? $type : (), scalar(@dims), @dims);
  axisvals2($out->slice("($_)"), $_, 1) for 0..$#dims;
  $out;
}
*PDL::allaxisvals = \&PDL::ndcoords;

=head2 hist, whist

=for ref

Create histogram, or weighted histogram, of an ndarray

=for usage

 $hist = hist($data);
 ($xvals,$hist) = hist($data);
 # or:
 $hist = hist($data,$min,$max,$step);
 ($xvals,$hist) = hist($data,[$min,$max,$step]);
 # weighted:
 $whist = whist($data, $wt, [$min,$max,$step]);
 ($xvals,$whist) = whist($data, $wt, [$min,$max,$step]);

If requested, C<$xvals> gives the computed bin centres
as type double values.  C<$data> and C<$wt> should have
the same dimensionality and extents.

A nice idiom (with L<PDL::Graphics::Simple>) is

 bins hist($data), {yrange=>[0,$data->dim(0)]};        # Plot histogram
 bin whist $data, $wt;                                 # weighted histogram
 bins whist($data, $wt), {yrange=>[0,$data->dim(0)]};  # weighted histogram

=for example

 pdl> p $y
 [13 10 13 10 9 13 9 12 11 10 10 13 7 6 8 10 11 7 12 9 11 11 12 6 12 7]
 pdl> $h = hist $y,0,20,1; # hist with step 1, min 0 and 20 bins
 pdl> p $h
 [0 0 0 0 0 0 2 3 1 3 5 4 4 4 0 0 0 0 0 0]
 # or, weighted:
 pdl> $wt = grandom($y->nelem)
 pdl> $h = whist $y, $wt, 0, 20, 1 # hist with step 1, min 0 and 20 bins
 pdl> p $h
 [0 0 0 0 0 0 -0.49552342  1.7987439 0.39450696  4.0073722 -2.6255299 -2.5084501  2.6458365  4.1671676 0 0 0 0 0 0]

=cut

sub PDL::hist {
    barf(<<'EOF') if !@_;
  Usage:          $hist  = hist($data)
                  $hist  = hist($data,$min,$max,$step)
          ($xvals,$hist) = hist($data)
          ($xvals,$hist) = hist($data,$min,$max,$step)
EOF
    my ($pdl,$min,$max,$step) = @_;
    ($step, $min, my $bins, my $xvals) =
        _hist_bin_calc($pdl, $min, $max, $step, wantarray);
    my $hist = PDL::Primitive::histogram($pdl->flat, $step,$min,$bins);
    wantarray ? ($xvals,$hist) : $hist;
}

sub PDL::whist {
    barf('Usage: ([$xvals],$hist) = whist($data,$wt,[$min,$max,$step])')
            if @_ < 2;
    my ($pdl,$wt,$min,$max,$step) = @_;
    ($step, $min, my $bins, my $xvals) =
        _hist_bin_calc($pdl, $min, $max, $step, wantarray);
    my $hist = PDL::Primitive::whistogram($pdl->flat,$wt->flat, $step, $min, $bins);
    wantarray ? ($xvals,$hist) : $hist;
}

sub _hist_bin_calc {
    my ($pdl,$min,$max,$step,$wantarray) = @_;
    my $nelem = $pdl->nelem;
    barf "empty ndarray, no values to work with" if $nelem == 0;
    $min //= $pdl->min;
    $max //= $pdl->max;
    $step //= ($max-$min)/(($nelem>10_000) ? 100 : sqrt($nelem));
    barf "step is zero (or all data equal to one value)" if $step == 0;
    my $bins = int(($max-$min)/$step+0.5);
    print "hist with step $step, min $min and $bins bins\n"
	if $PDL::debug;
    return ( $step, $min, $bins ) if !$wantarray;
    # Need to use double for $xvals here
    my $xvals = $min + $step/2 + sequence(PDL::Core::double,$bins)*$step;
    ( $step, $min, $bins, $xvals );
}

=head2 sequence

=for ref

Create array filled with a sequence of values

=for usage

 $w = sequence($y); $w = sequence [OPTIONAL TYPE], @dims;

etc. see L<zeroes|PDL::Core/zeroes>.

=for example

 pdl> p sequence(10)
 [0 1 2 3 4 5 6 7 8 9]
 pdl> p sequence(3,4)
 [
  [ 0  1  2]
  [ 3  4  5]
  [ 6  7  8]
  [ 9 10 11]
 ]

=cut

sub PDL::sequence {
    my $type_given = grep +(ref($_[$_])||'') eq 'PDL::Type', 0..1;
    $type_given ||= ref($_[0]) && UNIVERSAL::isa($_[0], 'PDL'); # instance method
    my $pdl = &PDL::Core::_construct;
    axisvals2($pdl->flat->inplace,0,$type_given);
    return $pdl;
}

=head2 rvals

=for ref

Fills an ndarray with radial distance values from some centre.

=for usage

 $r = rvals $ndarray,{OPTIONS};
 $r = rvals [OPTIONAL TYPE],$nx,$ny,...{OPTIONS};

=for options

 Options:

 Centre => [$x,$y,$z...] # Specify centre
 Center => [$x,$y.$z...] # synonym.
 Center => $c            # as 1d array
 Squared => 1 # return distance squared (i.e., don't take the square root)

=for example

 pdl> print rvals long,7,7,{Centre=>[2,2]}
 [
  [2 2 2 2 2 3 4]
  [2 1 1 1 2 3 4]
  [2 1 0 1 2 3 4]
  [2 1 1 1 2 3 4]
  [2 2 2 2 2 3 4]
  [3 3 3 3 3 4 5]
  [4 4 4 4 4 5 5]
 ]

If C<Center> is not specified, the midpoint for a given dimension of
size C<N> is given by C< int(N/2) > so that the midpoint always falls
on an exact pixel point in the data.  For dimensions of even size,
that means the midpoint is shifted by 1/2 pixel from the true center
of that dimension.

If C<Center> has less components than the number of dimensions of the
array, its remaining components are computed as above. If it has more,
a warning is issued.

Also note that the calculation for C<rvals> for integer values
does not promote the datatype so you will have wraparound when
the value calculated for C< r**2 > is greater than the datatype
can hold.  If you need exact values, be sure to use large integer
or floating point datatypes.

For a more general metric, one can define, e.g.,

 sub distance {
   my ($w,$centre,$f) = @_;
   my ($r) = $w->allaxisvals-$centre;
   $f->($r);
 }
 sub l1 { sumover(abs($_[0])); }
 sub euclid { use PDL::Math 'pow'; pow(sumover(pow($_[0],2)),0.5); }
 sub linfty { maximum(abs($_[0])); }

so now

 distance($w, $centre, \&euclid);

will emulate rvals, while C<\&l1> and C<\&linfty> will generate other
well-known norms.

=cut

sub PDL::rvals { # Return radial distance from given point and offset
  my $opt = ref($_[-1]) eq "HASH" ? pop @_ : undef;
  my %opt = defined $opt ?
    iparse( {
      CENTRE  => undef, # needed, otherwise centre/center handling painful
      Squared => 0,
    }, $opt ) : ();
  my $r = &PDL::Core::_construct;
  my @pos;
  if (defined $opt{CENTRE}) {
    my $pos = PDL->topdl($opt{CENTRE});
    barf "Center should be a 1D vector" unless $pos->getndims==1;
    barf "Center has more coordinates than dimensions of ndarray" if $pos->dim(0) > $r->getndims;
    @pos = $pos->list;
  }
  my $offset;
  $r .= 0.0;
  my $tmp = $r->copy;
  my $i;
  for ($i=0; $i<$r->getndims; $i++) {
    $offset = $pos[$i] // int($r->getdim($i)/2);
    # Note careful coding for speed and min memory footprint
    axisvals2($tmp, $i, 1);
    $tmp -= $offset; $tmp *= $tmp;
    $r += $tmp;
  }
  return $opt{Squared} ? $r : $r->inplace->sqrt;
}

=head2 sec

=for ref

Take a subsection of an ndarray with given coordinates.

=for usage

  $new  = sec($input,  $x1, $x2, $y1, $y2, $z1, $z2, ... ) # Take subsection

=cut

sub PDL::sec {
  my ($this,@coords) = @_;
  @coords = map int, @coords;
  my @maps;
  push @maps, shift(@coords).":".shift(@coords) while @coords;
  $this->slice(join ',',@maps)->sever;
}

=head2 ins

=for ref

Insert one ndarray into another at given coordinates.

=for usage

  $newimage = ins($bigimage,$smallimage,$x,$y,$z...) # Insert at x,y,z

=cut

sub PDL::ins {
  my ($this,$what,@coords) = @_;
  my $w = PDL->topdl($what);
  $this = $this->new_or_inplace;
  my @thisdims = $this->dims;
  my @wdims_m1 = map $_-1, $w->dims;
  @coords = map int, @coords;
  $this->slice(
     join ',',map $coords[$_].":".
          (($coords[$_]+$wdims_m1[$_])<$thisdims[$_] ?
           ($coords[$_]+$wdims_m1[$_]):$thisdims[$_]),
          0..$#coords)
          .= $w;
  $this;
}

sub PDL::similar_assign {
  my ($from,$to) = @_;
  if ((my $fd = join ',',@{$from->dims}) ne (my $td = join ',',@{$to->dims})) {
    barf "Similar_assign: dimensions [$fd] and [$td] do not match!\n";
  }
  $to .= $from;
}

=head2 transpose

=for ref

transpose rows and columns.

=for usage

 $y = transpose($w);

=for example

 pdl> $w = sequence(3,2)
 pdl> p $w
 [
  [0 1 2]
  [3 4 5]
 ]
 pdl> p transpose( $w )
 [
  [0 3]
  [1 4]
  [2 5]
 ]

=cut

sub PDL::transpose {
  my ($this) = @_;
  my $ndims = $this->dims;
  $ndims > 1 ? $this->xchg(0,1) :
  $ndims > 0 ? $this->dummy(0) :
  $this->dummy(0)->dummy(0);
}

=head2 t

=for usage

 $pdl = $pdl->t(SCALAR(conj))
 conj : Conjugate Transpose = 1 | Transpose = 0, default = 0;

=for ref

Convenient function for transposing real or complex 2D array(s).
For complex data, if conj is true returns conjugate transposed array(s).
Supports broadcasting. Not exported.

Originally by Grégory Vanuxem.

=cut

sub PDL::t {
  my ($m, $conj) = @_;
  my $r = $m->transpose;
  ($conj && !$r->type->real) ? $r->conj : $r;
}

1;
