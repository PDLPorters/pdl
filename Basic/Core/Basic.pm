
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
our @EXPORT_OK = qw/ ndcoords rvals axisvals allaxisvals xvals yvals zvals sec ins hist whist
	similar_assign transpose sequence xlinvals ylinvals
	zlinvals axislinvals/;
our %EXPORT_TAGS = (Func=>[@EXPORT_OK]);

# Exportable functions
*axisvals       = \&PDL::axisvals;		
*allaxisvals       = \&PDL::allaxisvals;		
*sec            = \&PDL::sec;		
*ins            = \&PDL::ins;		
*hist           = \&PDL::hist;		
*whist           = \&PDL::whist;		
*similar_assign = \&PDL::similar_assign;
*transpose      = \&PDL::transpose;
*xlinvals 	= \&PDL::xlinvals;
*ylinvals 	= \&PDL::ylinvals;
*zlinvals 	= \&PDL::zlinvals;

=head2 xvals

=for ref

Fills an ndarray with X index values.  Uses similar specifications to
L</zeroes> and L</new_from_specification>.

CAVEAT:

If you use the single argument ndarray form (top row
in the usage table) the output will have the same type as the input,
except that as of 2.064, the returned ndarray will default to at least type
C<double>. As of 2.085, this will respect a given type as in the second
or third form below.

=for usage

 $x = xvals($somearray); # at least type double
 $x = xvals([OPTIONAL TYPE],$nx,$ny,$nz...);
 $x = xvals([OPTIONAL TYPE], $somarray->dims);

etc. see L<zeroes|PDL::Core/zeroes>.

=for example

  pdl> print xvals zeroes(5,10)
  [
   [0 1 2 3 4]
   [0 1 2 3 4]
   [0 1 2 3 4]
   [0 1 2 3 4]
   [0 1 2 3 4]
   [0 1 2 3 4]
   [0 1 2 3 4]
   [0 1 2 3 4]
   [0 1 2 3 4]
   [0 1 2 3 4]
  ]

=head2 yvals

=for ref

Fills an ndarray with Y index values.  See the CAVEAT for L</xvals>.

=for usage

 $x = yvals($somearray); yvals(inplace($somearray));
 $x = yvals([OPTIONAL TYPE],$nx,$ny,$nz...);

etc. see L<zeroes|PDL::Core/zeroes>.

=for example

 pdl> print yvals zeroes(5,10)
 [
  [0 0 0 0 0]
  [1 1 1 1 1]
  [2 2 2 2 2]
  [3 3 3 3 3]
  [4 4 4 4 4]
  [5 5 5 5 5]
  [6 6 6 6 6]
  [7 7 7 7 7]
  [8 8 8 8 8]
  [9 9 9 9 9]
 ]

=head2 zvals

=for ref

Fills an ndarray with Z index values.  See the CAVEAT for L</xvals>.

=for usage

 $x = zvals($somearray); zvals(inplace($somearray));
 $x = zvals([OPTIONAL TYPE],$nx,$ny,$nz...);

etc. see L<zeroes|PDL::Core/zeroes>.

=for example

 pdl> print zvals zeroes(3,4,2)
 [
  [
   [0 0 0]
   [0 0 0]
   [0 0 0]
   [0 0 0]
  ]
  [
   [1 1 1]
   [1 1 1]
   [1 1 1]
   [1 1 1]
  ]
 ]

=head2 xlinvals

=for ref

X axis values between endpoints (see L</xvals>).

=for usage

 $w = zeroes(100,100);
 $x = $w->xlinvals(0.5,1.5);
 $y = $w->ylinvals(-2,-1);
 # calculate Z for X between 0.5 and 1.5 and
 # Y between -2 and -1.
 $z = f($x,$y);

C<xlinvals>, C<ylinvals> and C<zlinvals> return an ndarray with the same shape
as their first argument and linearly scaled values between the two other
arguments along the given axis.

=head2 ylinvals

=for ref

Y axis values between endpoints (see L</yvals>).

See L</xlinvals> for more information.

=head2 zlinvals

=for ref

Z axis values between endpoints (see L</zvals>).

See L</xlinvals> for more information.

=head2 xlogvals

=for ref

X axis values logarithmically spaced between endpoints (see L</xvals>).

=for usage

 $w = zeroes(100,100);
 $x = $w->xlogvals(1e-6,1e-3);
 $y = $w->ylinvals(1e-4,1e3);
 # calculate Z for X between 1e-6 and 1e-3 and
 # Y between 1e-4 and 1e3.
 $z = f($x,$y);

C<xlogvals>, C<ylogvals> and C<zlogvals> return an ndarray with the same shape
as their first argument and logarithmically scaled values between the two other
arguments along the given axis.

=head2 ylogvals

=for ref

Y axis values logarithmically spaced between endpoints (see L</yvals>).

See L</xlogvals> for more information.

=head2 zlogvals

=for ref

Z axis values logarithmically spaced between endpoints (see L</zvals>).

See L</xlogvals> for more information.

=cut

# Conveniently named interfaces to axisvals()

sub xvals { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->xvals : PDL->xvals(@_) }
sub yvals { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->yvals : PDL->yvals(@_) }
sub zvals { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->zvals : PDL->zvals(@_) }
sub PDL::xvals {
    my $type_given = grep +(ref($_[$_])||'') eq 'PDL::Type', 0..1;
    axisvals2(&PDL::Core::_construct,0,$type_given);
}
sub PDL::yvals {
    my $type_given = grep +(ref($_[$_])||'') eq 'PDL::Type', 0..1;
    axisvals2(&PDL::Core::_construct,1,$type_given);
}
sub PDL::zvals {
    my $type_given = grep +(ref($_[$_])||'') eq 'PDL::Type', 0..1;
    axisvals2(&PDL::Core::_construct,2,$type_given);
}

sub _dimcheck {
  my ($pdl, $whichdim, $name) = @_;
  my $dim = $pdl->getdim($whichdim);
  barf "Must have at least two elements in dimension for $name" if $dim <= 1;
  $dim;
}
sub _linvals {
  my ($pdl, $v1, $v2, $dim, $method) = @_;
  $pdl->$method * (($v2 - $v1) / ($dim-1)) + $v1;
}
sub PDL::xlinvals {
  _linvals(@_[0..2], _dimcheck($_[0], 0, 'xlinvals'), 'xvals');
}
sub PDL::ylinvals {
  _linvals(@_[0..2], _dimcheck($_[0], 1, 'ylinvals'), 'yvals');
}
sub PDL::zlinvals {
  _linvals(@_[0..2], _dimcheck($_[0], 2, 'zlinvals'), 'zvals');
}

sub _logvals {
  my ($pdl, $min, $max, $dim, $method) = @_;
  barf "min and max must be positive" if $min <= 0 || $max <= 0;
  my ($lmin,$lmax) = map log($_), $min, $max;
  exp($pdl->$method * (($lmax - $lmin) / ($dim-1)) + $lmin);
}
sub PDL::xlogvals {
  _logvals(@_[0..2], _dimcheck($_[0], 0, 'xlogvals'), 'xvals');
}
sub PDL::ylogvals {
  _logvals(@_[0..2], _dimcheck($_[0], 1, 'ylogvals'), 'yvals');
}
sub PDL::zlogvals {
  _logvals(@_[0..2], _dimcheck($_[0], 2, 'zlogvals'), 'zvals');
}

=head2 allaxisvals

=for ref

Synonym for L</ndcoords> - enumerates all coordinates in a
PDL or dim list, adding an extra dim on the front to accommodate
the vector coordinate index (the form expected by L</indexND>,
L</range>, and L</interpND>).  See L</ndcoords> for more detail.

=for usage

  $indices = allaxisvals($pdl);
  $indices = allaxisvals(@dimlist);
  $indices = allaxisvals($type,@dimlist);

=cut

=head2 ndcoords

=for ref

Enumerate pixel coordinates for an N-D ndarray

Returns an enumerated list of coordinates suitable for use in
L<indexND|PDL::Slices/indexND> or L<range|PDL::Slices/range>: you feed
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
  my $type;
  if(ref $_[0] eq 'PDL::Type') {
    $type = shift;
  }
  my @dims = (ref $_[0]) ? (shift)->dims : @_;
  my @d = @dims;
  unshift(@d,scalar(@dims));
  unshift(@d,$type) if defined($type);
  my $out = PDL->zeroes(@d);
  for my $d(0..$#dims) {
    my $w = $out->index($d);
    $w = $w->mv($d,0) if $d != 0;
    $w .= xvals($w);
  }
  $out;
}
*ndcoords = *allaxisvals = *PDL::allaxisvals = \&PDL::ndcoords;

=head2 hist

=for ref

Create histogram of an ndarray

=for usage

 $hist = hist($data);
 ($xvals,$hist) = hist($data);

or

 $hist = hist($data,$min,$max,$step);
 ($xvals,$hist) = hist($data,[$min,$max,$step]);

If C<hist> is run in list context, C<$xvals> gives the
computed bin centres as double values.

A nice idiom (with
L<PDL::Graphics::PGPLOT>) is

 bin hist $data;  # Plot histogram

=for example

 pdl> p $y
 [13 10 13 10 9 13 9 12 11 10 10 13 7 6 8 10 11 7 12 9 11 11 12 6 12 7]
 pdl> $h = hist $y,0,20,1; # hist with step 1, min 0 and 20 bins
 pdl> p $h
 [0 0 0 0 0 0 2 3 1 3 5 4 4 4 0 0 0 0 0 0]

=cut

sub PDL::hist {
    my $usage = "\n" . '  Usage:          $hist  = hist($data)' . "\n" .
                       '                  $hist  = hist($data,$min,$max,$step)' . "\n" .
                       '          ($xvals,$hist) = hist($data)' . "\n" .
                       '          ($xvals,$hist) = hist($data,$min,$max,$step)' . "\n" ;
    barf($usage) if $#_<0;
    my($pdl,$min,$max,$step)=@_;
    ($step, $min, my $bins, my $xvals) =
        _hist_bin_calc($pdl, $min, $max, $step, wantarray());
    PDL::Primitive::histogram($pdl->flat,(my $hist = null),
			      $step,$min,$bins);
    return wantarray() ? ($xvals,$hist) : $hist;
}

=head2 whist

=for ref

Create a weighted histogram of an ndarray

=for usage

 $hist = whist($data, $wt, [$min,$max,$step]);
 ($xvals,$hist) = whist($data, $wt, [$min,$max,$step]);

If requested, C<$xvals> gives the computed bin centres
as type double values.  C<$data> and C<$wt> should have
the same dimensionality and extents.

A nice idiom (with
L<PDL::Graphics::PGPLOT>) is

 bin whist $data, $wt;  # Plot histogram

=for example

 pdl> p $y
 [13 10 13 10 9 13 9 12 11 10 10 13 7 6 8 10 11 7 12 9 11 11 12 6 12 7]
 pdl> $wt = grandom($y->nelem)
 pdl> $h = whist $y, $wt, 0, 20, 1 # hist with step 1, min 0 and 20 bins
 pdl> p $h
 [0 0 0 0 0 0 -0.49552342  1.7987439 0.39450696  4.0073722 -2.6255299 -2.5084501  2.6458365  4.1671676 0 0 0 0 0 0]


=cut

sub PDL::whist {
    barf('Usage: ([$xvals],$hist) = whist($data,$wt,[$min,$max,$step])')
            if @_ < 2;
    my($pdl,$wt,$min,$max,$step)=@_;
    ($step, $min, my $bins, my $xvals) =
        _hist_bin_calc($pdl, $min, $max, $step, wantarray());

    PDL::Primitive::whistogram($pdl->flat,$wt->flat,
			       (my $hist = null), $step, $min, $bins);
    return wantarray() ? ($xvals,$hist) : $hist;
}

sub _hist_bin_calc {
    my($pdl,$min,$max,$step,$wantarray)=@_;
    $min = $pdl->min() unless defined $min;
    $max = $pdl->max() unless defined $max;
    my $nelem = $pdl->nelem;
    barf "empty ndarray, no values to work with" if $nelem == 0;

    $step = ($max-$min)/(($nelem>10_000) ? 100 : sqrt($nelem)) unless defined $step;
    barf "step is zero (or all data equal to one value)" if $step == 0;

    my $bins = int(($max-$min)/$step+0.5);
    print "hist with step $step, min $min and $bins bins\n"
	if $PDL::debug;
    # Need to use double for $xvals here
    my $xvals = $min + $step/2 + sequence(PDL::Core::double,$bins)*$step if $wantarray;

    return ( $step, $min, $bins, $xvals );
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

sub sequence { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->sequence : PDL->sequence(@_) }
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

sub rvals { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->rvals(@_[1..$#_]) : PDL->rvals(@_) }
sub PDL::rvals { # Return radial distance from given point and offset
    my $opt = pop @_ if ref($_[$#_]) eq "HASH";
    my %opt = defined $opt ?
               iparse( {
			CENTRE  => undef, # needed, otherwise centre/center handling painful
			Squared => 0,
		       }, $opt ) : ();
    my $r = &PDL::Core::_construct;
    my @pos;
    @pos = @{$opt{CENTRE}} if defined $opt{CENTRE};
    my $offset;

    $r .= 0.0;
    my $tmp = $r->copy;
    my $i;
    for ($i=0; $i<$r->getndims; $i++) {
         $offset = (defined $pos[$i] ? $pos[$i] : int($r->getdim($i)/2));
	 # Note careful coding for speed and min memory footprint
	 PDL::Primitive::axisvalues((0==$i?$tmp:$tmp->xchg(0,$i))->inplace);
	 $tmp -= $offset; $tmp *= $tmp;
         $r += $tmp;
    }
    return $opt{Squared} ? $r : $r->inplace->sqrt;
}

=head2 axisvals

=for ref

Fills an ndarray with index values on Nth dimension

=for usage

 $z = axisvals ($ndarray, $nth);

This is the routine, for which L</xvals>, L</yvals> etc
are mere shorthands. C<axisvals> can be used to fill along any dimension,
using a parameter.

See also L</allaxisvals>, which generates all axis values
simultaneously in a form useful for L</range>, L</interpND>,
L</indexND>, etc.

Note the 'from specification' style (see L<zeroes|PDL::Core/zeroes>) is
not available here, for obvious reasons.

=cut

sub PDL::axisvals {
	my($this,$nth) = @_;
	my $dummy = $this->new_or_inplace;
	if($dummy->getndims() <= $nth) {
		# This is 'kind of' consistency...
		$dummy .= 0;
		return $dummy;
	}
	my $bar = 0==$nth ? $dummy : $dummy->xchg(0,$nth);
	PDL::Primitive::axisvalues($bar->inplace);
	return $dummy;
}

# We need this version for xvals etc to work in place
sub axisvals2 {
	my($dummy,$nth,$keep_type) = @_;
	$dummy = PDL::Core::double($dummy)
	  if !$keep_type && $dummy->get_datatype < PDL::Core::double()->enum;
	if($dummy->getndims() <= $nth) {
		# This is 'kind of' consistency...
		$dummy .= 0;
		return $dummy;
	}
	my $bar = 0==$nth ? $dummy : $dummy->xchg(0,$nth);
	PDL::Primitive::axisvalues($bar->inplace);
	return $dummy;
}
sub PDL::sec {
	my($this,@coords) = @_;
	my $i; my @maps;
	while($#coords > -1) {
		$i = int(shift @coords) ;
		push @maps, "$i:".int(shift @coords);
	}
	my $tmp = PDL->null;
	$tmp .= $this->slice(join ',',@maps);
	return $tmp;
}

sub PDL::ins {
	my($this,$what,@coords) = @_;
	my $w = PDL::Core::alltopdl($PDL::name,$what);
	my $tmp;
	if($this->is_inplace) {
	  $this->set_inplace(0);
	} else {
	  $this = $this->copy;
	}
	($tmp = $this->slice(
	   (join ',',map {int($coords[$_]).":".
	   	((int($coords[$_])+$w->getdim($_)-1)<$this->getdim($_) ?
	   	(int($coords[$_])+$w->getdim($_)-1):$this->getdim($_))
	   	}
	   	0..$#coords)))
		.= $w;
	return $this;
}

sub PDL::similar_assign {
  my($from,$to) = @_;
  if((my $fd = join ',',@{$from->dims}) ne (my $td = join ',',@{$to->dims})) {
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
  $this->getndims > 1 ? $this->xchg(0,1) :
  $this->getndims > 0 ? $this->dummy(0) :
  $this->dummy(0)->dummy(0);
}

1;

