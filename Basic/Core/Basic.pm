
=head1 NAME

PDL::Basic -- Basic utility functions for PDL

=head1 DESCRIPTION

This module contains basic utility functions for
creating and manipulating piddles. Most of these functions
are simplified interfaces to the more flexible functions in
the modules 
L<PDL::Primitive|PDL::Primitive> 
and 
L<PDL::Slices|PDL::Slices>.

=head1 SYNOPSIS

 use PDL::Basic;

=head1 FUNCTIONS

=cut

package PDL::Basic;
use PDL::Core '';
use PDL::Types;
use PDL::Exporter;
use PDL::Options;

@ISA=qw/PDL::Exporter/;
@EXPORT_OK = qw/ ndcoords rvals axisvals allaxisvals xvals yvals zvals sec ins hist whist
	similar_assign transpose sequence xlinvals ylinvals
	zlinvals axislinvals/;
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

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

Fills a piddle with X index values

=for usage

 $x = xvals($somearray);
 $x = xvals([OPTIONAL TYPE],$nx,$ny,$nz...);

etc. see L<zeroes|PDL::Core/zeroes>.

=for example

  perldl> print xvals zeroes(5,10)
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

Fills a piddle with Y index values

=for usage

 $x = yvals($somearray); yvals(inplace($somearray));
 $x = yvals([OPTIONAL TYPE],$nx,$ny,$nz...);

etc. see L<zeroes|PDL::Core/zeroes>.

=for example

 perldl> print yvals zeroes(5,10)
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

Fills a piddle with Z index values

=for usage

 $x = zvals($somearray); zvals(inplace($somearray));
 $x = zvals([OPTIONAL TYPE],$nx,$ny,$nz...);

etc. see L<zeroes|PDL::Core/zeroes>.

=for example

 perldl> print zvals zeroes(3,4,2)
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

X axis values between endpoints (see L<xvals|/xvals>).

=for usage

 $a = zeroes(100,100);
 $x = $a->xlinvals(0.5,1.5);
 $y = $a->ylinvals(-2,-1);
 # calculate Z for X between 0.5 and 1.5 and
 # Y between -2 and -1.
 $z = f($x,$y);            

C<xlinvals>, C<ylinvals> and C<zlinvals> return a piddle with the same shape
as their first argument and linearly scaled values between the two other
arguments along the given axis.

=head2 ylinvals

=for ref

Y axis values between endpoints (see L<yvals|/yvals>).

See L<xlinvals|/xlinvals> for more information.

=head2 zlinvals

=for ref

Z axis values between endpoints (see L<zvals|/zvals>).

See L<xlinvals|/xlinvals> for more information.

=cut

# Conveniently named interfaces to axisvals()

sub xvals { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->xvals : PDL->xvals(@_) }
sub yvals { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->yvals : PDL->yvals(@_) }
sub zvals { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->zvals : PDL->zvals(@_) }
sub PDL::xvals {
    my $class = shift;
    my $pdl = scalar(@_)? $class->new_from_specification(@_) : $class->new_or_inplace;
    axisvals2($pdl,0);
    return $pdl;
}
sub PDL::yvals {
    my $class = shift;
    my $pdl = scalar(@_)? $class->new_from_specification(@_) : $class->new_or_inplace;
    axisvals2($pdl,1);
    return $pdl;
}
sub PDL::zvals {
    my $class = shift;
    my $pdl = scalar(@_)? $class->new_from_specification(@_) : $class->new_or_inplace;
    axisvals2($pdl,2);
    return $pdl;
}

sub PDL::xlinvals {
	my $dim = $_[0]->getdim(0);
	barf "Must have at least two elements in dimension for xlinvals"
		if $dim <= 1;
	return $_[0]->xvals * (($_[2] - $_[1]) / ($dim-1)) + $_[1];
}

sub PDL::ylinvals {
	my $dim = $_[0]->getdim(1);
	barf "Must have at least two elements in dimension for ylinvals"
		if $dim <= 1;
	return $_[0]->yvals * (($_[2] - $_[1]) / ($dim-1)) + $_[1];
}

sub PDL::zlinvals {
	my $dim = $_[0]->getdim(2);
	barf "Must have at least two elements in dimension for zlinvals"
		if $dim <= 1;
	return $_[0]->zvals * (($_[2] - $_[1]) / ($dim-1)) + $_[1];
}

=head2 ndcoords

=for ref

Enumerate pixel coordinates for an N-D piddle

=for usage

$indices = ndcoords($pdl)
$indices = ndcoords(@dimlist)

Returns an enumerated list of coordinates suitable for use in 
L<indexND|indexND> or L<range|range>:  you feed in a dimension list
and get out a piddle whose 0th dimension runs over dimension index
and whose 1st through Nth dimensions are the dimensions given in the 
input.  If you feed in a piddle instead of a perl list, then the 
dimension list is used, as in L<xvals|xvals> etc.

=for example

  perldl> print ndcoords(2,3)
  [
   [
    [0 0]
    [1 0] 
    [2 0]
   ]
   [
    [0 1]
    [1 1]
    [2 1]
   ]
  ]

=cut

sub ndcoords { 
  my $type;
  if(ref $_[0] eq 'PDL::Type') {
    $type = shift;
  }
  
  my @dims = (ref $_[0]) ? (shift)->dims : @_;
  my @d = @dims;
  unshift(@d,scalar(@dims));
  unshift(@d,$type) if defined($type);

  $out = PDL->zeroes(@d);
  
  for my $d(0..$#dims) {
    my $a = $out->index($d)->mv($d,0);
    $a .= xvals($a);
  }

  $out;
}

=head2 hist

=for ref

Create histogram of a piddle

=for usage

 $hist = hist($data,[$min,$max,$step]);
 ($xvals,$hist) = hist($data,[$min,$max,$step]);

If requested, C<$xvals> gives the computed bin centres

A nice idiom (with 
L<PDL::Graphics::PGPLOT|PDL::Graphics::PGPLOT>) is

 bin hist $data;  # Plot histogram

=for example

 perldl> p $y
 [13 10 13 10 9 13 9 12 11 10 10 13 7 6 8 10 11 7 12 9 11 11 12 6 12 7]
 perldl> $h = hist $y,0,20,1; # hist with step 1, min 0 and 20 bins
 perldl> p $h
 [0 0 0 0 0 0 2 3 1 3 5 4 4 4 0 0 0 0 0 0]

=cut

sub PDL::hist {
    barf('Usage: ([$xvals],$hist) = hist($data,[$min,$max,$step])') if $#_<0;
    my($pdl,$min,$max,$step)=@_;
    my $xvals;

    ($step, $min, $bins, $xvals) = 
        _hist_bin_calc($pdl, $min, $max, $step, wantarray());

    PDL::Primitive::histogram($pdl->clump(-1),(my $hist = null),
			      $step,$min,$bins);

    return wantarray() ? ($xvals,$hist) : $hist;
}

=head2 whist

=for ref

Create a weighted histogram of a piddle

=for usage

 $hist = whist($data, $wt, [$min,$max,$step]);
 ($xvals,$hist) = whist($data, $wt, [$min,$max,$step]);

If requested, C<$xvals> gives the computed bin centres.
C<$data> and C<$wt> should have the same dimensionality and extents.

A nice idiom (with 
L<PDL::Graphics::PGPLOT|PDL::Graphics::PGPLOT>) is

 bin whist $data, $wt;  # Plot histogram

=for example

 perldl> p $y
 [13 10 13 10 9 13 9 12 11 10 10 13 7 6 8 10 11 7 12 9 11 11 12 6 12 7]
 perldl> $wt = grandom($y->nelem)
 perldl> $h = whist $y, $wt, 0, 20, 1 # hist with step 1, min 0 and 20 bins
 perldl> p $h                        
 [0 0 0 0 0 0 -0.49552342  1.7987439 0.39450696  4.0073722 -2.6255299 -2.5084501  2.6458365  4.1671676 0 0 0 0 0 0]


=cut

sub PDL::whist {
    barf('Usage: ([$xvals],$hist) = whist($data,$wt,[$min,$max,$step])')
            if @_ < 2;
    my($pdl,$wt,$min,$max,$step)=@_;
    my $xvals;

    ($step, $min, $bins, $xvals) = 
        _hist_bin_calc($pdl, $min, $max, $step, wantarray());

    PDL::Primitive::whistogram($pdl->clump(-1),$wt->clump(-1),
			       (my $hist = null), $step, $min, $bins);
    return wantarray() ? ($xvals,$hist) : $hist;
}

sub _hist_bin_calc {
    my($pdl,$min,$max,$step,$wantarray)=@_;
    $min = $pdl->min() unless defined $min;
    $max = $pdl->max() unless defined $max;
    my $ntype = $pdl->get_datatype;
    unless (defined $step) {
	my $defbins = 100 < $pdl->nelem ? 100 : $pdl->nelem;
	$step = ($max-$min)/$defbins;
	$step = int($step) > 0 ? int($step) : 1 if $ntype < $PDL_F;
    }
    barf "step is zero (or all data equal to one value)" if $step == 0;
    my $bins = int(($max-$min)/$step);
    print "hist with step $step, min $min and $bins bins\n"
      if $PDL::debug;
    my $xvals = $min + $step/2 + sequence(PDL::Type->new($ntype),$bins)*
        PDL::convert($step,$ntype) if $wantarray;

    return ( $step, $min, $bins, $xvals );
}


=head2 sequence

=for ref

Create array filled with a sequence of values

=for usage

 $a = sequence($b); $a = sequence [OPTIONAL TYPE], @dims;

etc. see L<zeroes|PDL::Core/zeroes>.

=for example

 perldl> p sequence(10)
 [0 1 2 3 4 5 6 7 8 9]
 perldl> p sequence(3,4)
 [
  [ 0  1  2]
  [ 3  4  5]
  [ 6  7  8]
  [ 9 10 11]
 ]

=cut

sub sequence { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->sequence : PDL->sequence(@_) }
sub PDL::sequence {
    my $class = shift;
    my $pdl = scalar(@_)? $class->new_from_specification(@_) : $class->new_or_inplace;
    my $bar = $pdl->clump(-1)->inplace;
    my $foo = $bar->xvals;
    return $pdl;
}

=head2 rvals

=for ref

Fills a piddle with radial distance values from some centre.

=for usage

 $r = rvals $piddle,{OPTIONS};
 $r = rvals [OPTIONAL TYPE],$nx,$ny,...{OPTIONS};

=for options

 Options:

 Centre => [$x,$y,$z...] # Specify centre
 Center => [$x,$y.$z...] # synonym.

 Squared => 1 # return distance squared (i.e., don't take the square root)

=for example

 perldl> print rvals long,7,7,{Centre=>[2,2]}
 [
  [2 2 2 2 2 3 4]
  [2 1 1 1 2 3 4]
  [2 1 0 1 2 3 4]
  [2 1 1 1 2 3 4]
  [2 2 2 2 2 3 4]
  [3 3 3 3 3 4 5]
  [4 4 4 4 4 5 5]
 ]

For a more general metric, one can define, e.g.,

 sub distance {
   my ($a,$centre,$f) = @_;
   my ($r) = $a->allaxisvals-$centre;
   $f->($r);
 }
 sub l1 { sumover(abs($_[0])); }
 sub euclid { use PDL::Math 'pow'; pow(sumover(pow($_[0],2)),0.5); }
 sub linfty { maximum(abs($_[0])); }

so now

 distance($a, $centre, \&euclid);

will emulate rvals, while C<\&l1> and C<\&linfty> will generate other
well-known norms. 

=cut

sub rvals { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->rvals(@_[1..$#_]) : PDL->rvals(@_) }
sub PDL::rvals { # Return radial distance from given point and offset
    my $class = shift;
    my $opt = pop @_ if ref($_[$#_]) eq "HASH";
    my %opt = defined $opt ? 
               iparse( {
			CENTRE  => undef, # needed, otherwise centre/center handling painful
			Squared => 0,
		       }, $opt ) : ();
    my $r =  scalar(@_)? $class->new_from_specification(@_) : $class->new_or_inplace;

    my @pos;
    @pos = @{$opt{CENTRE}} if defined $opt{CENTRE};
    my $offset;

    $r .= 0.0;
    my $tmp = $r->copy;
    my $i;
    for ($i=0; $i<$r->getndims; $i++) {
         $offset = (defined $pos[$i] ? $pos[$i] : int($r->getdim($i)/2));
	 # Note careful coding for speed and min memory footprint
	 PDL::Primitive::axisvalues($tmp->xchg(0,$i));
	 $tmp -= $offset; $tmp *= $tmp;
         $r += $tmp;
    }
    return $opt{Squared} ? $r : $r->inplace->sqrt;
}

=head2 axisvals

=for ref

Fills a piddle with index values on Nth dimension

=for usage

 $z = axisvals ($piddle, $nth);

This is the routine, for which L<xvals|/xvals>, L<yvals|/yvals> etc
are mere shorthands. C<axisvals> can be used to fill
along any dimension.

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
#		barf("Too few dimensions given to axisvals $nth\n");
	}
	my $bar = $dummy->xchg(0,$nth);
	PDL::Primitive::axisvalues($bar);
	return $dummy;
}

# We need this version for xvals etc to work in place
sub axisvals2 {
	my($this,$nth) = @_;
	my $dummy = shift;
	if($dummy->getndims() <= $nth) {
		# This is 'kind of' consistency...
		$dummy .= 0;
		return $dummy;
#		barf("Too few dimensions given to axisvals $nth\n");
	}
	my $bar = $dummy->xchg(0,$nth);
	PDL::Primitive::axisvalues($bar);
	return $dummy;
}

=head2 allaxisvals

=for ref

Generates a piddle with index values

=for usage

 $z = allaxisvals ($piddle);

C<allaxisvals> produces an array with axis values along each dimension,
adding an extra dimension at the start.

C<allaxisvals($piddle)-E<gt>slice("($nth)")> will produce the same result
as C<axisvals($piddle,$nth)> (although with extra work and not inplace).

It's useful when all the values will be required, as in the example
given of a generalized L<rvals|/rvals>.

=cut

sub PDL::allaxisvals {
	my($this) = @_;
	my($dims) = $this->getndims;
	my($dummy) = $this->dummy(0,$dims)->new;
	my(@dums) = $dummy->mv(0,$dims)->dog;
	foreach (0 .. $dims-1) {
	  my $bar = $dums[$_]->xchg(0,$_);
	  PDL::Primitive::axisvalues($bar);
	}
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
	if((join ',',@{$from->dims}) ne (join ',',@{$to->dims})) {
		barf "Similar_assign: dimensions [".
			(join ',',@{$from->dims})."] and [".
			(join ',',@{$to->dims})."] do not match!\n";
	}
	$to .= $from;
}

=head2 transpose

=for ref

transpose rows and columns. 

=for usage

 $b = transpose($a); $b = ~$a;

Also bound to the C<~> unary operator in PDL::Matrix.

=for example

 perldl> $a = sequence(3,2)
 perldl> p $a
 [
  [0 1 2]
  [3 4 5]
 ]                                                                               
 perldl> p transpose( $a )
 [
  [0 3]
  [1 4]
  [2 5]                                                                          
 ]

=cut

sub PDL::transpose {
	my($this) = @_;
	if($this->getndims == 1) {
# 1-Dim: add dummy
		return pdl $this->dummy(0);
	}
	return $this->xchg(0,1)->sever;
}

1;

