
=head1 NAME

PDL::Basic -- Basic utility functions for PDL

=head1 DESCRIPTION

This module contains basic utility functions for
creating and manipulating piddles. Most of these functions
are simplified interfaces to the more flexible functions in
the modules PDL::Primitive and PDL::Slices.

=head1 SYNOPSIS

use PDL::Basic;

=head1 FUNCTIONS

=cut

package PDL::Basic;
use PDL::Core '';
use PDL::Types;
use PDL::Exporter;

@ISA=qw/PDL::Exporter/;
@EXPORT_OK = qw/ rvals axisvals xvals yvals zvals sec ins hist
	similar_assign transpose sequence xlinvals ylinvals
	zlinvals axislinvals/;
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

# Exportable functions
*axisvals       = \&PDL::axisvals;		
*sec            = \&PDL::sec;		
*ins            = \&PDL::ins;		
*hist           = \&PDL::hist;		
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

etc. see 'zeroes'

=for example

  perldl> print xvals zeroes(5,10)
  Dims:  5,10  DLen:  400

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

etc. see 'zeroes'

=for example

 perldl> print yvals zeroes(5,10)
 Dims:  5,10  DLen:  400

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

etc. see 'zeroes'

=for example

 perldl> print zvals zeroes(3,4,2)
 Dims:  3,4,2  DLen:  192

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

=head2 xlinvals, ylinvals, zlinvals

=for ref

X,Y or Z axis values between endpoints. (see xvals,yvals,zvals)

=for usage

 $a = zeroes(100,100);
 $x = $a->xlinvals(0.5,1.5);
 $y = $a->ylinvals(-2,-1);
 $z = f($x,$y);            # calculate Z for X between 0.5 and 1.5 and
 			   # Y between -2 and -1.

xlinvals, ylinvals and zlinvals return a piddle with the same shape
as their first argument and linearly scaled values between the two other
arguments along the given axis.

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
	barf "Must have at least two elements in dimension for xlinvals"
		if $dim <= 1;
	return $_[0]->yvals * (($_[2] - $_[1]) / ($dim-1)) + $_[1];
}

sub PDL::zlinvals {
	my $dim = $_[0]->getdim(2);
	barf "Must have at least two elements in dimension for xlinvals"
		if $dim <= 1;
	return $_[0]->zvals * (($_[2] - $_[1]) / ($dim-1)) + $_[1];
}

=head2 hist

=for ref

Create histogram of a piddle

=for usage

 $hist = hist($data,[$min,$max,$step]);
 ($xvals,$hist) = hist($data,[$min,$max,$step]);

If requested, $xvals gives the computed bin centres

A nice idiom (with PDL::Graphics::PG) is

 bin hist $data;  # Plot histogram

=for example

 perldl> p $y
 [13 10 13 10 9 13 9 12 11 10 10 13 7 6 8 10 11 7 12 9 11 11 12 6 12 7 10 10 10 13]
 perldl> $h = hist $y,0,20,1
 hist with step 1, min 0 and 21 bins

 perldl> p $h
 [0 0 0 0 0 0 2 3 1 3 8 4 4 5 0 0 0 0 0 0 0]


=cut

sub PDL::hist {
    barf('Usage: ([$xvals],$hist) = hist($data,[$min,$max,$step])') if $#_<0;
    my($pdl,$min,$max,$step)=@_;
    $min = $pdl->min() unless defined $min;
    $max = $pdl->max() unless defined $max;
    my $ntype = $pdl->get_datatype;
    if (!defined $step) {
	my $defbins = 100 < $pdl->nelem ? 100 : $pdl->nelem;
	$step = ($max-$min)/$defbins;
	$step = int($step) > 0 ? int($step) : 1 if $ntype < $PDL_F;
    }
    barf "step is zero (or all data equal to one value)" if $step == 0;
    my $bins = int(($max-$min)/$step);
    print "hist with step $step, min $min and $bins bins\n"
      if $PDL::debug;
    PDL::Primitive::histogram($pdl->clump(-1),(my $hist = null),
			      $step,$min,$bins);
    my $xvals = $min + $step/2 + sequence(PDL::Type->new($ntype),$bins)*
        PDL::convert($step,$ntype) if wantarray();
    return wantarray() ? ($xvals,$hist) : $hist;
}

=head2 sequence

=for ref

Create array filled with a sequence of values

=for usage

 $a = sequence($b); $a = sequence [OPTIONAL TYPE], @dims;

etc. see 'zeroes'

=for example

  perldl> p sequence(10)
  Dims:  10  DLen:  80
  [0 1 2 3 4 5 6 7 8 9]
  perldl> p sequence(3,4)
  Dims:  12  DLen:  96

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

=for example

 perldl> print rvals long,7,7,{Centre=>[2,2]}
 Dims:  7,7  DLen:  196

 [
  [2 2 2 2 2 3 4]
  [2 1 1 1 2 3 4]
  [2 1 0 1 2 3 4]
  [2 1 1 1 2 3 4]
  [2 2 2 2 2 3 4]
  [3 3 3 3 3 4 5]
  [4 4 4 4 4 5 5]
 ]

=cut

sub rvals { ref($_[0]) && ref($_[0]) ne 'PDL::Type' ? $_[0]->rvals(@_[1..$#_]) : PDL->rvals(@_) }
sub PDL::rvals { # Return radial distance from given point and offset
    my $class = shift;
    my $opt = pop @_ if ref($_[$#_]) eq "HASH";
    my $r =  scalar(@_)? $class->new_from_specification(@_) : $class->new_or_inplace;
    my (@pos) = @{$opt->{'Centre'}} if exists $opt->{'Centre'} ;
    (@pos) = @{$opt->{'Center'}} if exists $opt->{'Center'} ;
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
    my $nothing = sqrt $r->inplace;
    return $r;
}

=head2 axisvals

=for ref

Fills a piddle with index values on Nth dimension

=for usage

 $z = axisvals ($piddle, $nth);

This is the routine, for which xvals(), yvals() etc
are mere shorthands. axisvals() can be used to fill
along any dimension.

Note the 'from specification' style (see 'zeroes') is
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

sub PDL::transpose {
	my($this) = @_;
	if($this->getndims == 1) {
# 1-Dim: add dummy
		return pdl $this->dummy(0);
	}
	my $tmp = PDL->null;
	$tmp .= $this->xchg(0,1);
	return $tmp;
}

1;

