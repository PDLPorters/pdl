=head1 NAME

PDL::Filter::Linear - linear filtering for PDL

=head1 SYNOPSIS

	$a = new PDL::Filter::Linear(
		{Weights => $v,
		 Point => 10});

	$b = new PDL::Filter::Gaussian(15,2); # 15 points, 2 std devn.

	($pred,$corrslic) = $a->predict($dat);

=head1 DESCRIPTION

A wrapper for generic linear filters.
Just for convenience. This should in the future use DataPresenter.

Also, this class should at some point learn to do FFT whenever it is
useful.

=cut

package PDL::Filter::Linear;
use PDL;
use PDL::Basic;
use PDL::Slices;
use PDL::Primitive;
use strict;

sub new($$) {
	my($type,$pars) = @_;

	my $this = bless {},$type;
        barf("Must specify weights\n") unless defined $pars->{Weights};
	$this->{Weights} = delete $pars->{Weights};
	$this->{Point} = defined $pars->{Point} ? $pars->{Point} : 0;
	$this;
}

sub predict($$) {
	my($this,$data) = @_;
	my $ldata = $data->lags(0,1,$this->{Weights}->getdim(0));
	inner($ldata->xchg(0,1),$this->{Weights},
		(my $pred = PDL->null));
	return wantarray ?  ($pred,$ldata->slice(":,($this->{Point})")) :
		$pred ;
}

package PDL::Filter::Gaussian;
use PDL; use PDL::Basic; use PDL::Slices; use PDL::Primitive;
use strict;

@PDL::Filter::Gaussian::ISA = qw/PDL::Filter::Linear/;

sub new($$) {
	my($type,$npoints,$sigma) = @_;
	my $cent = int($npoints/2);
	my $x = ((PDL->zeroes($npoints )->xvals) - $cent)->float;
	my $y = exp(-($x**2)/(2*$sigma**2));
# Normalize to unit total
	$y /= sum($y);
	return PDL::Filter::Linear::new($type,{Weights => $y,
			Point => $cent});
}

# Savitzky-Golay (see Numerical Recipes)
package PDL::Filter::SavGol;
use PDL; use PDL::Basic; use PDL::Slices; use PDL::Primitive;
use strict;

@PDL::Filter::Gaussian::ISA = qw/PDL::Filter::Linear/;

# XXX Doesn't work
sub new($$) {
	my($type,$deg,$nleft,$nright) = @_;
	my $npoints = $nright + $nleft + 1;
	my $x = ((PDL->zeroes($npoints )->xvals) - $nleft)->float;
	my $mat1 = ((PDL->zeroes($npoints,$deg+1)->xvals))->float;
	for(0..$deg-1) {
		(my $tmp = $mat1->slice(":,($_)")) .= ($x ** $_);
	}
	my $y;
# Normalize to unit total
	return PDL::Filter::Linear::new($type,{Weights => $y,
			Point => $nleft});
}


