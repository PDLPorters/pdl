=head1 NAME

PDL::Fit::Linfit - routines for fitting data with linear combinations of functions.

=head1 DESCRIPTION

This module contains routines to perform general curve-fits to a set (linear combination)
of specified functions. 

Given a set of Data:

  (y0, y1, y2, y3, y4, y5, ...ynoPoints-1)

The fit routine tries to model y as:

  y' = beta0*x0 + beta1*x1 + ... beta_noCoefs*x_noCoefs

Where x0, x1, ... x_noCoefs, is a set of functions (curves) that
the are combined linearly using the beta coefs to yield an approximation
of the input data.

The Sum-Sq error is reduced to a minimum in this curve fit.

B<Inputs:>

=over 1

=item $data

This is your data you are trying to fit. Size=n

=item $functions

2D array. size (n, noCoefs). Row 0 is the evaluation
of function x0 at all the points in y. Row 1 is the evaluation of
of function x1 at all the points in y, ... etc.

Example of $functions array Structure:

$data is a set of 10 points that we are trying to model using
the linear combination of 3 functions. 

 $functions = ( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ],  # Constant Term
		[ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ],  # Linear Slope Term
		[ 0, 2, 4, 9, 16, 25, 36, 49, 64, 81] # quadradic term
	    )

=back

=head1 SYNOPSIS

    $yfit = linfit1d $data, $funcs


=head1 FUNCTIONS

=head2 linfit1d

=for ref

1D Fit linear combination of supplied functions to data using min chi^2 (least squares).

=for usage

 Usage: ($yfit, [$coeffs]) = linfit1d [$xdata], $data, $fitFuncs, [Options...]

=for signature

Signature: (xdata(n); ydata(n); $fitFuncs(n,order); [o]yfit(n); [o]coeffs(order))

Uses a standard matrix inversion method to do a least
squares/min chi^2 fit to data. 

Returns the fitted data and optionally the coefficients.

One can thread over extra dimensions to do multiple fits (except
the order can not be threaded over - i.e. it must be one fixed
set of fit functions C<fitFuncs>.

The data is normalised internally to avoid overflows (using the
mean of the abs value) which are common in large polynomial
series but the returned fit, coeffs are in
unnormalised units.


=for example

  # Generate data from a set of functions
  $xvalues = sequence(100);
  $data = 3*$xvalues + 2*cos($xvalues) + 3*sin($xvalues*2); 
  
  # Make the fit Functions
  $fitFuncs = cat $xvalues, cos($xvalues), sin($xvalues*2);
  
  # Now fit the data, Coefs should be the coefs in the linear combination
  #   above: 3,2,3
  ($yfit, $coeffs) = linfit1d $data,$fitFuncs;
  

=for options  

  Options:
     Weights    Weights to use in fit, e.g. 1/$sigma**2 (default=1)


=cut

package PDL::Fit::Linfit;

@EXPORT_OK  = qw( linfit1d );
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

use PDL::Core;
use PDL::Basic;
use PDL::Exporter;
@ISA    = qw( PDL::Exporter );
use PDL::Options ':Func';
use PDL::Slatec; # For matinv()

sub PDL::linfit1d {
   my $opthash = ref($_[-1]) eq "HASH" ? pop(@_) : {} ; 
   my %opt = parse( { Weights=>ones(1) }, $opthash ) ;
   barf "Usage: linfit1d incorrect args\n" if $#_<1 or $#_ > 3;
   my ($x, $y, $fitfuncs) = @_;
   if ($#_ == 1) {
      ($y, $fitfuncs) = @_;
      $x = $y->xvals;
   }
   
   my $wt = $opt{Weights};
   
   # Internally normalise data
   
   my $ymean = (abs($y)->sum)/($y->nelem);
   $ymean = 1 if $ymean == 0;
   my $y2 = $y / $ymean;
   
   # Do the fit
      
   my $M = $fitfuncs->xchg(0,1);
   my $C = $M->xchg(0,1) x ($M * $wt->dummy(0)) ;
   my $Y = $M->xchg(0,1) x ($y2->dummy(0) * $wt->dummy(0));

   # Fitted coefficients vector

   $a = matinv($C) x $Y;
   
   # Fitted data

   $yfit = ($M x $a)->clump(2); # Remove first dim=1
   
   $yfit *= $ymean; # Un-normalise
   if (wantarray) {
      my $coeff = $a->clump(2);
      $coeff *= $ymean; # Un-normalise
      return ($yfit, $coeff);
   }
   else{
      return $yfit;
   }  
   
}
*linfit1d = \&PDL::linfit1d;


1;
