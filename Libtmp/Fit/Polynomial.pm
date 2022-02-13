=head1 NAME

PDL::Fit::Polynomial - routines for fitting with polynomials

=head1 DESCRIPTION

This module contains routines for doing simple
polynomial fits to data

=head1 SYNOPSIS

    $yfit = fitpoly1d $data;


=head1 FUNCTIONS

=head2 fitpoly1d

=for ref

Fit 1D polynomials to data using min chi^2 (least squares)

=for usage

 Usage: ($yfit, [$coeffs]) = fitpoly1d [$xdata], $data, $order, [Options...]

=for sig

  Signature: (x(n); y(n); [o]yfit(n); [o]coeffs(order))

Uses a standard matrix inversion method to do a least
squares/min chi^2 polynomial fit to data. Order=2 is a linear
fit (two parameters).

Returns the fitted data and optionally the coefficients.

One can broadcast over extra dimensions to do multiple fits (except
the order can not be broadcasted over - i.e. it must be one fixed
scalar number like "4").

The data is normalised internally to avoid overflows (using the
mean of the abs value) which are common in large polynomial
series but the returned fit, coeffs are in
unnormalised units.

=for example

  $yfit = fitpoly1d $data,2; # Least-squares line fit
  ($yfit, $coeffs) = fitpoly1d $x, $y, 4; # Fit a cubic
  
  $fitimage = fitpoly1d $image,3  # Fit a quadratic to each row of an image
  
  $myfit = fitpoly1d $line, 2, {Weights => $w}; # Weighted fit

=for options  

  Options:
     Weights    Weights to use in fit, e.g. 1/$sigma**2 (default=1)

=cut

package PDL::Fit::Polynomial;

use strict;
use warnings;
use PDL::Core;
use PDL::Basic;
use PDL::Exporter;
use PDL::Options ':Func';
use PDL::MatrixOps; # for inv(), using this instead of call to Slatec routine

our @EXPORT_OK  = qw( fitpoly1d );
our %EXPORT_TAGS = (Func=>\@EXPORT_OK);
our @ISA    = qw( PDL::Exporter );

sub PDL::fitpoly1d {
   my $opthash = ref($_[-1]) eq "HASH" ? pop(@_) : {} ; 
   my %opt = parse( { Weights=>ones(1) }, $opthash ) ;
   barf "Usage: fitpoly1d incorrect args\n" if $#_<1 or $#_ > 2;
   my ($x, $y, $order) = @_;
   if ($#_ == 1) {
      ($y, $order) = @_;
      $x = $y->xvals;
   }

   my $wt = $opt{Weights};

   # Internally normalise data

   # means for each 1D data set
   my $xmean = (abs($x)->average)->dummy(0);  # dummy for correct broadcasting
   my $ymean = (abs($y)->average)->dummy(0);
   (my $tmp = $ymean->where($ymean == 0)) .= 1 if any $ymean == 0;
   ($tmp = $xmean->where($xmean == 0)) .= 1 if any $xmean == 0;
   my $y2 = $y / $ymean;
   my $x2 = $x / $xmean;

   # Do the fit

   my $pow = sequence($order);
   my $M = $x2->dummy(0) ** $pow;
   my $C = $M->transpose x ($M * $wt->dummy(0)) ;
   my $Y = $M->transpose x ($y2->dummy(0) * $wt->dummy(0));

   # Fitted coefficients vector

   ## $a1 = matinv($C) x $Y;
   ## print "matinv: \$C = $C, \$Y = $Y, \$a1 = $a1\n";
   my $a1 = inv($C) x $Y;  # use inv() instead of matinv() to avoid Slatec dependency
   ## print "inv:    \$C = $C, \$Y = $Y, \$a1 = $a1\n";
   
   # Fitted data

   my $yfit = ($M x $a1)->clump(2) * $ymean; # Remove first dim=1, un-normalise
   return wantarray ? ($yfit, $a1->clump(2) * $ymean / ($xmean ** $pow)) : $yfit;
}
*fitpoly1d = \&PDL::fitpoly1d;

=head1 BUGS

May not work too well for data with large dynamic range.

=head1 SEE ALSO

L<PDL::Slatec/"polyfit">

=head1 AUTHOR

This file copyright (C) 1999, Karl Glazebrook (kgb@aaoepp.aao.gov.au).
All rights reserved. There
is no warranty. You are allowed to redistribute this software
documentation under certain conditions. For details, see the file
COPYING in the PDL distribution. If this file is separated from the
PDL distribution, the copyright notice should be included in the file.

=cut

1;
