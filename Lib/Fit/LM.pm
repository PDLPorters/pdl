=head1 NAME

PDL::Fit::LM -- Levenber-Marquardt fitting routine for PDL

=head1 DESCRIPTION

This module provides fitting functions for PDL. Currently, only
Levenberg-Marquardt fitting is implemented. Other procedures should
be added as required. For a fairly concise overview on fitting
see Numerical Recipes, chapter 15 "Modeling of data".

=head1 SYNOPSIS

 use PDL::Fit::LM;
 $ym = lmfit $x, $y, $sig, \&expfunc, $a, {Maxiter => 300};

=head1 FUNCTIONS

=cut

package PDL::Fit::LM;

@EXPORT_OK  = qw( lmfit tlmfit);
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

use PDL::Core;
use PDL::Exporter;
use PDL::Options;
use PDL::Slatec; # for matrix inversion

@ISA    = qw( PDL::Exporter );

=head2 lmfit

=for ref

Levenberg-Marquardt fitting of a user supplied model function

=for example

 ($ym,$a,$covar,$iters) =
      lmfit $x, $y, $sig, \&expfunc, $a, {Maxiter => 300, Eps => 1e-3};

Options:

=for options

 Maxiter:  maximum number of iterations before giving up
 Eps:      convergence citerium for fit; success when normalized change
           in chisquare smaller than Eps

The user supplied sub routine reference should accept 4 arguments

=over 4

=item *

a vector of independent values $x

=item *

a vector of fitting parameters

=item *

a vector of dependent variables that will be assigned upon return

=item *

a matrix of partial derivatives with respect to the fitting parameters
that will be assigned upon return

=back

As an example take this definition of a single exponential with
3 parameters (width, amplitude, offset):

 sub expdec {
   my ($x,$par,$ym,$dyda) = @_;
   my ($a,$b,$c) = map {$par->slice("($_)")} (0..2);
   my $arg = $x/$a;
   my $ex = exp($arg);
   $ym .= $b*$ex+$c;
   my (@dy) = map {$dyda->slice(",($_)")} (0..2);
   $dy[0] .= -$b*$ex*$arg/$a;
   $dy[1] .= $ex;
   $dy[2] .= 1;
 }

Note usage of the C<.=> operator for assignment 

In scalar context returns a vector of the fitted dependent
variable. In list context returns fitted y-values, vector
of fitted parameters, an estimate of the covariance matrix
(as an indicator of goodness of fit) and number of iterations
performed.

=cut

sub PDL::lmfit {
  my ($x,$y,$sig,$func,$a,$opt) = @_; # not using $ia right now
  $opt = {iparse( { Maxiter => 200,
		  Eps => 1e-4}, ifhref($opt))};
  my ($maxiter,$eps) = map {$opt->{$_}} qw/Maxiter Eps/;
  # initialize some variables
  my ($isig2,$chisq) = (1/($sig*$sig),0);
  my ($ym,$al,$cov,$bet,$oldbet,$olda,$oldal,$ochisq,$di,$pivt,$info) =
    map {null} (0..10);
  my ($aldiag,$codiag);  # the diagonals for later updating
  # this will break threading
  my $dyda = zeroes($x->type,$x->getdim(0),$a->getdim(0));
  my $alv = zeroes($x->type,$x->getdim(0),$a->getdim(0),$a->getdim(0));
  my ($iter,$lambda) = (0,0.001);

  do {
    if ($iter>0) {
      $cov .= $al;
      # local $PDL::debug = 1;
      $codiag .= $aldiag*(1+$lambda);
      gefa $cov, $pivt, $info;     # gefa + gesl = solution by Gaussian elem.
      gesl $cov, $pivt, $bet, 0;   # solution returned in $bet
      # lusd($cov,$bet,$da);
      # print "changing by $da\n";
      $a += $bet;                  # what we used to call $da is now $bet
    }
    &$func($x,$a,$ym,$dyda);
    $chisq = ($y-$ym)*($y-$ym);
    $chisq *= $isig2;
    $chisq = $chisq->sumover;                   # calculate chi^2
    $dyda->xchg(0,1)->outer($dyda->xchg(0,1),$alv->mv(0,2));
    $alv *= $isig2;
    $alv->sumover($al);                         # calculate alpha
    (($y-$ym)*$isig2*$dyda)->sumover($bet);     # calculate beta
    if ($iter == 0) {$olda .= $a; $ochisq .= $chisq; $oldbet .= $bet;
                     $oldal .= $al; $aldiag = $al->diagonal(0,1);
                     $cov .= $al; $codiag = $cov->diagonal(0,1)}
    $di .= abs($chisq-$ochisq);
    # print "$iter: chisq, lambda, dlambda: $chisq, $lambda,",$di/$chisq,"\n";
    if ($chisq < $ochisq) {
      $lambda *= 0.1;
      $ochisq .= $chisq;
      $olda .= $a;
      $oldbet .= $bet;
      $oldal .= $al;
    } else {
      $lambda *= 10;
      $chisq .= $ochisq;
      $a .= $olda;      # go back to previous a
      $bet .= $oldbet;  # and beta
      $al .= $oldal;    # and alpha
    }
  } while ($iter++==0 || $iter < $maxiter && $di/$chisq > $eps);
  barf "iteration did not converge" if $iter >= $maxiter && $di/$chisq > $eps;
  # return inv $al as estimate of covariance matrix
  return wantarray ? ($ym,$a,matinv($al),$iter) : $ym;
}
*lmfit = \&PDL::lmfit;

=pod

An extended example script that uses lmfit is included below.
This nice example was provided by John Gehman and should
help you to master the initial hurdles. It can also be found in
the F<Example/Fit> directory.

   use PDL;
   use PDL::Math;
   use PDL::Fit::LM;
   use strict;


   ### fit using pdl's lmfit (Marquardt-Levenberg non-linear least squares fitting)
   ###
   ### `lmfit' Syntax: 
   ###
   ### ($ym,$a,$covar,$iters) 
   ###	= lmfit $x, $y, $sig, \&fn, $initp, {Maxiter => 300, Eps => 1e-3};
   ###
   ### Explanation of variables
   ### 
   ### OUTPUT
   ### $ym =    pdl of fitted values
   ### $a  =    pdl of paramters
   ### $covar = covariance matrix
   ### $iters = number of iterations actually used
   ###
   ### INPUT
   ### $x =      x data
   ### $y =      y data
   ### $sig =    weights for y data (can be set to scalar 1 for equal weighting)
   ### \&fn =    reference to function provided by user (more on this below) 
   ### $initp =  initial values for floating parameters 
   ###               (needs to be explicitly set prior to use of lmfit)
   ### Maxiter = maximum iterations
   ### Eps =     convergence criterium (maximum normalized change in Chi Sq.)

   ### Example:
   # make up experimental data:
   my $xdata = pdl sequence 5;
   my $ydata = pdl [1.1,1.9,3.05,4,4.9];

   # set initial prameters in a pdl (order in accord with fit function below)
   my $initp = pdl [0,1];

   # Weight all y data equally (else specify different weights in a pdl)
   my $wt = 1;

   # Use lmfit. Fourth input argument is reference to user-defined 
   # subroutine ( here \&linefit ) detailed below.
   my ($yf,$pf,$cf,$if) = lmfit $xdata, $ydata, $wt, \&linefit, $initp;

   # Note output
   print "\nXDATA\n$xdata\nY DATA\n$ydata\n\nY DATA FIT\n$yf\n\n";
   print "Slope and Intercept\n$pf\n\nCOVARIANCE MATRIX\n$cf\n\n";
   print "NUMBER ITERATIONS\n$if\n\n";


   # simple example of user defined fit function. Guidelines included on
   # how to write your own function subroutine.
   sub linefit {

	   # leave this line as is
	   my ($x,$par,$ym,$dyda) = @_;

	   # $m and $b are fit parameters, internal to this function
	   # call them whatever make sense to you, but replace (0..1)
	   # with (0..x) where x is equal to your number of fit parameters
	   # minus 1
	   my ($m,$b) = map { $par->slice("($_)") } (0..1);

	   # Write function with dependent variable $ym,
	   # independent variable $x, and fit parameters as specified above.
	   # Use the .= (dot equals) assignment operator to express the equality 
	   # (not just a plain equals)
	   $ym .= $m * $x + $b;

	   # Edit only the (0..1) part to (0..x) as above
	   my (@dy) = map {$dyda -> slice(",($_)") } (0..1);

	   # Partial derivative of the function with respect to first 
	   # fit parameter ($m in this case). Again, note .= assignment 
	   # operator (not just "equals")
	   $dy[0] .= $x;

	   # Partial derivative of the function with respect to next 
	   # fit parameter ($b in this case)
	   $dy[1] .= 1;

	   # Add $dy[ ] .= () lines as necessary to supply 
	   # partial derivatives for all floating paramters.
   }

=cut

# the OtherPar is the sub routine ref

=head2 tlmfit

=for ref

threaded version of Levenberg-Marquardt fitting routine mfit

=for example

 tlmfit $x, $y, float(1)->dummy(0), $na, float(200), float(1e-4),
       $ym=null, $afit=null, \&expdec;

Signature:

=for sig

 tlmfit(x(n);y(n);sig(n);a(m);iter();eps();[o] ym(n);[o] ao(m);
           OtherPar => subref)

a threaded version of C<lmfit> by using perl threading. Direct
threading in C<lmfit> seemed difficult since we have an if condition
in the iteration. In principle that can be worked around by
using C<where> but .... Send a threaded C<lmfit> version if
you work it out!

Since we are using perl threading here speed is not really great
but it is just convenient to have a threaded version for many
applications (no explicit for-loops required, etc). Suffers from
some of the current limitations of perl level threading.

=cut


thread_define 'tlmfit(x(n);y(n);sig(n);a(m);iter();eps();[o] ym(n);[o] ao(m)),
               NOtherPars => 1',
  over {
    $_[7] .= $_[3]; # copy our parameter guess into the output
    $_[6] .= PDL::lmfit $_[0],$_[1],$_[2],$_[8],$_[7],{Maxiter => $_[4],
					   Eps => $_[5]};
  };

1;

=head1 BUGS

Not known yet.

=head1 AUTHOR

This file copyright (C) 1999, Christian Soeller
(c.soeller@auckland.ac.nz).  All rights reserved. There is no
warranty. You are allowed to redistribute this software documentation
under certain conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution, the
copyright notice should be included in the file.

=cut

# return true
1;
