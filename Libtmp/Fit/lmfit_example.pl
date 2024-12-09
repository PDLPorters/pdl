use PDL;
use PDL::Math;
use PDL::Fit::LM;
use strict;


### fit using pdl's lmfit (Marquardt-Levenberg non-linear least squares fitting)
###
### `lmfit' Syntax:
###
### ($ym,$finalp,$covar,$iters)
###	= lmfit $x, $y, $sigma, \&fn, $initp, {Maxiter => 300, Eps => 1e-3};
###
### Explanation of variables
###
### OUTPUT
### $ym     = pdl of fitted values
### $finalp = pdl of parameters
### $covar  = covariance matrix
### $iters  = number of iterations actually used
###
### INPUT
### $x      = x data
### $y      = y data
### $sigma  = ndarray of y-uncertainties for each value of $y (can be set to scalar 1 for equal weighting)
### \&fn    = reference to function provided by user (more on this below)
### $initp  = initial values for floating parameters
###               (needs to be explicitly set prior to use of lmfit)
### Maxiter = maximum iterations
### Eps     = convergence criterion (maximum normalized change in Chi Sq.)

### Example:
# make up experimental data:
my $xdata = pdl sequence 5;
my $ydata = pdl [1.1,1.9,3.05,4,4.9];

# set initial prameters in a pdl (order in accord with fit function below)
my $initp = pdl [0,1];

# Weight all y data equally (else specify different uncertainties in a pdl)
my $sigma = 1;

# Use lmfit. Fourth input argument is reference to user-defined
# subroutine ( here \&linefit ) detailed below.
my ($yf,$pf,$cf,$if) = lmfit $xdata, $ydata, $sigma, \&linefit, $initp;

# Note output
print "\nXDATA\n$xdata\nY DATA\n$ydata\n\nY DATA FIT\n$yf\n\n";
print "Slope and Intercept\n$pf\n\nCOVARIANCE MATRIX\n$cf\n\n";
print "NUMBER ITERATIONS\n$if\n\n";


# simple example of user defined fit function. Guidelines included on
# how to write your own function subroutine.
sub linefit {

	# leave this line as is
	my ($x,$par,$ym,$dyda) = @_;

	# $m and $c are fit parameters, internal to this function
	# call them whatever make sense to you, but replace (0..1)
	# with (0..x) where x is equal to your number of fit parameters
	# minus 1
        my ($m,$c) = map { $par->slice("($_)") } (0..1);

	# Write function with dependent variable $ym,
	# independent variable $x, and fit parameters as specified above.
	# Use the .= (dot equals) assignment operator to express the equality
	# (not just a plain equals)
        $ym .= $m * $x + $c;

	# Edit only the (0..1) part to (0..x) as above
        my (@dy) = map {$dyda -> slice(",($_)") } (0..1);

	# Partial derivative of the function with respect to first
	# fit parameter ($m in this case). Again, note .= assignment
	# operator (not just "equals")
        $dy[0] .= $x;

	# Partial derivative of the function with respect to next
        # fit parameter ($c in this case)
	$dy[1] .= 1;

	# Add $dy[ ] .= () lines as necessary to supply
	# partial derivatives for all floating parameters.
}

