use PDL;
use PDL::Opt::Simplex;
#
# Simplex Demo - Alison Offer (aro@aaocbn.aao.gov.au)
#
#
# this is some sort of convergence criterion
$minsize = 1.e-6;
# max number of iterations ?
$maxiter = 100;
#
print " \n
        1: least squares gaussian fit to data + noise \n
	   32 *exp (-((x-10)/6)^2) + noise
        2: minimise polynomial \n 
	   (x-3)^2 + 2.*(x-3)*(y-2.5) + 3.*(y-2.5)^2 \n
	Please make your choice (1 or 2):";
chop($choice = <>);
if ($choice == 1) {
  print "Please enter noise factor (small number, 0-6):";
  chop($factor = <>);
#
# data : gaussian + noise
#
  foreach $j (0..19) {
       $data[$j] = 32*exp(-(($j-10)/6)**2) + 
                     $factor  * (rand() - 0.5);
  }
#
# initial guess - $initsize controls the size of the initial simplex (I think)
#
  $init = pdl [ 33, 9, 12 ];
  $initsize = 2;
#
#
  ($optimum,$ssize) = simplex($init,$initsize,$minsize,$maxiter,  
# this sub returns the function to be minimised.          
                            sub {my ($xv) =@_;
			        my $a = $xv->slice("(0)"); 
			        my $b = $xv->slice("(1)");                  
			        my $c = $xv->slice("(2)");   
				my $sum = $a * 0.0;
				foreach $j (0..19) {
				    $sum += ($data[$j] -
				     $a*exp(-(($j-$b)/$c)**2))**2;
				}
                                return $sum;
			    });
			    
} else {
  $init = pdl [ 2 , 2 ];
  $initsize = 2;
  ($optimum,$ssize) = simplex($init,$initsize,$minsize,$maxiter,           
                            sub {my ($xv) =@_;
			        my $x = $xv->slice("(0)"); 
			        my $y = $xv->slice("(1)"); 
			        return ($x-3)**2 + 2.*($x-3)*($y-2.5) 
				          + 3.*($y-2.5)**2; 
			    });
			     

}
print "OPTIMUM = $optimum \n";
print "SSIZE   = $ssize\n";

