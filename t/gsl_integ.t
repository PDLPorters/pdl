
# Test Script for the PDL interface to the GSL library
#  This tests mainly that the interface is working, i.e. that the
#   functions can be called. 
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use PDL;
use Test;
	
BEGIN{
  eval " use PDL::GSL::INTEG; ";
  unless ($@){
    plan tests => 22;
  }
  else {
    plan tests => 1;
    print "ok 1 # Skipped: PDL::GSL::INTEG not installed\n";
    exit;
  }
}


my $alfa = 2.6;

my ($res,$abserr,$neval,$ierr) = gslinteg_qng(\&f1,0,1,0,1e-9);
ok(abs($res - 0.0771604938270651) < 1e-6);
($res,$abserr,$neval,$ierr) = gslinteg_qng(\&f1,0,1,0,1e-9,{Warn => 'y'});
ok(abs($res - 0.0771604938270651) < 1e-6);

($res,$abserr,$ierr) = gslinteg_qag(\&f1,0,1,0,1e-10,1000,1);
ok(abs($res - 0.0771604938271586) < 1e-6);
($res,$abserr,$ierr) = gslinteg_qag(\&f1,0,1,0,1e-10,1000,1,{Warn => 'y'});
ok(abs($res - 0.0771604938271586) < 1e-6);

($res,$abserr,$ierr) = gslinteg_qags(\&f1,0,1,0,1e-10,1000);
ok(abs($res - 0.0771604938271579) < 1e-6);
($res,$abserr,$ierr) = gslinteg_qags(\&f1,0,1,0,1e-10,1000,{Warn => 'y'});
ok(abs($res - 0.0771604938271579) < 1e-6);

my $points = pdl(0,1,sqrt(2),3);
($res,$abserr,$ierr) = gslinteg_qagp(\&f454,$points,0,1e-3,1000);
ok(abs($res - 52.7408061167272) < 1e-6);
($res,$abserr,$ierr) = gslinteg_qagp(\&f454,$points,0,1e-3,1000,{Warn => 'y'});
ok(abs($res - 52.7408061167272) < 1e-6);

($res,$abserr,$ierr) = gslinteg_qagi(\&myfn1,1e-7,0,1000);
ok(abs($res -2.27587579446875 ) < 1e-6);
($res,$abserr,$ierr) = gslinteg_qagi(\&myfn1,1e-7,0,1000,{Warn => 'y'});
ok(abs($res -2.27587579446875 ) < 1e-6);

$alfa = 1;
($res,$abserr,$ierr) = gslinteg_qagiu(\&f16,99.9,1e-7,0,1000);
ok(abs($res -0.000100000000000671) < 1e-6);
($res,$abserr,$ierr) = gslinteg_qagiu(\&f16,99.9,1e-7,0,1000,{Warn => 'y'});
ok(abs($res -0.000100000000000671) < 1e-6);

($res,$abserr,$ierr) = gslinteg_qagil(\&myfn2,1.0,1e-7,0,1000);
ok(abs($res -2.71828182845905) < 1e-6);
($res,$abserr,$ierr) = gslinteg_qagil(\&myfn2,1.0,1e-7,0,1000,{Warn => 'y'});
ok(abs($res -2.71828182845905) < 1e-6);


($res,$abserr,$ierr) = gslinteg_qawc(\&f459,-1,5,0,0,1e-3,1000);
ok(abs($res + 0.08994400695837) < 1e-6);
($res,$abserr,$ierr) = gslinteg_qawc(\&f459,-1,5,0,0,1e-3,1000,{Warn => 'y'});
ok(abs($res + 0.08994400695837) < 1e-6);


($res,$abserr,$ierr) = gslinteg_qaws(\&f458,0,0,1,0,0,1,0,1e-7,1000);
ok(abs($res + 0.18927518534894) < 1e-6);
($res,$abserr,$ierr) = gslinteg_qaws(\&f458,0,0,1,0,0,1,0,1e-7,1000,{Warn => 'y'});
ok(abs($res + 0.18927518534894) < 1e-6);


my $PI = 3.14159265358979323846264338328;
($res,$abserr,$ierr) = gslinteg_qawo(\&f456,10*$PI,'sin',0,1,0,1e-7,1000);
ok(abs($res + 0.128136848399167) < 1e-6);
($res,$abserr,$ierr) = gslinteg_qawo(\&f456,10*$PI,'sin',0,1,0,1e-7,1000,{Warn => 'y'});
ok(abs($res + 0.128136848399167) < 1e-6);


($res,$abserr,$ierr) = gslinteg_qawf(\&f457,$PI/2.0,'cos',0,1e-7,1000);
ok(abs($res -0.999999999927978) < 1e-6);
($res,$abserr,$ierr) = gslinteg_qawf(\&f457,$PI/2.0,'cos',0,1e-7,1000,{Warn => 'y'});
ok(abs($res -0.999999999927978) < 1e-6);


sub f1{
    my ($x) = @_;
    return ($x**$alfa)*log(1.0/$x);
}

sub f454{
    my ($x) = @_;
    my $x2 = $x**2;
    my $x3 = $x**3;
    return $x3 * log(abs(($x2-1.0)*($x2-2.0)));
}

sub myfn1{    
    my ($x) = @_;
    return exp(-$x - $x*$x) ;
}

sub f16 {
  my ($x) = @_;
  if (($x==0) && ($alfa == 1)) {return 1;}
  if (($x==0) && ($alfa > 1)) {return 0;}
  return ($x**($alfa-1))/((1+10*$x)**2);
}

sub myfn2{
   my ($x) = @_;
   return exp($alfa*$x);
}

sub f459{
  my ($x) = @_;
  return 1.0 / (5.0 * $x * $x * $x + 6.0) ;
}

sub f458{
  my ($x) = @_;
  if($x==0){return 0;}
  else{
      my $u = log($x);
      my $v = 1 + $u*$u;
      return 1.0/($v*$v);
  }
}

sub f456{
  my ($x) = @_;
  if($x==0){return 0;}
  else{ return log($x);} 
}

sub f457{
    my ($x) = @_;
    if ($x == 0){return 0;}
    return 1.0/sqrt($x)    
}
