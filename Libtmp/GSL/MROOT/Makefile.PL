use strict;
use warnings;
use ExtUtils::MakeMaker;

our ($GSL_includes, $GSL_libs); 

sub gsl_mroot_links_ok {
  my($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
    trylink('',
      << 'EOI',
#include <stdio.h>
#include <stdlib.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>
     struct rparams{
        double a;         
        double b;      
     };      
     int   rosenbrock_f (const gsl_vector * x, void *params,gsl_vector * f){
       double a = ((struct rparams *) params)->a;      
       double b = ((struct rparams *) params)->b;
       const double x0 = gsl_vector_get (x, 0);     
       const double x1 = gsl_vector_get (x, 1);     
       const double y0 = a * (1 - x0);  
       const double y1 = b * (x1 - x0 * x0);           
       gsl_vector_set (f, 0, y0);   
       gsl_vector_set (f, 1, y1);      
       return GSL_SUCCESS;    
    }
EOI
	<< 'EOB', $lib, $inc);

      const gsl_multiroot_fsolver_type *T;
      gsl_multiroot_fsolver *s;
      int status;
      size_t i, iter = 0;
      const size_t n = 2;
      struct rparams p = {1.0, 10.0};
      gsl_multiroot_function f = {&rosenbrock_f, n, &p};     
      double x_init[2] = {-10.0, -5.0};
      gsl_vector *x = gsl_vector_alloc (n);     
      gsl_vector_set (x, 0, x_init[0]);
      gsl_vector_set (x, 1, x_init[1]);    

      T = gsl_multiroot_fsolver_hybrids;
      s = gsl_multiroot_fsolver_alloc (T, 2);
      gsl_multiroot_fsolver_set (s, &f, x);
 
      do
         {
           iter++;
           status = gsl_multiroot_fsolver_iterate (s);
           if (status)   
                   break;                
           status =
             gsl_multiroot_test_residual (s->f, 1e-7);
         }
       while (status == GSL_CONTINUE && iter < 1000);            
     
     gsl_multiroot_fsolver_free (s);
     gsl_vector_free (x);       
     return 0;

EOB
}

if (!gsl_mroot_links_ok($GSL_libs, $GSL_includes)) {
  write_dummy_make( "Skipping build of PDL::GSL::MROOT." );
  return;
}

my @pack = (["gsl_mroot.pd", qw(MROOT PDL::GSL::MROOT)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;
$hash{OBJECT} .= ' FUNC$(OBJ_EXT)';

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };
WriteMakefile(%hash);
