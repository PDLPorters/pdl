# PDL interface to GSL differentation routines
# Makefile.PL for a package defined by PP code.

use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

my $msg = undef;
my $forcebuild=0;
my $skip = 0;

# this Makefile uses get_gsl_libs which is defined in
# the parent Makefile.PL

sub gsl_mroot_links_ok {
  my($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
    trylink 'gsl multidimensional root finding libraries',
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
	<< 'EOB', $lib, $inc;

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

if (defined $PDL::Config{WITH_GSL} && $PDL::Config{WITH_GSL}==0) {
  $msg = "\n   Will skip build of PDL::GSL::MROOT on this system   \n";
  $skip = 1;
} elsif (defined $PDL::Config{WITH_GSL} && $PDL::Config{WITH_GSL}==1) {
  print "\n   Will forcibly try and build PDL::GSL::MROOT on this system   \n\n";
  $forcebuild=1;
}

if (($skip && !$forcebuild) ||
    !gsl_mroot_links_ok($GSL_libs, $GSL_includes)) {
  warn "trying to force GSL build but link test failed\n".
    "\t -- aborting GSL build\n" if $forcebuild;
  $msg ||=
    "\n GSL Libraries not found... Skipping build of PDL::GSL::MROOT.\n";
  warn $msg . "\n";
  $msg =~ s/\n//g;
  write_dummy_make( $msg );
  return;
} else {
  print "\n   Building PDL::GSL::MROOT.", 
    "Turn off WITH_GSL if there are any problems\n\n";
}

@pack = (["gsl_mroot.pd",MROOT,PDL::GSL::MROOT]);
%hash = pdlpp_stdargs_int(@::pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;
WriteMakefile(%hash);


sub MY::postamble {
        pdlpp_postamble_int(@::pack);
}  # Add genpp rule
