use strict;
use warnings;
use ExtUtils::MakeMaker;

our ($GSL_includes, $GSL_libs);

sub gsl_linalg_links_ok {
  my ($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
    trylink('',
      << 'EOI',
#include <gsl/gsl_linalg.h>
EOI
	<< 'EOB', $lib, $inc);
  double a_data[] = { 0.18, 0.60, 0.57, 0.96,
                      0.41, 0.24, 0.99, 0.58,
                      0.14, 0.30, 0.97, 0.66,
                      0.51, 0.13, 0.19, 0.85 };
  double b_data[] = { 1.0, 2.0, 3.0, 4.0 };
  gsl_matrix_view m = gsl_matrix_view_array(a_data, 4, 4);
  gsl_vector_view b = gsl_vector_view_array(b_data, 4);
  gsl_vector *x = gsl_vector_alloc(4);
  int s;
  gsl_permutation *p = gsl_permutation_alloc(4);
  gsl_linalg_LU_decomp(&m.matrix, p, &s);
  gsl_linalg_LU_solve(&m.matrix, p, &b.vector, x);
  gsl_permutation_free(p);
  gsl_vector_free(x);
  return 0;
EOB
}

if (!gsl_linalg_links_ok($GSL_libs, $GSL_includes)) {
  write_dummy_make( "Skipping build of PDL::GSL::LINALG." );
  return;
}

my @pack = (["gsl_linalg.pd", qw(LINALG PDL::GSL::LINALG)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };
WriteMakefile(%hash);
