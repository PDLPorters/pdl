use strict;
use warnings;
use Test::More;
use Config;
use File::Spec;
use File::Temp qw(tempdir);
use PDL::Core::Dev;

eval { require ExtUtils::CBuilder; 1 }
  or plan skip_all => 'ExtUtils::CBuilder not available';

my $cb = ExtUtils::CBuilder->new(quiet => 1);
$cb->have_cplusplus
  or plan skip_all => 'No C++ compiler available';

my $dir = tempdir(CLEANUP => 1);
my $src = File::Spec->catfile($dir, 'pdl_cpp_test.cpp');

open my $fh, '>', $src or die "open $src: $!";
print {$fh} <<'EOF';
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "pdl.h"
#include "pdlcore.h"
#include "pdlperl.h"

template <typename I>
I pdl_test_template_I_after(I x) { return x; }

int pdl_cpp_header_smoke_test(void) {
  PDL_Anyval a, b, c;
  PDL_CDouble z = pdl_cmplx(3.5, 2.25);
  ANYVAL_FROM_CTYPE(a, PDL_CD, z);
  ANYVAL_TO_ANYVAL_NEWTYPE(a, b, PDL_CF);
  ANYVAL_TO_ANYVAL_NEWTYPE(b, c, PDL_D);
  double d = 0.0;
  ANYVAL_TO_CTYPE(d, double, a);
  int n   = ANYVAL_ISNAN(a);
  int eq1 = ANYVAL_EQ_ANYVAL(a, b);
  int eq2 = ANYVAL_EQ_ANYVAL(a, c);
  int bad = ANYVAL_ISBAD(a, b);
  return (int)d + n + eq1 + eq2 + bad
       + pdl_test_template_I_after(2);
}
EOF
close $fh;

my $pdl_inc = PDL::Core::Dev::PDL_INCLUDE();
$pdl_inc =~ s/^"//;
$pdl_inc =~ s/"$//;
$pdl_inc =~ s/^-I//;

my $obj = eval {
  $cb->compile(
    source       => $src,
    'C++'        => 1,
    include_dirs => [ $pdl_inc ],
  );
};
my $err = $@;
ok(defined $obj && -f $obj, 'pdl.h + pdlcore.h + pdlperl.h compile cleanly as C++')
  or diag("C++ compilation failed: $err");

done_testing;
