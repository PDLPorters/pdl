use Config;
use File::Basename qw(&basename &dirname);

require './Types.pm';

# List explicitly here the variables you want Configure to
# generate.  Metaconfig only looks for shell variables, so you
# have to mention them as if they were shell variables, not
# %Config entries.  Thus you write
#  $startperl
# to ensure Configure will look for $Config{startperl}.

# This forces PL files to create target in same directory as PL file.
# This is so that make depend always knows where to find PL derivatives.
chdir(dirname($0));
my $file;
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//
	if ($Config{'osname'} eq 'VMS' or
	    $Config{'osname'} eq 'OS2');  # "case-forgiving"
open OUT,">$file" or die "Can't create $file: $!";

print "Extracting $file (with variable substitutions)\n";

# In this section, perl variables will be expanded during extraction.
# You can use $Config{...} to use Configure variables.

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'EOF';

#include <complex.h>
#include <stddef.h>
#include <stdint.h>
#ifndef __PDL_H

/* These are kept automatically in sync with pdl.h during perl build */
EOF

my @methods = qw(symbol ctype ppsym shortctype defbval realctype);
sub makeg { map { my $t = $_; [map $t->$_, @methods] } grep $_[0]->($_), PDL::Types::types() }
sub makelister {
  my ($name, $is2, $underscore, @list) = @_;
  my $suff = $is2 ? '2' : '';
  my $arg1 = $is2 ? 'X, X2' : 'X';
  my $arg2 = $is2 ? 'X2, ' : '';
  $underscore = $underscore ? '_' : '';
  ("#define PDL_TYPELIST${suff}_$name$underscore($arg1) \\\n",
    (map " X($arg2".join(',', @$_).")\\\n", @list), "\n\n");
}
my @generics = makeg(sub {1});
print OUT makelister('ALL', 0, 0, @generics);

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'EOF';

#define X(sym, ...) \
  , sym
typedef enum {
   PDL_INVALID=-1
PDL_TYPELIST_ALL(X)
} pdl_datatypes;
#undef X

#define X(sym, ctype, ppsym, shortctype, defbval, realctype, ...) \
  typedef realctype ctype;
PDL_TYPELIST_ALL(X)
#undef X

#endif

/*
   Define a simple pdl C data structure which maps onto passed
   ndarrays for passing with callext().

   Note it is up to the user at the perl level to get the datatype
   right. Anything more sophisticated probably ought to go through
   PP anyway (which is fairly trivial).
*/

typedef struct {
   pdl_datatypes datatype;  /* whether byte/int/float etc. */
   void       *data;  /* Generic pointer to the data block */
   PDL_Indx  nvals;  /* Number of data values */
   PDL_Indx  *dims;  /* Array of data dimensions */
   PDL_Indx  ndims;  /* Number of data dimensions */
} pdlsimple;
EOF
