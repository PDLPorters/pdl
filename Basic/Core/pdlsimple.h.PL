#!/usr/local/bin/perl

use Config;
use File::Basename qw(&basename &dirname);

require "pdl_hfiles.p";

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

print OUT <<"!GROK!THIS!";

#ifndef __PDL_H

/* These are kept automaticallu in sync with pdl.h during perl build */

$PDL_DATATYPES

#endif

/*
   Define a simple pdl C data structure which maps onto passed
   piddles for passing with callext().

   Note it is up to the user at the perl level to get the datatype
   right. Anything more sophisticated probably ought to go through
   PP anyway (which is fairly trivial).
*/

struct pdlsimple {
   int    datatype;  /* whether byte/int/float etc. */
   void  *data;      /* Generic pointer to the data block */
   int    nvals;     /* Number of data values */
   PDL_Long   *dims; /* Array of data dimensions */
   int    ndims;     /* Number of data dimensions */
};

typedef struct pdlsimple pdlsimple;

!GROK!THIS!
