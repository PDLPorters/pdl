# This test checks this works: use Inline with => 'PDL';
# Also that the XS code in PDL::API works.

use strict;
use Test::More;
use blib;  # otherwise possible error on virgin systems not finding PDL::Core

use PDL::LiteF;

my $inline_test_dir;
# First some Inline administrivia.
BEGIN {
   # Test for Inline and set options
   $inline_test_dir = './.inlinewith';
   mkdir $inline_test_dir unless -d $inline_test_dir;

   # See if Inline loads without trouble, or bail out
   eval {
      no strict 'subs';
      if ( require (Inline) and $Inline::VERSION < 0.68 ) {
         plan skip_all => "Skipped: Inline not installed or correct VERSION < 0.68";
      }
   };
   eval {
      Inline->import (Config => DIRECTORY => $inline_test_dir , FORCE_BUILD => 1);
#      Inline->import ('NOCLEAN');
      1;
   } or do {
      plan skip_all => "Skipped: Inline->import(...) failed";
   };
}
use File::Path;
END { rmtree $inline_test_dir if -d $inline_test_dir }

# use Inline 'INFO'; # use to generate lots of info
use Inline with => 'PDL';
use Inline 'C';

note "Inline Version: $Inline::VERSION\n";
ok 1, 'compiled';

my $pdl = myfloatseq();
note $pdl->info,"\n";

is $pdl->dims, 3, 'dims correct';

done_testing;

__DATA__

__C__

static pdl* new_pdl(int datatype, PDL_Indx dims[], int ndims)
{
  pdl *p = PDL->pdlnew();
  PDL->setdims (p, dims, ndims);  /* set dims */
  p->datatype = datatype;         /* and data type */
  PDL->allocdata (p);             /* allocate the data chunk */

  return p;
}

pdl* myfloatseq()
{
  PDL_Indx dims[] = {5,5,5};
  pdl *p = new_pdl(PDL_F,dims,3);
  PDL_Float *dataf = (PDL_Float *) p->data;
  PDL_Indx i; /* dimensions might be 64bits */

  for (i=0;i<5*5*5;i++)
    dataf[i] = i; /* the data must be initialized ! */
  return p;
}
