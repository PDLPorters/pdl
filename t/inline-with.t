# This test checks this works: use Inline with => 'PDL';
# Also that the XS code in PDL::API works.

use strict;
use warnings;
use Test::More;
use PDL::LiteF;

my $inline_test_dir;
# First some Inline administrivia.
BEGIN {
   # Test for Inline and set options
   $inline_test_dir = './.inlinewith';
   mkdir $inline_test_dir unless -d $inline_test_dir;

   # See if Inline loads without trouble, or bail out
   eval {
      require Inline;
      Inline->import (Config => DIRECTORY => $inline_test_dir , FORCE_BUILD => 1);
#      Inline->import ('NOCLEAN');
      1;
   } or do {
      plan skip_all => "Skipped: Inline not installed";
   };

   if( $Inline::VERSION < 0.83 ) {
      plan skip_all => "Skipped: Inline has ILSM-finding bug";
   }	   
}
use File::Path;
END {
  if ($^O =~ /MSWin32/i) {
    for (my $i = 0; $i < @DynaLoader::dl_modules; $i++) {
      if ($DynaLoader::dl_modules[$i] =~ /inline_with_t/) {
        DynaLoader::dl_unload_file($DynaLoader::dl_librefs[$i]);
      }
    }
  }
}

SKIP: {
  #use Inline 'INFO'; # use to generate lots of info
  use_ok 'Inline', with => 'PDL' or skip 'with PDL failed', 3;
  eval { Inline->bind(C => <<'EOF') };
static pdl* new_pdl(int datatype, PDL_Indx dims[], int ndims)
{
  pdl *p = PDL->pdlnew();
  if (!p) return p;
  pdl_error err = PDL->setdims(p, dims, ndims);  /* set dims */
  if (err.error) { PDL->destroy(p); return NULL; }
  p->datatype = datatype;  /* and data type */
  err = PDL->allocdata(p); /* allocate the data chunk */
  if (err.error) { PDL->destroy(p); return NULL; }
  return p;
}

pdl* myfloatseq()
{
  PDL_Indx dims[] = {5,5,5};
  pdl *p = new_pdl(PDL_F,dims,3);
  if (!p) return p;
  PDL_Float *dataf = (PDL_Float *) p->data;
  PDL_Indx i; /* dimensions might be 64bits */

  for (i=0;i<5*5*5;i++)
    dataf[i] = i; /* the data must be initialized ! */
  return p;
}
EOF
  is $@, '', 'bind no error' or skip 'Inline C failed', 2;

  note "Inline Version: $Inline::VERSION\n";
  ok 1, 'compiled';

  my $pdl = myfloatseq();
  note $pdl->info,"\n";

  is $pdl->dims, 3, 'dims correct';
}

done_testing;
