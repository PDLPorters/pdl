# -*-perl-*-
#
# The contents depend on whether we have bad-value support in PDL.
# - in fact we don't create anything if there is no bad-value support
#
# This is probably overkill - the NDF module provides the
# starlink bad values as constants, so we can code up perl-level
# functions to do both the check and get functions implemented here.
#
 
use strict;

use Config;
use File::Basename qw(&basename &dirname);

# check for bad value support
use File::Spec;
require File::Spec->catfile( File::Spec->updir, File::Spec->updir, "perldl.conf" );
use vars qw( %PDL_CONFIG );
my $bvalflag = $PDL_CONFIG{WITH_BADVAL} || 0;
 
# This forces PL files to create target in same directory as PL file.
# This is so that make depend always knows where to find PL derivatives.
chdir(dirname($0));
my $file;
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//
    if ($Config{'osname'} eq 'VMS' or
	$Config{'osname'} eq 'OS2');  # "case-forgiving"

# if we don't have bad value support, we don't need a
# NDF.xs file
if ( $bvalflag == 0 ) {
    print "No need to create $file (removing any old copies)\n";
    unlink $file if -e $file;
    exit;
}
 
print "Extracting $file (WITH bad value support)\n";

open OUT,">$file" or die "Can't create $file: $!";
chmod 0644, $file;

#########################################################

use PDL::Core;
use PDL::Types;
my $ntypes = $#PDL::Types::names;

sub convert_ctype ($) { $_ = shift; s/^PDL_//; return $_; }

#########################################################

print OUT <<'!NO!SUBS!';

/* 
 * This file is automatically created by NDF.xs.PL 
 *  - bad value support = 1
 */

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "ppport.h"
#include "XSUB.h"     /* XSUB include */

#include <float.h>    /* use these to define STARLINK bad values */
#include <limits.h>

#include "pdl.h"
#include "pdlcore.h"

/* define variables */

SV* CoreSV;       /* Gets pointer to perl var holding core structure */
static Core* PDL; /* Structure hold core C functions */

struct starlink_badvalues {
!NO!SUBS!

foreach my $i ( reverse(0 .. $ntypes) ) {
    my $name = $PDL::Types::names[$i];
    my $realctype = $PDL::Types::typehash{$name}->{realctype};
    my $bname     = convert_ctype $PDL::Types::typehash{$name}->{ctype};
    print OUT "   $realctype $bname;\n";
}

print OUT <<'!NO!SUBS!';    
};

struct starlink_badvalues starlink;

/*
 * compare bad values (cbv)
 *
 * return 1 if the STARLINK bad value for this type matches
 * that of the pdl data type, 0 otherwise
 * should take advantage of the knowledge that if we use NaNs
 * then there is no match
 */

/*
 * get bad value (gbv)
 *
 * return the STARLINK bad value for a given piddle type
 * - this is actually NOT needed, since the NDF module
 *   provides access to the values
 */

MODULE = PDL::IO::NDF     PACKAGE = PDL::IO::NDF

!NO!SUBS!

my $str;
foreach my $i ( 0 .. $ntypes ) {
    my $type = PDL::Type->new( $i );

    my $ctype     = $type->ctype;
    my $bname     = convert_ctype $ctype;
    my $realctype = $type->realctype;

    $str .= 
"
int
_cbv_int${i}()
  CODE:
    RETVAL = (PDL->bvals.$bname == starlink.$bname);
  OUTPUT:
    RETVAL
  
$realctype
_gbv_int${i}()
  CODE:
    RETVAL = starlink.$bname;
  OUTPUT:
    RETVAL
  
";

} # foreach: $i = 0 .. $ntypes

print OUT $str;
print OUT <<'!NO!SUBS!';

BOOT:
   /* Get pointer to structure of core shared C routines */
   CoreSV = perl_get_sv("PDL::SHARE",FALSE);  /* SV* value */
#ifndef aTHX_
#define aTHX_
#endif
   if (CoreSV==NULL)
     Perl_croak(aTHX_ "This module requires use of PDL::Core first");
   PDL = (Core*) (void*) SvIV( CoreSV );  /* Core* value */
   if (PDL->Version != PDL_CORE_VERSION)
     croak("PDL::IO::NDF needs to be recompiled against the newly installed PDL");

   /* initialise bad values */
   starlink.Byte   = UCHAR_MAX;
   starlink.Short  = SHRT_MIN;
   starlink.Ushort = USHRT_MAX;
   starlink.Long   = INT_MIN;
   starlink.Float  = -FLT_MAX;
   starlink.Double = -DBL_MAX;

!NO!SUBS!
