
use Config;
$PDL_Indx_type = $Config{'ivtype'};
warn "Using new 64bit index support\n" if $Config{'ivsize'}==8;

use lib ".";
require 'Types.pm';
*T = *PDL::Types::typehash; # Alias

# This file defines things that are in common between
# pdl.h and pdlsimple.h

$enum = '';
$typedefs = '';
$union = "\n";
for (sort { $T{$a}{'numval'}<=>$T{$b}{'numval'} }  keys %T) {
 $enum .= $T{$_}{'sym'}.", ";
 $typedefs .= "typedef $T{$_}{'realctype'}              $T{$_}{'ctype'};\n";
 $union .= "    $T{$_}{'ctype'} $T{$_}{'ppsym'};\n"
}
chop $enum;
chop $enum;
chop $typedefs;

$PDL_DATATYPES = <<"EOD";

/*****************************************************************************/
/*** This section of .h file generated automatically - don't edit manually ***/

/* Data types/sizes [must be in order of complexity] */

enum pdl_datatypes { $enum }; 

/* Define the pdl data types */

$typedefs

/* typedef $PDL_Indx_type    PDL_Indx; */

union PDL_Generic { $union };

typedef union PDL_Generic PDL_Generic;

/*****************************************************************************/

EOD

$PDL_DATATYPES .= "\n".PDL::Types::typesynonyms()."\n";

$PDL_DATATYPES; # OK
