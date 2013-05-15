
use Config;
$PDL_Index_type = $Config{'ivtype'};
warn "Using new 64bit index support\n" if $Config{'ivsize'}==8;

use lib ".";
require 'Types.pm';
*T = *PDL::Types::typehash; # Alias

# This file defines things that are in common between
# pdl.h and pdlsimple.h

$enum = '';
for (sort { $T{$a}{'numval'}<=>$T{$b}{'numval'} }  keys %T) {
 $enum .= $T{$_}{'sym'}.", ";
 $typedefs .= "typedef $T{$_}{'realctype'}              $T{$_}{'ctype'};\n";
}
chop $enum;
chop $enum;

$PDL_DATATYPES = <<"EOD";

/*****************************************************************************/
/*** This section of .h file generated automatically - don't edit manually ***/

/* Data types/sizes [must be in order of complexity] */

enum pdl_datatypes { $enum };

/* Define the pdl data types */

$typedefs

/* typedef $PDL_Index_type    PDL_Index; */


/*****************************************************************************/

EOD

$PDL_DATATYPES .= "\n".PDL::Types::typesynonyms()."\n";

1; # OK
