

package PDL::Examples;

$VERSION = "1.00";

@EXPORT_OK = qw( fibonacci cc8compt );

use PDL::Core;
use DynaLoader;
@ISA    = qw( PDL::Exporter DynaLoader ); 

bootstrap PDL::Examples;


;# Exit with OK status

1;

