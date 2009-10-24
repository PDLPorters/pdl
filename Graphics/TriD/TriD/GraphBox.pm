
package PDL::Graphics::TriD::GraphBox;
@ISA=qw/PDL::Graphics::TriD::Object/;

BEGIN {
   use PDL::Config;
   if ( $PDL::Config{USE_POGL} ) {
      eval "use OpenGL $PDL::Config{POGL_VERSION} qw(:all)";
      eval 'use PDL::Graphics::OpenGL::Perl::OpenGL';
   } else {
      eval 'use PDL::Graphics::OpenGL';
   }
}

use PDL::Lite;

sub new {
}
