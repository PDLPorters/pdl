
package PDL::Graphics::TriD::GraphBox;
@ISA=qw/PDL::Graphics::TriD::Object/;

BEGIN {
   if ( $PDL::Config{USE_POGL} ) {
      eval 'use OpenGL 0.58_007 qw(:all)';
      eval 'use PDL::Graphics::OpenGL::Perl::OpenGL';
   } else {
      eval 'use PDL::Graphics::OpenGL';
   }
}

use PDL::Lite;

sub new {
}
