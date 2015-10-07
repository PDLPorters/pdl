# -*-perl-*-
BEGIN{
	  # Set perl to not try to resolve all symbols at startup
	  #   The default behavior causes some problems because 
	  #    opengl.pd builds an interface for all functions
	  #    defined in gl.h and glu.h even though they might not
	  #    actually be in the opengl libraries.
	  $ENV{'PERL_DL_NONLAZY'}=0;
}

# use PDL::Graphics::OpenGL;

sub hasDISPLAY {
  return defined $ENV{DISPLAY} && $ENV{DISPLAY} !~ /^\s*$/;
}

use Test::More;

BEGIN { 
   use PDL::Config;
   if ( $PDL::Config{WITH_3D} ) {  # check if compiled
      if ( $PDL::Config{USE_POGL} ) {  # check if using Perl OpenGL
         plan tests => 2;
         use_ok("OpenGL $PDL::Config{POGL_VERSION}", qw(:all));
         use_ok('PDL::Graphics::OpenGL::Perl::OpenGL');
      } else {
         plan skip_all => 'Non-POGL TriD graphics not supported';
      }
   } else {
      plan skip_all => 'TriD graphics not compiled';
   }
}

#
# TODO: add runtime tests
#
