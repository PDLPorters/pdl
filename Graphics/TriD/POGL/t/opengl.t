BEGIN{
	  # Set perl to not try to resolve all symbols at startup
	  #   The default behavior causes some problems because
	  #    opengl.pd builds an interface for all functions
	  #    defined in gl.h and glu.h even though they might not
	  #    actually be in the opengl libraries.
	  $ENV{'PERL_DL_NONLAZY'}=0;
}

use strict;
use warnings;
use Test::More;
use_ok("OpenGL", qw(:all));
use_ok('PDL::Graphics::OpenGL::Perl::OpenGL');

# TODO: add runtime tests

done_testing;
