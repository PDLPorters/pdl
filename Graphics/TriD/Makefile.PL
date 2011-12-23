
use strict;

use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

# do we build the OpenGL/OpenGLQ stuff
#  (rather than have these 2 modules do it themselves)
#
# try and find out whether we should build the OpenGL/Mesa stuff
# - first check is to dump win32 systems
#
my $gl_build = 0;
my $gl_msg = "";
my $gl_dir = "";

my @subdirs = qw( Rout VRML );

if ( $PDL::Config{USE_POGL} ) { # build with OpenGL for GL bindings and compile/link info
   print "Graphics/TriD/Makefile.PL: using POGL for OpenGL bindings and compile options\n";
   @subdirs = ( @subdirs, qw( POGL OpenGLQ ) );
   $gl_build = 1;
} else {
   print "Graphics/TriD/Makefile.PL: skipping build of TriD::OpenGL(Q) modules - no POGL\n";
}

$PDL::Config{GL_BUILD} = $gl_build; # record for Config.pm

WriteMakefile (
     'NAME'         => "PDL::Graphics::TriD",
     'VERSION_FROM' => '../../Basic/Core/Version.pm',
     'DIR'          => [ @subdirs ],
);
