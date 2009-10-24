
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

if ( $PDL::Config{USE_POGL} ) {
   print "Graphics/TriD/Makefile.PL: using POGL for OpenGL bindings and compile options\n";
} else {
   if ($^O =~ /win32/i) {
      $gl_msg = unsupported('PDL::Graphics::TriD::OpenGL(Q)','win32');
   } else {
      #
      # default locations:
      my @check_dirs = qw( /lib/ /usr/lib/ /usr/local/lib/ /usr/X11R6/lib/ /usr/local/X11R6/lib/ /usr/X11/lib/ /usr/local/X11/lib/ );

      # append locations from perldl.conf, if specified:
      @check_dirs =
      (
         @check_dirs,
         grep s/^-L//g, split(' ',$PDL::Config{OPENGL_LIBS})
      ) if defined $PDL::Config{OPENGL_LIBS};

      my $lib = ""; # Gets the first lib found...
      my @patterns = qw( GL.a GL.so GL.dll.a MesaGL.a MesaGL.so Mesa.dll.a );

      my %Ldirs = ();
      foreach my $dir (@check_dirs) {
         my $h = "${dir}/lib";

         map { if (-e "${h}$_") { $lib = "${h}$_" unless $gl_build; 
               $Ldirs{$dir}++;} } @patterns;

         if( !$gl_build && $lib ) {
            $gl_dir   = $dir;
            $gl_build = 1;

            ## Check for Mesa GLAPI hassles --CED 21-Nov-2003
            unless( defined $PDL::Config{OPENGL_DEFINE} ) {
               $PDL::Config{OPENGL_DEFINE} = '-DGLAPI=extern'
               if `strings $lib | grep mesa | head -1`;
            }
         }
      }
      $PDL::Config{OPENGL_LIBS} = join(" ",(map { "-L$_" } keys %Ldirs),$PDL::Config{OPENGL_LIBS});

      $gl_msg = "skipping build of TriD::OpenGL(Q) modules - unable to find libs"
      unless $gl_build;
   } # if: OS == win32
}

my @subdirs = qw( Rout VRML );
if ( $PDL::Config{USE_POGL} ) {
   # build with OpenGL for GL bindings and compile/link info
   @subdirs = ( @subdirs, qw( POGL OpenGLQ ) );
} elsif ( $gl_build ) {
   # legacy PP configure and build
   @subdirs = ( @subdirs, qw( OpenGL OpenGLQ ) );
} else {
   print "   $gl_msg\n\n" unless $gl_build;
}

$PDL::Config{GL_BUILD} = $gl_build; # record for Config.pm

WriteMakefile
    (
     NAME => "PDL::Graphics::TriD",
     'VERSION_FROM' => '../../Basic/Core/Version.pm',
     DIR => [@subdirs],
     );
