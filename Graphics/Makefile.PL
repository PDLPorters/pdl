use ExtUtils::MakeMaker;
PDL::Core::Dev->import(); # for trylink

@subdirs = ("PGPLOT", "LUT", "IIS", "Karma", "PLplot", "Limits");

# we try and build unless WITH_3D == 0
$t = $PDL::Config{WITH_3D};
if ( defined($t) and $t == 0 ) {
   print "\n   WITH_3D: Not building TriD or OpenGL. Turn on WITH_3D if this is incorrect.\n\n";
} else {
# Do we have the xdpyinfo command in PATH?
   use File::Find;
   my $have_xdpyinfo_command = 0;
   print "\n   WITH_3D: Looking for xdpyinfo to check GLX ";

   find( { follow_fast => 1,
           follow_skip => 2,
           wanted => sub {
              -f && /^xdpyinfo(.exe)?$/i
              && ++$have_xdpyinfo_command && print ".";
              $File::Find::prune = 1 if tr!/\\!! ;
              #print "$File::Find::name\n";
           },
         }, (split ':', $ENV{PATH})
       );

            # If yes, run it to check for working GLX extension
            my $have_glx_extension = 0;
            if ($have_xdpyinfo_command) {
            print "\n   WITH_3D: Found xdpyinfo, now checking for GLX extension\n";
            $have_glx_extension = grep /^\s*GLX\s*$/, qx{xdpyinfo};
            if ($have_glx_extension) {
               print "   WITH_3D: Found GLX ($have_glx_extension match)\n";
            }
         }

         # If no GLX we won't choose to build TriD or OpenGL
         unless ($have_glx_extension or $t) {
            print "\n   WITH_3D: No GLX extension found for this X server, won't build TriD or OpenGL\n".
            "   WITH_3D: Turn on WITH_3D if this is incorrect.\n\n";
            $PDL::Config{WITH_3D} = 0;
         } else {
            # Working GLX so continue with automated check for libraries and includes
            print  "\n   WITH_3D: Trying OpenGL initial OPENGL_LIB configuration ->$PDL::Config{OPENGL_LIBS}<-\n\n";

            $PDL::Config{OPENGL_LIBS} = guessogllibs($PDL::Config{OPENGL_LIBS});

            if ( defined $PDL::Config{OPENGL_LIBS} ) {
               unshift @subdirs,"TriD";
               if ( $PDL::Config{OPENGL_LIBS} =~ /Mesa/ ) {
                  $PDL::Config{OPENGL_DEFINE} .= ' -DGL_GLEXT_LEGACY';
               }
               $PDL::Config{WITH_3D} = 1;

               # hack for cygwin
               #
               # note: the current OpenGL implementation requires X windows in its
               # implementation, maybe we could eliminate the need for these hacks
               # by adding explicit X11 include locations for all cases
               #
               if ($^O eq 'cygwin' and not defined $PDL::Config{OPENGL_INC}) {
                  $PDL::Config{OPENGL_INC} = '';
                  if ( -d '/usr/X11R6/include') {
                    $PDL::Config{OPENGL_INC} = $PDL::Config{OPENGL_INC} . '-I/usr/X11R6/include ';
                  }
                  if ( -d '/usr/X11R6/include/GL') {
                    $PDL::Config{OPENGL_INC} = $PDL::Config{OPENGL_INC} . '-I/usr/X11R6/include/GL ';
                  }
               }

               ### if ($^O eq 'cygwin' and not defined $PDL::Config{OPENGL_DEFINE}) {
               ###    $PDL::Config{OPENGL_DEFINE} =
               ###    '-DGL_MESA_trace ' .
               ###    '-DGLX_ARB_render_texture ' .
               ###    '-DGLX_GLEXT_LEGACY ' .
               ###    '-DGLX_MESA_agp_offset ' .
               ###    '-DGLX_NV_vertex_array_range';
               ### }

               # hack for OS-X
               if ($^O eq "darwin" and not defined $PDL::Config{OPENGL_INC}) {
                  $PDL::Config{OPENGL_INC} = "-I/usr/X11R6/include";
               }

               # hack for FreeBSD 
               #   chm: Need to add include path check that #include <GL/gl.h> works
               #
               if ($^O eq "freebsd" and not defined $PDL::Config{OPENGL_INC}) {
                  $PDL::Config{OPENGL_INC} = "-I/usr/X11R6/include";
               }

               print "\n   WITH_3D: Success using $PDL::Config{OPENGL_LIBS} $PDL::Config{OPENGL_DEFINE}\n".
               "\n   WITH_3D: Set WITH_3D => 1 and build OpenGL and TriD\n\n";
            } else {
               warn "\n   WITH_3D: COULD NOT link OpenGL \n".
               "    WITH_3D: Not building OpenGL and TriD options!  Turn on WITH_3D if this is incorrect.\n\n";
               $PDL::Config{WITH_3D}=0;
            }
         }
      }

#WriteMakefile(
#     'NAME' => 'PDL',
#     VERSION_FROM => '../Basic/Core/Version.pm',
#     DIR => [@subdirs]
#);
      my @pm_names = qw (Graphics2D.pm State.pm);
      my %pm = map { $h = '$(INST_LIBDIR)/';
         $h .= 'PDL/' if $_ !~ /PDL.pm$/;
         $h .= 'Graphics/' if $_ =~ /State.pm$/;
         ( $_, $h . $_ );
      } ( @pm_names);
      my %man3pods = map { $h = '$(INST_MAN3DIR)/';
         $h .= 'PDL::' if $_ !~ /PDL.pm$/;
         ( $_, $h . substr($_,0,length($_)-3) . '.$(MAN3EXT)' ); 
      } @pm_names;


      WriteMakefile(
         'NAME' => 'PDL',
         VERSION_FROM => '../Basic/Core/Version.pm',
         'PM'       => \%pm,
         'MAN3PODS' => \%man3pods,
         DIR => [@subdirs],
#	      LIBS => ['-lm'],  # corrected from LIB=>..., then commented out
      );

      sub guessogllibs{
         my $libs=shift;
         use Config;
         my $have_GL;
         my $mmpre = {MakeMaker => 1}; # preprocess by MakeMaker

         if(defined $libs){
            if (trylink $libs,
               '', 'char glBegin(); glBegin();',
               libs($libs), $mmpre) {
               return $libs;
            }else{
               undef $libs;
               warn "     User specified OpenGL configuration $libs did not work"
               ."\n     trying to guess...\n\n";
            }
         }
         if (trylink( 'libGL',
               '', 'char glBegin(); glBegin();',
               libs('-lGL'), $mmpre)) {
            $libs = libs('-lGLU -lGL');
         } elsif (trylink ('libMesaGL',
               '', 'char glBegin(); glBegin();',
               libs('-lMesaGL'), $mmpre)) {
            $libs = libs('-lMesaGLU -lMesaGL');
         } elsif (trylink ('libMesaGL with pthread',
               '', 'char glBegin(); glBegin();',
               libs('-lMesaGL -lpthread'), $mmpre)) {
            $libs = libs('-lMesaGLU -lMesaGL -lpthread');
         }

         # Add -lm to libs (Seems to be needed on Linux and Solaris, and shouldn't hurt for others)
         # [only do if we've actually found anything]
         $libs .= ' -lm' if defined $libs and $libs !~ /\-lm\b/;
         return $libs;
      }

      sub libs ($) {
         require ExtUtils::MakeMaker;
         require ExtUtils::Liblist;
         my ($libs) = @_;
         my $lpath = '-L/usr/X11R6/lib64 -L/usr/X11R6/lib -L/usr/lib64/mesa -L/usr/lib/mesa';
         my $extra = '-lXext -lX11 -lm';
         return ${[ExtUtils::Liblist->ext("$lpath $libs $extra")]}[0];
      }
print "exiting graphics...\n";
