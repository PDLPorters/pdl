use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

# do we build the OpenGL/OpenGLQ stuff
#  (rather than have these 2 modules do it themselves)
#
# try and find out whether we should build the OpenGL/Mesa stuff
# - first check is to dump win32 systems
#
$gl_build = 0; $gl_msg = ""; $gl_dir = "";
if ($^O =~ /win32/i) {
    $gl_msg = unsupported('PDL::Graphics::TriD::OpenGL(Q)','win32');
} else {
    #
    # default locations:
    @check_dirs = ('/lib/','/usr/lib/','/usr/local/lib/'); 
    
    @openGL_dirs = grep s/^-L//g, split(' ',$PDL_CONFIG{OPENGL_LIBS});

    # append locations from perldl.conf, if specified:
    @check_dirs = ( @check_dirs, @openGL_dirs ) 
        if( defined $PDL_CONFIG{OPENGL_LIBS} ); 

    
    foreach $dir (@check_dirs) {
	last if $gl_build;
	$h = "${dir}/lib";
	# if statement a bit OTT?
	if ( -e "${h}GL.a" or -e "${h}GL.so" or -e "${h}MesaGL.a" or -e "${h}MesaGL.so" ) {
	    $gl_dir   = $dir;
	    $gl_build = 1;
	}
    }
    $gl_msg = "skipping build of TriD::OpenGL(Q) modules - unable to find libs"
	unless $gl_build;
} # if: OS == win32

@subdirs = qw( Rout VRML );
if ( $gl_build ) { @subdirs = ( @subdirs, qw( OpenGL OpenGLQ ) ); }
else             { print "   $gl_msg\n\n" unless $gl_build; }
$PDL_CONFIG{GL_BUILD} = $gl_build; # record for Config.pm

WriteMakefile
    (
     NAME => "PDL::Graphics::TriD",
     'VERSION_FROM' => '../../Basic/Core/Version.pm',
     DIR => [@subdirs],
     );
