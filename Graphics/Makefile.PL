use ExtUtils::MakeMaker;
PDL::Core::Dev->import(); # for trylink

@subdirs = ("PGPLOT", "LUT", "IIS", "Karma", "PLplot");

# we try and build unless WITH_3d == 0
$t = $PDL_CONFIG{WITH_3D};
if( defined($t) and $t == 0 ) {
	print "\n   Not building TriD or OpenGL. Turn on WITH_3D if this is incorrect.\n\n";
} else {
  print  "\n   Trying OpenGL configuration $PDL_CONFIG{OPENGL_LIBS}\n\n";
  
  $PDL_CONFIG{OPENGL_LIBS} = guessogllibs($PDL_CONFIG{OPENGL_LIBS});

  if(defined $PDL_CONFIG{OPENGL_LIBS}){
	 unshift @subdirs,"TriD";
    if($PDL_CONFIG{OPENGL_LIBS} =~ /Mesa/){
		$PDL_CONFIG{OPENGL_DEFINE} .= ' -DGL_GLEXT_LEGACY';
	 }
	 $PDL_CONFIG{WITH_3D}=1;
    print "\n    Success using $PDL_CONFIG{OPENGL_LIBS} $PDL_CONFIG{OPENGL_DEFINE}\n\n";
  }else{
	 warn "\n     COULD NOT link OpenGL \n".
		"    Not building OpenGL and TriD options!\n\n";
	 $PDL_CONFIG{WITH_3D}=0;
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
	 $libs = '-lGLU -lGL';
  } elsif (trylink ('libMesaGL',
			  '', 'char glBegin(); glBegin();',
			  libs('-lMesaGL'), $mmpre)) {
	 $libs = '-lMesaGLU -lMesaGL';
  } elsif (trylink ('libMesaGL with pthread',
			  '', 'char glBegin(); glBegin();',
			  libs('-lMesaGL -lpthread'), $mmpre)) {
	 $libs = '-lMesaGLU -lMesaGL -lpthread';
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
  my $lpath = '-L/usr/X11R6/lib -L/usr/lib/mesa';
  my $extra = '-lXext -lX11 -lm';
  return "$lpath $libs $extra";
}
