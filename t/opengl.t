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

use Test;

BEGIN { 
  use PDL::Config;
  if( $PDL::Config{OPENGL_LIBS} && $PDL::Config{WITH_3D} 
      # only if GL modules have actually been built
      && $PDL::Config{GL_BUILD} && hasDISPLAY()) {
	 plan tests => 3; 
	 eval 'use PDL::Graphics::OpenGL';
	 ok($@, ''); 
  }else{
	 plan tests => 1; 
         print hasDISPLAY() ? "ok 1 # Skipped: OpenGL support not compiled\n"
	   : "ok 1 # Skipped: DISPLAY environment variable not set\n";
	 exit;
  }
}

#
# Try opening 2 GL windows
#
my $numwins = 2;
my @windows;
my $opt;
$opt->{width} = 90;
$opt->{height} = 90;

foreach(0..$numwins-1){
  $opt->{x} = ($numwins % 10) *100;
  $opt->{y} = int($numwins / 10) *100;
  my $win=new PDL::Graphics::OpenGL::OO($opt);
  ok(ref($win), 'PDL::Graphics::OpenGL::OO');
  push @windows, $win;
}
exit;
#
# More test code not currently used.
# 

my $angle=0;
my $i=0;
while($i++<100){
  $angle++;
  foreach my $win (@windows){
	 $win->glXMakeCurrent() || die "glXMakeCurrent failed\n";
    if(PDL::Graphics::OpenGL::XPending($win->{Display})>0){
		my @ev = PDL::Graphics::OpenGL::glpXNextEvent($win->{Display});
    
		if($ev[0] ==  PDL::Graphics::OpenGL::ConfigureNotify){
		
		  glFlush();
		  glViewport(0, 0, $opt->{width}, $opt->{height});
		  glMatrixMode(GL_PROJECTION);
		  glLoadIdentity();
		  glOrtho(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);

		}	 
	 }
#	 my $out = Dumper($win);
#	 print "$out\n";

	 glShadeModel(GL_FLAT);


    glClearColor(0., 0.5, 0., 1.0); 

	 glClear(GL_COLOR_BUFFER_BIT);
	 

	 glPushMatrix();

	 glRotatef($angle, 0, 0, 1);

	 glBegin(GL_TRIANGLES);

	 # draw pink triangle 

	 glColor3f(1.0, 0.3, 0.5);
	 glVertex2f(0, 0.8);
	 glVertex2f(-0.8, -0.7);
	 glVertex2f(0.7, 0.8);
	 glEnd();

    glPopMatrix();


    $win->glXSwapBuffers();
  }
}
