#!/usr/bin/perl -w
use FileHandle;
use strict;
use warnings;
use Carp;

$SIG{__DIE__} = sub {print Carp::longmess(@_); 
							die;};


STDOUT->autoflush(1);
STDERR->autoflush(1);

use PDL::Graphics::OpenGL;

my $numwins = shift;
$numwins = 1 unless(defined $numwins);
my @windows;
my $opt;
$opt->{width} = 90;
$opt->{height} = 90;

#PDL::Graphics::OpenGL::glpSetDebug(1);

foreach(0..$numwins-1){
  $opt->{x} = ($numwins % 10) *100;
  $opt->{y} = int($numwins / 10) *100;

  my $win=new PDL::Graphics::OpenGL::OO($opt);
  push @windows, $win;
}

my $angle=0;

use Data::Dumper;
my $out = Dumper($windows[0]);
print "$out\n";



while(1){
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
