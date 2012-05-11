#!/usr/local/bin/perl
#
# This is an example program from the legacy TriD/OpenGL/examples
# directory moved here for reference.  It only uses OpenGL features
# so can probably be deprecated in favor of the Perl OpenGL
# examples eventually.
#
#     clip
#
# This program demonstrates arbitrary clipping planes.
# Based on the "clip.c" program in the 
# OpenGL Programming Guide, Chapter 3, page 108, Listing 3-5.
# However this program clips the front part of a rotating cube
# with flat shaded faces instead of a wire frame sphere.
#
# the C synopsis of glClipPlane is
#
#   void glClipPlane(GLenum plane,const GLdouble *equation ) 
#
# For PERL the routine glpClipPlane was added, and the synopsis is:
#
#   void glpClipPlane(GLenum plane,GLdouble a,GLdouble b,GLdouble c,GLdouble d)
#
# and the 4 double vector (equasion) is packaged by the XSUB.
# Or you can still use glClipPlane but then you have to pack() the structure
# 
# note:  the statement f(@a) is equivalent to f($a[0],$a[1], ... $a[n])
#        i.e. elements of a list are put on the call stack
# 

use PDL::Graphics::OpenGL::Perl::OpenGL;
use OpenGL qw(:all);

sub tacky_cube {
    local($s) = @_;
    local(@x,@y,@z,@f);
    local($i,$j,$k);
    local(@r,@g,@b);
    $s = $s/2.0;
    @x=(-$s,-$s,-$s,-$s,$s,$s,$s,$s);
    @y=(-$s,-$s,$s,$s,-$s,-$s,$s,$s);
    @z=(-$s,$s,$s,-$s,-$s,$s,$s,-$s);
    @f=( 
        0, 1, 2, 3,
        3, 2, 6, 7,
        7, 6, 5, 4,
        4, 5, 1, 0,
        5, 6, 2, 1,
        7, 4, 0, 3,
        );
    @r=(1.0, 0,   0,   1.0, 1.0, 0);
    @g=(0,   1.0, 0,   1.0, 0,   1.0);
    @b=(0,   0,   1.0, 0,   1.0, 1.0);
    for($i=0;$i<6;$i++){
	glColor3f($r[$i],$g[$i],$b[$i]);
        glBegin(GL_POLYGON);
        for($j=0;$j<4;$j++){
                $k=$f[$i*4+$j];
                glVertex3d($x[$k],$y[$k],$z[$k]);
        }
        glEnd();
    }

}

sub add_clip_plane {
    # give the plane a slight tilt-away to prove we're not just
    # clipping against the view volume
    @eqn = (0.0, -0.3, -1.0, 1.2);
    OpenGL::glpClipPlane(GL_CLIP_PLANE0, @eqn);
    glEnable(GL_CLIP_PLANE0);
}

sub display{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glPushMatrix();
    glRotatef($spin, 0.0, 1.0, 0.0);
    tacky_cube(3.0);
    glPopMatrix();
    glFlush();
    glXSwapBuffers;
}
 

sub myReshape {
    # glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(60.0, 1.0 , 1.0, 20.0); 
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity ();
}

OpenGL::glpOpenWindow(width => 400, height => 400,
	      attributes => [GLX_RGBA,GLX_DOUBLEBUFFER]);
glClearColor(0,0,0,1);
glShadeModel (GL_FLAT);
myReshape();
glDisable(GL_CULL_FACE);
glEnable(GL_DEPTH_TEST);

glLoadIdentity ();
glTranslatef (0.0, 0.0, -5.0);
add_clip_plane;

# test glGetClipPlane()
($a,$b,$c,$d)=OpenGL::glpGetClipPlane(GL_CLIP_PLANE0);
print "Clipping plane (a,b,c,d) = ($a,$b,$c,$d)\n";

$spin=0;
while(1) {$spin += 1.0; display;}
