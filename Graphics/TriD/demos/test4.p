use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Image;
use PDL::IO::Pic;

$s = 10;
$k = zeroes($s,$s);

$x = $k->xvals();
random($k->inplace); $x += $k;

$y = $k->yvals();
random($k->inplace); $y += $k;

random($k->inplace);
$z = $k;

$x /= $s; $y /= $s; $z /= $s;

$pa = PDL::Graphics::TriD::Lattice->new([$x,$y,$z]);
$pb = PDL::Graphics::TriD::Points->new([$x,$y,$z+1]);

$win = PDL::Graphics::TriD::get_current_window();
$win->clear_objects();
$win->add_object($pa);
$win->add_object($pb);

$nx = zeroes(3,20);
$nc = zeroes(3,20);
random($nx->inplace);
random($nc->inplace);

use OpenGL qw(:all);
glShadeModel (&GL_SMOOTH);

$lb = $win->glpRasterFont("5x8",0,255);
$win->add_object(TOBJ->new);
$win->twiddle();

package TOBJ;
our @ISA; BEGIN { @ISA = qw/PDL::Graphics::TriD::Object/ }
use PDL::Graphics::OpenGLQ;

sub new {
	bless {},$_[0];
}

sub togl {
	OpenGL::glDisable(&OpenGL::GL_LIGHTING);
	PDL::gl_line_strip_col($::nx,$::nc);
	OpenGL::glColor3f(1,0,1);
	gl_texts(PDL->pdl(0,0,0.5), OpenGL::GLUT::done_glutInit(),
	  $::lb, ["HELLO HELLO HELLO GLWORLD!!!"]);
	OpenGL::glEnable(&OpenGL::GL_LIGHTING);
}
