#
#
# ToDo:
#  - multiple windows - requires editing generate.pl in OpenGL/
#  - clean up
#

package PDL::Graphics::TriD::GL;
use PDL::Graphics::OpenGL;
use PDL::Graphics::OpenGLQ;

$PDL::Graphics::TriD::create_window_sub = sub {
	return new PDL::Graphics::TriD::GL::Window;
};

sub PDL::Graphics::TriD::Material::togl{
  my $this = shift;
  my $shin = pack "f*",$this->{Shine};
  glMaterialfv(&GL_FRONT_AND_BACK,&GL_SHININESS,$shin);
  my $spec = pack "f*",@{$this->{Specular}};
  glMaterialfv(&GL_FRONT_AND_BACK,&GL_SPECULAR,$spec);
  my $amb = pack "f*",@{$this->{Ambient}};
  glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,$amb);
  my $diff = pack "f*",@{$this->{Diffuse}};
  glMaterialfv(&GL_FRONT_AND_BACK,&GL_DIFFUSE,$diff);
}

$PDL::Graphics::TriD::any_cannots = 0;

sub PDL::Graphics::TriD::Object::cannot_mklist {
	return 0;
}

sub PDL::Graphics::TriD::Object::gl_update_list {
	my($this) = @_;
	if($this->{List}) {
		glDeleteLists($this->{List},1);
	}
	my $lno = glGenLists(1);
	$this->{List} = $lno;
	print "GENLIST $lno\n" if $PDL::Graphics::TriD::verbose;
	glNewList($lno,GL_COMPILE);
	if ($PDL::Graphics::TriD::any_cannots) {
	  for(@{$this->{Objects}}) {
	        if(!$_->cannot_mklist()) {
			$_->togl();
		}
	  }
	} else { for (@{$this->{Objects}}) {$_->togl()} }
	print "EGENLIST $lno\n" if $PDL::Graphics::TriD::verbose;
#	pdltotrianglemesh($pdl, 0, 1, 0, ($pdl->{Dims}[1]-1)*$mult);
	glEndList();
	print "VALID1 $this\n" if $PDL::Graphics::TriD::verbose;
	$this->{ValidList} = 1;
}

sub PDL::Graphics::TriD::Object::gl_call_list {
	my($this) = @_;
	print "CALLIST $this->{List}!\n" if $PDL::Graphics::TriD::verbose;
	print "CHECKVALID $this\n" if $PDL::Graphics::TriD::verbose;
	if(!$this->{ValidList}) {
		$this->gl_update_list();
	}
	glCallList($this->{List});
	if ($PDL::Graphics::TriD::any_cannots) {
	  for(@{$this->{Objects}}) {
		if($_->cannot_mklist()) {
			$_->togl();
		}
	  }
        }
}

sub PDL::Graphics::TriD::Object::delete_displist {
	my($this) = @_;
	if($this->{List}) {
		glDeleteLists($this->{List},1);
		undef $this->{List};
	}
}

sub PDL::Graphics::TriD::Object::togl {
	my($this) = @_;
	for(@{$this->{Objects}}) { $_->togl() }
}


# XXX Aspect handling.
sub PDL::Graphics::TriD::ViewPort::togl_vp {
	my($this,$win,$rec) = @_;
	my $aspect = $win->{Aspect};
	my ($foo,$x0,$y0,$x1,$y1) = @$rec;
	print "VPTOGL\n" if $PDL::Graphics::TriD::verbose;
	my $w = $win->{W}; my $h = $win->{H};
	my $x = $win->{X0}; my $y = $win->{Y0};
	print "VPTO: $w,$h,$x,$y,$x0,$x1,$y0,$y1\n" if $PDL::Graphics::TriD::verbose;
	my @vp = ($w * $x0 + $x,$h * $y0 + $y,$w * ($x1-$x0) + $x,$h * ($y1-$y0) + $y);
	$this->{W} = $vp[2]-$vp[0];
	$this->{H} = $vp[3]-$vp[1];
	$this->{X0} = $vp[0];
	$this->{Y0} = $vp[1];
	print "VPTO2: $vp[0] $vp[1] $vp[2] $vp[3]\n" if $PDL::Graphics::TriD::verbose;
	glViewport(@vp);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0,1,0,1,-1,1);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	$this->gl_call_list();
	if($this->{ViewPorts}) {
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
#		print "DOVP\n" if $PDL::Graphics::TriD::verbose;
		for(@{$this->{ViewPorts}}) {
			$_->[0]->togl_vp($this,$_);
		}
	}
}

sub PDL::Graphics::TriD::BoundingBox::togl { my($this) = @_;
	$this = $this->{Box};
	glDisable(&GL_LIGHTING);
	glColor3d(1,1,1);
	glBegin(&GL_LINE_STRIP);
	for([0,4,2],[0,1,2],[0,1,5],[0,4,5],[0,4,2],[3,4,2],
		[3,1,2],[3,1,5],[3,4,5],[3,4,2]) {
		glVertex3d(@{$this}[@$_]);
	}
	glEnd();
	glBegin(&GL_LINES);
	for([0,1,2],[3,1,2],[0,1,5],[3,1,5],[0,4,5],[3,4,5]) {
		glVertex3d(@{$this}[@$_]);
	}
	glEnd();
	glEnable(&GL_LIGHTING);
}

sub PDL::Graphics::TriD::Scale::togl {my ($this) = @_;
	print "Scale ",(join ',',@{$this->{Args}}),"\n";
	PDL::Graphics::OpenGL::glScalef(@{$this->{Args}});
}


sub PDL::Graphics::TriD::Translation::togl {my($this) = @_;
	print "Transl ",(join ',',@{$this->{Args}}),"\n";
	PDL::Graphics::OpenGL::glTranslatef(@{$this->{Args}});
}


sub PDL::Graphics::TriD::Transformation::togl {
	my($this) = @_;
	PDL::Graphics::OpenGL::glPushMatrix();
	for (@{$this->{Transforms}}) {$_->togl();}
	$this->SUPER::togl();
	PDL::Graphics::OpenGL::glPopMatrix();
}

sub PDL::Graphics::TriD::Graph::togl {
	my($this) = @_;
#	print "TOGL Axis\n";
	for(keys %{$this->{Axis}}) {
		if($_ eq "Default") {next}
		$this->{Axis}{$_}->togl_axis($this);
	}
#	print "TOGL DATA\n";
	for(keys %{$this->{Data}}) {
#		print "TOGL   $_, $this->{Data}{$_}\n";
		$this->{Data}{$_}->togl_graph($this,$this->get_points($_));
	}
}


sub PDL::Graphics::TriD::EuclidAxes::togl_axis {
	my($this,$graph) = @_;
#	print "TOGLAX\n";
	my $fontbase = $PDL::Graphics::TriD::GL::fontbase;
#	print "TOGL EUCLID\n";
	glDisable(&GL_LIGHTING);
	glColor3d(1,1,1);
	glBegin(&GL_LINES);
	my $dim;
	for $dim (0..2) {
		glVertex3f(0,0,0);
		glVertex3f(map {$_==$dim} 0..2);
	}
	glEnd();
	for $dim (0..2) {
		my @coords = (0,0,0);
		my @coords0 = (0,0,0);
		for(0..2) {if($dim != $_) {
				$coords[$_] -= 0.1;
			}
		}
		my $s = $this->{Scale}[$dim];
		my $ndiv = 3;
		my $radd = 1.0/$ndiv;
		my $nadd = ($s->[1]-$s->[0])/$ndiv;
		my $nc = $s->[0];
		for(0..$ndiv) {
			glRasterPos3f(@coords);
			PDL::Graphics::OpenGL::glpPrintString($fontbase,
				sprintf("%.3f",$nc));
			glBegin(&GL_LINES);
			glVertex3f(@coords0);
			glVertex3f(@coords);
			glEnd();
#			print "PUT: $nc\n";
			$coords[$dim] += $radd;
			$coords0[$dim] += $radd;
			$nc += $nadd;
		}
		$coords0[$dim] = 1.1;
		glRasterPos3f(@coords0);
		PDL::Graphics::OpenGL::glpPrintString($fontbase,
			$this->{Names}[$dim]);
	}
	glEnable(&GL_LIGHTING);
}

use POSIX qw/acos/;
sub PDL::Graphics::TriD::Quaternion::togl {my($this) = @_;
	if(abs($this->[0]) == 1) { return ; }
	if(abs($this->[0]) >= 1) {
		# die "Unnormalized Quaternion!\n";
		$this->normalize_this();
	}
	glRotatef(2*acos($this->[0])/3.14*180, @{$this}[1..3]);
}

##################################
# Graph Objects
#
#

sub PDL::Graphics::TriD::GObject::togl {
	$_[0]->gdraw($_[0]->{Points});
}

# (this,graphs,points)
sub PDL::Graphics::TriD::GObject::togl_graph {
#	print "TOGLGRAPH: $_[0]\n";
	$_[0]->gdraw($_[2]);
}

sub PDL::Graphics::TriD::Points::gdraw {
	my($this,$points) = @_;
#	print "DRAWPOINTS: \n",$points;
	glDisable(&GL_LIGHTING);
	PDL::gl_points($points,$this->{Colors});
	glEnable(&GL_LIGHTING);
}

sub PDL::Graphics::TriD::Lattice::gdraw {
	my($this,$points) = @_;
	glDisable(&GL_LIGHTING);
	PDL::gl_lines($points,$this->{Colors});
	PDL::gl_lines($points->xchg(1,2),$this->{Colors}->xchg(1,2));
	glEnable(&GL_LIGHTING);
}


sub PDL::Graphics::TriD::Lines::gdraw {
	my($this,$points) = @_;
	glDisable(&GL_LIGHTING);
	PDL::gl_lines($points,$this->{Colors});
	glEnable(&GL_LIGHTING);
}

sub PDL::Graphics::TriD::SLattice::gdraw {
	my($this,$points) = @_;
	glPushAttrib(&GL_LIGHTING_BIT | &GL_ENABLE_BIT);
	glDisable(&GL_LIGHTING);
# By-vertex doesn't make sense otherwise.
	glShadeModel (&GL_SMOOTH);
	my @sls1 = (":,0:-2,0:-2",
	            ":,1:-1,0:-2",
		    ":,0:-2,1:-1");
	my @sls2 = (":,1:-1,1:-1",
		    ":,0:-2,1:-1",
	            ":,1:-1,0:-2"
		    );
	PDL::gl_triangles(
		(map {$points->slice($_)} @sls1),
		(map {$this->{Colors}->slice($_)} @sls1)
	);
	PDL::gl_triangles(
		(map {$points->slice($_)} @sls2),
		(map {$this->{Colors}->slice($_)} @sls2)
	);
	if ($this->{Options}{Lines}) {
	  my $black = PDL->pdl(0,0,0)->dummy(1)->dummy(1);
	  PDL::gl_lines($points,$black);
	  PDL::gl_lines($points->xchg(1,2),$black);
	}
	glPopAttrib();
}

sub PDL::Graphics::TriD::SCLattice::gdraw {
	my($this,$points) = @_;
	glPushAttrib(&GL_LIGHTING_BIT | &GL_ENABLE_BIT);
	glDisable(&GL_LIGHTING);
# By-vertex doesn't make sense otherwise.
	glShadeModel (&GL_FLAT);
	my @sls1 = (":,0:-2,0:-2",
	            ":,1:-1,0:-2",
		    ":,0:-2,1:-1");
	my @sls2 = (":,1:-1,1:-1",
		    ":,0:-2,1:-1",
	            ":,1:-1,0:-2"
		    );
	PDL::gl_triangles(
		(map {$points->slice($_)} @sls1),
		(map {$this->{Colors}} @sls1)
	);
	PDL::gl_triangles(
		(map {$points->slice($_)} @sls2),
		(map {$this->{Colors}} @sls2)
	);
	if ($this->{Options}{Lines}) {
	  my $black = PDL->pdl(0,0,0)->dummy(1)->dummy(1);
	  PDL::gl_lines($points,$black);
	  PDL::gl_lines($points->xchg(1,2),$black);
	}
	glPopAttrib();
}

sub PDL::Graphics::TriD::SLattice_S::gdraw {
	my($this,$points) = @_;
	glPushAttrib(&GL_LIGHTING_BIT | &GL_ENABLE_BIT);
# For some reason, we need to set this here as well.
	glLightModeli(&GL_LIGHT_MODEL_TWO_SIDE, &GL_TRUE);
# By-vertex doesn't make sense otherwise.
	glShadeModel (&GL_SMOOTH);
	my @sls1 = (":,0:-2,0:-2",
	            ":,1:-1,0:-2",
		    ":,0:-2,1:-1");
	my @sls2 = (":,1:-1,1:-1",
		    ":,0:-2,1:-1",
	            ":,1:-1,0:-2"
		    );
	if ($this->{Options}{Smooth}) {
	  $this->{Normals} = $this->smoothn($points)
	    unless defined($this->{Normals});
	  my $n = $this->{Normals};
	  my $f = (!$this->{Options}{Material} ?
	  		\&PDL::gl_triangles_wn : \&PDL::gl_triangles_wn_mat);
	  &$f(
			       (map {$points->slice($_)} @sls1),
			       (map {$n->slice($_)} @sls1),
			       (map {$this->{Colors}->slice($_)} @sls1)
			      );
	  &$f(
			       (map {$points->slice($_)} @sls2),
			       (map {$n->slice($_)} @sls2),
			       (map {$this->{Colors}->slice($_)} @sls2)
			      );
	} else {
	  my $f = (!$this->{Options}{Material} ?
	  		\&PDL::gl_triangles_n : \&PDL::gl_triangles_n_mat);
	  &$f(
			      (map {$points->slice($_)} @sls1),
			      (map {$this->{Colors}->slice($_)} @sls1)
			     );
	  &$f(
			      (map {$points->slice($_)} @sls2),
			      (map {$this->{Colors}->slice($_)} @sls2)
			     );
	}
	glDisable(&GL_LIGHTING);
	if ($this->{Options}{Lines}) {
	  my $black = PDL->pdl(0,0,0)->dummy(1)->dummy(1);
	  PDL::gl_lines($points,$black);
	  PDL::gl_lines($points->xchg(1,2),$black);
	}
	glPopAttrib();
}

##################################
# PDL::Graphics::TriD::Image
#
#

sub PDL::Graphics::TriD::Image::togl {
	$_[0]->gdraw();
}

sub PDL::Graphics::TriD::Image::togl_graph {
	&PDL::Graphics::TriD::Image::togl;
}


# The quick method is to use texturing for the good effect.
sub PDL::Graphics::TriD::Image::gdraw {
	my($this,$vert) = @_;
	my ($p,$xd,$yd,$txd,$tyd) = $this->flatten(1); # do binary alignment
	glColor3d(1,1,1);
	glTexImage2D(&GL_TEXTURE_2D,
		0,
		&GL_RGB,
		$txd,
		$tyd,
		0,
		&GL_RGB,
		&GL_FLOAT,
		${$p->get_dataref()}
	);
	 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
	    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
	       glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
	          glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );

	glDisable(&GL_LIGHTING);
	glNormal3d(0,0,1);
	glEnable(&GL_TEXTURE_2D);
	glBegin(&GL_QUADS);
	my @texvert = (
		[0,0],
		[$xd/$txd, 0],
		[$xd/$txd, $yd/$tyd],
		[0, $yd/$tyd]
	);
	if(!defined $vert) {$vert = $this->{Points}}
	for(0..3) {
#	for([0,0],[$xd/$txd,0],[$xd/$txd,$yd/$tyd],[0,$yd/$tyd]) {
		glTexCoord2f(@{$texvert[$_]});
		glVertex3f($vert->slice(":,($_)")->list);
	}
	glEnd();
	glEnable(&GL_LIGHTING);
	glDisable(&GL_TEXTURE_2D);
}

# XXX NOT USED

# The careful method is to plot the image as quadrilaterals inside
# the 0..1,0..1,0 box
sub PDL::Graphics::TriD::Image::togl_old {
	my($this) = @_;
	my($x,$y);
	my @dims = $this->{Im}->dims;
	shift @dims; # remove the '3'
	glDisable(GL_LIGHTING);
	glNormal3d(0,0,1);
	my $xmul = 1.0/$dims[0]; my $ymul = 1.0/$dims[1];
#	print "IMAG2GL, $this->{R}, $this->{G}, $this->{B}\n";
	my @rvals = $this->{R}->list;
	my @gvals = $this->{G}->list;
	my @bvals = $this->{B}->list;
	for $x (0..$dims[0]-1) {
#		print "$x\n";
		for $y (0..$dims[1]-1) {
			glColor3f((shift @rvals),(shift @gvals),(shift @bvals));
			glRectd($x*$xmul,$y*$ymul,
				($x+1)*$xmul,($y+1)*$ymul);
		}
	}
#	print "IMAGDONE\n";
	glEnd();
	glEnable(GL_LIGHTING);
}



sub PDL::Graphics::TriD::EventHandler::new {
	my($type) = @_;
	my $this = bless {"X" => -1, "Y" => -1,Buttons => [],Moved => 0},$type;
	return $this;
}

sub PDL::Graphics::TriD::EventHandler::event {
	my($this,$win,$type,@args) = @_;
	print "EH: $type\n" if $PDL::Graphics::TriD::verbose;
	if($type == &MotionNotify) {
#		print "MOTION\n";
	  my $but = -1;
	 SWITCH: { $but = 0, last SWITCH if ($args[0] & (&Button1Mask));
		   $but = 1, last SWITCH if ($args[0] & (&Button2Mask));
		   $but = 2, last SWITCH if ($args[0] & (&Button3Mask));
		   goto NOBUT;
		 }
	   if($this->{Buttons}[$but]) {
	     $this->{Buttons}[$but]->mouse_moved(
						 $this->{X},$this->{Y},
						 $args[1],$args[2]);
	   }
	   $this->{X} = $args[1]; $this->{Y} = $args[2];
	   $this->{Moved} = 1;
	 NOBUT:
	} elsif($type == &ButtonPress) {
#		print "BUTTONPRESS\n";
		$this->{X} = $args[1]; $this->{Y} = $args[2];
		$this->{Moved} = 0;
	} elsif($type == &ButtonRelease) {
#		print "BUTTONRELEASE\n";
		my $but = $args[0];
		if($but == 2 or !$this->{Moved}) {
			if(defined &PDL::Graphics::TriD::Tk::post_menu) {
				PDL::Graphics::TriD::Tk::post_menu($win,@args[3,4]);
			}
		}
	}
}

sub PDL::Graphics::TriD::EventHandler::set_button {
	my($this,$butno,$act) = @_;
	$this->{Buttons}[$butno] = $act;
}

sub PDL::Graphics::TriD::SimpleController::togl {
	my($this) = @_;
#	print "CONTROL\n";
	$this->{CRotation}->togl();
	glTranslatef(0,0,-$this->{CDistance});
	$this->{WRotation}->togl();
	glTranslatef(map {-$_} @{$this->{WOrigin}});
}

##############################################
#
# A window with mouse control over rotation.
#
# Do not make two of these!
#
package PDL::Graphics::TriD::GL::Window;
use PDL::Graphics::OpenGL;
@ISA = qw/PDL::Graphics::TriD::Window/;
sub i_keep_list {return 1} # For Object, so I will be notified of changes.
use strict;
use FileHandle;
BEGIN {
	$PDL::Graphics::TriD::GL::xsize = 300;
	$PDL::Graphics::TriD::GL::ysize = 300;
}

sub new {my($type) = @_;
	my($w,$h) = ($PDL::Graphics::TriD::GL::xsize, $PDL::Graphics::TriD::GL::ysize);
	my $x = 0;
	my @db = &GLX_DOUBLEBUFFER;
#	my @db = ();
	if($PDL::Graphics::TriD::offline) {$x = -1; @db=()}

	print "STARTING OPENGL\n" if $PDL::Graphics::TriD::verbose;
	glpOpenWindow(attributes=>[&GLX_RGBA, @db,
				&GLX_RED_SIZE,1,
				&GLX_GREEN_SIZE,1,
				&GLX_BLUE_SIZE,1,
				&GLX_DEPTH_SIZE,1,
# Alpha size?
			],
		mask => (KeyPressMask | ButtonPressMask |
			ButtonMotionMask | ButtonReleaseMask |
			ExposureMask | StructureNotifyMask |
			PointerMotionMask),
		width => $w,height => $h,
		"x" => $x);

	glClearColor(0,0,0,1);
	my $lb = PDL::Graphics::OpenGL::glpRasterFont(
		($ENV{PDL_3D_FONT} or "5x8"),0,256);
	$PDL::Graphics::TriD::GL::fontbase = $lb;
#	glDisable(&GL_DITHER);
	glShadeModel (&GL_FLAT);
	glEnable(&GL_DEPTH_TEST);
	glEnable(&GL_NORMALIZE);
	glEnable(&GL_LIGHTING);
	glEnable(&GL_LIGHT0);
	glLightModeli(&GL_LIGHT_MODEL_TWO_SIDE, &GL_TRUE);

	# Will this bring us trouble?
#	if(defined *PDL::Graphics::TriD::GL::Window::glPolygonOffsetEXT{CODE}) {
		glEnable(&GL_POLYGON_OFFSET_EXT);
		glPolygonOffsetEXT(0.0000000000001,0.000002);
#	}

	my $this = bless {
		Ev => {	&ConfigureNotify => \&doconfig,
				&MotionNotify => \&domotion,
		},
		Angle => 0.0,
		Mouse => [undef,undef,undef],
		W => $w,
		H => $h,
		DefMaterial => new PDL::Graphics::TriD::Material,
	},$type;
	$this->reshape();
	my $light = pack "f*",1.0,1.0,1.0,0.0;
	glLightfv(&GL_LIGHT0,&GL_POSITION,$light);
	$this->{DefMaterial}->togl();
	glColor3f(1,1,1);

# Try to interface with Tk event loop?
        if(defined &Tk::DoOneEvent) {
		my $gld = PDL::Graphics::OpenGL::glpXConnectionNumber();
		# Create new mainwindow just for us.
		my $mw = MainWindow->new();
		$mw->iconify();
		my $fh = new FileHandle("<&=$gld\n")
			or die("Couldn't reopen GL filehandle");
		$mw->fileevent($fh,'readable',
		   sub {# print "GLEV\n";
		        $this->twiddle(1)});
		$this->{FileHandle} = $fh;
		$this->{MW} = $mw;
	}

	$this->{Interactive} = 1;
	print "STARTED OPENGL\n" if $PDL::Graphics::TriD::verbose;

	if($PDL::Graphics::TriD::offline) {
		$this->doconfig($PDL::Graphics::TriD::GL::xsize, $PDL::Graphics::TriD::GL::ysize);
	}

	return $this;
}

sub add_resizecommand {
	my($this,$com) = @_;
	push @{$this->{ResizeCommands}},$com;
	print "ARC: $this->{W},$this->{H}\n" if $PDL::Graphics::TriD::verbose;
	&$com($this->{W},$this->{H});
}

sub get_size {return ($PDL::Graphics::TriD::GL::size,$PDL::Graphics::TriD::GL::size);}

sub set_eventhandler {my($this,$handler) = @_;
	$this->{EHandler} = $handler;
}

sub set_transformer {my($this,$transform) = @_;
	$this->{Transformer} = $transform;
}

sub set_material {
  $_[0]->{DefMaterial} = $_[1];
}

sub reshape {
	my($this,$x,$y) = @_;
	$this->{W} = $x; $this->{H} = $y;
	print "ARC: $this->{W},$this->{H}\n" if $PDL::Graphics::TriD::verbose;
	for(@{$this->{ResizeCommands}}) {
		&$_($this->{W},$this->{H});
	}
}

sub do_perspective {
	my($this) = @_;
	if($this->{W}==0 or $this->{H}==0) {return;}
	$this->{AspectRatio} = (1.0*$this->{W})/$this->{H};
#	glResizeBuffers();
	glViewport(0,0,$this->{W},$this->{H});
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
#	glOrtho (-50.0, 50.0, -50.0,50.0,-1.0,1.0);
	gluPerspective(40.0, $this->{AspectRatio} , 0.1, 200000.0);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity ();
#	glTranslatef(0,0,-3);
}

sub setlist { my($this,$list) = @_;
	$this->{List} = $list;

}

# Resize window.
sub doconfig {
	my($this,$x,$y) = @_;
	$this->reshape($x,$y);
	print "CONFIGURENOTIFY\n" if $PDL::Graphics::TriD::verbose;
}

sub domotion {
	my($this) = @_;
	print "MOTIONENOTIFY\n" if $PDL::Graphics::TriD::verbose;
}

sub display {my($this) = @_;
	$this->do_perspective();
	if($this->{W}==0 or $this->{H}==0) {return;}
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	if($this->{Transformer}) {
		print "Transforming!\n" if $PDL::Graphics::TriD::verbose;
		$this->{Transformer}->togl();
	} else {
		glTranslatef(0,0,-3);
		glRotatef($this->{"Angle"},sin($this->{"Angle"}/360),cos($this->{"Angle"}/360),
			sin(2.5*$this->{Angle}/360));
	}
	glTranslatef(-1,-1,-1);
	glScalef(2,2,2);
	my $s = 1.5;
#	glScalef($s,$s,$s);
	$this->gl_call_list();
	glPopMatrix();
	if($this->{ViewPorts}) {
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		print "DOVP: $this->{W} $this->{H}\n" if $PDL::Graphics::TriD::verbose;
		for(@{$this->{ViewPorts}}) {
			$_->[0]->togl_vp($this,$_);
		}
	}
	glFlush();
	glXSwapBuffers();
	$this->{Angle}+= 3;
}

sub read_picture {
	my($this) = @_;
	my($w,$h) = @{$this}{qw/W H/};
	my $res = PDL->zeroes(PDL::byte,3,$w,$h);
	glPixelStorei(&GL_UNPACK_ALIGNMENT,1);
	glPixelStorei(&GL_PACK_ALIGNMENT,1);
	glReadPixels(0,0,$w,$h,&GL_RGB,&GL_UNSIGNED_BYTE,
		${$res->get_dataref});
	return $res;
}

sub new_viewport {
	my($this,$x0,$y0,$x1,$y1) = @_;
	# print "NEW_VIEWPORT: $#{$this->{ViewPorts}}\n";
	push @{$this->{ViewPorts}},[(new PDL::Graphics::TriD::ViewPort()),
		$x0,$y0,$x1,$y1];
	return $this->{ViewPorts}[-1][0];
}

sub clear_viewports {
	my($this) = @_;
	# print "CLEAR VPs: $this->{ViewPorts} $#{$this->{ViewPorts}}\n";
	for(@{$this->{ViewPorts}}) {
		$_->[0]->clear();
		undef %{$_->[0]};
	}
	$this->{ViewPorts} = [];
	# print "CLEARED VPs: $this->{ViewPorts} $#{$this->{ViewPorts}}\n";
}

sub twiddle {my($this,$getout,$dontshow) = @_;
	my (@e);
	my $quit;
	if($PDL::Graphics::TriD::offline) {
		$PDL::Graphics::TriD::offlineindex ++;
		$this->display();
		require PDL::IO::Pic;
		wpic($this->read_picture(),"PDL_$PDL::Graphics::TriD::offlineindex.jpg");
		return;
	}
	if($getout and $dontshow) {
		if(!&XPending()) {return}
	}
	if(!defined $getout) {
		$getout = not $PDL::Graphics::TriD::keeptwiddling;
	}
	$this->display();
	my $hap = 0;
	my $gotev = 0;
	TWIDLOOP: while(1) {
#		print "EVENT!\n";
		$gotev=0;
		if(&XPending() or !$getout) {
			@e = &glpXNextEvent();
			$gotev=1;
		}
		if($e[0] == &VisibilityNotify) {
			$hap = 1;
		}
		if($e[0] == &ConfigureNotify) {
			print "CONFIGNOTIFE\n" if $PDL::Graphics::TriD::verbose;
			$this->reshape($e[1],$e[2]);
			$hap=1;
		}
		if($e[0] == &KeyPress) {
			print "KEYPRESS: '$e[1]'\n" if $PDL::Graphics::TriD::verbose;
			if((lc $e[1]) eq "q") {
				$quit = 1;
			}
			if((lc $e[1]) eq "c") {
				$quit = 2;
			}
			if((lc $e[1]) eq "q" and not $getout) {
				last TWIDLOOP;
			}
			$hap=1;
		}
		if($gotev && defined($this->{EHandler})) {
#			print "HANDLING\n";
			$this->{EHandler}->event($this,@e);
			$hap=1;
		}
		if(!&XPending()) {
			if($hap) {$this->display();}
			if($getout) {last TWIDLOOP}
		}
		undef @e;
	}
	print "STOPTWIDDLE\n" if $PDL::Graphics::TriD::verbose;
	return $quit;
}

###############
#
# Because of the way GL does texturing, this must be the very last thing
# in the object stack before the actual surface. There must not be any
# transformations after this.
#
# There may be several of these but all of these must have just one texture.

@PDL::Graphics::TriD::GL::SliceTexture::ISA = /PDL::Graphics::TriD::Object/;

sub PDL::Graphics::TriD::GL::SliceTexture::new {
	my $image;
	glPixelStorei(GL_UNPACK_ALIGNMENT,1);
	glTexImage1D(GL_TEXTURE_1D,0 , 4, 2,0,GL_RGBA,GL_UNSIGNED_BYTE,
		$image);
	glTexParameterf(GL_TEXTURE_1D,GL_TEXTURE_WRAP_S,GL_CLAMP);
	glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
}

sub PDL::Graphics::TriD::GL::SliceTexture::togl {
	my ($this) = @_;
	glEnable(GL_TEXTURE_1D);
	glTexGen();
	$this->SUPER::togl();
	glDisable(GL_TEXTURE_1D);
}



1;
