# ToDo:
#  - multiple windows - requires editing generate.pl in OpenGL/
#  - clean up
#
#package PDL::Graphics::TriD::GL;

use strict;
use warnings;
no warnings 'redefine';
use OpenGL qw/ :glfunctions :glconstants gluPerspective gluOrtho2D /;
use OpenGL::GLUT qw( :all );
use PDL::Graphics::OpenGL::Perl::OpenGL;
use PDL::Core qw(barf);

$PDL::Graphics::TriD::create_window_sub = # warnings
$PDL::Graphics::TriD::create_window_sub = sub {
  return PDL::Graphics::TriD::GL::Window->new(@_);
};

sub PDL::Graphics::TriD::Material::togl{
  my $this = shift;
  my $shin = pack "f*",$this->{Shine};
  glMaterialfv(GL_FRONT_AND_BACK,GL_SHININESS,$shin);
  my $spec = pack "f*",@{$this->{Specular}};
  glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,$spec);
  my $amb = pack "f*",@{$this->{Ambient}};
  glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,$amb);
  my $diff = pack "f*",@{$this->{Diffuse}};
  glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,$diff);
}

$PDL::Graphics::TriD::any_cannots = 0;
$PDL::Graphics::TriD::verbose //= 0;

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
  print "GENLIST $lno\n" if($PDL::Graphics::TriD::verbose);
  glNewList($lno,GL_COMPILE);
  eval {
    my @objs = @{$this->{Objects}};
    @objs = grep !$_->cannot_mklist(), @objs if $PDL::Graphics::TriD::any_cannots;
    $_->togl() for @objs;
    print "EGENLIST $lno\n" if($PDL::Graphics::TriD::verbose);
    #	pdltotrianglemesh($pdl, 0, 1, 0, ($pdl->{Dims}[1]-1)*$mult);
  };
  glEndList();
  die if $@;
  print "VALID1 $this\n" if($PDL::Graphics::TriD::verbose);
  $this->{ValidList} = 1;
}

sub PDL::Graphics::TriD::Object::gl_call_list {
	my($this) = @_;
	print "CALLIST ",$this->{List}//'undef',"!\n" if($PDL::Graphics::TriD::verbose);
	print "CHECKVALID $this\n" if($PDL::Graphics::TriD::verbose);
	$this->gl_update_list if !$this->{ValidList};
	glCallList($this->{List});
	return if !$PDL::Graphics::TriD::any_cannots;
	for (grep $_->cannot_mklist, @{$this->{Objects}}) {
	  print ref($_)," cannot mklist\n";
	  $_->togl();
	}
}

sub PDL::Graphics::TriD::Object::delete_displist {
	my($this) = @_;
	return if !$this->{List};
	glDeleteLists($this->{List},1);
	delete @$this{qw(List ValidList)};
}

sub PDL::Graphics::TriD::Object::togl { $_->togl for @{$_[0]->{Objects}} }

my @bb1 = ([0,4,2],[0,1,2],[0,1,5],[0,4,5],[0,4,2],[3,4,2],
	   [3,1,2],[3,1,5],[3,4,5],[3,4,2]);
my @bb2 = ([0,1,2],[3,1,2],[0,1,5],[3,1,5],[0,4,5],[3,4,5]);
sub PDL::Graphics::TriD::BoundingBox::togl { 
  my($this) = @_;
  $this = $this->{Box};
  glDisable(GL_LIGHTING);
  glColor3d(1,1,1);
  glBegin(GL_LINES);
  glVertex3d(@{$this}[@$_]) for @bb1;
  glEnd();
  glBegin(GL_LINE_STRIP);
  glVertex3d(@{$this}[@$_]) for @bb2;
  glEnd();
  glEnable(GL_LIGHTING);
}

sub PDL::Graphics::TriD::Graph::togl {
	my($this) = @_;
	$this->{Axis}{$_}->togl_axis($this) for grep $_ ne "Default", keys %{$this->{Axis}};
	$this->{Data}{$_}->togl_graph($this,$this->get_points($_)) for keys %{$this->{Data}};
}

use PDL;
sub PDL::Graphics::TriD::CylindricalEquidistantAxes::togl_axis {
	my($this,$graph) = @_;
	my $fontbase = $PDL::Graphics::TriD::GL::fontbase;
        my (@nadd,@nc,@ns);
	for my $dim (0..1) {
	  my $width = $this->{Scale}[$dim][1]-$this->{Scale}[$dim][0];
	  if($width > 100){
	    $nadd[$dim] = 10;
	  }elsif($width>30){
	    $nadd[$dim] = 5;
	  }elsif($width>20){
	    $nadd[$dim] = 2;
	  }else{
	    $nadd[$dim] = 1;
	  }
	  $nc[$dim] = int($this->{Scale}[$dim][0]/$nadd[$dim])*$nadd[$dim];
	  $ns[$dim] = int($width/$nadd[$dim])+1;
	}
	# can be changed to topo heights?
	my $verts = zeroes(3,$ns[0],$ns[1]);
	(my $t = $verts->slice("2")) .= 1012.5;
	($t = $verts->slice("0")) .= $verts->ylinvals($nc[0],$nc[0]+$nadd[0]*($ns[0]-1));
	($t = $verts->slice("1")) .= $verts->zlinvals($nc[1],$nc[1]+$nadd[1]*($ns[1]-1));
	my $tverts = zeroes(3,$ns[0],$ns[1]);
	$tverts = $this->transform($tverts,$verts,[0,1,2]);
	glDisable(GL_LIGHTING);
	glColor3d(1,1,1);
	for(my $j=0;$j<$tverts->getdim(2)-1;$j++){
	  my $j1=$j+1;
	  glBegin(GL_LINES);
	  for(my $i=0;$i<$tverts->getdim(1)-1;$i++){
	    my $i1=$i+1;
	    glVertex2f($tverts->at(0,$i,$j),$tverts->at(1,$i,$j));
	    glVertex2f($tverts->at(0,$i1,$j),$tverts->at(1,$i1,$j));
	    glVertex2f($tverts->at(0,$i1,$j),$tverts->at(1,$i1,$j));
	    glVertex2f($tverts->at(0,$i1,$j1),$tverts->at(1,$i1,$j1));
	    glVertex2f($tverts->at(0,$i1,$j1),$tverts->at(1,$i1,$j1));
	    glVertex2f($tverts->at(0,$i,$j1),$tverts->at(1,$i,$j1));
	    glVertex2f($tverts->at(0,$i,$j1),$tverts->at(1,$i,$j1));
	    glVertex2f($tverts->at(0,$i,$j),$tverts->at(1,$i,$j));
	  }
	  glEnd();
	}
	glEnable(GL_LIGHTING);
}

sub PDL::Graphics::TriD::EuclidAxes::togl_axis {
	my($this,$graph) = @_;
        print "togl_axis: got object type " . ref($this) . "\n" if $PDL::Graphics::TriD::verbose;
	my $fontbase = $PDL::Graphics::TriD::GL::fontbase;
	glLineWidth(1); # ought to be user defined
	glDisable(GL_LIGHTING);
	my $ndiv = 4;
	my $line_coord = zeroes(3,3)->append(my $id3 = identity(3));
	my $starts = zeroes($ndiv+1)->xlinvals(0,1)->transpose->append(zeroes(2,$ndiv+1));
	my $ends = $starts + append(0, ones 2) * -0.1;
	my $dupseq = sequence(3)->dummy(0,$ndiv+1)->flat;
	$_ = $_->dup(1,3)->rotate($dupseq) for $starts, $ends;
	$line_coord = $line_coord->glue(1, $starts->append($ends));
	my $axisvals = zeroes(3,$ndiv+1)->ylinvals($this->{Scale}->dog)->transpose->flat->transpose;
	my @label = map [@$_[0..2], sprintf "%.3f", $_->[3]], @{ $ends->append($axisvals)->unpdl };
	my $dim = 0; push @label, map [@$_, $this->{Names}[$dim++]], @{ ($id3*1.1)->unpdl };
	glColor3d(1,1,1);
	for (@label) {
		glRasterPos3f(@$_[0..2]);
		if ( done_glutInit() ) {
		   glutBitmapString($fontbase, $_->[3]);
		} else {
		   OpenGL::glpPrintString($fontbase, $_->[3]);
		}
	}
	PDL::gl_lines_nc($line_coord->splitdim(0,3)->clump(1,2));
	glEnable(GL_LIGHTING);
}

use POSIX qw//;
sub PDL::Graphics::TriD::Quaternion::togl {
  my($this) = @_;
  if(abs($this->[0]) == 1) { return ; }
  if(abs($this->[0]) >= 1) {
    $this->normalize_this();
  } 
  glRotatef(2*POSIX::acos($this->[0])/3.14*180, @{$this}[1..3]);
}

##################################
# Graph Objects

sub PDL::Graphics::TriD::GObject::togl {
	$_[0]->gdraw($_[0]->{Points});
}

# (this,graphs,points)
sub PDL::Graphics::TriD::GObject::togl_graph {
	$_[0]->gdraw($_[2]);
}

sub PDL::Graphics::TriD::Points::gdraw {
	my($this,$points) = @_;
	$this->glOptions();
	glDisable(GL_LIGHTING);
	PDL::gl_points_col($points,$this->{Colors});
	glEnable(GL_LIGHTING);
}

sub PDL::Graphics::TriD::Spheres::gdraw {
   my($this,$points) = @_;
   $this->glOptions();
   glEnable(GL_LIGHTING);
   glShadeModel(GL_SMOOTH);
   PDL::gl_spheres($points, 0.025, 15, 15);
}

sub PDL::Graphics::TriD::Lattice::gdraw {
	my($this,$points) = @_;
	barf "Need 3D points AND colours"
	  if grep $_->ndims < 3, $points, $this->{Colors};
	$this->glOptions();
	glDisable(GL_LIGHTING);
	PDL::gl_line_strip_col($points,$this->{Colors});
	PDL::gl_line_strip_col($points->xchg(1,2),$this->{Colors}->xchg(1,2));
	glEnable(GL_LIGHTING);
}

sub PDL::Graphics::TriD::LineStrip::gdraw {
	my($this,$points) = @_;
	$this->glOptions();
	glDisable(GL_LIGHTING);
	PDL::gl_line_strip_col($points,$this->{Colors});
	glEnable(GL_LIGHTING);
}

sub PDL::Graphics::TriD::Lines::gdraw {
	my($this,$points) = @_;
	$this->glOptions();
	glDisable(GL_LIGHTING);
	PDL::gl_lines_col($points,$this->{Colors});
	glEnable(GL_LIGHTING);
}

sub PDL::Graphics::TriD::GObject::glOptions {
  my ($this) = @_;
  glLineWidth($this->{Options}{LineWidth} || 1);
  glPointSize($this->{Options}{PointSize} || 1);
}

sub PDL::Graphics::TriD::Contours::gdraw {
  my($this,$points) = @_;
  $this->glOptions();
  glDisable(GL_LIGHTING);
  my $pcnt=0;
  my $i=0;
  foreach (grep defined, @{$this->{ContourSegCnt}}){
	my $colors =  $this->{Colors};
	$colors = $colors->slice(":,($i)") if $colors->getndims==2;
	PDL::gl_lines_col($points->slice(":,$pcnt:$_"),$colors);
	$i++;
	$pcnt=$_+1;
  }
  if(defined $this->{Labels}){
	 glColor3d(1,1,1);
	 my $seg = sprintf(":,%d:%d",$this->{Labels}[0],$this->{Labels}[1]);
	 PDL::Graphics::OpenGLQ::gl_texts($points->slice($seg),
		 $this->{Options}{Font}
		 ,$this->{LabelStrings});
  }
  glEnable(GL_LIGHTING);
}

my @sls1 = (
  ":,0:-2,0:-2",
  ":,1:-1,0:-2",
  ":,0:-2,1:-1");
my @sls2 = (
  ":,1:-1,1:-1",
  ":,0:-2,1:-1",
  ":,1:-1,0:-2");
sub _lattice_slice {
  my ($f, @pdls) = @_;
  for my $s (\@sls1, \@sls2) {
    my @args;
    for my $p (@pdls) {
      push @args, map $p->slice($_), @$s;
    }
    &$f(@args);
  }
}

sub PDL::Graphics::TriD::SLattice::gdraw {
	my($this,$points) = @_;
	barf "Need 3D points"
	  if grep $_->ndims < 3, $points;
	$this->glOptions();
	glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
	glDisable(GL_LIGHTING);
# By-vertex doesn't make sense otherwise.
	glShadeModel(GL_SMOOTH);
	eval {
	  _lattice_slice(\&PDL::gl_triangles, $points, $this->{Colors});
	  if ($this->{Options}{Lines}) {
	    glColor3f(0,0,0);
	    PDL::gl_line_strip_nc($points);
	    PDL::gl_line_strip_nc($points->xchg(1,2));
	  }
	};
	{ local $@; glPopAttrib(); }
	die if $@;
}

sub PDL::Graphics::TriD::SCLattice::gdraw {
	my($this,$points) = @_;
	barf "Need 3D points"
	  if grep $_->ndims < 3, $points;
	$this->glOptions();
	glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
	glDisable(GL_LIGHTING);
# By-vertex doesn't make sense otherwise.
	glShadeModel(GL_FLAT);
	eval {
	  _lattice_slice(\&PDL::gl_triangles, $points, $this->{Colors});
	  if ($this->{Options}{Lines}) {
	    glColor3f(0,0,0);
	    PDL::gl_line_strip_nc($points);
	    PDL::gl_line_strip_nc($points->xchg(1,2));
	  }
	};
	{ local $@; glPopAttrib(); }
	die if $@;
}

sub PDL::Graphics::TriD::SLattice_S::gdraw {
	my($this,$points) = @_;
	barf "Need 3D points"
	  if grep $_->ndims < 3, $points;
	$this->glOptions();
	glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
# For some reason, we need to set this here as well.
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
# By-vertex doesn't make sense otherwise.
	glShadeModel(GL_SMOOTH);
	eval {
	  my $f = $this->{Options}{Smooth}
	    ? (!$this->{Options}{Material} ? \&PDL::gl_triangles_wn : \&PDL::gl_triangles_wn_mat)
	    : (!$this->{Options}{Material} ? \&PDL::gl_triangles_n : \&PDL::gl_triangles_n_mat);
	  my @pdls = $points;
	  push @pdls, $this->{Normals} // $this->smoothn($points)
	    if $this->{Options}{Smooth};
	  push @pdls, $this->{Colors};
	  _lattice_slice($f, @pdls);
	  if ($this->{Options}{Lines}) {
	    glDisable(GL_LIGHTING);
	    glColor3f(0,0,0);
	    PDL::gl_line_strip_nc($points);
	    PDL::gl_line_strip_nc($points->xchg(1,2));
	  }
	};
	{ local $@; glPopAttrib(); }
	die if $@;
}

sub PDL::Graphics::TriD::STrigrid_S::gdraw {
  my($this,$points) = @_;
  glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
  eval {
    # For some reason, we need to set this here as well.
    glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    # By-vertex doesn't make sense otherwise.
    glShadeModel(GL_SMOOTH);
    my @sls = (":,(0)",":,(1)",":,(2)");
    my $idx = [0,1,2,0]; # for lines, below
    if ($this->{Options}{Smooth}) {
      $this->{Normals} //= $this->smoothn($this->{Points});
      my $f=(!$this->{Options}{Material}?\&PDL::gl_triangles_wn
					:\&PDL::gl_triangles_wn_mat);
      my $tmpn=$this->{Normals}->dice_axis(1,$this->{Faceidx}->clump(-1))
		      ->splitdim(1,$this->{Faceidx}->dim(0));
      my @args=((map {$this->{Faces}->slice($_)} @sls),   # faces is a slice of points
		(map {$tmpn->slice($_)} @sls),
		(map {$this->{Colors}->slice($_)} @sls) );&$f(@args);
    } else {
      my $f=(!$this->{Options}{Material}?\&PDL::gl_triangles_n
					:\&PDL::gl_triangles_n_mat);
      &$f( (map {$this->{Faces}->slice($_)} @sls),   # faces is a slice of points
	   (map {$this->{Colors}->slice($_)} @sls) );
    }
    if ($this->{Options}{Lines}) {
      glDisable(GL_LIGHTING);
      glColor3f(0,0,0);
      PDL::gl_lines_nc($this->{Faces}->dice_axis(1,$idx));
    }
  };
  { local $@; glPopAttrib(); }
  die if $@;
}

sub PDL::Graphics::TriD::STrigrid::gdraw {
  my($this,$points) = @_;
  glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
  eval {
    glDisable(GL_LIGHTING);
# By-vertex doesn't make sense otherwise.
    glShadeModel (GL_SMOOTH);
    my @sls = (":,(0)",":,(1)",":,(2)");
    my $idx = [0,1,2,0];
    PDL::gl_triangles(
      (map {$this->{Faces}->slice($_)} @sls),   # faces is a slice of points
      (map {$this->{Colors}->slice($_)} @sls));
    if ($this->{Options}{Lines}) {
      glColor3f(0,0,0);
      PDL::gl_lines_nc($this->{Faces}->dice_axis(1,$idx));
  }
  };
  { local $@; glPopAttrib(); }
  die if $@;
}

##################################
# PDL::Graphics::TriD::Image

sub PDL::Graphics::TriD::Image::togl {
# A special construct which always faces the display and takes the entire window
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0,1,0,1);
  &PDL::Graphics::TriD::Image::togl_graph;
}

sub PDL::Graphics::TriD::Image::togl_graph {
	$_[0]->gdraw();
}

# The quick method is to use texturing for the good effect.
sub PDL::Graphics::TriD::Image::gdraw {
	my($this,$vert) = @_;
	my ($p,$xd,$yd,$txd,$tyd) = $this->flatten(1); # do binary alignment
	if(!defined $vert) {$vert = $this->{Points}}
	barf "Need 3,4 vert"
	  if grep $_->dim(1) < 4 || $_->dim(0) != 3, $vert;
	glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
	glColor3d(1,1,1);
         glTexImage2D_s(GL_TEXTURE_2D, 0, GL_RGB, $txd, $tyd, 0, GL_RGB, GL_FLOAT, $p->get_dataref());
	 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
	    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
	       glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
	          glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
	glDisable(GL_LIGHTING);
	glNormal3d(0,0,1);
	glEnable(GL_TEXTURE_2D);
	glBegin(GL_QUADS);
	eval {
	  my @texvert = (
		  [0,0],
		  [$xd/$txd, 0],
		  [$xd/$txd, $yd/$tyd],
		  [0, $yd/$tyd]
	  );
	  for(0..3) {
		  glTexCoord2f(@{$texvert[$_]});
		  glVertex3f($vert->slice(":,($_)")->list);
	  }
	};
	{ local $@; glEnd(); glPopAttrib(); }
	die if $@;
	glDisable(GL_TEXTURE_2D);
}

sub PDL::Graphics::TriD::SimpleController::togl {
	my($this) = @_;
	$this->{CRotation}->togl();
	glTranslatef(0,0,-$this->{CDistance});
	$this->{WRotation}->togl();
	glTranslatef(map {-$_} @{$this->{WOrigin}});
}

##############################################
# A window with mouse control over rotation.
package PDL::Graphics::TriD::Window;

use OpenGL qw/ :glfunctions :glconstants :glxconstants /;
use OpenGL::GLUT qw( :all );
use PDL::Graphics::OpenGL::Perl::OpenGL;

use base qw/PDL::Graphics::TriD::Object/;
use fields qw/Ev Width Height Interactive _GLObject
              _ViewPorts _CurrentViewPort /;

sub i_keep_list {return 1} # For Object, so I will be notified of changes.
use strict;

sub gdriver {
  my($this, $options) = @_;
  print "GL gdriver...\n" if($PDL::Graphics::TriD::verbose);
  if(defined $this->{_GLObject}){
	 print "WARNING: Graphics Driver already defined for this window \n";
	 return;
  }
  my @db = OpenGL::GLX_DOUBLEBUFFER;
  if($PDL::Graphics::TriD::offline) {$options->{x} = -1; @db=()}
  $options->{attributes} = [GLX_RGBA, @db,
			    GLX_RED_SIZE,1,
			    GLX_GREEN_SIZE,1,
			    GLX_BLUE_SIZE,1,
			    GLX_DEPTH_SIZE,1,
			    # Alpha size?
			   ] unless defined $options->{attributes};
  $options->{mask} = (KeyPressMask | ButtonPressMask |
			 ButtonMotionMask | ButtonReleaseMask |
			 ExposureMask | StructureNotifyMask |
			 PointerMotionMask) unless defined $options->{mask};
  print "STARTING OPENGL $options->{width} $options->{height}\n" if($PDL::Graphics::TriD::verbose);
  print "gdriver: Calling OpengGL::OO($options)...\n" if ($PDL::Graphics::TriD::verbose);
  $this->{_GLObject}= new PDL::Graphics::OpenGL::OO($options);
  if (exists $this->{_GLObject}->{glutwindow}) {
     if ($PDL::Graphics::TriD::verbose) {
        print "gdriver: Got OpenGL::OO object(GLUT window ID# " . $this->{_GLObject}->{glutwindow} . ")\n";
     }
     $this->{_GLObject}->{winobjects}->[$this->{_GLObject}->{glutwindow}] = $this;      # circular ref
  }
  print "gdriver: Calling glClearColor...\n" if $PDL::Graphics::TriD::verbose;
  glClearColor(0,0,0,1);
  print "gdriver: Calling glpRasterFont...\n" if $PDL::Graphics::TriD::verbose;
  $PDL::Graphics::TriD::GL::fontbase = $this->{_GLObject}->glpRasterFont($ENV{PDL_3D_FONT} || "5x8", 0, 256);
  glShadeModel(GL_FLAT);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_NORMALIZE);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  my $light = pack "f*",1.0,1.0,1.0,0.0;
  glLightfv_s(GL_LIGHT0,GL_POSITION,$light);
  glColor3f(1,1,1);
  print "STARTED OPENGL!\n" if $PDL::Graphics::TriD::verbose;
  if($PDL::Graphics::TriD::offline) {
    $this->doconfig($options->{width}, $options->{height});
  }
  return 1;  # Interactive Window
}

sub ev_defaults{
  return {	ConfigureNotify => \&doconfig,
				MotionNotify => \&domotion,
			}
}

sub reshape {
	my($this,$x,$y) = @_;
	my $pw = $this->{Width};
	my $ph = $this->{Height};
	$this->{Width} = $x; $this->{Height} = $y;
	for my $vp (@{$this->{_ViewPorts}}){
	  my $nw = $vp->{W} + ($x-$pw) * $vp->{W}/$pw;
	  my $nx0 = $vp->{X0} + ($x-$pw) * $vp->{X0}/$pw;
	  my $nh = $vp->{H} + ($y-$ph) * $vp->{H}/$ph;
	  my $ny0 = $vp->{Y0} + ($y-$ph) * $vp->{Y0}/$ph;
	  print "reshape: resizing viewport to $nx0,$ny0,$nw,$nh\n" if($PDL::Graphics::TriD::verbose);
	  $vp->resize($nx0,$ny0,$nw,$nh);
	}
}

sub get_size { @{$_[0]}{qw(Width Height)} }

sub twiddle {
  my($this,$getout,$dontshow) = @_;
  my (@e);
  my $quit;
  if($PDL::Graphics::TriD::offline) {
	 $PDL::Graphics::TriD::offlineindex ++;
	 $this->display();
	 require PDL::IO::Pic;
	 wpic($this->read_picture(),"PDL_$PDL::Graphics::TriD::offlineindex.jpg");
	 return;
  }
  return if $getout and $dontshow and !$this->{_GLObject}->XPending;
  $getout //= !($PDL::Graphics::TriD::keeptwiddling && $PDL::Graphics::TriD::keeptwiddling);
  $this->display();
 TWIDLOOP: while(1) {
   print "EVENT!\n" if($PDL::Graphics::TriD::verbose);
	 my $hap = 0;
	 my $gotev = 0;
         # Run a MainLoop event if GLUT windows
         # this pumps the system allowing callbacks to populate
         # the fake XEvent queue.
         #
         glutMainLoopEvent() if $this->{_GLObject}->{window_type} eq 'glut' and not $this->{_GLObject}->XPending();
         if ($this->{_GLObject}->XPending() or !$getout) {
            @e = $this->{_GLObject}->glpXNextEvent();
            $gotev=1;
         }
   print "e= ".join(",",@e)."\n" if($PDL::Graphics::TriD::verbose);
	 if(@e){
		if ($e[0] == VisibilityNotify || $e[0] == Expose) {
		  $hap = 1;
		} elsif ($e[0] == ConfigureNotify) {
		  print "CONFIGNOTIFE\n" if($PDL::Graphics::TriD::verbose);
		  $this->reshape($e[1],$e[2]);
		  $hap=1;
		} elsif ($e[0] == DestroyNotify) {
		  print "DESTROYNOTIFE\n" if $PDL::Graphics::TriD::verbose;
		  $quit = 1;
		  $hap=1;
		  $this->close;
		  last TWIDLOOP;
		} elsif($e[0] == KeyPress) {
		  print "KEYPRESS: '$e[1]'\n" if($PDL::Graphics::TriD::verbose);
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
	 }
	 if($gotev){
		#			print "HANDLING $this->{EHandler}\n";
		foreach my $vp (@{$this->{_ViewPorts}}) {
		  if(defined($vp->{EHandler})) {
			 $hap += $vp->{EHandler}->event(@e) || 0;
		  }
		}
	 }
	 if(! $this->{_GLObject}->XPending()) {
		if($hap) {
		  $this->display();
		}
		if($getout) {last TWIDLOOP}
	 }
	 @e = ();
  }
  print "STOPTWIDDLE\n" if($PDL::Graphics::TriD::verbose);
  return $quit;
}

sub close {
  my ($this, $close_window) = @_;
  print "CLOSE\n" if $PDL::Graphics::TriD::verbose;
  undef $this->{_GLObject};
  $PDL::Graphics::TriD::current_window = undef;
}

sub setlist { my($this,$list) = @_;
	$this->{List} = $list;
}

# Resize window.
sub doconfig {
	my($this,$x,$y) = @_;
	$this->reshape($x,$y);
	print "CONFIGURENOTIFY\n" if($PDL::Graphics::TriD::verbose);
}

sub domotion {
	my($this) = @_;
	print "MOTIONENOTIFY\n" if($PDL::Graphics::TriD::verbose);
}

sub display {
  my($this) = @_;
  return unless defined($this);
  # set GLUT context to current window (for multiwindow support)
  if ( $this->{_GLObject}->{window_type} eq 'glut' ) {
     glutSetWindow($this->{_GLObject}->{glutwindow});
  }
  print "display: calling glClear()\n" if ($PDL::Graphics::TriD::verbose);
  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  for my $vp (@{$this->{_ViewPorts}}) {
	 glPushMatrix();
	 $vp->do_perspective();
	 if($vp->{Transformer}) {
		print "display: transforming viewport!\n" if ($PDL::Graphics::TriD::verbose);
		$vp->{Transformer}->togl();
	 }
	 $vp->gl_call_list();
	 glPopMatrix();
  }
  print "display: SwapBuffers() call on return\n" if ($PDL::Graphics::TriD::verbose);
  if ( $this->{_GLObject}->{window_type} eq 'glut' ) {  # need to make method call
    glutSwapBuffers();
  } elsif ( $this->{_GLObject}->{window_type} eq 'x11' ) {  # need to make method call
    $this->{_GLObject}->glXSwapBuffers();
  } else {
    print "display: got object with inconsistent _GLObject info\n";
  }
}

# should this really be in viewport?
sub read_picture {
	my($this) = @_;
	my($w,$h) = @{$this}{qw/Width Height/};
	my $res = PDL->zeroes(PDL::byte,3,$w,$h);
	glPixelStorei(GL_UNPACK_ALIGNMENT,1);
	glPixelStorei(GL_PACK_ALIGNMENT,1);
        glReadPixels_s(0,0,$w,$h,GL_RGB,GL_UNSIGNED_BYTE,$res->get_dataref);
	return $res;
}

######################################################################
######################################################################
# EVENT HANDLER MINIPACKAGE FOLLOWS!

package PDL::Graphics::TriD::EventHandler;

use OpenGL qw(
  ConfigureNotify MotionNotify DestroyNotify
  ButtonPress ButtonRelease Button1Mask Button2Mask Button3Mask
);
use PDL::Graphics::OpenGL::Perl::OpenGL;

use fields qw/X Y Buttons VP/;
use strict;
sub new {
  my $class = shift;
  my $vp = shift;
  no strict 'refs';
  my $self = fields::new($class);
  $self->{X} = -1;
  $self->{Y} = -1;
  $self->{Buttons} = [];
  $self->{VP} = $vp;

  $self;
}

sub event {
  my($this,$type,@args) = @_;
  print "EH: ",ref($this)," $type (",join(",",@args),")\n" if($PDL::Graphics::TriD::verbose);
  my $retval;
  if($type == MotionNotify) {
	 my $but = -1;
  SWITCH: {
		$but = 0, last SWITCH if ($args[0] & (Button1Mask));
		$but = 1, last SWITCH if ($args[0] & (Button2Mask));
		$but = 2, last SWITCH if ($args[0] & (Button3Mask));
		print "No button pressed...\n" if($PDL::Graphics::TriD::verbose);
		goto NOBUT;
	 }
	 print "MOTION $but $args[0]\n" if($PDL::Graphics::TriD::verbose);
	 if($this->{Buttons}[$but]) {
		if($this->{VP}->{Active}){
		  print "calling ".($this->{Buttons}[$but])."->mouse_moved ($this->{X},$this->{Y},$args[1],$args[2])...\n" if($PDL::Graphics::TriD::verbose);
		  $retval = $this->{Buttons}[$but]->mouse_moved(
								$this->{X},$this->{Y},
								$args[1],$args[2]);
		}
	 }
	 $this->{X} = $args[1]; $this->{Y} = $args[2];
  NOBUT:
       } elsif($type == ButtonPress) {
	 my $but = $args[0]-1;
	 print "BUTTONPRESS $but\n" if($PDL::Graphics::TriD::verbose);
	 $this->{X} = $args[1]; $this->{Y} = $args[2];
	 $retval = $this->{Buttons}[$but]->ButtonPress($args[1],$args[2])
	   if($this->{Buttons}[$but]);
       } elsif($type == ButtonRelease) {
	 my $but = $args[0]-1;
	 print "BUTTONRELEASE $but\n" if($PDL::Graphics::TriD::verbose);
	 $retval = $this->{Buttons}[$but]->ButtonRelease($args[1],$args[2])
	   if($this->{Buttons}[$but]);
       } elsif($type== ConfigureNotify) {
	 # Kludge to force reshape of the viewport associated with the window -CD
	 print "ConfigureNotify (".join(",",@args).")\n" if($PDL::Graphics::TriD::verbose);
	 print "viewport is $this->{VP}\n" if($PDL::Graphics::TriD::verbose);
       }
  $retval;
}

sub set_button {
	my($this,$butno,$act) = @_;
	$this->{Buttons}[$butno] = $act;
}

######################################################################
######################################################################
# VIEWPORT MINI_PACKAGE FOLLOWS!

package PDL::Graphics::TriD::ViewPort;
use base qw/PDL::Graphics::TriD::Object/;
use fields qw/X0 Y0 W H Transformer EHandler Active ResizeCommands 
              DefMaterial AspectRatio Graphs/;

use OpenGL qw/ :glfunctions :glconstants :glufunctions /;
use OpenGL::GLUT qw( :all );
use PDL::Graphics::OpenGL::Perl::OpenGL;
use PDL::Graphics::OpenGLQ;

sub highlight {
  my ($vp) = @_;
  my $pts =  new PDL [[0,0,0],
		      [$vp->{W},0,0],
		      [$vp->{W},$vp->{H},0],
		      [0,$vp->{H},0],
		      [0,0,0]];
  glDisable(GL_LIGHTING);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0,$vp->{W},0,$vp->{H});
  glLineWidth(4);
  glColor3f(1,1,1);
  gl_line_strip_nc($pts);
  glLineWidth(1);
  glEnable(GL_LIGHTING);
}

sub do_perspective {
	my($this) = @_;
	print "do_perspective ",$this->{W}," ",$this->{H} ,"\n" if($PDL::Graphics::TriD::verbose);
	print Carp::longmess() if $PDL::Graphics::TriD::verbose>1;
        unless($this->{W}>0 and $this->{H}>0) {return;}
	$this->{AspectRatio} = (1.0*$this->{W})/$this->{H};
	glViewport($this->{X0},$this->{Y0},$this->{W},$this->{H});
	$this->highlight() if($this->{Active});
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(40.0, $this->{AspectRatio} , 0.1, 200000.0);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity ();
}

###############
#
# Because of the way GL does texturing, this must be the very last thing
# in the object stack before the actual surface. There must not be any
# transformations after this.
#
# There may be several of these but all of these must have just one texture.

@PDL::Graphics::TriD::GL::SliceTexture::ISA = qw/PDL::Graphics::TriD::Object/;

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
