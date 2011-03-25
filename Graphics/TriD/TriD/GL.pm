#
#
# ToDo:
#  - multiple windows - requires editing generate.pl in OpenGL/
#  - clean up
#
#package PDL::Graphics::TriD::GL;


BEGIN {
   use PDL::Config;
   if ( $PDL::Config{USE_POGL} ) {
      eval "use OpenGL $PDL::Config{POGL_VERSION} qw(:all)";
      eval 'use PDL::Graphics::OpenGL::Perl::OpenGL';
   } else {
      eval 'use PDL::Graphics::OpenGL';
   }
}

$PDL::Graphics::TriD::create_window_sub = sub {
  return new PDL::Graphics::TriD::GL::Window(@_);
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
  if ($PDL::Graphics::TriD::any_cannots) {
	 for(@{$this->{Objects}}) {
		if(!$_->cannot_mklist()) {
		  $_->togl();
		}
	 }
  } else { 
	 for (@{$this->{Objects}}) {
		$_->togl()
	 } 
  }
  print "EGENLIST $lno\n" if($PDL::Graphics::TriD::verbose);
  #	pdltotrianglemesh($pdl, 0, 1, 0, ($pdl->{Dims}[1]-1)*$mult);
  glEndList();
  print "VALID1 $this\n" if($PDL::Graphics::TriD::verbose);
  $this->{ValidList} = 1;
}

sub PDL::Graphics::TriD::Object::gl_call_list {
	my($this) = @_;
	print "CALLIST ",$this->{List},"!\n" if($PDL::Graphics::TriD::verbose);
	print "CHECKVALID $this\n" if($PDL::Graphics::TriD::verbose);

	if(!$this->{ValidList}) {
		$this->gl_update_list();
	}
	glCallList($this->{List});
	if ($PDL::Graphics::TriD::any_cannots) {
	  for(@{$this->{Objects}}) {
		if($_->cannot_mklist()) {

         print ref($_)," cannot mklist\n";

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


sub PDL::Graphics::TriD::BoundingBox::togl { 
  my($this) = @_;
  $this = $this->{Box};
  glDisable(GL_LIGHTING);
  glColor3d(1,1,1);
  glBegin(GL_LINES);
  for([0,4,2],[0,1,2],[0,1,5],[0,4,5],[0,4,2],[3,4,2],
		[3,1,2],[3,1,5],[3,4,5],[3,4,2]) {
	 &glVertex3d(@{$this}[@$_]);
  }
  glEnd();
  glBegin(GL_LINE_STRIP);
  for([0,1,2],[3,1,2],[0,1,5],[3,1,5],[0,4,5],[3,4,5]) {
	 &glVertex3d(@{$this}[@$_]);
  }
  glEnd();
  glEnable(GL_LIGHTING);
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
#	  print "TOGL   $_, $this->{Data}{$_}  $this->{Data}{$_}{Options}{LineWidth}\n";
	  $this->{Data}{$_}->togl_graph($this,$this->get_points($_));
	}
}

use PDL;
sub PDL::Graphics::TriD::CylindricalEquidistantAxes::togl_axis {
	my($this,$graph) = @_;

	my $fontbase = $PDL::Graphics::TriD::GL::fontbase;

   
        my (@nadd,@nc,@ns);

	for $dim (0..1) {
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

	($t = $verts->slice("2")) .= 1012.5;
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

        print "togl_axis: got object type " . ref($this) . "\n" if $PDL::debug_trid;
#	print "TOGLAX\n";
	my $fontbase = $PDL::Graphics::TriD::GL::fontbase;
#	print "TOGL EUCLID\n";
   glLineWidth(1); # ought to be user defined
	glDisable(GL_LIGHTING);
	glColor3d(1,1,1);
	glBegin(GL_LINES);
	my $dim;
	for $dim (0..2) {
		glVertex3f(0,0,0);
		&glVertex3f(map {$_==$dim} 0..2);
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
			&glRasterPos3f(@coords);
                        if ( $PDL::Config{USE_POGL} ) {
                              if ( OpenGL::done_glutInit() ) {
                                 OpenGL::glutBitmapString($fontbase, sprintf("%.3f",$nc));
                              } else {
                                 OpenGL::glpPrintString($fontbase, sprintf("%.3f",$nc));
                              }
                           } else {
                              PDL::Graphics::OpenGL::glpPrintString($fontbase, sprintf("%.3f",$nc));
                           }
			glBegin(GL_LINES);
			&glVertex3f(@coords0);
			&glVertex3f(@coords);
			glEnd();
#			print "PUT: $nc\n";
			$coords[$dim] += $radd;
			$coords0[$dim] += $radd;
			$nc += $nadd;
		}
		$coords0[$dim] = 1.1;
		&glRasterPos3f(@coords0);
                if ( $PDL::Config{USE_POGL} ) {
                   if ( OpenGL::done_glutInit() ) {
                      OpenGL::glutBitmapString($fontbase, $this->{Names}[$dim]);
                   } else {
                      OpenGL::glpPrintString($fontbase, $this->{Names}[$dim]);
                   }
                } else {
                   PDL::Graphics::OpenGL::glpPrintString($fontbase, $this->{Names}[$dim]);
                }
             }
	glEnable(GL_LIGHTING);
}



use POSIX qw//;
sub PDL::Graphics::TriD::Quaternion::togl {
  my($this) = @_;
  if(abs($this->[0]) == 1) { return ; }
  if(abs($this->[0]) >= 1) {
    # die "Unnormalized Quaternion!\n";
    $this->normalize_this();
  } 
  &glRotatef(2*POSIX::acos($this->[0])/3.14*180, @{$this}[1..3]);
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

	$this->glOptions();
	glDisable(GL_LIGHTING);
	PDL::gl_points($points,$this->{Colors});
	glEnable(GL_LIGHTING);
}

sub PDL::gl_spheres { 
   my ($coords,$colors) = @_;   
   for (my $np=0; $np<$coords->dim(1); $np++) {
      glPushMatrix();
      my ($x,$y,$z) = ($coords->slice(":,($np)"))->float->list;
      glTranslatef($x,$y,$z);
      glutSolidSphere(0.025,15,15);
      glPopMatrix();
   }
}

sub PDL::Graphics::TriD::Spheres::gdraw {
   my($this,$points) = @_;
   $this->glOptions();
   glShadeModel(GL_SMOOTH);
   PDL::gl_spheres($points,$this->{Colors});
}

sub PDL::Graphics::TriD::Lattice::gdraw {
	my($this,$points) = @_;

	$this->glOptions();
	glDisable(GL_LIGHTING);
	PDL::gl_line_strip($points,$this->{Colors});
	PDL::gl_line_strip($points->xchg(1,2),$this->{Colors}->xchg(1,2));
	glEnable(GL_LIGHTING);
}


sub PDL::Graphics::TriD::LineStrip::gdraw {
	my($this,$points) = @_;

	$this->glOptions();
	glDisable(GL_LIGHTING);
	PDL::gl_line_strip($points,$this->{Colors});
	glEnable(GL_LIGHTING);
}

sub PDL::Graphics::TriD::Lines::gdraw {
	my($this,$points) = @_;

	$this->glOptions();
	glDisable(GL_LIGHTING);
	PDL::gl_lines($points,$this->{Colors});
	glEnable(GL_LIGHTING);
}

sub PDL::Graphics::TriD::GObject::glOptions {
  my ($this) = @_;

  if($this->{Options}{LineWidth}){
	 glLineWidth($this->{Options}{LineWidth});
  }else{
	 glLineWidth(1);
  }

  if($this->{Options}{PointSize}){
	 glPointSize($this->{Options}{PointSize});
  }else{
	 glPointSize(1);
  }
  
  



}


sub PDL::Graphics::TriD::Contours::gdraw {
  my($this,$points) = @_;

  $this->glOptions();

  glDisable(GL_LIGHTING);
  my $pcnt=0;
  my $i=0;


  foreach(@{$this->{ContourSegCnt}}){
	 my $colors;
	 if($this->{Colors}->getndims==2){
		$colors = $this->{Colors}->slice(":,($i)");
	 }else{
		$colors =  $this->{Colors};
	 }
	 next unless(defined $_);
	 PDL::gl_lines($points->slice(":,$pcnt:$_"),$colors);
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

sub PDL::Graphics::TriD::SLattice::gdraw {
	my($this,$points) = @_;

	$this->glOptions();

	glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
	glDisable(GL_LIGHTING);
# By-vertex doesn't make sense otherwise.
	glShadeModel (GL_SMOOTH);
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
	  PDL::gl_line_strip($points,$black);
	  PDL::gl_line_strip($points->xchg(1,2),$black);
	}
	glPopAttrib();
}

sub PDL::Graphics::TriD::SCLattice::gdraw {
	my($this,$points) = @_;

	$this->glOptions();

	glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
	glDisable(GL_LIGHTING);
# By-vertex doesn't make sense otherwise.
	glShadeModel (GL_FLAT);
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
	  PDL::gl_line_strip($points,$black);
	  PDL::gl_line_strip($points->xchg(1,2),$black);
	}
	glPopAttrib();
}

sub PDL::Graphics::TriD::SLattice_S::gdraw {
	my($this,$points) = @_;


	$this->glOptions();

	glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
# For some reason, we need to set this here as well.
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
# By-vertex doesn't make sense otherwise.
	glShadeModel (GL_SMOOTH);
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
	glDisable(GL_LIGHTING);
	if ($this->{Options}{Lines}) {
	  my $black = PDL->pdl(0,0,0)->dummy(1)->dummy(1);
	  PDL::gl_line_strip($points,$black);
	  PDL::gl_line_strip($points->xchg(1,2),$black);
	}
	glPopAttrib();
}

#################################################################### 
################### JNK 15mar11 added section start ################
sub PDL::Graphics::TriD::STrigrid_S::gdraw {
  my($this,$points) = @_;
  glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
  # For some reason, we need to set this here as well.
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  # By-vertex doesn't make sense otherwise.
  glShadeModel (GL_SMOOTH);   
  my @sls = (":,(0)",":,(1)",":,(2)");
  my $idx = [0,1,2,0]; # for lines, below
  if ($this->{Options}{Smooth}) {
    $this->{Normals}=$this->smoothn($this->{Points})
      unless defined($this->{Normals}); 
    my $f=(!$this->{Options}{Material}?\&PDL::gl_triangles_wn
                                      :\&PDL::gl_triangles_wn_mat);
    my $tmpn=$this->{Normals}->dice_axis(1,$this->{Faceidx}->clump(-1))
                    ->splitdim(1,($this->{Faceidx}->dims)[0]);
    my @args=((map {$this->{Faces}->slice($_)} @sls),   # faces is a slice of points
              (map {$tmpn->slice($_)} @sls),
              (map {$this->{Colors}->slice($_)} @sls) );&$f(@args); }
  else {
    my $f=(!$this->{Options}{Material}?\&PDL::gl_triangles_n
                                      :\&PDL::gl_triangles_n_mat);
    &$f( (map {$this->{Faces}->slice($_)} @sls),   # faces is a slice of points
         (map {$this->{Colors}->slice($_)} @sls) ); } 
  glDisable(GL_LIGHTING);
  if ($this->{Options}{Lines}) {
    my $black = PDL->pdl(0,0,0)->dummy(1)->dummy(1);
    PDL::gl_lines($this->{Faces}->dice_axis(1,$idx),$black); } glPopAttrib(); }

sub PDL::Graphics::TriD::STrigrid::gdraw {
  my($this,$points) = @_;
  glPushAttrib(GL_LIGHTING_BIT | GL_ENABLE_BIT);
  glDisable(GL_LIGHTING);
# By-vertex doesn't make sense otherwise.
  glShadeModel (GL_SMOOTH);
  my @sls = (":,(0)",":,(1)",":,(2)");
  my $idx = [0,1,2,0];
  PDL::gl_triangles(
    (map {$this->{Faces}->slice($_)} @sls),   # faces is a slice of points
    (map {$this->{Colors}->slice($_)} @sls));
  if ($this->{Options}{Lines}) {
    my $black = PDL->pdl(0,0,0)->dummy(1)->dummy(1);
    PDL::gl_lines($this->{Faces}->dice_axis(1,$idx),$black); }
  glPopAttrib(); }
################### JNK 15mar11 added section finis ################
####################################################################

##################################
# PDL::Graphics::TriD::Image
#
#

sub PDL::Graphics::TriD::Image::togl {
#  glDisable(GL_LIGHTING);
#
# A special construct which always faces the display and takes the entire window
#  
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
	glColor3d(1,1,1);
        if ( $PDL::Config{USE_POGL} ) {
           glTexImage2D_s(GL_TEXTURE_2D, 0, GL_RGB, $txd, $tyd, 0, GL_RGB, GL_FLOAT, $p->get_dataref());
        } else {
           glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, $txd, $tyd, 0, GL_RGB, GL_FLOAT, $p->get_dataref());
        }
	 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
	    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
	       glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
	          glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );

	glDisable(GL_LIGHTING);
	glNormal3d(0,0,1);
	glEnable(GL_TEXTURE_2D);
	glBegin(GL_QUADS);
	my @texvert = (
		[0,0],
		[$xd/$txd, 0],
		[$xd/$txd, $yd/$tyd],
		[0, $yd/$tyd]
	);
	if(!defined $vert) {$vert = $this->{Points}}
	for(0..3) {
		&glTexCoord2f(@{$texvert[$_]});
		&glVertex3f($vert->slice(":,($_)")->list);
	}
	glEnd();
	glEnable(GL_LIGHTING);
	glDisable(GL_TEXTURE_2D);
}

sub PDL::Graphics::TriD::SimpleController::togl {
	my($this) = @_;

	$this->{CRotation}->togl();
	glTranslatef(0,0,-$this->{CDistance});

	$this->{WRotation}->togl();
	&glTranslatef(map {-$_} @{$this->{WOrigin}});
}


##############################################
#
# A window with mouse control over rotation.
#
#
package PDL::Graphics::TriD::Window;

BEGIN {
   use PDL::Config;
   if ( $PDL::Config{USE_POGL} ) {
      eval "use OpenGL $PDL::Config{POGL_VERSION} qw(:all)";
      eval 'use PDL::Graphics::OpenGL::Perl::OpenGL';
   } else {
      eval 'use PDL::Graphics::OpenGL';
   }
}

use base qw/PDL::Graphics::TriD::Object/;
use fields qw/Ev Width Height Interactive _GLObject 
              _ViewPorts _CurrentViewPort /;

sub i_keep_list {return 1} # For Object, so I will be notified of changes.
use strict;

sub gdriver {
  my($this, $options) = @_;
  
  print "GL gdriver...\n" if($PDL::debug_trid);

  if(defined $this->{_GLObject}){
	 print "WARNING: Graphics Driver already defined for this window \n";
	 return;
  }
  my @db = GLX_DOUBLEBUFFER;

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

  print "gdriver: Calling OpengGL::OO($options)...\n" if ($PDL::debug_trid);

  $this->{_GLObject}= new PDL::Graphics::OpenGL::OO($options);

  if (exists $this->{_GLObject}->{glutwindow}) {
     if ($PDL::debug_trid) {
        print "gdriver: Got OpenGL::OO object(GLUT window ID# " . $this->{_GLObject}->{glutwindow} . ")\n";
     }
     $this->{_GLObject}->{winobjects}->[$this->{_GLObject}->{glutwindow}] = $this;      # circular ref
  }

#glpOpenWindow(%$options);
  
  print "gdriver: Calling glClearColor...\n" if ($PDL::debug_trid);
  glClearColor(0,0,0,1);

  print "gdriver: Calling glpRasterFont...\n" if ($PDL::debug_trid);
  if ( $this->{_GLObject}->{window_type} eq 'glut' ) {
     print STDERR "gdriver: window_type => 'glut' so not actually setting the rasterfont\n" if ($PDL::debug_trid);
     eval '$PDL::Graphics::TriD::GL::fontbase = GLUT_BITMAP_8_BY_13';
  } else {
     # NOTE: glpRasterFont() will die() if the requested font cannot be found
     #       The new POGL+GLUT TriD implementation uses the builtin GLUT defined
     #       fonts and does not have this failure mode.
     
     my $lb =  eval { $this->{_GLObject}->glpRasterFont( ($ENV{PDL_3D_FONT} or "5x8"), 0, 256 ) };
     if ( $@ ) {
        die "glpRasterFont: unable to load font '%s', please set PDL_3D_FONT to an existing X11 font.";
     }
     $PDL::Graphics::TriD::GL::fontbase = $lb
  }
  #	glDisable(GL_DITHER);
  glShadeModel (GL_FLAT);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_NORMALIZE);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  
  # Will this bring us trouble?
  #	if(defined *PDL::Graphics::TriD::GL::Window::glPolygonOffsetEXT{CODE}) {
#  glEnable(GL_POLYGON_OFFSET_EXT);
#  glPolygonOffsetEXT(0.0000000000001,0.000002);
  #	}
  
  # Inherits attributes of Object class
#  my $this = $type->SUPER::new();
#  $this->reshape($options->{width},$options->{height});

  my $light = pack "f*",1.0,1.0,1.0,0.0;
  if ( $PDL::Config{USE_POGL} ) {
     glLightfv_s(GL_LIGHT0,GL_POSITION,$light);
  } else {
     glLightfv(GL_LIGHT0,GL_POSITION,$light);
  }

  glColor3f(1,1,1);
  
#  $this->{Interactive} = 1;
  print "STARTED OPENGL!\n" if($PDL::Graphics::TriD::verbose);
  
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



sub get_size {
  my $this=shift;
  return ($this->{Width},$this->{Height});
}


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
  if ($getout and $dontshow) {
	 if ( !$this->{_GLObject}->XPending() ) {
            return;
         }
  }
  if(!defined $getout) {
	 $getout = not $PDL::Graphics::TriD::keeptwiddling;
  }
  
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
			 $hap += $vp->{EHandler}->event(@e);
		  }
		}
	 }
	 if(! $this->{_GLObject}->XPending()) {
		if($hap) {
		  $this->display();
		}
		if($getout) {last TWIDLOOP}
	 }
	 undef @e;
  }
  print "STOPTWIDDLE\n" if($PDL::Graphics::TriD::verbose);
  return $quit;
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

  # set GLUT context to current window (for multiwindow support
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

	 glTranslatef(-1,-1,-1);
	 glScalef(2,2,2);  # double the scale in each direction ?

	 $vp->gl_call_list();

	 glPopMatrix();

  }

  if ( $PDL::Config{USE_POGL} ) {

     print "display: SwapBuffers() call on return\n" if ($PDL::Graphics::TriD::verbose);
     if ( $this->{_GLObject}->{window_type} eq 'glut' ) {  # need to make method call
        glutSwapBuffers();
     } elsif ( $this->{_GLObject}->{window_type} eq 'x11' ) {  # need to make method call
        $this->{_GLObject}->glXSwapBuffers();
     } else {
        print "display: got object with inconsistent _GLObject info\n";
     }

  } else {
     $this->{_GLObject}->glXSwapBuffers();
  }
#  $this->{Angle}+= 3;
}

# should this reallyt be in viewport?
sub read_picture {
	my($this) = @_;
	my($w,$h) = @{$this}{qw/Width Height/};
	my $res = PDL->zeroes(PDL::byte,3,$w,$h);
	glPixelStorei(GL_UNPACK_ALIGNMENT,1);
	glPixelStorei(GL_PACK_ALIGNMENT,1);

        if ( $PDL::Config{USE_POGL} ) {
           glReadPixels_s(0,0,$w,$h,GL_RGB,GL_UNSIGNED_BYTE,$res->get_dataref);
        } else {
           glReadPixels(0,0,$w,$h,GL_RGB,GL_UNSIGNED_BYTE,$res->get_dataref);
        }

	return $res;
}

######################################################################
######################################################################
# EVENT HANDLER MINIPACKAGE FOLLOWS!

package PDL::Graphics::TriD::EventHandler;

BEGIN {
   use PDL::Config;
   if ( $PDL::Config{USE_POGL} ) {
      eval "use OpenGL $PDL::Config{POGL_VERSION} qw(ConfigureNotify MotionNotify ButtonPress ButtonRelease Button1Mask Button2Mask Button3Mask)";
      eval 'use PDL::Graphics::OpenGL::Perl::OpenGL';
   } else {
      eval 'use PDL::Graphics::OpenGL';
   }
}

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
#	 $retval = $this->reshape(@args);

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

BEGIN {
   use PDL::Config;
   if ( $PDL::Config{USE_POGL} ) {
      eval "use OpenGL $PDL::Config{POGL_VERSION} qw(:all)";
      eval 'use PDL::Graphics::OpenGL::Perl::OpenGL';
   } else {
      eval 'use PDL::Graphics::OpenGL';
   }
}

use PDL::Graphics::OpenGLQ;


sub highlight {
  my ($vp) = @_;

  my $pts =  new PDL [[0,0,0],
				  [$vp->{W},0,0],
				  [$vp->{W},$vp->{H},0],
				  [0,$vp->{H},0],
				  [0,0,0]];
  my $colors;

  $colors = PDL->ones(3,5);

  glDisable(GL_LIGHTING);
  
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0,$vp->{W},0,$vp->{H});
  glLineWidth(4);
  
  gl_line_strip($pts,$colors);

  glLineWidth(1);
		
  glEnable(GL_LIGHTING);

}



sub do_perspective {
	my($this) = @_;

	print "do_perspective ",$this->{W}," ",$this->{H} ,"\n" if($PDL::Graphics::TriD::verbose);

	if($PDL::Graphics::TriD::verbose>1){
	  my ($i,$package,$filename,$line);
          $i = 0;
	  do { 
	    ($package,$filename,$line) = caller($i++);
	    print "$package ($filename, line $line)\n";
	  } while($package);
	  print "\n";
	}
	      

        unless($this->{W}>0 and $this->{H}>0) {return;}
#	if($this->{W}==0 or $this->{H}==0) {return;}
	$this->{AspectRatio} = (1.0*$this->{W})/$this->{H};
#	glResizeBuffers();

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
