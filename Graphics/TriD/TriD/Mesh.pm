##############################################
#
# Given 2D data, make a mesh.
#
# This is only for a set of rectangular meshes.
#
# Use PDL::Graphics::TriD::Surface for more general stuff.
#
# I try to make this general enough that all Surface methods
# work on this data also.
#

# The different types of normals are a headache.
# If we have a normal per vertex, we get smoothing.
# If we want flat shading, ... need normal / square or triangle.

# But: if we have
# normal(3, polygons, vertices, @overdims),
# we could possibly map it for both cases.
# Flat:   normal(3,polygon, (),     @)
# Smooth: normal(3,(),      vertex, @)

package PDL::Graphics::TriD::Mesh;
use PDL::Graphics::OpenGL;
use PDL::LiteF;
@ISA=qw/PDL::Graphics::TriD::Object/;

# For now, x and y coordinates = values.

sub new {
	my($type,$data,$xaxis,$yaxis) = @_;
	my @dims = $data->dims;
	my @xydims = splice @dims,0,2;
	my @overdims = @dims;
	my $this = {
		Vertices => PDL->zeroes(3,@xydims,@overdims)->double,
		XYDims => [@xydims],
		OverDims => [@overdims],
		Data => $data,
	};
	PDL::Primitive::axisvalues($this->{Vertices}->slice('(0),:,:'));
	PDL::Primitive::axisvalues($this->{Vertices}->slice('(1),:,:')->xchg(0,1));
	PDL::Ops::assgn($this->{Data},$this->{Vertices}->slice('(2),:,:'));
	bless $this,$type;
}

sub printdims {print $_[0].": ".(join ', ',$_[1]->dims)," and ",
		(join ', ',$_[1]->threadids),"\n"}

sub get_boundingbox {
	my($this) = @_;
	my $foo = PDL->zeroes(6)->double;
	$a = $this->{Vertices}; printdims "A",$a;
	$b = $a->thread(0); printdims "B",$b;
	$c = $b->clump(-1); printdims "C",$c;
	$d = $c->unthread(1); printdims "D",$d;

	$this->{Vertices}->thread(0);

	PDL::Primitive::minimum($this->{Vertices}->thread(0)->clump(-1)->unthread(1),
		 $foo->slice('0:2'));
	PDL::Primitive::maximum($this->{Vertices}->thread(0)->clump(-1)->unthread(1),
		 $foo->slice('3:5'));
	print "MeshBound: ",(join ',',$foo->list()),"\n";
	return PDL::Graphics::TriD::BoundingBox->new( $foo->list() );
}

sub normals_flat {
	my($this) = @_;
	$this->_allocnormalspoly();
	my $v00 = $this->{Vertices}->slice('(2),0:-2,0:-2');
	my $v01 = $this->{Vertices}->slice('(2),0:-2,1:-1');
	my $v10 = $this->{Vertices}->slice('(2),1:-1,0:-2');
	my $v11 = $this->{Vertices}->slice('(2),1:-1,1:-1');
#	$this->{Normals}->printdims("NORMALS");
	my $nx = $this->{Normals}->slice('(0),:,:,(0),(0)');
#	$nx->printdims("NX"); $v00->printdims("V0");
	$nx *= PDL->pdl(0);
	$nx += $v11; $nx -= $v01; $nx += $v10; $nx -= $v00;
	$nx *= PDL->pdl(-0.5);
	my $ny = $this->{Normals}->slice('(1),:,:,(0),(0)');
	$ny *= PDL->pdl(0);
	$ny += $v11; $ny -= $v10; $ny += $v01; $ny -= $v00;
	$ny *= PDL->pdl(-0.5);
	my $nz = $this->{Normals}->slice('(2),:,:,(0),(0)');
	$nz .= PDL->pdl(1);
	print $this->{Vertices};
	print $this->{Normals};
	print $nx,$ny,$nz;
}

sub _allocnormalspoly {
	my($this) = @_;
	$this->{Normals} = (PDL->zeroes(3, (map {$_-1} @{$this->{XYDims}}),
						@{$this->{OverDims}})->double )
			-> dummy(3,$this->{XYDims}[1])
			-> dummy(3,$this->{XYDims}[0]);
}

sub _allocnormalsvertex {
	my($this) = @_;
	$this->{Normals} = (double zeroes(3, (@{$this->{XYDims}}),
						@{$this->{OverDims}}))
			-> dummy(1,$this->{XYDims}[1]-1)
			-> dummy(1,$this->{XYDims}[0]-1);
}

# Right now, I assume the flat model.
sub togl {
	my($this) = @_;
	my ($x,$y);
#	forextradims ([$this->{Vertices},_],
	for $x (0..$this->{XYDims}[0]-2) {
		for $y (0..$this->{XYDims}[1]-2) {
			my ($x1,$y1) = ($x+1,$y+1);
			glBegin(GL_TRIANGLE_STRIP);
			print "ONESTRIP\n", (join '',
				$this->{Normals}->slice(":,($x),($y),($x),($y)")
				),"\n";
			glNormal3d(
				$this->{Normals}->slice(":,($x),($y),($x),($y)")
					->list());
#			print "VERTEX0: ",(join ',',
#				$this->{Vertices}->slice(":,($x),($y)")->list()),
#				"\n";
			glVertex3d($this->{Vertices}->slice(":,($x),($y)")->list());
			glVertex3d($this->{Vertices}->slice(":,($x1),($y)")->list());
			glVertex3d($this->{Vertices}->slice(":,($x),($y1)")->list());
			glVertex3d($this->{Vertices}->slice(":,($x1),($y1)")->list());
			glEnd();
# Show normal
#			glBegin(&GL_LINES);
#			glVertex3d($this->{Vertices}->slice(":,($x),($y)")->list());
#			glVertex3d(
#			  ($this->{Vertices}->slice(":,($x),($y)") +
#				$this->{Normals}->slice(":,($x),($y),($x),($y)"))
#				 ->list());
#			glEnd();
		}
	}
}

package PDL::Graphics::TriD;
use PDL::Graphics::OpenGL;
use PDL::Core '';

sub pdltotrianglemesh {
	my($pdl,$x0,$x1,$y0,$y1) = @_;
	if($#{$pdl->{Dims}} != 1) { barf "Too many dimensions for PDL::GL::Mesh: $#{$pdl->{Dims}}  \n"; }
	my ($d0,$d1); my($x,$y);
	$xincr = ($x1 - $x0) / ($pdl->{Dims}[0]-1.0);
	$yincr = ($y1 - $y0) / ($pdl->{Dims}[1]-1.0);
	$x = $x0;
	my($v00,$v01,$v11,$v10);
	my($nx,$ny);
	for $d0 (0..$pdl->{Dims}[0]-2) {
		$y = $y0;
		for $d1 (0..$pdl->{Dims}[1]-2) {
			glBegin(GL_TRIANGLE_STRIP);
			($v00,$v01,$v11,$v10) =
			  (PDL::Core::at($pdl,$d0,$d1)
			  ,PDL::Core::at($pdl,$d0,$d1+1)
			  ,PDL::Core::at($pdl,$d0+1,$d1+1)
			  ,PDL::Core::at($pdl,$d0+1,$d1));

			($nx,$ny) = (-0.5*($v11+$v10-$v01-$v00)/$xincr,
			          -0.5*($v11-$v10+$v01-$v00)/$yincr);
			glNormal3d($nx,$ny,1);
			glVertex3d($x,$y,$v00);
			glVertex3d($x+$xincr,$y,$v10);
			glVertex3d($x,$y+$yincr,$v01);
			glVertex3d($x+$xincr,$y+$yincr,$v11);
			glEnd();
			if(0) {
				glBegin(GL_LINES);
				glVertex3d($x,$y,$v00);
				glVertex3d($x+$nx/10,$y+$ny/10,$v00+1/10);
				glEnd();
			}
			$y += $yincr;
		}
		$x += $xincr;
	}
}

sub pdl2normalizedmeshlist {
	my($pdl) = @_;
	my $mult = 1.0/($pdl->{Dims}[0]-1);
	my $lno = glGenLists(1);
	glNewList($lno,GL_COMPILE);
	pdltotrianglemesh($pdl, 0, 1, 0, ($pdl->{Dims}[1]-1)*$mult);
	glEndList();
	return $lno;
}


1;
