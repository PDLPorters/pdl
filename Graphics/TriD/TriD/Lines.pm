
package PDL::Graphics::TriD::LinesFOOOLD;
@ISA=qw/PDL::Graphics::TriD::Object/;
use PDL::Graphics::OpenGL;
use PDL::Lite;

sub new {
	my($type,$x,$y,$z,$color) = @_;
	my @xdims = $x->dims;
	$color = PDL->pdl(1) if !defined $color;
	my $this = {
		X => $x, Y => $y, Z => $z,
		Color => $color,
	};
	bless $this,$type;
}

sub get_boundingbox {
	my ($this) = @_;
	my (@mins,@maxs);
	for (X,Y,Z) {
		push @mins, $this->{$_}->min();
		push @maxs, $this->{$_}->max();
	}
	print "LineBound: ",(join ',',@mins,@maxs),"\n";
	return PDL::Graphics::TriD::BoundingBox->new( @mins,@maxs );
}

# XXX Color is ignored.
sub togl {
	my($this) = @_;
	glDisable(GL_LIGHTING);
	glBegin(&GL_LINE_STRIP);
	my $first = 1;
	PDL::threadover_n($this->{X},$this->{Y},$this->{Z},$this->{Color},sub {
		if(shift > 0) {
			if(!$first) {
			glEnd();
			glBegin(&GL_LINE_STRIP);
			} else {$first = 0;}
		}
		my $color = pop @_;
		glColor3f($color,0,1-$color);
		glVertex3d(@_);
#		print "VERTEX: ",(join ",",@_),"\n";
	}) ;
	glEnd();
	glEnable(GL_LIGHTING);
}

1;
