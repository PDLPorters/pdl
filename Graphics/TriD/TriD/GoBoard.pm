
# Some 3D/2D representations of Go Boards.
# this may not be of great interest to people who don't play Go
# except in some strange visualization senses.

# Also Go players will not find this to look too much like a real go board.

package PDL::Graphics::TriD::GoBoard;

use base qw/PDL::Graphics::TriD::Object/;
use fields qw /Data InLays BG/;
use PDL::Graphics::OpenGL;
use PDL::Lite;


sub new {
	my($type,$opts) = @_;
	my $this = $type->SUPER::new();
	$this->{Data} = $opts->{Data};

	my $d = $opts->{Data};
	my $eo = ($d->slice("(3)")+0.000005) /
		($d->slice("(2)") + $d->slice("(3)") + 0.00001);
	$this->{BG} = new PDL::Graphics::TriD::Image([$eo*0, $eo, 0*$eo]);
	return $this;
}

sub add_inlay {
	my($this,$data,$x,$y,$z) = @_;
	push @{$this->{InLays}},[$z,(new PDL::Graphics::TriD::GoBoard({Data => $data})),
		$x,$y, $data->dims];
}

sub togl {
	my($this) = @_;
#	my $z = 0.5;
#	my $z = 0.001;
	my $z = 0.01;
	print "BOARD2GL\n" if $PDL::Graphics::TriD::verbose;
# 0 = white, 1 = black, 2 = outside, 3 = empty.
	my $d = $this->{Data};
	$this->{BG}->togl();
	glDisable(GL_LIGHTING);
# 1. stones.
	my $hass = $d->slice("(0)") + $d->slice("(1)");
	my $stoc = $d->slice("(0)") / ($hass+0.00001);
	my ($x,$y);
	my ($foo,$nx,$ny) = ($this->{Data}->dims);
	my $xs = 0.5/$nx; my $ys = 0.5/$ny;
	glBegin(GL_QUADS);
	for $x (0..$nx-1) {
		for $y (0..$ny-1) {
			my $c = $stoc->at($x,$y);
			my $s = $hass->at($x,$y);
			my $cx = ($x+0.5)/$nx;
			my $cy = ($y+0.5)/$ny;
#			glColor3f($c,$c,$c);
			glColor3f($c,0.3,1-$c);
			glVertex3d($cx+$s*$xs,$cy,$z);
			glVertex3d($cx,$cy+$s*$ys,$z);
			glVertex3d($cx-$s*$xs,$cy,$z);
			glVertex3d($cx,$cy-$s*$ys,$z);
		}
	}
	glEnd();
	for (@{$this->{InLays}}) {
		glEnable(&GL_DEPTH_TEST);
		glPushMatrix();
		glTranslatef($xs*2*$_->[2],$ys*2*$_->[3],
			$_->[0]);
		my $z = -$_->[0];
		glScalef($_->[5]/$nx,$_->[6]/$ny,1);
		$_->[1]->togl();
		glDisable(&GL_LIGHTING);
		glColor3d(1,1,1);
		glBegin(&GL_LINES);
		glVertex3d(0,0,0);
		glVertex3d(0,0,$z);
		glVertex3d(0,1,0);
		glVertex3d(0,1,$z);
		glVertex3d(1,0,0);
		glVertex3d(1,0,$z);
		glVertex3d(1,1,0);
		glVertex3d(1,1,$z);
		glEnd();
		glPopMatrix();
	}
	glEnable(&GL_LIGHTING);
}
1;
