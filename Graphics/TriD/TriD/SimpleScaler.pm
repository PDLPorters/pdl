##############################################################
#
# a very simple unsophisticated scaler that
# takes advantage of the nice infrastructure provided by
# TJL
#
##############################################################
package PDL::Graphics::TriD::SimpleScaler;

@ISA = qw/PDL::Graphics::TriD::ScaleController/;

# x,y to distance from center
sub xy2fac {
	my($this,$x0,$y0,$x1,$y1) = @_;
	my $dy = $y0-$y1;
	return $dy>0 ? 1+2*$dy : 1/(1-2*$dy);
}


package PDL::Graphics::TriD::ScaleController;

# use PDL::Quaternion;

sub new {my($type,$win,$dist) = @_;
	my $this = {
		Win => $win,
		Dist => $dist,
	};
	bless $this,$type;
	$win->add_resizecommand(sub {$this->set_wh(@_)});
	return $this;
}

sub set_wh {
	my($this,$w,$h) = @_;
	print "SCALESETWH: $w,$h\n" if $PDL::Graphics::TriD::verbose;
	$this->{W} = $w; $this->{H} = $h;
	if($w > $h) {
		$this->{SC} = $h/2;
	} else {
		$this->{SC} = $w/2;
	}
}

# coordinates normalised relative to center
sub xy2norm {
	my($this,$x,$y) = @_;
	$x -= $this->{W}/2; $y -= $this->{H}/2;
	$x /= $this->{SC}; $y /= $this->{SC};
	return ($x,$y);
}

sub mouse_moved {
	my($this,$x0,$y0,$x1,$y1) = @_;
	${$this->{Dist}} *=
           $this->xy2fac($this->xy2norm($x0,$y0),$this->xy2norm($x1,$y1));
}



1;
