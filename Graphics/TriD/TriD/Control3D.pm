package PDL::Graphics::TriD::Control3D;

# Mustn't have empty package in some perl versions.

##############################################
#
# A quaternion-based controller framework with the following transformations:
#   1. world "origin". This is what the world revolves around
#   2. world "rotation" at origin.
#   3. camera "distance" along z axis after that (camera looks
#	at negative z axis).
#   4. camera "rotation" after that (not always usable).

package PDL::Graphics::TriD::SimpleController;
use strict;

sub new {
	my($type) = @_;
	my $this = bless {
	},$type;
	$this->reset();
	return $this;
}

sub normalize { my($this) = @_;
	$this->{WRotation}->normalize_this();
	$this->{CRotation}->normalize_this();
}

sub reset { my($this) = @_;
	$this->{WOrigin}   = [0,0,0];
	$this->{WRotation} = PDL::Graphics::TriD::Quaternion->new(1,0,0,0);
	$this->{WRotation} = PDL::Graphics::TriD::Quaternion->new(
		0.847, -0.458, -0.161, -0.216);
	$this->{WRotation} = PDL::Graphics::TriD::Quaternion->new(
		0.347, -0.458, -0.161, -0.216);
	$this->{CDistance} = 5;
	$this->{CRotation} = PDL::Graphics::TriD::Quaternion->new(1,0,0,0);
}

1;
