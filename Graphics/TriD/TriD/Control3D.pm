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
use warnings;
use fields qw/WOrigin WRotation CDistance CRotation/;

sub new{
  my ($class) = @_;
  
  my $self = fields::new($class);
  $self->reset();

  $self;
}

sub normalize { my($this) = @_;
	$this->{WRotation}->normalize_this();
	$this->{CRotation}->normalize_this();
}

sub reset { 
  my($this) = @_;
  $this->{WOrigin}   = [0.7,0.7,0.5];
  $this->{WRotation} = PDL::Graphics::TriD::Quaternion->new(
		0.718, -0.609, -0.193, -0.281); # isometric-ish like gnuplot
  $this->{CDistance} = 3;
  $this->{CRotation} = PDL::Graphics::TriD::Quaternion->new(1,0,0,0);
}

sub set {
  my($this,$options) = @_;

  foreach my $what (keys %$options){
	 if($what =~ /Rotation/){
		$this->{$what}[0] = $options->{$what}[0];
		$this->{$what}[1] = $options->{$what}[1];
		$this->{$what}[2] = $options->{$what}[2];
		$this->{$what}[3] = $options->{$what}[3];
	 }elsif($what eq 'WOrigin'){
		$this->{$what}[0] = $options->{$what}[0];
		$this->{$what}[1] = $options->{$what}[1];
		$this->{$what}[2] = $options->{$what}[2];
	 }else{
		$this->{$what} = $options->{$what};
	 }
  }
}

1;
