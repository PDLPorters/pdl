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
use fields qw/WOrigin WRotation CDistance CRotation/;

sub new{
  my ($class) = @_;
  
  no strict 'refs';
  my $self = bless [ \%{"$class\::FIELDS"}], $class;
  $self->reset();

  return $self;
}

sub normalize { my($this) = @_;
	$this->{WRotation}->normalize_this();
	$this->{CRotation}->normalize_this();
}

sub reset { 
  my($this) = @_;
  $this->{WOrigin}   = [0,0,0];
  $this->{WRotation} = PDL::Graphics::TriD::Quaternion->new(1,0,0,0);
#	$this->{WRotation} = PDL::Graphics::TriD::Quaternion->new(
#		0.847, -0.458, -0.161, -0.216);
#	$this->{WRotation} = PDL::Graphics::TriD::Quaternion->new(
#		0.347, -0.458, -0.161, -0.216);

  $this->{CDistance} = 5;
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
