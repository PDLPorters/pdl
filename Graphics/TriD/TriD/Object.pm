###################################
#
#
package PDL::Graphics::TriD::Object;

use strict;

use fields qw(Objects ValidList ChangedSub List VRML);
sub new{
  my $class = shift;
  no strict 'refs';
  my $self = bless [ \%{"$class\::FIELDS"}], $class;
  $self;
}

sub clear_objects {
	my($this) = @_;
	$this->{Objects} = [];
	$this->{ValidList} = 0;
}


sub delete_object {
  my($this,$object) = @_;
  return unless(defined $object && defined $this->{Objects});
  for(0..$#{$this->{Objects}}){
    if($object == $this->{Objects}[$_]){
      splice(@{$this->{Objects}},$_,1);
      redo;
    }
  }
}

# XXXXXXXXX sub {} makes all these objects and this window immortal!
sub add_object {
  my($this,$object) = @_;
  push @{$this->{Objects}},$object;
  $this->{ValidList} = 0;
  for(@{$this->{ChangedSub}}) {
    $object->add_changedsub($_);
  }
  if($this->i_keep_list) {
    $object->add_changedsub(sub {$this->changed_from_above()});
  }
}

sub changed_from_above {
	my($this) = @_;
	print "CHANGED_FROM_ABOVE\n" if $PDL::Graphics::TriD::verbose;
	$this->changed();
}

sub add_changedsub {
	my($this,$chsub) = @_;
	push @{$this->{ChangedSub}}, $chsub;
	for (@{$this->{Objects}}) {
		$_->add_changedsub($chsub);
	}
}


sub clear {
	my($this) = @_;
	# print "Clear: $this\n";
	for(@{$this->{Objects}}) {
		$_->clear();
	}
	$this->delete_displist();
	delete $this->{ChangedSub};
	delete $this->{Objects};
}

sub changed {
	my($this) = @_;
	print "VALID0 $this\n" if $PDL::Graphics::TriD::verbose;
	$this->{ValidList} = 0;
	for(@{$this->{ChangedSub}}) {
		&$_($this);
	}
}

sub i_keep_list {
	return 0;
}


sub vrml_update {
  my ($this) = @_;
  use PDL::Graphics::VRML;

  $this->{VRML} = new PDL::Graphics::VRMLNode('Transform',
				   'translation' => "-1 -1 -1",
				   'scale' => "2 2 2");
  $this->{ValidList} = 1;
}

sub tovrml {
	my($this) = @_;

   print ref($this)," valid=",$this->{ValidList}," tovrml\n";

	if (!$this->{ValidList}) {
	  $this->vrml_update();
	}
	$this->{VRML}->add('children',
			   [map {$_->tovrml()} @{$this->{Objects}}]);
}


1;
