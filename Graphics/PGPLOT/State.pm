package PDL::Graphics::State;

#
# This is a very simple package to deal with the graphics state.
#

sub new {
  my $type = shift;

  my $self = {
	      'Commands' => [],
	     };

  bless $self, ref($type) || $type;
  return $self;
}


sub DESTROY {
  my $self = shift;
  $self->clear();
}

sub add {
  my $self = shift;
  # The command is a reference to the subroutine, the data is an
  # anonymous array containing the data passed to the routine and
  # opt is the options hash PASSED TO THE ROUTINE..
  my ($command, $data, $opt) = @_;

  # Compact and not user-friendly storage.
  push @{$self->{Commands}}, [$command, $data, $opt];

#  return $#{$self->{Commands}}+1;
}


sub remove {
  my $self = shift;
  my $num = shift;

  # Remove entry #1
  splice @{$self->{Commands}}, $num, 1;
}

sub get {
  my $self = shift;
  return @{$self->{Commands}};
}

sub info {
  my $self = shift;
  print "The state has ".($#{$self->{Commands}}+1)." commands in the stack\n";
}

sub clear {
  my $self = shift;
  # Do I need to do more?
  $self->{Commands}=[];
}


sub copy {
  my $self = shift;
  my $new = PDL::Graphics::State->new();
  foreach my $arg (@{$self->{Commands}}) {
    $new->add(@$arg);
  }
  return $new;
}

1;
