package PDL::Graphics::TriD::ButtonControl;

#
# This package simply defines default event handler subroutines that don't 
# do anything
#

sub new{
  my $class = shift;
  no strict 'refs';
  my $self = bless [ \%{"$class\::FIELDS"}], $class;
  $self;
}

sub mouse_moved{
  print "mouse_moved @_\n" if $PDL::Graphics::TriD::verbose;
}

sub ButtonRelease{
  print "ButtonRelease @_\n" if $PDL::Graphics::TriD::verbose;
}

sub ButtonPress{
  print "ButtonPress @_\n" if $PDL::Graphics::TriD::verbose;
}

1;
