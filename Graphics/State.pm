=head1 NAME

State - A package to keep track of plotting commands

=head1 SYNOPSIS

  use PDL::Graphics::State;

=head1 DESCRIPTION

This is a very simple, at present almost trivial, package to keep track
of the current set of plotting commands.

=head1 USAGE

You create a new object by calling the C<new> operator

  $state = PDL::Graphics::State->new();

Then for each new command you call C<add> on this object so that for a
call to C<line> of the form

  line $x, $y, $opt;

the call to C<add> would be like

  $state->add(\&line, 'line', [$x, $y], $opt);

which is stored internally as:

  [\&line, 'line', [$x, $y], $opt]

The state can later be extracted using C<get> which returns the state
object which is an array of anonymous arrays like the one above where
the first object is a reference to the function, the second an anomymous
array of arguments to the function and finally an anonymous hash with
options to the command.

If you know the order in which you inserted commands they can be removed
by calling C<remove> with the number in the stack. No further interaction
is implmented except C<clear> which clears the stack and C<copy> which
returns a "deep" copy of the state.



=head1 AUTHOR

Jarle Brinchmann (jarle@astro.ox.ac.uk) after some prodding by
Karl Glazebrook.

All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut

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
  my ($command, $command_name, $data, $opt) = @_;

  # Compact and not user-friendly storage.
  push @{$self->{Commands}}, [$command, $command_name, $data, $opt];

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

sub show {
  my $self = shift;
  my $count=0;
  foreach my $arg (@{$self->{Commands}}) {
    print "$count - Func=$$arg[1]\n";
    $count++;
  }

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
