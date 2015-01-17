#!/usr/bin/perl 
#
#  PDL::Graphics::TriD::ButtonControl - This package simply defines 
#  default event handler subroutines.      $Revision$  
#
#  James P. Edwards
#  Instituto Nacional de Meteorologia
#  Brasilia, DF, Brasil
#  jedwards@inmet.gov.br  
#
#  This distribution is free software; you can
#  redistribute it and/or modify it under the same terms as Perl itself.
#  

=head1 NAME

PDL::Graphics::TriD::ButtonControl - default event handler subroutines

=head1 FUNCTIONS

=head2 new()

=for ref

Bless an oject into the class ButtonControl, expects the associated
Window object to be supplied as an argument.

=for usage

The ButtonControl class is a base class which all TriD event
controllers should inherit from.  By itself it does not do much.  It
defines ButtonPressed and ButtonRelease functions which are expected by
the Event loop.



=cut

package PDL::Graphics::TriD::ButtonControl;
use strict;
use fields qw/Win W H SC/;

sub new {
  my ($class,$win) = @_;
  
  my $self = fields::new($class);
  $self->{Win} = $win;

  $self;
}


=head2 mouse_moved

=for ref
  A do nothing function to prevent errors if not defined in a subclass

=cut

sub mouse_moved{
  print "mouse_moved @_\n" if $PDL::Graphics::TriD::verbose;
}

=head2 ButtonRelease

=for ref
  A do nothing function to prevent errors if not defined in a subclass

=cut

sub ButtonRelease{
  my ($this,$x,$y) = @_;
  $this->{Win}{Active} = 0;
  print "ButtonRelease @_\n"  if $PDL::Graphics::TriD::verbose;
}

=head2 ButtonPressed

=for ref

  Activates the viewport the mouse is inside when pressed

=cut

sub ButtonPress{
  my ($this,$x,$y) = @_;

  
#
# GL (0,0) point is Lower left X and Tk is upper left.
#
  $y = $PDL::Graphics::TriD::cur->{Height}-$y;

#  print "$x $y ",$this->{Win}{X0}," ",$this->{Win}{Y0}," ",$this->{Win}{W}," ",$this->{Win}{H},"\n";

  if($this->{Win}{X0} <= $x && $this->{Win}{X0}+$this->{Win}{W}>=$x 
	  && $this->{Win}{Y0} <= $y && $this->{Win}{Y0}+$this->{Win}{H}>=$y ){
	 $this->{Win}{Active} = 1;
  }
  print "ButtonPress @_ ",ref($this->{Win}),"\n" if $PDL::Graphics::TriD::verbose;
}

=head2 set_wh

=for ref

  Define the width and Height of the window for button control

=cut

sub set_wh {
  my($this,$w,$h) = @_;
  print ref($this)," $w,$h\n" if $PDL::Graphics::TriD::verbose;
  $this->{W} = $w; 
  $this->{H} = $h;
  $w = 0 unless defined $w;
  $h = 0 unless defined $h;
  if($w > $h) {
	 $this->{SC} = $h/2;
  } else {
	 $this->{SC} = $w/2;
  }
}


1;
