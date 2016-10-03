######################################################################
######################################################################
## ScrollButtonController -- this is the module that enables support
## for middle button scrollwheels to zoom in and out of the display
## window.  Scrolling forward zooms in, while scrolling backwards zooms
## out.

package PDL::Graphics::TriD::ScrollButtonController;
use base qw/PDL::Graphics::TriD::ButtonControl/;
use fields qw/Dist Zoom/;

sub new {
  my($type,$win,$dist,$zoom) = @_;

  my $this = $type->SUPER::new($win);

  $this->{Dist} = $dist;
  $this->{Zoom} = $zoom;  # multiplier for zooming
                          # >1 zooms out, <1 zooms in
  return $this;
}

sub ButtonRelease{
  my ($this,$x,$y) = @_;
  $this->{Dist} *= $this->{Zoom};
  print "ButtonRelease @_\n"  if $PDL::Graphics::TriD::verbose;
}

sub ButtonPress{
  my ($this,$x,$y) = @_;
  print "ButtonPress @_ ",ref($this->{Win}),"\n" if $PDL::Graphics::TriD::verbose;
}

1;
