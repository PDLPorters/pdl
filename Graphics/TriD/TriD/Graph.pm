package PDL::Graphics::TriD::Graph;

=head1 NAME

PDL::Graphics::TriD::Graph - PDL 3D graph object with axes

=head1 SYNOPSIS

  use PDL::Graphics::TriD;
  use PDL::Graphics::TriD::Graph;
  $g = PDL::Graphics::TriD::Graph->new;
  $g->default_axes;
  $g->add_dataseries(PDL::Graphics::TriD::Lattice->new($y,$c), "lat0");
  $g->bind_default("lat0");
  $g->add_dataseries(PDL::Graphics::TriD::LineStrip->new($y+pdl(0,0,1),$c), "lat1");
  $g->bind_default("lat1");
  $g->scalethings;
  $win = PDL::Graphics::TriD::get_current_window();
  $win->clear_objects;
  $win->add_object($g);
  $win->twiddle;

=cut

use strict;
use warnings;
use base qw/PDL::Graphics::TriD::Object/;
use PDL::LiteF; # XXX F needed?

use fields qw(Data DataBind UnBound DefaultAxes Axis );

sub add_dataseries {
  my($this,$data,$name) = @_;
  if(!defined $name) {
    $name = "Data0";
    while(defined $this->{Data}{$name}) {$name++;}
  }
  $this->{Data}{$name} = $data;
  $this->{DataBind}{$name} = [];
  $this->{UnBound}{$name} = 1;
  $this->add_object($data);
  $this->changed();
  return $name;
}

sub bind_data {
	my($this,$dser,$axes,$axis) = @_;
	push @{$this->{DataBind}{$dser}},[$axis,$axes];
	delete $this->{UnBound}{$dser};
	$this->changed();
}

sub bind_default {
	my($this,$dser,$axes) = @_;
	$axes //= $this->{DefaultAxes};
	$this->{DataBind}{$dser} = [['Default',$axes]];
	delete $this->{UnBound}{$dser};
}

sub set_axis {
	my($this,$axis,$name) = @_;
	$this->{Axis}{$name} = $axis;
	$this->changed();
}

# Bind all unbound things here...
sub scalethings {
	my($this) = @_;
	$this->bind_default($_) for keys %{$this->{UnBound}};
	$_->init_scale() for values %{$this->{Axis}};
	my ($k,$v);
	while(($k,$v) = each %{$this->{DataBind}}) {
		$this->{Axis}{$_->[0]}->add_scale($this->{Data}{$k}->get_points(), $_->[1]) for @$v;
	}
	$_->finish_scale() for values %{$this->{Axis}};
}

sub get_points {
	my($this,$name) = @_;
# 	print Dumper($this->{Axis});
	my $d = $this->{Data}{$name}->get_points();
	my @ddims = $d->dims; shift @ddims;
	my $p = PDL->zeroes(PDL::float(),3,@ddims);
	my $pnew;
	for(@{$this->{DataBind}{$name}}) {
		defined($this->{Axis}{$_->[0]}) or die("Axis not defined: $_->[0]");
# Transform can return the same or a different ndarray.
		$p = $pnew = $this->{Axis}{$_->[0]}->transform($p,$d,$_->[1]);
	}
	return $pnew;
}

sub clear_data {
	my($this) = @_;
	$this->{$_} = {} for qw(Data DataBind UnBound);
	$this->changed;
}

sub delete_data {
	my($this,$name) = @_;
	delete $this->{$_}{$name} for qw(Data DataBind UnBound);
	$this->changed;
}

sub default_axes {
	my($this) = @_;
	$this->set_axis(PDL::Graphics::TriD::EuclidAxes->new(),"Euclid3");
	$this->set_default_axis("Euclid3",[0,1,2]);
}

sub set_default_axis {
	my($this,$name,$axes) = @_;
	$this->{Axis}{Default} = $this->{Axis}{$name};
	$this->{DefaultAxes} = $axes;
}

sub changed {}

package PDL::Graphics::TriD::EuclidAxes;

sub new {
  my($type) = @_; bless {Names => [qw(X Y Z)]},$type;
}

sub init_scale {
  my($this) = @_;
  $this->{Scale} = undef;
}

sub add_scale {
  my($this,$data,$inds) = @_;
  $data = $data->dice_axis(0, $inds);
  my $to_minmax = $data->clump(1..$data->ndims-1); # xyz,...
  $to_minmax = $to_minmax->glue(1, $this->{Scale}); # include old min/max
  my ($mins, $maxes) = $to_minmax->transpose->minmaxover; # each is xyz
  $this->{Scale} = PDL->pdl($mins, $maxes); # xyz,minmax
}

sub finish_scale {
  my($this) = @_;
# Normalize the smallest differences away.
  my ($min, $max) = $this->{Scale}->dog;
  my $diff = $max - $min;
  my ($got_smalldiff, $got_bigdiff) = PDL::which_both(abs($diff) < 1e-6);
  $max->dice_axis(0, $got_smalldiff) .= $min->dice_axis(0, $got_smalldiff) + 1;
  my ($min_big, $max_big, $shift) = map $_->dice_axis(0, $got_bigdiff), $min, $max, $diff;
  $shift = $shift * 0.05; # don't mutate
  $min_big -= $shift, $max_big += $shift;
}

# Add 0..1 to each axis.
sub transform {
  my($this,$point,$data,$inds) = @_;
  my ($min, $max) = map $this->{Scale}->slice("0:$#$inds,$_"), 0, 1;
  (my $tmp = $point->slice("0:$#$inds")) +=
    ($data->dice_axis(0, $inds) - $min) / ($max - $min);
  return $point;
}

# projects from the sphere to a cylinder
package PDL::Graphics::TriD::CylindricalEquidistantAxes;
use PDL::Core '';

sub new {
	my($type) = @_; 
	bless {Names => [qw(LON LAT Pressure)]},$type;
}

sub init_scale {
	my($this) = @_;
	$this->{Scale} = [];
}

sub add_scale {
  my($this,$data,$inds) = @_;
  my $i = 0;
  for(@$inds) {
    my $d = $data->slice("($_)");
    my $max = $d->max->sclr;
    my $min = $d->min->sclr;
    if($i==1){
      if($max > 89.9999 or $min < -89.9999){
	barf "Error in Latitude $max $min\n";
      }
    }
    elsif($i==2){
      $max = 1012.5 if($max<1012.5);
      $min = 100 if($min>100);
    }
    if(!defined $this->{Scale}[$i]) {
      $this->{Scale}[$i] = [$min,$max];
    } else {
      if($min < $this->{Scale}[$i][0]) {
	$this->{Scale}[$i][0] = $min;
      }
      if($max > $this->{Scale}[$i][1]) {
	$this->{Scale}[$i][1] = $max;
      }
    }
    $i++;
  }
# Should make the projection center an option
  $this->{Center} = [$this->{Scale}[0][0]+($this->{Scale}[0][1]-$this->{Scale}[0][0])/2,
		     0];
}

sub finish_scale {
  my($this) = @_;
  my @dist;
  # Normalize the smallest differences away.
  for(@{$this->{Scale}}) {
    if(abs($_->[0] - $_->[1]) < 0.000001) {
      $_->[1] = $_->[0] + 1;
    } 
    push(@dist,$_->[1]-$_->[0]);
  }
  # for the z coordinate reverse the min and max values
  my $max = $this->{Scale}[2][0];
  if($max < $this->{Scale}[2][1]){
    $this->{Scale}[2][0] = $this->{Scale}[2][1];
    $this->{Scale}[2][1] = $max;
  }
# Normalize longitude and latitude scale
  if($dist[1] > $dist[0]){
    $this->{Scale}[0][0] -= ($dist[1]-$dist[0])/2;
    $this->{Scale}[0][1] += ($dist[1]-$dist[0])/2;
  }elsif($dist[0] > $dist[1] && $dist[0]<90){
    $this->{Scale}[1][0] -= ($dist[0]-$dist[1])/2;
    $this->{Scale}[1][1] += ($dist[0]-$dist[1])/2;
  }elsif($dist[0] > $dist[1]){
    $this->{Scale}[1][0] -= (90-$dist[1])/2;
    $this->{Scale}[1][1] += (90-$dist[1])/2;
  }    
}

sub transform {
  my($this,$point,$data,$inds) = @_;
  my $i = 0;
  if($#$inds!=2){
    barf("Wrong number of arguments to transform $this\n");
    exit;
  }
  my $pio180 = 0.017453292;
  (my $tmp1 = $point->slice("(0)")) +=
    0.5+($data->slice("($inds->[0])")-$this->{Center}[0]) /
      ($this->{Scale}[0][1] - $this->{Scale}[0][0])
	*cos($data->slice("($inds->[1])")*$pio180);
  (my $tmp2 = $point->slice("(1)")) +=
    0.5+($data->slice("($inds->[1])")-$this->{Center}[1]) /
      ($this->{Scale}[1][1] - $this->{Scale}[1][0]);
  (my $tmp3 = $point->slice("(2)")) .=
    log($data->slice("($inds->[2])")/1012.5)/log($this->{Scale}[2][1]/1012.5);
  return $point;
}

package PDL::Graphics::TriD::PolarStereoAxes;
use PDL::Core '';

sub new {
	my($type) = @_; 
	bless {Names => [qw(LONGITUDE LATITUDE HEIGHT)]},$type;
}

sub init_scale {
	my($this) = @_;
	$this->{Scale} = [];
}

sub add_scale {
  my($this,$data,$inds) = @_;
  my $i = 0;
  for(@$inds) {
    my $d = $data->slice("($_)");
    my $max = $d->max->sclr;
    my $min = $d->min->sclr;
    if($i==1){
      if($max > 89.9999 or $min < -89.9999){
	barf "Error in Latitude $max $min\n";
      }
    }
    elsif($i==2){
      $max = 1012.5 if($max<1012.5);
      $min = 100 if($min>100);
    }
    if(!defined $this->{Scale}[$i]) {
      $this->{Scale}[$i] = [$min,$max];
    } else {
      if($min < $this->{Scale}[$i][0]) {
	$this->{Scale}[$i][0] = $min;
      }
      if($max > $this->{Scale}[$i][1]) {
	$this->{Scale}[$i][1] = $max;
      }
    }
    $i++;
  }
  $this->{Center} = [$this->{Scale}[0][0]+($this->{Scale}[0][1]-$this->{Scale}[0][0])/2,
		     $this->{Scale}[1][0]+($this->{Scale}[1][1]-$this->{Scale}[1][0])/2];
}

sub finish_scale {
  my($this) = @_;
  my @dist;
  # Normalize the smallest differences away.
  for(@{$this->{Scale}}) {
    if(abs($_->[0] - $_->[1]) < 0.000001) {
      $_->[1] = $_->[0] + 1;
    } 
    push(@dist,$_->[1]-$_->[0]);
  }
  # for the z coordinate reverse the min and max values
  my $max = $this->{Scale}[2][0];
  if($max < $this->{Scale}[2][1]){
    $this->{Scale}[2][0] = $this->{Scale}[2][1];
    $this->{Scale}[2][1] = $max;
  }
# Normalize longitude and latitude scale
  if($dist[1] > $dist[0]){
    $this->{Scale}[0][0] -= ($dist[1]-$dist[0])/2;
    $this->{Scale}[0][1] += ($dist[1]-$dist[0])/2;
  }elsif($dist[0] > $dist[1] && $dist[0]<90){
    $this->{Scale}[1][0] -= ($dist[0]-$dist[1])/2;
    $this->{Scale}[1][1] += ($dist[0]-$dist[1])/2;
  }elsif($dist[0] > $dist[1]){
    $this->{Scale}[1][0] -= (90-$dist[1])/2;
    $this->{Scale}[1][1] += (90-$dist[1])/2;
  }    
}

sub transform {
  my($this,$point,$data,$inds) = @_;
  my $i = 0;
  if($#$inds!=2){
    barf("Wrong number of arguments to transform $this\n");
    exit;
  }
  my $pio180 = 0.017453292;
  (my $tmp1 = $point->slice("(0)")) +=
    0.5+($data->slice("($inds->[0])")-$this->{Center}[0]) /
      ($this->{Scale}[0][1] - $this->{Scale}[0][0])
	*cos($data->slice("($inds->[1])")*$pio180);
  (my $tmp2 = $point->slice("(1)")) +=
    0.5+($data->slice("($inds->[1])")-$this->{Center}[1]) /
      ($this->{Scale}[1][1] - $this->{Scale}[1][0])
	*cos($data->slice("($inds->[1])")*$pio180);
# Longitude transformation
#  (my $tmp = $point->slice("(0)")) =
#    ($this->{Center}[0]-$point->slice("(0)"))*cos($data->slice("(1)"));
# Latitude transformation
#  (my $tmp = $point->slice("(1)")) =
#    ($this->{Center}[1]-$data->slice("(1)"))*cos($data->slice("(1)"));
# Vertical transformation
#  -7.2*log($data->slice("(2)")/1012.5
  (my $tmp3 = $point->slice("(2)")) .=
    log($data->slice("($inds->[2])")/1012.5)/log($this->{Scale}[2][1]/1012.5);
  return $point;
}

1;
