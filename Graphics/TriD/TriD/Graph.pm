package PDL::Graphics::TriD::Graph;
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
	if(!defined $axes) {$axes = $this->{DefaultAxes}};
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
	for(keys %{$this->{UnBound}}) {
		$this->bind_default($_);
	}
	for(values %{$this->{Axis}}) {
	  $_->init_scale() ;
	}
	my ($k,$v);
	while(($k,$v) = each %{$this->{DataBind}}) {
		for(@$v) {
			$this->{Axis}{$_->[0]}->add_scale(
				$this->{Data}{$k}->get_points(), $_->[1]);
		}
	}
	for(values %{$this->{Axis}}) {
		$_->finish_scale();
	}
}

# use Data::Dumper;
sub get_points {
	my($this,$name) = @_;
# 	print Dumper($this->{Axis});

	my $d = $this->{Data}{$name}->get_points();


	my @ddims = $d->dims; shift @ddims;
	my $p = PDL->zeroes(&PDL::float(),3,@ddims);
	my $pnew;
	for(@{$this->{DataBind}{$name}}) {
		defined($this->{Axis}{$_->[0]}) or die("Axis not defined: $_->[0]");
# Transform can return the same or a different piddle.
		$pnew = $this->{Axis}{$_->[0]}->transform($p,$d,$_->[1]);
		$p = $pnew;
	}
	return $pnew;
}

sub clear_data {
	my($this) = @_;
	$this->{Data} = {};
	$this->{DataBind} = {};
	$this->{UnBound} = {};
	$this->changed();
}

sub delete_data {
	my($this,$name) = @_;
	delete $this->{Data}{$name};
	delete $this->{DataBind}{$name};
	delete $this->{UnBound}{$name};
	$this->changed();
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
	my($type) = @_; bless {Names => [X,Y,Z]},$type;
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
		my $max = $d->max;
		my $min = $d->min;
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
}

sub finish_scale {
	my($this) = @_;
# Normalize the smallest differences away.
	for(@{$this->{Scale}}) {
		if(abs($_->[0] - $_->[1]) < 0.000001) {
			$_->[1] = $_->[0] + 1;
		} else {
			my $shift = ($_->[1]-$_->[0])*0.05;
			$_->[0] -= $shift;
			$_->[1] += $shift;
		}
	}
}

# Add 0..1 to each axis.
sub transform {
	my($this,$point,$data,$inds) = @_;
	my $i = 0;
	for(@$inds) {
		(my $tmp = $point->slice("($i)")) +=
		  ($data->slice("($_)") - $this->{Scale}[$i][0]) /
		  ($this->{Scale}[$i][1] - $this->{Scale}[$i][0]) ;
		$i++;
	}
	return $point;
}


#
# projects from the sphere to a cylinder
# 

package PDL::Graphics::TriD::CylindricalEquidistantAxes;
use PDL::Core '';


sub new {
	my($type) = @_; 
	bless {Names => [LON,LAT,Pressure]},$type;
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
    my $max = $d->max;
    my $min = $d->min;



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
  
#  $this->{Center} = [$this->{Scale}[0][0]+($this->{Scale}[0][1]-$this->{Scale}[0][0])/2,
#		     $this->{Scale}[1][0]+($this->{Scale}[1][1]-$this->{Scale}[1][0])/2];
#
# Should make the projection center an option
#
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
  # for the z coordiniate reverse the min and max values
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
	bless {Names => [LONGITUDE,LATITUDE,HEIGHT]},$type;
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
    my $max = $d->max;
    my $min = $d->min;

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
  # for the z coordiniate reverse the min and max values
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
