
package PDL::Graphics::TriD::Graph;
@ISA=qw/PDL::Graphics::TriD::Object/;
use PDL::LiteF; # XXX F needed?

sub new {
	my($type) = @_;
	bless {},$type;
}

sub add_dataseries {
	my($this,$data,$name) = @_;
	if(!defined $name) {
		$name = "Data0";
		while(defined $this->{Data}{$name}) {$name++;}
	}
	$this->{Data}{$name} = $data;
	$this->{DataBind}{$name} = [];
	$this->add_object($data);
	$this->changed();
	$this->{UnBound}{$name} = 1;
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
		$_->init_scale();
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
	$this->{Data} = {}; $this->{DataBind} = {};
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

