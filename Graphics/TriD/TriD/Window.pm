#
# The PDL::Graphics::TriD::Window is already partially defined in
# the appropriate gdriver (GL or VRML) items defined here are common
# to both
# 
package PDL::Graphics::TriD::Window;

use Data::Dumper;
use strict;

sub new {
  my($arg,$options) = @_;

  my $this = $arg->SUPER::new();

  # Make sure the Graphics has been initialized
  $options->{width} = 	800 unless defined $options->{width};
  $options->{height} = 	800 unless defined $options->{height};
  $this->{Width} = $options->{width};
  $this->{Height} = $options->{height};

  $this->{Interactive} = $this->gdriver($options);

  # set default values
  if($this->{Interactive}){
	 $this->{Ev} = $this->ev_defaults(); 
	 $this->new_viewport(0,0,$this->{Width},$this->{Height});  
  }else{
	 $this->new_viewport(0,0,1,1);  
  }
  $this->current_viewport(0);

  return($this);
}

#
# adds to all viewports
#
sub add_object {
  my($this,$object) = @_;
#  print "add_object ",ref($this),"\n";

  for(@{$this->{_ViewPorts}}) {
	 $_->add_object($object);
  }
}



sub new_viewport {
  my($this,$x0,$y0,$x1,$y1, $options) = @_;
  my $vp = new PDL::Graphics::TriD::ViewPort($x0,$y0,$x1,$y1);
#
  print "Adding viewport $x0,$y0,$x1,$y1\n";
  push @{$this->{_ViewPorts}}, $vp;
#

  if($this->{Interactive} ){
	 # set a default controller
	 use PDL::Graphics::TriD::ArcBall;
	 use PDL::Graphics::TriD::SimpleScaler;
	 use PDL::Graphics::TriD::Control3D;

	 my $ev = $options->{EHandler};
	 $ev = new PDL::Graphics::TriD::EventHandler($vp) unless defined($ev);
	 my $cont = $options->{Transformer};
	 $cont = new PDL::Graphics::TriD::SimpleController() unless defined($cont);

	 $vp->transformer($cont);
    if(ref($ev)){
		$ev->set_button(0,new PDL::Graphics::TriD::ArcCone(
																			$vp, 0,
																			$cont->{WRotation}));
		$ev->set_button(2,new PDL::Graphics::TriD::SimpleScaler(
																				  $vp,
																				  \$cont->{CDistance}));

		$vp->eventhandler($ev);
	 }
  }
  print "new_viewport: ",ref($vp)," ",$#{$this->{_ViewPorts}},"\n" if($PDL::Graphics::TriD::verbose);

  return $vp;
}

sub resize_viewport {
  my($this,$x0,$y0,$x1,$y1,$vpnum) = @_;
  
  $vpnum = $this->{_CurrentViewPort} unless(defined $vpnum);

  my $vp;
  if(defined($this->{_ViewPorts}[$vpnum])){
	 $vp = $this->{_ViewPorts}[$vpnum]->resize($x0,$y0,$x1,$y1);
  }
  return $vp;

}

sub current_viewport {
  my($this,$num) = @_;

  if(defined $num){
	 if(ref($num)){
		my $cnt=0;
		foreach (@{$this->{_ViewPorts}}){
		  if($num == $_){
			 $this->{_CurrentViewPort} = $cnt;
			 $_->{Active}=1;
		  }elsif(defined $_){
			 $_->{Active}=0;
		  }
		  $cnt++;
		}
	 }else{
		if(defined $this->{_ViewPorts}[$num]){
		  $this->{_CurrentViewPort} = $num;
		  $this->{_ViewPorts}[$num]->{Active}=1;
		}else{
		  print "ERROR: ViewPort $num undefined\n";
		}
	 }
  }
  return $this->{_ViewPorts}[$this->{_CurrentViewPort}];
}


sub viewports {
  my ($this) = shift;
  return $this->{_ViewPorts};
}


sub clear_viewport {
	my($this, $vp) = @_;
   my $cnt;
	if(($cnt=$#{$this->{_ViewPorts}})<= 0){
	  print "WARNING: Cannot delete final viewport - request ignored\n";

	}else{
	  if(ref($vp)){
		 for(0 .. $cnt){
			if($vp == $this->{_ViewPorts}[$_]){
			  $vp = $_;
			  last;
			}
		 }
	  }
	  $this->{_ViewPorts}[$vp]->clear();
	  splice(@{$this->{_ViewPorts}},$vp,1);
     $this->{_CurrentViewPort}= $cnt-1 if($this->{_CurrentViewPort} > $cnt-1);
	}
}

sub set_eventhandler {
  my($this,$handler) = @_;

  $this->{EHandler} = $handler;

#  for(@{$this->{_ViewPorts}}) {
#	 $_->eventhandler($handler);
#  }
}

#sub set_transformer {
#  my($this,$transformer) = @_;
#
#  for(@{$this->{_ViewPorts}}) {
#	 $_->transformer($transformer);
#  }
#}


sub AUTOLOAD {
  my ($self,@args)=@_;
  use vars qw($AUTOLOAD);
  my $sub = $AUTOLOAD;
  
# If an unrecognized function is called for window it trys to apply it
# to all of the defined ViewPorts

  $sub =~ s/.*:://;
  for(@{$self->{_ViewPorts}}) {
    next unless defined $_;
	 $_->$sub(@args);
  }
}

1;
