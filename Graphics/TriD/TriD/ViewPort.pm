#
# The PDL::Graphics::TriD::ViewPort is already partially defined in
# the appropriate gdriver (GL or VRML), items defined here are common
# to both
# 
package PDL::Graphics::TriD::ViewPort;
use strict;

sub new {
     my($type,$x0,$y0,$w,$h) = @_;
	  
	  my $this= $type->SUPER::new();

	  $this->{X0} = $x0;
	  $this->{Y0} = $y0;
	  $this->{W} = $w;
	  $this->{H} = $h;
	  $this->{DefMaterial} = new PDL::Graphics::TriD::Material;

     return $this;
}


sub graph {
  my($this,$graph) = @_;

  if(defined($graph)){  
	 $this->add_object($graph);
	 push(@{$this->{Graphs}},$graph);
  }elsif(defined $this->{Graphs}){
	 $graph = $this->{Graphs}[0];
  }
  return($graph);
  
}  

sub delete_graph {
  my($this,$graph) = @_;
  
  $this->delete_object($graph);
  for(0..$#{$this->{Graphs}}){
    if($graph == $this->{Graphs}[$_]){
      splice(@{$this->{Graphs}},$_,1);
      redo;
    }
  }
  

}


sub resize {
  my($this,$x0,$y0,$w,$h) = @_;

  $this->{X0} = $x0;
  $this->{Y0} = $y0;
  $this->{W} = $w;
  $this->{H} = $h;
  return $this;
}



sub add_resizecommand {
	my($this,$com) = @_;
	push @{$this->{ResizeCommands}},$com;
	print "ARC: $this->{W},$this->{H}\n" if($PDL::Graphics::TriD::verbose);
	&$com($this->{W},$this->{H});
}

sub set_material {
  $_[0]->{DefMaterial} = $_[1];
}

sub eventhandler {
  my ($this,$eh) = @_;
  if(defined $eh){
	 $this->{EHandler} = $eh;
  }
  return $this->{EHandler};
}

sub set_transformer {
  $_[0]->transformer($_[1]);
}

sub transformer {
  my ($this,$t) = @_;
  if(defined $t){
	 $this->{Transformer} = $t;
  }
  return $this->{Transformer};
}


#
# restore the image view to a known value
#
sub setview{
  my($vp,$view) = @_;

  my $transformer = $vp->transformer();

  if(ref($view) eq "ARRAY"){
	 $transformer->set({WRotation=>$view});
  }elsif($view eq "Top"){
	 $transformer->set({WRotation=>[1,0,0,0]});
  }elsif($view eq "East"){
	 $transformer->set({WRotation=>[0.5,-0.5,-0.5,-0.5]});
  }elsif($view eq "South"){
	 $transformer->set({WRotation=>[0.6,-0.6,0,0]});
  }

}



1;
