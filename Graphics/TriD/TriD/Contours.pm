package PDL::Graphics::TriD::Contours;
use strict;
use PDL;
use PDL::Graphics::TriD::Rout;
use PDL::Graphics::TriD::Labels;
use Data::Dumper;

use base qw/PDL::Graphics::TriD::Graph/;
use fields qw/ContourMin ContourMax ContourInt ContourVals 
             _MaxDistance/;

sub new{
  my($type,$data,$points,$colors,$options) = @_;

  if(ref($colors) eq "HASH"){
    $options=$colors ;
    undef $colors;
  }
  $colors = pdl[1,1,1] unless defined $colors;

  if(!defined $colors) {
    $colors = PDL->pdl(1,1,1);
    $colors = $type->cdummies($colors,$points);
    $options->{UseDefcols} = 1;  # for VRML efficiency
  } else {
    $colors = PDL::Graphics::TriD::realcoords("COLOR",$colors);
  }

  my $grid = PDL::Graphics::TriD::realcoords('SURF2D',$points);

  my $this = $type->SUPER::new();

  my $zval;

  my @lines;
  my($xmin,$dx,$ymin,$dy);

  my @dims = $data->dims();

  my $d0 = $dims[0]-1;
  my $d1 = $dims[1]-1;

  
  my ($min,$max) = $data->minmax();


  my $fac=1;
  my $plane;

  if(defined $options->{Surface}){
      my $surf =  $options->{Surface};
      foreach(keys %{$options->{Surface}}){
	  if(defined $zval){
	      barf "Only one of XY XZ YZ surfaces allowed";
	  }
	  $zval=$options->{Surface}{$_};
	  $plane=$_;
      }
      if($plane eq "XZ"){
	(my $t = $grid->slice("1:2")) .= $grid->slice("1:2")->rotate(1);
      }elsif($plane eq "YZ"){
	$grid=$grid->rotate(1);
      }
  }else{
      $plane="XY";
      $zval=0;
  }


  if(defined $options->{ContourInt}){
    $this->{ContourInt} = $options->{ContourInt};
  }

  if(defined $options->{ContourMin}){
    $this->{ContourMin} = $options->{ContourMin};
  }else{
    while($fac*($max-$min)<10){
      $fac*=10;
    }
    if(int($fac*$min) == $fac*$min){
      $this->{ContourMin} = $min;
    }else{      
      $this->{ContourMin} = int($fac*$min+1)/$fac;
      print "ContourMin =  ",$this->{ContourMin},"\n";
    }
  }
  if(defined $options->{ContourMax}){
    $this->{ContourMax} = $options->{ContourMax};
  }else{
    if(defined $this->{ContourInt}){
      $this->{ContourMax} = $this->{ContourMin};
      while($this->{ContourMax}+$this->{ContourInt} < $max){
	$this->{ContourMax}= $this->{ContourMax}+$this->{ContourInt};
      }
    }else{
      if(int($fac*$max) == $fac*$max){
	$this->{ContourMax} = $max;
      }else{
	$this->{ContourMax} = (int($fac*$max)-1)/$fac;
      print "ContourMax =  ",$this->{ContourMax},"\n";
      }
    }
  }
  unless(defined $this->{ContourInt}){
    $this->{ContourInt} = int($fac*($this->{ContourMax}-$this->{ContourMin}))/(10*$fac);
    print "ContourInt =  ",$this->{ContourInt},"\n";
  }
#
# The user could also name cvals
#
  my $cvals;
  if(defined $options->{ContourVals}){
    $cvals = $options->{ContourVals};
  }else{
    $cvals=zeroes(int(($this->{ContourMax}-$this->{ContourMin})/$this->{ContourInt}+1));
    $cvals = $cvals->xlinvals($this->{ContourMin},$this->{ContourMax});  
    print $cvals;
  }
  $this->{ContourVals} = $cvals;

  
  
  my ($i,$j,$i1,$j1);
#  $i1=$dims[0]-2;
#  $j1=$dims[1]-2;

#
# Used to compute label spacing -3 assures that both arrays are the same size.
#

  $this->{_MaxDistance} = vdistance($grid->slice(":,0:-3:2,0:-3:2"),
			 	   $grid->slice(":,1:-2:2,1:-2:2"))->max;

  my $mpts = &PDL::Graphics::TriD::Rout::contour_segments($cvals,$data,$grid);

  for($i=0;$i<$cvals->nelem;$i++){
    my $cval = sprintf("%-6.6g",$cvals->slice("($i)"));
#    print "cval = $cval, \n";

    $this->add_dataseries(new PDL::Graphics::TriD::Lines($mpts->[$i]
				      ,$colors),"_CONTOUR_$cval")
      unless($mpts->[$i]->isempty);
  } 

  return $this;
}      



=head2 addlabels

$contour->addlabels($labelint,$density,$font);

=over 4
=item * labelint is the integer interval between labeled contours ie if you
have 8 countour levels and specify labelint=3 addlabels will attempt
to label the 1st, 4th, and 7th contours.  labelint defaults to 1.

=item * density is the number of contour line segments to label as determined
by the length of the line segment with respect to the length of the
longest possible line segment in the plot.  density defaults to 1.

=back

=cut



sub addlabels{
  my ($self,$labelint, $density ,$font) = @_;

  $labelint = 1 unless(defined $labelint);
  $font =  $PDL::Graphics::TriD::GL::fontbase unless(defined $font);
  my $df;
  if(defined $density){
    $df = 1-$density;
  }else{
    $df=0;
  }

  my $cnt=0;

  my $strlist;
  my $lp=pdl->null;

  foreach(sort keys %{$self->{Data}}){
    next if($cnt++ % $labelint);
    next unless(/^_CONTOUR_(.*)$/);
    my $val = $1;

    my $lp1 = $self->{Data}{$_}->get_points();
    my $lp2 = $lp1->slice(":,1:-1:2");  
    $lp1=$lp1->slice(":,0:-1:2");
#
# normalized length of line
#
    my $dis;
    if($df>0){
      $dis =  vdistance($lp1,$lp2)/$self->{_MaxDistance} >= $df;
    }else{
      $dis =  ones($lp1->getdim(1))->dummy(0);
    }
#    print "clump 1 ", $dis->info,"\n";


    $dis = $dis->append($dis->append($dis))->clump(2);


    $lp2 = ($lp1+($lp2-$lp1)*0.5);

#    print "clump 2 ",$lp2->info,$lp2,$dis->info,$dis,"\n";
 

    $lp2=$lp2->clump(2)->where($dis);

#    print "clump out\n";

    next if($lp2->isempty);

    $lp = $lp->append($lp2);
# need a label string for each point    
    
    for(my $i=0;$i<$lp2->getdim(0)/3;$i++){
      push(@$strlist,$val);
    }

  }


  my $l = new PDL::Graphics::TriD::Labels($lp->reshape(3,$lp->nelem/3),
					  {Strings=>$strlist
					   ,Font=>$font});

  $self->add_dataseries($l,"Labels");

  $self->scalethings;

}


sub vdistance{
  my($lp1,$lp2) = @_;

  sqrt(($lp2->slice("0,:")-$lp1->slice("0,:"))**2+
       ($lp2->slice("1,:")-$lp1->slice("1,:"))**2+
       ($lp2->slice("2,:")-$lp1->slice("2,:"))**2);

}





sub get_valid_options{
    return{ ContourInt => 0, 
	    ContourMin => 0, 
	    ContourMax=> 0, 
	    ContourVals=> 0,
            Surface=>{XY=>0},
	    UseDefcols=>1}
}

sub get_attribute{
  my($self, $attname) = @_;
  my $allowed_attributes = $self->get_valid_options;
  if(defined $allowed_attributes->{$attname}){
    return $self->{$attname};
  }
  barf "Invalid attribute to $self\n";
}

sub set_colortable{
  my($self,$table) = @_;
  
  my $min = $self->{ContourMin};
  my $max = $self->{ContourMax};  
  my $int = $self->{ContourInt};
  my $ncolors=($max-$min)/$int+1;  

     
  my $colors= &$table($ncolors);
  if($colors->getdim(0)!=3){
    $colors->reshape(3,$colors->nelem/3);
  }

#  print "nc =",$ncolors," nelem = ",$colors->info,"\n";

  for(my $i=0;$i<$ncolors;$i++){
    $self->{Objects}[$i]->set_colors($colors->slice(":,$i"));
  }

#  use Data::Dumper;
#  my $out = Dumper($self);
#  print $out;
}
  
sub coldhot_colortable{
  my($ncolors) = @_;
  my $colorpdl;
  # 0 red, 1 green, 2 blue
  for(my $i=0;$i<$ncolors;$i++){
    my $color = zeroes(float,3);
    (my $t = $color->slice("0")) .= 0.75*($i)/$ncolors;
    ($t = $color->slice("2")) .= 0.75*($ncolors-$i)/$ncolors;
    if($i==0){
      $colorpdl = $color;
    }else{
      $colorpdl = $colorpdl->append($color);
    }
  }
  return($colorpdl);  
}
  
1;














