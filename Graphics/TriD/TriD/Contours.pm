package PDL::Graphics::TriD::Contours;

use PDL;
use PDL::Graphics::TriD::Graph;
#use PDL::Graphics::TriD::MathGraph;

@ISA=qw/PDL::Graphics::TriD::Graph/;

sub new{
  my($type,$data,$points,$colors,$options) = @_;
#  my($type,$points,$colors,$options) = @_;
  if(ref($colors) eq "HASH"){
    $options=$colors ;
    undef $colors;
  }
  $colors = pdl[1,1,1] unless defined $colors;
  my $this = $type->SUPER::new();
  my $zval;
#  $this->default_axes();

  $this->{Rawdata} = $data;

  $points = PDL::Graphics::TriD::realcoords("",$points);


  my @lines;
  my($xmin,$dx,$ymin,$dy);

  my @dims = $data->dims();

  my $d0 = $dims[0]-1;
  my $d1 = $dims[1]-1;

  
  my ($min,$max) = $data->minmax();


  my $fac=1;
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
	($t = $points->slice("1:2")) .= $points->slice("1:2")->rotate(1);
      }elsif($plane eq "YZ"){
	$points=$points->rotate(1);
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
      $this->{ContourMin} = (int($fac*$min)+1)/$fac;
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
      }
    }
  }
  unless(defined $this->{ContourInt}){
    $this->{ContourInt} = int($fac*($this->{ContourMax}-$this->{ContourMin}))/(10*$fac);
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
  }
  $this->{ContourVals} = $cvals;

  
  
  my ($i,$j,$i1,$j1);
  $i1=$dims[0]-2;
  $j1=$dims[1]-2;

#
# Used to compute label spacing
#

  $this->{MaxDistance} = vdistance($points->slice(":,0:$i1:2,0:$j1:2"),
				   $points->slice(":,1:$i1:2,1:$j1:2"))->max;


  for($j=0;$j<$dims[1]-1;$j++){
      $j1=$j+1;
      for($i=0;$i<$dims[0]-1;$i++){
	  $i1= $i+1;
          $this->segments($data->slice("$i:$i1,$j:$j1"),
                          $points->slice(":,$i:$i1,$j:$j1"),
			  $cvals);

      }
  }
  sub numerically{ $a<=>$b};
  my $i=0;  

  foreach (sort numerically keys %{$this->{RawLines}}){
#    my $ans .= $this->{RawLines}{$_};
    
    $this->{RawLines}{$_} = $this->{RawLines}{$_}->reshape(3,$this->{RawLines}{$_}->nelem/3);
    if($plane eq "XZ"){
      ($t = $this->{RawLines}{$_}->slice("1:2")) .= $this->{RawLines}{$_}->slice("1:2")->rotate(1);
    }elsif($plane eq "YZ"){
      $this->{RawLines}{$_}=$this->{RawLines}{$_}->rotate(1);
    }
    my $color;
    if(defined $options->{ColorTable}){
      $color = $colors->slice(":,$i");
    }else{
      $color = $colors;
    }
    $this->add_dataseries(new PDL::Graphics::TriD::Lines($this->{RawLines}{$_},$color),"$_");
#    undef  $this->{RawLines}{$_};
    $i++;
  }

  $this->default_axes;
  $this->scalethings;

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
  my $lp;

  foreach(sort keys %{$self->{RawLines}}){
    next if($cnt++ % $labelint);

    my $max = $self->{RawLines}{$_}->getdim(1)-1;

    my $lp1 = $self->{RawLines}{$_}->slice(":,0:$max:2");  
    my $lp2 = $self->{RawLines}{$_}->slice(":,1:$max:2");  
    
#
# normalized length of line
#
    my $dis;
    if($df>0){
      $dis =  vdistance($lp1,$lp2)/$self->{MaxDistance} >= $df;
    }else{
      $dis =  ones($lp1->getdim(1));
    }
#    print "clump 1 ", $dis->info,"\n";


    $dis = $dis->append($dis->append($dis))->clump(2);


    $lp2 = ($lp1+($lp2-$lp1)*0.5);

#    print "clump 2 ",$lp2->info,$lp2,$dis->info,$dis,"\n";
 

    $lp2=$lp2->clump(2)->where($dis);

#    print "clump out\n";

    next if($lp2->isempty);

    if(defined $lp){
      $lp = $lp->append($lp2);
    }else{
      $lp = $lp2;
    }    
# need a label string for each point    

    for(my $i=0;$i<$lp2->getdim(0)/3;$i++){
      push(@$strlist,$_);
    }

  }

  $PDL::Graphics::TriD::verbose=1;

  use PDL::Graphics::TriD::Labels;

  my $l = new PDL::Graphics::TriD::Labels($lp->reshape(3,$lp->nelem/3),
					  {Strings=>$strlist
					   ,Font=>$font});



  $PDL::Graphics::TriD::verbose=0;
  $self->add_dataseries($l,"Labels");

  $self->scalethings;

}


sub vdistance{
  my($lp1,$lp2) = @_;

  sqrt(($lp2->slice("0,:")-$lp1->slice("0,:"))**2+
       ($lp2->slice("1,:")-$lp1->slice("1,:"))**2+
       ($lp2->slice("2,:")-$lp1->slice("2,:"))**2);

}



sub segments{
    my ($self,$data,$points,$cvals) = @_;

    my ($min,$max) = $data->minmax();
    
    $cvals = $cvals->where($min<$cvals & $max>=$cvals);
    $zval = 0 unless(defined $zval);
     
    return if($cvals->isempty);

#    $ltcnt = $ltcnt+$ltcnt->orover->dummy(0);
#    print "$ltcnt\n";
#    exit;
    my (@d, @x ,@y, @z);

    for(my $i=0;$i<4;$i++){
      my $i1 = $i%2;
      my $j1 = int($i/2);
      $d[$i] = $data->slice("$i1,$j1");

      $x[$i] = $points->slice("0,$i1,$j1");
      $y[$i] = $points->slice("1,$i1,$j1");
      $z[$i] = $points->slice("2,$i1,$j1");
    }

    for(my $i=0;$i<$cvals->nelem;$i++){
	my $cval = $cvals->slice("$i");
        my $cname = $cvals->at($i);

        $self->{RawLines}{$cname} = PDL->null 
	  unless(defined $self->{RawLines}{$cname});

	my $ltcnt = ($data<$cval);
	if($ltcnt->slice(":,0")->sumover==1){
            my $x = linear_interp($cval,$x[0],$x[1]-$x[0],$d[0],$d[1]);
            my $y = linear_interp( $x,  $y[0],$y[1]-$y[0],$x[0],$x[1]);
            my $z = linear_interp( $x,  $z[0],$z[1]-$z[0],$x[0],$x[1]);
	    $self->{RawLines}{$cname} = $self->{RawLines}{$cname}->append($x->append($y->append($z)));
	}
	if($ltcnt->slice(":,1")->sumover==1){
            my $x = linear_interp($cval,$x[2],$x[3]-$x[2],$d[2],$d[3]);
            my $y = linear_interp( $x,  $y[2],$y[3]-$y[2],$x[2],$x[3]);
            my $z = linear_interp( $x,  $z[2],$z[3]-$z[2],$x[2],$x[3]);
	    $self->{RawLines}{$cname} = $self->{RawLines}{$cname}->append($x->append($y->append($z)));
	}
        $ltcnt=$ltcnt->xchg(0,1);
	if($ltcnt->slice(":,0")->sumover==1){
            my $y = linear_interp($cval,$y[0],$y[2]-$y[0],$d[0],$d[2]);
            my $x = linear_interp( $y,  $x[0],$x[2]-$x[0],$y[0],$y[2]);
            my $z = linear_interp( $y,  $z[0],$z[2]-$z[0],$y[0],$y[2]);
	    $self->{RawLines}{$cname} = $self->{RawLines}{$cname}->append($x->append($y->append($z)));
	}
	if($ltcnt->slice(":,1")->sumover==1){
            my $y = linear_interp($cval,$y[1],$y[3]-$y[1],$d[1],$d[3]);
            my $x = linear_interp( $y,  $x[1],$x[3]-$x[1],$y[1],$y[3]);
            my $z = linear_interp( $y,  $z[1],$z[3]-$z[1],$y[1],$y[3]);
	    $self->{RawLines}{$cname} = $self->{RawLines}{$cname}->append($x->append($y->append($z)));
	}
    }	
}


sub linear_interp{
    my ($cval,$xmin,$dx,$z1,$z2) = @_;
    return ($dx*$cval+$xmin*$z2-($xmin+$dx)*$z1)/($z2-$z1);
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

  for($i=0;$i<$ncolors;$i++){
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














