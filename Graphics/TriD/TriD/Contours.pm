package PDL::Graphics::TriD::Contours;
use strict;
use PDL;
use PDL::Graphics::TriD::Rout;
use PDL::Graphics::TriD::Labels;
use Data::Dumper;

use base qw/PDL::Graphics::TriD::GObject/;
use fields qw/ContourSegCnt _MaxDistance Labels LabelStrings/;

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

#  my $grid = PDL::Graphics::TriD::realcoords('SURF2D',$points);

  my $this = $type->SUPER::new($points,$colors,$options);

#  my $out = Dumper($this);
#  print $out;
#  exit;

  
  my $grid = $this->{Points};
    

  $this->{ContourSegCnt} = [];

  my $zval;

  my @lines;
  my($xmin,$dx,$ymin,$dy);

  my @dims = $data->dims();

  my $d0 = $dims[0]-1;
  my $d1 = $dims[1]-1;

  
  my ($min,$max) = $data->minmax();


  my $fac=1;
  my $plane;

  if(defined $this->{Options}{Surface}){
      my $surf =  $this->{Options}{Surface};
      foreach(keys %{$this->{Options}{Surface}}){
	  if(defined $zval){
	      barf "Only one of XY XZ YZ surfaces allowed";
	  }
	  $zval=$this->{Options}{Surface}{$_};
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


  unless(defined $this->{Options}{ContourMin}){
    while($fac*($max-$min)<10){
      $fac*=10;
    }
    if(int($fac*$min) == $fac*$min){
      $this->{Options}{ContourMin} = $min;
    }else{      
      $this->{Options}{ContourMin} = int($fac*$min+1)/$fac;
      print "ContourMin =  ",$this->{Options}{ContourMin},"\n"
		  if($PDL::Graphics::TriD::verbose);
    }
  }
  unless(defined $this->{Options}{ContourMax}){
    if(defined $this->{Options}{ContourInt}){
      $this->{Options}{ContourMax} = $this->{Options}{ContourMin};
      while($this->{Options}{ContourMax}+$this->{Options}{ContourInt} < $max){
	$this->{Options}{ContourMax}= $this->{Options}{ContourMax}+$this->{Options}{ContourInt};
      }
    }else{
      if(int($fac*$max) == $fac*$max){
	$this->{Options}{ContourMax} = $max;
      }else{
	$this->{Options}{ContourMax} = (int($fac*$max)-1)/$fac;
      print "ContourMax =  ",$this->{Options}{ContourMax},"\n"
		  if($PDL::Graphics::TriD::verbose);
      }
    }
  }
  unless(defined $this->{Options}{ContourInt}){
    $this->{Options}{ContourInt} = int($fac*($this->{Options}{ContourMax}-$this->{Options}{ContourMin}))/(10*$fac);
    print "ContourInt =  ",$this->{Options}{ContourInt},"\n"
		if($PDL::Graphics::TriD::verbose);
  }
#
# The user could also name cvals
#
  my $cvals;
  if( !defined($this->{Options}{ContourVals}) || $this->{Options}{ContourVals}->isempty){
    $cvals=zeroes(int(($this->{Options}{ContourMax}-$this->{Options}{ContourMin})/$this->{Options}{ContourInt}+1));
    $cvals = $cvals->xlinvals($this->{Options}{ContourMin},$this->{Options}{ContourMax});  
  }else{
    $cvals = $this->{Options}{ContourVals};
  }
  $this->{Options}{ContourVals} = $cvals;

  print "Cvals = $cvals\n" if($PDL::Graphics::TriD::verbose);  
  
  my ($i,$j,$i1,$j1);
#  $i1=$dims[0]-2;
#  $j1=$dims[1]-2;

#
# Used to compute label spacing -3 assures that both arrays are the same size.
#

  $this->{_MaxDistance} = vdistance($grid->slice(":,0:-3:2,0:-3:2"),
			 	   $grid->slice(":,1:-2:2,1:-2:2"))->max;
  $this->contour_segments($cvals,$data,$grid);

#  for($i=0;$i<$cvals->nelem;$i++){
#    my $cval = sprintf("%-6.6g",$cvals->slice("($i)"));
#    print "cval = $cval, \n";
#
#    $this->add_dataseries(new PDL::Graphics::TriD::Lines(
#				      ,$colors),"_CONTOUR_$cval")
#      unless($mpts->[$i]->isempty);
#  } 

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

  my $pcnt = 0;
  my $cnt;

  for(my $i=0; $i<= $#{$self->{ContourSegCnt}}; $i++){
    next unless defined $self->{ContourSegCnt}[$i];
    $cnt = $self->{ContourSegCnt}[$i];
    my $val = $self->{Options}{ContourVals}->slice("($i)");
    next if($i % $labelint);

    my $lp1 = $self->{Points}->slice(":,$pcnt:$cnt");
    my $lp2 = $lp1->slice(":,1:-1:2");  
	 $lp1=$lp1->slice(":,0:-2:2");
    $pcnt = $cnt+1 ;
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
    next unless($dis->orover);

    $lp2 = ($lp1+($lp2-$lp1)*0.5);

    print "clump 2 ",$lp2->info,$lp2,$dis->info,$dis,"\n";
 

    $lp2=$lp2->clump(2)->where($dis);

    print "clump out\n";

    next if($lp2->isempty);

    $lp = $lp->append($lp2);
# need a label string for each point    
    
    for(my $i=0;$i<$lp2->getdim(0)/3;$i++){
      push(@$strlist,$val);
    }

  }
  if($lp->nelem>0){
	 $self->{Points} = $self->{Points}->xchg(0,1)
		->append($lp->reshape(3,$lp->nelem/3)->xchg(0,1))->xchg(0,1);
	 $self->{Labels} = [$cnt+1,$cnt+$lp->nelem/3];
	 $self->{LabelStrings} = $strlist;
	 $self->{Options}{Font}=$font;
  }

}


sub vdistance{
  my($lp1,$lp2) = @_;

  sqrt(($lp2->slice("0,:")-$lp1->slice("0,:"))**2+
       ($lp2->slice("1,:")-$lp1->slice("1,:"))**2+
       ($lp2->slice("2,:")-$lp1->slice("2,:"))**2);

}





sub get_valid_options{
    return{ ContourInt => undef, 
	    ContourMin => undef, 
	    ContourMax=>  undef, 
	    ContourVals=> pdl->null,
            Surface=>{XY=>0},
	    UseDefcols=>1,
	    Font=>$PDL::Graphics::TriD::GL::fontbase}
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
  
  my $min = $self->{Options}{ContourMin};
  my $max = $self->{Options}{ContourMax};  
  my $int = $self->{Options}{ContourInt};
  my $ncolors=($max-$min)/$int+1;  

  my $colors= &$table($ncolors);

  if($colors->getdim(0)!=3){
    $colors->reshape(3,$colors->nelem/3);
  }
  print "Color info ",$self->{Colors}->info," ",$colors->info,"\n" if($PDL::Graphics::TriD::verbose);
 
  $self->{Colors} = $colors;

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














