package PDL::Demos::TkTriD_demo;
use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Contours;
use PDL::Graphics::TriD::GL;
use Tk;
use PDL::Graphics::TriD::Tk;
use strict;
my ($TriDW,$graph); # declare the graph object in main, defined in initialize
my $e_button;

PDL::Demos::Routines->import();
sub act($);
sub comment($);


sub run {
  my $MW = MainWindow->new();

  my $bframe = $MW->Frame()->pack(-side=>'top',-fill=>'x');

# This is the TriD Tk widget it is a Tk Frame widget and has all of the
# attributes of a Frame

  $TriDW = $MW->Tk( )->pack(-expand=>1, 
			   -fill=>'both');

#
# The exit button
#
  $e_button = $bframe->Button(-text => "Exit",
			      -command => sub { exit }
			     )->pack(-side=>'right',-anchor=>'nw',-fill=>'y');
#
# The other menus
# 

  my $menus=[{Name=>'Simple',
	      Type=>'radio',
	      Options=>["Off","B&W","Color"],
	      Command=>\&linedemos,
	      Value=>'Off'},
	     {Name=>'Surface',
	      Type=>'radio',
	      Options=>["Off","Points","Lines","Lattice"],
	      Command=>\&Linesdemos,
	      Value=>'Off'},
	     {Name=>'Volume',
	      Type=>'radio',
	      Options=>["Off","Colors","Lighting"],
	      Command=>\&Torusdemos,
	      Value=>'Lighting'},
	     {Name=>'Contours',
	      Type=>'radio',
	      Options=>["Off","2DB&W","2DColor","3DColor"],
	      Command=>\&Contourdemos,
	      Value=>'Off'},
	     {Name=>'Object View',
	      Type=>'command',
	      Options=>['Top','East','South'],
	      Command=>\&setview}];
  
  foreach my $menu (@$menus){
    my $mew = $bframe->Menubutton(-text=>$menu->{Name},
				  -relief=>'raised'
				 )->pack(-side=>'left');
    
    if($menu->{Type} eq "radio"){
      foreach(@{$menu->{Options}}){
	
	$mew->radiobutton(-label=> $_,
			  -value=> $_,
			  -variable=> \$menu->{Value},
			  -command=> [$menu->{Command},$_] );
      }
    }elsif($menu->{Type} eq "command"){
      foreach(@{$menu->{Options}}){
	$mew->AddItems(["command" => $_,
			-command=> [$menu->{Command},$_] ]);
      }
    }
    
  }
  
#
# This sets the graphic that will be displayed when the window is first opened
#

  $e_button->bind("<Configure>",[ \&Torusdemos,0 ]);


  $TriDW->MainLoop;
}



sub linedemos{
  my($bh,$demo) = @_;
  $demo=$bh unless(ref($bh));
 
  return unless defined $TriDW->{GLwin};
  if(! $demo){
    # Remove the configure binding
    $e_button->bind("<Configure>","");
    # define the graph object
    $graph = new PDL::Graphics::TriD::Graph();
    $graph->default_axes();
    $demo="B&W";
  }
  $graph->delete_data("LinesB&W");
  $graph->delete_data("LinesColor");

  if($demo ne "Off"){
    my $data;
    my $size = 25;
    my $cz = (xvals zeroes $size+1) / $size;  # interval 0..1
    my $cx = 0.5+sin($cz*12.6)/2;	# Corkscrew
    my $cy = 0.5+cos($cz*12.6)/2;
    if($demo eq "B&W"){
      $graph->delete_data("LinesColor");
      $data=new PDL::Graphics::TriD::LineStrip([$cx,$cy,$cz]);
    }elsif($demo eq "Color"){
      $graph->delete_data("LinesB&W");

      my $r = sin($cz*6.3)/2 + 0.5;
      my $g = cos($cz*6.3)/2 + 0.5;
      my $b = $cz;
      
      $data=new PDL::Graphics::TriD::LineStrip([$cx,$cy,$cz],[$r,$g,$b]);
    }
    $graph->add_dataseries($data,"Lines$demo");
  }
  $graph->scalethings();
  $TriDW->{GLwin}->delete_object($graph);
    
  $TriDW->{GLwin}->add_object($graph);
  $TriDW->refresh();
}



sub Linesdemos{
  my($bh,$demo) = @_;
  $demo=$bh unless(ref($bh));
 
  return unless defined $TriDW->{GLwin};
  if(! $demo){
    # Remove the configure binding
    $e_button->bind("<Configure>","");
    # define the graph object
    $graph = new PDL::Graphics::TriD::Graph();
    $graph->default_axes();
    $demo="Lattice";
  }
  $graph->delete_data("LinesPoints");
  $graph->delete_data("LinesLines");
  $graph->delete_data("LinesLattice");

  if($demo ne "Off"){
    my $data;
    my $size = 25;
    my($x,$y,$z);
    $x = (xvals zeroes $size+1,$size+1) / $size;
    $y = (yvals zeroes $size+1,$size+1) / $size;
    $z = 0.5 + 0.5 * (sin($x*6.3) * sin($y*6.3)) ** 3; 
    if($demo eq "Lines"){
      $data=new PDL::Graphics::TriD::LineStrip([$x,$y,$z],[$x,$y,$z]);
    }elsif($demo eq "Lattice"){
      $data=new PDL::Graphics::TriD::Lattice([$x,$y,$z],[$x,$y,$z]);
    }elsif($demo eq "Points"){
      $data=new PDL::Graphics::TriD::Points([$x,$y,$z],[$x,$y,$z]);
    }

    $graph->add_dataseries($data,"Lines$demo");
  }
  $graph->scalethings();
  $TriDW->{GLwin}->delete_object($graph);
    
  $TriDW->{GLwin}->add_object($graph);
  $TriDW->refresh();
}


#	    Options=>["Off","2DB&W","2DColor","3DColor"],
sub Contourdemos{
  my($bh,$demo) = @_;
  $demo=$bh unless(ref($bh));
 
  return unless defined $TriDW->{GLwin};
  if(! $demo){
    # Remove the configure binding
    $e_button->bind("<Configure>","");
    # define the graph object
    $graph = new PDL::Graphics::TriD::Graph();
    $graph->default_axes();
    $demo="3DColor";
  }
  $graph->delete_data("Contours2DB&W");
  $graph->delete_data("Contours2DColor");
  $graph->delete_data("Contours3DColor");

  if($demo ne "Off"){
    my $data;
    my $size = 25;
    my($x,$y,$z);
    $x = (xvals zeroes $size,$size) / $size;
    $y = (yvals zeroes $size,$size) / $size;
    $z = (sin($x*6.3) * sin($y*6.3)) ** 3;
   
    if($demo eq "2DB&W"){
      $data=new PDL::Graphics::TriD::Contours($z,[$z->xvals/$size,$z->yvals/$size,0]);
    }elsif($demo eq "2DColor"){
      $data=new PDL::Graphics::TriD::Contours($z,[$z->xvals/$size,$z->yvals/$size,0]);
      $data->set_colortable(\&PDL::Graphics::TriD::Contours::coldhot_colortable);
    }elsif($demo eq "3DColor"){
      $data=new PDL::Graphics::TriD::Contours($z,[$z->xvals/$size,$z->yvals/$size,$z]);
      $data->set_colortable(\&PDL::Graphics::TriD::Contours::coldhot_colortable);
    }
	 $data->addlabels(2,5);

    $graph->add_dataseries($data,"Contours$demo");
  }
  $graph->scalethings();
  $TriDW->{GLwin}->delete_object($graph);
    
  $TriDW->{GLwin}->add_object($graph);

  $TriDW->refresh();
}


sub Torusdemos{
  my($bh,$demo) = @_;
  $demo=$bh unless(ref($bh));
 
  return unless defined $TriDW->{GLwin};
  if(! $demo){
    # Remove the configure binding
    $e_button->bind("<Configure>","");
    # define the graph object
    $graph = new PDL::Graphics::TriD::Graph();
    $graph->default_axes();
    $demo="Lighting";
  }
  $graph->delete_data("TorusColors");
  $graph->delete_data("TorusLighting");

  if($demo ne "Off"){
    my $data;
    my $s=40;
    my $a=zeroes 2*$s,$s/2;
    my $t=$a->xlinvals(0,6.284);
    my $u=$a->ylinvals(0,6.284); 
    my $o=0.5;
    my $i=0.1;
    my $v=$o+$i*sin$u;

    my $x = $v*sin $t;
    my $y = $v*cos $t;
    my $z = $i*cos($u)+$o*sin(3*$t);

    if($demo eq "Colors"){
      $data=new PDL::Graphics::TriD::SLattice([$x,$y,$z],
					      [0.5*(1+sin $t),0.5*(1+cos $t),0.25*(2+cos($u)+sin(3*$t))]);
    }else{
      $data=new PDL::Graphics::TriD::SLattice_S([$x,$y,$z]);
    }
    $graph->add_dataseries($data,"Torus$demo");
  }
  $graph->scalethings();

  $TriDW->{GLwin}->delete_object($graph);
    
  $TriDW->{GLwin}->add_object($graph);
  $TriDW->refresh();
}

#
# restore the image view to a known value
#
sub setview{
  my($view) = @_;

  $TriDW->{GLwin}{Transformer}{WRotation}[1]," ",
  $TriDW->{GLwin}{Transformer}{WRotation}[2]," ",
  $TriDW->{GLwin}{Transformer}{WRotation}[3],"\n";

  if($view eq "Top"){
    print "set view top\n";
    $TriDW->{GLwin}{Transformer}{WRotation}[0]=0;
    $TriDW->{GLwin}{Transformer}{WRotation}[1]=0;
    $TriDW->{GLwin}{Transformer}{WRotation}[2]=0;
    $TriDW->{GLwin}{Transformer}{WRotation}[3]=0;
  }elsif($view eq "East"){
    $TriDW->{GLwin}{Transformer}{WRotation}[0]=  0.5;
    $TriDW->{GLwin}{Transformer}{WRotation}[1]= -0.5;
    $TriDW->{GLwin}{Transformer}{WRotation}[2]= -0.5;
    $TriDW->{GLwin}{Transformer}{WRotation}[3]= -0.5;
  }elsif($view eq "South"){
    $TriDW->{GLwin}{Transformer}{WRotation}[0]=0.6;
    $TriDW->{GLwin}{Transformer}{WRotation}[1]=-0.6;
    $TriDW->{GLwin}{Transformer}{WRotation}[2]= 0;
    $TriDW->{GLwin}{Transformer}{WRotation}[3]= 0;
  }

  $TriDW->refresh();

}


1;
