package PDL::Demos::TkTriD_demo;
use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Contours;
use PDL::Graphics::TriD::GL;
use Tk;
use PDL::Graphics::TriD::Tk;
use strict;
my $TriDW;      # declare the graph object in main, defined in initialize
PDL::Demos::Routines->import();
sub act($);
sub comment($);

#BEGIN{
#  if(defined $PDL::Graphics::TriD::cur){
#	 print "Configuration error in TkTriD demo\n";
#	 print "This demo cannot be run after you have loaded TriD\n";
#	 print "Please restart perldl then try again.\n";
#	 exit;
#  }
#}

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
my  $e_button = $bframe->Button(-text => "Exit",
										  -command => sub { exit }
										 )->pack(-side=>'right',-anchor=>'nw',-fill=>'y');
#
# The other menus
# 

  my $menus=
	 [{Name=>'Simple',
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
		Command=>\&setview},
	  {Name=>'ViewPorts',
		Type=>'command',
		Options=>['Split Horizontal','Split Vertical',
					 'Un-Split (Save This)','Un-Split (Save Others)'],
		Command=>\&setviewports},
	  {Name=>'Focus',
		Type=>'radio',
		Options=>["Pointer","DoubleClick"],
		Command=>\&setfocusstyle,
		Value=>'Pointer'}
	 ];
  
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
        if(/^Un-Split/){
			 $mew->AddItems(["command" => $_,
								  -state => 'disabled',
								  -command=> [$menu->{Command},$mew,$_] ]);
		  }else{
			 $mew->AddItems(["command" => $_,
								  -command=> [$menu->{Command},$mew,$_] ]);
		  }
      }
    }
    
  }
#  
# Sets a default focus style for viewport
# 
  setfocusstyle('Pointer');
#
# This sets the graphic that will be displayed when the window is first opened
#

  $e_button->bind("<Configure>",[ sub { my $but = shift; 
													 Torusdemos(); 
													 $but->bind("<Configure>",'') }]);

  $TriDW->MainLoop;
}



sub linedemos{
  my($bh,$demo) = @_;
  $demo=$bh unless(ref($bh));

  return unless defined $TriDW->{GLwin};

  my $graph;

  $graph = $TriDW->{GLwin}->current_viewport->graph();
  $demo="B&W" unless(defined $demo);

  unless(defined $graph){
    # define the graph object
    $graph = new PDL::Graphics::TriD::Graph();
    $graph->default_axes();
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
  $TriDW->current_viewport()->delete_graph($graph);
    
  $TriDW->current_viewport()->graph($graph);
  $TriDW->refresh();
}



sub Linesdemos{
  my($bh,$demo) = @_;
  $demo=$bh unless(ref($bh));
 
  return unless defined $TriDW->{GLwin};
  my $graph;

  $graph = $TriDW->{GLwin}->current_viewport->graph();
  $demo="Lattice" unless(defined $demo);

  unless(defined $graph){
    # define the graph object
    $graph = new PDL::Graphics::TriD::Graph();
    $graph->default_axes();
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
  $TriDW->current_viewport()->delete_graph($graph);
    
  $TriDW->current_viewport()->graph($graph);
  $TriDW->refresh();
}


#	    Options=>["Off","2DB&W","2DColor","3DColor"],
sub Contourdemos{
  my($bh,$demo) = @_;
  $demo=$bh unless(ref($bh));
 
  return unless defined $TriDW->{GLwin};
  my $graph;

  $graph = $TriDW->{GLwin}->current_viewport->graph();
  $demo="3DColor" unless(defined $demo);

  unless(defined $graph){
    # define the graph object
    $graph = new PDL::Graphics::TriD::Graph();
    $graph->default_axes();
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
  $TriDW->current_viewport()->delete_graph($graph);
    
  $TriDW->current_viewport()->graph($graph);

  $TriDW->refresh();
}


sub Torusdemos{
  my($bh,$demo) = @_;
  $demo=$bh unless(ref($bh));
 
  return unless defined $TriDW->{GLwin};
  my $graph;

  $graph = $TriDW->{GLwin}->current_viewport->graph();
  $demo="Lighting" unless defined $demo;
  
  unless(defined $graph){
    # define the graph object
    $graph = new PDL::Graphics::TriD::Graph();
    $graph->default_axes();
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

  $TriDW->current_viewport()->delete_graph($graph);
    
  $TriDW->current_viewport()->graph($graph);
  $TriDW->refresh();
}


#
# restore the image view to a known value
#
sub setview{
  my($menu,$view) = @_;

  my $transformer = $TriDW->current_viewport()->setview($view);

  $TriDW->refresh();

}



sub setviewports{
  my($menu,$request) = @_;
#  print "svp $request\n";

  my $vp = $TriDW->current_viewport();
  my $nvp;  
  if($request eq 'Split Horizontal'){
	 $nvp=$TriDW->new_viewport($vp->{X0}+$vp->{W}/2,$vp->{Y0},$vp->{W}/2,$vp->{H});
	 $vp->resize($vp->{X0},$vp->{Y0},$vp->{W}/2,$vp->{H});
  }elsif($request eq 'Split Vertical'){
	 $nvp=$TriDW->new_viewport($vp->{X0},$vp->{Y0}+$vp->{H}/2,$vp->{W},$vp->{H}/2);
	 $vp->resize($vp->{X0},$vp->{Y0},$vp->{W},$vp->{H}/2);
  }elsif($request eq 'Un-Split (Save This)'){
	 my $cnt=0;
	 foreach (@{$TriDW->viewports()}){
		if(defined $_ && $_ != $vp){
		  $TriDW->clear_viewport($cnt);
		}
		$cnt++;
	 }
	 $vp->resize(0,0,$TriDW->{GLwin}{Width},$TriDW->{GLwin}{Height});
  }elsif($request eq 'Un-Split (Save Others)'){
	 if($vp->{W} < $TriDW->{GLwin}{Width}){
		my $x0 = $vp->{X0};
		my $x1 = $vp->{X0}+$vp->{W};
		foreach (@{$TriDW->viewports()}){
		  if(($_->{X0} == $x1) || ($_->{X0}+$_->{W} == $x0)){
			 $x0 = $_->{X0} if($x0>$_->{X0});
			 $_->resize(min($x0,$_->{X0}),$_->{Y0},$_->{W}+$vp->{W},$_->{H});
		  }
		}
	 }

	 $TriDW->clear_viewport($vp);
  }
  
  if($#{$TriDW->viewports()} > 0){
	 $menu->entryconfigure('Un-Split (Save This)', -state=>'normal');
	 $menu->entryconfigure('Un-Split (Save Others)', -state=>'normal');
  }else{
	 $menu->entryconfigure('Un-Split (Save This)', -state=>'disabled');
	 $menu->entryconfigure('Un-Split (Save Others)', -state=>'disabled');
  }
}

sub setfocusstyle{
  my($fs) = @_;

  if($fs eq 'Pointer'){
	 $TriDW->bind("<Motion>",[ \&setfocus, Ev('x'),Ev('y')]); 
	 $TriDW->bind("<Double-Button>",'');
  }else{
	 $TriDW->bind("<Motion>",'');
	 $TriDW->bind("<Double-Button>",[ \&setfocus, Ev('x'),Ev('y')]); 
  }
}

sub setfocus{
  my($this,$x,$y)=@_;

  $y = $TriDW->{GLwin}{Height}-$y;
  
  my $num=0;

  foreach my $vp (@{$TriDW->{GLwin}->viewports()}){ 
    if($vp->{X0}+4 <= $x && $vp->{X0}+$vp->{W}-4>=$x 
	    && $vp->{Y0}+4 <= $y && $vp->{Y0}+$vp->{H}-4>=$y ){
		next if($vp->{Active}==1);
	   $vp->{Active} = 1;
      $TriDW->{GLwin}->current_viewport($num);

		$TriDW->refresh();

    }else{
	   $vp->{Active} = 0;
    }
    $num++;
  }
}
1;
