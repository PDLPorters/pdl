=head1 NAME

PDL::Graphics::TriD::VRML -- TriD VRML backend

=head1 SYNOPSIS

  BEGIN { $PDL::Graphics::TriD::device = "VRML"; }

  use PDL::Graphics::TriD;
  use PDL::LiteF;

  # set some vrml parameters
  my $set = tridsettings(); # get the defaults
  $set->browser_com('netscape/unix');
  $set->compress();
  $set->file('/www-serv/vrml/dynamic_scene.wrl.gz');

  line3d([$x,$y,$z]); # plot some lines and view the scene with a browser

=head1 DESCRIPTION

This module implements the VRML for PDL::Graphics::TriD (the generic
3D plotting interface for PDL). You can use this backend either (a)
for generating 3D graphics on your machine which can be directly
viewed with a VRML browser or (b) generate dynamic VRML worlds to
distribute over the web.

With VRML, you can generate objects for everyone to see with e.g.
Silicon Graphics' Cosmo Player. You can find out more about VRML
at C<http://vrml.sgi.com/> or C<http://www.vrml.org/>

=cut

#'

###################################
##
package PDL::Graphics::TriD::VRML;

use PDL::Core '';  # barf
use PDL::Graphics::VRML;
use PDL::LiteF;
PDL::Graphics::VRMLNode->import();
PDL::Graphics::VRMLProto->import();

$PDL::homepageURL = 'http://pdl.perl.org/';

sub PDL::Graphics::TriD::Logo::tovrml {
   my ($this) = @_;
   my ($p,$tri) = ("","");
   PDL::Graphics::VRMLPdlNode::v3array($this->{Points},\$p,"");
   PDL::Graphics::VRMLPdlNode::triangles((map
                     {$this->{Index}->slice("($_)")} (0..2)),\$tri,"");
   my $indface = vrn('IndexedFaceSet',
                    'coord' => vrn('Coordinate',
                                   'point' => "[ $p ]"),
                    'coordIndex' => "[ $tri ]",
                    'solid' => 'TRUE');
   return vrn('Transform',
              'children' => [vrn('Anchor',
                                 'description' => "\"The PDL Homepage\"",
                                 'url' => "\"$PDL::homepageURL\"",
                                 'children' =>
				      vrn('Shape',
					  'appearance' =>
					    vrn('Appearance',
						'material' =>
						  $this->{Material}->tovrml),
					  'geometry' => $indface)),
			vrn(Viewpoint,
				position => '0 0 25',
				description => "\"PDL Logo\""
			)

		],
              'translation' => vrml3v($this->{Pos}),
              'scale'    => vrml3v([map {$this->{Size}} (0..2)]));
}

sub PDL::Graphics::TriD::Description::tovrml {
	my($this) = @_;
#	print "DESCRTIPTION : TOVRML\n";
	return vrn(Transform,
	 	rotation => '1 0.1 0 1.1',
		translation => '1.5 0 0.5',
		children => [
		vrn(Shape,
			geometry => vrn(Text,
					string => $this->{TText},
					fontStyle =>     vrn(FontStyle,
							    'family' => "\"SANS\"",
							   size => '0.075',
							   spacing => '1.33',
							   justify => '["BEGIN","MIDDLE"]'
							 ),
			),
			appearance => vrn(Appearance,
				material => vrn(Material,
					   diffuseColor => '0.9 0.9 0.9',
					   ambientIntensity => '0.1'
				)
			)
		),
		vrn(Viewpoint,
			position => '0 0 3',
			description => "\"Description\""
		)
		]
	);
}

sub PDL::Graphics::VRML::vrmltext {
  my ($this,$text,$coords) = @_;
  $this->uses('TriDGraphText');
  return vrn('TriDGraphText',
	     'text' => "\"$text\"",
	     'position' => vrml3v($coords));
}

sub PDL::Graphics::TriD::Material::tovrml {
  my $this = shift;
  my $ambi = (pdl(@{$this->{Ambient}})**2)->sum /
    (pdl(@{$this->{Diffuse}})**2)->sum;
  $ambi = sqrt($ambi);
  new PDL::Graphics::VRMLNode('Material',
			'diffuseColor' => vrml3v($this->{Diffuse}),
			'emissiveColor' => vrml3v($this->{Emissive}),
			'shininess' => $this->{Shine},
			'ambientIntensity' => $ambi,
			'specularColor' => vrml3v($this->{Specular}),
		     );
}

sub PDL::Graphics::TriD::Scale::tovrml {my ($this) = @_;
	print "Scale ",(join ',',@{$this->{Args}}),"\n";
	new PDL::Graphics::VRMLNode('Transform',
		   'scale',vrml3v(@{$this->{Args}}));
    }


sub PDL::Graphics::TriD::Translation::tovrml {
  my ($this) = @_;
  new PDL::Graphics::VRMLNode('Transform',
		   'translation',vrml3v(@{$this->{Args}}));
}

# XXXXX this has to be fixed -> wrap in one transform + children
sub PDL::Graphics::TriD::Transformation::tovrml {
	my($this) = @_;
	my @nodes = map {$_->tovrml()} @{$this->{Transforms}};
	push @nodes,$this->SUPER::tovrml();
}


sub PDL::Graphics::TriD::Quaternion::tovrml {my($this) = @_;
	if(abs($this->[0]) == 1) { return ; }
	if(abs($this->[0]) >= 1) {
		# die "Unnormalized Quaternion!\n";
		$this->normalize_this();
	}
	new PDL::Graphics::VRMLNode('Transform',
		   'rotation',vrml3v(@{$this}[1..3])." $this->[0]");
}


# this 'poor mans viewport' implementation makes an image from its objects
# and writes it as a gif file
sub PDL::Graphics::TriD::ViewPort::togif_vp {
  require PDL::IO::Pic;
  my ($this,$win,$rec,$file) = @_;
  my $p;
  # this needs more thinking
  for (@{$this->{Objects}}) {
    barf "can't display object type" unless $_->can('toimage');
    $p = $_->toimage;
  }
  $p->wpic($file);
}

sub PDL::Graphics::TriD::GObject::tovrml {
	return $_[0]->vdraw($_[0]->{Points});
}

sub PDL::Graphics::TriD::GObject::tovrml_graph {
	return $_[0]->vdraw($_[2]);
}

sub PDL::Graphics::TriD::Points::vdraw {
	my($this,$points) = @_;
	new PDL::Graphics::VRMLNode('Shape',
			 'geometry' =>
			 new PDL::Graphics::VRMLPdlNode($points,$this->{Colors},
					     {Title => 'PointSet',
					     DefColors => $this->defcols}));
}

sub PDL::Graphics::TriD::LineStrip::vdraw {
	my($this,$points) = @_;
	new PDL::Graphics::VRMLNode('Shape',
			 'geometry' =>
			 new PDL::Graphics::VRMLPdlNode($points,$this->{Colors},
					     {Title => 'IndexedLineSet',
					      DefColors => $this->defcols}));
}

sub PDL::Graphics::TriD::Lattice::vdraw {
	my($this,$points) = @_;
	new PDL::Graphics::VRMLNode('Shape',
			 'geometry' =>
			 new PDL::Graphics::VRMLPdlNode($points,$this->{Colors},
					     {Title => 'IndexedLineSet',
					      DefColors => $this->defcols,
					      IsLattice => 1}));
}

sub PDL::Graphics::TriD::SLattice::vdraw {
	my($this,$points) = @_;
	my $children = [vrn('Shape',
		'geometry' =>
		 new PDL::Graphics::VRMLPdlNode($points,$this->{Colors},
				      {Title => 'IndexedFaceSet',
				       DefColors => $this->defcols,
				       IsLattice => 1,
				      }))];
	push @$children, vrn('Shape',
		 'geometry' =>
		 new PDL::Graphics::VRMLPdlNode($points,$this->{Colors},
				      {Title => 'IndexedLineSet',
				       DefColors => 0,
				       Surface => 1,
				       Lines => 1,
				       IsLattice => 1,
				      }))
	  if $this->{Options}->{Lines};
	vrn('Group',
	    'children' => $children);
}

sub PDL::Graphics::TriD::SLattice_S::vdraw {
	my($this,$points) = @_;
   my $vp =  &PDL::Graphics::TriD::get_current_window()->current_viewport;
	my $mat = $vp->{DefMaterial}->tovrml;
	my $children = [vrn('Shape',
		 'appearance' => vrn('Appearance',
				    'material' => $mat),
		'geometry' =>
		 new PDL::Graphics::VRMLPdlNode($points,$this->{Colors},
				      {Title => 'IndexedFaceSet',
				       DefColors => 1,
				       IsLattice => 1,
				       Smooth => $this->{Options}->{Smooth},
				      }))];
	push @$children, vrn('Shape',
		 'geometry' =>
		 new PDL::Graphics::VRMLPdlNode($points,$this->{Colors},
				      {Title => 'IndexedLineSet',
				       DefColors => 0,
				       Surface => 1,
				       Lines => 1,
				       IsLattice => 1,
				      }))
	  if $this->{Options}->{Lines};
	vrn('Group',
	    'children' => $children);
}

##################################
# PDL::Graphics::TriD::Image
#
#

sub PDL::Graphics::TriD::Image::tovrml {
	$_[0]->vdraw();
}

sub PDL::Graphics::TriD::Image::tovrml_graph {
	&PDL::Graphics::TriD::Image::tovrml;
}


# The quick method is to use texturing for the good effect.
# XXXXXXXXXXXX wpic currently rescales $im 0..255, that's not correct (in $url->save)! fix
sub PDL::Graphics::TriD::Image::vdraw {
  my ($this,$vert) = @_;
  my $p = $this->flatten(0); # no binary alignment
  if(!defined $vert) {$vert = $this->{Points}}
  my $url = new PDL::Graphics::TriD::VRML::URL('image/JPG');
  $url->save($p);
  vrn('Shape',
      'appearance' => vrn('Appearance',
			 'texture' => vrn('ImageTexture',
					   'url' => '"'.$url->totext.'"')),
     'geometry' =>
      vrn('IndexedFaceSet',
	  'coord' => vrn('Coordinate',
			 'point' =>
			   [map {vrml3v([$vert->slice(":,($_)")->list])}
			    (0..3)]),
	  'coordIndex' => '[0, 1, 2, 3, -1]',
	  'solid' => 'FALSE'),
  );
}

sub PDL::Graphics::TriD::Graph::tovrml {
	my($this) = @_;
	my @children = ();
	for(keys %{$this->{Axis}}) {
		if($_ eq "Default") {next}
		push @children, @{$this->{Axis}{$_}->tovrml_axis($this)};
	}
	for(keys %{$this->{Data}}) {
	    push @children,
	     $this->{Data}{$_}->tovrml_graph($this,$this->get_points($_));
	}
	return vrn('Group', 'children' => [@children]);
}


sub PDL::Graphics::TriD::EuclidAxes::tovrml_axis {
  my($this,$graph) = @_;
  my $vrml = $PDL::Graphics::VRML::cur;
  my $lset = vrn('Shape',
		 'geometry' => vrn('IndexedLineSet',
				   'coord',
				   vrn('Coordinate',
				       'point',["0 0 0",
						"1 0 0",
						"0 1 0",
						"0 0 1"]),
				   'coordIndex',["0,1,-1",
						 "0,2,-1",
						 "0,3,-1"]));
  my ($vert,$indx,$j) = ([],[],0);
  my @children = ($lset);
  for $dim (0..2) {
    my @coords = (0,0,0);
    my @coords0 = (0,0,0);
    for(0..2) {
      if($dim != $_) { $coords[$_] -= 0.1 }
    }
    my $s = $this->{Scale}[$dim];
    my $ndiv = 3;
    my $radd = 1.0/$ndiv;
    my $nadd = ($s->[1]-$s->[0])/$ndiv;
    my $nc = $s->[0];
    for(0..$ndiv) {
      push @children, $vrml->vrmltext(sprintf("%.3f",$nc),[@coords]);
      push @$vert,(vrml3v([@coords0]),vrml3v([@coords]));
      push @$indx,$j++.", ".$j++.", -1";
      $coords[$dim] += $radd;
      $coords0[$dim] += $radd;
      $nc += $nadd;
    }
    $coords0[$dim] = 1.1;
    push @children, $vrml->vrmltext($this->{Names}[$dim],[@coords0]);
  }
  push @children, vrn('Shape',
		      'geometry' => vrn('IndexedLineSet',
					'coord' =>
				          vrn('Coordinate',
					      'point' => $vert),
				        'coordIndex' => $indx));
  return [@children];
}

sub PDL::Graphics::TriD::SimpleController::tovrml {
  # World origin is disregarded XXXXXXX
  my $this = shift;
  my $inv = new PDL::Graphics::TriD::Quaternion(@{$this->{WRotation}});
  $inv->invert_rotation_this;
  my $pos = $inv->rotate([0,0,1]);
#  print "SC: POS0:",(join ',',@$pos),"\n";
  for (@$pos) { $_ *=  $this->{CDistance}}
#  print "SC: POS:",(join ',',@$pos),"\n";
# ASSUME CRotation 0 for now
  return vrn('Viewpoint',
	     'position' => vrml3v($pos),
#	     'orientation' => vrml3v(@{$this->{CRotation}}[1..3]).
#	                " $this->{CRotation}->[0]",
	     'orientation' => vrml3v([@{$inv}[1..3]])." ".
	     		-atan2(sqrt(1-$this->{WRotation}[0]**2),
				$this->{WRotation}[0]),
	     'description' => "\"Home\"");
}


package Win32;

sub Win32::fn_win32_format {
  my ($file) = @_;
  $file =~ s|\\|/|g;
  $file = "//$file" if $file =~ m|^[a-z,A-Z]+:|;
  return $file;
}

package Win32::DDE::Netscape;
use PDL::Core '';  # barf
require Win32::DDE::Client if $^O =~ /win32/i;


sub checkerr {
  my $this = shift;
  if ($this->Error) {
    print Win32::DDE::ErrorText($this->Error), "\n# ",
    $this->ErrorText;
    barf "client: couldn't connect to netscape";
  }
  return $this;
}

sub activate {
  my $client = new Win32::DDE::Client ('Netscape','WWW_Activate');
  checkerr($client);
  $client->Request('0xFFFFFFFF,0x0');
  barf "can't disconnect" unless $client->Disconnect;
}

sub geturl {
  my ($url) = @_;
  my $client = new Win32::DDE::Client ('Netscape','WWW_OpenURL');
  checkerr($client);
  $status = $client->Request("\"$url\",,0xFFFFFFFF,0x1");
  barf "can't disconnect" unless $client->Disconnect;
}

package PDL::Graphics::TriD::VRML::Parameter;
use PDL::Core '';  # barf

sub new {
  my ($type,%hash) = @_;
  my $this = bless {},$type;
  $this->{Mode} = 'VRML';
  for (keys %hash) { $this->{$_} = $hash{$_} }
  return $this;
}

sub gifmode {
  my ($this) = @_;
  $this->{Mode} = 'GIF';
}

sub vrmlmode {
  my ($this) = @_;
  $this->{Mode} = 'VRML';
}

sub set {
  my ($this,%hash) = @_;
  for (keys %hash) { $this->{$_} = $hash{$_} }
  return $this;
}

sub browser {
  my ($this) = @_;
  $this->{'Browser'} = $_[1] if $#_ > 0;
  return $this->{'Browser'};
}

sub file {
  my ($this) = @_;
  if ($#_ > 0) {
    $this->{'GifFile'} = $_[1];
    $this->{'GifFile'} =~ s/[.][^.]+$/.gif/;
    $this->{'HTMLFile'} = $_[1];
    $this->{'HTMLFile'} =~ s/[.][^.]+$/.html/;
    $this->{'File'} = $_[1];
    $this->{'File'} =~ s/[.][^.]+$/.wrl/;
    }
  if ($this->{Mode} eq 'VRML') {
    return $this->{'File'};
  } elsif ($this->{Mode} eq 'GIF') {
    return $this->{'HTMLFile'};
  } else {
    barf "wfile error: unknown mode";
  }
}

sub wfile {
  my ($this) = @_;
  my $file = $this->{Mode} eq 'GIF' ? $this->{GifFile} : $this->{File};
  if (defined $this->{Compress} && $this->{Compress}) {
    $file .= '.gz' unless $file =~ /[.]gz$/; $this->file($file);
    $file = '|gzip -c' . ($file =~ /^\s*>/ ? '' : '>') . $file;
  }
  return $file;
}


$PDL::Graphics::TriD::VRML::Parameter::lastfile = '';

my %subs = (
	    'netscape/unix' =>
	      sub {my $file = $_[0]->file; my $cmd;
		   if ($file eq
		       $PDL::Graphics::TriD::VRML::Parameter::lastfile)
		     { $cmd = 'reload' }
		   else { my $target = $#_ > 0 ? "#$_[1]" : '';
			  $cmd = "openURL(file:$file$target)"}
		   system('netscape','-remote',$cmd);
		 $PDL::Graphics::TriD::VRML::Parameter::lastfile = $file},
	    'netscape/win32' =>
	     sub {my $file = $_[0]->file; $file = Win32::fn_win32_format $file;
		  Win32::DDE::Netscape::activate;
		  my $target = $#_ > 0 ? "#$_[1]" : '';
		  Win32::DDE::Netscape::geturl("file:$file$target");
		},
	    'none' => sub {print STDERR "not sending it anywhere\n"},
);

sub browser_com {
  my ($this,$browser) = @_;
  barf("unknown browser '$browser'") unless defined $subs{$browser};
  $this->{'Browser'} = $subs{$browser};
}

sub send_to_browser {my $this=$_[0]; &{$this->{'Browser'}}(@_)
		       if defined $this->{'Browser'}}


package PDL::Graphics::TriD::VRML::URL;
use PDL::Core '';  # barf

my %types = (
	     'image/JPG' => {'save' => sub {local $PDL::debug=0; $_[1]->wpic($_[0]->wfile)},
			     'ext'  => 'jpg',
			     'setup' => sub {require PDL::IO::Pic},
			    },
);

my $urlnum = 0;

sub new {
  my ($type,$mime) = @_;
  my $this = bless {},$type;
  barf "unknown mime type '$mime'" unless defined $types{$mime};
  $this->{'Type'} = $types{$mime};
  &{$this->{'Type'}->{'setup'}} if defined $this->{'Type'}->{'setup'};
  $this->{'Binding'} = 'local';
  $this->{'Filestem'} = ($ENV{'TMP'} || $ENV{'TEMP'} || '/tmp') .
    "/tridim_$urlnum"; $urlnum++;
  return $this;
}

sub wfile {
  my ($this) = @_;
  return $this->{'Filestem'}.'.'.$this->{'Type'}->{'ext'};
}

sub totext {
  my ($this) = @_;
  my $proto;
  if ($this->{'Binding'} eq 'local') { $proto = 'file' }
  elsif ($this->{'Binding'} eq 'publish') {
    $proto = 'http'; barf "not yet implemented" }
  else { barf "unknown binding" }
  return "$proto:".$this->wfile;
}

sub save { &{$_[0]->{Type}->{save}}(@_) }

package PDL::Graphics::TriD::VRML;
$PDL::Graphics::VRML::cur = undef;
$PDL::Graphics::TriD::create_window_sub = sub {
	return new PDL::Graphics::TriD::Window;
};

# set up the default parameters for VRML
my $tmpdir = $ENV{'TMP'} || $ENV{'TEMP'} || '/tmp';
my $tmpname = "$tmpdir/tridvrml_$$.wrl";
my $para = $PDL::Graphics::TriD::Settings =
  PDL::Graphics::TriD::VRML::Parameter->new() ;
$para->file($tmpname);
$para->browser_com($^O =~ /win32/i ? 'netscape/win32' : 'none');


package PDL::Graphics::TriD::VRMLObject;
use base qw/PDL::Graphics::TriD::Object/;
use fields qw/Node/;

sub new {
  my($type,$node) = @_;
  my $this = $type->SUPER::new();
  $this->{Node} = $node;
  return $this;

}

sub tovrml {
  return $_[0]->{Node};
}

#package PDL::Graphics::TriD::VRML::Window;
package PDL::Graphics::TriD::Window;

use PDL::Graphics::TriD::Control3D;
PDL::Graphics::VRMLNode->import();
PDL::Graphics::VRMLProto->import();
use PDL::Core '';  # barf

use base qw/PDL::Graphics::TriD::Object/;
use fields qw/Width Height Interactive _ViewPorts _CurrentViewPort
              VRMLTop DefMaterial/;
use strict;

sub gdriver {
  my($this) = @_;

  require PDL::Version if not defined $PDL::Version::VERSION;
  $this->{Width} = 300; $this->{Height} = 300;
  $this->{VRMLTop} = new PDL::Graphics::VRML("\"PDL::Graphics::TriD::VRML Scene\"",
				  ["\"generated by the PDL::Graphics::TriD module\"",
				   "\"version $PDL::Version::VERSION\""]);
  my $fontstyle = new PDL::Graphics::VRMLNode('FontStyle',
				    'size' => 0.04,
				    'family' => "\"SANS\"",
				    'justify' => "\"MIDDLE\"");
  $PDL::Graphics::TriD::VRML::fontstyle = $fontstyle;
  $this->{VRMLTop}->add_proto(PDL::Graphics::TriD::SimpleController->new->tovrml);
  $PDL::Graphics::VRML::cur = $this->{VRMLTop};
  $this->{VRMLTop}->register_proto(
	    vrp('TriDGraphText',
		[fv3f('position',"0 0 0"),
		 fmstr('text')],
		vrn('Transform',
		    'translation' => "IS position",
		    'children' =>
		      [vrn('Billboard',
			  'axisOfRotation' => '0 0 0',
			  'children' =>
			    [vrn('Shape',
			   'geometry' =>
			       vrn('Text',
				   'string' => "IS text",
				   'fontStyle' => $fontstyle))])])));
  return 0;
}

#sub set_material {
#  $_[0]->{DefMaterial} = $_[1];
#}

# we only allow [0,0,1,1] viewports and just write a gif of the write size
# for any children
sub new_viewport {
	my($this,$x0,$y0,$x1,$y1) = @_;
	# print STDERR "Installing new viewport\n";
	barf "only allowing [0,1,0,1] viewports with VRML backend"
	  if abs(PDL->pdl($x0,$y0,$x1-1,$y1-1))->max > 0.01;

	my $vp = new PDL::Graphics::TriD::ViewPort($x0,$y0,$x1,$y1);
	push @{$this->{_ViewPorts}},$vp;
	return $vp;
}

sub clear_viewports {
	my($this) = @_;
	$this->{_ViewPorts} = [];
}


sub display {
  my $this = shift;
  my $vrmlparam =  $PDL::Graphics::TriD::Settings;

#  if (@{$this->{_ViewPorts}}) {
  if (0) {
    # show the image
    $vrmlparam->gifmode();
    # print STDERR "writing a GIF image\n";
    # print STDERR "Filename: ",$vrmlparam->wfile,"\n";
    for(@{$this->{_ViewPorts}}) {
      $_->togif_vp($this,$_,$vrmlparam->wfile);
    }
    my ($hfile,$gfile) = ($vrmlparam->file,$vrmlparam->wfile);
    $hfile = '>'.$hfile unless $hfile =~ /^\s*[>|]/;
    $gfile = Win32::fn_win32_format($gfile) if $^O =~ /win32/i;
    open HTML, $hfile or barf "couldn't open html file $hfile";
    print HTML <<"EOH";

<HTML>
<HEAD>
   <TITLE> PDL::Graphics::TriD Display </TITLE>
   <META NAME="GENERATOR" CONTENT="PDL::Graphics::TriD::VRML">
</HEAD>
<BODY>
<TD align="center"><IMG SRC="$gfile" ALT="Gif image" HEIGHT=$this->{H}
    WIDTH=$this->{W}></TD>
</BODY>
</HTML>

EOH
    close HTML;
    $vrmlparam->send_to_browser();
  } else {
    # a 'normal' world
    # print STDERR "printing a VRML world\n";
    # print STDERR "Filename: ",$vrmlparam->wfile,"\n";
    my $vp = $this->current_viewport;
    $vp->tovrml;
    if ($vp->{Transformer}) {
      $this->{VRMLTop}->addview($vp->{Transformer}->tovrml)
    }

    $this->{VRMLTop}->ensure_protos();

#    use Data::Dumper;
#    my $out = Dumper($this->{VRML});
#    print $out;




    $this->{VRMLTop}->set_vrml($vp->{VRML});
    $vrmlparam->vrmlmode();
    local $| = 1;
    print "*********starting output\n";
    $this->{VRMLTop}->print($vrmlparam->wfile);
    print "*********finished output\n";
    $vrmlparam->send_to_browser('Home'); #XXX make target selectable
  }
}

sub twiddle {
  my $this = shift;
  if ($PDL::Graphics::TriD::keeptwiddling) {
    $this->display();
    print "---- (press enter)";
    <>
  }
  # should probably wait for input of character 'q' ?
}

package PDL::Graphics::TriD::ViewPort;
use base qw/PDL::Graphics::TriD::Object/;
use fields qw/X0 Y0 W H Transformer EHandler Active ResizeCommands 
              DefMaterial AspectRatio Graphs/;




1;

=head1 BUGS

Probably incomplete/buggy implementation of some TriD features.

=head1 AUTHOR

Copyright (C) 1997, 1998 Christian Soeller (c.soeller@auckland.ac.nz).
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut
