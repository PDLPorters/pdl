# XXXXX print methods need to be changed to
# reduce memory consumption

###################
#
# VRMLProto

package PDL::Graphics::VRMLProto;
use Exporter;
use PDL::Core '';

@ISA = qw/ Exporter /;
@EXPORT = qw/ vrp fv3f fmstr /;

sub new {
  my $type = shift;
  my ($name,$fields,$node) = @_;
  my $this = bless {},$type;
  $this->{Name} = $name;
  $this->{Fields} = $fields;
  $this->{Node} = $node;
  return $this;
}

sub vrp {
  return PDL::Graphics::VRMLProto->new(@_);
}

sub fv3f {
  my ($name,$def) = @_;
  return ["field SFVec3f", "$name", "$def"];
}

sub fmstr {
  my ($name,$def) = @_;
  return ["field MFString", "$name", defined $def ? "$def" : "[]"];
}

sub to_text {
  my $this = shift;
  my $text = "PROTO $this->{Name} [\n";
  for (@{$this->{Fields}}) {
    $text .= "  $_->[0] $_->[1]\t$_->[2]\n";
  }
  $text .= "]\n{\n";
  $text .= $this->{Node}->to_text;
  return $text . "}\n";
}

#####################
#
# VRMLNode

package PDL::Graphics::VRMLNode;
use Exporter;

@ISA = qw/ Exporter /;
@EXPORT = qw/ vrn vrml3v /;
@EXPORT_OK = qw/ tabs postfix prefix /;

sub vrn {
  return PDL::Graphics::VRMLNode->new(@_);
}

sub new {
  my $type = shift;
  my $title = shift;
  my $this = bless {},$type;
  $this->{'Container'} = {};
  $this->{'Title'} = $title;
  $this->add(@_);
  return $this;
}

sub add {
  my ($this,%items) = @_;
  for (keys %items) {
    $this->{Container}{$_} = $items{$_};
  }
  return $this;
}

sub add_children {
  my ($this) = shift;
  for(@_) {
  	push @{$this->{Container}{children}}, $_;
  }
}

sub to_text {
  my $this = shift;
  my $level = $#_ > -1 ? shift : 1;
  my $text = $this->prefix($level);
  my($k,$v);
  while (($k,$v) = each %{$this->{Container}}) {
    $text .= tabs($level) . "$k".
      (ref $v ? 
          ref $v eq "ARRAY" ?
            $this->array_out($v,$level+1) :
            (" ".$v->to_text($level+1)) :
          "\t$v\n");
  }
  return $text.$this->postfix($level);
}

sub array_out {
  my ($this,$array,$level) =  @_;
  my $text = " [\n";
  for (@$array) {
    $text .= tabs($level) . (ref $_ ?
      $_->to_text($level+1) : "$_,\n")
  }
  $text .= tabs($level-1) . "]\n";
  return $text;
}

sub prefix {
  return $_[0]->{Title}." {\n";
}

sub postfix {
  return "\t"x($_[1]-1)."}\n";
}

sub tabs {
  return "\t"x$_[0];
}

sub vrml3v {
  my $list = shift;
  return sprintf '%.3f %.3f %.3f', @{$list}[0..2];
}

#################
#
# VRMLPdlNode

package PDL::Graphics::VRMLPdlNode;
@ISA = qw/ PDL::Graphics::VRMLNode /;
use PDL::Lite;
use PDL::Dbg;
PDL::Graphics::VRMLNode->import(qw/tabs vrml3v postfix prefix/);

sub new {
  my ($type,$points,$colors,$options) = @_;
  my $this = bless {},$type;
  $this->{'Points'} = $points;
  $this->{'Colors'} = $colors;
  $this->checkoptions($options);
  return $this;
}

sub checkoptions {
  my ($this,$options) = @_;
  my $aopts = $this->getvopts();
  for (keys %$aopts) {
    if (!defined $options->{$_}) {
      $this->{$_} = $aopts->{$_};
    } else {
      $this->{$_} = delete $options->{$_};
    }
  }

  if (keys %$options) {
    barf "Invalid options left: ".(join ',',%$options);
  }
}

sub getvopts {
  my ($this) = @_;
  return {Title => 'PointSet',
	  PerVertex => 0,
	  Lighting => 0,
	  Surface => 0,
	  Lines => 1,
	  Smooth => 0,
	  IsLattice => 0,
	  DefColors => 0};
}

sub to_text {
  my $this = shift;
  my $level = $#_ > -1 ? shift : 1;
  my $text = $this->prefix($level);
  my ($vtxt,$vidx,$ctxt,$extra,$useidx) = ("","","","",0);
  if ($this->{Title} eq 'PointSet') {
    coords($this->{Points},$this->{Colors},\$vtxt,\$ctxt,tabs($level+2));
  } elsif ($this->{Title} eq 'IndexedLineSet') {
    my @dims = $this->{Points}->dims;
    shift @dims;
    my $cols = $this->{Colors};
    my $seq = PDL->sequence(@dims);
    require PDL::Dbg;
    local $PDL::debug = 0;
    $cols = pdl(0,0,0)->dummy(1)->dummy(2)->px
      if $this->{IsLattice} && $this->{Surface} && $this->{Lines};
    lines($this->{Points},$cols,$seq,
	  \$vtxt,\$ctxt,\$vidx,tabs($level+1));
    lines($this->{Points}->xchg(1,2),$cols->xchg(1,2),
	  $seq->xchg(0,1),undef,\$ctxt,\$vidx,
	  tabs($level+1)) if $this->{IsLattice};
    $useidx = 1;
  } elsif ($this->{Title} eq 'IndexedFaceSet') {
    my @dims = $this->{Points}->dims;
    shift @dims;
    my @sls1 = ("0:-2,0:-2",
		"1:-1,0:-2",
		"0:-2,1:-1");
    my @sls2 = ("1:-1,1:-1",
		"0:-2,1:-1",
		"1:-1,0:-2"
	       );
    my $seq = PDL->sequence(@dims);
    coords($this->{Points},$this->{Colors},\$vtxt,\$ctxt,tabs($level+2));
    triangles((map {$seq->slice($_)} @sls1),\$vidx,tabs($level+1));
    triangles((map {$seq->slice($_)} @sls2),\$vidx,tabs($level+1));
    $useidx = 1;
    $extra = tabs($level)."colorPerVertex\tTRUE\n".
      tabs($level)."solid\tFALSE\n";
    $extra .= tabs($level)."creaseAngle\t3.14\n" if $this->{Smooth};
  }
  $text .= vprefix('coord',$level).$vtxt.vpostfix('coord',$level);
  $text .= vprefix('index',$level).$vidx.vpostfix('index',$level)
    if $useidx;
  $text .= vprefix('color',$level).$ctxt.vpostfix('color',$level)
    unless $this->{DefColors};
  return $text.$extra.$this->postfix($level);
}

sub vprefix {
  my ($type,$level) = @_;
  return tabs($level) . "coord Coordinate {\n" . tabs($level+1) .
      "point [\n" if $type eq 'coord';
  return tabs($level) . "color Color {\n" . tabs($level+1) .
      "color [\n" if $type eq 'color';
  return tabs($level) . "coordIndex [\n" if $type eq 'index';
}

sub vpostfix {
  my ($type,$level) = @_;
  return tabs($level+1)."]\n".tabs($level)."}\n" unless $type eq 'index';
  return tabs($level)."]\n";
}

PDL::thread_define 'coords(vertices(n=3); colors(n)) NOtherPars => 3',
  PDL::over {
    ${$_[2]} .= $_[4] . sprintf("%.3f %.3f %.3f,\n",$_[0]->list);
    ${$_[3]} .= $_[4] . sprintf("%.3f %.3f %.3f,\n",$_[1]->list);
};

PDL::thread_define 'v3array(vecs(n=3)) NOtherPars => 2',
  PDL::over {
    ${$_[1]} .= $_[2] . sprintf("%.3f %.3f %.3f,\n",$_[0]->list);
};

PDL::thread_define 'lines(vertices(n=3,m); colors(n,m); index(m))'.
  'NOtherPars => 4',
  PDL::over {
    my ($lines,$cols,$index,$vt,$ct,$it,$sp) = @_;
    v3array($lines,$vt,$sp."\t") if defined $vt;
    v3array($cols,$ct,$sp."\t") if defined $ct;
    $$it .= $sp.join(',',$index->list).",-1,\n" if defined $it;
};

PDL::thread_define 'triangles(inda();indb();indc()), NOtherPars => 2',
  PDL::over {
    ${$_[3]} .= $_[4].join(',',map {$_->at} @_[0..2]).",-1,\n";
};

#####################
#
# VRML

package PDL::Graphics::VRML;
use PDL::Core '';

%PDL::Graphics::VRML::Protos = ();

sub new {
  my ($type,$title,$info) = @_;
  my $this = bless {},$type;
  $this->{Header} = '#VRML V2.0 utf8';
  $this->{Info} = new PDL::Graphics::VRMLNode('WorldInfo',
				    'title' => $title,
				    'info' => $info);
  $this->{NaviInfo} = new PDL::Graphics::VRMLNode('NavigationInfo',
			'type' => '["EXAMINE", "ANY"]');
  $this->{Protos} = {};
  $this->{Uses} = {};
  $this->{Scene} = undef;
  return $this;
}

sub register_proto {
  my ($this,@protos) = @_;
  for (@protos) {
    barf "proto already registered"
      if defined $PDL::Graphics::VRML::Protos{$_->{Name}};
    $PDL::Graphics::VRML::Protos{$_->{Name}} = $_;
  }
}

sub set_vrml {
  print "set_vrml ",ref($_[0]),"\n";

  $_[0]->{Scene} = $_[1];
}

sub uses {
  $_[0]->{Uses}->{$_[1]} = 1;
}

sub ensure_protos {
  my $this = shift;
  for (keys %{$this->{Uses}}) {
    barf "unknown Prototype $_" unless defined $PDL::Graphics::VRML::Protos{$_};
    delete $this->{Uses}->{$_};
    $this->add_proto($PDL::Graphics::VRML::Protos{$_});
  }
}

sub add_proto {
  my ($this,$proto) = @_;
  $this->{Protos}->{$proto->{Name}} = $proto
    unless exists $this->{Protos}->{$proto->{Name}};
  return $this;
}

sub print {
  my $this = shift;
  if ($#_ > -1) {
    my $file = ($_[0] =~ /^\s*[|>]/ ? '' : '>') .$_[0];
    open VRML,"$file" or barf "can't open $file";
  } else { *VRML = *STDOUT }
  print VRML "$this->{Header}\n";
  print VRML $this->{Info}->to_text;
  print VRML $this->{NaviInfo}->to_text;
  for (keys %{$this->{Protos}}) { print VRML $this->{Protos}->{$_}->to_text }
  barf "no scene hierarchy" unless defined $this->{Scene};
  print VRML $this->{Scene}->to_text;
  close VRML if $#_ > -1;
}


1;
