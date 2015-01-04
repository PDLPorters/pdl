=head1 NAME

PDL::Graphics::TriD::Objects - Simple Graph Objects for TriD

=head1 SYNOPSIS

  Look in PDL/Demos/TkTriD_demo.pm for several examples, the code
  in PDL/Demos/TriD1.pm and PDL/Demos/TriD2.pm also uses objects
  but it hides them from the user.

=head1 DESCRIPTION

GObjects can be either stand-alone or in Graphs, scaled properly.
All the points used by the object must be in the member {Points}.
I guess we can afford to force data to be copied (X,Y,Z) -> (Points)...

=head1 OBJECTS

=head2 PDL::Graphics::TriD::GObject

Inherits from base PDL::Graphics::TriD::Object and adds fields Points, Colors and
Options.  Need lots more here...

=cut

package PDL::Graphics::TriD::GObject;
use base qw/PDL::Graphics::TriD::Object/;
use fields qw/Points Colors Options/;

sub new {
	my($type,$points,$colors,$options) = @_;

	print "GObject new.. calling SUPER::new...\n" if($PDL::debug_trid);
	my $this = $type->SUPER::new();
	print "GObject new - back (SUPER::new returned $this)\n" if($PDL::debug_trid);

	if(!defined $options and ref $colors eq "HASH") {
		$options = $colors;
		undef $colors;
	}

	print "GObject new - calling realcoords\n" if($PDL::debug_trid);
	$points = PDL::Graphics::TriD::realcoords($type->r_type,$points);
	print "GObject new - back from  realcoords\n" if($PDL::debug_trid);

	if(!defined $colors) {$colors = PDL->pdl(1,1,1);
		$colors = $type->cdummies($colors,$points);
	        $options->{UseDefcols} = 1;  # for VRML efficiency
	} else {
		$colors = PDL::Graphics::TriD::realcoords("COLOR",$colors);
	}

        $this->{Options} = $options;
	$this->{Points}  = $points;
	$this->{Colors}  = $colors;

	$this->check_options();
	
	print "GObject new - returning\n" if($PDL::debug_trid);
	return $this;
}


sub check_options {
	my($this) = @_;
	my %newopts;
	my $opts = $this->get_valid_options();
	print "FETCHOPT: $this ".(join ',',%$opts)."\n" if $PDL::Graphics::TriD::verbose;
	for(keys %$opts) {
		if(!exists $this->{Options}{$_}) {
			$newopts{$_} = $opts->{$_};
		} else {
			$newopts{$_} = delete $this->{Options}{$_};
		}
	}
	if(keys %{$this->{Options}}) {
		die("Invalid options left: ".(join ',',%{$this->{Options}}));
	}
	$this->{Options} = \%newopts;
}


sub set_colors {
  my($this,$colors) = @_;
  if(ref($colors) eq "ARRAY"){
    $colors = PDL::Graphics::TriD::realcoords("COLOR",$colors);
  }
  $this->{Colors}=$colors;
  $this->data_changed;
}

sub get_valid_options {
	return {UseDefcols => 0};
}

sub get_points {
	return $_[0]->{Points};
}


# In the future, have this happen automatically by the piddles.
sub data_changed {
	my($this) = @_;
	$this->changed();
}

sub cdummies {return $_[1];}

sub r_type { return ""; }

sub defcols {
  return defined($_[0]->{Options}->{UseDefcols}) &&
    $_[0]->{Options}->{UseDefcols};
}
1;

package PDL::Graphics::TriD::Points;
use base qw/PDL::Graphics::TriD::GObject/;
sub get_valid_options {
	return {UseDefcols => 0, PointSize=> 1};
}


package PDL::Graphics::TriD::Spheres;
use base qw/PDL::Graphics::TriD::GObject/;
sub get_valid_options {  # need to add radius
	return {UseDefcols => 0, PointSize=> 1};
}

###########################################################################
################# JNK 15mar11 added section start #########################
# JNK 06dec00 -- edited from PDL::Graphics/TriD/GObject in file Objects.pm
# GObjects can be either stand-alone or in Graphs, scaled properly.
# All the points used by the object must be in the member {Points}.
# I guess we can afford to force data to be copied (X,Y,Z) -> (Points)...
# JNK:  I don't like that last assumption for all cases..

# JNK 27nov00 new object type:
package PDL::Graphics::TriD::GPObject;
# @ISA=qw/PDL::Graphics::TriD::GObject/;
use base qw/PDL::Graphics::TriD::GObject/;
# use fields qw/.../;

sub new { my($type,$points,$faceidx,$colors,$options) = @_;
  # faceidx is 2D pdl of indices into points for each face
  if(!defined $options and ref $colors eq "HASH") {
    $options = $colors;undef $colors; } 
  $points = PDL::Graphics::TriD::realcoords($type->r_type,$points);
  $faces = $points->dice_axis(1,$faceidx->clump(-1))->splitdim(1,3);
  # faces is 3D pdl slices of points, giving cart coords of face verts
  if(!defined $colors) { $colors = PDL->pdl(1,1,1);
    $colors = $type->cdummies($colors,$faces);
    $options->{ UseDefcols } = 1; } # for VRML efficiency
  else { $colors = PDL::Graphics::TriD::realcoords("COLOR",$colors); }
  my $this = bless { Points => $points, Faceidx => $faceidx, Faces => $faces,
                     Colors => $colors, Options => $options},$type;
  $this->check_options();return $this; }

sub get_valid_options {
  return { UseDefcols=>0, Lines=>0, Smooth=>1, Material=>0 }; }  

sub cdummies {
  return $_[1]->dummy(1,$_[2]->getdim(2))->dummy(1,$_[2]->getdim(1)); }
  
# JNK 13dec00 new object type:
package PDL::Graphics::TriD::STrigrid_S;
# @ISA=qw/PDL::Graphics::TriD::GPObject/;
use base qw/PDL::Graphics::TriD::GPObject/;
# use fields qw/.../;

sub cdummies {
  return $_[1]->dummy(1,$_[2]->getdim(2))->dummy(1,$_[2]->getdim(1)); }
    
sub get_valid_options {
  return { UseDefcols=>0, Lines=>0, Smooth=>1, Material=>0 }; }
  
# calculate smooth normals
sub smoothn { my ($this,$ddd) = @_;
  my $v=$this->{Points};my $f=$this->{Faces};my $fvi=$this->{Faceidx};
# ----------------------------------------------------------------------------
  my @p = map { $f->slice(":,($_),:") } (0..(($fvi->dims)[0]-1));
# ----------------------------------------------------------------------------
  # the following line assumes all faces are triangles
  my $fn = ($p[1]-$p[0])->crossp($p[2]-$p[1])->norm;
#   my $vfi = PDL::cat(map {PDL::cat(PDL::whichND($fvi==$_))->slice(':,(1)')}
#                          (0..(($v->dims)[1]-1)));
# the above, spread into several statements:
#   my @vfi2=();for my $idx (0..($v->dims)[1]-1) {
#     my @vfi0=PDL::whichND($fvi==$idx);
#     my $vfi1=PDL::cat(@vfi0);
#     $vfi2[$idx]=$vfi1->slice(':,(1)'); }
#   my $vfi=PDL::cat(@vfi2);
#   my $vmn = $fn->dice_axis(1,$vfi->clump(-1))->splitdim(1,($fvi->dims)[0]);
#   my $vn = $vmn->mv(1,0)->sumover->norm;
# ----------------------------------------------------------------------------
  my $vn=PDL::cat(
    map { my $vfi=PDL::cat(PDL::whichND($fvi==$_))->slice(':,(1)');
          $fn->dice_axis(1,$vfi)->mv(1,0)->sumover->norm }
        (0..(($v->dims)[1]-1)) );
# ----------------------------------------------------------------------------
  return $vn; }
# JNK 06dec00 new object type:
package PDL::Graphics::TriD::STrigrid;
# @ISA=qw/PDL::Graphics::TriD::GPObject/;
use base qw/PDL::Graphics::TriD::GPObject/;
# use fields qw/.../;

sub cdummies { # copied from SLattice_S; not yet modified...
  # called with (type,colors,faces)
  return $_[1]->dummy(1,$_[2]->getdim(2))->dummy(1,$_[2]->getdim(1)); }

sub get_valid_options { # copied from SLattice_S; not yet modified...
  return { UseDefcols => 0, Lines => 1, Smooth => 0, Material => 0 }; }

################# JNK 15mar11 added section finis #########################
###########################################################################   

package PDL::Graphics::TriD::Lattice;
use base qw/PDL::Graphics::TriD::GObject/;

sub r_type {return "SURF2D";}

sub cdummies { return $_[1]->dummy(1)->dummy(1); }

package PDL::Graphics::TriD::Lines;
use base qw/PDL::Graphics::TriD::GObject/;

sub cdummies { return $_[1]->dummy(1); }

sub r_type { return "SURF2D";}

sub get_valid_options { return {UseDefcols => 0, LineWidth => 1}; }

package PDL::Graphics::TriD::LineStrip;
use base qw/PDL::Graphics::TriD::GObject/;

sub cdummies { return $_[1]->dummy(1); }

sub r_type { return "SURF2D";}

sub get_valid_options { return {UseDefcols => 0, LineWidth => 1}; }

package PDL::Graphics::TriD::GObject_Lattice;
use base qw/PDL::Graphics::TriD::GObject/;

sub r_type {return "SURF2D";}

sub get_valid_options { return {UseDefcols => 0,Lines => 1}; }

# colors associated with vertices, smooth
package PDL::Graphics::TriD::SLattice;
use base qw/PDL::Graphics::TriD::GObject_Lattice/;

sub cdummies { return $_[1]->dummy(1,$_[2]->getdim(2))
			-> dummy(1,$_[2]->getdim(1)); }

# colors associated with surfaces
package PDL::Graphics::TriD::SCLattice;
use base qw/PDL::Graphics::TriD::GObject_Lattice/;

sub cdummies { return $_[1]->dummy(1,$_[2]->getdim(2)-1)
			-> dummy(1,$_[2]->getdim(1)-1); }


# colors associated with vertices
package PDL::Graphics::TriD::SLattice_S;
use base qw/PDL::Graphics::TriD::GObject_Lattice/;
use fields qw/Normals/;

sub cdummies { return $_[1]->dummy(1,$_[2]->getdim(2))
			-> dummy(1,$_[2]->getdim(1)); }


sub get_valid_options { return {UseDefcols => 0,Lines => 1, Smooth => 0,
	Material => 0}; }

# calculate smooth normals
sub smoothn {
  my ($this,$p) = @_;
  # coords of parallel sides (left and right via 'lags')
  my $trip = $p->lags(1,1,2)->slice(':,:,:,1:-1') -
		$p->lags(1,1,2)->slice(':,:,:,0:-2');
  # coords of diagonals with dim 2 having original and reflected diags
  my $tmp;
  my $trid = ($p->slice(':,0:-2,1:-1')-$p->slice(':,1:-1,0:-2'))
		    ->dummy(2,2);
  # $ortho is a (3D,x-1,left/right triangle,y-1) array that enumerates
  # all triangles
  my $ortho = $trip->crossp($trid);
  $ortho->norm($ortho); # normalise inplace

  # now add to vertices to smooth
  my $aver = ref($p)->zeroes($p->dims);
  # step 1, upper right tri0, upper left tri1
  ($tmp=$aver->lags(1,1,2)->slice(':,:,:,1:-1')) += $ortho;
  # step 2, lower right tri0, lower left tri1
  ($tmp=$aver->lags(1,1,2)->slice(':,:,:,0:-2')) += $ortho;
  # step 3, upper left tri0
  ($tmp=$aver->slice(':,0:-2,1:-1')) += $ortho->slice(':,:,(0)');
  # step 4, lower right tri1
  ($tmp=$aver->slice(':,1:-1,0:-2')) += $ortho->slice(':,:,(1)');
  $aver->norm($aver);
  return $aver;
}

1;
