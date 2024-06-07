=encoding UTF-8

=head1 NAME

PDL::Graphics::TriD::Objects - Simple Graph Objects for TriD

=head1 SYNOPSIS

  use PDL::Graphics::TriD::Objects;

This provides the following class hierarchy:

  PDL::Graphics::TriD::GObject           (abstract) base class
  ├ PDL::Graphics::TriD::Points          individual points
  ├ PDL::Graphics::TriD::Spheres         fat 3D points :)
  ├ PDL::Graphics::TriD::Lines           separate lines
  ├ PDL::Graphics::TriD::LineStrip       continuous paths
  ├ PDL::Graphics::TriD::STrigrid        polygons
  │ └ PDL::Graphics::TriD::STrigrid_S    polygons with normals
  └ PDL::Graphics::TriD::GObject_Lattice (abstract) base class
    ├ PDL::Graphics::TriD::SCLattice     colored lattice
    ├ PDL::Graphics::TriD::SLattice      ...with color per vertex
    └ PDL::Graphics::TriD::SLattice_S    ...and with normals

=head1 DESCRIPTION

This module contains a collection of classes which represent graph
objects.  It is for internal use and not meant to be used by PDL
users.  GObjects can be either stand-alone or in Graphs, scaled
properly.  All the points used by the object must be in the member
{Points}.  I guess we can afford to force data to be copied (X,Y,Z) ->
(Points)...

=head1 OBJECTS

=head2 PDL::Graphics::TriD::GObject

Inherits from base PDL::Graphics::TriD::Object and adds fields Points,
Colors and Options.

=cut

package PDL::Graphics::TriD::GObject;
use strict;
use warnings;
use base qw/PDL::Graphics::TriD::Object/;
use fields qw/Points Colors Options/;

$PDL::Graphics::TriD::verbose //= 0;

sub new {
	my($type,$points,$colors,$options) = @_;
	print "GObject new.. calling SUPER::new...\n" if($PDL::Graphics::TriD::verbose);
	my $this = $type->SUPER::new();
	print "GObject new - back (SUPER::new returned $this)\n" if($PDL::Graphics::TriD::verbose);
	if(!defined $options and ref $colors eq "HASH") {
		$options = $colors;
		undef $colors;
	}
	$options = { $options ? %$options : () };
	$options->{UseDefcols} = 1 if !defined $colors; # for VRML efficiency
	$this->{Options} = $options;
	$this->check_options;
	print "GObject new - calling realcoords\n" if($PDL::Graphics::TriD::verbose);
	$this->{Points} = $points = PDL::Graphics::TriD::realcoords($type->r_type,$points);
	print "GObject new - back from  realcoords\n" if($PDL::Graphics::TriD::verbose);
	$this->{Colors} = defined $colors
	  ? PDL::Graphics::TriD::realcoords("COLOR",$colors)
	  : $this->cdummies(PDL->pdl(1,1,1),$points);
	print "GObject new - returning\n" if($PDL::Graphics::TriD::verbose);
	return $this;
}

sub check_options {
	my($this) = @_;
	my $opts = $this->get_valid_options();
	$this->{Options} = $opts, return if !$this->{Options};
	print "FETCHOPT: $this ".(join ',',%$opts)."\n" if $PDL::Graphics::TriD::verbose;
	my %newopts = (%$opts, %{$this->{Options}});
	my @invalid = grep !exists $opts->{$_}, keys %newopts;
	die "Invalid options left: @invalid" if @invalid;
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

sub get_valid_options { +{UseDefcols => 0} }
sub get_points { $_[0]{Points} }
sub cdummies { $_[1] }
sub r_type { "" }
sub defcols { $_[0]{Options}{UseDefcols} }

# In the future, have this happen automatically by the ndarrays.
sub data_changed {
	my($this) = @_;
	$this->changed;
}

package PDL::Graphics::TriD::Points;
use base qw/PDL::Graphics::TriD::GObject/;
sub get_valid_options {
	return {UseDefcols => 0, PointSize=> 1};
}

package PDL::Graphics::TriD::Spheres;
use base qw/PDL::Graphics::TriD::GObject/;
# need to add radius
sub get_valid_options {
  +{UseDefcols => 0, PointSize=> 1}
}

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

package PDL::Graphics::TriD::STrigrid;
use base qw/PDL::Graphics::TriD::GObject/;
sub new {
  my($type,$points,$faceidx,$colors,$options) = @_;
  # faceidx is 2D pdl of indices into points for each face
  if(!defined $options and ref $colors eq "HASH") {
    $options = $colors;undef $colors; } 
  $points = PDL::Graphics::TriD::realcoords($type->r_type,$points);
  my $faces = $points->dice_axis(1,$faceidx->flat)->splitdim(1,3);
  # faces is 3D pdl slices of points, giving cart coords of face verts
  if(!defined $colors) { $colors = PDL->pdl(0.8,0.8,0.8);
    $colors = $type->cdummies($colors,$faces);
    $options->{ UseDefcols } = 1; } # for VRML efficiency
  else { $colors = PDL::Graphics::TriD::realcoords("COLOR",$colors); }
  my $this = bless { Points => $points, Faceidx => $faceidx, Faces => $faces,
                     Colors => $colors, Options => $options},$type;
  $this->check_options;
  $this;
}
sub get_valid_options { { UseDefcols => 0, Lines => 1 } }
sub cdummies { # called with (type,colors,faces)
  return $_[1]->dummy(1,$_[2]->getdim(2))->dummy(1,$_[2]->getdim(1)); }

package PDL::Graphics::TriD::STrigrid_S;
use base qw/PDL::Graphics::TriD::STrigrid/;
sub new {
  my $this = shift->SUPER::new(@_);
  $this->{Normals} //= $this->smoothn if $this->{Options}{Smooth};
  $this;
}
sub get_valid_options { { UseDefcols=>0, Lines=>0, Smooth=>1, ShowNormals=>0 } }
sub smoothn { my ($this) = @_;
  my ($points, $faces, $faceidx) = @$this{qw(Points Faces Faceidx)};
  my @p = $faces->mv(1,-1)->dog;
  my $fn = ($p[1]-$p[0])->crossp($p[2]-$p[1])->norm; # flat faces, >= 3 points
  $this->{FaceNormals} = $fn if $this->{Options}{ShowNormals};
  PDL::cat(
    map $fn->dice_axis(1,($faceidx==$_)->whichND->slice('(1)'))->mv(1,0)->sumover->norm,
        0..($points->dim(1)-1) );
}

package PDL::Graphics::TriD::GObject_Lattice;
use base qw/PDL::Graphics::TriD::GObject/;
sub r_type {return "SURF2D";}
sub get_valid_options { return {UseDefcols => 0,Lines => 1}; }

package PDL::Graphics::TriD::Lattice;
use base qw/PDL::Graphics::TriD::GObject_Lattice/;
sub cdummies { return $_[1]->dummy(1)->dummy(1); }

# colors associated with surfaces
package PDL::Graphics::TriD::SCLattice;
use base qw/PDL::Graphics::TriD::GObject_Lattice/;
sub cdummies { return $_[1]->dummy(1,$_[2]->getdim(2)-1)
			-> dummy(1,$_[2]->getdim(1)-1); }

# colors associated with vertices, smooth
package PDL::Graphics::TriD::SLattice;
use base qw/PDL::Graphics::TriD::GObject_Lattice/;
sub cdummies { return $_[1]->dummy(1,$_[2]->getdim(2))
			-> dummy(1,$_[2]->getdim(1)); }

# colors associated with vertices
package PDL::Graphics::TriD::SLattice_S;
use base qw/PDL::Graphics::TriD::GObject_Lattice/;
use fields qw/Normals/;
sub cdummies {
  $_[1]->slice(":," . join ',', map "*$_", ($_[2]->dims)[1,2])
}
sub get_valid_options {
  {UseDefcols => 0,Lines => 1, Smooth => 1}
}
sub new {
  my ($class,$points,$colors,$options) = @_;
  my $this = $class->SUPER::new($points,$colors,$options);
  $this->{Normals} //= $this->smoothn($this->{Points}) if $this->{Options}{Smooth};
  $this;
}
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
