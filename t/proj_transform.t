#!/usr/bin/perl

#
# t/proj_transform.t
#
# Test program for the PDL::Transform::Proj4 library
#
# Judd Taylor, USF IMaRS
#
# 30 March 2006
#

no strict;
no warnings;

BEGIN
{
    # Set perl to not try to resolve all symbols at startup
    #   The default behavior causes some problems because 
    #    opengl.pd builds an interface for all functions
    #    defined in gl.h and glu.h even though they might not
    #    actually be in the opengl libraries.
    $ENV{'PERL_DL_NONLAZY'}=0;
}

sub hasDISPLAY 
{
    return defined $ENV{DISPLAY} && $ENV{DISPLAY} !~ /^\s*$/;
}

use PDL;
use Test::More;

#use lib '../blib/lib';
#use lib '../blib/arch';

BEGIN
{   
   eval( " use PDL::Transform::Proj4; " );
   if( !($@) 
      && $PDL::Config{OPENGL_LIBS} 
      && $PDL::Config{WITH_3D} 
      && $PDL::Config{GL_BUILD} 
      && $PDL::Config{WITH_BADVAL}
      && hasDISPLAY() )
   {
      plan tests => 4;
   }
   else
   {
      plan skip_all => "PDL::Transform::Proj4 requires the Proj4 & TridD module and Bad Values enabled.";
   }
}



#
# Test integration with PDL::Transform
#

### Load the necessary modules
use PDL::Graphics::TriD;
#$PDL::Graphics::TriD::verbose = 1;

use PDL::Transform::Cartography;
use PDL::Transform::Proj4;

### Get the vector coastline map (and a lon/lat grid), and load the Earth
### RGB daytime image -- both of these are built-in to the module. The
### coastline map is a set of (X,Y,Pen) vectors.
my $coast = earth_coast()->glue( 1, graticule(15,1) );

my $map = earth_image( 'day' );

$map->badflag(1);

#my $screen_size = [15,8];
my $map_size = [500,500];

sub draw 
{
    my ($tx, $t, $pix, $opt ) = @_;
    nokeeptwiddling3d();
    imagrgb( double( $map->map( $tx, $pix, $opt )->mv(2,0) ) / 255, 
        {width => $map_size[0], height => $map_size[0]} );
    twiddle3d();
    sleep(5);
    release3d();
}

my $cyl_eq = "+proj=eqc +lon_0=0";
draw( t_proj( proj_params => $cyl_eq ), "Proj4 - \'$cyl_eq\'", $map_size );
ok(1);

my $ortho = "+proj=ortho +ellps=WGS84 +lon_0=-90 +lat_0=40";
draw( t_proj( proj_params => $ortho ), "Proj4 - \'$ortho\'", $map_size );
ok(1);

#
# Test the auto-generated methods:
#
draw( t_proj_ortho( ellps => 'WGS84', lon_0 => -90, lat_0 => 40 ), "Proj4 - t_proj_orhto()", $map_size );
ok(1);

draw( t_proj_robin( ellps => 'WGS84', over => 1 ), "Proj4 - t_proj_robin()", $map_size );
ok(1);
