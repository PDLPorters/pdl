#
package PDL::Demos::Cartography_demo;

use PDL;
use PDL::Graphics::PGPLOT::Window;
use PDL::Transform::Cartography;

use File::Spec;

PDL::Demos::Routines->import();
sub comment($);
sub act($);
sub output;

sub run {
  local($PDL::debug) = 0;
  local($PDL::verbose) = 0;

##$ENV{PGPLOT_XW_WIDTH}=0.6;
$ENV{PGPLOT_DEV}=$^O =~ /MSWin32/ ? '/GW' : "/XSERVE";

comment q|

 This demo illustrates the PDL::Transform::Cartography module.
 It also requires PGPLOT support: you must have PGPLOT installed to run it.

 PDL::Transform::Cartography includes a global earth vector coastline map
 and night and day world image maps, as well as the infrastructure for 
 transforming them to different coordinate systems.
   
 If you are using an RGB-enabled version of PGPLOT, then the map
 images will appear in color on your screen.  Otherwise they will
 appear in greyscale.  (The vector data will appear in color
 regardless).

 |;

act q|
  ### Load the necessary modules 
    use PDL::Graphics::PGPLOT::Window;
    use PDL::Transform::Cartography;
    
  ### Get the vector coastline map (and a lon/lat grid), and load the Earth
  ### RGB daytime image -- both of these are built-in to the module. The
  ### coastline map is a set of (X,Y,Pen) vectors.
    $coast = earth_coast() -> glue( 1, graticule(15,1) );
    print "Coastline data are a collection of vectors:  ",
             join("x",$coast->dims),"\n";

    $map = earth_image(day);
    print "Map data are RGB:   ",join("x",$map->dims),"\n\n";
|;

act q&
  ### Map data are stored natively in Plate Caree format. 
  ### The image contains a FITS header that contains coordinate system info.
  print "FITS HEADER INFORMATION:\n";
  for $_(keys %{$map->hdr}){
    next if(m/SIMPLE/ || m/HISTORY/ || m/COMMENT/);
    printf ("  %8s: %10s%s", $_, $map->hdr->{$_}, (++$i%3) ? "  " : "\n"); 
  }
  print "\n";

  $w = pgwin(Dev=>"/xw",size=>[8,6]);
  $w->fits_imag($map, {Title=>"NASA/MODIS Earth Map (Plate Caree)",J=>0});
&;

act q&
  ### The map data are co-aligned with the vector data, which can be drawn
  ### on top of the window with the "->lines" PGPLOT method.  The 
  ### clean_lines method breaks lines that pass over the map's singularity 
  ### at the 180th parallel.
  
  $w->hold;
  $w->lines( $coast -> clean_lines );
  $w->release;

&;

act q&
### There are a large number of map projections -- to list them all, 
### say "??cartography" in the perldl shell.  Here are four of them:

undef $w;   # Close old window
$w = pgwin(Dev=>"/xw", size=>[8,6], nx=>2, ny=>2 ) ;

sub draw {
 ($tx, $t, $pix, $opt ) = @_;
 $w->fits_imag( $map->map( $tx, $pix, $opt ), {Title=>$t, CharSize=>1.5} );
 $w->hold;   $w->lines( $coast -> apply( $tx ) -> clean_lines ); $w->release;
}

## (The "or" option specifies the output range of the mapping)
draw( t_mercator,  "Mercator Projection",    [400,300] );
draw( t_aitoff,    "Aitoff / Hammer",        [400,300] );
draw( t_gnomonic,  "Gnomonic",               [400,300],{or=>[[-3,3],[-2,2]]} );
draw( t_lambert,   "Lambert Conformal Conic",[400,300],{or=>[[-3,3],[-2,2]]} );


&;

act q|
### You can create oblique projections by feeding in a different origin.
### Here, the origin is centered over North America.

draw( t_mercator(  o=>[-90,40] ), "Mercator Projection",    [400,300] );
draw( t_aitoff (   o=>[-90,40] ), "Aitoff / Hammer",        [400,300] );
draw( t_gnomonic(  o=>[-90,40] ), "Gnomonic",[400,300],{or=>[[-3,3],[-2,2]]} );
draw( t_lambert(   o=>[-90,40] ), "Lambert ",[400,300],{or=>[[-3,3],[-2,2]]} );

|;

act q|
### There are three main perspective projections (in addition to special
### cases like stereographic and gnomonic projection): orthographic,
### vertical, and true perspective.  The true perspective has options for
### both downward-looking and aerial-view coordinate systems.

draw( t_orthographic( o=>[-90,40] ), 
      "Orthographic",  [400,300]);

draw( t_vertical( r0=> (2 + 1), o=>[-90,40] ), 
      "Vertical (Altitude = 2 r\\\\de\\\\u)", [400,300]);

draw( t_perspective( r0=> (2 + 1), o=>[-90,40] ),
      "True Perspective (Altitude= 2 r\\\\de\\\\u)", [400,300]);

# Observer is 0.1 earth-radii above surface, lon 117W, lat 31N (over Tijuana).
# view is 45 degrees below horizontal, azimuth -22 (338) degrees.
draw( t_perspective( r0=> 1.1, o=>[-117,31], cam=>[-22,-45,0] ),
      "Aerial view of West Coast of USA", [400,300],
      {or=>[[-60,60],[-45,45]], method=>'linear'});

|;

comment q|

That concludes the basic cartography demo.  Numerous other transforms
are available.  

Because PDL's cartographic transforms work with in the Transform module
and are invertible, it's easy to use them both forwards and backwards.
In particular, the perspective transformation is useful for ingesting 
scientific image data of the Earth or other planets, and converting to
a map of the imaged body.

Similarly, scanned images of map data can easily be converted into 
lat/lon coordinates or reprojected to make other projections. 

Be sure to view "demo transform" if you haven't already.
|;

}

1;
