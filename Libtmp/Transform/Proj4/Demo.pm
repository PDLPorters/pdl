package PDL::Demos::Proj4_demo;

sub init {'
use PDL::Transform::Proj4;
'}
sub done {'
undef $w;
'}

sub info {('proj','Transforms with PROJ (Req.: PDL::Graphics::Simple)')}

my @demos = (
PDL->rpiccan('JPEG') ? () :
    [comment => q|
This demo illustrates the PDL::Transform::Proj4 module.

It requires PDL::Graphics::Simple and also the ability to read JPEG images.

You don't seem to have that ability at the moment -- this is likely
because you do not have NetPBM installed.  See the man page for PDL::IO::Pic.

I'll continue with the demo anyway, but it will likely crash on the
earth_image('day') call on the next screen.

|],

[comment => q|
This demo illustrates the PDL::Transform::Proj4 module.
Also you must have PDL::Graphics::Simple installed to run it.

PDL::Transform::Proj4 binds the PROJ library, which together with
PDL::Transform lets you project between coordinate systems. It has more
transformations available than PDL::Transform::Cartography. Let's take
a look at some of them.
|],

[act => q|
### Load the necessary modules
use PDL::Graphics::Simple;
use PDL::Transform::Proj4;
use PDL::Transform::Cartography;

### Get the vector coastline map (and a lon/lat grid), and load the Earth
### RGB daytime image -- both of these are built-in to the module. The
### coastline map is a set of (X,Y,Pen) vectors.
$earth = earth_image('day');
($coast, $pen) = graticule(10,2)->glue(1,earth_coast())->clean_lines;
|],

[act => q&
# Like the PDL::Transform::Cartography demo, let's start by just looking at
# the Plate Carree map of the Earth, with gridlines.
$w = pgswin();
$w->plot(with=>'fits', $earth,
 with=>'polylines', clean_lines($coast, $pen),
 {j=>0, title=>'NASA/MODIS Earth Map (Plate Carree)'});
&],

[act => q&
# Now let's look at an orthographic projection. If you've looked at the
# cartography demo, you'll notice PROJ uses metres, rather than body radii.
# Otherwise, it's quite similar so far.
$t = t_proj_ortho( ellps => 'WGS84', lon_0=>0, lat_0=>40 );
$w->plot(with=>'fits', $earth->map($t),
 with=>'polylines', clean_lines($coast->apply($t), $pen),
 {j=>1, title=>'NASA/MODIS Earth Map (Orthographic)'});
&],

[act => q&
### There are a large number of map projections -- to list them all,
### say "?? t_proj" in the perldl or pdl2 shell.  Here are four
### of them:

undef $w; # Close old window
$w = pgswin( size=>[8,6], multi=>[2,2] ) ;

sub draw {
 ($tx, $t, $px, @opt ) = @_;
 $w->plot(with=>'fits', $earth->map( $tx, $px, @opt ),
   with=>'polylines', clean_lines($coast->apply($tx), $pen, @opt),
   {Title=>$t});
}

draw( t_proj_imoll,            "Interrupted Mollweide",        [400,300] );
draw( t_proj_cass,             "Cassini (Cassini-Soldner)",    [400,300] );
draw( t_proj_bonne(lat_1=>40), "Bonne",                        [400,300] );
draw( t_proj_rouss,            "Roussilhe Stereographic",      [400,300] );
&],

[act => q|
### Here are some more, showing the use of additional arguments to
### the constructor.

draw( t_proj_murd3(lat_1=>30, lat_2=>50), "Murdoch III", [400,300]);
draw( t_proj_vandg, "van der Grinten (I)", [400,300]);
draw( t_proj_gstmerc, "Gauss-Schreiber Transverse Mercator", [400,300]);
draw( t_proj_som(inc_angle=>98, ps_rev=>0.06, asc_lon=>64),
  "Space Oblique Mercator", [400,300]);
|],

[act => q|
### Due to the way PDL::Transform resamples images, it needs to have
### inverse versions of the transforms available for that. But with only
### forward transforms we can still draw vectors.

sub drawL {
 ($tx, $t, $px, @opt ) = @_;
 $w->plot(with=>'polylines', clean_lines($coast->apply($tx), $pen, @opt),
   {Title=>$t});
}

drawL( t_proj_urm5(n=>0.9, alpha=>2, q=>4), "Urmaev V", [400,300]);
drawL( t_proj_adams_ws1, "Adams World in a Square I",   [400,300]);
drawL( t_proj_nicol,     "Nicolosi Globular",           [400,300]);
drawL( t_proj_apian,     "Apian Globular I",            [400,300]);
|],

[comment => q|

That concludes the demo of PROJ. Numerous other transforms are available.
Take a look at https://proj.org/en/9.5/operations/projections/index.html
for more!
|],
);

sub demo { @demos }

1;
