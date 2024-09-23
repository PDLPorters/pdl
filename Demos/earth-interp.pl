##############################
# Demo code to interpret the map data in earth.txt
# 
# Craig DeForest, 17-Dec-2002
# 
$| = 1;
print "Initializing...\n";
use PDL;
use PDL::NiceSlice;

# Snarf the file and build a separate ndarray for each polyline
print "Interpreting map file...\n";

open my $mapfh, "<", "earth.txt";
$nelem = 0;
while(<$mapfh>) {
  next if m/^\#/ || m/^\s*$/;
  s/^\s+//;
  ($x,$y,$z,$color) = split /\s+/;
  if ($color==0 and @this_polyline > 2) {
    push @this_polyline,$this_polyline[0];
    my $c = $this_polyline[1]->[3];
    print $c;
    ($this_polyline[-1])->[3] = $c;
    $nelem++;
    push @mainlist,pdl(\@this_polyline);
    @this_polyline = ();
  }
  push @this_polyline,[$z,$x,$y,$color ? $color-1:0];
  $nelem++;
}

print "Breaking up elements...\n";
$elements = zeroes(4,$nelem);
$pos = 0;
foreach $z(@mainlist) {
  $n = $z->dim(1);
  $elements->(0:2,$pos:$pos+$n-1) .= $z->(0:2);
  $elements->((3),$pos:$pos+$n-2) .= $z->((3),1:-1);
  $elements->((3),$pos+$n-1) .= 0;
  $pos += $n;
}

print "... $nelem vectors\n";

# Transform all the polylines into spherical coordinates.
use PDL::Transform;
$t = t_compose(t_scale(ones(3)*180/3.14159),t_spherical());

$lonlat = $t->apply( $elements(0:2) ) ->(0:1); # discard radius

# Clean up the pen values at the longitude singularity
print "cleaning up singularities...\n";
use PDL::Transform::Cartography;
$lonlatp = $lonlat->append($elements(3))->clean_lines;

# Plot map
use PDL::Graphics::Simple;
$w = pgswin(size=>[8,4]);
$w->plot(with=>'polylines', $lonlatp,
  {Title=>'Lat/Lon map of world coastlines (Plate Carree)',
    xlabel=>'East Longitude',ylabel=>'North Latitude'}
);
print "ret> "; <STDIN>;

#wfits($lonlatp,'earth_coast.vec.fits');
1;
