##############################
# Demo code to interpret the map data in earth.txt
# 
# Craig DeForest, 17-Dec-2002
# 
$| = 1;
print "Initializing...\n";
use PDL;
use PDL::NiceSlice;

# Snarf the file and build a separate piddle for each polyline
print "Interpreting map file...\n";

open(MAP,"<earth.txt");
$nelem = 0;
while(<MAP>) {
  next if(m/^\#/ || m/^\s*$/);
  s/^\s+//;
  ($x,$y,$z,$color) = split /\s+/;
  if($color==0 and $#a > 1) {
    push(@a,$a[0]);
    my $c = $a[1]->[3];
    print $c;
    ($a[-1])->[3] = $c;
    $nelem++;
    push(@mainlist,pdl(@a)); undef @a;
  }
  push(@a,[$x,$y,$z,$color ? $color-1:0]);
  $nelem++;
}

print "Breaking up elements...\n";
$elements = zeroes(4,$nelem);
$pos = 0;
foreach $z(@mainlist) {
  $n = $z->dim(1);
  $elements->(0:2,$pos:$pos+$n-1) .= $z->(0:2);
  $elements->((3),$pos:$pos+$n-2) .= $z->((3),1:-1);
  $elements->((3),$pos+n-1) .= 0;
  $pos += $n;
}

print "... $nelem vectors\n";

# Transform all the polylines into spherical coordinates.
use PDL::Transform;
$t = t_compose(t_scale(ones(3)*180/3.14159),t_spherical());

$lonlat = $t->apply( $elements->(pdl(2,0,1)) )  ->(1:2); # discard radius

# Clean up the pen values at the longitude singularity
print "cleaning up singularities...\n";
$p = $elements->((3));
$idx = which((abs($lonlat->((0),0:-2) - $lonlat->((0),1:-1))) > 355);
$p->($idx) .= 0 if($idx->nelem > 0);

# Plot map
use PDL::Graphics::PGPLOT::Window;
$w = pgwin(dev=>'/xs',size=>[10,10]);
$w->lines($lonlat,$p,{Title=>'Lat/Lon map of world coastlines',XTitle=>'East Longitude',YTitle=>'North Latitude',Axis=>2});

wfits($lonlat->glue(0,$p->dummy(0,1)),'earth_coast.vec.fits');
1;
