use strict;
use warnings;
use PDL::LiteF;
use PDL::Transform::Proj4;
use PDL::Transform::Cartography;
use Test::More;

# Test integration with PDL::Transform

my ($w, $h) = (128,64);
my $im = sequence($w,$h)/$w/$h*255.99;
$im = raster2fits($im->byte, @PDL::Transform::Cartography::PLATE_CARREE);
$im->hdrcpy(1);
$im->badflag(1);

# use epsilon of 1.1 as MacOS 11 gives off-by-one differences
sub pdl_cmp {
  my ($g, $e, $l) = @_;
  $_ = PDL->topdl($_) for $g, $e;
  local $Test::Builder::Level = $Test::Builder::Level + 1;
  my $res = all(approx($g, pdl($e), 1.1));
  fail("$l: result was BAD value"), return if $res->isbad;
  ok $res, $l or diag "got:\n$g\nexpected:\n$e";
}

SKIP: {

   my $map = $im->copy;

   my $map_size = [25,25];

   my @slices = (
      "12:15,3:6",
      "6:9,13:16",
      "12:15,13:16",
      "19:22,12:15",
      "13:16,20:23"
   );

   ##############
   # TESTS 1-5: #
   ##############
   # Get EQC reference data:
   my @ref_eqc_slices = get_ref_eqc_slices();

   # Check EQC map against reference:
   my $eqc_opts = "+proj=eqc +lon_0=0 +datum=WGS84";
   my $eqc = eval { $map->map( t_proj( proj_params => $eqc_opts ), $map_size ) };
   if (defined($eqc)) {
       foreach my $i ( 0 .. $#slices ) {
          my $str = $slices[$i];
          my $slice = $eqc->slice($str);
          pdl_cmp($slice, $ref_eqc_slices[$i], "check ref_eqc for slices[$i]");
       }
   } else {
      diag("PROJ4 error: $@\n");
      skip "Possible bad PROJ4 install",20 if $@ =~ m/Projection initialization failed/;
   }

   ###############
   # TESTS 6-10: #
   ###############
   # Get Ortho reference data:
   my @ref_ortho_slices = get_ref_ortho_slices();

   # Check Ortho map against reference:
   my $ortho_opts = "+proj=ortho +ellps=WGS84 +lon_0=-90 +lat_0=40";
   my $ortho = $map->map( t_proj( proj_params => $ortho_opts ), $map_size );
   foreach my $i ( 0 .. $#slices )
   {
      my $str = $slices[$i];
      my $slice = $ortho->slice($str);
      pdl_cmp($slice, $ref_ortho_slices[$i], "check ref_ortho for slices[$i]");
   }

   #
   # Test the auto-generated methods:
   #
   ################
   # TESTS 11-15: #
   ################
   my $ortho2 = $map->map( t_proj_ortho( ellps => 'WGS84', lon_0 => -90, lat_0 => 40 ), $map_size );
   foreach my $i ( 0 .. $#slices )
   {
      my $str = $slices[$i];
      my $slice = $ortho2->slice($str);
      pdl_cmp($slice, $ref_ortho_slices[$i], "check ref_ortho2 for slices[$i]");
   }

   ################
   # TESTS 16-20: #
   ################
   # Get Robinson reference data:
   my @ref_robin_slices = get_ref_robin_slices();

   # Check Robinson map against reference:
   my $robin = $map->map( t_proj_robin( ellps => 'WGS84', over => 1 ), $map_size );
   foreach my $i ( 0 .. $#slices )
   {
      my $str = $slices[$i];
      my $slice = $robin->slice($str);
      pdl_cmp($slice, $ref_robin_slices[$i], "check ref_robin for slices[$i]");
   }

}

done_testing;

sub get_ref_robin_slices {
    my @slices = ();
    push(@slices, <<"END");
 [45 46 46 46]
 [53 54 54 54]
 [61 62 62 62]
 [73 74 74 74]
END
    push(@slices, <<"END");
 [137 137 137 137]
 [145 145 145 145]
 [152 153 153 153]
 [164 165 165 165]
END
    push(@slices, <<"END");
 [137 138 138 138]
 [145 146 146 146]
 [153 154 154 154]
 [165 166 166 166]
END
    push(@slices, <<"END");
 [131 131 131 131]
 [139 139 139 139]
 [147 147 147 147]
 [155 155 155 155]
END
    push(@slices, <<"END");
 [202 202 202 202]
 [210 210 210 210]
 [222 222 222 222]
 [234 234 234 234]
END
    return @slices;
}

sub get_ref_ortho_slices {
    my @slices = ();
    push(@slices, <<"END");
 [116 117 117 117]
 [128 129 125 125]
 [136 137 133 133]
 [144 145 141 141]
END
    push(@slices, <<"END");
 [180 184 184 188]
 [188 188 192 192]
 [192 196 200 200]
 [196 200 204 208]
END
    push(@slices, <<"END");
 [188 189 189 189]
 [196 197 197 193]
 [204 205 201 201]
 [208 209 209 209]
END
    push(@slices, <<"END");
 [173 169 165 161]
 [177 173 169 165]
 [185 181 177 169]
 [189 185 181 173]
END
    push(@slices, <<"END");
 [241 237 233 225]
 [245 241 237 229]
 [246 242 234 226]
 [238 234 226 218]
END
    return @slices;
}

sub get_ref_eqc_slices {
    my @slices = ();
    push(@slices, <<"END");
 [33 34 34 34]
 [45 46 46 46]
 [57 58 58 58]
 [65 66 66 66]
END
    push(@slices, <<"END");
 [137 137 137 137]
 [149 149 149 149]
 [157 157 157 157]
 [169 169 169 169]
END
    push(@slices, <<"END");
 [137 138 138 138]
 [149 150 150 150]
 [157 158 158 158]
 [169 170 170 170]
END
    push(@slices, <<"END");
 [131 131 131 131]
 [139 139 139 139]
 [151 151 151 151]
 [159 159 159 159]
END
    push(@slices, <<"END");
 [210 210 210 210]
 [222 222 222 222]
 [230 230 230 230]
 [242 242 242 242]
END
    return @slices;
}
