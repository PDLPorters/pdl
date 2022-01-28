use strict;
use warnings;
use PDL::LiteF;
use Test::More;
use PDL::Transform::Proj4;
use PDL::GIS::Proj qw(proj_version);

my $test_jpegtopnm = 1;
if($^O =~ /MSWin32/i) {
    $test_jpegtopnm = `jpegtopnm --help 2>&1`;
    $test_jpegtopnm = $test_jpegtopnm =~ /^jpegtopnm:/ ? 1 : 0;
} elsif ( !defined scalar qx(jpegtopnm --help 2>&1) ) {
    $test_jpegtopnm = 0;
}
plan skip_all => "The jpegtopnm utility (needed for proj_transform.t tests) not found."
    if !$test_jpegtopnm;

my @projections = sort keys %{PDL::GIS::Proj::load_projection_information()};
my @proj_version = proj_version();

# Test integration with PDL::Transform

use_ok('PDL::Transform::Cartography');

### Get the vector coastline map (and a lon/lat grid), and load the Earth
### RGB daytime image -- both of these are built-in to the module. The
### coastline map is a set of (X,Y,Pen) vectors.

ok defined graticule(10,2)->glue(1,earth_coast());

my $map = eval { earth_image( 'day' ) };

SKIP: {
   skip("earth_image() can not load test data", 23) if $@;
   pass("earth_image() loaded");
   $map->badflag(1);
   my $checksum = unpack "%16C*", ${$map->get_dataref};
   my $goodcheck = 56639;
   if ($checksum != $goodcheck) {
      skip "earth_image() map has bad checksum: $checksum (expected $goodcheck)", 22;
   }

   my $map_size = [500,500];

   my @slices = (
      "245:254,68:77,(0)",
      "128:137,272:281,(0)",
      "245:254,262:271,(0)",
      "390:399,245:254,(0)",
      "271:280,464:473,(0)"
   );

   # Get EQC reference data:
   my @ref_eqc_slices = get_ref_eqc_slices();

   # Check EQC map against reference:
   my $eqc_opts = "+proj=eqc +lon_0=0 +datum=WGS84";
   my $proj = eval { t_proj( proj_params => $eqc_opts ) };
   isnt $proj, undef;
   isnt $proj->proj_params, undef;
   my $eqc = eval { $map->map( $proj, $map_size ) };
   if (! defined($eqc)) {
      diag("PROJ4 error: $@\n");
      skip "Possible bad PROJ4 install",20 if $@ =~ m/Projection initialization failed/;
   }
   foreach my $i ( 0 .. $#slices )
   {
      my $str = $slices[$i];
      my $slice = $eqc->slice($str)->copy;
      $slice->badflag(0);
      # ok( "$slice" eq $ref_eqc_slices[$i], "check ref_eqc for slices[$i]" );
      is( "$slice", $ref_eqc_slices[$i], "check ref_eqc for slices[$i]" );
   }

   # Get Ortho reference data:
   my @ref_ortho_slices = get_ref_ortho_slices();

   # Check Ortho map against reference:
   my $ortho_opts = "+proj=ortho +ellps=WGS84 +lon_0=-90 +lat_0=40";
   my $ortho = $map->map( t_proj( proj_params => $ortho_opts ), $map_size );
   foreach my $i ( 0 .. $#slices )
   {
      my $str = $slices[$i];
      my $slice = $ortho->slice($str)->copy;
      $slice->badflag(0);
      # ok( "$slice" eq $ref_ortho_slices[$i], "check ref_ortho for slices[$i]" );
      is( "$slice", $ref_ortho_slices[$i], "check ref_ortho for slices[$i]" );
   }

   # Test the auto-generated methods:
   my $ortho2 = $map->map( t_proj_ortho( ellps => 'WGS84', lon_0 => -90, lat_0 => 40 ), $map_size );
   foreach my $i ( 0 .. $#slices )
   {
      my $str = $slices[$i];
      my $slice = $ortho2->slice($str)->copy;
      $slice->badflag(0);
      # ok( "$slice" eq $ref_ortho_slices[$i], "check ref_ortho2 for slices[$i]" );
      is( "$slice", $ref_ortho_slices[$i], "check ref_ortho2 for slices[$i]" );
   }

   # Get Robinson reference data:
   my @ref_robin_slices = get_ref_robin_slices();

   # Check Robinson map against reference:
   my $robin = $map->map( t_proj_robin( ellps => 'WGS84', over => 1 ), $map_size );
   foreach my $i ( 0 .. $#slices )
   {
      my $str = $slices[$i];
      my $slice = $robin->slice($str);
      # ok( "$slice" eq $ref_robin_slices[$i], "check ref_robin for slices[$i]" );
      is( "$slice", $ref_robin_slices[$i], "check ref_robin for slices[$i]" );
   }
}

my $eqc_opts = "+proj=eqc +lon_0=0 +datum=WGS84";
for my $proj (@projections) {
   my $alias = "t_proj_$proj";
   my $proj = eval { no strict 'refs'; $alias->( proj_params => $eqc_opts ) };
   is $@, '';
   isnt $proj, undef;
}

done_testing;

sub get_ref_robin_slices {
    my @slices = ();
    push(@slices, <<"END");

[
 [0 0 0 0 0 0 0 2 0 0]
 [0 0 0 0 0 0 0 3 0 0]
 [0 0 2 2 2 2 2 5 1 1]
 [0 0 2 2 2 2 2 5 1 1]
 [0 0 2 2 2 2 2 5 1 1]
 [0 0 2 2 2 2 2 2 0 0]
 [2 2 1 1 1 1 1 2 0 0]
 [2 2 0 0 0 0 0 2 0 0]
 [0 0 0 0 0 0 0 2 0 0]
 [0 0 0 4 1 2 2 2 0 0]
]
END
    push(@slices, <<"END");

[
 [ 1  4  4  4  4  0  0  2 13  0]
 [ 5  4  4  4  4  0  0  0  0 32]
 [ 6  2  4  4  4  0  0  6  3 70]
 [ 8  2  4  4  4  0  9 20 34 60]
 [ 0  1  0  5  4  0 48 75 69 64]
 [ 0  1  0  0  3  0 48 66 44  4]
 [ 3  1  0  1  2  0 69 60  3  0]
 [ 1  1  0  4 23 57 63 36  2  0]
 [ 0  1  0  3 35 71 58 21  0  0]
 [ 5  5  0 48 59 72 65  0  0  0]
]
END
    push(@slices, <<"END");

[
 [  1   1   1   1   1   1   1   1   1   0]
 [  4   0  10   4   0   1   1   1   1   0]
 [  3   2   4   5   5   1   1   1   1   0]
 [ 14  46  85   9   4   1   1   1   1   0]
 [ 93  83  67  96  27   1   1   1   1   0]
 [ 74  70  70  75  81  37  11  10   4   2]
 [ 77  63  71  82  71  95   0   3   9   4]
 [ 80  70  62  69  69  87 107  43  12  11]
 [ 75  53  80  79  62 107  86 119 117 102]
 [ 80  77  74  71  73  74  92  90 119 106]
]
END
    push(@slices, <<"END");

[
 [47 50 60 35 39 13  1  5  9  0]
 [39 52 61 27 33  5  0  9  5  0]
 [35 47 54 28 12  0  4  5  4  0]
 [30 48 51 40  0 21  8  0  1  0]
 [29 39 41 19 11 18  7  0  0  0]
 [41 48 54 26 14 10  2  1  0  0]
 [52 61 38 42  0  2  0  4  1  0]
 [65 45 18 70  9 13  0  4  2  0]
 [65 32 41 49 38  1  0  4  4  1]
 [59 23  9 31 69  0  0  1  2  1]
]
END
    push(@slices, <<"END");

[
 [ 8  0  0  0  4  9  1  0  0  0]
 [ 0  0  6  0  0  7  3  0  0  0]
 [ 0  0  9 12  0  2  0  3  0  0]
 [ 1  5  2  1  0  0  4  7  3  0]
 [ 3  2  6  0  1  0  4  5  0  3]
 [ 0  2  2  0  2  5  0  0  0  8]
 [ 0  0  0  0  2  2  0  0  0  0]
 [ 0  0  0  0  0  0  0  4  0  0]
 [ 0  0  0  0  0  0  0  0  0  0]
 [ 0  0  0  0  0  0  0  0  0  0]
]
END
    return @slices;
}

sub get_ref_ortho_slices {
    my @slices = ();
    push(@slices, <<"END");

[
 [0 0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0 0]
]
END
    push(@slices, $proj_version[0] < 8 ? <<"END" : <<"END");

[
 [0 0 0 3 3 3 3 0 0 1]
 [0 0 0 3 3 3 3 1 0 0]
 [0 0 0 0 3 3 3 1 0 0]
 [0 0 0 0 3 3 3 3 0 0]
 [0 0 0 0 3 3 3 3 1 0]
 [0 0 0 0 0 3 3 3 3 0]
 [0 0 0 0 0 3 3 3 3 1]
 [0 0 0 0 0 0 3 3 3 3]
 [0 0 0 0 0 0 3 3 3 3]
 [0 0 0 0 0 0 0 3 3 3]
]
END

[
 [0 0 0 3 3 3 3 0 0 0]
 [0 0 0 3 3 3 3 1 0 0]
 [0 0 0 0 3 3 3 3 0 0]
 [0 0 0 0 3 3 3 3 1 0]
 [0 0 0 0 0 3 3 3 1 0]
 [0 0 0 0 0 3 3 3 3 0]
 [0 0 0 0 0 3 3 3 3 1]
 [0 0 0 0 0 0 3 3 3 3]
 [0 0 0 0 0 0 3 3 3 3]
 [0 0 0 0 0 0 0 3 3 3]
]
END
    push(@slices, $proj_version[0] < 8 ? <<"END" : <<"END");

[
 [59 61 61 61 59 55 56 60 64 66]
 [62 58 58 58 56 50 52 59 64 66]
 [63 57 57 57 55 48 50 58 64 66]
 [63 57 57 57 55 47 49 58 65 66]
 [55 58 63 59 51 43 47 57 65 52]
 [53 58 59 54 47 47 50 59 67 46]
 [53 58 56 51 45 51 53 61 68 52]
 [54 57 53 51 57 52 54 61 66 66]
 [62 55 53 60 66 50 51 55 59 67]
 [63 53 52 61 71 49 49 51 53 56]
]
END

[
 [61 59 59 59 57 53 54 60 64 66]
 [63 57 57 57 55 48 50 58 64 66]
 [63 57 57 57 55 47 49 58 65 66]
 [56 58 65 63 54 40 44 55 64 62]
 [53 58 59 54 47 47 50 59 67 46]
 [53 58 56 51 45 51 53 61 68 52]
 [54 57 53 51 49 52 54 61 66 66]
 [62 55 53 60 66 50 51 55 59 67]
 [63 53 52 61 71 49 49 51 53 56]
 [62 50 52 54 55 53 51 49 48 48]
]
END
    push(@slices, <<"END");

[
 [1 1 1 1 1 1 1 1 1 1]
 [1 1 1 1 1 1 1 1 1 1]
 [1 1 1 1 1 1 1 1 1 1]
 [1 1 1 1 1 1 1 1 1 1]
 [1 1 1 1 1 1 1 1 1 1]
 [1 1 1 1 1 1 1 1 1 1]
 [1 1 1 1 1 1 1 1 1 1]
 [1 1 1 1 1 1 1 1 1 1]
 [1 1 1 1 1 1 1 1 1 1]
 [1 1 1 1 1 1 1 1 1 1]
]
END
    push(@slices, $proj_version[0] < 8 ? <<"END" : <<"END");

[
 [155 159   8   0   9   3   4   0   0   0]
 [255 208  27   2   1   1   2   5   2   0]
 [ 13   4   4   5   0   3   1   1   4   0]
 [  9   0   0   0   0   0   0   0   0   0]
 [  0   0   0   0  12   0   0   0   0   0]
 [  7   0   0   0   6   5  13 168   7   3]
 [239 241 241 218 247 233 248 245 243 246]
 [ 37 138  34 211  71  17   1   0   1  10]
 [  0   0   0   0   1   0   0   9  20  11]
 [  0   0   0   0   0   1   5   2   0   0]
]
END

[
 [202 200   0   7   0   0   4   0   0   0]
 [197 136  14   3   1   2   2   5   5   2]
 [  3   2   4   0   0   2   3   1   0   0]
 [  9   2   0   0   0   0   0   0   0   0]
 [  0   1   0   0  16   0  16   4   7   0]
 [  0   0   0   0   1   9   8 186  73 184]
 [233 222 241 241 223 248 248 245 248 247]
 [  8  81   9   1  38  10   8  13   7  39]
 [  0   0   0   0   0   0   5  17  18   4]
 [  0   0   0   0   0   0   5   2   0   0]
]
END
    return @slices;
}

sub get_ref_eqc_slices {
    my @slices = ();
    push(@slices, <<"END");

[
 [254 254 254 254 254 254 254 254 254 254]
 [254 254 254 254 254 254 254 254 254 254]
 [252 240 238 252 252 242 243 254 255 251]
 [254 226 254 254 254 233 236 255 255 253]
 [229 241 255 255 253 249 254 255 255 255]
 [254 242 254 253 248 238 255 255 255 255]
 [247 247 115  38 247  42 242 255 255 254]
 [ 64 132  59 163   0  35 242 229 226 239]
 [ 11  36  49   0   0   3 233 157 159 214]
 [ 26  31   0   4  16   0   0  37   0 167]
]
END
    push(@slices, <<"END");

[
 [ 5  4  4  4  4  0 11 17 53 75]
 [ 0  3  0  3  1  0 49 92 74 69]
 [ 0  2  0  0  5  0 55 69 44 19]
 [ 0  2  0  0  0 22 66 34  0  0]
 [ 0  2  0  8 16 66 52 10  1  0]
 [ 0  2  4 57 63 95 59  0  0  0]
 [ 7  4  0 74 82 65 15  0  0  0]
 [10  4  1 69 90 84  0  0  0  1]
 [ 0  0  9 66 90 76  1  0  0  2]
 [ 4 15 35 10 80 48  0  0  0  1]
]
END
    push(@slices, <<"END");

[
 [  0  12   3   0   9   1   1   1   1   0]
 [  2   1  42   3   3   1   1   1   1   0]
 [ 61  86  83  64   0   1   1   1   1   0]
 [ 82  68  74  67  82   1   1   1   1   0]
 [ 77  63  71  82  71  95   0   3   9   4]
 [ 80  70  62  69  69  87 107  43  12  11]
 [ 75  53  80  79  62 107  86 119 117 102]
 [ 80  77  74  71  73  74  92  90 119 106]
 [ 72  85  83  73  64  73  86  92 110 110]
 [ 81  78  75  77  78  78  91  88  91 102]
]
END
    push(@slices, <<"END");

[
 [42 44 57 45 35 48 27 15  0  0]
 [41 57 66 36 29 30  0 11  5  0]
 [38 47 56 23 31  8  1  8  4  0]
 [33 48 53 39  0  2  5  1  2  0]
 [29 43 44 31  0 28  8  0  1  0]
 [45 58 53 30  0  5  0  4  0  0]
 [59 52 22 59  0  6  0  4  1  0]
 [66 43 31 65 28 17  0  4  4  0]
 [62 17 43 37 41  9  0  4  5  2]
 [42 66  0 31 61  0  0  1  4  2]
]
END
    push(@slices, <<"END");

[
 [245 226 212 124 194   6   8   1   7  61]
 [163 215 221 215 178 103  47  17 114 201]
 [100 138 149 184 131 163  42  18 125 189]
 [124 103 127 195 213 220  68  86 168 173]
 [123 178 225 251 243 237 205 208 121  65]
 [126 115 241 247 221 247 179 115 105  36]
 [119 174 236 172 247 237 161 212 172 142]
 [206 245 244 218 107 219 169 175 185 200]
 [ 33  30  83 101  14 210 249 221 251 125]
 [  6   2   3  85 145 155  75 142 251 251]
]
END
    return @slices;
}
