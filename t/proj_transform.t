#!/usr/bin/perl

#
# t/proj_transform.t
#
# Test program for the PDL::Transform::Proj4 library
#
# Judd Taylor, Orbital Systems, Ltd.
# judd.t@orbtialsystems.com
#
# 17 April 2008
#

use PDL;
use Test::More;

BEGIN
{
    use PDL::Config;
    if ( $PDL::Config{WITH_PROJ} ) 
    {
        eval( " use PDL::Transform::Proj4; " );
        if( !($@) ) 
        {
            $test_jpegtopnm = 1;
            if($^O =~ /MSWin32/i) 
            {
                $test_jpegtopnm = `jpegtopnm --help 2>&1`;
                $test_jpegtopnm = $test_jpegtopnm =~ /^jpegtopnm:/ ? 1 : 0;
            }
            elsif ( !defined( scalar( qx(jpegtopnm --help 2>&1) ) ) ) 
            {
                $test_jpegtopnm = 0;
            }
            if( $PDL::Bad::Status ) 
            {
                if( $test_jpegtopnm ) 
                {
                    plan tests => 22 
                }
                else 
                {
                    plan skip_all => "The jpegtopnm utility (needed for proj_transform.t tests) not found.";
                }
            }
            else 
            {
                plan skip_all => "PDL::Transform::Proj4 requires the PDL::Bad module.";
            }
        }
        else 
        {
            plan skip_all => "PDL::Transform::Proj4 module compiled, but not available.";
        }
    }
    else
    {
        plan skip_all => "PDL::Transform::Proj4 module not compiled.";
    }
}

#
# Test integration with PDL::Transform
#

BEGIN
{
   use_ok(PDL::Transform::Cartography);                 # TEST 1
}

### Get the vector coastline map (and a lon/lat grid), and load the Earth
### RGB daytime image -- both of these are built-in to the module. The
### coastline map is a set of (X,Y,Pen) vectors.
###
### This doesn't seem to be used.  # chm 14-May-2009
### my $coast = earth_coast()->glue( 1, graticule(15,1) );

eval { my $map = earth_image( 'day' ) };

if ($@) {
   skip("earth_image() can not load test data", 21);
} else {
   pass("earth_image() loaded");                     # TEST 2
}

my $map = earth_image( 'day' );
$map->badflag(1);

SKIP: {

   my $checksum = unpack "%16C*", ${$map->get_dataref};
   my $goodcheck = 56639;
   if ($checksum != $goodcheck) {
      skip "earth_image() map has bad checksum: $checksum (expected $goodcheck)", 20;
   }

   my $map_size = [500,500];

   my @slices = (
      "245:254,68:77,(0)",
      "128:137,272:281,(0)",
      "245:254,262:271,(0)",
      "390:399,245:254,(0)",
      "271:280,464:473,(0)"
   );


   ##############
   # TESTS 3-7: #
   ##############
   # Get EQC reference data:
   my @ref_eqc_slices = get_ref_eqc_slices();

   # Check EQC map against reference:
   my $eqc_opts = "+proj=eqc +lon_0=0";
   my $eqc = eval '$map->map( t_proj( proj_params => $eqc_opts ), $map_size )';
   if (! defined($eqc)) {
      diag("PROJ4 error: $@\n");
      skip "Possible bad PROJ4 install",20 if $@ =~ m/Projection initialization failed/;
   }
   foreach my $i ( 0 .. $#slices )
   {
      my $str = $slices[$i];
      my $slice = $eqc->slice($str);
      # ok( "$slice" eq $ref_eqc_slices[$i], "check ref_eqc for slices[$i]" );
      is( "$slice", $ref_eqc_slices[$i], "check ref_eqc for slices[$i]" );
   }

   ###############
   # TESTS 8-12: #
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
      # ok( "$slice" eq $ref_ortho_slices[$i], "check ref_ortho for slices[$i]" );
      is( "$slice", $ref_ortho_slices[$i], "check ref_ortho for slices[$i]" );
   }

   #
   # Test the auto-generated methods:
   #
   ################
   # TESTS 13-17: #
   ################
   my $ortho2 = $map->map( t_proj_ortho( ellps => 'WGS84', lon_0 => -90, lat_0 => 40 ), $map_size );
   foreach my $i ( 0 .. $#slices )
   {
      my $str = $slices[$i];
      my $slice = $ortho2->slice($str);
      # ok( "$slice" eq $ref_ortho_slices[$i], "check ref_ortho2 for slices[$i]" );
      is( "$slice", $ref_ortho_slices[$i], "check ref_ortho2 for slices[$i]" );
   }

   ################
   # TESTS 18-22: #
   ################
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
exit(0);


sub get_ref_robin_slices
{
    my @slices = ();
    push(@slices, <<"END");

[
 [2 0 0 0 0 0 0 0 6 3]
 [2 0 0 2 2 2 2 5 5 1]
 [2 0 0 2 2 2 2 5 5 1]
 [2 0 0 2 2 2 2 5 5 1]
 [2 0 0 2 2 2 2 5 5 1]
 [2 0 0 2 2 2 2 2 2 0]
 [0 3 3 0 0 0 0 2 2 0]
 [0 0 0 0 0 0 0 2 2 0]
 [0 0 0 0 0 0 0 3 3 0]
 [0 0 0 0 2 4 3 3 3 0]
]
END
    push(@slices, <<"END");

[
 [ 1  0  4  4  4  4  0  0  4 17]
 [ 1  1  4  4  4  4  0  0  0  0]
 [ 4  6  4  4  4  4  0  3  0 22]
 [ 5  7  4  4  4  4  0 11 16 46]
 [ 1  0  3  0  3  1  0 49 92 70]
 [ 3  0  2  0  0  5  0 55 74 43]
 [ 4  1  2  0  0  0 22 66 50  0]
 [ 0  0  2  0  0  4 61 52 22  2]
 [ 9  1  4  2 46 62 89 64  0  0]
 [ 8  4  6  1 62 69 79 60  0  0]
]
END
    push(@slices, <<"END");

[
 [  1   1   1   1   1   1   1   1   1   0]
 [  0   5   0   0   0   4   1   1   1   0]
 [  5   3   0   0  10   0   1   1   1   0]
 [  1  17  65  66   1  13   1   1   1   0]
 [ 91  94  67  72  72   0   1   1   1   0]
 [ 77  73  69  72  65 104  23  19  16   0]
 [ 80  81  63  69  71  68 120  17   0   1]
 [ 87  79  66  62  71  68 103  99  25  13]
 [ 94  70  57  88  74  54 112  98 121 113]
 [ 81  80  75  76  69  79  82  95  95 122]
]
END
    push(@slices, <<"END");

[
 [23 50 52 62 34 18  0  6  5  7]
 [38 32 51 48 32 37  0  1 11  2]
 [52 33 50 51 43 11  0  5  4  4]
 [56 41 56 42 51 16  9  7  0  4]
 [49 38 52 34 22  8  1  5  0  2]
 [45 55 60 41 29 14  0  0  4  0]
 [48 61 56 35 52  9  0  0  4  0]
 [51 65 37 11 66 28  0  0  4  1]
 [49 71 26 29 49 29  0  0  4  4]
 [54 60 20  0 53 23  0  0  1  2]
]
END
    push(@slices, <<"END");

[
 [ 6  6  0  0  0  0  2  0  4  0]
 [ 3  0  0  4  0  0  5  1  0  0]
 [ 1  0  0 10  3  0  1  0  0  0]
 [ 0  3  0  6  2  0  0  5  6  0]
 [ 0 17  9  3  0  3  0  5  7  2]
 [ 0  2  2  2  0  5  5  0  0  8]
 [ 0  0  0  0  0  2  0  0  0  0]
 [ 0  0  0  0  0  0  0  0  2  0]
 [ 0  0  0  0  0  0  0  0  0  0]
 [ 0  0  0  0  0  0  0  0  0  0]
]
END
    return @slices;
} # End of get_ref_robin_slices()...

sub get_ref_ortho_slices
{
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
    push(@slices, <<"END");

[
 [0 0 0 3 3 3 3 0 0 1]
 [0 0 0 3 3 3 3 1 0 0]
 [0 0 0 3 3 3 3 1 0 0]
 [0 0 0 0 3 3 3 3 0 0]
 [0 0 0 0 3 3 3 3 1 0]
 [0 0 0 0 0 3 3 3 3 0]
 [0 0 0 0 0 3 3 3 3 1]
 [0 0 0 0 0 0 3 3 3 3]
 [0 0 0 0 0 0 3 3 3 3]
 [0 0 0 0 0 0 0 3 3 3]
]
END
    push(@slices, <<"END");

[
 [61 61 61 61 59 55 56 60 64 66]
 [62 58 58 58 56 50 52 59 64 66]
 [63 57 57 57 55 48 50 58 64 66]
 [63 57 57 57 55 47 49 58 65 66]
 [55 58 63 59 51 43 47 57 65 52]
 [53 58 59 54 47 47 50 59 67 46]
 [53 58 56 51 45 51 53 61 68 52]
 [54 57 53 55 57 52 53 61 66 66]
 [62 55 53 60 66 50 51 55 59 67]
 [63 53 52 61 71 49 49 51 53 56]
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
    push(@slices, <<"END");

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
    return @slices;
} # End of get_ref_ortho_slices()...

sub get_ref_eqc_slices
{
    my @slices = ();
    push(@slices, <<"END");

[
 [254 254 254 254 254 254 254 254 254 254]
 [249 253 246 254 254 242 243 254 255 251]
 [255 209 255 255 255 233 236 255 255 253]
 [242 228 255 253 255 249 254 255 255 255]
 [255 242 255 249 254 238 255 255 255 255]
 [248 248  73  23 234  42 242 255 255 254]
 [ 64 114  61 108   4  35 242 229 226 239]
 [ 89   0  16   0   0   3 233 157 159 214]
 [ 14  32  12   0   1   0   0  37   0 167]
 [ 95   0   9  14   0   8   8   0   7   0]
]
END
    push(@slices, <<"END");

[
 [ 2  4  4  4  0  6 22 34 60 67]
 [ 1  0  5  0  1 32 58 69 64 68]
 [ 1  0  0  3  7 30 57 44  4 47]
 [ 1  0  1  2  0 61 64  3  0  0]
 [ 1  0  4 23 57 63 46  2  0  0]
 [ 3  1 27 54 61 57  0  0  0  0]
 [ 8  0 57 62 84 73  0  0  0  0]
 [ 7  0 30 76 81 56  0  0  1  1]
 [ 0  2 48 61 72 11  0  0  2  2]
 [ 0  0 53 33 78  0  0  0  1  1]
]
END
    push(@slices, <<"END");

[
 [  3   0   0   9   0   1   1   1   1   0]
 [ 17  65  66   5   9   1   1   1   1   0]
 [ 94  67  72  87   6   1   1   1   1   0]
 [ 73  69  72  68  95  37  11  10   4   2]
 [ 81  63  69  76  67 109  51   1   0   1]
 [ 72  63  71  71  64  95 101 104  72  53]
 [ 75  63  88  76  62  92  93  98 122 118]
 [ 79  77  71  72  69  75  85  98 121 109]
 [ 75  93  87  75  73  72  87  85  96 109]
 [ 81  79  73  82  82  78  91  88  91 102]
]
END
    push(@slices, <<"END");

[
 [48 48 58 44 39 36  0 10 10  7]
 [43 49 56 40 37 37  0  5 11  1]
 [44 41 54 38 44  0  0  5  2  2]
 [41 39 57 54 32 21  1  5  0  4]
 [36 31 50 48 43 20 12  4  0  2]
 [47 48 47 50 20 13  0  2  5  0]
 [55 49 19 52 23 14  0  2  5  0]
 [55 46 23 57 30 30  0  2  5  2]
 [49 22 26 23 50 19  0  2  5  4]
 [44 70  6 21 57  0  0  0  2  4]
]
END
    push(@slices, <<"END");

[
 [212 202 236  96 173 153 155   0   0  92]
 [208 204 244 181 113  76   5  70  82  67]
 [103 115 179 214 139 156 110  79 102 139]
 [129 145 136 179 187 212 120  74  93 158]
 [115 176 162 226 237 171 138 126 146 159]
 [175 172 251 243 248 248 249 232 189   5]
 [102 110 231 228 209 200 181 188 184  22]
 [161 240 247 169 162 239 197 190 189 239]
 [153 172 176 194 146 247 236 251 241 238]
 [  5   5  24  67  36 117 208  52 214 137]
]
END
    return @slices;
} # End of get_ref_eqc_slices()...
