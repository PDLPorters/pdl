=head1 NAME

PDL::Transform - Image transformations and N-D functions

=head1 SYNOPSIS

use PDL::Transform;

my $t = new PDL::Transform::<type>(<opt>)

$out = $t->apply($in)    # (threaded): apply a transform to some N-vectors

$im1 = $t->map($im);     # Transform image coordinates

$t2 = $t->compose($t1);  # compose two transforms

$t3 = $t2->inverse();    # invert a transform

=head1 DESCRIPTION

PDL::Transform is a convenient way to represent image coordinate
transformations.  It embodies functions mapping R^N -> R^M, both with
and without inverses.  Provision exists for parametrizing functions,
and for composing them.  You can use this part of the Transform
object to keep track of arbitrary functions mapping R^N -> R^M
with or without inverses.  

Transform also includes image mapping methods that use interpND.  You
define a coordinate transform using a Transform object, then apply
that to a PDL that contains an image.  The output is a remapped,
resampled image.  Eventually, the resampled image will have an updated
FITS header that corresponds to what coordinates it represents.

In keeping with standard practice, but somewhat counterintuitively,
the transform is used to map coordinates FROM the destination
dataspace (or image plane) TO the source dataspace; thus, to convert a
solar image from perspective to heliospheric lat/lon coordinates, you
need to apply a deprojection transform.  Since transforms AND their 
inverses are stored, this just means you can define the forward
transform, provided that you remember to use L<unmap|Transform::unmap> instead
of L<map|Transform::map>.

You can define and compose several transformations, then apply them
all at once to an image.  The image is interpolated only once, when
all the composed transformations are applied.

For terseness and convenience, most of the constructors are exported
into the current package with the name C<t_<transform>>, so the following
(for example) are synonyms:

  $t = new PDL::Transform::Radial();  # Long way

  $t = t_radial();                    # Short way

=head1 EXAMPLE

Coordinate transformations and mappings are a little counterintuitive
at first, because image mapping is usually done in the reverse
direction (output plane coordinates -> map -> input plane coordinates
to be interpolated).  Here are some examples of transforms in 
action:

   use PDL::Transform;
   $a = rfits('m51.fits');   # Substitute path if necessary!
   $tf = t_fits($a);         # FITS pixel transform
   $ts = t_linear(Scale=>3); # Scaling transform

   $w = pgwin(xs);
   $w->imag($a);

   ## Shrink m51 by a factor of 3; origin is at lower left.
   $b = $ts->map($a); 
   $w->imag($b);

   ## Grow m51 by a factor of 3; origin still at lower left.
   $c = $ts->unmap($a);
   $w->imag($c);

   ## Move the transform into FITS scientific space;
   ## $t gets ($tf^-1 o $ts o $tf).
   $t = $ts->wrap($tf);

   ## Shrink m51 by a factor of 3; origin is at scientific origin.
   $d = $t->map($a);
   $w->imag($d);

   ## Grow m51 by a factor of 3; origin is still at sci. origin.
   $e = $t->unmap($a);
   $w->imag($e);

   ## Grow m51 by a factor of 3; use sampling instead of bilinear interp.
   ##  (much faster!)
   $f = $t->unmap($a,{method=>sample});
   $w->imag($f);


=head1 INTERNALS

Transforms are perl hashes.  Here's a list of the meaning of each key:

=over 3

=item func

Ref to a subroutine that evaluates the transformed coordinates.  It's
called with the input coordinate, and the "params" hash.  This
springboarding is done via explicit ref rather than by subclassing,
for convenience both in coding new transforms (just add the
appropriate sub to the module) and in adding custom transforms at
run-time. Note that, if possible, new C<func>s should support
L<inplace|inplace> operation to save memory when the data are flagged 
inplace.  But C<func> should always return its result even when 
flagged to compute in-place.

C<func> should treat the 0th dimension of its input as a dimensional
index (running 0..N-1 for R^N operation) and thread over all other input
dimensions.

=item inv

Ref to an inverse method that reverses the transformation.  It must
accept the same "params" hash that the forward method accepts.  This
key can be left undefined in cases where there is no inverse.

=item indim, outdim

Number of useful dimensions for indexing on the input and output sides
(ie the order of the 0th dimension of the coordinates to be fed in or 
that come out).  If this is set to 0, then as many are allocated as needed.

=item name

A shorthand name for the transformation (convenient for debugging).
You should plan on using UNIVERAL::isa to identify classes of
transformation, e.g. all linear transformations should be subclasses
of PDL::Transform::Linear.  That makes it easier to add smarts to,
e.g., the compose() method.

=item params

Hash ref containing relevant parameters or anything else the func needs to 
work right.

=item is_inverse

Bit indicating whether the transform has been inverted.  That is useful
for some stringifications (see the PDL::Transform::Linear
stringifier), and may be useful for other things.

=back

Transforms should be inplace-aware where possible, to prevent excessive
memory usage.

If you define a new type of transform, consider generating a new stringify
method for it.  Just define the sub "stringify" in the subclass package.
It should call SUPER::stringify to generate the first line (though
the PDL::Transform::Composition bends this rule by tweaking the 
top-level line), then output (indented) additional lines as necessary to 
fully describe the transformation.

=head1 NOTES

Transforms currently have no mechanism for labeling the units or type
of each coordinate at each stage.  That is probably a necessary addition.
Currently, it just assumes that the coordinates are correct for (e.g.)
FITS scientific-to-pixel transformations.

Composition works OK but should be done in a more sophisticated way
so that, e.g., linear transformations are combined at the matrix level
instead of just strung together pixel-to-pixel.

Linear transformations should be handled with heterogeneous Hershey matrices,
rather than matrix-multiply-and-add operations.  

=head1 METHODS

=cut
use PDL::MatrixOps;

our $PI = 3.1415926535897932384626;
our $DEG2RAD = $PI / 180;
our $RAD2DEG = 180/$PI;
our $E  = exp(1);

package PDL::Transform;
use overload '""' => \&_strval;

$VERSION = "0.7";

BEGIN {
   use Exporter ();
   @ISA = ( Exporter );
   @EXPORT_OK = qw( t_identity t_lookup t_linear t_fits t_radial t_code t_inverse t_compose t_wrap t_scale t_rot t_shift t_pincushion );
   @EXPORT = @EXPORT_OK;
   %EXPORT_TAGS = ( Func=>[@EXPORT_OK] );
}

use PDL;
use PDL::MatrixOps;

use Carp;

use strict;

#### little helper kludge parses a list of synonyms

sub _opt {
  my($hash) = shift;
  my($synonyms) = shift;
  my($alt) = shift;  # default is undef -- ok.
  local($_);
  foreach $_(@$synonyms){
    return (UNIVERSAL::isa($alt,'PDL')) ? PDL->pdl($hash->{$_}) : $hash->{$_}
    if defined($hash->{$_}) ;
  }
  return $alt;
}

######################################################################
#
# Stringification hack.  _strval just does a method search on stringify
# for the object itself.  This gets around the fact that stringification
# overload is a subroutine call, not a method search.
#
sub _strval {
  my($me) = shift;
  $me->stringify();
}

######################################################################
#
# PDL::Transform overall stringifier.  Subclassed stringifiers should
# call this routine first then append auxiliary information.
#
sub stringify {
  my($me) = shift;
  my $out = (ref $me) . " (" . $me->{name} . "): ";
  $out .= "forward ". ((defined ($me->{func})) ? "ok" : "missing")."; ";
  $out .= "inverse ". ((defined ($me->{inv})) ? "ok" : "missing").".\n";
}


######################################################################

=head2 PDL::Transform::apply

=for sig

 Signature: ( PDL::Transform a; a(n) )

=for usage

  $out = $f->apply($data);

=for ref 

Apply the transform to some input coordinates.

The method just calls the predefined transform func -- you can call it
yourself, if you like, but then your code might break if the module
changes.

=cut

sub apply {
  my($me) = shift;
  my($from) = shift;

  print "PDL::Transform::apply...\n";

  return &{$me->{func}}($from,$me->{params})
    if defined($me->{func});

  croak "PDL::Transform with no defined func -- this is a bug.\n";
}

######################################################################

=head2 PDL::Transform::invert

=for usage

  $out = $f->invert($data);

=for ref

If an inverse exists, apply the inverse transform to some coordinates.

If the inverse does not exist, return undef.

=cut

sub invert {
  my($me) = shift;

  return undef unless defined($me->{inv});
  return &{$me->{inv}}(shift, $me->{params});
}
  

######################################################################

=head2 PDL::Transform::map

=for sig

 Signature: (PDL::Transform a; data(); template(); \%opt)

=for usage

  $output = $t->map($input,[<template>],[<options>]);

=for ref

Resample an image or N-D dataset using a coordinate transform.

The transform is applied to the coordinates of $output to obtain 
coordinates for interpolation from the $input array.  NOTE that this is
somewhat counterintuitive at first; but it is the correct way to do 
image distortion.

The output has the same data type as the input.  This is a feature,
but it can lead to strange-looking banding behaviors if you use interpolation
on an integer input variable.

<template> can be one of:

=over 3

=item * a PDL

The PDL is used as a template for the output: it and its header are
copied to the output array, which is then populated with data.  If the
PDL has a FITS header, then the FITS transform is automatically
applied so that $t applies to the output scientific coordinates and
not to the output pixel coordinates.  In this case the NAXIS fields of
the FITS header are ignored.

=item * a list ref

This is a list of dimensions for the output array.  

=item * a FITS header hash ref

The FITS NAXIS fields are used to define the output array, and the
FITS transformation is applied to the coordinates so that $t applies to the
output scientific coordinates.

=item * nothing

In this case, $input (including any FITS header) is used as a template,
so the output image matches the input image in size and sci-to-pixel mapping.

=back

OPTIONS:

The following options are interpreted:

=over 3

=item m, method, Method

This option controls the interpolation method to be used.  Interpolation
greatly affects both speed and quality of output.  Possible options, in order
from fastest to slowest, are:
  
=over 3

=item * s, sample (default for integers)

Pixel values in the output plane are sampled from the closest data value
in the input plane.  This is very fast but not very accurate for either 
magnification or decimation (shrinking).  It is the default for templates
of integer type.

=item * l, linear (default for floats)

Pixel values are linearly interpolated from the closst data value in the 
input plane.  This is reasonably fast but only accurate for magnification.
Decimation (shrinking) of the image causes aliasing and loss of photometry
as features fall between the samples.  It is the default for floating-point
templates.

=item * j, jacobian

Pixel values are filtered through a spatially-variable filter tuned to the
computed Jacobian of the transformation.  This is the mathematically
correct way to deform images and yields very good results -- but 
at a cost.  It runs perhaps 10 times slower than linear interpolation.
See the notes on Jacobian tracking, below.

=back

=item e, ecc, eccentricity, Eccentricity (default=10)

This is the maximum eccentricity that is allowed for the local ellipse of
transformation in the Jacobian method of interpolation.  Lower numbers
yield better memory efficiency and speed, at a cost of some blurring in the
case of pathological transformations (that stretch much more in one direction
than others).  

=item b, blur, Blur

This is the half-radius of the Gaussian filter used for the "jacobian"
method, in units of output pixels.  It defaults to 0.7 pixel, which provides
a minimal amount of overlap with adjacent pixels while minimizing aliasing.

=item big, Big

This is the largest allowable input spot size which may be mapped to a
single output pixel.  The default is 0.2 x the largest dimension of the 
input array.  

=item p,phot, photometry, Photometry

This lets you set the style of photometric conversion to be used in the 
"jacobian" method.  You may choose:

=over 3

=item * s, surf, surface, Surface

(this is the default): surface brightness is preserved over the transformation,
so features maintain their original intensity.

=item * f, flux, Flux

Total flux is preserved over the transformation, so that the brightness
integral over image regions is preserved.  Parts of the image that are
shrunk wind up brighter; parts that are enlarged end up fainter.

=back

=back

JACOBIAN TRACKING:

This method of interpolation gives photometrically accurate resampling
of the input data for arbitrary transformations.  The Jacobian of the
reverse transformation is the matrix C<J_ij = d X_i / d x_j>, where i
and j are index variables, the X_i are the input-plane coordinates,
and the x_j are the output-plane coordinates.  At each pixel, the code
generates a linear approximation to the transformation using the local
discrete Jacobian.  The output pixels are treated as circles of radius
1.0, and transformed via the linear approximation to ellipses in the
input plane.  The eigenvalues of the Jacobian are padded to a minimum
of 1.4, ensuring that the transformed ellipses are fat enough to
encounter at least one sample point in the input plane.  

To avoid numerical runaway, there are some limitations on the reverse-
transformed ellipses.  In particular, the computational efficiency
scales inversely as the ratio between the largest and smallest
eigenvalues, so the ellipses are not allowed to get too eccentric.  The
maximum eccentricity is given in the C<eccentricity> option, and 





NOTES:

Currently FITS headers are detected but not acted upon.  You must 
handle your FITS transformations manually.

Jacobian tracking is a memory hog, especially if the transformation includes
very large regions outside of the original input plane.  Some sort of memory
guard needs to be put in place.

=cut

sub map {
  my($me) = shift;
  my($in) = shift;

  my($tmp) = shift;
  my($opt) = shift;
     
  # Check for options-but-no-template case
  if(ref $tmp eq 'HASH' && !(defined $opt)) {
    if(!defined($tmp->{NAXIS})) {  # FITS headers all have NAXIS.
      $opt = $tmp;
      $tmp = undef;
    }
  }

  $tmp = $in unless(defined($tmp));


  my($out);
  if(UNIVERSAL::isa($tmp,'PDL')) {
    $out = $tmp->copy;
    
    my($a);
    if(defined ($a = $tmp->gethdr)) {
      my(%b) = %{$a};
      $out->sethdr(\%b);
    }
  } elsif(ref $tmp eq 'HASH') {
    if($tmp->{NAXIS}) {
      my(@axes);
      for my $i(1..$tmp->{NAXIS}){
	push(@axes,$tmp->{NAXIS$i});
      }
      $out = zeroes(@axes);

      my(%b) = %{$tmp};
      $out->sethdr(\%b);
    }
  } elsif(ref $tmp eq 'ARRAY') {
    $out = zeroes(@$tmp);
  } else {
    $out = zeroes($tmp);
  }

  $PDL::debugerooni_out = $out;

  my($integrate) = scalar(_opt($opt,['m','method','Method']) =~ m/[jJ](ac(obian)?)?/);


  if(!$integrate) {
    
    ##############################
    # Make an index array, then transform it to get the coordinates
    # in the input plane.
    my $indices = PDL->zeroes($out->ndims,$out->dims); 
    for my $dim(0..$out->ndims-1) { 
      my($sl) =  $indices->slice("(".($dim).")")->xchg(0,$dim); 
      $sl .= PDL->xvals($sl->dims); 
    }
    
    $indices = $me->apply($indices->inplace);

    $out .= $in->interpND($indices,$opt);
  }

  ##############################
  ### Integration code -- 
  ### Find the local Jacobian, then use a warped filter to 
  ### grab the values of all points within the N-cube defined 
  ### by the largest eigenvalue.  Keep track of the filter 
  ### weighting and scale it to match the original determinant of the 
  ### Jacobian.

  else {
    my($i,$j);
    my($nd) = $out->ndims;
    my($nd_in) = $in->ndims;
    my(@sizes) = $out->dims;
    
    #
    # Chicken out
    #
    barf("can't do integrative mapping from N->M when N != M (yet).\n")
      if($nd != $nd_in);

    ###############
    ### Interpret integration-specific options...
    my($ecc) = _opt($opt,['e','ecc','eccentricity','Eccentricity'],10.0);

    my $blur  = _opt($opt,['b','blur','Blur']) || 0.7;
    my $blur2 = 1.0 / $blur / $blur ; # used inside the Gaussian, below

    my $flux = scalar((_opt($opt,['p','phot','photometry','Photometry'])) =~
		m/[fF](lux)?/);

    ###############
    ### Enumerate & warp the coordinates of all pixel corners.
    ### Then average the pixel corners together to get the 
    ### transformed pixel centers, and difference them to get the jacobian.

    my($indices) = PDL->zeroes($nd, (PDL->pdl(@sizes)+1)->list );#(coord,dims)
    for my $dim(0..$out->ndims-1) {
      my($sl) = $indices->slice("(".($dim).")")->xchg(0,$dim);
      $sl .= PDL->xvals($sl->dims)-0.5;
    }

    $indices = $me->apply($indices->inplace); #(dim,list)


    my ($slstr1,$slstr2);
    chop ( $slstr1 = ":,".("0:-2," x $nd) );
    chop ( $slstr2 = ":,".("1:-1," x $nd) );
		   
    my $center = (pdl(0.5) * 
		  ( ($indices->slice($slstr1) + $indices->slice($slstr2))->
		    reorder(1..$nd,0)->  # (dims, coord)
		    clump($nd)           # (list, coord)
		    )
		  );  # center gets (list,coord)

    my($jacob) = PDL->zeroes($nd, $nd, @sizes); # (col, row, dims)
    my($slstr) = ",0:-2"x($nd-1);
    for($i=0;$i<$nd;$i++){
      my($ind) = $indices->xchg(1,$i+1);
      $jacob->slice("($i)") .= ( $ind->slice(":,1:-1".$slstr) - 
				 $ind->slice(":,0:-2".$slstr)
				 ) -> xchg(1,$i+1);
    }
    
    my $jac  = $jacob->reorder(2..($nd+1),0,1)->clump($nd)
		->mv(0,2);                              # (col, row, list)
    $jacob = undef;

    ###############
    ### Determinant comes in handy for the photon-preserving case,
    ### but needs to be taken from the non-fattened Jacobian (see below)
    my $jdet = $jac->det;

    ###############
    ### Singular-value decompose the Jacobian and fatten it to ensure 
    ### at least one intersection with the grid.  Then save the size
    ### of the enclosing N-cube, for use down below. 
    ### This is in a block to get rid of the temporary variables.
    ###
    ### smin gets the maximum of itself, the N-cube diagonal, and the largest 
    ### singular value divided by the largest acceptable aspect ratio.
    my $sizes;
    { 
      my ($r1, $s, $r2) = svd $jac;
      $s .= $s->cat( $s->maximum->dummy(0,$nd),
		     ones($sm)*sqrt($nd)
		     )
	->mv(-1,0)->maximum;
      $r2 *= $s->dummy(1,$nd);  # cheap mult; dummy keeps threading right
      $jac .= $r2 x $r1;
    }      


    ###############
    ### We're done with the Jacobian but need the inverse Jacobian.
    ### Fortunately, $jac is always invertible since we've constructed
    ### its singular values to be nonzero.
    my $ijac = $jac->inv;
    $jac = undef;

    ###############
    ### Loop over increasing size, doubling the size each time until
    ### the very largest pixels are handled.  The largest pixel we
    ### can handle is the size of the input dataset!
    ###
    ### The sampling is done not on the original image (necessarily) but
    ### rather on a reduced version of the original image.  The amount of
    ### reduction is determined by the pixel size divided by the maximum
    ### allowed eccentricity.
    my $size;
    my $last_size=0;
    my $outf = $out->flat;
    my $maxdim = (PDL->pdl($in->dims))->max;
    my $reduced = $in;
    my $reduction = 1;

    for($size=4; $size < $maxdim*2; $size *= 2){
      $size = $maxdim
	if($size > $maxdim);
      my($pixels) = ($size < $maxdim) ? 
	which($sizes >= $last_size/2.0 & $sizes < $size/2.0) :
	which($sizes >= $last_size/2.0);

      print "found ",$pixels->nelem," points..." if($PDL::debug);
      next if($pixels->nelem == 0);

      ###############
      ### Figure out if we have to reduce and, if so, do it.
      ### Reduction is brute-force (and hence introduces aliasing)
      ### but that shouldn't matter much since we antialias later;
      ### it certainly doesn't contribute in first order.
      ### 
      ### The image is freshly reduced each time, which is a waste
      ### of cycles -- it should be reduced by a factor 
      my($reduce) = $size / ($ecc + 1);
      if($reduce > 2) {
	$reduce = 2 ** ( (PDL->log($reduce) / PDL->log(2))->floor );
      }

      if($reduce > $reduction) {
	my @rdims = PDL->pdl($in->dims) / $reduce -> ceil;
	my $r2dex = ndcoords($rdims) * $reduce / $reduction;
	my $r2 = $reduced->range($r2dex

	
      }


      ###############
      ### Enumerate a cube of coordinate offsets of the current size.
      ### float not double, to save space.
      print "enumerating $nd-cube, side $size..." if($PDL::debug);
      my @zlist = (zeroes($nd)+$size)->list;
      my($coords_in) = zeroes( float, $nd, (zeroes($nd)+$size)->list );

      for my $i(0..$nd-1) {
	$coords_in->index($i)->xchg(0,$i) .= xvals(@zlist);
      }
      
      ###############
      ### Duplicate the coordinate cube for each pixel.  This step
      ### is a real memory hog -- if crashes occur here, then subdivide
      ### down to reasonable memory sizes with a for loop.
      print "duplicating ",$pixels->nelem," times..." if($PDL::debug);
      $coords_in = $coords_in->                       # (coord,<dims>)
		   reorder(1..$nd,0)->                # (<dims>,coord)
		   clump($nd)->                       # (rgn-list,coord)
		   xchg(0,1)->                        # (coord,rgn-list)
		   dummy(0,$pixels->nelem)->          # (pix-list,coord,rgn-list)
		   sever;

      ###############
      ### Offset to get an array of indices into the input
      ### the indexND call is used because of the double-threading.
      ###
      print "offsetting..." if($PDL::debug);
      my($points) = $center->indexND($pixels->dummy(0,1));  # (pix-list, coord)
      $coords_in += $points->floor - 0.5*$size + 1;         # (pix-list, coord, rgn-list)

      ###############
      ### Calculate the offsets (in output space) of each center from 
      ### the appropriate pixel center, and convert to r^2;
      ###
      ### The intermediate variable ijac_p could be eliminated...
      print "calculating offsets..." if($PDL::debug);
      my $ijac_p = ( $ijac->            # (col, row, list)
		     reorder(2,0,1)->   # (list, col, row)
		     indexND($pixels->dummy(0,1))->   # (pix-list, col, row)
		     reorder(1,2,0) );  # (col, row, pix-list)
#      my $of0 = ($coords_in - $points);
#      $of0 *= $of0;
      my $offsets = ($ijac_p x                   # (col, row, pix-list)
		     ($coords_in - $points)->    #    (pix-list,coord,rgn-list)
		        xchg(0,1)->dummy(0,1)    # (null,coord,pix-list,rgn-list)
		     )	->slice("(0)");          # (coord,pixlist,rgn-list)
       $offsets *= $offsets; # (coord, pixlist, rgn-list)
      
      ###############
      ### Calculate filter weighting values.  Since we scale at the 
      ### end to match either 1.0 or the original jacobian, we don't
      ### bother to scale the weighting function here -- but we do accumulate
      ### the filter weighting total into f_norm.

      print "calculating filter values..." if($PDL::debug);
      my $filt = $offsets->sumover->xchg(0,1);  # (rgn-list,pix-list) R^2
      $offsets = undef;    # free up memory    
      $filt *= (-$blur2);                # blur width scale from above
      $filt->inplace->exp;               # make Gaussian. (rgn-list, pix-list)
      my $f_norm = $filt->sumover;       # (pix-list)

      if(defined $PDL::debug::w && ($size >4)) {
	  print "Saving pixels...\n" if($PDL::debug);
	my $i;
	for ($i=0;$i<$filt->dim(1);$i++) {
	  $PDL::debug::w = new PDL::Graphics::PGPLOT::Window(Dev=>sprintf("%6.6d.gif/gif",++($PDL::debug::n)),
				 Size=>[3,3]);
	  $PDL::debug::w->imag($filt->slice(":,($i)")->reshape($size,$size));
	  $PDL::debug::w->close;
	  print " . " if($PDL::debug && !($i%100));
	}
      }
      
      ###############
      ### Pedal to the metal -- accumulate input values and scale by their
      ### appropriate filter value.
      print "grabbing inputs..." if($PDL::debug);

      my($input) = $in->range(
			      $coords_in->         #(pix-list,coord,rgn-list)
			        slice(":,:,(0)")-> #(pix-list,coord)
			        xchg(0,1)          #(coord,pix-list)
			      ,
			      $size,'e')->         # (pix-list,<dims>)
		      reorder(1..$nd,0)->          # (<dims>,pix-list)
		      clump($nd)                   # (rgn-list,pix-list)
		      ->sever;

      $filt *= $input;  # (rgn-list, pix-list)

      ### Stick the summed, filtered output back into the output array!
      ### Whew!  (Come back and scale here for photometric rather than
      ### surface area preservation).
      ### range is used here because indexND doesn't preserve dataflow
      ### (it should and probably will sooner or later).
      if($flux) {
	my $p_jdet = $jdet->indexND($pixels->dummy(0,1));
	$out->flat->range($pixels->dummy(0,1)) .=
	  ($filt->sumover * $p_jdet / $f_norm);
      } else {
	$out->flat->range($pixels->dummy(0,1)) .= 
	  ($filt->sumover / $f_norm);  # (pix-list)
      }

      print "ok\n" if($PDL::debug);
      $last_size = $size;
    }
  }
  $out;
}  
  
######################################################################

=head2 PDL::Transform::unmap

=for sig

 Signature: (PDL::Transform a; data(); template(); \%opt)

=for usage

  $out_image = $t->unmap($in_image,[<options>],[<template>]);

=for ref

Map an image or N-D dataset using the inverse as a coordinate transform.

This convenience function just inverts $t and calls map
on the inverse; everything works the same otherwise.

=cut

sub unmap {
  my($me) = shift;
  my(@params) = @_;

  return $me->inverse->map(@_);
}

######################################################################

=head1 OPERATORS

These are quasi-constructors: they accept PDL::Transform objects,
and return others.  You can access operators using perl's method call
mechanism or by direct call using the exported C<t_<name>> subroutine.

=cut

######################################################################

=head2 t_inverse 

=for usage 

  $t2 = t_inverse($t);
  $t2 = $t->inverse;

=for ref

Return the inverse of a PDL::Transform.  This just reverses the 
func/inv pair and the idims/odims pair.  Note that sometimes you
end up with a transform that can't be applied or mapped, because
either the mathematical inverse doesn't exist or the inverse func isn't
implemented.

The inverse transform remains connected to the main transform because
they both point to the original parameters hash.  That turns out to be
useful.

=cut

*t_inverse = \&inverse;

sub inverse {
  my($me) = shift;

  unless(defined($me->{inv})) {
    Carp::cluck("PDL::Transform::inverse:  got a transform with no inverse.\n");
    return undef;
  }

  my(%out) = %$me; # force explicit copy of top-level
  my($out) = \%out;
  
  $out->{inv}  = $me->{func}; 
  $out->{func} = $me->{inv};

  $out->{idims} = $me->{odims};
  $out->{odims} = $me->{idims};

  $out->{name} = "(inverse ".$me->{name}.")";

  $out->{is_inverse} = !($out->{is_inverse});

  bless $out,(ref $me);
  return $out;
}
  
  
######################################################################

=head2 t_compose 

=for usage

  $f2 = t_compose($f, $g,[...]);
  $f2 = $f->compose($g[,$h,$i,...]);

=for ref 

Function composition: f(g(x)), f(g(h(x))), ...

This is accomplished by inserting a splicing code ref into the C<func>
and C<inv> slots.  It combines multiple compositions into a single
list of transforms to be executed in orer.  If one of the functions is
itself a composition, it is interpolated into the list rather than left
intact.  Ultimately, linear transformations may also be combined within
the list.

=cut

@PDL::Transform::Composition::ISA = ('PDL::Transform');
sub PDL::Transform::Composition::stringify {
  package PDL::Transform::Composition;
  my($me) = shift;
  my($out) = SUPER::stringify $me;
  $out =~ s/ /\n  /;
  $out =~ s/: /:\n  /;
  $out;
}

*t_compose = \&compose;

sub compose {
  local($_);
  my(@funcs) = @_;
  my($me) = PDL::Transform->new;

  # No inputs case: return the identity function
  return $me
    if(!@funcs);

  $me->{name} = "";
  my($f);
  my(@clist);

  for $f(@funcs) {
    if(UNIVERSAL::isa($f,"PDL::Transform::Composition")) {
      if($f->{is_inverse}) {
	for(reverse(@{$f->{params}->{clist}})) {
	  push(@clist,$_->inverse);
	  $me->{name} .= " o inverse ( ".$_->{name}." )";
	}
      } else {
	for(@{$f->{params}->{clist}}) {
	  push(@clist,$_);
	  $me->{name} = " o ".$_->{name};
	}
      }
    } else {  # Not a composition -- just push the transform onto the list.
      push(@clist,$f);
      $me->{name} .= " o ".$f->{name};
    }
  }

  $me->{name}=~ s/^ o //; # Get rid of leading composition mark

  $me->{params}->{clist} = \@clist;

  $me->{func} = sub {
    my ($data,$p) = @_;
    my ($ip) = $data->is_inplace;
    for my $t ( reverse @{$p->{clist}} ) {
      $data = $t->{func}($ip ? $data->inplace : $data, $t->{params});
    }
    $data;
  };

  $me->{inv} = sub {
    my($data,$p) = @_;
    my($ip) = $data->is_inplace;
    for my $t ( @{$p->{clist}} ) {
      $data = $t->{inv}($ip ? $data->inplace : $data, $t->{params});
    }
    $data;
  };

  return bless($me,'PDL::Transform::Composition');
}

######################################################################

=head2 t_wrap

=for usage

  $g1fg = $f->wrap($g);
  $g1fg = t_wrap($f,$g);

=for ref

Shift a transform into a different space by 'wrapping' it with a second.

This is just a convenience function for two L<compose|Transform::compose> calls.
It is useful to make a single transformation happen in some other space.
For example, to shift the origin of rotation, do this:

  $im = rfits('m51.fits');
  $tf = new PDL::Transform::FITS($im);
  $tr = new PDL::Transform::Linear({rot=>30});
  $im1 = $tr->unmap($tr);               # Rotate around pixel origin
  $im2 = $tr->unmap($tr->wrap($tf));    # Rotate round FITS scientific origin

=cut

*t_wrap = \&wrap;

sub wrap {
  my($f) = shift;
  my($g) = shift;
  
  return $g->inverse->compose($f,$g);
}



######################################################################

=head1 CONSTRUCTORS

The constructors all return subclasses of PDL::Transform, so you can say:
  
  $a = new PDL::Transform::<name>(<params>)
or
  $a = t_<name>(<params>)

and get the same result.

=cut

######################################################################

=head2 t_identity

=for usage

  my $xform = t_identity
  my $xform = new PDL::Transform;

=for ref

Generic constructor generates the identity transform.

This constructor really is trivial -- it is mainly used by the other transform 
constructors.  It takes no parameters and returns the identity transform.

=cut

sub _identity { return shift; }
sub t_identity { new PDL::Transform(@_) };

sub new {
  my($class) = shift;
  my $me = {name=>'identity', 
	    indim => 0,
	    outdim => 0,
	    func=>{\&PDL::Transform::_identity}, 
	    inv=>{\&PDL::Transform::_identity},
	    params=>{}
	  };
  
  return bless $me,$class;
}
  
######################################################################

=head2 t_lookup 

=for usage

  $f = t_lookup($lookup, {<options>});
  $f = new PDL::Transform::Lookup($lookup, { <options> });

=for ref

Transform by lookup into an explicit table.

You specify an N+1-D PDL that is interpreted as an N-D lookup table of
column vectors (vector index comes last).  The last dimension has
order equal to the output dimensionality of the transform.

For added flexibility in data space, You can specify pre-lookup linear
scaling and offset of the data.  Of course you can specify the
interpolation method to be used.  The linear scaling stuff is a little
primitive; if you want more, try composing the linear transform with
this one.

The prescribed values in the lookup table are treated as
pixel-centered: that is, if your input array has N elements per row
then valid data exist between the locations (-0.5) and (N-0.5) in
lookup pixel space, because the pixels (which are numbered from 0 to
N-1) are centered on their locations.

Lookup is done using L<interpND|interpND>, so the boundary conditions
and threading behaviour follow from that.

The indexed-over dimensions come first in the table, followed by a
single dimension containing the column vector to be output for each
set of other dimensions -- ie to output 2-vectors from 2 input
parameters, each of which can range from 0 to 49, you want an index
that has dimension list (50,50,2).  For the identity lookup table
you could use  C<cat(xvals(50,50),yvals(50,50))>.  

If you want to output a single value per input vector, you still need
that last index threading dimension -- if necessary, use C<dummy(-1,1)>.

The lookup index scaling is: out = lookup[ (scale * data) + offset ].

The inverse transform is calculated. 

Options are listed below; there are several synonyms for each.

=over 3

=item s, scale, Scale

(default 1.0) Specifies the linear amount of scaling to be done before 
lookup.  You can feed in a scalar or an N-vector; other values may cause
trouble.  If you want to save space in your table, then specify smaller 
scale numbers. 

=item o, offset, Offset

(default 0.0) Specifies the linear amount of offset before lookup.  
This is only a scalar, because it is intended to let you switch to 
corner-centered coordinates if you want to (just feed in o=-0.25).

=item b, bound, boundary, Boundary

Boundary condition to be fed to L<interpND|interpND>

=item m, method, Method

Interpolation method to be fed to L<interpND|interpND>

=back

EXAMPLE

To scale logarithmically the Y axis of m51, try:

  $a = rfits('m51.fits');
  $lookup = xvals(256,256) -> cat( 10**(yvals(256,256)/100) * 256/10**2.55 );
  $t = t_lookup($lookup);
  $b = $t->map($a);

To do the same thing but with a smaller lookup table, try:

  $lookup = 16 * xvals(17,17)->cat(10**(yvals(17,17)/(100/16)) * 16/10**2.55);
  $t = t_lookup($lookup,{scale=>1/16.0});
  $b = $t->map($a);

(Notice that, although the lookup table coordinates are is divided by 16, 
it is a 17x17 -- so linear interpolation works right to the edge of the original
domain.)

NOTES

Inverses are not yet implemented -- the best way to do it might be by 
judicious use of map() on the forward transformation.

=cut

@PDL::Transform::Lookup::ISA = ('PDL::Transform');

sub t_lookup {new PDL::Transform::Lookup(@_);}

sub PDL::Transform::Lookup::new {
  my($class) = shift;
  my($source)= shift;
  my($o) = shift;

  if(!defined($o) && ((ref $source) eq 'HASH')) {
    Carp::cluck("lookup transform called as sub not method; using 'PDL::Transform' as class...\n");
    $o = $source;
    $source = $class;
    $class = "PDL::Transform";
  }

  $o = {} unless(ref $o eq 'HASH');

  my($me) = PDL::Transform::new($class);

  my($bound) = _opt($o,['b','bound','boundary','Boundary']);
  my($method)= _opt($o,['m','meth','method','Method']);

  $me->{idims} = $source->ndims - 1;
  $me->{odims} = $source->dims($source->ndims-1);

  $me->{params} = {
      table => $source,
      scale =>  _opt($o,['s','scale','Scale'],1.0),
      offset => _opt($o,['o','off','offset','Offset'],0.0),
      interpND_opt => {
	method => $method,
        bound =>  $bound,
	bad   => _opt($o,['bad'],0)
      }
    };

  
   my $lookup_func = sub {
     my($data,$p,$table_name) = @_;

    if($data->dim(0) > $me->{idims}) {
      croak("Too many dims (".$data->dim(0).") for your table (".$me->{idims}.")\n");
    };

    $data = pdl($data) 
      unless ((ref $data) && (UNIVERSAL::isa($data,'PDL')));

    my($a)= ($p
	     ->{$table_name}
	     ->interpND(float($data) * $p->{scale} + $p->{offset},
			$p->{interpND_opt}
			)
	     );
    

    # Put the index dimension (and threaded indices) back at the front of
    # the dimension list.
    my($dnd) = $data->ndims - 1;
    return ($a -> ndims > $data->ndims - 1) ? 
      ($a->reorder( $dnd..($dnd + $p->{$table_name}->ndims - $data->dim(0)-1)
		    , 0..$data->ndims-2
		    )
       ) : $a;
  };

  $me->{func} = sub {my($data,$p) = @_;  &$lookup_func($data,$p,'table')};

  #######
  ## Lazy inverse -- find it if and only if we need it...
  $me->{inv} = sub {
      my $p = shift;
      if(!defined($p->{'itable'})) {
	barf "Inversion of lookup transforms is not yet implemented\n";
      }
      &$lookup_func(@_[0..1], 'itable') ;
    };


  $me->{name} = 'Lookup';

  return $me;
}

######################################################################

=head2 t_linear

=for usage

$f = t_linear({options});  
$f = new PDL::Transform::Linear( {Options} );

=for ref

Heterogeneous linear coordinate transformations.  

You specify the linear transformation with pre-offset, a mixing
matrix, and a post-offset.  That overspecifies the transformation, so
you can choose your favorite method to specify the transform you want.
The inverse transform is automagically generated, provided that it
actually exists (the transform matrix is invertible).  Otherwise, the
inverse transform just croaks.

The options you can usefully pass in are:

=over 3

=item s, scale, Scale

A scaling scalar (heh), vector, or matrix.  If you specify a vector
it is treated as a diagonal matrix (for convenience).  It gets
left-multiplied with the transformation matrix you specify (or the
identity), so that if you specify both a scale and a matrix the
scaling is done after the rotation or skewing or whatever.

=item r, rot, rota, rotation, Rotation

A rotation angle in degrees -- useful for 2-D and 3-D data only.  If
you pass in a scalar, it specifies a rotation from the 0th axis toward
the 1st axis.  If you pass in a 3-vector, it is treated as a set of
Euler angles, and a rotation matrix is generated that does the following, in
order:

=over 3

=item * Rotate by rot->(2) degrees from 0th to 1st axis

=item * Rotate by rot->(1) degrees from the 2nd to the 0th axis

=item * Rotate by rot->(0) degrees from the 1st to the 2nd axis

=back

The rotation matrix is left-multiplied with the transformation matrix
you specify, so that if you specify both rotation and a general matrix
the rotation happens after the more general operation -- though that is
deprecated.

Of course, you can duplicate this functionality -- and get more
general -- by generating your own rotation matrix and feeding it in 
with the C<matrix> option.

=item m, matrix, Matrix

The transformation matrix.  It does not even have to be square, if you want
to change the dimensionality of your input.  If it is invertible (note: 
must be square for that), then you automagically get an inverse transform too.

=item pre, preoffset, offset, Offset

The vector to be added to the data before they get multiplied by the matrix
(equivalent of CRVAL in FITS, if you are converting from scientific to 
pixel units).

=item post, postoffset, shift, Shift

The vector to be added to the data after it gets multiplied by the matrix
(equivalent of CRPIX-1 in FITS, if youre converting from scientific to pixel 
units).

=item dims, Dims

Most of the time it is obvious how many dimensions you want to deal
with: if you supply a matrix, it defines the transformation; if you
input offset vectors in the C<pre> and C<post> options, those define
the number of dimensions.  But if you only supply scalars, there is no way
to tell and the default number of dimensions is 2.  This provides a way 
to do, e.g., 3-D scaling: just set C<{s=><scale-factor>, dims=>3}> and
you are on your way.

=back


=cut

@PDL::Transform::Linear::ISA = ('PDL::Transform');

sub t_linear { new PDL::Transform::Linear(@_); }

sub PDL::Transform::Linear::new {
  my($class) = shift;
  my($o) = $_[0];
  if(!(ref $o)) {
    $o = {@_};
  }
  
  my($me) = PDL::Transform::new($class);
  
  $me->{name} = "linear";
  
  $me->{params}->{pre} = _opt($o,['pre','Pre','preoffset','offset',
				  'Offset','PreOffset','Preoffset'],0);
  $me->{params}->{pre} = pdl($me->{params}->{pre}) 
    if(defined $me->{params}->{pre});
  
  $me->{params}->{post} = _opt($o,['post','Post','postoffset','PostOffset',
				   'shift','Shift'],0);
  $me->{params}->{post} = pdl($me->{params}->{post}) 
    if(defined $me->{params}->{post});

  $me->{params}->{matrix} = _opt($o,['m','matrix','Matrix','mat','Mat']);
  $me->{params}->{matrix} = pdl($me->{params}->{matrix}) 
    if(defined $me->{params}->{matrix});

  $me->{params}->{rot} = _opt($o,['r','rot','rota','rotation','Rotation']);
  $me->{params}->{rot} = 0 unless defined($me->{params}->{rot});
  $me->{params}->{rot} = pdl($me->{params}->{rot});

  my $o_dims = _opt($o,['dims','Dims']);
  $o_dims = pdl($o_dims)
    if defined($o_dims);
  
  my $scale  = _opt($o,['s','scale','Scale']);
  $scale = pdl($scale) 
    if defined($scale);
  
  # Figure out the number of dimensions to transform, and, 
  # if necessary, generate a new matrix.
  
  if(defined($me->{params}->{matrix})) {
    
    $me->{idim} = $me->{params}->{matrix}->dim(0);
    $me->{odim} = $me->{params}->{matrix}->dim(1);
    
  } else {
    
    if(defined($scale) && 
       UNIVERSAL::isa($scale,'PDL') && 
       $scale->getndims > 0) {
      $me->{idim} = $me->{odim} = $scale->dim(0);
      $me->{odim} = $scale->dim(0);
      
    } elsif(defined($me->{params}->{pre}) && 
	    UNIVERSAL::isa($me->{params}->{pre},'PDL') &&
	    $me->{params}->{pre}->getndims > 0) {
      $me->{idim} = $me->{odim} = $me->{params}->{pre}->dim(0);
      
    } elsif(defined($me->{params}->{post}) &&
	    UNIVERSAL::isa($me->{params}->{post},'PDL') &&
	    $me->{params}->{post}->getndims > 0) {
      $me->{idim} = $me->{odim} = $me->{params}->{post}->dim(0);
    } elsif(defined($o_dims)) {
      $me->{idim} = $me->{odim} = $o_dims;
    } else {
      print "PDL::Transform::Linear: assuming 2-D transform (set dims option to change)\n" if($PDL::debug);
      $me->{idim} = $me->{odim} = 2;
    }
    
    $me->{params}->{matrix} = PDL->zeroes($me->{idim},$me->{odim});
    $me->{params}->{matrix}->diagonal(0,1) .= 1;
  }

  ### Handle rotation option 
  my $rot = $me->{params}->{rot};
  if(defined($rot)) {
    # Subrotation closure -- rotates from axis $d->(0) --> $d->(1).
    my $subrot = sub { 
                       my($d,$angle,$m)=@_;
		       my($subm) = $m->dice($d,$d);

		       $angle = $angle->at(0)
			 if(UNIVERSAL::isa($angle,'PDL'));

		       my($a) = $angle * $DEG2RAD;
		       $subm x= pdl([cos($a),-sin($a)],[sin($a),cos($a)]);
		     };
    
    if(UNIVERSAL::isa($rot,'PDL') && $rot->nelem > 1) {
      if($rot->ndims == 2) {
	$me->{params}->{matrix} x= $rot;
      } elsif($rot->nelem == 3) {
	my $rm = zeroes(3,3);
	$rm->diagonal(0,1)++;
	
	&$subrot(pdl(0,1),$rot->at(2),$rm);
	&$subrot(pdl(2,0),$rot->at(1),$rm);
	&$subrot(pdl(1,2),$rot->at(0),$rm);
	$me->{params}->{matrix} x= $rm;
      } else {
	barf("PDL::Transform::Linear: Got a strange rot option -- giving up.\n");
      }
    } else {
      &$subrot(pdl(0,1),$rot,$me->{params}->{matrix});
    }
  }


  #
  # Apply scaling
  #
  $me->{params}->{matrix}->diagonal(0,1) *= $scale
    if defined($scale);
  
  #
  # Check for an inverse and apply it if possible
  #
  my($o2);
  if($me->{params}->{matrix}->det($o2 = {lu=>undef})) {
    $me->{params}->{inverse} = $me->{params}->{matrix}->inv($o2);
  } else {
    delete $me->{params}->{inverse};
  }
  
  $me->{func} = sub {
    my($in,$opt) = @_;
    my($a) = $in + $opt->{pre};

    my($outmat) = $a x $opt->{matrix};
    return $outmat+$_[1]->{post};
  };
  
  $me->{inv} = (defined $me->{params}->{inverse}) ? sub {
    my($in,$opt) = @_;
    my($a) = $in - $opt->{post};


    my($outmat) = $a x $opt->{inverse};
    return $outmat-$_[1]->{pre};
  } : undef;
  
  return $me;
}

sub PDL::Transform::Linear::stringify {
  package PDL::Transform::Linear;
  my($me) = shift;  my($out) = SUPER::stringify $me;
  my $mp = $me->{params};
  
  if(!($me->{is_inverse})){
    $out .= "Pre-add: ".($mp->{pre})."\n"
      if(defined $mp->{pre});
    
    $out .= "Post-add: ".($mp->{post})."\n"
      if(defined $mp->{post});
    
    $out .= "Forward matrix:".($mp->{matrix})
      if(defined $mp->{matrix});
    
    $out .= "Inverse matrix:".($mp->{inverse})
      if(defined $mp->{inverse});
  } else {
    $out .= "Pre-add: ".(-$mp->{post})."\n"
      if(defined $mp->{post});

    $out .= "Post-add: ".(-$mp->{pre})."\n"
      if(defined $mp->{pre});
    
    $out .= "Forward matrix:".($mp->{inverse})
      if(defined $mp->{inverse});
    
    $out .= "Inverse matrix:".($mp->{matrix})
      if(defined $mp->{matrix});
  }
    
  $out =~ s/\n/\n  /go;
  $out;
}


######################################################################
########## Convenience interfaces to Linear...

=head2 t_scale

=for usage 

  $f = t_scale(<scale>)

=for ref

Convenience interface to L<t_linear|t_linear>.

t_scale produces a tranform that scales around the origin by a fixed
amount.  It acts exactly the same as L<t_linear(Scale=><scale>)|t_linear>.

=cut

sub t_scale { new PDL::Transform::Linear(Scale=>$_[0]); }


##########
##########

=head2 t_offset 

=for usage

  $f = t_offset(<shift>)

=for ref

Convenience interface to L<t_linear|t_linear>.

t_offset produces a transform that shifts the origin to a new location.
It acts exactly the same as L<t_linear(Pre=><shift>)|t_linear>.

=cut

sub t_offset {new PDL::Transform::Linear(Pre=>$_[0]);}

##########
##########

=head2 t_rot

=for usage

  $f = t_rot(<rotation-in-degrees>)

=for ref

Convenience interface to L<t_linear|t_linear>.

t_rot produces a rotation transform in 2-D (scalar), 3-D (3-vector), or
N-D (matrix).  It acts exactly the same as L<t_linear(Rot=><shift>)|t_linear>.

=cut

sub t_rot {new PDL::Transform::Linear(Rot=>$_[0]);}


######################################################################

=head2 t_fits

=for usage

  $f = t_fits($fits);
  $f = new PDL::Transform::FITS;

=for ref 

FITS pixel-to-scientific transformation with inverse

You feed in a hash ref or a PDL with one of those as a header, and you
get back a transform that converts 0-originated, pixel-centered
coordinates into scientific coordinates via the transformation in the
FITS header.  For most FITS headers, the transform is reversible, so
applying the inverse goes the other way.  This is just a convenience
subclass of PDL::Transform::Linear.

For now, this transform is rather limited -- it really ought to 
accept units differences and stuff like that, but they are just
ignored for now.  Probably that would require putting units into
the whole transform framework.  

This transform implements the linear transform part of the WCS FITS
standard outlined in Greisen & Calabata 2002 (A&A in press; find it at
"http://arxiv.org/abs/astro-ph/0207407").

=cut

@PDL::Transform::FITS::ISA = ('PDL::Transform::Linear');

sub t_fits { new PDL::Transform::FITS(@_); }

sub PDL::Transform::FITS::new {
  my($class) = shift;
  my($hdr) = shift;

  $hdr = $hdr->gethdr  
    if(defined $hdr && UNIVERSAL::isa($hdr,'PDL'));

  croak('PDL::Transform::FITS::new requires a FITS header hash\n')
    if(!defined $hdr || ref $hdr ne 'HASH' || !defined($hdr->{NAXIS}));
  
  my($n) = $hdr->{NAXIS}; $n = $n->at(0) if(UNIVERSAL::isa($n,'PDL'));

  my($matrix) = PDL->zeroes($hdr->{NAXIS});
  my($pre) = PDL->zeroes($n);
  my($post) = PDL->zeroes($n);

  ##############################
  # Scaling: Use CDi_j formalism if present (mostly in Hubble
  # datasets); otherwise use CPi_j + CDELTi formalism.  

  my(@hgrab);

  if(@hgrab = grep(m/^CD\d{1,3}_\d{1,3}/,keys %$hdr)) {   # assignment

    #
    # CDi_j formalism
    #
    for my $h(@hgrab) {
      $h =~ m/CD(\d{1,3})_(\d{1,3})/;  # Should always match
      $matrix->slice("($1),($2)") .= $hdr->{h};
    }
    print "PDL::Transform::FITS: Detected CDi_j matrix: \n",$matrix,"\n"
      if($PDL::debug);
  
  } else {
    
    #
    # CPi_j + CDELTi formalism
    # If CPi_j arent present, and N=2, then try using CROTA or
    # CROTA1 to generate a rotation matrix instea.  
    #
  
    my($cdm) = PDL->zeroes($n,$n);
    my($cd) = $cdm->diagonal(0,1);

    my($cpm) = PDL->zeroes($n,$n);
    $cpm->diagonal(0,1) .= 1;     # CP: diagonal defaults to unity
    $cd .= 1;


    if( @hgrab = grep(m/^CP\d{1,3}_\d{1,3}/,keys %$hdr) ) {  # assignment

      for my $h(@hgrab) {
	$h =~ m/CP(\d{1,3})_(\d{1,3})/;  # Should always match
	$cpm->slice("($1),($2)") .= $hdr->{h};
      }
      print "PDL::Transform::FITS: Detected CPi_j matrix: \n",$cpm,"\n"
	if($PDL::debug && @hgrab);

    } elsif($n==2 && ( defined $hdr->{CROTA} || defined $hdr->{CROTA1} ) ) {

      my $cr = $hdr->{CROTA};
      $cr = $hdr->{CROTA1} unless defined $cr;

      $cr *= $DEG2RAD;
      $cpm .= pdl( [cos($cr), sin($cr)],[-sin($cr),cos($cr)] );

    }
      
    for my $i(1..$n) {
      $cd->slice("(".($i-1).")") .= $hdr->{"CDELT$i"};
    }
    print "PDL::Transform::FITS:  CDELT diagonal is $cd\n"
      if($PDL::debug);
    
    $matrix = $cdm x $cpm
  }

  my($i1) = 0;
  for my $i(1..$n) {
    $pre->slice("(".$i1.")") .= 1 - $hdr->{"CRPIX$i"};
    $post->slice("(".$i1.")").= $hdr->{"CRVAL$i"};
    $i1++;
  }

  my($me) = PDL::Transform::Linear::new($class,
					{'pre'=>$pre,
					 'post'=>$post,
					 'matrix'=>$matrix
					 });
  $me->{name} = 'FITS';
  return $me;
}

  


######################################################################

=head2 t_code 

=for usage

  $f = t_code(<func>,[<inv>],[options]);
  $f = new PDL::Transform::Code(<func>,[<inv>],[options]);

=for ref

Transform implementing arbitrary perl code.  

This is a way of getting quick-and-dirty new transforms.  You pass in
anonymous (or otherwise) code refs pointing to subroutines that
implement the forward and, optionally, inverse transforms.  The
subroutines should accept a data PDL followed by a parameter hash ref,
and return the transformed data PDL.  The parameter hash ref can be
set via the options, if you want to.

Options that are accepted are:

=over 3

=item p,params

The parameter hash that will be passed back to your code (defaults to the
empty hash).

=item j, jac, jacobian, Jacobian

A code ref that returns the Jacobian of the transform (optional; it'll
be calculated by multiple calls to func and/or by inverting ij if you
need it and don't specify it).

=item ij, j2, inv_jac, inv_jacobian, inverse_jacobian, Inverse_Jacobian

A code ref that returns the Jacobian of the inverse transform
(optional; it'll be calculated by multiple calls to inv and/or by
inverting j2 if you need it and don't specify it).

=item n,name

The name of the transform (defaults to "code").

=back

The code variables are executable perl code, either as a code ref or
as a string that will be eval'ed to produce code refs.  If you pass in
a string, it gets eval'ed at call time to get a code ref.  If it compiles
OK but does not return a code ref, then it gets re-evaluated with "sub {
... }" wrapped around it, to get a code ref.

Note that code callbacks like this can be used to do really weird
things and generate equally weird results -- caveat scriptor!

=cut

@PDL::Transform::Code::ISA = ('PDL::Transform');

sub t_code {new PDL::Transform::Code(@_);}

sub PDL::Transform::Code {
  my($class, $func, $inv, $o) = @_;
  if(ref $inv eq 'HASH') {
    $o = $inv;
    $inv = undef;
  }

  my($me) = PDL::Transform::new($class);
  $me->{name} = _opt($o,['n','name','Name']) || "code";
  $me->{func} = $func;
  $me->{inv} = $inv;
  $me->{params} = _opt($o,['p','params','Params']) || {};
  $me->{jacobian} = _opt($o,['j','jac','jacobian','Jacobian']);
  $me->{inv_jacobian}= _opt($o,['ij','j2','inv_jac','inv_jacobian','inverse_jacobian','Inverse_Jacobian']);
  $me;
}

######################################################################

=head2 t_cylindrical

=head2 t_radial

=for usage

  $f = t_radial(<options>);
  $f = new PDL::Transform::Radial(<options>);

=for ref

Convert Cartesian to radial/cylindrical coordinates.  (2-D/3-D; with inverse)

Converts 2-D Cartesian to radial (theta,r) coordinates.  You can choose
direct or conformal conversion.  Direct conversion preserves radial
distance from the origin; conformal conversion preserves local angles,
so that each small-enough part of the image only appears to be scaled
and rotated, not stretched.  Conformal conversion puts the radius on a
logarithmic scale, so that scaling of the original image plane is
equivalent to a simple offset of the transformed image plane.

If you use three or more dimensions, the higher dimensions are ignored,
yielding a conversion from Cartesian to cylindrical coordinates, which
is why there are two aliases for the same transform.  If you use higher
dimensionality than 2, you must manually specify the origin or you will 
get dimension mismatch errors when you apply the transform.

Theta runs B<clockwise> instead of the more usual counterclockwise; that is
to preserve the mirror sense of small structures.

OPTIONS:

=over 3

=item d, direct, Direct 

Generate (theta,r) coordinates out (this is the default); incompatible
with Conformal.  Theta is in radians, and the radial coordinate is 
in the units of distance in the input plane.

=item r0, c, conformal, Conformal

If defined, this floating-point value causes t_radial to generate
(theta, ln(r/r0)) coordinates out.  Theta is in radians, and the
radial coordinate varies by 1 for each e-folding of the r0-scaled
distance from the input origin.  The logarithmic scaling is useful for
viewing both large and small things at the same time, and for keeping 
shapes of small things preserved in the image.

=item o, origin, Origin

This is the origin of the expansion.  Pass in a PDL or an array ref.

=back

EXAMPLES

These examples do transformations back into the same size image as they
started from; by suitable use of the "transform" option to 
L<unmap|unmap> you can send them to any size array you like.

Examine radial structure in M51:
Here, we scale the output to stretch 2*pi radians out to the
full image width in the horizontal direction, and to stretch 1 radius out
to a diameter in the vertical direction.

  $a = rfits('m51.fits');
  $ts = t_linear(s => [250/2.0/3.14159, 2]); # Scale to fill orig. image
  $tu = t_radial(o => [130,130]);            # Expand around galactic core
  $b = $ts->compose($tu)->unmap($a);            

Examine radial structure in M51 (conformal):
Here, we scale the output to stretch 2*pi radians out to the full image width
in the horizontal direction, and scale the vertical direction by the exact
same amount to preserve conformality of the operation.  Notice that 
each piece of the image looks "natural" -- only scaled and not stretched.

  $a = rfits('m51.fits')
  $ts = t_linear(s=> 250/2.0/3.14159);    # Note scalar (heh) scale.
  $tu = t_radial(o=> [130,130], r0=>5); # 5 pix. radius -> bottom of image
  $b = $ts->compose($tu)->unmap($a);


=cut

@PDL::Transform::Radial::ISA = ('PDL::Transform');

sub t_radial { new PDL::Transform::Radial(@_); }
sub t_cylindrical { new PDL::Transform::Radial(@_); }

sub PDL::Transform::Radial::new {
  my($class) = shift;
  my($o) = $_[0];
  if(ref $o ne 'HASH') {
    $o = { @_ };
  }

  my($me) = PDL::Transform::new($class);

  $me->{params}->{origin} = _opt($o,['o','origin','Origin']);
  $me->{params}->{origin} = pdl(0,0) 
    unless defined($me->{params}->{origin});
  $me->{params}->{origin} = PDL->pdl($me->{params}->{origin});
  
  
  $me->{params}->{r0} = _opt($o,['r0','R0','c','conformal','Conformal']);
  $me->{params}->{origin} = PDL->pdl($me->{params}->{origin});
  
  $me->{name} = "radial (direct)";
  
  $me->{func} = sub {

      my($data,$o) = @_;

      my($out) = ($data->is_inplace) ? $data : zeroes($data);

      my($d) = $data->copy - $o->{origin};


      my($d0) = $d->slice("(0)");
      my($d1) = $d->slice("(1)");

      # (mod operator on atan2 puts everything in the interval [0,2*PI).)
      $out->slice("(0)") .= atan2(-$d1,$d0) % (2*$PI);

      $out->slice("(1)") .= (defined $o->{r0}) ?
	      0.5 * log( ($d1*$d1 + $d0 * $d0) / ($o->{r0} * $o->{r0}) ) :
	      sqrt($d1*$d1 + $d0*$d0);
      
      $out;
  };

  $me->{inv} = sub {

    my($d,$o) = @_;
    my($d0,$d1,$out)=
	( ($d->is_inplace) ?
	  ($d->slice("(0)")->copy, $d->slice("(1)")->copy->dummy(0,2), $d) :
	  ($d->slice("(0)"),       $d->slice("(1)")->dummy(0,2),       $d->copy)
	  );
      
    my($os) = $out->slice("0:1");
    $os .= append(cos($d0)->dummy(0,1),-sin($d0)->dummy(0,1));
    $os *= defined $o->{r0}  ?  ($o->{r0} * exp($d1))  :  $d1;
    $os += $o->{origin};

    $out;
  };
  
  
  $me;
}

######################################################################

=head2 t_pincushion

=for usage

    $t = t_pincushion(<options>);
  
=for ref

Azimuthally symmetric quadratic (pincushion) scaling (2-d; with inverse)

Radial distortion is characteristic of a nonflat focal plane in a
symmetric paraxial optical system.  The first distortion term is the
quadratic (pincushion) term.  C<t_pincushion> uses
L<t_radial|t_radial> and L<t_quadratic|t_quadratic> to achieve
azimuthally symmetric pincushion distortion.  The options are the same
as for L<t_quadratic|t_quadratic>, but only scalars are allowed.

=cut





######################################################################

=head2 t_quadratic

=for usage

  $t = t_square(<options>);
  $t = new PDL::Transform::Pincushion(<options>);

=for ref

Quadratic scaling -- cylindrical pincushion (n-d; with inverse)

Quadratic scaling emulates pincushion in a cylindrical optical system:
separate quadratic scaling is applied to each axis.  You can apply
separate distortion along any of the principal axes.  If you want
different axes, use L<wrap|wrap> and L<t_linear|t_linear> to rotate
them to the correct angle.  The scaling options may be scalars or
vectors; if they are scalars then the expansion is isotropic.

The formula for the expansion is: 

    f(a) = ( <a> + <strength> * a^2/<L_0> ) / (abs(<strength>) + 1)

where <strength> is a scaling coefficient and <L_0> is a fundamental
length scale.   Negative values of <strength> result in a pincushion 
contraction.

OPTIONS

=over 3

=item o,origin,Origin

The origin of the pincushion.

=item l,l0,length,Length,r0

The fundamental scale of the transformation -- the radius that remains
unchanged.

=item s,str,strength,Strength

The relative strength of the pincushion.

=back

=cut

@PDL::Transform::Pincushion::ISA = ('PDL::Transform');

sub t_pincushion { new PDL::Transform::Pincushion(@_); }
sub PDL::Transform::Pincushion::new {
    my($class) = shift;
    my($o) = $_[0];
    if(ref $o ne 'HASH') {
	$o = {@_};
    }
    my($me) = PDL::Transform::new($class);
    
    $me->{params}->{origin} = _opt($o,['o','origin','Origin'],pdl(0,0));
    $me->{params}->{l0} = _opt($o,['r0','l','l0','length','Length'],pdl(1));
    $me->{params}->{str} = _opt($o,['s','str','strength','Strength'],pdl(0.1));

    $me->{name} = "pincushion";
    
    $me->{func} = sub {
	my($data,$o) = @_;
	my($d) = $data->copy - $me->{params}->{origin};
	$d += $me->{params}->{str} * ($d * abs($d)) / $me->{params}->{l0};
	$d /= (abs($me->{params}->{str}) + 1);
	$d += $me->{params}->{origin};
	if($data->is_inplace) {
	    $data .= $d;
	    return $data;
	}
	$d;
    };
    
    $me->{inv} = sub {
	my($data,$o) = @_;
	my($d) = $data->copy ;
	my($o) = $me->{params}->{origin};
	my($s) = $me->{params}->{str};
	my($l) = $me->{params}->{l0};

	$d .= ((-1 + sqrt(1 + 4 * $s/$l * abs($data-$o) * (1+abs($s))))
	    / 2 / $s * $l) * (1 - 2*($data < $o));
	$d += $me->{params}->{origin};
	if($data->is_inplace) {
	    $data .= $d;
	    return $data;
	}
	$d;
    };
    $me;
}


######################################################################

=head2 t_spherical

=for usage

    $t = t_spherical(<options>);
    $f = new PDL::Transform::Spherical(<options>);

=for ref

Convert Cartesian to spherical coordinates.  (3-D; with inverse)

Convert 3-D Cartesian to spherical (theta, phi, r) coordinates.  Theta
is longitude, centered on 0, and phi is latitude, also centered on 0.
Unless you specify Euler angles, the pole points in the +Z direction
and the prime meridian is in the +X direction.  The default is for
theta and phi to be in radians; you can select degrees if you want
them.

Just as the L<t_radial|Radial> 2-D transform acts like a 3-D
cylindrical transform by ignoring third and higher dimensions,
Spherical acts like a hypercylindrical transform in four (or higher)
dimensions.  Also as with L<t_radial|Radial>, you must manually specify
the origin if you want to use more dimensions than 3.

OPTIONS:

=over 3

=item o, origin, Origin [default (0,0,0)]

This is the Cartesian origin of the spherical expansion.  Pass in a PDL
or an array ref.  

=item e, euler, Euler [default (0,0,0)]

This is a 3-vector containing Euler angles to change the angle of the
pole and ordinate.  The first two numbers are the (theta, phi) angles
of the pole in a (+Z,+X) spherical expansion, and the last is the
angle that the new prime meridian makes with the meridian of a simply
tilted sphere.  This is implemented by composing the output transform
with a PDL::Transform::Linear object.

=item u, unit, Unit (default radians)

This option sets the angular unit to be used.  Acceptable values are
"degrees","radians", or reasonable substrings thereof (e.g. "deg", and
"rad", but "d" and "r" are deprecated).  Once genuine unit processing
comes online (a la Math::Units) any angular unit should be OK.

=back

sub t_spherical { new PDL::Transform::Spherical(@_); }
@PDL::Transform::Spherical::ISA = ('PDL::Transform');

sub PDL::Transform::Spherical::new {
    my($class) = shift;
    my($o) = $_[0];
    if(ref $o ne 'HASH') {
	$o = { @_ } ;
    }

    my($me) = PDL::Transform::new($class);

    $me->{params}->{origin} = _opt($o,['o','origin','Origin']);
    $me->{params}->{origin} = PDL->zeroes(3)
	unless defined($me->{params}->{origin});
    $me->{params}->{origin} = PDL->pdl($me->{params}->{origin});
    
    $me->{params}->{deg} = _opt($o,['d','degrees','Degrees']);
    
    my $unit = _opt($o,['u','unit','Unit']);
    $me->{params}->{angunit} = ($unit =~ m/^d/i) ? 
	$DEG2RAD : undef;
    
    $me->{name} = "spherical";
    
    $me->{func} = sub {
	my($data,$o) = @_;
	my($d) = $data->copy - $o->{origin};

	my($d0,$d1,$d2) = ($d->slice("(0)"),$d->slice("(1)"),$d->slice("(2)"));
	my($out) =   ($d->is_inplace) ? $data ? $data->copy;

	($out->slice("(0)"), $out->slice("(1)"), $out->slice("(2)"));

	$out->slice("(0)") .= sqrt($d0*$d0 + $d1*$d1 + $d2*$d2);
	$out->slice("(1)") .= asin($d2 / $r);
	$out->slice("(2)") .= atan2($d1, -$d0);


	$out->slice("1:2") *= $o->{angunit}
	  if(defined $o->{angunit});
	
	$out;
    }

    $me->{inv} = sub {
	my($d,$o) = @_;
	
	my($theta,$phi,$r,$out) = 
	    ( ($d->is_inplace) ? 
	      ($d->slice("(0)")->copy,$d->slice("(1)")->copy,$d->slice("(2)")->copy, $d) :
	      ($d->slice("(0)"), $d->slice("(1)"), $d->slice("(2)"), $d->copy)
	      );

	
	my($x,$y,$z) = 
	    ($out->slice("(0)"),$out->slice("(1)"),$out->slice("(2)"));

	if(defined $o->{angunit}){
	    $z .= $r * sin($phi / $o-
	$z .= $r * sin($phi);
	$x .= $r * cos($phi);
	$y .= $x * sin($theta);
	$x *= cos($theta);

	$out += $o->{origin};

	$out;
    }

    $me;
}

=head1 AUTHOR

Copyright 2002 Craig DeForest.  This module may be modified and
distributed under the same terms as PDL itself.

=cut

  1;
