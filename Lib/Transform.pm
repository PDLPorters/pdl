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
resampled image.   Eventually, the resampled image will have an updated
FITS header that corresponds to what coordinates it's in.

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
key can be left undefined in cases where the inverse doesn't exist.

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

Bit indicating whether the transform has been inverted.  That's useful
for some stringifications (see the PDL::Transform::Linear
stringifier), and may be useful for other things.

=item jacobian

Ref to a method that returns the jacobian matrix of the transformation --
this is the partial derivative of each output coordinate with respect to 
each input coordinate.

=item inv_jacobian

Ref to a method that returns the inverse jacobian matrix of the 
transformation -- this is the partial derivative of each input 
coordinate with respect to each output coordinate.

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

Until full Jacobean tracking is in place, transforms that shrink the 
image (or a part of it -- see L<t_radial|t_radial>) will lose information
because they sample the input image sparsely.

=head1 METHODS

=cut
use PDL::MatrixOps;

package PDL::Transform;
use overload '""' => \&_strval;

$VERSION = "0.7";

BEGIN {
   use Exporter ();
   @ISA = ( Exporter );
   @EXPORT_OK = qw( t_identity t_lookup t_linear t_fits t_radial t_code t_inverse t_compose);
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
    return $hash->{$_}
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

If the inverse doesn't exist, return undef.

=cut

sub invert {
  my($me) = shift;

  return undef unless defined($me->{inv});
  return &{$me->{inv}}(shift, $me->{params});
}
  

######################################################################

=head2 t_inverse 

=head2 PDL::Transform::inverse

=for usage 

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

=head2 PDL::Transform::compose

=for usage

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
    for my $t ( reverse(@{$p->{clist}}) ) {
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

=head2 PDL::Transform::wrap

=for usage

  $g1fg = $f->wrap($g);

=for ref

Shift a transform into a different space by 'wrapping' it with a second.

This is just a convenience function for two L<compose|Transform::compose> calls.
It's useful to make a single transformation happen in some other space.
For example, to shift the origin of rotation, do this:

  $im = rfits('m51.fits');
  $tf = new PDL::Transform::FITS($im);
  $tr = new PDL::Transform::Linear({rot=>30});
  $im1 = $tr->unmap($tr);               # Rotate around pixel origin
  $im2 = $tr->unmap($tr->wrap($tf));    # Rotate round FITS scientific origin

=cut

sub wrap {
  my($f) = shift;
  my($g) = shift;
  
  return $g->inverse->compose($f,$g);
}

######################################################################

=head2 PDL::Transform::map

=for sig

 Signature: (PDL::Transform a; data(); template(); \%opt)

=for usage

  $output = $t->map($input,[<template>],[<options>]);

=for ref

Map an image or N-D dataset using the transform as a coordinate transform.

The transform is applied to the coordinates of $output to obtain 
coordinates for interpolation from the $input array.  NOTE that this is
somewhat counterintuitive at first; but it's the correct way to do 
image distortion.

The output has the same data type as the input.  This is a feature,
but it can lead to `strange' banding behaviors if you use interpolation
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

The options hash is sent directly to L<interpND|interpND>, so you can
change the interpolation behavior (currently bilinear by default).  In
particular, setting "method=>sample" will speed up the operation by a factor
of 2-3.  

Some options that are unused by L<interpND|interpND> are used here: 

=over 3

=item nofits

(boolean): prevent interpretation of FITS headers in the template
input image. (default = false)

=item i, int, integrate

If set, and if the data set is 2-D (an image), then do actual
integration of each output pixels's mapped area on the input plane.
This is pretty slow compared to sampling, but is much more accurate.

=back

NOTES:

Map currently does simple interpolation only, so in places where
incrementing an output pixel coordinate sweeps across several input
pixels, some input pixels are ignored.  (For example, reducing a
periodic image with high spatial frequencies gives strong Moire
effects).  A future version should probably incorporate some sort of
footprint mapping so that the output pixel is an appropriate average
when it maps to more than one input pixel.

Currently FITS headers are detected but not acted upon.  You must 
handle your FITS transformations manually.

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

  my($integrate) = _opt($opt,['i','int','integrate','Integrate']);
  print "integrate=$integrate\n";

  ## Half-width of the sampling gaussian -- should be at least 0.5.
  my($blur) = _opt($opt,['b','blur','Blur']);
  $blur = 0.7 unless($blur);
  my $blur2 = 0.5 / $blur / $blur ; 

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
    ### Enumerate & warp the coordinates of all pixel corners.
    print "enumerating indices...\n";
    my($indices) = PDL->zeroes($nd, (PDL->pdl(@sizes)+1)->list );#(coord,dims)
    for my $dim(0..$out->ndims-1) {
      my($sl) = $indices->slice("(".($dim).")")->xchg(0,$dim);
      $sl .= PDL->xvals($sl->dims)-0.5;
    }

    $indices = $me->apply($indices->inplace); #(dim,list)

    ###############
    ### Generate the pixel-center map and flatten it into a single list of 
    ### N-vectors for easier iteration.  List dim first, other stuff last.
    ### This is a little time consuming but probably quicker than calling the 
    ### whole transformation again on the pixel centers.  (the corners are
    ### needed for the jacobian, below)

    print "flattening maps...\n";

    my ($slstr1,$slstr2);
    chop ( $slstr1 = ":,".("0:-2," x $nd) );
    chop ( $slstr2 = ":,".("1:-1," x $nd) );
		   
    my $center = (pdl(0.5) * 
		  ( ($indices->slice($slstr1) + $indices->slice($slstr2))->
		    reorder(1..$nd,0)->  # (dims, coord)
		    clump($nd)           # (list, coord)
		    )
		  );  # center gets (list,coord)

    ###############
    ### Calculate the jacobian everywhere: nxn matrix with trailing index dims
    ### for each pixel. 
    print "enumerating jacobian...\n";
    my($jacob) = PDL->zeroes($nd, $nd, @sizes); # (col, row, dims)

    my($slstr) = ",0:-2"x($nd-1);
    for($i=0;$i<$nd;$i++){
      print "i=$i\n";
      my($ind) = $indices->xchg(1,$i+1);
      $jacob->slice("($i)") .= ( $ind->slice(":,1:-1".$slstr) - 
				 $ind->slice(":,0:-2".$slstr)
				 ) -> xchg(1,$i+1);
      print "ok\n";
    }
    
    print "flattening jacobian...\n";
    my $jdet = $jacob->determinant->flat; # original determinant
    my $jac  = $jacob->reorder(2..($nd+1),0,1)->clump($nd); # (list, col, row)

    ###############
    ### Figure out the footprint size of each outpus pixel, in 
    ### input coordinates.  This relies on the peculiar coincidence 
    ### (or whatever) that the eigenvalues of the squared Jacobian are 
    ### the squared lengths of the principal axes of the ellipsoid formed 
    ### by transforming the pixel.  We just grab a cube as big as the 
    ### longest axis.

    my ($ev, $e) = (eigens ($jac->reorder(2,1,0) x 
			    $jac->reorder(1,2,0)));
    $e = $e->inplace->abs; # (eig, list)

    # Figure out the size needed for each subfield.  Don't allow any
    # sizes larger than half the largest dimension of the source field.

    my $big = PDL->pdl($in->dims)->max * 0.1;

    my $sizes = $e->maximum->sqrt->clip(undef,$big);    # (list)


    ###############
    ### Pad the eigenvalues of the jacobian out to 1, and generate
    ### the inverse of the padded version.  We lose information about
    ### rotations (by symmetrizing in eigens) and reflection (by 
    ### taking the absolute value of the eigenvalue), but all of that 
    ### stuff takes place inside each pixel anyhow, so it's no big deal.
    ($ev, $e) = (eigens ($jac->reorder(1,2,0)));
    $e = $e->inplace->clip(1,undef); 
    my $ijac = $ev->xchg(0,1)->sever;   # (row, col, list)
    $ijac->mv(0,2) /= $e;               # safe since $e is padded to 1.
    $ijac x= $ev;                       # (col, row, list)

    ###############
    ### Loop over increasing size, doubling the size each time until
    ### the very largest pixels are handled.
    my $size;
    my $last_size=0;
    my $outf = $out->flat;
    for($size=4; $size < $big*1.9999; $size *= 2){
      print "size is $size...";
      $size = PDL->pdl($big)->floor + 1 
	if($size > $big);
      print "big is $big\n";
      my($pixels) = ($size < $big) ? 
	which($sizes >= $last_size/2.0 & $sizes < $size/2.0) :
	which($sizes >= $last_size/2.0);

      print "found ",$pixels->nelem," points...";
      next if($pixels->nelem == 0);

      ###############
      ### Enumerate a cube of coordinate offsets of the current size.
      ### float not double, to save space.
      print "enumerating coordinates for a $size-on-a-side $nd-cube...";
      my @zlist = (zeroes($nd)+$size)->list;
      my($coords_in) = zeroes( float, $nd, (zeroes($nd)+$size)->list );
      print "coords_in has dims ",$coords_in->dims,"\n";
      for my $i(0..$nd-1) {
	$coords_in->index($i)->xchg(0,$i) .= xvals(@zlist);
      }
      
      ###############
      ### Duplicate the coordinate cube for each pixel.  This step
      ### is a real memory hog -- if crashes occur here, then subdivide
      ### down to reasonable memory sizes with a for loop.
      print "duplicating ",$pixels->nelem," times...\n";
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
      print "offsetting...  center is ",join("x",$center->dims),"; pixels is ",join("x",$pixels->dims),"\n";
      my($points) = $center->indexND($pixels->dummy(0,1));  # (pix-list, coord)
      print " points has dimension ",join("x",$points->dims),"\n";
      $coords_in += $points->floor - 0.5*$size;       # (pix-list, coord, rgn-list)

      ###############
      ### Calculate the offsets (in output space) of each center from 
      ### the appropriate pixel center, and convert to r^2;
      ###
      ### The intermediate variable ijac_p could be eliminated...
      print "calculating offsets\n";
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

      print "calculating filter values\n";
      my $filt = $offsets->sumover->xchg(0,1);  # (rgn-list,pix-list) R^2
      $offsets = undef;    # free up memory    
      $filt *= (-$blur2);               # Gaussian has HW of 2/3
      $filt->inplace->exp;               # make Gaussian. (rgn-list, pix-list)
      my $f_norm = $filt->sumover;       # (pix-list)
      
      ###############
      ### Pedal to the metal -- accumulate input values and scale by their
      ### appropriate filter value.
      print "grabbing inputs...\n";
      my($input) = $in->range(
			      $coords_in->         #(pix-list,coord,rgn-list)
			        slice(":,:,(0)")-> #(pix-list,coord)
			        xchg(0,1)          #(coord,pix-list)
			      ,
			      $size,'e')->         # (pix-list,<dims>)
		      reorder(1..$nd,0)->          # (<dims>,pix-list)
		      clump($nd)                   # (rgn-list,pix-list)
		      ->sever;
      print "input is ",join("x",$input->dims),"\n";

      $PDL::debug::filterpanel->range($coords_in->
				      slice(":,:,(0)")->
				      xchg(0,1)
				      ,
				      $size,'e') += $filt
				      if(defined $PDL::debug::filterpanel);

      $filt *= $input;  # (rgn-list, pix-list)

      ### Stick the summed, filtered output back into the output array!
      ### Whew!  (Come back and scale here for photometric rather than
      ### surface area preservation).
      ### range is used here because indexND doesn't preserve dataflow
      ### (it should and probably will sooner or later).
      $out->flat->range($pixels->dummy(0,1)) .= 
	($filt->sumover / $f_norm);  # (pix-list)

      print "\n";
      $PDL::debug_w->imag($out) if(defined $PDL::debug_w);
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


sub PDL::Transform::string {
  print "Stringified a transform!\n";
}

######################################################################

=head1 CONSTRUCTORS

These routines just construct different types of transformation.  They're
all trivial subclasses of PDL::Transform, so you can say
   
  $a = new PDL::Transform::<type>(<params>)

but it would be a real pain to put each one in its own .pm file.

=cut

######################################################################

=head2 t_identity

=head2 new PDL::Transform

=for usage

 my $xform = new PDL::Transform

=for ref

Generic constructor generates the identity transform.

This constructor really is trivial -- it's mainly used by the other transform 
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

=head2 new PDL::Transform::Lookup

=for usage

  $f = new PDL::Transform::Lookup($lookup, { <options> });

=for ref

Transform by lookup into an explicit table.

You specify an N+1-D PDL that is interpreted as an N-D lookup table
of column vectors.  The last dimension has order equal to the output
dimensionality of the transform.

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

There is no inverse transform -- that's too hard and sometimes
impossible, so the inverse transform just croaks.

Options are listed below; there are several synonyms for each.

=over 3

=item s, scale, Scale

(default 1.0) Specifies the linear amount of scaling to be done before 
lookup.  You can feed in a scalar or an N-vector; other values may cause
trouble.

=item o, offset, Offset

(default 0.0) Specifies the linear amount of offset before lookup.  
This is only a scalar, because it's intended to let you switch to 
corner-centered coordinates if you want to (just feed in o=-0.25).

=item b, bound, boundary, Boundary

Boundary condition to be fed to L<interpND|interpND>

=item m, method, Method

Interpolation method to be fed to L<interpND|interpND>

=back

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

  $me->{func} = sub {
    my($data) = shift;
    if($data->dim(0) > $me->{idim}) {
      croak("Too many dims (".$data->dim(0).") for your table (".$me->{idim}.")\n");
    };

    my($p) = shift;
    
    $data = pdl($data) 
      unless ((ref $data) && (UNIVERSAL::isa($data,'PDL')));
    
    my($a)= ($p
	     ->{table}
	     ->interpND($data * $p->{scale} + $p->{offset},
			$p->{interpND_opt}
			)
	     );
    

    # Put the index dimension (and threaded indices) back at the front of
    # the dimension list.
    my($dnd) = $data->ndims - 1;
    return ($a -> ndims > $data->ndims - 1) ? 
      ($a->reorder( $dnd..($dnd + $p->{table}->ndims - $data->dim(0)-1)
		    , 0..$data->ndims-2
		    )
       ) : $a;
  };

  $me->{inv} = undef;

  $me->{name} = 'Lookup';

  return $me;
}				  

######################################################################

=head2 t_linear

=head2 new PDL::Transform::Linear
  
=for usage
  
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
it's treated as a diagonal matrix (for convenience).  It gets
left-multiplied with the transformation matrix you specify (or the
identity), so that if you specify both a scale and a matrix the
scaling is done after the rotation or skewing or whatever.

=item r, rot, rota, rotation, Rotation

A rotation angle in degrees -- useful for 2-D and 3-D data only.  If
you pass in a scalar, it specifies a rotation from the 0th axis toward
the 1st axis.  If you pass in a 3-vector, it's treated as a set of
Euler angles, and a rotation matrix is generated that does the following, in
order:

=over 3

=item * Rotate by rot->(2) degrees from 0th to 1st axis

=item * Rotate by rot->(1) degrees from the 2nd to the 0th axis

=item * Rotate by rot->(0) degrees from the 1st to the 2nd axis

The rotation matrix is 

=back

The rotation matrix is left-multiplied with the transformation matrix
you specify, so that if you specify both rotation and a general matrix
the rotation happens after the more general operation -- though that's
deprecated.

Of course, you can duplicate this functionality -- and get more
general -- by generating your own rotation matrix and feeding it in 
with the C<matrix> option.

=item m, matrix, Matrix

The transformation matrix.  It doesn't even have to be square, if you want
to change the dimensionality of your input.  If it's invertible (note: 
must be square for that), then you automagically get an inverse transform too.

=item pre, preoffset, offset, Offset

The vector to be added to the data before it gets multiplied by the matrix
(equivalent of CRVAL in FITS, if you are converting from scientific to 
pixel units).

=item post, postoffset, shift, Shift

The vector to be added to the data after it gets multiplied by the matrix
(equivalent of CRPIX-1 in FITS, if youre converting from scientific to pixel 
units).

=item dims, Dims

Most of the time it's obvious how many dimensions you want to deal
with: if you supply a matrix, it defines the transformation; if you
input offset vectors in the C<pre> and C<post> options, those define
the number of dimensions.  But if you only supply scalars, there's no way
to tell and the default number of dimensions is 2.  This provides a way 
to do, e.g., 3-D scaling: just set C<{s=><scale-factor>, dims=>3}> and
you're on your way.

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
  
  $me->{params}->{post} = _opt($o,['post','Post','postoffset','PostOffset',
				   'shift','Shift'],0);

  $me->{params}->{matrix} = _opt($o,['m','matrix','Matrix','mat','Mat']);

  $me->{params}->{rot} = _opt($o,['r','rot','rota','rotation','Rotation']);
  $me->{params}->{rot} = pdl(0) unless defined($me->{params}->{rot});

  print "me->{params}->{rot} = ".$me->{params}->{rot}."\n";

  my $o_dims = _opt($o,['dims','Dims']);
  my $scale  = _opt($o,['s','scale','Scale']);
  
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

		       my($a) = $angle*3.1415926535897932384626/180;
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

=head2 t_fits

=head2 new PDL::Transform::FITS

=for usage

  $f = new PDL::Transform::FITS ($fits)

=for ref 

FITS pixel-to-scientific transformation with inverse

You feed in a hash ref or a PDL with one of those as a header, and you
get back a transform that converts 0-originated, pixel-centered
coordinates into scientific coordinates via the transformation in the
FITS header.  For most FITS headers, the transform is reversible, so
applying the inverse goes the other way.  This is just a convenience
subclass of PDL::Transform::Linear.

For now, this transform is rather limited -- it really ought to 
accept units differences and stuff like that, but they're just
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

      $cr *= 3.14159265358979323846264338 / 180;
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

=head2 new PDL::Transform::Code 

=for usage

  $f = new PDL::Transform::Code(<func>,[<inv>],[options])

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
a string, it's eval'ed at call time to get a code ref.  If it compiles
OK but doesn't return a code ref, then it's re-evaluated with "sub {
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

=head2 t_radial 

=head2 new PDL::Transform::Radial

=for usage

  $f = new PDL::Transform::Radial(<options>);

=for ref

Convert to radial coordinates.  (2-D; with inverse)

Converts Cartesian to radial (theta,r) coordinates.  You can choose
direct or conformal conversion.  Direct conversion preserves radial
distance from the origin; conformal conversion preserves local angles,
so that each small-enough part of the image only appears to be scaled
and rotated, not stretched.  Conformal conversion puts the radius on a
logarithmic scale, so that scaling of the original image plane is
equivalent to a simple offset of the transformed image plane.

Theta runs B<clockwise> instead of the more usual counterclockwise; that's
to preserve the mirror sense of small structures.

OPTIONS:

=over 3

=item d, direct, Direct 

Generate (theta,r) coordinates out (this is the default); incompatible
with Conformal.  Theta is in radians, and the radial coordinate is 
in the units of distance in the input plane.

=item r0, c, conformal, Conformal

This is a floating-point value that causes t_radial to generate
(theta, ln(r/r0)) coordinates out.  Theta is in radians, and the
radial coordinate varies by 1 for each e-folding of the r0-scaled
distance from the input origin.  The logarithmic scaling is useful for
viewing both large and small things at the same time, and for keeping 
shapes of small things preserved in the image.

=item o, origin, Origin

This is the origin of the expansion.

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
  $ts = t_linear(s => pdl(250/2.0/3.14159, 2)); # Scale to fill orig. image
  $tu = t_radial(o => pdl(130,130));            # Expand around galactic core
  $b = $ts->compose($tu)->unmap($a);            

Examine radial structure in M51 (conformal):
Here, we scale the output to stretch 2*pi radians out to the full image width
in the horizontal direction, and scale the vertical direction by the exact
same amount to preserve conformality of the operation.  Notice that 
each piece of the image looks "natural" -- only scaled and not stretched.

  $a = rfits('m51.fits')
  $ts = t_linear(s=> 250/2.0/3.14159);    # Note scalar (heh) scale.
  $tu = t_radial(o=>pdl(130,130), r0=>5); # 5 pix. radius -> bottom of image
  $b = $ts->compose($tu)->unmap($a);


=cut

@PDL::Transform::Radial::ISA = ('PDL::Transform');

sub t_radial { new PDL::Transform::Radial(@_); }

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

  $me->{params}->{r0} = _opt($o,['r0','R0','c','conformal','Conformal']);

  $me->{name} = "radial (direct)";
  
  $me->{func} = sub {
    my($data,$o) = @_;
    my($d) = $data - $o->{origin};
    my($d0) = $d->index(0);
    my($d1) = $d->index(1);

    my $out = atan2($d1->dummy(0,1), - $d0->dummy(0,1));

    if(defined $o->{r0}) {
      return append($out,log( sqrt($d1*$d1+$d0*$d0)->dummy(0,1) / $o->{r0} ));
    }

    return   append($out,    sqrt($d1*$d1+$d0*$d0)->dummy(0,1) );
  };
  
  $me->{inv} = sub {
    my($data,$o) = @_;
    my($d0) = $data->index(0);
    my($d1) = $data->index(1);
    my $out;

    if(defined $o->{r0}) {
      $out = ($o->{r0} * exp($d1))->dummy(0,2) *
	     append( cos($d0)->dummy(0,1), -sin($d0)->dummy(0,1) );	     
    } else {
      $out = $d1->dummy(0,2) * 
	     append( cos($d0)->dummy(0,1), -sin($d0)->dummy(0,1) );
    }

    $out += $o->{origin};
  };
  
  
  $me;
}

=head1 AUTHOR

Copyright 2002 Craig DeForest.  This module may be modified and
distributed under the same terms as PDL itself.

=cut

  1;
