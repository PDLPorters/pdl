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
resampled image.  In keeping with standard practice, but somewhat
counterintuitively, the transform is used to map coordinates FROM the
destination dataspace (or image plane) TO the source dataspace; thus,
to convert a solar image from perspective to heliospheric lat/lon
coordinates, you need to apply a deprojection transform.

You can define and compose several transformations, then apply them
all at once to an image.  The image is interpolated only once, when
all the composed transformations are applied.

=head1 EXAMPLE

Coordinate transformations and mappings are a little counterintuitive
at first, because image mapping is usually done in the reverse
direction (output plane coordinates -> map -> input plane coordinates
to be interpolated).  Here are some examples of transforms in 
action:

   use PDL::Transform;
   $a = rfits('m51.fits');  # Substitute path if necessary!
   $tf = new PDL::Transform::FITS($a);           # FITS pixel transform
   $ts = new PDL::Transform::Linear({Scale=>3}); # Scaling transform

   $w = pgwin(xs);
   $w->imag($a);

   ## Shrink m51 by a factor of 3; origin is at lower left.
   $b = $ts->map($a); 
   $w->imag($b);

   ## Grow m51 by a factor of 3; origin still at lower left.
   $c = $ts->unmap($a);
   $w->imag($c);

   ## Move the transform into FITS scientific space;
   ## maybe a convenience method is needed for this.
   $t = $tf->inverse -> compose($ts) -> compose($tf);

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
embody the transformation.

=head1 NOTES

Transforms currently have no mechanism for labeling the units or type
of each coordinate at each stage.  That is probably a necessary addition.
Currently, it just assumes that the coordinates are correct for (e.g.)
FITS scientific-to-pixel transformations.

Composition works OK but should be done in a more sophisticated way
so that, e.g., linear transformations can be combined at the matrix level
instead of just strung together pixel-to-pixel.

Linear transformations should be handled with heterogeneous Hershey matrices,
rather than matrix-multiply-and-add operations.  

Nonlinear transformations are coming soon.

=head2 HISTORY

=over 3

=item * 0.5 - initial upload to CVS

=item * 0.6 - Converted to use PDL::MatrixOps instead of PDL::Slatec

=back


=head1 METHODS

=cut

use PDL::MatrixOps;

package PDL::Transform;
use overload '""' => \&_strval;


$VERSION = "0.6";

use strict;
use Carp;
use PDL;
use PDL::MatrixOps;

eval { use PDL::Slatec };

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

=head2 apply

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

=head2 invert

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

=head2 inverse

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

=head2 compose

=for usage

  $f_o_g = $f->compose($g);

=for ref 

Function composition: f(g(x))

This is accomplished by inserting a splicing code ref into the C<func> 
and C<inv> slots.  It will be replaced with something slightly more 
sophisticated, eventually -- this one certainly works OK.

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

sub compose {
  my($f) = shift;
  my($g) = shift;

  Carp::croak("compose: undefined f or g\n") unless(defined $f and defined $g);
  
  my($me) = PDL::Transform->new;
  $me->{name} = $f->{name} . " o " . $g->{name};

  my(@clist);
  if(UNIVERSAL::isa($f,"PDL::Transform::Composition")) {
    push(@clist,@{$f->{params}->{clist}});
  } else {
    push(@clist,$f);
  }

  if(UNIVERSAL::isa($g,"PDL::Transform::Composition")) {
    push(@clist,@{$g->{params}->{clist}});
  } else {
    push(@clist,$g);
  }
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

=head2 wrap

=for usage

  $g1fg = $f->wrap($g);

=for ref

Shift a transform into a different space by 'wrapping' it with a second.

This is just a convenience function for two L<compose|compose> calls.
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
  
  return $g->inverse->compose($f)->compose($g);
}

######################################################################

=head2 map

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

If $input and/or $template have FITS headers, then by default they are
applied to the transformation, so that $t applies to the scientific
coordinates and not to pixel coordinates.  You can override that behavior,
forcing pixel coordinates, with the C<nofits> option.

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

Some options that are unused by L<interpND|interpND> are used here: "nofits" is
a flag that will prevent interpretation of FITS headers in the template
input image.

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

  my($opt);

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

  # Generate index arrays
  print "PDL::Transform:map: Generating index; dim list is ",join(",",($out->ndims,$out->dims)),"\n"
    if($PDL::debug);

  my $indices = PDL->zeroes($out->ndims,$out->dims); for my
  $dim(1..$out->ndims) { my($sl) =
  $indices->slice("(".($dim-1).")")->xchg(0,$dim-1); $sl .=
  PDL->xvals($sl->dims); }

  print "PDL::Transform::map: calling apply..." if($PDL::debug);
  $indices = $me->apply($indices->inplace);
  
  print "PDL::Transform::map: transformed indices are ",join(",",$indices->dims),"\n" 
    if($PDL::debug);

  $out .= $in->interpND($indices,$opt);
}
  
  
######################################################################

=head2 unmap

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

=head2 new PDL::Transform

=for usage

 my $xform = new PDL::Transform

=for ref

Generic constructor generates the identity transform.

This constructor really is trivial -- it's mainly used by the other transform 
constructors.  It takes no parameters and returns the identity transform.

=cut

sub _identity { return shift; }

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

Options are:

=over 3

=item Scale, scale, s

(default 1.0) Specifies the linear amount of scaling to be done before 
lookup.  You can feed in a scalar or an N-vector; other values may cause
trouble.

=item Offset, offset, o

(default 0.0) Specifies the linear amount of offset before lookup.  
This is only a scalar, because it's intended to let you switch to 
corner-centered coordinates if you want to (just feed in o=-0.25).

=item Boundary, boundary, bound, b

Boundary condition to be fed to L<interpND|interpND>

=item Method, method, m

Interpolation method to be fed to L<interpND|interpND>

=back

=cut
@PDL::Transform::Lookup::ISA = ('PDL::Transform');

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

The options you can pass in are:

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
      print "PDL::Transform::Linear: assuming 2-D transform (set dims option to change)\n";
      $me->{idim} = $me->{odim} = 2;
    }
    
    $me->{params}->{matrix} = PDL->zeroes($me->{idim},$me->{odim});
    $me->{params}->{matrix}->diagonal(0,1) .= 1;
  }

  ### Handle rotation option 
  my $rot = $me->{params}->{rot};
  if(defined($rot)) {
    # Subrotation closure -- rotates from axis $d->(0) --> $d->(1).
    my $subrot = sub { my($d,$angle,$m)=@_;
		       my($subm) = $m->dice($d,$d);
		       my($a) = $angle*3.1415926535897932384626/180;
		       $subm .= matmult(pdl([cos($a),sin($a)],[-sin($a),cos($a)]),$subm);
		       print "\n";
		     };
    
    if(UNIVERSAL::isa($rot,'PDL') && $rot->nelem > 1) {
      if($rot->ndims == 2) {
	$me->{params}->{matrix} .= matmult($rot,$me->{params}->{matrix});
      } elsif($rot->nelem == 3) {
	my $rm = zeroes(3,3);
	$rm->diagonal(0,1)++;
	
	&$subrot(pdl(0,1),$rot->at(2),$rm);
	&$subrot(pdl(2,0),$rot->at(1),$rm);
	&$subrot(pdl(1,2),$rot->at(0),$rm);
	$me->{params}->{matrix} .= matmult($rm,$me->{params}->{matrix});
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

    my($outmat) = $a->matmult($opt->{matrix});
    return $outmat+$_[1]->{post};
  };
  
  $me->{inv} = (defined $me->{params}->{inverse}) ? sub {
    my($in,$opt) = @_;
    my($a) = $in - $opt->{post};

    my($outmat) = $a->matmult($opt->{inverse});
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
    
    $matrix = PDL::Slatec::matmult($cdm,$cpm);
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

The parameteter hash that will be passed back to your code (defaults to the
empty hash).

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

=head1 AUTHOR

Copyright 2002 Craig DeForest.  This module may be modified and
distributed under the same terms as PDL itself.

=cut

  1;
