
=head1 NAME

PDL::Func - useful functions

=head1 SYNOPSIS

 use PDL::Func;
 use PDL::Math;

 # somewhat pointless way to estimate cos and sin,
 # but is shows that you can thread if you want to
 # (and the library lets you)
 #
 my $obj = PDL::Func->init( Interpolate => "Hermite" );
 # 
 my $x = pdl( 0 .. 45 ) * 4 * 3.14159 / 180;
 my $y = cat( sin($x), cos($x) );
 $obj->set( x => $x, y => $y, bc => "simple" );
 #
 my $xi = pdl( 0.5, 1.5, 2.5 );
 my $yi = $obj->interpolate( $xi );
 #
 print "sin( $xi ) equals ", $yi->slice(':,(0)'), "\n";
 sin( [0.5 1.5 2.5] ) equals  [0.87759844 0.070737667 -0.80115622]
 #
 print "cos( $xi ) equals ", $yi->slice(':,(1)'), "\n";
 cos( [0.5 1.5 2.5] ) equals  [ 0.4794191 0.99768655 0.59846449]
 #
 print sin($xi), "\n", cos($xi), "\n";
 [0.47942554 0.99749499 0.59847214]
 [0.87758256 0.070737202 -0.80114362]

=head1 DESCRIPTION

This module aims to contain useful functions. Honest.

=head1 INTERPOLATION AND MORE

This module aims to provide a relatively-uniform interface
to the various interpolation methods available to PDL.
The idea is that a different interpolation scheme
can be used just by changing an attribute of a C<PDL::Func>
object.
Some interpolation schemes (as exemplified by the SLATEC
library) also provide additional functionality, such as 
integration and gradient estimation.

Throughout this documentation, C<$x> and C<$y> refer to the function
to be interpolated whilst C<$xi> and C<$yi> are the interpolated values.

The avaliable types, or I<schemes>, of interpolation are listed below.
Also given are the valid attributes for each scheme: the flag value
indicates whether it can be set (s), got (g), and if it is
required (r) for the method to work.

=over 4

=item Interpolate => Linear

An extravagent way of calling the linear interpolation routine
L<PDL::Primitive::interpolate|PDL::Primitive/interpolate>.

The valid attributes are:

 Attribute    Flag  Description
 x            sgr   x positions of data
 y            sgr   function values at x positions
 err          g     error flag

=item Interpolate => Hermite

Use the piecewice cubic Hermite interpolation routines
from the SLATEC library.
Only available if L<PDL::Slatec|PDL::Slatec> is installed.

The valid attributes are:

 Attribute    Flag  Description
 x            sgr   x positions of data
 y            sgr   function values at x positions
 bc           sgr   boundary conditions
 g            g     estimated gradient at x positions
 err          g     error flag

Given the initial set of points C<(x,y)>, an estimate of the
gradient is made at these points, using the given boundary 
conditions. The gradients are stored in the C<g> attribute,
accessible via:

 $gradient = $obj->get( 'g' );

However, as this gradient is only calculated 'at the last moment',
C<g> will only contain data I<after> one of 
C<interpolate>, C<gradient>, or C<integrate> is used.

=back

=head2 Boundary conditions for the Hermite routines

If your data is monotonic, and you are not too bothered about
edge effects, then the default value of C<bc> of C<simple> is for you.
Otherwise, take a look at the description of
L<PDL::Slatec::chic|PDL::Slatec/chic> and use a hash reference
for the C<bc> attribute, with the following keys:

=over 3

=item monotonic

0 if the interpolant is to be monotonic in each interval (so
the gradient will be 0 at each switch point), 
otherwise the gradient is calculated using a 3-point difference
formula at switch points. 
If E<gt> 0 then the interpolant is forced to lie close to the 
data, if E<lt> 0 no such control is imposed.
Default = B<0>.

=item start

A perl list of one or two elements. The first element defines how the
boundary condition for the start of the array is to be calculated;
it has a range of C<-5 .. 5>, as given for the C<ic> parameter
of L<chic|PDL::Slatec/chic>. 
The second element, only used if options 2, 1, -1, or 2
are chosen, contains the value of the C<vc> parameter.
Default = B<[ 0 ]>.

=item end

As for C<start>, but for the end of the data.

=back

An example would be

 $obj->set( bc => { start => [ 1, 0 ], end => [ 1, -1 ] } )

which sets the first derivative at the first point to 0, 
and at the last point to -1.

=head2 Errors

The C<status> method provides a simple mechanism to check if
the previous method was successful. 
If the function returns an error flag, then it is stored
in the C<err> attribute.
To find out which routine was used, use the
C<routine> method.

=cut

#' fool emacs

package PDL::Func;

use strict;
use Carp;

####################################################################
#
# what modules are available ?
#
my %modules;
BEGIN {
    eval "use PDL::Slatec";
    $modules{slatec} = ($@ ? 0 : 1);
}

####################################################################
 
## Public routines:

=head1 FUNCTIONS

=head2 PDL::Func::init

=for usage

 $obj = PDL::Func->init( Interpolate => "Hermite", x => $x, y => $y );
 $obj = PDL::Func->init( { x => $x, y => $y } );

=for ref

Create a PDL::Func object, which can interpolate, and possibly
integrate and calculate gradients of a dataset.

If not specified, the value of Interpolate is taken to be 
C<Linear>, which means the interpolation is performed by
L<PDL::Primitive::interpolate|PDL::Primitive/interpolate>.
A value of C<Hermite> uses piecewise cubic Hermite functions,
which also allows the integral and gradient of the data
to be estimated.

Options can either be provided directly to the method, as in the
first example, or within a hash reference, as shown in the second
example.

=cut

# meaning of types:
#  required  - required, if this attr is changed, we need to re-initialise
#  settable  - can be changed with a init() or set() command
#  gettable  - can be read with a get() command
#
# do we really need gettable? Not currently, that's for sure,
# as everything is gettable

my %attr = 
    (
     Default => { 
	 x   => { required => 1, settable => 1, gettable => 1 },
	 y   => { required => 1, settable => 1, gettable => 1 },
	 err => { gettable => 1 },
     },
     Linear  => {},
     Hermite => {
	 bc  => { settable => 1, gettable => 1, required => 1, default => "simple" },
	 g   => { gettable => 1 },
     },
     );

sub init {
    my $this  = shift;
    my $class = ref($this) || $this;

    # class structure
    my $self = { };

    # make $self into an object
    bless $self, $class;

    # set up default attributes
    #
    my ( %opt ) = @_; 
    $opt{Interpolate} = "Linear" unless exists $opt{Interpolate};

    # set variables
    $self->set( %opt );
 
    # return the object
    return $self;
                                                                                
} # sub: init()

#####################################################################

# $self->_init_attr( $interpolate )
#
# set up the object for the given interpolation method
# - uses the values stored in %attr to fill in the
#   fields in $self AFTER clearing the object
#
# NOTE: called by set()
#
sub _init_attr {
    my $self = shift;
    my $interpolate = shift;

    croak "ERROR: Unknown interpolation scheme <$interpolate>.\n"
	unless defined $attr{$interpolate};

    # fall over if slatec library isn't present
    # and asking for Hermite interpolation
    croak "ERROR: Hermite interpolation is not available without PDL::Slatec.\n"
        if $interpolate eq "Interpolate" and $modules{slatec} == 0;

    # clear out the old data (if it's not the first time through)
    $self->{attributes} = {};
    $self->{values}     = {};
    $self->{types}      = { required => 0, settable => 0, gettable => 0 };
    $self->{flags}      = { scheme => $interpolate, status => 1, routine => "none", changed => 1 };

    # set up default values
    my $ref = $attr{Default};
    foreach my $attr ( keys %{$ref} ) {
	# set default values
	foreach my $type ( keys %{$self->{types}} ) {
	    $self->{attributes}{$attr}{$type} = $self->{types}{$type};
	}

	# change the values to those supplied
	foreach my $type ( keys %{$ref->{$attr}} ) {
	    $self->{attributes}{$attr}{$type} = $ref->{$attr}{$type}
		if exists $self->{types}{$type};
	}
	# set value to undef
	$self->{values}{$attr} = undef;
    }

    # now set up for the particular interpolation scheme
    $ref = $attr{$interpolate};
    foreach my $attr ( keys %{$ref} ) {
	# set default values, if not known
	unless ( defined $self->{attributes}{$attr} ) {
	    foreach my $type ( keys %{$self->{types}} ) {
		$self->{attributes}{$attr}{$type} = $self->{types}{$type};
	    }
	}

	# change the values to those supplied
	foreach my $type ( keys %{$ref->{$attr}} ) {
	    next if $type eq "default";
	    $self->{attributes}{$attr}{$type} = $ref->{$attr}{$type}
		if exists $self->{types}{$type};
	}
	# set value to default value/undef
	$self->{values}{$attr} = 
	    exists $ref->{$attr}{default} ? $ref->{$attr}{default} : undef;
    }
} # sub: _init_attr()

####################################################################

# call this at the start of each method that needs data
# stored in the object. This function ensures that all required 
# attributes exist and, if necessary, re-initialises the object
# - ie if the data has changed.
#
sub _check_attr {
    my $self = shift;
    return unless $self->{flags}{changed};

    my @emsg;
    foreach my $name ( keys %{ $self->{attributes} } ) {
	if( $self->{attributes}{$name}{required} ) {
	    push @emsg, $name unless defined($self->{values}{$name});
	}
    }
    croak "ERROR - the following attributes must be supplied:\n [ @emsg ]\n"
	unless $#emsg == -1;
    
    $self->{flags}{routine} = "none";
    $self->{flags}{status} = 1;
    
    $self->_initialise;
    $self->{flags}{changed} = 0;

} # sub: _check_attr()

####################################################################

# for a given scheme, it may be necessary to perform certain
# operations before the main routine of a method is called.
# It's done here.
#
# Due to lazy evaluation we try to do this as late as possible -
# _initialise() should only be called by _check_attr() 
#   [ at least at the moment ]
#
sub _initialise {
    my $self = shift;

    my $iflag = $self->scheme();
    if ( $iflag eq "Hermite" ) {
	_init_hermite( $self );
    }
    
} # sub: _initialise()

# something has changed, so we need to recalculate the gradient
# - actually, some changes don't invalidate the gradient,
#   however, with the current design, it's impossible to know
#   this. (poor design)
#
sub _init_hermite {
    my $self = shift;

    # set up error flags
    $self->{flags}{status} = 0;
    $self->{flags}{routine} = "none";

    # get values in one go
    my ( $x, $y, $bc ) = $self->_get_value( qw( x y bc ) );

    # check 1st dimention of x and y are the same
    #  ie allow the possibility of threading
    my $xdim = $x->getdim( 0 );
    my $ydim = $y->getdim( 0 );
    croak "ERROR: x and y piddles must have the same first dimension.\n"
	unless $xdim == $ydim;

    my ( $g, $ierr );
    if ( ref($bc) eq "HASH" ) {
	my $monotonic = $bc->{monotonic} || 0;
	my $start     = $bc->{start}     || [ 0 ];
	my $end       = $bc->{end}       || [ 0 ];

	my $ic = $x->short( $start->[0], $end->[0] );
	my $vc = $x->float( 0, 0 );

	if ( $#$start == 1 ) { $vc->set( 0, $start->[1] ); }
	if ( $#$end   == 1 ) { $vc->set( 1, $end->[1] ); }

	my $wk = $x->zeroes( $x->float, 2*$xdim );
	croak "ERROR: Hermite interpolation is not available without PDL::Slatec.\n"
	  if $modules{slatec} == 0;
	( $g, $ierr ) = chic( $ic, $vc, $monotonic, $x, $y, $wk );

	$self->{flags}{routine} = "chic";

    } elsif ( $bc eq "simple" ) {
	# chim
	croak "ERROR: Hermite interpolation is not available without PDL::Slatec.\n"
	  if $modules{slatec} == 0;
	( $g, $ierr ) = chim( $x, $y );

	$self->{flags}{routine} = "chim";

    } else {
	# Unknown boundary condition
	croak "ERROR: unknown boundary condition <$bc>.\n";
	# return;
    }

    $self->_set_value( g => $g, err => $ierr );

    if ( all $ierr == 0 ) {
	# everything okay
	$self->{flags}{status} = 1;
    } elsif ( any $ierr < 0 ) {
	# a problem
	$self->{flags}{status} = 0;
    } else {
	# there were switches in monotonicity
	$self->{flags}{status} = -1;
    }


}

####################################################################
####################################################################

# a version of set that ignores the settable flag
# and doesn't bother about the presence of an Interpolate
# value.
#
# - for use by the class, not by the public
#
# it still ignores unknown attributes
#
sub _set_value {
    my $self = shift;
    my %attrs = ( @_ );
    
    foreach my $attr ( keys %attrs ) {
	if ( exists($self->{values}{$attr}) ) {
	    $self->{values}{$attr} = $attrs{$attr};
	    $self->{flags}{changed} = 1;
	}
    }

} # sub: _set_value()

# a version of get that ignores the gettable flag
# - for use by the class, not by the public
#
# an unknown attribute returns an undef
#
sub _get_value {
    my $self = shift;

    my @ret;
    foreach my $name ( @_ ) {
	if ( exists $self->{values}{$name} ) {
	    push @ret, $self->{values}{$name};
	} else {
	    push @ret, undef;
	}
    }

    return wantarray ? @ret : $ret[0];

} # sub: _get_value()

####################################################################

=head2 PDL::Func::set

=for usage

 my $nset = $obj->set( x = $newx, $y => $newy );
 my $nset = $obj->set( { x = $newx, $y => $newy } );

=for ref

Set attributes for a PDL::Func object.

The return value gives the number of the supplied attributes
which were actually set. 

=cut

sub set {
    my $self = shift;
    return if $#_ == -1;

    my $vref;
    if ( $#_ == 0 and ref($_[0]) eq "HASH" ) {
	$vref = shift;
    } else {
	my %vals = ( @_ ); 
	$vref = \%vals;
    }

    # initialise attributes IFF Interpolate 
    # is specified
    #
    $self->_init_attr( $vref->{Interpolate} )
	if exists $vref->{Interpolate};

    my $ctr = 0;
    foreach my $name ( keys %{$vref} ) {
	next if $name eq "Interpolate";
	if ( exists $self->{attributes}{$name}{settable} ) {
	    $self->{values}{$name} = $vref->{$name};
	    $ctr++;
	}
    }

    $self->{flags}{changed} = 1 if $ctr;
    $self->{flags}{status}  = 1;
    return $ctr;

} # sub: set()

####################################################################

=head2 PDL::Func::get

=for usage

 my $x         = $obj->get( x );
 my ( $x, $y ) = $obj->get( qw( x y ) );

=for ref

Get attributes from a PDL::Func object.

Given a list of attribute names, return a list of
their values; in scalar mode return a scalar value.
If the supplied list contains an unknown attribute,
C<get> returns a value of C<undef> for that
attribute.

=cut

sub get {
    my $self = shift;

    my @ret;
    foreach my $name ( @_ ) {
	if ( exists $self->{attributes}{$name}{gettable} ) {
	    push @ret, $self->{values}{$name};
	} else {
	    push @ret, undef;
	}
    }

    return wantarray ? @ret : $ret[0];

} # sub: get()

####################################################################
#
# access to flags - have individual methods for these

=head2 PDL::Func::scheme

=for usage

 my $scheme = $obj->scheme;

=for ref

Return the type of interpolation of a PDL::Func object.

Returns either C<Linear> or C<Hermite>.

=cut

sub scheme { return $_[0]->{flags}{scheme}; }

=head2 PDL::Func::status

=for usage

 my $status = $obj->status;

=for ref

Returns the status of a PDL::Func object.

This method provides a high-level indication of 
the success of the last method called
(except for C<get> which is ignored).
Returns B<1> if everything is okay, B<0> if 
there has been a serious error,
and B<-1> if there
was a problem which was not serious.
In the latter case, C<$obj-E<gt>get("err")> may
provide more information, depending on the
particular scheme in use.

=cut

sub status { return $_[0]->{flags}{status}; }

=head2 PDL::Func::routine

=for usage

 my $name = $obj->routine;

=for ref

Returns the name of the last routine called by a PDL::Func object.

This is mainly useful for decoding the value stored in the
C<err> attribute.

=cut

sub routine { return $_[0]->{flags}{routine}; }

=head2 PDL::Func::attributes

=for usage

 $obj->attributes;
 PDL::Func->attributes;

=for ref

Print out the flags for the attributes of a PDL::Func object. 

Useful in case the documentation is just too opaque!

=for example

 PDL::Func->attributes;
 Flags  Attribute
  SGR    x
  SGR    y
  G      err

=cut

# note, can be called with the class, rather than just
# an object. However, not of great use, as this will only
# ever return the values for Interpolate => Linear
#
# to allow this, I've used a horrible hack - we actually
# create an object and then print out the attributes from that
# Ugh!
#
# It would have been useful if I'd stuck to sub-classes
# for different schemes
#
sub attributes { 
    my $self = shift;

    # ugh
    $self = $self->init unless ref($self);

    print "Flags  Attribute\n";
    while ( my ( $attr, $hashref ) = each %{$self->{attributes}} ) {
	my $flag = "";
	$flag .= "S" if $hashref->{settable};
	$flag .= "G" if $hashref->{gettable};
	$flag .= "R" if $hashref->{required};
	
	printf " %-3s    %s\n", $flag, $attr;
    }
    return;
} # sub: attributes()

####################################################################

=head2 PDL::Func::interpolate

=for usage

 my $yi = $obj->interpolate( $xi );

=for ref

Returns the interpolated function at a given set of points
(PDL::Func).

A status value of -1, as returned by the C<status> method, 
means that some of the C<$xi> points lay outside the 
range of the data. The values for these points
were calculated by extrapolation (the details depend on the
scheme being used).

=cut

sub interpolate {
    my $self = shift;
    my $xi   = shift;

    croak 'Usage: $obj->interpolate( $xi )' . "\n"
	unless defined $xi;

    # check everything is fine
    $self->_check_attr();

    # get values in one go
    my ( $x, $y ) = $self->_get_value( qw( x y ) );

    # farm off to routines
    my $iflag = $self->scheme;
    if ( $iflag eq "Linear" ) {
	return _interp_linear( $self, $xi, $x, $y );
    } elsif ( $iflag eq "Hermite" ) {
	return _interp_hermite( $self, $xi, $x, $y );
    }

} # sub: interpolate()

sub _interp_linear {
    my ( $self, $xi, $x, $y ) = ( @_ );

    my ( $yi, $err ) = PDL::Primitive::interpolate( $xi, $x, $y );

    $self->{flags}{status} = (any $err) ? -1 : 1;
    $self->_set_value( err => $err );
    $self->{flags}{routine} = "interpolate";

    return $yi;
} # sub: _interp_linear()

sub _interp_hermite {
    my ( $self, $xi, $x, $y ) = ( @_ );

    # get gradient
    my $g = $self->_get_value( 'g' );

    my ( $yi, $ierr ) = chfe( $x, $y, $g, 0, $xi );
    $self->{flags}{routine} = "chfe";
    $self->_set_value( err => $ierr );

    if ( all $ierr == 0 ) {
	# everything okay
	$self->{flags}{status} = 1;
    } elsif ( all $ierr > 0 ) {
	# extrapolation was required
	$self->{flags}{status} = -1;
    } else {
	# a problem
	$self->{flags}{status} = 0;
    }
	
    return $yi;
} # sub: _interp_linear()

=head2 PDL::Func::gradient

=for usage

 my $gi          = $obj->gradient( $xi );
 my ( $yi, $gi ) = $obj->gradient( $xi );

=for ref

Returns the derivative and, optionally,
the interpolated function for the C<Hermite>
scheme (PDL::Func).

=cut

sub gradient {
    my $self = shift;
    my $xi   = shift;

    croak 'Usage: $obj->gradient( $xi )' . "\n"
	unless defined $xi;

    croak 'Error: can not call gradient for Interpolate => "Linear".' ."\n"
	unless $self->scheme eq "Hermite";

    # check everything is fine
    $self->_check_attr();

    # get values in one go
    my ( $x, $y, $g ) = $self->_get_value( qw( x y g ) );

    my ( $yi, $gi, $ierr ) = chfd( $x, $y, $g, 0, $xi );
    $self->{flags}{routine} = "chfd";
    $self->_set_value( err => $ierr );

    if ( all $ierr == 0 ) {
	# everything okay
	$self->{flags}{status} = 1;
    } elsif ( all $ierr > 0 ) {
	# extrapolation was required
	$self->{flags}{status} = -1;
    } else {
	# a problem
	$self->{flags}{status} = 0;
    }
	
    # note order of values
    return wantarray ? ( $yi, $gi ) : $gi;

} # sub: gradient

=head2 PDL::Func::integrate

=for usage

 my $ans = $obj->integrate( index => pdl( 2, 5 ) );
 my $ans = $obj->integrate( x => pdl( 2.3, 4.5 ) );

=for ref

Integrate the function stored in the PDL::Func
object, if the scheme is C<Hermite>.

The integration can either be between points of
the original C<x> array (C<index>), or arbitrary x values
(C<x>). For both cases, a two element piddle
should be given,
to specify the start and end points of the integration.

=over 7

=item index

The values given refer to the indices of the points
in the C<x> array.

=item x

The array contains the actual values to integrate between.

=back

If the C<status> method returns a value of -1, then
one or both of the integration limits did not
lie inside the C<x> array. I<Caveat emptor> with the
result in such a case.

=cut

sub integrate {
    my $self = shift;

    croak 'Usage: $obj->integrate( $type => $limits )' . "\n"
	unless $#_ == 1;

    croak 'Error: can not call integrate for Interpolate => "Linear".' ."\n"
	unless $self->{flags}{scheme} eq "Hermite";

    # check everything is fine
    $self->_check_attr();

    $self->{flags}{status} = 0;
    $self->{flags}{routine} = "none";

    my ( $type, $indices ) = ( @_ );

    croak "Unknown type ($type) sent to integrate method.\n"
	unless $type eq "x" or $type eq "index";

    my $fdim = $indices->getdim(0);
    croak "Indices must have a first dimension of 2, not $fdim.\n"
	unless $fdim == 2;

    my $lo = $indices->slice('(0)');
    my $hi = $indices->slice('(1)');

    my ( $x, $y, $g ) = $self->_get_value( qw( x y g ) );
    my ( $ans, $ierr );

    if ( $type eq "x" ) {
	( $ans, $ierr ) = chia( $x, $y, $g, 0, $lo, $hi );
	$self->{flags}{routine} = "chia";

	if ( all $ierr == 0 ) {
	    # everything okay
	    $self->{flags}{status} = 1;
	} elsif ( any $ierr < 0 ) {
	    # a problem
	    $self->{flags}{status} = 0;
	} else {
	    # out of range
	    $self->{flags}->{status} = -1;
	}

    } else {
	( $ans, $ierr ) = chid( $x, $y, $g, 0, $lo, $hi );
	$self->{flags}->{routine} = "chid";

	if ( all $ierr == 0 ) {
	    # everything okay
	    $self->{flags}{status} = 1;
	} elsif ( all $ierr != -4 ) {
	    # a problem
	    $self->{flags}{status} = 0;
	} else {
	    # out of range (ierr == -4)
	    $self->{flags}{status} = -1;
	}

    }

    $self->_set_value( err => $ierr );
    return $ans;

} # sub: integrate()

####################################################################

=head1 TODO

It should be relatively easy to provide an interface to other
interpolation routines, such as those provided by the
Gnu Scientific Library (GSL), or the B-spline routines
in the SLATEC library.

In the documentation, the methods are preceeded by C<PDL::Func::>
to avoid clashes with functions such as C<set> when using
the C<help> or C<apropos> commands within I<perldl>.

=head1 HISTORY

Amalgamated C<PDL::Interpolate> and C<PDL::Interpolate::Slatec>
to form C<PDL::Func>. Comments greatly appreciated on the
current implementation, as it is not too sensible.

Thanks to Robin Williams, Halldór Olafsson, and Vince McIntyre.

=head1 THE FUTURE

Robin is working on a new version, that improves on the current version
a lot. No time scale though!

=head1 AUTHOR

Copyright (C) 2000,2001 Doug Burke (dburke@cfa.harvard.edu).
All rights reserved. There is no warranty. 
You are allowed to redistribute this software / documentation as 
described in the file COPYING in the PDL distribution.

=cut

####################################################################
# End with a true
1;

