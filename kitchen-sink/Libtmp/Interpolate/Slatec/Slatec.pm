
=head1 NAME

PDL::Interpolate::Slatec - simple interface to SLATEC interpolation routines

=head1 SYNOPSIS

 use PDL::Interpolate::Slatec;
 use PDL::Math;

 # somewhat pointless way to estimate cos and sin,
 # but is shows that you can thread if you want to
 #
 my $x   = pdl( 0 .. 45 ) * 4 * 3.14159 / 180;
 my $y   = cat( sin($x), cos($x) );
 #
 my $obj = new PDL::Interpolate::Slatec( x => $x, y = $y );
 #
 my $xi  = pdl( 0.5, 1.5, 2.5 );
 my $yi  = $obj->interpolate( $xi );
 #
 print "cos( $xi ) equals ", $yi->slice(':,(0)'), "\n";
 cos( [0.5 1.5 2.5] ) equals  [0.87759844 0.070737667 -0.80115622]
 #
 print "sin( $xi ) equals ", $yi->slice(':,(1)'), "\n";
 sin( [0.5 1.5 2.5] ) equals  [ 0.4794191 0.99768655 0.59846449]
 #
 print cos($xi), "\n", sin($xi), "\n";
 [0.87758256 0.070737202 -0.80114362]
 [0.47942554 0.99749499 0.59847214]

=head1 DESCRIPTION

Use the interface defined by L<PDL::Interpolate|PDL::Interpolate>
to provide a simple way to use the SLATEC interpolation
routines (e.g. see L<PDL::Slatec|PDL::Slatec>).
Hence the name for this library - as returned by the C<library>
method - is C<"Slatec">.

Currently, only the 
L<piecewise cubic Hermite polynomial routines|PDL::Slatec/Piecewise_cubic_Hermite_interpol> 
are available (C<type == "pch">).

=head2 Attributes

The following changes are made to the attributes 
of L<PDL::Interpolate|PDL::Interpolate>:

 Attribute  Flag  Description
 bc         sgr   boundary conditions
 g          g     estimated gradient at x positions

 Attribute  Default    Allowed values
 bc         "simple"   see Boundary conditions section
 type       "pch"

Given the initial set of points C<(x,y)>, the C<"pch"> 
library estimates the gradient at these points using the
given boundary conditions (as specified by the
C<bc> attribute).
The estimated gradient can be obtained using

 $gradient = $obj->get( 'g' );

As described in the L<interpolate|/interpolate> method,
the C<"pch"> routines can also estimate the gradient,
as well as the function value, for a set of C<$xi>.

=head2 Boundary conditions for the pch routines

If your data is monotonic, and you are not too bothered about
edge effects, then the default value of C<bc> of C<"simple"> is for you.
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

 $obj->set( bc => { start => [ 1, 0 ], end => [ 1, -1 ] }

which sets the first derivative at the first point to 0, 
and at the last point to -1.

=head2 Errors

The C<status> method provides a simple mechanism to check if
the previous method was successful. The C<err> attribute
contains the C<$ierr> piddle returned by the Slatec
routine if a more precise diagnostic is required.
To find out which routine was called, use the
C<routine> method.

=cut

package PDL::Interpolate::Slatec;

use PDL::Interpolate;
use PDL::Slatec;
use Carp;

use strict;
use vars qw( @ISA );

@ISA     = qw ( PDL::Interpolate );

####################################################################
#

####################################################################
# 
## Public routines:

sub new {
    my $this  = shift;
    my $class = ref($this) || $this;
    my $self  = $class->SUPER::new();  # note: do not send in values

    # change from PDL::Interpolate to PDL::Interpolate::Slatec
    bless ($self, $class);

    # change class attributes
    $self->_change_attr( 
			 bc => { required => 1, settable => 1 }, # already gettable
			 );
    $self->_set_value( bc => "simple", type => "pch" );
    $self->_add_attr( 
		      g => { gettable => 1 },
		     );

    $self->{flags}->{library} = "Slatec";
    $self->{flags}->{routine} = "none";

    # set variables
    $self->set( @_ );

    return $self;         
                                               
} # sub: new

####################################################################

# set up the interpolation
#
sub _initialise {
    my $self = shift;

    # set up error flags
    $self->{flags}->{status} = 0;
    $self->{flags}->{routine} = "none";

    # get values in one go
    my ( $x, $y, $g, $bc ) = $self->_get_value( qw( x y g bc ) );

    # check 1st dimention of x and y are the same
    #  ie allow the possibility of threading
    my $xdim = $x->getdim( 0 );
    my $ydim = $y->getdim( 0 );
    croak "ERROR: x and y piddles must have the same first dimension.\n"
	unless $xdim == $ydim;

    # if a gradient has been specified, then we don't need to do anything
    # - other than check the dimensions
    if ( defined $g ) {
	croak "ERROR: gradient piddle must have the same first dimension as x and y.\n"
	    unless $g->getdim( 0 ) == $xdim;
	$self->{flags}->{status} = 1;
	return;
    }

    my $ierr;
    if ( ref($bc) eq "HASH" ) {
	my $monotonic = $bc->{monotonic} || 0;
	my $start     = $bc->{start}     || [ 0 ];
	my $end       = $bc->{end}       || [ 0 ];

	my $ic = $x->short( $start->[0], $end->[0] );
	my $vc = $x->float( 0, 0 ); # it will get promoted if required

	if ( $#$start == 1 ) { $vc->set( 0, $start->[1] ); }
	if ( $#$end   == 1 ) { $vc->set( 1, $end->[1] ); }

	my $wk = $x->zeroes( $x->float, 2*$xdim );
	( $g, $ierr ) = chic( $ic, $vc, $monotonic, $x, $y, $wk );

	$self->{flags}->{routine} = "chic";

    } elsif ( $bc eq "simple" ) {
	# chim
	( $g, $ierr ) = chim( $x, $y );

	$self->{flags}->{routine} = "chim";

    } else {
	# Unknown boundary condition
	croak "ERROR: unknown boundary condition <$bc>.\n";
	# return;
    }

    $self->_set_value( g => $g, err => $ierr );

    if ( all $ierr == 0 ) {
	# everything okay
	$self->{flags}->{status} = 1;
    } elsif ( any $ierr < 0 ) {
	# a problem
	$self->{flags}->{status} = 0;
    } else {
	# there were switches in monotonicity
	$self->{flags}->{status} = -1;
    }

} # sub: _initialise

####################################################################

=head2 interpolate

=for usage

 my $yi          = $obj->interpolate( $xi );
 my ( $yi, $gi ) = $obj->interpolate( $xi );

=for ref

Returns the interpolated function and derivative
at a given set of points.

If evaluated in scalar mode, it returns only 
the interpolated function values.

=cut

sub interpolate {
    my $self = shift;
    my $xi   = shift;

    croak 'Usage: $obj->interpolate( $xi )' . "\n"
	unless defined $xi;

    # check everything is fine
    $self->_check_attr();

    # get values in one go
    my ( $x, $y, $g ) = $self->_get_value( qw( x y g ) );

    my ( $yi, $gi, $ierr );

    if ( wantarray ) {
	( $yi, $gi, $ierr ) = chfd( $x, $y, $g, 0, $xi );
	$self->{flags}->{routine} = "chfd";
    } else {
	( $yi, $ierr ) = chfe( $x, $y, $g, 0, $xi );
	$self->{flags}->{routine} = "chfe";
    }

    # set err/status info
    $self->_set_value( err => $ierr );

    if ( all $ierr == 0 ) {
	# everything okay
	$self->{flags}->{status} = 1;
    } elsif ( all $ierr > 0 ) {
	# extrapolation was required
	$self->{flags}->{status} = -1;
    } else {
	# a problem
	$self->{flags}->{status} = 0;
    }
	
    return wantarray ? ( $yi, $gi ) : $yi;

} # sub: interpolate

=head2 integrate

=for usage

 my $ans = $obj->integrate( index => pdl( 2, 5 ) );
 my $ans = $obj->integrate( x => pdl( 2.3, 4.5 ) );

=for ref

Integrate the function stored in the PDL::Interpolate::Slatec
object. 

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

The reason for using piddles, rather than arrays, is that
it allows for threading.

=cut

sub integrate {
    my $self = shift;

    croak 'Usage: $obj->integrate( $type => $limits )' . "\n"
	unless $#_ == 1;

    # check everything is fine
    $self->_check_attr();

    $self->{flags}->{status} = 0;
    $self->{flags}->{routine} = "none";

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
	$self->{flags}->{routine} = "chia";

	if ( all $ierr == 0 ) {
	    # everything okay
	    $self->{flags}->{status} = 1;
	} elsif ( any $ierr < 0 ) {
	    # a problem
	    $self->{flags}->{status} = 0;
	} else {
	    # out of range
	    $self->{flags}->{status} = -1;
	}

    } else {
	( $ans, $ierr ) = chid( $x, $y, $g, 0, $lo, $hi );
	$self->{flags}->{routine} = "chid";

	if ( all $ierr == 0 ) {
	    # everything okay
	    $self->{flags}->{status} = 1;
	} elsif ( all $ierr != -4 ) {
	    # a problem
	    $self->{flags}->{status} = 0;
	} else {
	    # out of range (ierr == -4)
	    $self->{flags}->{status} = -1;
	}

    }

    $self->_set_value( err => $ierr );
    return $ans;
}

=head1 AUTHOR

Copyright (C) 2000 Doug Burke (burke@ifa.hawaii.edu).
All rights reserved. There is no warranty. 
You are allowed to redistribute this software / documentation as 
described in the file COPYING in the PDL distribution.                                    

=head1 SEE ALSO

L<PDL::Interpolate>, L<PDL>, perltoot(1).

=cut

####################################################################
# End with a true
1;
