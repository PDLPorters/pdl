
=head1 NAME

PDL::Interpolate - provide a consistent interface to the interpolation routines available in PDL

=head1 SYNOPSIS

 use PDL::Interpolate;

 my $i = new PDL::Interpolate( x => $x, y = $y );
 my $y = $i->interpolate( $xi ); 

=head1 DESCRIPTION

This module aims to provide a relatively-uniform interface
to the various interpolation methods available to PDL.
The idea is that a different interpolation scheme
can be used just by changing the C<new> call.

At present, PDL::Interpolate itself just provides
a somewhat-convoluted interface to the C<interpolate>
function of L<PDL::Primitive|PDL::Primitive/interpolate>. 
However, it is expected that derived classes,
such as 
L<PDL::Interpolate::Slatec|PDL::Interpolate::Slatec>,
will actually be used in real-world situations.

To use, create a PDL::Interpolate (or a derived class)
object, supplying it with its required attributes.

=head1 LIBRARIES

Currently, the avaliable classes are

=over 4

=item PDL::Interpolate

Provides an interface to the interpolation routines of PDL.
At present this is the linear interpolation routine
L<PDL::Primitive::interpol|PDL::Primitive/interpol>.

=item PDL::Interpolate::Slatec

The SLATEC library contains several approaches to interpolation:
piecewise cubic Hermite functions and B-splines.
At present, only the former method is available.

=back

It should be relatively easy to provide an interface to other
interpolation routines, such as those provided by the
Gnu Scientific Library (GSL).

=head1 ATTRIBUTES

The attributes (or options) of an object are as follows; 
derived classes may modify this list.

 Attribute  Flag  Description
 x          sgr   x positions of data
 y          sgr   function values at x positions
 bc         g     boundary conditions
 err        g     error flag
 type       g     type of interpolation

A flag of C<s> means that a user can set this attribute 
with the L<new|/new> or L<set|/set> methods,
a flag of C<g> means that the user can obtain the 
value of this attribute using L<get|/get>,
and a flag of C<r> means that the attribute is required
when an object is created (see the L<new|/new> method).

 Attribute  Default value
 bc         "none"
 type       "linear"

If a routine is sent an attribute it does not understand, then
it ignores that attribute, except for L<get|/get>, which
returns C<undef> for that value.

=head1 METHODS

The default methods are described below. However, defined classes
may extend them as they see fit, and add new methods.

Throughout this documentation, C<$x> and C<$y> refer to the function
to be interpolated whilst C<$xi> and C<$yi> are the interpolated values.

=head1 THREADING

The class will thread properly if the routines it calls do so.
See the SYNOPSIS section of L<PDL::Interpolate::Slatec>
(if available) for an example.

=cut

package PDL::Interpolate;

use Carp;
use strict;

####################################################################
 
## Public routines:

=head2 new

=for usage

 $obj = new PDL::Interpolate( x => $x, y => $y );

=for ref

Create a PDL::Interpolate object.

The required L<attributes|/attributes> are 
C<x> and C<y>.
At present the only available interpolation method 
is C<"linear"> - which just uses
L<PDL::Primitive::interpolate|PDL::Primitive::interpolate> - and
there are no options for boundary conditions, which is why
the C<type> and C<bc> attributes can not be changed.

=cut

# meaning of types:
#  required  - required, if this attr is changed, we need to re-initialise
#  settable  - can be changed with a new() or set() command
#  gettable  - can be read with a get() command
#
sub new {
    my $this  = shift;
    my $class = ref($this) || $this;

    # class structure
    my $self = { 
	attributes => {},
	values     => {},
	types      => { required => 0, settable => 0, gettable => 0 },
	flags      => { library => "PDL", status => 1, routine => "none", changed => 1 },
    };

    # make $self into an object
    bless $self, $class;

    # set up default attributes
    #
    $self->_add_attr( 
		      x    => { required => 1, settable => 1, gettable => 1 },
		      y    => { required => 1, settable => 1, gettable => 1 },
		      bc   => { gettable => 1 },
		      err  => { gettable => 1 },
		      type => { gettable => 1 }, 
		      );
    $self->_set_value( 
		       bc   => "none",
		       type => "linear",
		       );

    # set variables
    # - expect sub-classes to call this new with no variables, so $#_ == -1
    $self->set( @_ ) if ( @_ );
 
    # return the object
    return $self;
                                                                                
} # sub: new()

#####################################################################

# in _add_attr(), _change_attr() and _add_attr_type()
# we set flags->changed to 1 when something changes. It's
# a bit over the top to do this, as these should only be called when
# creating the object, when the changed flag should be set to 1 anyway

# add attributes to the object and sets value to undef
#
# supply a hash array, keys == variable name,
# values are a hash array with keys matching
# $self->{values}, which also gives the default value
# for the type
#
# this can only be used to create an attribute - 
# see _change_attr() to change an already exsiting attribute.
#
# the fields are set to the default values, then filled in with the supplied values
# any value that is unknown is ignored
#
sub _add_attr {
    my $self = shift;
    my %attrs = ( @_ );

    foreach my $attr ( keys %attrs ) {
	croak "ERROR: adding an attribute ($attr) which is already known.\n"
	    if defined $self->{attributes}->{$attr};

	# set default values
	foreach my $type ( keys %{$self->{types}} ) {
	    $self->{attributes}->{$attr}->{$type} = $self->{types}->{$type};
	}

	# change the values to those supplied
	foreach my $type ( keys %{$attrs{$attr}} ) {
	    $self->{attributes}->{$attr}->{$type} = $attrs{$attr}->{$type}
		if exists $self->{types}->{$type};
	}
	# set value to undef
	$self->{values}->{$attr} = undef;
    }
    $self->{flags}->{changed} = 1;

} # sub: _add_attr()
 
# changes attributes of the object
# 
# the given attributes MUST already exist
#
sub _change_attr {
    my $self = shift;
    my %attrs = ( @_ );

    foreach my $attr ( keys %attrs ) {
	croak "ERROR: changing an attribute ($attr) which is not known.\n"
	    unless defined $self->{attributes}->{$attr};

	# change the values to those supplied
	foreach my $type ( keys %{$attrs{$attr}} ) {
	    if ( exists $self->{types}->{$type} ) {
		$self->{attributes}->{$attr}->{$type} = $attrs{$attr}->{$type};
		$self->{flags}->{changed} = 1;
	    }
	}
    }
} # sub: _change_attr()

# adds the given types to the allowed list, and
# updates all attributes to contain the default value
#
# Useful for sub-classes which add new types
#
sub _add_attr_type {
    my $self = shift;
    my %types = ( @_ );

    foreach my $type ( keys %types ) {
	croak "ERROR: adding type ($type) that is already known.\n"
	    if exists $self->{types}->{$type};
	$self->{types}->{$type} = $types{$type};

	# loop through each attribute, adding this type
	foreach my $attr ( keys %{$self->{attributes}} ) {
	    $self->{attributes}->{$attr}->{$type} = $types{$type};
	}

	$self->{flags}->{changed} = 1;
    }
} # sub: _add_attr_type()
 
####################################################################

# if an attribute has changed, check all required attributes
# still exist and re-initialise the object (for PDL::Interpolate
# this is a nop)
#
sub _check_attr {
    my $self = shift;
    return unless $self->{flags}->{changed};

    my @emsg;
    foreach my $name ( keys %{ $self->{attributes} } ) {
	if( $self->{attributes}->{$name}->{required} ) {
	    push @emsg, $name unless defined($self->{values}->{$name});
	}
    }
    croak "ERROR - the following attributes must be supplied:\n [ @emsg ]\n"
	unless $#emsg == -1;
    
    $self->{flags}->{routine} = "none";
    $self->{flags}->{status} = 1;
    
    $self->_initialise;
    $self->{flags}->{new} = 0;

} # sub: check_attr()

####################################################################
#
# method to be over-ridden by sub-classes

# PDL::Interpolate needs no initialisation
#
sub _initialise {}

####################################################################

# a version of set that ignores the settable flag
# - for use by the class, not by the public
#
# it still ignores unknown attributes
#
sub _set_value {
    my $self = shift;
    my %attrs = ( @_ );
    
    foreach my $attr ( keys %attrs ) {
	if ( exists($self->{values}->{$attr}) ) {
	    $self->{values}->{$attr} = $attrs{$attr};
	    $self->{flags}->{changed} = 1;
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
	if ( exists $self->{values}->{$name} ) {
	    push @ret, $self->{values}->{$name};
	} else {
	    push @ret, undef;
	}
    }

    return wantarray ? @ret : $ret[0];

} # sub: _get_value()

####################################################################

=head2 set

=for usage

 my $nset = $obj->set( x = $newx, $y => $newy );

=for ref

Set attributes for a PDL::Interpolate object.

The return value gives the number of the supplied attributes
which were actually set. 

=cut

sub set {
    my $self = shift;
    my %vals = ( @_ );

    my $ctr = 0;
    foreach my $name ( keys %vals ) {
	if ( exists $self->{attributes}->{$name}->{settable} ) {
	    $self->{values}->{$name} = $vals{$name};
	    $ctr++;
	}
    }

    $self->{flags}->{changed} = 1 if $ctr;
    return $ctr;

} # sub: set()

####################################################################

=head2 get

=for usage

 my $x         = $obj->get( x );
 my ( $x, $y ) = $obj->get( qw( x y ) );

=for ref

Get attributes from a PDL::Interpolate object.

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
	if ( exists $self->{attributes}->{$name}->{gettable} ) {
	    push @ret, $self->{values}->{$name};
	} else {
	    push @ret, undef;
	}
    }

    return wantarray ? @ret : $ret[0];

} # sub: get()

####################################################################

=head2 interpolate

=for usage

 my $yi = $obj->interpolate( $xi );

=for ref

Returns the interpolated function at a given set of points.

A status value of -1, as returned by the C<status> method, 
means that some of the C<$xi> points lay outside the 
range of the data. The values for these points
were calculated using linear extrapolation.

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

    my ( $yi, $err ) = PDL::Primitive::interpolate( $xi, $x, $y );

    if ( any $err ) {
	$self->{flags}->{status} = -1;
    } else {
	$self->{flags}->{status} = 1;
    }
    $self->_set_value( err => $err );

    $self->{flags}->{routine} = "interpolate";

    return $yi;
}

####################################################################
#
# access to flags - have individual methods for these

=head2 status

=for usage

 my $status = $obj->status;

=for ref

Returns the status of a PDL::Interpolate object

Returns B<1> if everything is okay, B<0> if 
there has been a serious error since the last time
C<status> was called, and B<-1> if there
was a problem which was not serious.
In the latter case, C<$obj-E<gt>get("err")> may
provide more information, depending on the
particular class.

=cut

sub status { my $self = shift; return $self->{flags}->{status}; }

=head2 library

=for usage

 my $name = $obj->library;

=for ref

Returns the name of the library used by a PDL::Interpolate object

For PDL::Interpolate, the library name is C<"PDL">.

=cut

sub library { my $self = shift; return $self->{flags}->{library}; }

=head2 routine

=for usage

 my $name = $obj->routine;

=for ref

Returns the name of the last routine called by a PDL::Interpolate object.

For PDL::Interpolate, the only routine used is C<"interpolate">.
This will be more useful when calling derived classes,
in particular when trying to decode the values stored in the
C<err> attribute.

=cut

sub routine { my $self = shift; return $self->{flags}->{routine}; }

=head2 attributes

=for usage

 $obj->attributes;
 PDL::Interpolate::attributes;

=for ref

Print out the flags for the attributes of an object. 
Useful in case the documentation is just too opaque!

=for example

 PDL::Interpolate->attributes;
 Flags  Attribute
  SGR    x
  SGR    y
  G      err
  G      type
  G      bc                                                                      

=cut

# note, can be called with the class, rather than just
# an object
#
# to allow this, I've used a horrible hack - we actually
# create an object and then print out the attributes from that
# Ugh!
#
sub attributes { 
    my $self = shift;

    # ugh
    $self = $self->new unless ref($self);

    print "Flags  Attribute\n";
    while ( my ( $attr, $hashref ) = each %{$self->{attributes}} ) {
	my $flag = "";
	$flag .= "S" if $hashref->{settable};
	$flag .= "G" if $hashref->{gettable};
	$flag .= "R" if $hashref->{required};
	
	printf " %-3s    %s\n", $flag, $attr;
    }
    return;
}

####################################################################

=head1 AUTHOR

Copyright (C) 2000 Doug Burke (burke@ifa.hawaii.edu).
All rights reserved. There is no warranty. 
You are allowed to redistribute this software / documentation as 
described in the file COPYING in the PDL distribution.                                    

=head1 SEE ALSO

L<PDL>, perltoot(1).

=cut

####################################################################
# End with a true
1;
