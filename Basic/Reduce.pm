=head1 NAME

PDL::Reduce -- a C<reduce> function for PDL

=head1 DESCRIPTION

Many languages have a C<reduce> function used to reduce
the rank of an N-D array by one. It works by applying a selected
operation along a specified dimension. This module implements
such a function for PDL by providing a simplified interface
to the existing projection functions (e.g. C<sumover>,
C<maximum>, C<average>, etc).

=head1 SYNOPSIS

 use PDL::Reduce;
 $a = sequence 5,5;
 # reduce by adding all
 # elements along 2nd dimension
 $b = $a->reduce('add',1);
 @ops = $a->canreduce; # return a list of all allowed operations

=head1 FUNCTIONS

=cut

# in a very similar vein we want the following methods
# (1) accumulate
# (2) outer
# what's reduceat ??

# TODO
# - aliases (e.g. plus -> add)
# - other binary ops?
# - allow general subs?

package PDL::Reduce;
use PDL::Core ''; # barf
use PDL::Exporter;
use strict;

@PDL::Reduce::ISA = qw/PDL::Exporter/;
@PDL::Reduce::EXPORT_OK = qw/reduce canreduce/;
%PDL::Reduce::EXPORT_TAGS = (Func=>[@PDL::Reduce::EXPORT_OK]);

# maps operations onto underlying PDL primitives
my %reduce = (
	      add  => 'sumover',
	      '+'    => 'sumover',
	      plus => 'sumover',
	      mult => 'prodover',
	      '*'    => 'prodover',
	      dadd => 'dsumover',
	      dmult => 'dprodover',
	      avg  => 'average',
	      davg => 'daverage',
	      and  => 'andover',
	      band => 'bandover',
	      bor  => 'borover',
	      or   => 'orover',
	      median => 'medover',
	      integral => 'intover',
	      max  => 'maximum',
	      min  => 'minimum',
	      oddmedian => 'oddmedover',
	      iszero => 'zcover',
	     );

=head2 reduce

=for ref

reduce dimension of piddle by one by applying an operation
along the specified dimension

=for example

 $a = sequence 5,5;
 # reduce by adding all
 # elements along 2nd dimension
 $b = $a->reduce('add',1);
 $b = $a->reduce('plus',1);
 $b = $a->reduce('+',1);     # three ways to do the same thing

[ As an aside: if you are familiar with threading you will see that
this is actually the same as

 $b = $a->mv(1,0)->sumover

]

NOTE: You should quote the name of the operation (1st arg) that
you want C<reduce> to perform. This is important since some of the
names are identical to the names of the actual PDL functions
which might be imported into your namespace. And you definitely
want a string as argument, not a function invocation! For example,
this will probably fail:

  $b = $a->reduce(avg,1); # gives an error from invocation of 'avg'

Rather use

  $b = $a->reduce('avg',1);

C<reduce> provides a simple and unified interface to the
I<projection> functions and makes people coming from other
data/array languages hopefully feel more at home.

=for usage

 $result = $pdl->reduce($operation [,$dim]);

C<reduce> applies the named operation along the specified
dimension reducing the input piddle dimension by one. If the
dimension is omitted the operation is applied along the first
dimension. To get a list of valid operations see L<canreduce>.

=cut

*reduce = \&PDL::reduce;
sub PDL::reduce ($$;$) {
  my ($pdl, $op, $dim) = @_;
  barf "trying to reduce using unknown operation"
    unless exists $reduce{$op};
  if (defined $dim && $dim != 0) { # move the target dim to the front
    my $n = $pdl->getndims;
    $dim += $n if $dim < 0;
    barf "dimension out of bounds" if $dim <0 || $dim >= $n;
    $pdl = $pdl->mv($dim,0);
  }
  my $method = $reduce{$op};
  return $pdl->$method();
}

=head2 canreduce

=for ref

return list of valid named C<reduce> operations
Some common operations can be accessed using a
number of names, e.g. C<'+'>, C<add> and C<plus>
all sum the elements along the chosen dimension.

=for example

  @ops = PDL->canreduce;

This list is useful if you want to make sure which
operations can be used with C<reduce>.

=cut

*canreduce = \&PDL::canreduce;
sub PDL::canreduce {
  my ($this) = @_;
  return keys %reduce;
}

=head1 AUTHOR

Copyright (C) 2000 Christian Soeller (c.soeller@auckland.ac.nz). All
rights reserved. There is no warranty. You are allowed to redistribute
this software / documentation under certain conditions. For details,
see the file COPYING in the PDL distribution. If this file is
separated from the PDL distribution, the copyright notice should be
included in the file.

=cut

1;
