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
 $x = sequence 5,5;
 # reduce by adding all
 # elements along 2nd dimension
 $y = $x->reduce('add',1);
 @ops = $x->canreduce; # return a list of all allowed operations

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

 $x = sequence 5,5;
 # reduce by adding all
 # elements along 2nd dimension
 $y = $x->reduce('add',1);
 $y = $x->reduce('plus',1);
 $y = $x->reduce('+',1);     # three ways to do the same thing

[ As an aside: if you are familiar with threading you will see that
this is actually the same as

 $y = $x->mv(1,0)->sumover

]

NOTE: You should quote the name of the operation (1st arg) that
you want C<reduce> to perform. This is important since some of the
names are identical to the names of the actual PDL functions
which might be imported into your namespace. And you definitely
want a string as argument, not a function invocation! For example,
this will probably fail:

  $y = $x->reduce(avg,1); # gives an error from invocation of 'avg'

Rather use

  $y = $x->reduce('avg',1);

C<reduce> provides a simple and unified interface to the
I<projection> functions and makes people coming from other
data/array languages hopefully feel more at home.

=for usage

 $result = $pdl->reduce($operation [,@dims]);

C<reduce> applies the named operation along the specified
dimension(s) reducing the input piddle dimension by as many
dimensions as supplied as arguments. If the
dimension(s) argument is omitted the operation is applied along the first
dimension. To get a list of valid operations see L<canreduce>.

NOTE - new power user feature: you can now supply a code
reference as operation to reduce with.

=for example

  # reduce by summing over dims 0 and 2
  $result = $pdl->reduce(\&sumover, 0, 2);

It is your responsibility to ensure that this is indeed a
PDL projection operation that turns vectors into scalars!
You have been warned.

=cut

*reduce = \&PDL::reduce;
sub PDL::reduce ($$;$) {
  my ($pdl, $op, @dims) = @_;
  barf "trying to reduce using unknown operation"
    unless exists $reduce{$op} || ref $op eq 'CODE';
  my $dim;
  if (@dims > 1) {
    my $n = $pdl->getndims;
    @dims = map { $_ < 0 ? $_ + $n : $_ } @dims;
    my $min = $n;
    my $max = 0;
    for (@dims) { $min = $_ if $_ < $min; $max = $_ if $_ > $max }
    barf "dimension out of bounds (one of @dims >= $n)"
      if $min >= $n || $max >= $n;
    $dim = $min; # this will be the resulting dim of the clumped piddle
    $pdl = $pdl->clump(@dims);
  } else {
    $dim = @dims > 0 ? $dims[0] : 0;
  }
  if (defined $dim && $dim != 0) { # move the target dim to the front
    my $n = $pdl->getndims;
    $dim += $n if $dim < 0;
    barf "dimension out of bounds" if $dim <0 || $dim >= $n;
    $pdl = $pdl->mv($dim,0);
  }
  my $method = ref $op eq 'CODE' ? $op : $reduce{$op};
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
