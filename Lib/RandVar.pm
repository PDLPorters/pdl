=head1  NAME

RandVar -- an object that generates random (or quasirandom) sequences.

=head1 VERSION

This document refers to version 0.01 of RandVar

=head1 SYNOPSIS

  use RandVar;

  $m = new RandVar(<dims>,<options>)


=head1 DESCRIPTION

=head2 Overview

This package implements random variable streams with various options.  It's designed
for easy subclassing -- see, for example, PDL::Lib::RandVar::Sobol for subrandom sequences.
It's a class so that you can store internal information on a per-variable basis; but
the base class just uses a class-variable seed.

=head2 History

  0.01     4-Dec-2001 -- Basic functionality (CED)

=head2 Author, license, no warranty

Copyright 2001, Craig DeForest.

This code may be distributed under the same terms as Perl itself
(license available at http://ww.perl.org).  Copying, reverse
engineering, distribution, and modification are explicitly allowed so
long as this notice is preserved intact and modified versions are
clearly marked as such.

If you modify the code and it's useful, please check it in to the
PDL source tree or send a copy of the modified version to 
cdeforest@solar.stanford.edu.

This package comes with NO WARRANTY.

=head2 Bugs:

At the moment, no repeatability is included because this is just a wrapper around
perl's rand function call.  A real random number generator probably ought to go in
here sooner or later, for real seed-based reprodcible number sequences.

=head1 FUNCTIONS

=cut

use strict;
use Carp;
use PDL::NiceSlice;
use PDL;

$PDL::Lib::RandVar::VERSION = 0.01;

######################################################################
=pod

=head2 new

=for ref

Construct a uniformly distributed random variable.

=for sig

  Signature: (size())

=for usage

  $a = new RandVar(<size>,<opt>);

=for opt
  
=over 3

=item range

2xn piddle containing min and max values for each dimension of the R.V.

=item seed

A number to use as the seed.  If omitted, then the system clock is used.
(Not implemented at this level but placed here as a hook)

=back

=for example

  $xyrand = new RandVar(2,{range=>pdl([$xmin,$xmax],[$ymin,$ymax])});
  $newxy = sample $xyrand;

=cut

sub PDL::Lib::RandVar::new {
  my($opt);
  for(my $i=0;$i<@_;$i++) {
    if(ref $_[$i] eq 'HASH') {
      $opt = splice(@_,$i,1);
      last;
    }
  }
  my($type,$size) = @_;

  my($me);
  print "PDL::Lib::RandVar::new($type,$size,$opt)\n";
  croak("PDL::Lib::RandVar::new: options must be a hash ref or not exist") 
    if(defined $opt  and  ref $opt ne 'HASH');

  $me = {size => ( $opt->{size} || 1 )
	 };

  if(defined($opt->{seed})) {
    $me->{seed} = $opt->{seed} 
  } else {
    my($s) = crypt(time,reverse(time));
    my($i,$a);
    for $i(0..7) { ($a *= 64) += ord(substr($s,$i+3,1));}
    $me->{seed} = $a;
  }

  $me->{range} = ( defined $opt->{range} 
		   ? $opt->{range} 
		   : pdl(0,1)->dummy(1,$me->{size}) );

  croak('PDL::Lib::RandVar::new: must specify at least 2-element pdl as range!\n')
    if($me->{range}->nelem < 2);

  $me->{range} = $me->{range}->dummy(1,$me->{size})
    if($me->{range}->ndims < 2);

  $me->{start} = $me->{range}->((0),:);
  $me->{scale} = $me->{range}->((1),:) - $me->{start};
  $me->{opt} = {%$opt} if(ref $opt eq 'HASH');
  
  return bless($me,$type);
}

##############################
=pod
=head2 sample

=for ref

Return one or more samples of the random variable

This is a pretty stoopid implementation -- it just calls the perl rand() function
a bunch of times and is here primarily to get the ball rolling on the object.

=for sig

 Signature: sample(n(),[o]out(d,n))
 
 You can pass in an output pdl to avoid having to reallocate each time.

=for usage

  $out = <RandVar>->sample(<n>);

=for example 

  $rv = new RandVar;
  $samps = $rv->sample(10);

You get back an <n>x<d> array, where <n> is the number of samples you ask for
and <d> is the dimension of the random variable.  This may seeem transposed but
it allows you to drop the last dimension if your variable is a scalar.

=cut

sub PDL::Lib::RandVar::sample {
  my($me,$n,$out) = @_;
  print "PDL::Lib::RandVar::sample($me,$n,$out)\n";
  $n = 1 unless(defined $n);

  $out = ($me->{size}>1) ? zeroes($n,$me->{size}) : zeroes($n)
    unless defined($out);


  if($me->{size}>1) {
    my($i,$j);
    for $i(0..$n-1) {
      for $j(0..$me->{size}-1) {
	$out->(($i),($j)) .= rand;
      }
      
      ($out->(($i),:) *= $me->{scale}) += $me->{start};
    }
  } else {
    my($i);
    for $i(0..$n-1) {
      $out->($i) .= rand;
    }
    ($out *= $me->{scale}) += $me->{start};
  }
  
  return $out;
}




1;
  
  
  
                      






