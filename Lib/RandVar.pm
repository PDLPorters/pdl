=head1  NAME

PDL::RandVar -- Random number sequences.

=head1 VERSION

This document refers to version 1.0 of RandVar

=head1 SYNOPSIS

  use PDL::RandVar;

  $m = new PDL::RandVar(<dims>,<options>)

=head1 DESCRIPTION

This package implements random variable streams with various options.
It provides a uniform interface to (hopefully, eventually) a wide variety
of random and pseudo-random number generators.  The base class uses
a uniformly distributed engine (currently just perl's own rand function),
and subclasses generate different distributions.  

Once you've declared a random variable, you can get out samples with
the explicit ->sample method.  Eventually, sampling will be
made implicit, so that you can just include random variables in expressions
when you want a sample per element of the expression, or use ->sample for 
more complex sampling.

RandVar is designed for easy subclassing.  You need only implement 
->sample and ->new to get a new class.

When you ``use PDL::RandVar'' you also get some standard subclasses.
They're pretty cheap to load.  When you use RandVar, the following classes
are also automagically loaded (and have their own documentation):

=over 3

=item RandVar::Sobol

Subrandom sequences that help some types of algorithm converge faster.

=item RandVar::Histogram

Arbitarily distributed random variables.

=item RandVar::Gaussian

Gaussian distributions.  

=back

=head1 History

  0.01     4-Dec-2001 -- Basic functionality (CED)
  1.0      9-Jan-2002 -- seems to work OK (CED)

=head1 Author, license, no warranty

This file copyright(C) 2001, 2002 Craig DeForest
(cdeforest@solar.stanford.edu).  This software/documentation may be
distributed under the same terms as PDL itself (license available at
http://pdl.perl.org). This package comes with NO WARRANTY.

=head1 Bugs:

At the moment, repeatability by seeding is not implemented.  More work
needs to be done to get reproducible sequences.

=head1 To Do:

=over 3

=item Implement repeatable sequences

(see Bugs)


=item Make RVs act more like pdls

ideally you ought to be able to declare a variable as a RandVar, and
then use it in expressions to get samples automagically on-demand, 
without explicitly calling ->sample, ie ($a * $randvar) ought to do
the Right Thing.  The random variable ought to draw as many samples
as needed for context (e.g. zeroes(100,200)+$randvar out to get 
20,000 samples); if you want fewer, you can fall back on ->sample
to specify how many (e.g. zeroes(100,200)+$randvar->sample(100) 
gets 100 samples and automagically threads over the 200 dimension).

This gets implemented at the top level -- subclasses need only implement
sample() and RandVar should handle the rest.

=item Tie in the Gnu library

The gnu random variable functions are extensive and just need tiny
wrappers to turn into subclasses.  

=back

=head1 FUNCTIONS

=cut

BEGIN {
  my($a);

  for $a('Histogram','Sobol') {
#    print "Using PDL::RandVar::$a...\n";
    eval "use PDL::RandVar::$a;";
    print $@;
  }

  package PDL::RandVar;
  @ISA=('PDL');
}

use strict;
use Carp;
use PDL;


$PDL::RandVar::VERSION = 1.0;

######################################################################
=pod

=head2 new

=for ref

Construct a uniformly distributed random variable.

=for sig

  Signature: (size())

=for usage

  $a = new RandVar(<size>,<opt>);

Options:

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

sub PDL::RandVar::new {
  my($opt);
  for(my $i=0;$i<@_;$i++) {
    if(ref $_[$i] eq 'HASH') {
      $opt = splice(@_,$i,1);
      last;
    }
  }
  my($type,$size) = @_;

  my($me);

  croak("PDL::RandVar::new: options must be a hash ref or not exist") 
    if(defined $opt  and  ref $opt ne 'HASH');

  $me = {size => ( $opt->{size} || 1 )
	 };

  if(defined($opt->{seed})) {
    $me->{seed} = $opt->{seed} 
  } else {
    my($s) = crypt(time,reverse(time));
    my($a) = 0;
    my($i);
    for $i(0..7) { ($a *= 64) += ord(substr($s,$i+3,1));}
    $me->{seed} = $a;
  }

  $me->{range} = ( (defined ($opt->{range})) 
		   ? ($opt->{range}) 
		   : (pdl(0,1)->dummy(1,$me->{size})) );

  croak('PDL::RandVar::new: must specify at least 2-element pdl as range!\n')
    if($me->{range}->nelem < 2);

  $me->{range} = $me->{range}->dummy(1,$me->{size})
    if($me->{range}->ndims < 2);

  $me->{start} = $me->{range}->slice("(0),:");
  $me->{scale} = $me->{range}->slice("(1),:") - $me->{start};
  $me->{opt} = {%$opt} if(ref $opt eq 'HASH');
  
  return bless($me,$type);
}


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

sub PDL::RandVar::sample {
  my($me,$n,$out) = @_;

  $n = 1 unless(defined $n);

  $out = ($me->{size}>1) ? zeroes($n,$me->{size}) : zeroes($n)
    unless defined($out);


  my($i);
  if($me->{size}>1) {
    my($j);
    for $i(0..$n-1) {
      for $j(0..$me->{size}-1) {
	  my($fmh) = $out->slice("($i),($j)");
	  $fmh .= rand;
      }
      
      my($fmh) = ($out->slice("($i),:"));
      $fmh *= $me->{scale};
      $fmh += $me->{start};
    }
  } else {
    for $i(0..$n-1) {
      my($fmh) = $out->slice("$i");
      $fmh .= rand;
    }
    ($out *= $me->{scale}) += $me->{start};
  }
  
  return $out;
}




1;
  
  
  
                      






