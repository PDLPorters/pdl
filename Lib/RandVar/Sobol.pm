=head1 NAME

PDL::RandVar::Sobol -- Sobol subrandom sequences

=head1 VERSION

  Current version is 1.0

=head1 SYNOPSIS

  use PDL::RandVar::Sobol;
  $m = new PDL::RandVar::Sobol

=head1 DESCRIPTION

Sobol subrandom sequences are used for more uniform sampling of a domain than normal
random variables can achieve.  They're described cursorily in W.H. Press's _Numerical_
_Recipes_, 2nd edition, section 7.7 (Cambridge Univ. Press), and this implementation
is based on their description.  

=head1 History

  0.01    4-Dec-2001 -- Basic functionality (CED)
  0.1    11-Dec-2001 -- First fully functional version (CED)
  1.0     9-Jan-2002 -- Tests out OK (CED)

=head2 Author, license, no warranty

This file copyright(C) 2001, 2002 Craig DeForest
(cdeforest@solar.stanford.edu).  This software/documentation may be
distributed under the same terms as PDL itself (license available at
http://pdl.perl.org). This package comes with NO WARRANTY.

=head2 Bugs:

Should have a way of reproducibly seeding the initial values.  For now,
you always get the same initial seed with every random variable -- this is
the Wrong way to do it.

=head1 FUNCTIONS

=cut


@ISA = ('PDL::RandVar');
package PDL::RandVar::Sobol;
use PDL;
use Carp;

BEGIN {
package PDL::RandVar::Sobol;
$VERSION = 1.0;
@ISA = ('PDL::RandVar');
  @polydim = 
  ( undef  #spacer to make the loop work better
    ,[0]                  #1
    ,[1]                  #2
    ,[1,2]                #3
    ,[1,4]                #4
    ,[2,4,7,11,13,14]     #5
    ,[1,13,16,19,22,25]   #6
    ,[1,4,7,8,14,19,21,27,31,32,37,41,42,50,55,56,59,62]
    ,[14,21,22,38,47,49,50,52,56,67,70,84,97,103,115,122]
    ,[8,13,16,22,25,44,47,52,55,59,62,67,74,81,82,87,91,94,103,104,109,122,124
      ,137,138,143,145,152,157,167,1733,176,181,182,185,191,194,199,218,220,227
      ,229,230,234,236,241,244,253]
    ,[4,13,19,22,50,55,64,69,98,107,115,121,127,134,140,145,152,158,161,171,181
      ,194,199,203,208,227,242,251,253,265,266,274,283,289,295,301,316,319,324
      ,346,352,361,367,382,395,398,400,412,419,422,426,428,433,446,454,457,472
      ,493,505,508]
    );
  
  my($i,$j,$n);
  for($i=1;$i<@polydim;$i++) {
    $n += @{$polydim[$i]};
    push(@poly,@{$polydim[$i]});

    for $j(1..@{$polydim[$i]}) {
      push(@mdeg,$i);
    }

  }
  
  @iv = ([1,3,5,15]
	,[1,1,7,11]
	,[1,3,7,5],
	,[1,3,3,15]
	,[1,1,3,13]
	,[1,1,5,9]);


 $MAXBIT = 30;
 $MAXDIM = 6;
}

use strict;

######################################################################
=pod

=head2 new

=for ref

Construct a new Sobol subrandom variable

=for sig

  Signature: (See PDL::RandVar::new)

=for usage
 
  $a = new PDL::RandVar::Sobol(<size>,<opt>);

Options:

=over 3

=item range

2xn piddle containing min and max values for each dimension.

=back

=for example

  $a = new PDL::RandVar::Sobol(2,{range=>pdl([$xmin,$xmax],[$ymin,$ymax])});
  $xy = sample $a;

=cut

sub PDL::RandVar::Sobol::new {
  my($opt);
    for(my $i=0;$i<@_;$i++) {
    if(ref $_[$i] eq 'HASH') {
      $opt = splice(@_,$i,1);
      last;
    }
  }
  my($type,$size) = @_;
  my($me) = &PDL::RandVar::new(@_);

  my($dim) = $size || $opt->{dim} || 1;

  if($dim > $PDL::RandVar::Sobol::MAXDIM) {
    croak "PDL::RandVar::Sobol currently doesn't support more than $PDL::RandVar::Sobol::MAXDIM simultaneous dimensions (asked for $dim).\n";
  }


  # Following code assigns polynomials in order to each dimension.  Could also
  # mix 'em up a bit -- that involves changes on lines that include '##'.

  $me->{dim} = $dim;
  $me->{in} = 0;
  $me->{ix} = zeroes($dim);
  $me->{iv} = zeroes($PDL::RandVar::Sobol::MAXBIT,$dim);
  $me->{fac} = 1.0 / (2<<$PDL::RandVar::Sobol::MAXBIT);
  $me->{poly} = [@PDL::RandVar::Sobol::poly[0..$dim-1]];  ##
  $me->{mdeg} = [@PDL::RandVar::Sobol::mdeg[0..$dim-1]];  ##

  my($j,$k,$l,$i);
  for($k=0;$k<$me->{dim};$k++) {
    # Copy initial values from the class variable into this value
    my($fmh) = $me->{iv}->slice(sprintf("0:%d,%d",$#{$PDL::RandVar::Sobol::iv[$k]},$k));
    $fmh .=      pdl($PDL::RandVar::Sobol::iv[$k])                               ##
	<< (($PDL::RandVar::Sobol::MAXBIT - xvals(scalar(@{$PDL::RandVar::Sobol::iv[$k]})))); ##
    
    # Iterate to get MAXBITS values in iv
    for ($j=$me->{mdeg}->[$k]; $j < $PDL::RandVar::Sobol::MAXBIT; $j++) {

      my($ipp) = $me->{poly}[$k]; 

      my($i) = $me->{iv}->slice(sprintf("%d,%d",$j - $me->{mdeg}->[$k], $k));

      # Exclusive-OR with highest-order term (implicit)
      $i ^= ($i >> $me->{mdeg}->[$k]);

      # Loop over lower-order terms in the polynomial.
      for($l=$me->{mdeg}->[$k]-1;$l>=0;$l--) {
	$i ^= ($me->{iv}->slice($j-$l)) if($ipp & 1);
	$ipp >>= 1;
      }

      my($fmsh) =  $me->{iv}->slice("$j,$k");
      $fmsh .= $i;
    }

  } # end of cross-dimension initialization loop 

  return $me;
}


sub PDL::RandVar::Sobol::sample() {
  my($me,$n,$out) = @_;

  $n=1 unless(defined $n);
  $out = ($me->{dim}>1) ? zeroes($me->{dim},$n) : zeroes($n)
    unless defined($out);
  my($o) = ($out->getndims == 1) ? $out->dummy(0,1) : $out;
  
  my($i,$j,$k);


  # Find number of rightmost zero bit
  for($i=0;$i<$n;$i++) {
    my($im) = ($me->{in})++;

    for($j=0;($im & 1) && $j<$PDL::RandVar::Sobol::MAXBIT;$j++) {$im >>= 1;}
    croak("Randomness overflow in PDL::RandVar::Sobol::sample\n")
      if($j>=$PDL::RandVar::Sobol::MAXBIT);
    
    # XOR with appropriate direction variable
    my $fmh = $o->slice(":,($i)");
    $fmh .= ($me->{ix} ^= $me->{iv}->slice("($j),:"));
  }
	   
  ($o *= ($me->{fac} * $me->{scale})) += $me->{start};

  return $out;
}

1;
