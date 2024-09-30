package PDL::Opt::Simplex;
use strict;
use warnings;
use PDL;
use PDL::Exporter;

our @ISA = qw/PDL::Exporter/;
our @EXPORT_OK = qw/simplex make_simplex/;
our %EXPORT_TAGS = ( Func => [@EXPORT_OK] );

*simplex = \&PDL::simplex;

sub make_simplex {
  my ($init, $initsize) = @_;
  my ( $nd, $nd2, @otherdims ) = ( $init->dims, 1 );
  pop @otherdims if @otherdims; # drop the spurious "1"
  return unless $nd2 == $nd + 1 or $nd2 == 1;
  return $init if $nd2 == $nd + 1;
  my $simp = PDL->zeroes( $nd, $nd + 1, @otherdims );
  # Constructing a tetrahedron:
  # At step n (starting from zero)
  # take vertices 0..n and move them 1/(n+1) to negative dir on axis n.
  # Take vertex n+1 and move it n/(n+1) to positive dir on axis n
  my $uppertri = $simp->copy;
  ones($nd * ($nd+1) / 2)->tritosquare($uppertri->slice(",:-2")->t);
  my $iseq = sequence($nd);
  my $pjseq = $iseq / ( $iseq + 1 );
  $simp .= $init - $initsize * $pjseq * $uppertri;
  $simp->slice(",1:")->diagonal(0,1) += $initsize * (1-$pjseq);
  $simp;
}

sub PDL::simplex {
  my ( $init, $initsize, $minsize, $maxiter, $sub, $logsub, $t ) = @_;
  my $simp = make_simplex($init, $initsize) // return;
  my $nd = $simp->dim(0);
  my $vals = $sub->($simp);
  my $ssize;
  while ($maxiter--) {
    $ssize = ( $simp - $simp->slice(":,0") )->magnover->maxover;
    $logsub->( $simp, $vals, $ssize ) if $logsub;
    last unless $ssize > $minsize;
    my $valsn = !$t ? $vals : $vals - $t * log( $vals->random + 0.00001 );
    my $minind = $valsn->minimum_ind;
    my $maxind = $valsn->maxover_n_ind(2);
    my $maxind0 = $maxind->at(0);
    my @worstvals = $valsn->index($maxind)->list;
    my $bestval = $valsn->at($minind);
    my $ssum = ($simp->t->sumover - (my $worst = $simp->slice(":,($maxind0)"))) / $nd;
    my $new = 2 * $ssum - $worst;
    my $val = $sub->($new)->at(0);
    $val += $t * log( rand() + 0.00001 ) if $t;
    my $removetop = 0;
    if ( $val < $bestval ) {
      my $newnew = $new + $ssum - $worst;
      my $val2   = $sub->($newnew);
      if ( $val2->at(0) < $val ) { # CASE1 Reflection and Expansion
        $new = $newnew;
        $val = $val2;
      } # else CASE2 Reflection
      $removetop = 1;
    } elsif ( $val < $worstvals[1] ) { # CASE3 Reflection
      $removetop = 1;
    } else {
      my $newnew = 0.5 * ($ssum + $worst);
      my $val2   = $sub->($newnew);
      if ( $val2->at(0) < $worstvals[0] ) { # CASE4 Contraction
        $new = $newnew;
        $val = $val2;
        $removetop = 1;
      }
    }
    if ($removetop) {
      $worst .= $new;
      $vals->slice( "($maxind0)" ) .= $val;
    } else { # CASE5 Multiple Contraction
      $simp .= 0.5 * ($simp->slice(":,$minind") + $simp);
      my $idx = which( sequence($nd+1) != $minind );
      $vals->index($idx) .= $sub->($simp->dice_axis(1,$idx));
    }
  }
  my $mmind = $vals->minimum_ind;
  return ( $simp->slice(":,$mmind"), $ssize, $vals->index($mmind) );
}

1;

__END__

=head1 NAME

PDL::Opt::Simplex -- Simplex optimization routines

=head1 SYNOPSIS

  use PDL::Opt::Simplex;

  ($optimum,$ssize,$optval) = simplex($init,$initsize,$minsize,
 		 $maxiter,
 		 sub {evaluate_func_at($_[0])},
 		 sub {display_simplex($_[0])}
 		 );

  # more involved:
  use PDL;
  use PDL::Opt::Simplex;
  my $count = 0;
  # find value of $x that returns a minimum
  sub f {
    my ($vec) = @_;
    $count++;
    my $x = $vec->slice('(0)');
    # The parabola (x+3)^2 - 5 has a minimum at x=-3:
    return (($x+3)**2 - 5);
  }
  sub log {
    my ($vec, $vals, $ssize) = @_;
    # $vec is the array of values being optimized
    # $vals is f($vec)
    # $ssize is the simplex size, or roughly, how close to being converged.
    my $x = $vec->slice('(0)');
    # each vector element passed to log() has a min and max value.
    # ie: x=[6 0] -> vals=[76 4]
    # so, from above: f(6) == 76 and f(0) == 4
    print "$count [$ssize]: $x -> $vals\n";
  }
  my ($optimum, $ssize, $optval) = simplex(pdl(30), 3, 1e-6, 100, \&f, \&log);
  print "ssize=$ssize  opt=$optimum -> minimum=$optval\n";

=head1 DESCRIPTION

This package implements the commonly used simplex optimization
algorithm. The basic idea of the algorithm is to move
a "simplex" of N+1 points in the N-dimensional search space
according to certain rules. The main
benefit of the algorithm is that you do not need to calculate
the derivatives of your function. 

C<$init> is a 1D vector holding the initial values of the N fitted
parameters, C<$optimum> is a vector holding the final values.
C<$optval> is the evaluation of the final values.

C<$initsize> is the size of C<$init>. It is only used if your supplied
C<$init> is a single point in your search space, to construct the simplex
("cloud") of N+1 points the algorithm uses, being the distance away from
your single C<$init> point along each dimension. This is done by the
exportable function C<make_simplex($init, $initsize)>, e.g.:

  pdl> use PDL::Opt::Simplex
  pdl> p $t = make_simplex(pdl(0,0,0), pdl(0.12,0.12,0.12))
  [
   [    0 -0.06 -0.08]
   [ 0.12 -0.06 -0.08]
   [    0  0.06 -0.08]
   [    0     0  0.04]
  ]
  pdl> use PDL::Graphics::TriD
  pdl> spheres3d $t # spheres not points so can easily see

C<$minsize> is the convergence criterion, e.g. C<$minsize> = 1e-6;
the algorithm will terminate when all the values of C<$ssize> are less
than C<$minsize>.

The sub is assumed to understand more than 1 dimensions and broadcasting.
Its signature is C<inp(nparams); [ret]out()>. An example would be

	sub evaluate_func_at {
		my($xv) = @_;
		my ($x1, $x2) = $xv->using(0,1);
		return $x1**4 + ($x2-5)**4 + $x1*$x2;
	}

Here C<$xv> is a vector holding the current values of the parameters
being fitted which are then sliced out explicitly as C<$x1> and C<$x2>.

C<$ssize> gives a very very approximate estimate of how close we might
be - it might be miles wrong. It is the largest Euclidean distance between
the first vertex and any other. If it is not very small, the algorithm
has not converged.

=head1 FUNCTIONS

=head2 simplex

=for ref

Simplex optimization routine

Mutates its C<$init> input if given as a full simplex (dims C<n,n+1>).

=for usage

 ($optimum,$ssize,$optval) = simplex($init,$initsize,$minsize,
 		 $maxiter,
 		 sub {evaluate_func_at($_[0])},
 		 sub {display_simplex($_[0])}
 		 );

See module C<PDL::Opt::Simplex> for more information.

=head1 CAVEATS

Do not use the simplex method if your function has local minima.
It will not work. Use genetic algorithms or simulated annealing
or conjugate gradient or momentum gradient descent.

They will not really work either but they are not guaranteed not to work ;)
(if you have infinite time, simulated annealing is guaranteed to work
but only after it has visited every point in your space).

=head1 SEE ALSO

=over

=item L<PDL::Opt::Simplex::Simple> - Use names for Simplex-optimized values

=item L<PDL::Opt::ParticleSwarm> - A PDL implementation of Particle Swarm

=item L<PDL::Opt::ParticleSwarm::Simple> - Use names for Particle Swarm-optimized values

=item L<https://web.archive.org/web/19981206200518/http://chem1.nrl.navy.mil/~shaffer/chemoweb.html> - Ron Shaffer's chemometrics web page and references therein (archive from 1998)

=back

The demonstration (Examples/Simplex/tsimp.pl and tsimp2.pl).

=head1 AUTHOR

Copyright(C) 1997 Tuomas J. Lukka. 
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL 
distribution. If this file is separated from the PDL distribution, 
the copyright notice should be included in the file.

=cut
