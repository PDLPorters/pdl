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
    # The parabola (x+3)^2 - 5 has a minima at x=-3:
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

  my $vec_initial = pdl [30];
  my ( $vec_optimal, $ssize, $optval ) = simplex($vec_initial, 3, 1e-6, 100, \&f, \&log);
  my $x = $vec_optimal->slice('(0)');
  print "ssize=$ssize  opt=$x -> minima=$optval\n";

=head1 DESCRIPTION

This package implements the commonly used simplex optimization
algorithm. The basic idea of the algorithm is to move
a "simplex" of N+1 points in the N-dimensional search space
according to certain rules. The main
benefit of the algorithm is that you do not need to calculate
the derivatives of your function. 

$init is a 1D vector holding the initial values of the N fitted
parameters, $optimum is a vector holding the final solution.
$optval is the evaluation of the final solution.

$initsize is the size of $init (more...)

$minsize is some sort of convergence criterion (more...)
- e.g. $minsize = 1e-6

The sub is assumed to understand more than 1 dimensions and broadcasting.
Its signature is 'inp(nparams); [ret]out()'. An example would be

	sub evaluate_func_at {
		my($xv) = @_;
		my $x1 = $xv->slice("(0)");
		my $x2 = $xv->slice("(1)");
		return $x1**4 + ($x2-5)**4 + $x1*$x2;
	}

Here $xv is a vector holding the current values of the parameters
being fitted which are then sliced out explicitly as $x1 and $x2.

$ssize gives a very very approximate estimate of how close we might
be - it might be miles wrong. It is the euclidean distance between
the best and the worst vertices. If it is not very small, the algorithm
has not converged.

=head1 FUNCTIONS

=head2 simplex

=for ref

Simplex optimization routine

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

package PDL::Opt::Simplex;
use strict;
use warnings;
use PDL;
use PDL::Primitive;
use PDL::Exporter;

# use AutoLoader;

@PDL::Opt::Simplex::ISA = qw/PDL::Exporter/;

@PDL::Opt::Simplex::EXPORT_OK = qw/simplex/;
%PDL::Opt::Simplex::EXPORT_TAGS = ( Func => [@PDL::Opt::Simplex::EXPORT_OK] );

*simplex = \&PDL::simplex;

sub PDL::simplex {
    my ( $init, $initsize, $minsize, $maxiter, $sub, $logsub, $t ) = @_;
    if ( !defined $t ) { $t = 0 }
    my ( $i, $j );
    my ( $nd, $nd2 ) = ( dims($init), 1 );
    my $simp;
    if ( $nd2 == 1 ) {
        $simp = PDL->zeroes( $nd, $nd + 1 );
        $simp .= $init;

        # Constructing a tetrahedron:
        # At step n (starting from zero)
        # take vertices 0..n and move them 1/(n+1) to negative dir on axis n.
        # Take vertex n+1 and move it n/(n+1) to positive dir on axis n
        if ( !ref $initsize ) {
            $initsize = PDL->pdl($initsize)->dummy( 0, $nd );
        }
        for ( $i = 0 ; $i < $nd ; $i++ ) {
            my $pj = $i / ( $i + 1 );
            ( my $stoopid = $simp->slice("$i,0:$i") ) -=
              $initsize->at($i) * $pj;
            ( my $stoopid1 = $simp->slice( "$i," . ( $i + 1 ) ) ) +=
              $initsize->at($i) * ( 1 - $pj );
        }
    }
    elsif ( $nd2 == $nd + 1 ) {
        $simp = $init;
    }
    else {
        return;
    }
    my $maxind = PDL->zeroes(2);
    my $minind = PDL->null;
    my $ssum   = PDL->null;
    my $worst;
    my $new;
    my $vals = &{$sub}($simp);
    my $ss1  = ( $simp - $simp->slice(":,0") )**2;
    sumover( $ss1, ( my $ss2 = PDL->null ) );
    my $ssize = PDL::max( sqrt($ss2) );
    &{$logsub}( $simp, $vals, $ssize )
      if $logsub;

    while ( $maxiter-- and max( PDL->topdl($ssize) ) > $minsize ) {
        my $valsn = $vals;
        if ($t) {
            my $noise = $vals->random();
            $noise->random;
            $valsn = $vals + $t * ( -log( $noise + 0.00001 ) );
        }
        maximum_n_ind( $valsn, $maxind );
        minimum_ind( $valsn, $minind );
        my @worstvals = map { $valsn->at( $maxind->at($_) ) } 0 .. 1;
        my $bestval = $valsn->at($minind);

        sumover( $simp->xchg( 0, 1 ), $ssum );
        $ssum -= ( $worst = $simp->slice( ":,(" . $maxind->at(0) . ")" ) );
        $ssum /= $nd;
        $new = 2 * $ssum - $worst;
        my $val = ( &{$sub}($new) )->at(0);
        if ($t) {
            $val = $val - $t * ( -log( rand() + 0.00001 ) );
        }
        my $removetop = 0;
        if ( $val < $bestval ) {
            my $newnew = $new + $ssum - $worst;
            my $val2   = &{$sub}($newnew);
            if ( $val2->at(0) < $val ) {
#                print "CASE1 Reflection and Expansion\n";
                $worst .= $newnew;
                $val = $val2;
            }
            else {
#                print "CASE2 Reflection, $newnew, $val, $val2\n";
                $worst .= $new;
            }
            $removetop = 1;
        }
        elsif ( $val < $worstvals[1] ) {
#            print "CASE3 Reflection\n";
            $worst .= $new;
            $removetop = 1;
        }
        else {
            my $newnew = 0.5 * $ssum + 0.5 * $worst;
            my $val2   = &{$sub}($newnew);
            if ( $val2->at(0) < $worstvals[0] ) {
#                print "CASE4 Contraction, $newnew, $val, $val2\n";
                $worst .= $newnew;
                $val = $val2;
                $removetop = 1;
            }
        }
        if ($removetop) {
            ( my $stoopid = $vals->slice( "(" . $maxind->at(0) . ")" ) ) .= $val;
        }
        else {
#            print "CASE5 Multiple Contraction\n";
            $simp = 0.5 * $simp->slice(":,$minind") + 0.5 * $simp;
            my $idx = which( sequence($nd+1) != $minind );
            ( my $stoopid = $vals->index($idx) ) .= &{$sub}($simp->dice_axis(1,$idx));
        }
        my $ss1 = ( $simp - $simp->slice(":,0") )**2;
        sumover( $ss1, ( $ss2 = PDL->null ) );
        $ssize = PDL::max( sqrt($ss2) );
        &{$logsub}( $simp, $vals, $ssize )
          if $logsub;
    }
    minimum_ind( $vals, ( my $mmind = PDL->null ) );
    return ( $simp->slice(":,$mmind"), $ssize, $vals->index($mmind) );
}

1;
