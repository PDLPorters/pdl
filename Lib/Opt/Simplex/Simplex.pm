=head1 NAME

PDL::Opt::Simplex -- Simplex optimization routines

=head1 SYNOPSIS

 use PDL::Opt::Simplex;

 ($optimum,$ssize) = simplex($init,$initsize,$minsize,
 		 $maxiter,
 		 sub {evaluate_func_at($_[0])},
 		 sub {display_simplex($_[0])}
 		 );

=head1 DESCRIPTION

This package implements the commonly used simplex optimization
algorithm. The basic idea of the algorithm is to move
a "simplex" of N+1 points in the N-dimensional search space
according to certain rules. The main
benefit of the algorithm is that you do not need to calculate
the derivatives of your function. 

$init is a 1D vector holding the initial values of the N fitted
parameters, $optimum is a vector holding the final solution.

$initsize is the size of $init (more...)

$minsize is some sort of convergence criterion (more...)
- e.g. $minsize = 1e-6

The sub is assumed to understand more than 1 dimensions and threading.
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

 ($optimum,$ssize) = simplex($init,$initsize,$minsize,
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

Ron Shaffer's chemometrics web page and references therein:
C<http://chem1.nrl.navy.mil/~shaffer/chemoweb.html>.

Numerical Recipes (bla bla bla XXX ref).

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
use PDL;
use PDL::Primitive;
use strict;
use PDL::Exporter;
# use AutoLoader;

@PDL::Opt::Simplex::ISA = qw/PDL::Exporter/;

@PDL::Opt::Simplex::EXPORT_OK = qw/simplex/;
%PDL::Opt::Simplex::EXPORT_TAGS = (Func=>[@PDL::Opt::Simplex::EXPORT_OK]);

*simplex = \&PDL::simplex;
sub PDL::simplex {
	my($init,$initsize,$minsize,$maxiter,$sub,$logsub,$t) = @_;
	if(!defined $t) {$t = 0}
	my ($i,$j); my $nd = $init->getdim(0);
	my $simp = PDL->zeroes($nd,$nd+1);
	$simp .= $init;
# Constructing a tetrahedron:
# At step n (starting from zero)
# take vertices 0..n and move them 1/(n+1) to negative dir on axis n.
# Take vertex n+1 and move it n/(n+1) to positive dir on axis n 
	if(!ref $initsize) {
		$initsize = PDL->pdl($initsize)->dummy(0,$nd);
	}
	for($i=0; $i<$nd; $i++) {
		my $pj = $i/($i+1);
		(my $stoopid = $simp->slice("$i,0:$i"))
				  -= $initsize->at($i) * $pj;
		(my $stoopid1 = $simp->slice("$i,".($i+1)))
				  += $initsize->at($i) * (1-$pj);
	}
	my $maxind = PDL->null;
	my $minind = PDL->zeroes(2);;
	my $ssum = PDL->null;
	my $worst;
	my $new;
	my $realnew;
	my $vals = &{$sub}($simp);
	my $ssize = $initsize->min*2;
	&{$logsub}($simp,$vals,$ssize)
		if $logsub;
	while($maxiter-- and max(PDL->topdl($ssize)) > $minsize ) {
		my $valsn = $vals;
		if($t) {
			my $noise = $vals->copy();
			$noise->random;
			$valsn = $vals + $t*(-log($noise+0.00001));
		}
		maximum_ind($valsn,$maxind);
		minimum_n_ind($valsn,$minind);
		my $worstval = ($valsn->at("$maxind"));
		my @bestvals = map {$valsn->at($minind->at($_))} 0..1;
		
		sumover($simp->xchg(0,1),$ssum);
		$ssum -= ($worst = $simp->slice(":,($maxind)"));
		$ssum /= $nd;
		$new = 2*$ssum - $worst;
		my $valv = &{$sub}($new);
		my $val = $valv->at();
		if($t) {
			$val = $val - $t*(-log(rand()+0.00001));
		}
		if(($val) < $bestvals[0]) {
			my $newnew = $new + $ssum-$worst;
			my $val2 = &{$sub}($newnew);
			if($val2->at() < $val) {
#				print "CASE1\n";
				$realnew = $newnew;
			} else {
#				print "CASE2, $newnew, $val, $val2\n";
				$realnew = $new;
			}
		} elsif($val < $bestvals[1]) {
#			print "CASE3\n";
			$realnew = $new;
		} elsif($val < $worstval) {
#			print "CASE4\n";
			$realnew = 1.5*$ssum-0.5*$worst;
		} else {
#			print "CASE5\n";
			$realnew = 0.5*$ssum+0.5*$worst;
		}
		$worst .= $realnew;
		(my $stoopid2= $vals->slice("($maxind)")) .= &{$sub}($worst);
		my $ss1 = ($simp - $simp->slice(":,0"))**2;
		sumover($ss1,(my $ss2=PDL->null));
		$ssize = PDL::max(sqrt($ss2));
		&{$logsub}($simp,$vals,$ssize)
			if $logsub;
	}
	minimum_ind($vals,(my $mmind=PDL->null));
	return ($simp->slice(":,$mmind"),$ssize);
}


