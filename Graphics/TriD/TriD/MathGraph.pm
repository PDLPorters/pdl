=head1 NAME

PDL::Graphics::TriD::MathGraph -- Mathematical Graph objects for PDL

=head1 SYNOPSIS

see the file Demos/TriD/tmathgraph.p in the PDL distribution.

=head1 WARNING

This module is experimental and the interface will probably change.

=head1 DESCRIPTION

This module exists for plotting mathematical graphs (consisting
of nodes and arcs between them) in 3D and optimizing the placement of
the nodes so that the graph is visualizable in a clear way.

=head1 AUTHOR

Copyright (C) 1997 Tuomas J. Lukka (lukka@husc.harvard.edu).

All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut
package PDL::Graphics::TriD::MathGraph;
use base qw/PDL::Graphics::TriD::GObject/;
use fields qw/ArrowLen ArrowWidth/;
use PDL::Graphics::OpenGL;

sub gdraw {
	my($this,$points) = @_;
	glDisable(&GL_LIGHTING);
# 	print "Color: $this->{Color} @{$this->{Color}}\n";
	glColor3d(@{$this->{Options}{Color}});
	PDL::Graphics::OpenGLQ::gl_arrows($points,$this->{Options}{From},
		$this->{Options}{To},$this->{ArrowLen},$this->{ArrowWidth});
	glEnable(&GL_LIGHTING);
}

sub get_valid_options {
	return {UseDefcols => 0,From => [],To => [],Color => [1,1,1],
		ArrowWidth => 0.05, ArrowLen => 0.1}
}

package PDL::GraphEvolverOLD;
use PDL::LiteF;

sub new {
	my($type,$nnodes) = @_;
       bless {NNodes => $nnodes,Coords => 500*PDL::random(PDL->zeroes(3,$nnodes))},
         $type;
}

sub set_links {
	my($this,$from,$to,$strength) = @_;
	my $cd = $this->{NNodes};
	$this->{DistMult} = PDL->zeroes($cd,$cd);
	$distmult = PDL->zeroes($cd,$cd);
	(my $t1 = $this->{DistMult}->index2d($from,$to)) += $strength;
	(my $t2 = $this->{DistMult}->index2d($to,$from)) += $strength;
	print "DM: $distmult\n" if $verbose;
}

sub set_distmult {
	my($this,$mat) = @_;
	$this->{DistMult} = $mat;
}

sub set_fixed {
	my($this,$ind,$coord) = @_;
	$this->{FInd} = $ind; $this->{FCoord} = $coord;
}

sub step {
#	$verbose=1;
	my($this) = @_;
	my $c = $this->{Coords};
	my $vecs = $c - $c->dummy(1);
	my $dists = sqrt(($vecs**2)->sumover)+0.0001;
						print "D: $dists\n" if $verbose;
	(my $t1 = $dists->diagonal(0,1)) .= 1000000;
	my $d2 = $dists ** -0.5; # inverse
	my $m = $d2**4 - 2*($this->{DistMult})*($dists+5*$dists**2) + 0.00001
		- 0.000001 * $dists;
						print "DN: $m\n" if $verbose;
						print "V: $vecs\n" if $verbose;
	my $tst = 1;
	$this->{Velo} -= $tst * 0.04 * (inner($m->dummy(1), $vecs->mv(1,0)));
	$this->{Velo} *=
	  ((0.96*50/(50+sqrt(($this->{Velo}**2)->sumover->dummy(0)))))**$tst;
	$c += $tst * 0.05 * $this->{Velo};
	(my $tmp = $c->xchg(0,1)->index($this->{FInd}->dummy(0)))
		.= $this->{FCoord}
			if (defined $this->{FInd});
						print "C: $c\n" if $verbose;
}

sub getcoords {return $_[0]{Coords}}

package PDL::GraphEvolver;
use PDL::Lite;
use PDL::Graphics::TriD::Rout ":Func";

sub new {
	my($type,$nnodes) = @_;
       bless {NNodes => $nnodes,Coords => PDL::random(PDL->zeroes(3,$nnodes)),
		BoxSize => 3, DMult => 5000,
		A => -100.0, B => -5, C => -0.1, D => 0.01,
		M => 30, MS => 1,
		},$type;
}

sub set_links {
	my($this,$from,$to,$strength) = @_;
	$this->{From} = $from;
	$this->{To} = $to;
	$this->{Strength} = $strength;
}

sub set_fixed {
	my($this,$ind,$coord) = @_;
	$this->{FInd} = $ind; $this->{FCoord} = $coord;
}

sub step {
#	$verbose=1;
	my($this) = @_;
	my $c = $this->{Coords};
	my $velr = repulse($c,@{$this}{BoxSize,DMult,A,B,C,D});
	my $vela;
	if("ARRAY" eq ref $this->{From}) {
		my $ind;
		for $_ (0..$#{$this->{From}}) {
		   $vela += attract($c,
		   	$this->{From}[$_],
		   	$this->{To}[$_],
		   	$this->{Strength}[$_],$this->{M},$this->{MS});
		}
	} else {
		$vela = attract($c,@{$this}{From,To,Strength},$this->{M},
			$this->{MS});
	}

#	print "V: $velr $vela\n";

	$tst = 0.10;
	$this->{Velo} += $tst * 0.02 * ($velr + $vela);
	$this->{Velo} *=
	  ((0.92*50/(50+sqrt(($this->{Velo}**2)->sumover->dummy(0)))))**$tst;
	$c += $tst * 0.05 * $this->{Velo};
	(my $tmp = $c->xchg(0,1)->index($this->{FInd}->dummy(0)))
		.= $this->{FCoord}
			if (defined $this->{FInd});
						print "C: $c\n" if $verbose;
}

sub getcoords {return $_[0]{Coords}}

1;

1;
