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
use strict;
use warnings;
use base qw/PDL::Graphics::TriD::GObject/;
use OpenGL qw(:all);
use PDL::Graphics::OpenGL::Perl::OpenGL;

sub gdraw {
	my($this,$points) = @_;
	glDisable(&GL_LIGHTING);
	glColor3d(@{$this->{Options}{Color}});
	PDL::Graphics::OpenGLQ::gl_arrows($points,@{$this->{Options}}{qw(From To
		ArrowLen ArrowWidth)});
	glEnable(&GL_LIGHTING);
}

sub get_valid_options {
	return {UseDefcols => 0,From => [],To => [],Color => [1,1,1],
		ArrowWidth => 0.02, ArrowLen => 0.1}
}

package PDL::GraphEvolver;
use PDL::Lite;
use PDL::Graphics::TriD::Rout ":Func";

sub new {
	my($type,$coords) = @_;
	bless {Coords => $coords,
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
	my($this) = @_;
	my $c = $this->{Coords};
	my $velr = repulse($c,@$this{qw(BoxSize DMult A B C D)});
	my $vela = attract($c,@$this{qw(From To Strength M MS)});
	my $tst = 0.10;
	$this->{Velo} = ($this->{Velo}//0) + $tst * 0.02 * ($velr + $vela);
	$this->{Velo} *=
	  (0.92*50/(50+$this->{Velo}->magnover->dummy(0)))**$tst;
	$c += $tst * 0.05 * $this->{Velo};
	(my $tmp = $c->transpose->index($this->{FInd}->dummy(0)))
		.= $this->{FCoord}
			if (defined $this->{FInd});
	print "C: $c\n" if $PDL::Graphics::TriD::verbose;
}

sub getcoords {return $_[0]{Coords}}

1;
