=head1 NAME

PDL::Graphics::TriD::Labels -- Text tools

=head1 SYNOPSIS

  my $l = new PDL::Graphics::TriD::Labels($lablepoints,
					  {Strings=>$strlist
					   ,Font=>$font});


=head1 WARNING

This module is experimental and the interface will probably change.

=head1 DESCRIPTION

This module is used to write Labels on the graphs of TriD

=head1 AUTHOR

Copyright (C) 1997 Tuomas J. Lukka (lukka@husc.harvard.edu).
              2000 James P.  Edwards (jedwards@inmet.gov.br)
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut
package PDL::Graphics::TriD::Labels;
use PDL::Graphics::OpenGL;
use PDL::Graphics::OpenGLQ;
use base qw/PDL::Graphics::TriD::GObject/;

sub gdraw {
	my($this,$points) = @_;
	glDisable(&GL_LIGHTING);
	glColor3d(1,1,1);
	PDL::Graphics::OpenGLQ::gl_texts($points,$this->{Options}{Font},$this->{Options}{Strings});
	glEnable(&GL_LIGHTING);
}

sub get_valid_options {
  return {UseDefcols => 0, Font=>$PDL::Graphics::TriD::GL::fontbase, Strings => [] }
}


1;
