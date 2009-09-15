package PDL::Graphics::OpenGL::Perl::OpenGL;

use warnings;
use strict;

=head1 NAME

PDL::Graphics::OpenGL::Perl::OpenGL - PDL TriD OpenGL interface using POGL

=head1 VERSION

Version 0.01_07

=cut

our $VERSION = '0.01_07';


=head1 SYNOPSIS

This module provides the glue between the Perl
OpenGL functions and the API defined by the internal
PDL::Graphics::OpenGL one. It also supports any
miscellaneous OpenGL or GUI related functionality to
support PDL::Graphics::TriD refactoring.

You should be able to replace:

    use PDL::Graphics::OpenGL
by
    use PDL::Graphics::OpenGL::Perl::OpenGL;

This module also includes support for FreeGLUT and
GLUT instead of X11+GLX as mechanism for creating
windows and graphics contexts.

=head1 EXPORT

See the documentation for the OpenGL module.
More details to follow as the refactored TriD module
interface and build environment matures


=head1 FUNCTIONS

=head2 TBD

=cut


=head2 TBD

=cut


=head1 AUTHOR

Chris Marshall, C<< <devel dot chm dot 01 at gmail.com> >>

=head1 BUGS

Bugs and feature requests may be submitted through the PDL sourceforge
project page at L<http://sourceforge.net/tracker/?group_id=612> .


=head1 SUPPORT

PDL uses a mailing list support model.  The Perldl mailing list
is the best for questions, problems, and feature discussions with
other PDL users and PDL developers.

To subscribe see the page at L<http://mailman.jach.hawaii.edu/mailman/listinfo/perldl>



=head1 ACKNOWLEDGEMENTS

TBD including PDL TriD developers and POGL developers...thanks to all.

=head1 COPYRIGHT & LICENSE

Copyright 2009 Chris Marshall.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of PDL::Graphics::OpenGL::Perl::OpenGL
