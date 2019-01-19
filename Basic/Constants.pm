=head1 NAME

PDL::Constants -- basic compile time constants for PDL

=head1 DESCRIPTION

This module is used to define compile time constant
values for PDL.  It uses Perl's L<constant> pragma for
simplicity and availability.

=head1 SYNOPSIS

 use PDL::Constants qw(PI E);
 print 'PI is ' . PI . "\n";
 print 'E  is ' .  E . "\n";

=cut

package PDL::Constants;
our $VERSION = "0.02";
$VERSION = eval $VERSION;

require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(PI DEGRAD E I J);  # symbols to export

use PDL::Lite;
use PDL::Complex qw(i);

=head2 PI

The ratio of a circle's circumference to its diameter

=cut

use constant PI    => 4 * atan2(1, 1);

=head2 DEGRAD

The number of degrees of arc per radian (180/PI)

=cut

use constant DEGRAD => 180/PI;

=head2 E

The base of the natural logarithms or Euler's number

=cut

use constant E     => exp(1);

=head2 I

The imaginary unit, C< I*I == -1 >

=cut

use constant I     => i;

=head2 J

The imaginary unit for engineers, C< J*J == -1 >

=cut

use constant J     => i;

=head1 COPYRIGHT & LICENSE

Copyright 2010 Chris Marshall (chm at cpan dot org).

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut

1;
