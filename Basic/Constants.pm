=head1 NAME

PDL::Constants -- basic compile time constants for PDL

=head1 DESCRIPTION

This module is used to define compile time constant
values for PDL.  It uses the constant module for
simplicity and availability.  We'll need to sort
out exactly which constants make sense but PI and
E seem to be fundamental.

=head1 SYNOPSIS

 use PDL::Constants;
 print 'PI is ' . PI . "\n";
 print 'E  is ' .  E . "\n";

=cut

package PDL::Constants;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(PI E);  # symbols to export
                           
use constant PI    => 4 * atan2(1, 1);
use constant E     => exp(1);

=head1 COPYRIGHT & LICENSE

Copyright 2010 Chris Marshall (chm at cpan dot org).

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut

1;
