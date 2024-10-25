package PDL::Config;
use strict;
use warnings;

=head1 NAME

PDL::Config - vestige of configuration for PDL

=head1 DESCRIPTION

This was used for control of, and then storage of, various options
for building PDL. Many have been replaced with environment variables
- consult the various F<Makefile.PL>s in the GD, GSL, etc modules.

To see if a module is installed, use this boolean expression:

  eval { require PDL::Module::Name; 1 }

=cut

1;
