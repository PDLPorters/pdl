=head1 NAME

PDL::Exporter - PDL export control

=head1 DESCRIPTION

Implements the standard conventions for
import of PDL modules in to the namespace

Hopefully will be extended to allow fine
control of which namespace is used.

=head1 SYNOPSIS

use PDL::Exporter;

 use PDL::MyModule;       # Import default function list ':Func'
 use PDL::MyModule '';    # Import nothing (OO)
 use PDL::MyModule '...'; # Same behaviour as Exporter

=head1 DESCRIPTION

C<PDL::Exporter> is a drop-in replacement for the L<Exporter|Exporter>
module. It confers the standard PDL export conventions to your module.
Usage is fairly straightforward and best illustrated by an example. The
following shows typical usage near the top of a simple PDL module:


   package PDL::MyMod;

   use strict;
   use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

   require PDL::Exporter;

   @ISA = qw(PDL::Exporter);
   @EXPORT_OK = qw(inc myfunc); # these will be exported by default
   %EXPORT_TAGS = (Func=>[@EXPORT_OK],
		   Internal => [qw/internfunc1 internfunc2/],
		  );

    # ... body of your module

   1; # end of simple module


=cut

package PDL::Exporter;

use Exporter;

sub import {
   my $pkg = shift;
   return if $pkg eq 'PDL::Exporter'; # Module don't export thyself :)
   my $callpkg = caller($Exporter::ExportLevel);
   print "DBG: pkg=$pkg callpkg = $callpkg :@_\n" if($PDL::Exporter::Verbose);
   push @_, ':Func' unless @_;
   @_=() if scalar(@_)==1 and $_[0] eq '';
   Exporter::export($pkg, $callpkg, @_);
}

1;

=head1 SEE ALSO

L<Exporter>

=head1 AUTHOR

Copyright (C) Karl Glazebrook (kgb@aaoepp.aao.gov.au).
Some docs by Christian Soeller.
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut
