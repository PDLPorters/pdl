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
 use PDL::MyModule '...'; # Same as Exporter

=cut

package PDL::Exporter;

use Exporter;

sub import {
   my $pkg = shift;
   return if $pkg eq 'PDL::Exporter'; # Module don't export thyself :)
   my $callpkg = caller($Exporter::ExportLevel);
   #print "DBG: pkg=$pkg callpkg = $callpkg\n";
   push @_, ':Func' unless @_;
   @_=() if scalar(@_)==1 and $_[0] eq '';
   Exporter::export($pkg, $callpkg, @_);
}

1;

=head1 AUTHOR

Copyright (C) Karl Glazebrook (kgb@aaoepp.aao.gov.au).
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut
