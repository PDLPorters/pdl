# Below is the stub of documentation for your module. You better edit it!

=head1 NAME

PDL::MyInlineMod - a simple PDL module containing inlined Pdlpp code

=head1 SYNOPSIS

  use PDL::MyInlineMod;

  $a = zeroes 10, 10;
  $twos = $a->plus2;  # a simple function

=head1 DESCRIPTION

A simple example module that demonstrates the usage of inlined Pdlpp
in a module that can be installed in the usual way.

=head1 FUNCTIONS

=cut

package PDL::MyInlineMod;

# use strict;  # strict results in trouble with barewords when using Inline :(
# no strict 'vars';
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

require PDL::Exporter;
@ISA = qw(PDL::Exporter);
# functions you want to export into the caller's name space
@EXPORT_OK = qw(myinc plus2);
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

BEGIN { # in BEGIN to make sure we can use $VERSION in the
        # 'use Inline...' call below
$VERSION = '0.60'; # Inline requires this to be a *string* that matches
                   #          /^\d\.\d\d$/
                   # see Inline-FAQ for more info
}

use PDL::LiteF;

# quirk 1 follows
use Inline::MakePdlppInstallable;  # allow installation of this module

use Inline Pdlpp => DATA => # inlined PP code is below in DATA section
  NAME => PDL::MyInlineMod,    # required info for module installation
  VERSION => $VERSION;      # ditto, see Inline-FAQ for more info

# quirk 2 follows
Inline->init;               # you need this if you want to 'use' your module
                            # from within perldl and your Pdlpp code resides
                            # in the DATA section (as in this example)

# following required to make exported functions work!
# PDL::PP used to make these automatically but now we have
# to make them manually since *we* are writing the pm-file
*myinc = \&PDL::myinc;      # make alias in this module's name space
*plus2 = \&PDL::plus2;      # ditto

1;

__DATA__

__Pdlpp__

# some simple functions to test the whole thing

=head2 myinc

=for ref

a very simple pp function that increments its argument

=for sig

  myinc(i();[o] o())

=cut

pp_def('myinc',
          Pars => 'i();[o] o()',
          Code => '$o() = $i() + 1;',
         );

=head2 plus2

=for ref

a very simple pp function that increments its argument by 2

=for sig

  plus2(i();[o] o())

=cut

pp_def('plus2',
          Pars => 'i();[o] o()',
          Code => '$o() = $i() + 2;',
         );

=head1 AUTHOR

C. Soeller (C) 2002. All rights reserved. This code can be distributed
under the same terms as PDL itself (see the file COPYING in the PDL
distribution).

=head1 SEE ALSO

perl(1).

L<Inline>.

L<Inline::Pdlpp>.

=cut

