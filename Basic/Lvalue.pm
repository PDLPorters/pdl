=head1 NAME

PDL::Lvalue - declare PDL lvalue subs

=head1 DESCRIPTION

Declares a subset of PDL functions so that they
can be used as lvalue subs. In particular, this allows
simpler constructs such as

  $x->slice(',(0)') .= 1;

instead of the clumsy

  (my $tmp = $x->slice(',(0)')) .= 1;

This will only work if your perl supports lvalue subroutines
(i.e. versions  >= v5.6.0). Note that lvalue subroutines
are currently regarded experimental.

=head1 SYNOPSIS

 use PDL::Lvalue; # automatically done with all PDL loaders

=head1 FUNCTIONS

=cut

package PDL::Lvalue;

# list of functions that can be used as lvalue subs
# extend as necessary
my @funcs = qw/ clump diagonal dice dice_axis dummy flat
                index index2d indexND indexNDb mslice mv
                nslice nslice_if_pdl nnslice polyfillv px
                range rangeb reorder reshape sever slice
                where whereND xchg /;

my $prots = join "\n", map {"use attributes 'PDL', \\&PDL::$_, 'lvalue';"}
  @funcs;

=head2 subs

=for ref

test if routine is a known PDL lvalue sub

=for example

  print "slice is an lvalue sub" if PDL::Lvalue->subs('slice');

returns the list of PDL lvalue subs if no routine name is given, e.g.

  @lvfuncs = PDL::Lvalue->subs;

It can be used in scalar context to find out if your
PDL has lvalue subs:

  print 'has lvalue subs' if PDL::Lvalue->subs;

=cut

sub subs {
  my ($type,$func) = @_;
  if (defined $func) {
    $func =~ s/^.*:://;
    return ($^V and $^V >= 5.006007) && scalar grep {$_ eq $func} @funcs;
  } else {
    return ($^V and $^V >= 5.006007) ? @funcs : ();
  }
}

# print "defining lvalue subs:\n$prots\n";

eval << "EOV" if ($^V and $^V >= 5.006007);
{ package PDL;
  no warnings qw(misc);
  $prots
}
EOV

=head1 AUTHOR

Copyright (C) 2001 Christian Soeller (c.soeller@auckland.ac.nz). All
rights reserved. There is no warranty. You are allowed to redistribute
this software / documentation under certain conditions. For details,
see the file COPYING in the PDL distribution. If this file is
separated from the PDL distribution, the copyright notice should be
included in the file.

=cut

1;
