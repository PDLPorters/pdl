package PDL::Complex::Overloads;
use strict;
use warnings;
use parent 'Math::Complex';
use overload fallback => 1;

sub cplx { bless &Math::Complex::cplx, __PACKAGE__ }

=head1 NAME

PDL::Complex::Overloads - subclass of Math::Complex with overload fallbacks

=head1 SYNOPSIS

  require PDL::Complex::Overloads;
  my $same = PDL::Complex::Overloads::cplx(1, 2) eq '1+2i';

=head1 DESCRIPTION

This is a subclass whose only purpose is to provide L<Math::Complex>'s
overloads but with C<fallback> true, mainly to allow string-comparison
for backwards compatibility.

=head1 AUTHOR

Ed J

=head1 SEE ALSO

L<Math::Complex>

=cut

1;
