=head1 NAME

PDL::Install::Files -- Module for use by L<ExtUtils::Depends> and L<Inline>

=head1 SYNOPSIS

  use Inline with => 'PDL';
  # or alternatively, if your XS module uses PDL:
  use ExtUtils::Depends;
  my $pkg = ExtUtils::Depends->new(qw(MyPackage PDL));

=head1 DESCRIPTION

This module is for use by L<ExtUtils::Depends> and L<Inline>. There are
no user-serviceable parts inside.

=cut

package PDL::Install::Files;
# support ExtUtils::Depends
require PDL::Core::Dev;

our $VERSION = '2.009';

$self = {
  'typemaps' => [ &PDL::Core::Dev::PDL_TYPEMAP ],
  'inc' => &PDL::Core::Dev::PDL_INCLUDE,
  'libs' => '',
  'deps' => [],
};
@deps = @{ $self->{deps} };
@typemaps = @{ $self->{typemaps} };
$libs = $self->{libs};
$inc = $self->{inc};
$CORE = undef;
foreach (@INC) {
  if ( -f "$_/PDL/Install/Files.pm") { $CORE = $_ . "/PDL/Install/"; last; }
}

sub deps { }
# support: use Inline with => 'PDL';

require Inline;

sub Inline {
  my ($class, $lang) = @_;
  return {} if $lang eq 'Pdlpp';
  return unless $lang eq 'C';
  unless($ENV{"PDL_Early_Inline"} // ($Inline::VERSION >= 0.68) ) {
      die "PDL::Inline: requires Inline version 0.68 or higher to make sense\n  (yours is $Inline::VERSION). You should upgrade Inline, \n   or else set \$ENV{PDL_Early_Inline} to a true value to ignore this message.\n";
  }
  +{
    TYPEMAPS      => [ &PDL::Core::Dev::PDL_TYPEMAP ],
    INC           => &PDL::Core::Dev::PDL_INCLUDE,
    AUTO_INCLUDE  => &PDL::Core::Dev::PDL_AUTO_INCLUDE,
    BOOT          => &PDL::Core::Dev::PDL_BOOT,
  };
}

1;
