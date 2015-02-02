package PDL::Install::Files;
# support ExtUtils::Depends
require PDL::Core::Dev;

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
sub Inline {
  my ($class, $lang) = @_;
  return {} if $lang eq 'Pdlpp';
  return unless $lang eq 'C';
  +{
    TYPEMAPS      => [ &PDL::Core::Dev::PDL_TYPEMAP ],
    INC           => &PDL::Core::Dev::PDL_INCLUDE,
    AUTO_INCLUDE  => &PDL::Core::Dev::PDL_AUTO_INCLUDE,
    BOOT          => &PDL::Core::Dev::PDL_BOOT,
  };
}

1;
