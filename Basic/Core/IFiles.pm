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

1;
