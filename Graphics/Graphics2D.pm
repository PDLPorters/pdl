package PDL::Graphics2D;


=head1 NAME

PDL::Graphics2D - An object oriented interface to PDL graphics

=head1 SYNOPSIS

use PDL::Graphics2D;
$win = PDL::Graphics2D->new(<Interface>, <Options>);

=head1 DESCRIPTION

This is an umbrella class allowing for a simple interface to all plotting
routines in PDL. On its own it does not do any work it merely passes
information to the appropriate class. Ideally this should probably offer
a uniform interface to a variety of packages.

This requires a lot more work before it is useful I feel, but it can be
used already.

=cut


{
  my %lookup=(
	      'PGPLOT' => 'PDL::Graphics::PGPLOT::Window'
	     );
  sub new {

    my $type=shift;
    my $interface=shift;

    #
    # Translate the interface name to the appropriate class name.
    #
    $interface=uc($interface);
    die "Interface $interface is not known!\n" if !exists($lookup{$interface});
    my $class = $lookup{$interface};
    eval "require $class;";
    return $class->new(@_);
  }
}

1;

