## package Foo;
use Module::Compile -base;
use strict;
use warnings;

sub pmc_compile {
    my ($class, $source) = @_;
    # Convert $source into (most likely Perl 5) $compiled_output
    my $filtered = perldlpp('PDL::NiceSlice', $source);
    return $filtered;
}

1;
