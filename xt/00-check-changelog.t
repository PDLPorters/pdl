use Test::More tests => 1;

use strict;
use warnings;

use CPAN::Changes;
use Data::Dumper;

my $changes = CPAN::Changes->load('Changes');

ok($changes);

my @releases = map { +{ $_->version => $_->date } } $changes->releases;

note Dumper \@releases;

done_testing;
