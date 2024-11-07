use strict;
use warnings;
use Test::More;
use PDL::Demos;

my @found = PDL::Demos->list;
ok scalar @found, 'found demos';
my ($general) = grep /General$/, @found;
isnt $general, undef, 'found the PDL demo' or diag 'found ', explain \@found;
my @kw = PDL::Demos->keywords;
ok scalar @kw, 'found keywords';
ok +(grep $_ eq 'pdl', @kw), 'found "pdl" in keywords' or diag explain \@kw;
my @info = PDL::Demos->info('pdl');
is $info[0], 'pdl';
is scalar @info, 3, 'three elts in info';
my @demo = PDL::Demos->demo('pdl');
ok scalar @demo, 'demo commands';

done_testing;
