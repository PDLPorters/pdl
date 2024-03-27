use strict;
use warnings;
use Test::More;
use PDL::IO::IDL;

# $PDL::IO::IDL::test=1;
my $mod = ridl("t/test.sav");
isnt $mod, undef, 'basic read';

done_testing;
