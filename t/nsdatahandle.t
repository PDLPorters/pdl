use strict;
use Test;


# check if PDL::NiceSlice clobbers the DATA filehandle
use PDL::LiteF;

plan tests => 1;

$| = 1;

use PDL::NiceSlice;

my $data = join '', <DATA>;
ok $data =~ "we've got data";

__DATA__

we've got data
