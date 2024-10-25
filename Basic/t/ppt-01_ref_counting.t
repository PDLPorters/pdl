use strict;
use warnings;

use Test::More;

use PDL::LiteF;
use PDL::Parallel::threads qw(retrieve_pdls free_pdls);

my $data = sequence(20);
is($data->datasv_refcount, 1, "Data's initial refcount for normal ndarray is 1");

my $copy = $data;
is($data->datasv_refcount, 1, "Shallow copy does not increase data's refcount");

$data->share_as('foo');
is($data->datasv_refcount, 2, "Sharing data increases data's refcount");

my $shallow = retrieve_pdls('foo');
is($data->datasv_refcount, 3, "Retrieving data increases data's refcount");

undef($shallow);
is($data->datasv_refcount, 2, "Undef'ing retrieved copy decreases data's refcount");

undef($copy);
is($data->datasv_refcount, 2, "Undef'ing one of two original copies does not decrease data's refcount");

undef($data);

# At this point, there should be only one reference, but we can't actually
# know because we don't have a reference to an ndarray to check! Get a new
# shared copy:
$shallow = retrieve_pdls('foo');
is($shallow->datasv_refcount, 2, "Getting rid of original does not destroy the data");

free_pdls('foo');
is($shallow->datasv_refcount, 1, "Freeing memory only decrements refcount by one");

done_testing;
