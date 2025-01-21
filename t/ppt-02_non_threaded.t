# Boilerplate
use strict;
use warnings;

# Test declaration
use Test::More;
use Test::PDL -atol => 0;

# Modules needed for actual testing
use PDL::LiteF;
use PDL::Parallel::threads qw(retrieve_pdls);

########################
# Direct comparison: 4 #
########################

# Create some memory with some irrational values. The goal here is to
# perform a strict comparison between floating point values that have
# something nontrivial across all its bits.
my $data = (sequence(10)+1)->sqrt->share_as('Test::Set1');
my $to_compare = $data;
is_pdl $to_compare, $data, 'A ndarray exactly equals itself';

# Now retrieve the value from the "off-site" storage
$to_compare = retrieve_pdls('Test::Set1');
is_pdl $to_compare, $data, 'Retrieved value exactly equals original';

###########################
# Shared modifications: 2 #
###########################

use PDL::NiceSlice;
# Modify the original, see if it is reflected in the retrieved copy
$data(3) .= -10;
is_pdl $to_compare, $data, 'Modification to original is reflected in retrieved';

$to_compare(8) .= -50;
is_pdl $to_compare, $data, 'Modification to retrieved is reflected in original';

###############################
# Undefine doesn't destroy: 3 #
###############################

my $expected = pdl(1, -10, -50);  # These need to line up with the
my $idx = pdl(0, 3, 8);           # indices and values used/set above

undef($to_compare);
is_pdl $data($idx), $expected, "Undeffing copy doesn't destroy data";

undef($data);
my $new = retrieve_pdls('Test::Set1');
is_pdl $new($idx), $expected, "Can retrieve data even after undefing original";

PDL::Parallel::threads::free_pdls('Test::Set1');
is_pdl $new($idx), $expected, "Reference counting works";

done_testing;
