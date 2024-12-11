# Boilerplate
use strict;
use warnings;

# Test declaration
use Test::More tests => 9;

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
ok(all($to_compare == $data), 'A ndarray exactly equals itself')
	or diag("Original is $data and comparison is $to_compare;\n"
		. "original - comparison = " . ($data - $to_compare));

# Now retrieve the value from the "off-site" storage
$to_compare = retrieve_pdls('Test::Set1');
is_deeply([$to_compare->dims], [$data->dims], 'Retrieved dims is correct')
	or diag("Original dims are " . join(', ', $data->dims)
		. " and retrieved dims are " . join', ', $to_compare->dims);

ok($data->type == $to_compare->type, 'Retrieved type is correct')
	or diag("Original type is " . $data->type
		. " and retrieved type is " . $to_compare->type);

ok(all($to_compare == $data), 'Retrieved value exactly equals original')
	or diag("Original is $data and retrieved is $to_compare;\n"
		. "original - retrieved = " . ($data - $to_compare));

###########################
# Shared modifications: 2 #
###########################

use PDL::NiceSlice;
# Modify the original, see if it is reflected in the retrieved copy
$data(3) .= -10;
ok(all($to_compare == $data), 'Modification to original is reflected in retrieved')
	or diag("Original is $data and retrieved is $to_compare;\n"
		. "original - retrieved = " . ($data - $to_compare));

$to_compare(8) .= -50;
ok(all($to_compare == $data), 'Modification to retrieved is reflected in original')
	or diag("Original is $data and retrieved is $to_compare;\n"
		. "original - retrieved = " . ($data - $to_compare));

###############################
# Undefine doesn't destroy: 3 #
###############################

my $expected = pdl(1, -10, -50);  # These need to line up with the
my $idx = pdl(0, 3, 8);           # indices and values used/set above

undef($to_compare);
ok(all($data($idx) == $expected), "Undeffing copy doesn't destroy data");

undef($data);
my $new = retrieve_pdls('Test::Set1');
ok(all($new($idx) == $expected), "Can retrieve data even after undefing original");

PDL::Parallel::threads::free_pdls('Test::Set1');
ok(all($new($idx) == $expected), "Reference counting works");

