use strict;
use warnings;
use PDL;
use Test::More;

for my $start (0, 4, -4, 20, -20) {
	for my $stop (0, 4, -4, 20, -20) {
		# Generate a simple data piddle and a bad slice of that piddle
		my $data = sequence(10);
		my $slice = $data->slice("$start:$stop");

		pass('Slice operation for properly formed slice does not croak');

		# Calculate the expected dimension size:
		my $expected_dim_size;
		my $real_start = $start;
		$real_start += 10 if $start < 0;
		my $real_stop = $stop;
		$real_stop += 10 if $stop < 0;
		$expected_dim_size = abs($real_stop - $real_start) + 1
			if 0 <= $real_stop and $real_stop < 10
				and 0 <= $real_start and $real_start < 10;
		
		my $expected_outcome_description
			= defined $expected_dim_size ? 'is fine' : 'croaks';
		
		my $dim1;
		# Should croak when we ask about the dimension:
		eval { $dim1 = $slice->dim(0) };
		is($dim1, $expected_dim_size, "Requesting dim(0) on slice($start:$stop) $expected_outcome_description");

		# Should *STILL* croak when we ask about the dimension:
		eval { $dim1 = $slice->dim(0) };
		is($dim1, $expected_dim_size, "Requesting dim(0) a second time on slice($start:$stop) $expected_outcome_description");

		# Calculate the expected value
		my $expected_value;
		$expected_value = $data->at($real_start) if defined $expected_dim_size;
		
		# Should croak when we ask about data
		my $value;
		eval { $value = $slice->at(0) };
		is($value, $expected_value, "Requesting first element on slice($start:$stop) $expected_outcome_description");
	}
}


done_testing;
