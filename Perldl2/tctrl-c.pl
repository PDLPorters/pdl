use strict;
use warnings;
use Term::ReadLine;

$| = 1;
my $cnt = 0;
sub handler {
	print "Caught a SIG '$_[0]' - continuing\n";
	die "Got three" if ++$cnt > 2;
}
$SIG{INT}  = \&handler;

my $term = Term::ReadLine->new('ProgramName');
while (1) {
	my $input = $term->readline('prompt> ');
	if (not defined $input) {
		print "EOF on input\n";
	} elsif ($input eq 'q') {
		print "quitting\n"; exit;
	} else {
		printf "Got: '%s'\n", $input;
	}
	Win32::Sleep(1000);	# stop runaway console
}

__END__
