# Test the error reporting for malformed PDL::PP code.
use Test::More tests => 3;

# Load up PDL::PP
use PDL::PP qw(foo::bar foo::bar foobar);

# Prevent file generation (does not prevent calling of functions)
$PDL::PP::done = 1;

# Check the loop malformed call:
eval {
	pp_def(test1 =>
		Pars => 'a(n)',
		Code => q{
			loop %{
				$a()++;
			%}
		}
	);
};

my $err_msg = $@;
isnt($@, undef, 'loop without dim name should throw an error');
like($@, qr/Expected.*loop.*%\{/, 'loop without dim name should explain the error')
	or diag("Got this error: $@");

TODO: {
	local $TODO = 'Have not figured out why @CARP_NOT is not working';
	unlike($@, qr/PP\.pm/, 'Should not report error as coming from PDL::PP');
};

