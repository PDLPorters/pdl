# Test the error reporting for malformed PDL::PP code.
use strict;
use warnings;
use Test::More;

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

eval {
  pp_def(test1 =>
    Pars => 'a(n)',
    OtherPars => 'int b; int c',
    OtherParsDefaults => { b => 0 },
    Code => q{;},
  );
};
isnt $@, '', 'error to give default for non-last params';

eval { pp_def( "func",
  Pars => "I(m);",
  Code => 'int foo = 1;'
) };
like $@, qr/Invalid Pars name/;

eval { pp_def( "func",
  Pars => "x(m);",
  OtherPars => 'int I;',
  Code => 'int foo = 1;'
) };
like $@, qr/Invalid OtherPars name/;

done_testing;
