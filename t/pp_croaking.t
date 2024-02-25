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
like $@, qr/Expected.*loop.*%\{/, 'loop without dim name should explain error';

eval {
  pp_def(test1 =>
    Pars => 'a(n)',
    OtherPars => 'int b; int c',
    OtherParsDefaults => { b => 0 },
    Code => q{;},
  );
};
isnt $@, '', 'error to give default for non-last params';

eval { pp_def( "func", Code => ';',
  Pars => "I(m);",
) };
like $@, qr/Invalid Pars name/;

eval { pp_def( "func", Code => ';',
  Pars => "x(m);",
  OtherPars => 'int I;',
) };
like $@, qr/Invalid OtherPars name/;

eval { pp_def( "func", Code => ';',
  Pars => "[o] a();",
  Inplace => ['a'],
) };
like $@, qr/is actually output/;

eval { pp_def( "func", Code => ';',
  Pars => "a(m);",
  Inplace => 1,
) };
like $@, qr/Inplace does not know name of output/;

eval { pp_def( "func", Code => ';',
  Pars => "[o] a(m);",
  Inplace => 1,
) };
like $@, qr/Inplace does not know name of input/;

eval { pp_def( "func", Code => ';',
  Pars => "[o] a(m);",
  Inplace => ['a', 'b', 'c'],
) };
like $@, qr/Inplace array-ref/;

eval { pp_def( "func", Code => ';',
  Pars => "a(); [o] b();",
  Inplace => ['a', 'b'],
) };
is $@, '';

eval { pp_def( "func", Code => ';',
  Pars => "a(); b();",
  Inplace => ['a', 'b'],
) };
like $@, qr/Inplace output arg b not \[o]/;

eval { pp_def( "func", Code => ';',
  Pars => "a(); [o] b(m);",
  Inplace => ['a', 'b'],
) };
like $@, qr/Inplace args a and b different number of dims/;

eval { pp_def( "func", Code => ';',
  Pars => "a(n); [o] b(m);",
  Inplace => ['a', 'b'],
) };
is $@, '', 'different but non-fixed dims OK';

eval { pp_def( "func", Code => ';',
  Pars => "a(n=2); [o] b(m);",
  Inplace => ['a', 'b'],
) };
is $@, '', 'one fixed dim OK';

eval { pp_def( "func", Code => ';',
  Pars => "a(n=2); [o] b(m=3);",
  Inplace => ['a', 'b'],
) };
like $@, qr/Inplace Pars a and b inds n=2 and m=3 not compatible/;

eval { pp_def( "func", Code => ';',
  Pars => "a(n=2); [o] b(m=3);",
  OtherPars => "int x; char *y",
  ArgOrder => [qw(a x y)],
) };
like $@, qr/missed params/;

eval { pp_def( "func", Code => ';',
  Pars => "a(n=2); [o] b(m=3);",
  OtherPars => "int x; char *y",
  ArgOrder => [qw(a x y b c)],
) };
like $@, qr/too many params/;

eval { pp_def( "func", Code => ';',
  Pars => "a(n=2); [o] b(m=3);",
  OtherPars => "int x; char *y",
  ArgOrder => [qw(a x b y)],
) };
like $@, qr/optional argument/;

eval { pp_def( "func", Code => ';',
  Pars => "a(n=2); [o] b(m=3);",
  OtherPars => "int x; char *y",
  ArgOrder => 1,
) };
is $@, '', 'non-ref true value OK';

eval { pp_def( "func", Code => ';',
  Pars => "a(n=2); [o] b(m=3);",
  OtherPars => "int x; char *y",
  ArgOrder => [qw(a x y b)],
) };
is $@, '', 'valid order OK';

eval { pp_def( "func", Code => ';',
  Pars => "a(n=2); [o] b(m=3);",
  GenericTypes => [qw(B INVALID)],
) };
like $@, qr/INVALID/, 'invalid GenericTypes caught';

my $got = [PDL::PP::reorder_args(my $sig = PDL::PP::Signature->new(
   "a(n=2); [o] b(m=3);", 1, "int x; char *y"
), {})];
is_deeply $got, [qw(a x y b)], 'right reorder no defaults' or diag explain $got;
is_deeply $got = [PDL::PP::reorder_args($sig, {x=>1})], [qw(a y x b)],
  'right reorder with default'
  or diag explain $got;
is_deeply $got = [PDL::PP::reorder_args($sig = PDL::PP::Signature->new(
   "a(n=2); [o] b(m=3);", 1, "[o] int x; char *y; double z"
), {})], [qw(a y z b x)], 'right reorder, output other, no defaults'
  or diag explain $got;
is_deeply $got = [PDL::PP::reorder_args($sig, {y=>'""'})], [qw(a z y b x)],
  'right reorder, output other, with default'
  or diag explain $got;

eval {
pp_def("rice_expand",
  Pars=>'in(n); [o]out(m);',
  OtherPars=>'IV dim0 => m; int blocksize',
  OtherParsDefaults=>{ blocksize=>32 },
);
};
is $@, '';

done_testing;
