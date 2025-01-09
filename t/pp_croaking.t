# Test the error reporting for malformed PDL::PP code.
use strict;
use warnings;
use Test::More;

# Load up PDL::PP
use PDL::PP qw(foo::bar foo::bar foobar);

# Prevent file generation (does not prevent calling of functions)
$PDL::PP::done = 1;

eval {pp_addpm({At=>'Mid'}, "blah")};
like $@, qr/Middle/, 'pp_addpm says valid options';

# Check loop malformed call:
eval {pp_def(test1 => Pars => 'a(n)', Code => 'loop %{ $a()++; %}')};
like $@, qr/Expected.*loop.*%\{/, 'loop without dim name should explain error';

# Check what looks like malformed var access in a string works:
eval {pp_def(test1 => Pars => 'a(n)', Code => '$CROAK("$iisframe must be in range");')};
is $@, '', '$var without brackets in a string is not error';
#like $@, qr/Expected brackets/, 'var access without ()';

eval { pp_def( "func", Code => ';',
  Pars => "I(m);",
) };
like $@, qr/Invalid Pars name/;

eval { pp_def( "func", Code => ';',
  Pars => "x(m);",
  OtherPars => 'int I;',
) };
like $@, qr/Invalid OtherPars name/;

for (
  'short a(o,c); short [o]b(o,c)',
) {
  eval { pp_def( "func", Code => ';', Pars => $_, Inplace => 1) };
  is $@, '', "Pars=>'$_'";
}
for (
  'a(); int [o]mask();',
  'r(m=2); float+ [o]p(m=2);',
) {
  eval { pp_def( "func", Code => ';', Pars => $_, Inplace => 1) };
  like $@, qr/have different type specifications/, "Pars=>'$_'";
}

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
  GenericTypes => [qw(B INVALID)],
) };
like $@, qr/INVALID/, 'invalid GenericTypes caught';

eval { pp_def( "func", Code => '$a(n);',
  Pars => "a(n=2); [o] b(m=3);",
) };
like $@, qr/no '=>' seen/, 'useful error when no "=>" in ndarray access';

eval { pp_def( "func", Code => '$a(n=>1 + 2);',
  Pars => "a(n=2); [o] b(m=3);",
) };
like $@, qr/func\).*no spaces/, 'useful error when no "=>" in ndarray access';

my @boilerplate = (my $pars = "a(n=2); [o] b(m=3)", "func", my $otherpars = "int x; char *y");
eval { PDL::PP::Signature->new(@boilerplate, {x=>0}, undef) };
isnt $@, '', 'error to give default for non-last params';

eval { PDL::PP::Signature->new(@boilerplate, {}, [qw(a x y)]) };
like $@, qr/missed params/;

eval { PDL::PP::Signature->new(@boilerplate, {}, [qw(a x y b c)]) };
like $@, qr/too many params/;

eval { PDL::PP::Signature->new(@boilerplate, {}, [qw(a x b y)]) };
like $@, qr/optional argument/;

eval { PDL::PP::Signature->new(@boilerplate, {}, 1) };
is $@, '', 'non-ref true value OK';

eval { PDL::PP::Signature->new(@boilerplate, undef, [qw(a x y b)]) };
is $@, '', 'valid order OK';

my $got = PDL::PP::Signature->new(
   $pars, 'name', $otherpars, {}, 1
)->args_callorder;
is_deeply $got, [qw(a x y b)], 'right reorder no defaults' or diag explain $got;
is_deeply $got = PDL::PP::Signature->new(
   $pars, 'name', $otherpars, {x=>1}, 1
)->args_callorder, [qw(a y x b)],
  'right reorder with default'
  or diag explain $got;
is_deeply $got = PDL::PP::Signature->new(
   $pars, 'name', "[o] $otherpars; double z", {}, 1
)->args_callorder, [qw(a y z b x)], 'right reorder, output other, no defaults'
  or diag explain $got;
is_deeply $got = PDL::PP::Signature->new(
   $pars, 'name', "[o] $otherpars; double z", {y=>'""'}, 1
)->args_callorder, [qw(a z y b x)],
  'right reorder, output other, with default'
  or diag explain $got;

eval {
pp_def("rice_expand",
  Pars=>'in(n); [o]out(m);',
  OtherPars=>'IV dim0 => m; int blocksize',
  OtherParsDefaults=>{ blocksize=>32 },
  GenericTypes=>['B','S','US','L'],
  Code => 'fits_rcomp$TBSUL(_byte,_short,_short,)();',
);
};
is $@, '';

done_testing;
