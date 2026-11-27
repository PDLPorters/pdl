# Test if we can still do scopes ok - multiple uses etc..
# Also see that PDL loaders get the correct symbols.
use strict;
use warnings;
use Test::More;

{
package A;
our $pa;
# note "A: ",%A::,"\n";
use PDL;
$pa = zeroes 5,5;
# note "A: %A::\n";
# note "AC: ",(bless {},A)->can("zeroes"),"\n";
}

ok((bless {},'A')->can("zeroes"));

{
package B;
use PDL;
}

#note "B: ",%B::,"\n";
#note "B: ",%B::,"\n";
# $pb = zeroes 5,5;
# note "BC: ",(bless {},B)->can("zeroes"),"\n";
ok((bless {},'B')->can("zeroes"));

{
package C;
use PDL::Lite;
}
ok(!((bless {},'C')->can("zeroes")));

{
package D;
use PDL::Lite;
}
ok(!((bless {},'D')->can("zeroes")));

{
package E;
use PDL::LiteF;
}
ok((bless {},'E')->can("zeroes"));

{
package F;
use PDL::LiteF;
}
ok((bless {},'F')->can("zeroes"));

ok(!((bless {},'C')->can("imag")));
ok(!((bless {},'D')->can("imag")));
ok(!((bless {},'E')->can("imag")));
ok(!((bless {},'F')->can("imag")));

#  Can PDL::Lite be loaded twice?
#  The first import was interfering with the second.  
{
    package mk1;
    use PDL::Lite;
    sub x {
        return PDL->pdl (1..10);
    }
}
{
    package mk2;
    use PDL::Lite;
    sub x {
        return PDL->pdl (11..20);
    }
}

foreach my $name (qw /x barf pdl piddle null/) {
    ok (mk1->can($name), "Sub loaded: mk1::" . $name);
    ok (mk2->can($name), "Sub loaded: mk2::" . $name);
}

# now try calling one of those functions
eval { my $x = mk1::pdl(0, 1) };
is $@, '', 'the imported pdl function ACTUALLY WORKS';

subtest 'PDL::Lite OO methods and non-exported functions' => sub {
  ok !mk1->can('sumover'), 'PDL::Lite does not export sumover';
  ok !mk1->can('sequence'), 'PDL::Lite does not export sequence';
  my $x = mk1::pdl(1, 2, 3, 4);
  isa_ok $x, 'PDL';
  is $x->sum, 10, 'Ufunc method sum works via PDL::Lite';
  is_deeply [ $x->slice('1:2')->list ], [ 2, 3 ], 'Slices method works via PDL::Lite';
  ok $PDL::Lite::VERSION, 'PDL::Lite::VERSION is set';
};

done_testing;
