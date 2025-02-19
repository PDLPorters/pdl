use strict;
use warnings;

use Test::More;
use PDL::PP qw(Foo::Bar Foo::Bar foobar);
#use Data::Dump;

pp_bless('Foo::Bar');

pp_def(foo_01 =>
  Pars => 'a(n)',
);

pp_def(foo_02 =>
    Pars => 'a(n)',
    NoExport => 1,
);

pp_def(foo_03 =>
  Pars => 'a(n); [o]b(n)',
);

pp_def(foo_04 =>
    Pars => 'a(n); [o]b(n)',
    OtherPars => 'int k',
);

pp_def(foo_05 =>
    Pars => 'a(n); b(n); [o]c(n)',
    Overload => '?:',
);

pp_def(foo_06 =>
    Pars => 'a(n); b(n); [o]c(n)',
    Overload => ['?:', 1],
    Inplace => ['a'],
);

pp_def(foo_07 =>
    Pars => 'a(n); b(n); [o]c(n)',
    Inplace => ['a'],
    NoExport => 1,
);

pp_def(foo_08 =>
    Pars => 'a(n); b(n); [o]c(n)',
    Overload => ['rho', 0, 0, 1],
);

pp_def(foo_09 =>
    Pars => 'a(n); b(n); [o]c(n)',
    OtherPars => 'int k',
    ArgOrder => [qw(a b k c)],
);

pp_def(foo_10 =>
    Pars => 'a(n); [o]b(n); [o]c(n)',
);

pp_def(foo_11 =>
    Pars => 'a(n); [o]b(n); [o]c(n)',
    OtherPars => 'int k',
    ArgOrder => [qw(a k b c)],
);

pp_done;

unlink 'foobar.xs';

my %sect;

# read generated pm file and collect sections by name
open my $fh, '<', 'foobar.pm';
while (<$fh>) {
    $1 && ($sect{$1} .= $_) if /^=head2 (\w+)/ .. /^=cut/;
}
close $fh;
unlink 'foobar.pm';

#dd \%sect;

# search pattern in pm file:
sub qout {
    my $str = quotemeta shift;
    qr/^\s+$str;/m;
}

note 'foo_01';
ok $sect{foo_01} =~ qout('foo_01($a)'), 'function call';
ok $sect{foo_01} =~ qout('$a->foo_01'), 'method call';

note 'foo_02';
ok $sect{foo_02} =~ qout('Foo::Bar::foo_02($a)'), 'no-exp function call';
ok $sect{foo_02} =~ qout('$a->foo_02'), 'no-exp exported method call';

note 'foo_03';
ok $sect{foo_03} =~ qout('$b = foo_03($a)'), 'function call w/ arg';
ok $sect{foo_03} =~ qout('foo_03($a, $b)'), 'all arguments given';
ok $sect{foo_03} =~ qout('$b = $a->foo_03'), 'method call';

note 'foo_04';
ok $sect{foo_04} =~ qout('$b = foo_04($a, $k)'), 'function call w/ arg';
ok $sect{foo_04} =~ qout('foo_04($a, $b, $k)'), 'all arguments given';
ok $sect{foo_04} =~ qout('$b = $a->foo_04($k)'), 'method call';

note 'foo_05';
ok $sect{foo_05} =~ qout('$c = $a ?: $b'), 'biop';

note 'foo_06';
ok $sect{foo_06} =~ qout('$a ?:= $b'), 'mutator';
ok $sect{foo_06} =~ qout('foo_06($a->inplace, $b)'), 'inplace function call';
ok $sect{foo_06} =~ qout('$a->inplace->foo_06($b)'), 'inplace method call';

note 'foo_07';
ok $sect{foo_07} =~ qout('Foo::Bar::foo_07($a->inplace, $b)'), 'inplace no-exp';

note 'foo_08';
ok $sect{foo_08} =~ qout('$c = rho $a, $b'), 'prefix biop';

note 'foo_09';
ok $sect{foo_09} =~ qout('$c = foo_09($a, $b, $k)'), 'argorder function call';
ok $sect{foo_09} =~ qout('$c = $a->foo_09($b, $k)'), 'argorder method call, all args';

note 'foo_10';
ok $sect{foo_10} =~ qout('foo_10($a, $b, $c)'), 'multi output function call, all args';
ok $sect{foo_10} =~ qout('($b, $c) = foo_10($a)'), 'multi output function call';
ok $sect{foo_10} =~ qout('($b, $c) = $a->foo_10'), 'multi output method call';

note 'foo_11';
TODO: {
    local $TODO = 'multiple output';
    ok $sect{foo_11} =~ qout('foo_11($a, $k, $b, $c)'), 'multi output, argorder, function call, all args';
}
ok $sect{foo_11} =~ qout('($b, $c) = foo_11($a, $k)'), 'multi output, argorder, function call';
ok $sect{foo_11} =~ qout('($b, $c) = $a->foo_11($k)'), 'multi output, argorder, method call';

done_testing;
