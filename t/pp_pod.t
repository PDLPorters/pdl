use strict;
use warnings;

use Test::More;
use PDL::PP qw(Foo::Bar Foo::Bar foobar);

# call pp_def and report args
sub call_pp_def {
    my $obj = pp_def(@_);
    $obj;
}

# search and remove pattern in generated pod:
sub find_usage {
    my ($obj, $str) = @_;
    $obj->{UsageDoc} =~ s/^\s+\Q$str\E;.*?$//m;
}

# all checked?
sub all_seen {
    my ($obj, $str) = @_;
    $obj->{UsageDoc} !~ /^.*?\b$str\b.*?;.*$/m;
}

pp_bless('Foo::Bar');

subtest a => sub {
    my $obj = call_pp_def(foo =>
        Pars => 'a(n)',
    );

    ok find_usage($obj, 'foo($a)'), 'function call';
    ok find_usage($obj, '$a->foo'), 'method call';
    ok all_seen($obj, 'foo'), 'all seen';
};

subtest a_b => sub {
    my $obj = call_pp_def(foo =>
      Pars => 'a(n); [o]b(n)',
    );

    ok find_usage($obj, '$b = foo($a)'), 'function call w/ arg';
    ok find_usage($obj, 'foo($a, $b)'), 'all arguments given';
    ok find_usage($obj, '$b = $a->foo'), 'method call';
    ok find_usage($obj, '$a->foo($b)'), 'method call, arg';
    ok all_seen($obj, 'foo'), 'all seen';
};

subtest a_b_k => sub {
    my $obj = call_pp_def(foo =>
        Pars => 'a(n); [o]b(n)',
        OtherPars => 'int k',
    );

    ok find_usage($obj, '$b = foo($a, $k)'), 'function call w/ arg';
    ok find_usage($obj, 'foo($a, $b, $k)'), 'all arguments given';
    ok find_usage($obj, '$b = $a->foo($k)'), 'method call';
    ok find_usage($obj, '$a->foo($b, $k)'), 'method call, arg';
    ok all_seen($obj, 'foo'), 'all seen';
};

subtest ab_c_o => sub {
    my $obj = call_pp_def(foo =>
        Pars => 'a(n); b(n); [o]c(n)',
        Overload => '?:',
    );

    ok find_usage($obj, '$c = $a ?: $b'), 'biop';
    ok find_usage($obj, '$c = foo($a, $b)'), 'function';
    ok find_usage($obj, 'foo($a, $b, $c)'), 'function, all args';
    ok find_usage($obj, '$c = $a->foo($b)'), 'method';
    ok find_usage($obj, '$a->foo($b, $c)'), 'method, all args';
    ok all_seen($obj, 'foo'), 'all seen';
};

subtest a_bc => sub {
    my $obj = call_pp_def(foo =>
        Pars => 'a(n); [o]b(n); [o]c(n)',
    );

    ok find_usage($obj, 'foo($a, $b, $c)'), 'multi output function call, all args';
    ok find_usage($obj, '($b, $c) = foo($a)'), 'multi output function call';
    ok find_usage($obj, '($b, $c) = $a->foo'), 'multi output method call';
    ok find_usage($obj, '$a->foo($b, $c)'), 'method call, all args';
    ok all_seen($obj, 'foo'), 'all seen';
};

done_testing;
