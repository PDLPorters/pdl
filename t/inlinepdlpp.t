use strict;
use warnings;
use Test::More;

BEGIN {
  my $inline_test_dir = './.inlinepdlpp';
  mkdir $inline_test_dir unless -d $inline_test_dir;
  eval {
    require Inline;
    require Inline::C;
    Inline->import(Config => DIRECTORY => $inline_test_dir, FORCE_BUILD => 1);
    1;
  } || plan skip_all => "Skipped: Inline or Inline::C not installed";
  note "Inline Version: $Inline::VERSION\n";
  eval { Inline->VERSION(0.43) };
  plan skip_all => "Skipped: not got Inline >= 0.43" if $@;
}

use PDL::LiteF;

# use Inline 'INFO'; # use to generate lots of info
eval { Inline->bind(Pdlpp => <<'EOF') };
# simple PP definition

pp_def('testinc',
  Pars => 'a(); [o] b()',
  Code => '$b() = $a() + 1;' # wow, that's complicated
);

# this tests the bug with a trailing comment and *no* newline
EOF
is $@, '', 'bind no error';
my $x = sequence(3,3);
my $y = $x->testinc;
is myshape($x), myshape($y), 'myshape eq';
ok(all $y == $x+1, '==');

sub myshape { join ',', $_[0]->dims }

eval { Inline->bind(Pdlpp => <<'EOF', PACKAGE => 'Other::Pkg') };
pp_addxs(<<'EOXS');
int
add1 (parm)
        int     parm
CODE:
        RETVAL =  parm + 1;
OUTPUT:
        RETVAL
EOXS
EOF
is $@, '', 'bind no error';
my $r = eval { Other::Pkg::add1(4) };
is $@, '', 'call no error';
is $r, 5, 'correct result';

done_testing;
