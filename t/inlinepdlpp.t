use strict;
use warnings;
use Test::More;
use PDL::LiteF;

my $inline_test_dir = './.inlinepdlpp';
mkdir $inline_test_dir unless -d $inline_test_dir;
SKIP: {
   use_ok('Inline', Config => DIRECTORY => $inline_test_dir, FORCE_BUILD => 1)
      || skip "Skipped: Inline not installed", 4;
   note "Inline Version: $Inline::VERSION\n";
   eval { Inline->VERSION(0.43) };
   is $@, '', 'at least 0.43' or skip "Skipped: not got Inline >= 0.43", 3;
   # use Inline 'INFO'; # use to generate lots of info
   eval { Inline->bind(Pdlpp => <<'EOF') };
# simple PP definition

pp_def('testinc',
	Pars => 'a(); [o] b()',
	Code => '$b() = $a() + 1;' # wow, that's complicated
);

# this tests the bug with a trailing comment and *no* newline
EOF
   is $@, '', 'bind no error' or skip "bind failed", 2;
   my $x = sequence(3,3);
   my $y = $x->testinc;
   is myshape($x), myshape($y), 'myshape eq';
   ok(all $y == $x+1, '==');
}

sub myshape { join ',', $_[0]->dims }

done_testing;
