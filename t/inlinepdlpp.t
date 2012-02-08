use strict;
use Test::More;
use blib;  # otherwise possible error on virgin systems not finding PDL::Core

use PDL::LiteF;

BEGIN {
   # clean out the _Inline directory on every test
   # (may be OTT but ensures that we're always testing the latest code)
   #
   # require File::Path;
   # File::Path::rmtree (["_Inline", ".Inline"], 0, 0);

   # Test for Inline and set options
   my $inline_test_dir = './.inlinepdlpp';
   mkdir $inline_test_dir unless -d $inline_test_dir;
   eval 'use Inline (Config => DIRECTORY => $inline_test_dir , FORCE_BUILD => 1)';
   if ( ! $@ ) {       # have Inline
      eval 'use Inline 0.43';
      if ( ! $@ ) {
         plan tests => 3;
      }
   }
   else {
      plan skip_all => "Skipped: Inline not installed";
   }
}

sub myshape { join ',', $_[0]->dims }

# use Inline 'INFO'; # use to generate lots of info
use Inline 'Pdlpp';

print "Inline Version: $Inline::VERSION\n";
ok(1); # ok, we made it so far

$a = sequence(3,3);

$b = $a->testinc;

ok(myshape($a) eq myshape($b));

ok(all $b == $a+1);

__DATA__

__Pdlpp__

# simple PP definition

pp_def('testinc',
	Pars => 'a(); [o] b()',
	Code => '$b() = $a() + 1;' # wow, that's complicated
);

# this tests the bug with a trailing comment and *no* newline
