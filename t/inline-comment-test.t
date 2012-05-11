# This test checks that multiline comments in user code does not cause
# compilation errors. Strictly speaking, this is not really an Inline test,
# but Inline happens to provide the simplest framework for performing these
# tests. :-)
#  -- DCM, April 16, 2012

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
ok(1, 'Everything seems to have compiled');

$a = sequence(3,3);

$b = $a->testinc;

ok(all ($b == $a+1), 'Sanity check runs correctly');

# Test the inability to comment-out a threadloop. This is documented on the
# 11th page of the PDL::PP chapter of the PDL book. If somebody ever fixes this
# wart, this test will fail, in which case the book's text should be updated.
$b = $a->testinc2;
TODO: {
	# Note: This test appears to fail on Cygwin, for reasons that are yet unknown.
	local $TODO = 'Cygwin does not have this wart?!' if $^O eq 'cygwin';
	ok(not (all $b == $a + 1), 'WART: commenting out a threadloop does not work')
		or diag("\$a is $a and \$b is $b");
}

__DATA__

__Pdlpp__

# simple PP definition with user irritation tests :-)

pp_def('testinc',
	Pars => 'a(); [o] b()',
	Code => q{
	   /* emulate user debugging */
	   
	   /* Why doesn't this work???!!!! */
       threadloop %{
    /*         printf("  %f, %f\r", $a(), $b());
             printf("  Here\n");
	*/
    
	         /* Sanity check */
	         $b() = $a() + 1;
   
         %}
	   
	},
);

# make sure that if the word "threadloop" appears, later automatic threadloops
# will not be generated, even if the original threadloop was commented-out

pp_def('testinc2',
	Pars => 'a(); [o] b()',
	Code => q{
	   /* emulate user debugging */
	   
	   /* Why doesn't this work???!!!! */
   /*    threadloop %{
             printf("  %f, %f\r", $a(), $b());
             printf("  Here\n");
         %}
	*/
          /* Sanity check */
          $b() = $a() + 1;
	   
	},
);
