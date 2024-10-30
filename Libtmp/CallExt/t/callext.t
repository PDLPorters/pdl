# Example of how to use callext() - also see callext.c

use strict;
use warnings;
use Test::More;
use PDL;
use PDL::CallExt;
use PDL::Core::Dev;
use Config;
use File::Spec;
use Test::PDL;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# Create the filenames
my $cfile = File::Spec->catfile('t', 'callext.c');
my $out   = File::Spec->catfile('t', 'callext.'.$Config{dlext});
my $obj   = File::Spec->catfile('t', 'callext'.$Config{obj_ext});

END { unlink File::Spec->catfile(qw(t callext.pdb)), $obj, $out } # MS compiler
eval { callext_cc($cfile, PDL_INCLUDE(), '', $out) }; # Compile the code

SKIP: {
is $@, '', 'callext_cc no error' or skip 'callext_cc failed', 1;
my $y = sequence(5,4)+2;  # Create PDL
my $x = $y*20+100;        # Another
is_pdl loglog($x,$y), log(float($x))/log(float($y));
}

done_testing;

# Return log $x to base $y using callext() routine -
# perl wrapper makes this nice and easy to use.

sub loglog {
  die 'Usage: loglog($x,$y)' if scalar(@_)!=2;
  my ($x, $y) = map float(PDL->topdl($_)), @_;
  my $ret = $x->copy; # Make copy of $x to return
  callext(File::Spec->rel2abs($out), "loglog_ext", $ret, $y);
  return $ret;
}
