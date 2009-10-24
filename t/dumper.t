
use strict;

use Test::More;

BEGIN {

   if ($^O =~ /win32/i) {
      plan skip_all => "On win32 use PDL::IO::Storable instead\n";
   }

   sub inpath {
      my ($prog) = @_;
      my $pathsep = $^O =~ /win32/i ? ';' : ':';
      my $exe = $^O =~ /win32/i ? '.exe' : '';
      for ( split $pathsep,$ENV{PATH} ) {
         return 1 if -x "$_/$prog$exe"
      }
      return;
   }

   eval "use Convert::UU;";
   my $hasuuencode = !$@ || (inpath('uuencode') && inpath('uudecode'));

   if ($hasuuencode) {
      plan tests => 16;
   }
   else {
      plan skip_all => "Skip neither uuencode/decode nor Convert:UU is available\n";
   }

   use PDL;
}

########### First test the load...
use_ok('PDL::IO::Dumper');

########### Dump several items and make sure we get 'em back...
# a: trivial
# b: 0-d
# c: inline
# d: advanced expr

my ( $s, $a );

eval '$s = sdump({a=>3,b=>pdl(4),c=>xvals(3,3),d=>xvals(4,4)});';
ok(!$@, 'Call sdump()');
$a = eval $s;
ok(!$@, 'Can eval dumped data code') or diag("The output string was '$s'\n");
ok(ref $a eq 'HASH', 'HASH was restored');
ok(($a->{a}==3), 'SCALAR value restored ok');
ok(((ref $a->{b} eq 'PDL') && ($a->{b}==4)), '0-d PDL restored ok');
ok(((ref $a->{c} eq 'PDL') && ($a->{c}->nelem == 9) 
   && (sum(abs(($a->{c} - xvals(3,3))))<0.0000001)), '3x3 PDL restored ok');
ok(((ref $a->{d} eq 'PDL') && ($a->{d}->nelem == 16)
   && (sum(abs(($a->{d} - xvals(4,4))))<0.0000001)), '4x4 PDL restored ok');

########## Dump a uuencoded expr and try to get it back...
# e: uuencoded expr
eval '$s = sdump({e=>xvals(25,25)});';
ok(!$@, 'sdump() of 25x25 PDL to test uuencode dumps');

#print $s,"\n";

$a = eval $s;
ok(!$@, 'Can eval dumped 25x25 PDL');

# $s and $@ can be long so try and make things a bit clearer in the
# output
#
if ( $@ ) {
    print "--- ERROR ---\n";
    print "--Error message start:\n";
    print $@;
    print "\n--Error message end:\n";
    print "String was:\n$s\n";
    print "--- ERROR (end) ---\n";
}

ok((ref $a eq 'HASH'), 'HASH structure for uuencoded 25x25 PDL restored');
ok(((ref $a->{e} eq 'PDL') 
   && ($a->{e}->nelem==625)
   && (sum(abs(($a->{e} - xvals(25,25))))<0.0000001)), 'Verify 25x25 PDL restored data');

########## Check header dumping...
eval '$a = xvals(2,2); $a->sethdr({ok=>1}); $a->hdrcpy(1); $b = xvals(25,25); $b->sethdr({ok=>2}); $b->hdrcpy(0); $s = sdump([$a,$b,yvals(25,25)]);';
ok(!$@, 'Check header dumping');

$a = eval $s;
ok((!$@ && (ref $a eq 'ARRAY')), 'ARRAY can restore');

ok(eval('$a->[0]->hdrcpy() == 1 && $a->[1]->hdrcpy() == 0'), 'Check hdrcpy()\'s persist');
ok(eval('($a->[0]->gethdr()->{ok}==1) && ($a->[1]->gethdr()->{ok}==2)'), 'Check gethdr() values persist');

# end
