
use strict;

use Test;

BEGIN {

 	if ($^O =~ /win32/i) {
	  print "1..1\n";
	  print "ok 1 # Skipped: use PDL::IO::Storable instead\n";
	  exit;
	} else {
	  plan tests => 16;
	}
}

use PDL;

sub inpath {
  my ($prog) = @_;
  my $pathsep = $^O =~ /win32/i ? ';' : ':';
  my $exe = $^O =~ /win32/i ? '.exe' : '';
  for(split $pathsep,$ENV{PATH}){return 1 if -x "$_/$prog$exe"}
  return 0;
}

eval "use Convert::UU;";
my $hasuuencode = !$@ || (inpath('uuencode') && inpath('uudecode'));

unless ($hasuuencode) {
    for (1..16) {
	skip( "Skip neither uuencode/decode nor Convert:UU is available" );
    }
    exit;
}

########### First test the load...
eval "use PDL::IO::Dumper;";
if($@){print $@,"\n";}
ok(!$@);

########### Dump several items and make sure we get 'em back...
# a: trivial
# b: 0-d
# c: inline
# d: advanced expr

my ( $s, $a );

eval '$s = sdump({a=>3,b=>pdl(4),c=>xvals(3,3),d=>xvals(4,4)});';
ok(!$@);
$a = eval $s;
ok(!$@);
ok(ref $a eq 'HASH');
ok($a->{a}==3);
ok((ref $a->{b} eq 'PDL') && ($a->{b}==4));
ok((ref $a->{c} eq 'PDL') && ($a->{c}->nelem == 9) 
   && (sum(abs(($a->{c} - xvals(3,3))))<0.0000001));
ok((ref $a->{d} eq 'PDL') && ($a->{d}->nelem == 16)
   && (sum(abs(($a->{d} - xvals(4,4))))<0.0000001));

########## Dump a uuencoded expr and try to get it back...
# e: uuencoded expr
eval '$s = sdump({e=>xvals(25,25)});';
ok(!$@);

#print $s,"\n";

$a = eval $s;
ok(!$@);

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

ok((ref $a eq 'HASH'));
ok((ref $a->{e} eq 'PDL') 
   && ($a->{e}->nelem==625)
   && (sum(abs(($a->{e} - xvals(25,25))))<0.0000001));

########## Check header dumping...
eval '$a = xvals(2,2); $a->sethdr({ok=>1}); $a->hdrcpy(1); $b = xvals(25,25); $b->sethdr({ok=>2}); $b->hdrcpy(0); $s = sdump([$a,$b,yvals(25,25)]);';
ok(!$@);

$a = eval $s;
ok(!$@ && (ref $a eq 'ARRAY'));

ok(eval '$a->[0]->hdrcpy() == 1 && $a->[1]->hdrcpy() == 0');
ok(eval '($a->[0]->gethdr()->{ok}==1) && ($a->[1]->gethdr()->{ok}==2)');

# end

	




