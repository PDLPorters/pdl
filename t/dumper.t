use PDL;

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

sub inpath {
  my ($prog) = @_;
  my $pathsep = $^O =~ /win32/i ? ';' : ':';
  my $exe = $^O =~ /win32/i ? '.exe' : '';
  for(split $pathsep,$ENV{PATH}){return 1 if -x "$_/$prog$exe"}
  return 0;
}

eval "use Convert::UU;";
$hasuuencode = !$@ || (inpath('uuencode') && inpath('uudecode'));
print "1..16\n";

unless ($hasuuencode) {
	for (1..16) {
		print "ok $_ # Skipped: neither uuencode/decode nor Convert:UU is available\n";
	}
	exit;
}

########### First test the load...
eval "use PDL::IO::Dumper;";
if($@){print $@,"\n";}
ok(1,!$@);

########### Dump several items and make sure we get 'em back...
# a: trivial
# b: 0-d
# c: inline
# d: advanced expr

eval '$s = sdump({a=>3,b=>pdl(4),c=>xvals(3,3),d=>xvals(4,4)});';
ok(2,!$@);
$a = eval $s;
ok(3,!$@);
ok(4,ref $a eq 'HASH');
ok(5,$a->{a}==3);
ok(6,(ref $a->{b} eq 'PDL') && ($a->{b}==4));
ok(7,(ref $a->{c} eq 'PDL') && ($a->{c}->nelem == 9) 
	&& (sum(abs(($a->{c} - xvals(3,3))))<0.0000001));
ok(8,(ref $a->{d} eq 'PDL') && ($a->{d}->nelem == 16)
	&& (sum(abs(($a->{d} - xvals(4,4))))<0.0000001));

########## Dump a uuencoded expr and try to get it back...
# e: uuencoded expr
eval '$s = sdump({e=>xvals(25,25)});';
ok(9,!$@);

print $s,"\n";

$a = eval $s;
ok(10,!$@);
print $@ if($@);
print "string was $s" if($@);
ok(11,(ref $a eq 'HASH'));
ok(12,(ref $a->{e} eq 'PDL') 
	&& ($a->{e}->nelem==625)
	&& (sum(abs(($a->{e} - xvals(25,25))))<0.0000001));

########## Check header dumping...
eval '$a = xvals(2,2); $a->sethdr({ok=>1}); $a->hdrcpy(1); $b = xvals(25,25); $b->sethdr({ok=>2}); $b->hdrcpy(0); $s = sdump([$a,$b,yvals(25,25)]);';
ok(13,!$@);

$a = eval $s;
ok(14,!$@ && (ref $a eq 'ARRAY'));

ok(15, eval '$a->[0]->hdrcpy() == 1 && $a->[1]->hdrcpy() == 0');
ok(16, eval '($a->[0]->gethdr()->{ok}==1) && ($a->[1]->gethdr()->{ok}==2)');

	




