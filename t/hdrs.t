use PDL::LiteF;

$|=1;

#  PDL::Core::set_debugging(1);
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub hdrcmp {
  my ($ah,$bh) = map {$_->gethdr} @_;
  return $ah == $bh;
}

print "1..5\n";

$a = zeroes(20);
$a->set_hdrcpy(1);
$a->dump;
$a->sethdr( {Field1=>'arg1',
	     Field2=>'arg2'});
print "a: ",$a->gethdr(),"\n";

$b = $a+1;
print "b: ",$b->gethdr(),"\n";
ok(1, defined($b->gethdr));
ok(2,hdrcmp($a,$b));

$c = $a->slice('0:5');
print "c: ",$c->gethdr(),"\n";
ok(3,hdrcmp($a,$c));

$d = $a->copy;
print "d: ",$d->gethdr(),"\n";
ok(4,hdrcmp($a,$d));

$a->set_hdrcpy(0);
ok(5,!defined($a->slice('3')->gethdr));

