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
# Copy-by-reference test is obsolete; check contents instead (CED 12-Apr-2003)
#   return $ah==$bh
  return join("",%{$ah}) eq join("",%{$bh});
}

print "1..9\n";

$a = zeroes(20);
$a->hdrcpy(1);
$a->dump;
$a->sethdr( {Field1=>'arg1',
	     Field2=>'arg2'});
print "a: ",$a->gethdr(),"\n";
ok(1,$a->hdrcpy);

$b = $a+1;
print "b: ",$b->gethdr(),"\n";
ok(2, defined($b->gethdr));
ok(3,hdrcmp($a,$b));

$b = ones(20) + $a;
print "b: ",$b->gethdr(),"\n";
ok(4, defined($b->gethdr));
ok(5,hdrcmp($a,$b));

$c = $a->slice('0:5');
print "c: ",$c->gethdr(),"\n";
ok(6,hdrcmp($a,$c));

$d = $a->copy;
print "d: ",$d->gethdr(),"\n";
ok(7,hdrcmp($a,$d));

$a->hdrcpy(0);
ok(8,defined($a->slice('3')->hdr) && !( keys (%{$a->slice('3')->hdr})));
ok(9,!defined($a->slice('3')->gethdr));
