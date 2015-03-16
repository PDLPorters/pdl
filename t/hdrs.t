use Test::More tests => 9;
use PDL::LiteF;

$|=1;

#  PDL::Core::set_debugging(1);
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub hdrcmp {
  my ($ah,$bh) = map {$_->gethdr} @_;
# Copy-by-reference test is obsolete; check contents instead (CED 12-Apr-2003)
#   return $ah==$bh
  my %ahh = %{$ah};
  my (@ahhkeys) = sort keys %ahh;
  my %bhh = %{$bh};
  my (@bhhkeys) =  sort keys %bhh;
  return join("",@bhh{@bhhkeys}) eq join("",@ahh{@ahhkeys});
}

$a = zeroes(20);
$a->hdrcpy(1);
$a->dump;
$a->sethdr( {Field1=>'arg1',
	     Field2=>'arg2'});
note "a: ",$a->gethdr(),"\n";
ok($a->hdrcpy);

$b = $a+1;
note "b: ",$b->gethdr(),"\n";
ok( defined($b->gethdr));
ok(hdrcmp($a,$b));

$b = ones(20) + $a;
note "b: ",$b->gethdr(),"\n";
ok( defined($b->gethdr));
ok(hdrcmp($a,$b));

$c = $a->slice('0:5');
note "c: ",$c->gethdr(),"\n";
ok(hdrcmp($a,$c));

$d = $a->copy;
note "d: ",$d->gethdr(),"\n";
ok(hdrcmp($a,$d));

$a->hdrcpy(0);
ok(defined($a->slice('3')->hdr) && !( keys (%{$a->slice('3')->hdr})));
ok(!defined($a->slice('3')->gethdr));
