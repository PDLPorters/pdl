BEGIN {
  print "1..4\n";
}
use PDL;

sub ok($;$$){
  my $no=shift;
  my $result = shift;
  print "not " unless $result;
  print "ok $no\n";
}

##1 Make sure the library loads
my($n);
print STDERR ++$n,"...";
eval 'use PDL::DiskCache;';
if($@) {print $@,"\n";}
ok(1,!$@);

##2 Make a DiskCache object
my($d) = "/tmp/test-$$/";
`mkdir $d`;

eval <<'BAR'
  do {
    my($a) = diskcache(["${d}1","${d}2","${d}3"],{verbose=>1});
    $a->[0] = zeroes(10,10);
    $a->[1] = xvals(10,10);
    $a->[2] = yvals(10,10);
  } while(0);
BAR
  ;
ok(2,!$@);

ok(3, (-e "${d}1") && (-e "${d}2") && (-e "${d}3"));

eval <<'BAZ'
  do {
    my($b) = diskcache(["${d}1","${d}2","${d}3"],{ro=>1});
    ok(4,($b->[0]->sum == 0) && ($b->[1]->sum == xvals(10,10)->sum));
  }
BAZ
  ;

`rm -rf $d`;
    

    
    
