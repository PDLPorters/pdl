use PDL::LiteF;

my $ctr = 1;
sub ok {
    my $result = shift ;
    print "not " unless $result ;
    print "ok $ctr\n" ;
    $ctr++;
#    print "[ $result ]\n";
}

my $ntests = 3;
print "1..$ntests\n";


use PDL::Reduce;

$a = sequence 5,5;
$b = $a->reduce('add',0);

ok(all $b == $a->sumover);
ok(all $a->reduce('add',1) == $a->mv(1,0)->sumover);
ok(all $a->reduce('mult',1) == $a->mv(1,0)->prodover);

