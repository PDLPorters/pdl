# Test datatype sizes in bytes are correct

use PDL::LiteF;
use PDL::Core ':Internal'; # For howbig()

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

print "1..6\n";

ok(1, howbig(byte(42)->get_datatype)==1);
ok(2, howbig(short(42)->get_datatype)==2);
ok(3, howbig(ushort(42)->get_datatype)==2);
ok(4, howbig(long(42)->get_datatype)==4);
ok(5, howbig(float(42)->get_datatype)==4);
ok(6, howbig(double(42)->get_datatype)==8);

