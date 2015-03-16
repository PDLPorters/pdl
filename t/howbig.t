# Test datatype sizes in bytes are correct

use Test::More tests => 6;
use PDL::LiteF;
use PDL::Core ':Internal'; # For howbig()

ok(howbig(byte(42)->get_datatype)==1);
ok(howbig(short(42)->get_datatype)==2);
ok(howbig(ushort(42)->get_datatype)==2);
ok(howbig(long(42)->get_datatype)==4);
ok(howbig(float(42)->get_datatype)==4);
ok(howbig(double(42)->get_datatype)==8);

