use blib ".";
use blib "../..";
use PDL;
# use PDL::Bench;
BEGIN{
require "Bench.pm";
PDL::Bench->import();
}


do_benchmark();
