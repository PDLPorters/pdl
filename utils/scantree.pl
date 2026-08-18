use strict;
use warnings;
use PDL::Doc;

my $dirarg = shift @ARGV;
die "$0: No dirarg given\n" if !defined $dirarg;
my @dirs = split /,/,$dirarg;
my $outdb  = shift @ARGV;
die "$0: No outdb given\n" if !defined $outdb;

PDL::Doc::gen_db($outdb, @dirs);
