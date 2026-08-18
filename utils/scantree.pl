use strict;
use warnings;
use PDL::Doc;
use File::Spec::Functions;
use File::Basename qw(dirname);
use File::Path qw(make_path);

our $opt_v = 0; # verbose

my $dirarg = shift @ARGV;
die "$0: No dirarg given\n" if !defined $dirarg;
print "DIR = $dirarg\n";
my @dirs = split /,/,$dirarg;
my $outdb  = shift @ARGV;
die "$0: No outdb given\n" if !defined $outdb;
print "DB  = $outdb\n";

my $onldc = PDL::Doc->new;

foreach my $dir (@dirs) {
    $onldc->scantree($dir =~ /script|Inline$/ ? $dir : catdir($dir, "PDL"),$opt_v);
}
$onldc->scan('lib/PDL.pm');

print STDERR "saving...\n";
make_path dirname $outdb;
umask 0022;
$onldc->savedb($outdb);
