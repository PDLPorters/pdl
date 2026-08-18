use strict;
use warnings;
use PDL::Doc;
use File::Spec::Functions;

our $opt_v = -1; # verbose

my $dirarg = shift @ARGV;
my $outdb  = shift @ARGV;
unless (defined $dirarg) {
	($dirarg = $INC{'PDL.pm'}) =~ s/[\/\\]*PDL\.pm$//i;
	if ($dirarg =~ /^blib/) { $dirarg .= ",blib/script,blib/lib/Inline" }
	umask 0022;
	print "DIR = $dirarg\n";
}
my @dirs = split /,/,$dirarg;
unless (defined $outdb) {
	$outdb = "$dirs[0]/PDL/pdldoc.db";
	print "DB  = $outdb\n";
}

my $onldc = PDL::Doc->new;

foreach my $dir (@dirs) {
    $onldc->scantree($dir =~ /script|Inline$/ ? $dir : catdir($dir, "PDL"),$opt_v);
}
$onldc->scan('lib/PDL.pm');

# manually add the PDL::Index to the doc database
$onldc->scan('lib/PDL/Index.pod', $opt_v);

print STDERR "saving...\n";
unlink $outdb if -e $outdb;
$onldc->savedb($outdb);
