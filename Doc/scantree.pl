use PDL::Doc;
use Getopt::Std;
use Config;
use Cwd;

$opt_v = 0;

getopts('v');
$dir = shift @ARGV;
$outdb  = shift @ARGV;

unless (defined $dir) {
	require PDL;
	($dir = $INC{'PDL.pm'}) =~ s/PDL\.pm$//i;
	umask 0022;
	print "DIR = $dir\n";
}
unless (defined $outdb) {
	$outdb = "$dir/PDL/pdldoc.db";
	print "DB  = $outdb\n";
}

$currdir = getcwd;

chdir $dir or die "can't change to $dir";
$dir = getcwd;

unlink $outdb if -e $outdb;
$onldc = new PDL::Doc ($outdb);
$onldc->scantree($dir."/PDL",$opt_v);
$onldc->scan($dir."/PDL.pm",$opt_v);

chdir $currdir;

$onldc->savedb();
