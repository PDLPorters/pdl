# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

use PDL;
use PDL::Config;
use Test::More;

BEGIN{
  use PDL::Config;
  if($PDL::Config{WITH_PLPLOT}) {
    plan tests => 29;
    use_ok( "PDL::Graphics::PLplot" );
  }
  else {
    plan skip_all => "PDL::Graphics::PLplot not installed";
  }
}

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

# Use xfig driver because it should always be installed.
#my $dev = 'png';
my $dev = 'xfig';

# redirect STDERR to purge silly 'opened *.xfig' messages

require IO::File;
local *SAVEERR;
*SAVEERR = *SAVEERR;  # stupid fix to shut up -w (AKA pain-in-the-...-flag)
open(SAVEERR, ">&STDERR");
my $tmp = new_tmpfile IO::File || die "couldn't open tmpfile";
my $pos = $tmp->getpos;
local *IN;
*IN = *$tmp;  # doesn't seem to work otherwise
open(STDERR,">&IN") or warn "couldn't redirect stdder";

my ($pl, $x, $y, $min, $max, $oldwin, $nbins);


### 
# Initial test to work around font file brain damage:  for some kinds of 
# PLplot errors, control never returns to us.  FMH.
#   --CED
###

my $tmpdir  = $PDL::Config{TEMPDIR} || "/tmp";
my $tmpfile = $tmpdir . "/foo$$.$dev";

if($pid = fork()) {
	$a = waitpid($pid,0);
} else {
	sleep 1;
	$pl = PDL::Graphics::PLplot->new(DEV=>$dev,FILE=>$tmpfile);
	exit(0);	
}

ok( ($not_ok = $? & 0xff )==0 , "PLplot crash test"  );
unlink $tmpfile;

if($not_ok) {
	printf SAVEERR <<"EOERR" ;

Return value $not_ok; a is $a; pid is $pid

************************************************************************
* PLplot failed the crash test: it appears to crash its owner process. *
* This is probably due to a misconfiguration of the PLplot libraries.  *
* Next we\'ll try creating a test window from which will probably dump  *
* some (hopefully helpful) error messages and then die.                *
************************************************************************

EOERR

	open(STDERR,">&SAVEERR");
}

$pl = PDL::Graphics::PLplot->new (DEV => $dev,
				  FILE => "test2.$dev",
				  BACKGROUND => [255,255,255]);
isa_ok( $pl, "PDL::Graphics::PLplot" ) or die;

$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y,
	    BOX => [-5,10,0,200],
	    PLOTTYPE => 'LINE');
$pl->close;
ok (-s "test2.$dev" > 0, "Simple line plot");

$pl = PDL::Graphics::PLplot->new (DEV => $dev,
				  FILE => "test2a.$dev",
				  LINEWIDTH => 10,
				  BACKGROUND => [255,255,255]);
$pl->xyplot($x, $y,
	    BOX => [-5,10,0,200],
	    PLOTTYPE => 'LINE');
$pl->close;
ok (-s "test2a.$dev" > 0, "Simple line plot with LINEWIDTH specified");

$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test3.$dev", 
				       BACKGROUND => 'WHITE');
$pl->xyplot($x, $y, PLOTTYPE => 'POINTS', COLOR => 'BLUEVIOLET', SYMBOL => 1, SYMBOLSIZE => 4);
$pl->close;
ok (-s "test3.$dev" > 0, "Symbol plot");

$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test4.$dev", FRAMECOLOR => 'BLUE');
$pl->xyplot($x, $y, PLOTTYPE => 'LINEPOINTS', COLOR => [50,230,30]);
$pl->close;
ok (-s "test4.$dev" > 0, "Lines and symbols");

$y = sequence(30)+1;
my $m = (50* (exp(1/$y**2) - 1) * random (30,20))->xchg(0,1);
my ($mean, $rms) = statsover($m);
my $x1 = $mean + $rms;
my $x2 = $mean - $rms;
my $n  = 500 - exp($y/5);

#$pl = PDL::Graphics::PLplot->new (DEV => "xwin", FILE => "trillian.cosmic.ucar.edu:0");

# Setting text to 1 like this does not work.  text is hard coded in ps.c ;(
#$pl = PDL::Graphics::PLplot->new (DEV => "psc", FILE => "test5.ps", OPTS => {'text' => '1'});

$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test5.$dev");
$pl->xyplot($x1,   $y, COLOR => 'GREEN',
	               BOX   => [($mean - $rms)->minmax, $y->minmax],
	               XBOX  => 'bnst', # bottom line, bottom numbers, ticks, subticks
	               YBOX  => 'bnst', # left line, left numbers, ticks, subticks
	               TITLE => 'Test statistics plot',
	               XLAB => 'X label',
	               YLAB => 'Y label');

$pl->xyplot($x2,   $y, COLOR => 'GREEN');
$pl->xyplot($mean, $y, COLOR => 'RED');
$pl->xyplot($n,    $y, COLOR => 'BLUE',
	               XBOX => 'cmst', # top line, top numbers, ticks, subticks
	               YBOX => 'cst',  # right line, ticks, subticks
	               BOX => [0, int(1.1*$n->max), $y->minmax]);
$pl->text("Count", COLOR => 'PINK',
	           TEXTPOSITION => ['t', 3, 0.5, 0.5]); # top, 3 units out, string ref. pt in
                                                        # center of string, middle of axis

$pl->close;
ok (-s "test5.$dev" > 0, "Sample layer statistics plot");

# test of setting page size.
$pl = PDL::Graphics::PLplot->new (DEV => $dev,
				       FILE => "test6.$dev",
				       PAGESIZE => [50,80]);
$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE');
$pl->close;
ok (-s "test6.$dev" > 0, "Setting pagesize");

# test of lines with gaps (plgapline)
$pl = PDL::Graphics::PLplot->new (DEV => $dev,
				  FILE => "test7.$dev");
$x  = sequence(10);
$y  = $x**2;
$x->inplace->setbadat(5); # insert gap
$y->inplace->setbadat(5); # insert gap
$pl->xyplot($x, $y, PLOTTYPE => 'LINE');
$pl->close;
ok (-s "test7.$dev" > 0, "Line plot with gaps (plgapline)");

# test of setting JUSTify = 1
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test8.$dev");
$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINEPOINTS', JUST => 1);
$pl->close;
ok (-s "test8.$dev" > 0, "Setting JUSTify = 1");

$pl = PDL::Graphics::PLplot->new (DEV  => $dev, FILE => "test9.$dev");

$pl->text("Test string outside of window", TEXTPOSITION => ['T', 1, 0, 0]);
$pl->text("Test string inside window",     TEXTPOSITION => [0, 0, 0.5, 0.5, 0]);
$pl->close;
ok (-s "test9.$dev" > 0, "Printing text inside and outside of plot window");

my $pi = atan2(1,1)*4;
my $a  = (sequence(20)/20) * 2 * $pi;
my $b  = sin($a);
my $c  = cos($a);

# test rainbow point plotting with color key
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test10.$dev");
$pl->xyplot ($a, $b, SYMBOL => 850, SYMBOLSIZE => 1.5, PALETTE => 'RAINBOW', PLOTTYPE => 'POINTS', COLORMAP => $c);
$pl->colorkey ($c, 'v', VIEWPORT => [0.93, 0.96, 0.15, 0.85]);
$pl->colorkey ($c, 'h', VIEWPORT => [0.15, 0.85, 0.92, 0.95]);
$pl->close;
ok (-s "test10.$dev" > 0, "Colored symbol plot with key");

# test reverse rainbow point plotting with color key
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test10a.$dev");
$pl->xyplot ($a, $b, SYMBOL => 850, SYMBOLSIZE => 1.5, PALETTE => 'REVERSERAINBOW', PLOTTYPE => 'POINTS', COLORMAP => $c);
$pl->colorkey ($c, 'v', VIEWPORT => [0.93, 0.96, 0.15, 0.85]);
$pl->colorkey ($c, 'h', VIEWPORT => [0.15, 0.85, 0.92, 0.95]);
$pl->close;
ok (-s "test10a.$dev" > 0, "Colored symbol plot with key: reverse rainbow");

# Test plot and color key (low level interface)
plsdev ($dev);
plsfnam ("test11.$dev");
plspage (0,0, 600,600, 0,0);
plinit();
pladv (0);
plvsta();
plwind (0, 1, 0, 1);
plvpor(0.1,0.85,0.1,0.9);
plwind (0, 10, 0, 100);
plcol0(1);
plbox (0, 0, 0, 0, 'BCNST', 'BCNST');
plpoin($x, $y, 2);
plvpor(0.86,0.90,0.1,0.9);
plwind (0, 10, 0, 100);
plbox (0, 0, 0, 0, '', 'TM');
plscmap1l (0, PDL->new(0,1), PDL->new(0,360), PDL->new(0.5, 0.5), PDL->new(1,1), pdl []);
for (my $i=0;$i<10;$i++) {
  plcol1($i/10);
  plfill (PDL->new(0,10,10,0), PDL->new($i*10,$i*10,($i+1)*10,($i+1)*10));
}
plend1();

ok (-s "test11.$dev" > 0, "Colored symbol plot with key, via low level interface");

# Test shade plotting (low level interface)
plsdev ($dev);
plsfnam ("test12.$dev");
plspage (0,0, 600,600, 0,0);
plinit();
pladv (0);
plvpor(0.1, 0.9, 0.1, 0.9); 
plwind (-1, 1, -1, 1);
plpsty(0); 

my $nx = 35;
my $ny = 46;
$x = (sequence($nx) - ($nx/2))/($nx/2);
$y = (sequence($ny) - ($ny/2))/(($ny/2) - 1.0);
my $xv = $x->dummy(1, $y->nelem);
my $yv = $y->dummy(0, $x->nelem);
my $z = -sin(7*$xv) * cos (7*$yv) + $xv**2 - $yv**2;
my $nsteps = 15;
my ($zmin, $zmax) = $z->minmax;
my $clevel = ((sequence($nsteps)*(($zmax - $zmin)/($nsteps-1))) + $zmin);
my $fill_width = 2;
my $cont_color = 0;
my $cont_width = 0;
my $xmap = ((sequence($nx)*(2/($nx-1))) + -1); # map X coords linearly to -1 to 1
my $ymap = ((sequence($ny)*(2/($ny-1))) + -1);
my $grid = plAllocGrid ($xmap, $ymap);
plshades($z, -1, 1, -1, 1,
         $clevel, $fill_width,
         $cont_color, $cont_width, 1, 
	 0, \&pltr1, $grid);
plend1();

ok (-s "test12.$dev" > 0, "3D color plot, low level interface");

# test shade plots with higher level interface. 
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test13.$dev");
$pl->shadeplot ($z, $nsteps, BOX => [-1, 1, -1, 1], PALETTE => 'RAINBOW'); 
$pl->colorkey ($z, 'v', VIEWPORT => [0.93, 0.96, 0.15, 0.85]);
$pl->close;
ok (-s "test13.$dev" > 0, "3D color plot, high level interface");

# Test histogram plotting (low level interface)
plsdev ($dev);
plsfnam ("test14.$dev");
plspage (0,0, 600,600, 0,0);
plinit();
pladv (0);
plvpor(0.1, 0.9, 0.1, 0.9); 
$x = random(100)*100;
($min, $max) = $x->minmax;
$nbins = 15;
$oldwin = 1; # dont call plenv

plwind ($min, $max, 0, 100);
plbox (0, 0, 0, 0, 'bcnst', 'bcnst');

plhist ($x, $min, $max, $nbins, $oldwin);
plend1();

ok (-s "test14.$dev" > 0, "Histogram plotting, low level interface");

# test histograms with higher level interface. 
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test15.$dev");
$pl->histogram ($x, $nbins, BOX => [$min, $max, 0, 100]); 
$pl->close;
ok (-s "test15.$dev" > 0, "Histogram plotting, high level interface");

# Test multiple plots per page (low level interface)
plsdev ($dev);
plsfnam ("test16.$dev");
plspage (0,0, 300,600, 0,0);
plssub (1,2);
plinit();
pladv (1);
plvpor(0.1, 0.9, 0.1, 0.9); 
$x = random(100)*100;
($min, $max) = $x->minmax;
$nbins = 15;
$oldwin = 1; # dont call plenv
plwind ($min, $max, 0, 100);
plbox (0, 0, 0, 0, 'bcnst', 'bcnst');
plhist ($x, $min, $max, $nbins, $oldwin);

pladv (2);
plvpor(0.1, 0.9, 0.1, 0.9); 
$x = random(200)*100;
($min, $max) = $x->minmax;
$nbins = 15;
$oldwin = 1; # dont call plenv

plwind ($min, $max, 0, 100);
plbox (0, 0, 0, 0, 'bcnst', 'bcnst');

plhist ($x, $min, $max, $nbins, $oldwin);

plend1();

ok (-s "test16.$dev" > 0, "Multiple plots per page, low level interface");

# test multiple pages per plot (high level interface)
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test17.$dev", SUBPAGES => [1,2]);
$pl->histogram ($x, $nbins, BOX => [$min, $max, 0, 100]); 
$pl->histogram ($x, $nbins, BOX => [$min, $max, 0, 100], SUBPAGE => 2); 
$pl->close;
ok (-s "test17.$dev" > 0, "Multiple plots per page, high level interface");

$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test18.$dev");
$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE', LINESTYLE => 2);
$pl->close;
ok (-s "test18.$dev" > 0, "Setting LINESTYLE");

# test setting plot orientation
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test19.$dev", ORIENTATION => 1);
$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE', LINESTYLE => 2);
$pl->close;
ok (-s "test19.$dev" > 0, "Setting plot orientation");

# test symbol plotting
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => "test20.$dev");
$pl->setparm (BOX => [0,200,0,200]);
for (my $x=0;$x<20;$x++) {
  for (my $y=0;$y<20;$y++) {
    my $xp = pdl(10*$x);
    my $yp = pdl(10*$y);
    $pl->xyplot($xp, $yp, PLOTTYPE => 'POINTS', SYMBOL => 20*$x+$y);
  }
}
$pl->close;
ok (-s "test20.$dev" > 0, "Symbol plotting");

# test label plotting in multiple subpage plots
$pl = PDL::Graphics::PLplot->new(DEV => $dev, 
				      FILE => "test21.$dev", 
				      PAGESIZE => [500,900],
				      SUBPAGES => [1,6]);
my @colors = qw(GREEN BLUE RED BROWN BLACK YELLOW);

for my $i (0..5) {

  my $x  = sequence(100)*0.1;
  my $y  = sin($x);

  $pl->xyplot($x, $y, 
	      COLOR => $colors[$i],
	      SUBPAGE => $i+1,
	      TITLE => "Title $i",
	      XLAB => "1 to 10", 
	      YLAB => "sin(x)");

}

$pl->close;
ok (-s "test21.$dev" > 0, "Multiple subpages");

# test bar graphs
$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => "test22.$dev");
$pl->bargraph([map { sprintf ("2002.%03d", $_) } (1..100)], 100*random(100), COLOR => 'BLUE');
$pl->close;
ok (-s "test22.$dev" > 0, "Bar graph");

$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => "test23.$dev");
my @labels = ((map { sprintf ("2001.%03d", $_) } (240..365)), (map { sprintf ("2002.%03d", $_) } (1..100)));
$pl->bargraph(\@labels, 100*random(scalar(@labels)), COLOR => 'GREEN');
$pl->close;
ok (-s "test23.$dev" > 0, "Bar graph part 2");

$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => "test23a.$dev");
@labels = ((map { sprintf ("2001.%03d", $_) } (240..365)), (map { sprintf ("2002.%03d", $_) } (1..100)));
$pl->bargraph(\@labels, 100*random(scalar(@labels)), COLOR => 'GREEN', MAXBARLABELS => 30);
$pl->close;
ok (-s "test23a.$dev" > 0, "Bar graph part 3");

$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => "test24.$dev");
$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE', XERRORBAR => ones(10)*0.5, XTICK => 2,  NXSUB => 5, 
                                        YERRORBAR => $y*0.1,       YTICK => 20, NYSUB => 10,
                                        MINTICKSIZE => 2, MAJTICKSIZE => 3);
$pl->close;
ok (-s "test24.$dev" > 0, "Setting error bars and tick size");

unlink glob ("test*.$dev");

# stop STDERR redirection and examine output

open(STDERR, ">&SAVEERR");
$tmp->setpos($pos);  # rewind
my $txt = join '',<IN>;
close IN; undef $tmp;

print "\ncaptured STDERR: ('Opened ...' messages are harmless)\n$txt\n";
$txt =~ s/Opened test\d*\.$dev\n//sg;
warn $txt unless $txt =~ /\s*/;

# Local Variables:
# mode: cperl
# End:
