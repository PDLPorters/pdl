# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

use strict;
use warnings;
use PDL;
use PDL::Config;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;

######################### We start with some black magic to print on failure.

BEGIN{
  use PDL::Config;
  if($PDL::Config{WITH_PLPLOT}) {
    if($^O =~ /mswin/i) {
      warn "No PLPLOT_LIB env var set - this script will die after the first test if the font files are not found"
        if !$ENV{PLPLOT_LIB};
    }
    plan tests => 37;
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

# Use svg driver because it should always be installed.
my $dev = 'svg';

my ($pl, $x, $y, $min, $max, $oldwin, $nbins);


###
# Initial test to work around font file brain damage:  for some kinds of
# PLplot errors, control never returns to us.  FMH.
#   --CED
###

my $tmpdir  = tempdir( CLEANUP => 1 );
unless($^O =~ /mswin/i) { # Causes problems on Windows.
  my $tmpfile = File::Spec->catfile($tmpdir, "foo.$dev");

# comment this out for testing!!!
  #my $pid = 0; my $a = 'foo';
my ($pid,$not_ok);
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
	printf <<"EOERR" ;

Return value $not_ok; a is $a; pid is $pid

************************************************************************
* PLplot failed the crash test: it appears to crash its owner process. *
* This is probably due to a misconfiguration of the PLplot libraries.  *
* Next we\'ll try creating a test window from which will probably dump  *
* some (hopefully helpful) error messages and then die.                *
************************************************************************

EOERR

  }
}
else { # MS Windows only
	my $ret = system(qq{"$^X" -Mblib -MPDL -MPDL::Graphics::PLplot -e "$pl = PDL::Graphics::PLplot->new(DEV=>\"xfig\",FILE=>\"foo.xfig\")"});
	ok( $ret == 0 , "PLplot crash test"  );
	unlink 'foo.xfig';
}

my $tmpfile02 = File::Spec->catfile($tmpdir, "test02.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev,
				  FILE => $tmpfile02,
				  BACKGROUND => [255,255,255]);
isa_ok( $pl, "PDL::Graphics::PLplot" ) or die;

$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y,
	    BOX => [-5,10,0,200],
	    PLOTTYPE => 'LINE');
$pl->close;
ok (-s $tmpfile02 > 0, "Simple line plot");

my $tmpfile02a = File::Spec->catfile($tmpdir, "test02a.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev,
				  FILE => $tmpfile02a,
				  LINEWIDTH => 10,
				  BACKGROUND => [255,255,255]);
$pl->xyplot($x, $y,
	    BOX => [-5,10,0,200],
	    PLOTTYPE => 'LINE');
$pl->close;
ok (-s $tmpfile02a > 0, "Simple line plot with LINEWIDTH specified");

my $tmpfile03 = File::Spec->catfile($tmpdir, "test03.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile03,
				       BACKGROUND => 'WHITE');
$pl->xyplot($x, $y, PLOTTYPE => 'POINTS', COLOR => 'BLUEVIOLET', SYMBOL => 1, SYMBOLSIZE => 4);
$pl->close;
ok (-s $tmpfile03 > 0, "Symbol plot");

my $tmpfile04 = File::Spec->catfile($tmpdir, "test04.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile04, FRAMECOLOR => 'BLUE');
$pl->xyplot($x, $y, PLOTTYPE => 'LINEPOINTS', COLOR => [50,230,30]);
$pl->close;
ok (-s $tmpfile04 > 0, "Lines and symbols");

$y = sequence(30)+1;
my $m = (50* (exp(1/$y**2) - 1) * random (30,20))->xchg(0,1);
my ($mean, $rms) = statsover($m);
my $x1 = $mean + $rms;
my $x2 = $mean - $rms;
my $n  = 500 - exp($y/5);

#$pl = PDL::Graphics::PLplot->new (DEV => "xwin", FILE => "trillian.cosmic.ucar.edu:0");

# Setting text to 1 like this does not work.  text is hard coded in ps.c ;(
#$pl = PDL::Graphics::PLplot->new (DEV => "psc", FILE => "test05.ps", OPTS => {'text' => '1'});

my $tmpfile05 = File::Spec->catfile($tmpdir, "test05.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile05);
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
ok (-s $tmpfile05 > 0, "Sample layer statistics plot");

# test of setting page size.
my $tmpfile06 = File::Spec->catfile($tmpdir, "test06.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev,
				       FILE => $tmpfile06,
				       PAGESIZE => [50,80]);
$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE');
$pl->close;
ok (-s $tmpfile06 > 0, "Setting pagesize");

# test of lines with gaps (plgapline)
my $tmpfile07 = File::Spec->catfile($tmpdir, "test07.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev,
				  FILE => $tmpfile07);
$x  = sequence(10);
$y  = $x**2;
$x->inplace->setbadat(5); # insert gap
$y->inplace->setbadat(5); # insert gap
$pl->xyplot($x, $y, PLOTTYPE => 'LINE');
$pl->close;
ok (-s $tmpfile07 > 0, "Line plot with gaps (plgapline)");

# test of setting JUSTify = 1
my $tmpfile08 = File::Spec->catfile($tmpdir, "test08.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile08);
$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINEPOINTS', JUST => 1);
$pl->close;
ok (-s $tmpfile08 > 0, "Setting JUSTify = 1");

my $tmpfile09 = File::Spec->catfile($tmpdir, "test09.$dev");
$pl = PDL::Graphics::PLplot->new (DEV  => $dev, FILE => $tmpfile09);

$pl->text("Test string outside of window", TEXTPOSITION => ['T', 1, 0, 0]);
$pl->text("Test string inside window",     TEXTPOSITION => [0, 0, 0.5, 0.5, 0]);
$pl->close;
ok (-s $tmpfile09 > 0, "Printing text inside and outside of plot window");

my $pi = atan2(1,1)*4;
my $a  = (sequence(20)/20) * 2 * $pi;
my $b  = sin($a);
my $c  = cos($a);

# test rainbow point plotting with color key
my $tmpfile10 = File::Spec->catfile($tmpdir, "test10.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile10);
$pl->xyplot ($a, $b, SYMBOL => 850, SYMBOLSIZE => 1.5, PALETTE => 'RAINBOW', PLOTTYPE => 'POINTS', COLORMAP => $c);
$pl->colorkey ($c, 'v', VIEWPORT => [0.93, 0.96, 0.15, 0.85]);
$pl->colorkey ($c, 'h', VIEWPORT => [0.15, 0.85, 0.92, 0.95]);
$pl->close;
ok (-s $tmpfile10 > 0, "Colored symbol plot with key");

# test reverse rainbow point plotting with color key
my $tmpfile10a = File::Spec->catfile($tmpdir, "test10a.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile10a);
$pl->xyplot ($a, $b, SYMBOL => 850, SYMBOLSIZE => 1.5, PALETTE => 'REVERSERAINBOW', PLOTTYPE => 'POINTS', COLORMAP => $c);
$pl->colorkey ($c, 'v', VIEWPORT => [0.93, 0.96, 0.15, 0.85]);
$pl->colorkey ($c, 'h', VIEWPORT => [0.15, 0.85, 0.92, 0.95]);
$pl->close;
ok (-s $tmpfile10a > 0, "Colored symbol plot with key: reverse rainbow");

# Test plot and color key (low level interface)
plsdev ($dev);
my $tmpfile11 = File::Spec->catfile($tmpdir, "test11.$dev");
plsfnam ($tmpfile11);
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

# view port dimensions in normalized device coordinates
my ($dev_xmin, $dev_xmax, $dev_ymin, $dev_ymax) = plgvpd();

# view port dimensions in world coordinates
my ($wld_xmin, $wld_xmax, $wld_ymin, $wld_ymax) = plgvpw();
plvpor(0.86,0.90,0.1,0.9);
plwind (0, 10, 0, 100);
plbox (0, 0, 0, 0, '', 'TM');
plscmap1l (0, PDL->new(0,1), PDL->new(0,360), PDL->new(0.5, 0.5), PDL->new(1,1), pdl []);
for (my $i=0;$i<10;$i++) {
  plcol1($i/10);
  plfill (PDL->new(0,10,10,0), PDL->new($i*10,$i*10,($i+1)*10,($i+1)*10));
}
plend1();

ok (-s $tmpfile11 > 0, "Colored symbol plot with key, via low level interface");

ok (sum(pdl(0.1, 0.85, 0.1, 0.9) - pdl($dev_xmin->sclr, $dev_xmax->sclr, $dev_ymin->sclr, $dev_ymax->sclr)) == 0,
    "plgvpd call works correctly");
ok (abs(sum(pdl(-0.0001, 10.0001, -0.001, 100.001) - pdl($wld_xmin->sclr, $wld_xmax->sclr, $wld_ymin->sclr, $wld_ymax->sclr))) < 0.000001,
    "plgvpw call works correctly");

# Test shade plotting (low level interface)
my $tmpfile12 = File::Spec->catfile($tmpdir, "test12.$dev");
plsdev ($dev);
plsfnam ($tmpfile12);
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

ok (-s $tmpfile12 > 0, "3D color plot, low level interface");

# test shade plots with higher level interface.
my $tmpfile13 = File::Spec->catfile($tmpdir, "test13.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile13);
$pl->shadeplot ($z, $nsteps, BOX => [-1, 1, -1, 1], PALETTE => 'RAINBOW');
$pl->colorkey ($z, 'v', VIEWPORT => [0.93, 0.96, 0.15, 0.85]);
$pl->close;
ok (-s $tmpfile13 > 0, "3D color plot, high level interface");

# Test histogram plotting (low level interface)
my $tmpfile14 = File::Spec->catfile($tmpdir, "test14.$dev");
plsdev ($dev);
plsfnam ($tmpfile14);
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

ok (-s $tmpfile14 > 0, "Histogram plotting, low level interface");

# test histograms with higher level interface.
my $tmpfile15 = File::Spec->catfile($tmpdir, "test15.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile15);
$pl->histogram ($x, $nbins, BOX => [$min, $max, 0, 100]);
$pl->close;
ok (-s $tmpfile15 > 0, "Histogram plotting, high level interface");

# Test multiple plots per page (low level interface)
my $tmpfile16 = File::Spec->catfile($tmpdir, "test16.$dev");
plsdev ($dev);
plsfnam ($tmpfile16);
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

ok (-s $tmpfile16 > 0, "Multiple plots per page, low level interface");

# test multiple pages per plot (high level interface)
my $tmpfile17 = File::Spec->catfile($tmpdir, "test17.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile17, SUBPAGES => [1,2]);
$pl->histogram ($x, $nbins, BOX => [$min, $max, 0, 100]);
$pl->histogram ($x, $nbins, BOX => [$min, $max, 0, 100], SUBPAGE => 2);
$pl->close;
ok (-s $tmpfile17 > 0, "Multiple plots per page, high level interface");

my $tmpfile18 = File::Spec->catfile($tmpdir, "test18.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile18);
$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE', LINESTYLE => 2);
$pl->close;
ok (-s $tmpfile18 > 0, "Setting LINESTYLE");

# test setting plot orientation
my $tmpfile19 = File::Spec->catfile($tmpdir, "test19.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile19, ORIENTATION => 1);
$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE', LINESTYLE => 2);
$pl->close;
ok (-s $tmpfile19 > 0, "Setting plot orientation");

# test symbol plotting
my $tmpfile20 = File::Spec->catfile($tmpdir, "test20.$dev");
$pl = PDL::Graphics::PLplot->new (DEV => $dev, FILE => $tmpfile20);
$pl->setparm (BOX => [0,200,0,200]);
for (my $x=0;$x<20;$x++) {
  for (my $y=0;$y<20;$y++) {
    my $xp = pdl(10*$x);
    my $yp = pdl(10*$y);
    $pl->xyplot($xp, $yp, PLOTTYPE => 'POINTS', SYMBOL => 20*$x+$y);
  }
}
$pl->close;
ok (-s $tmpfile20 > 0, "Symbol plotting");

# test label plotting in multiple subpage plots
my $tmpfile21 = File::Spec->catfile($tmpdir, "test21.$dev");
$pl = PDL::Graphics::PLplot->new(DEV => $dev,
				      FILE => $tmpfile21,
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
ok (-s $tmpfile21 > 0, "Multiple subpages");

# test bar graphs
my $tmpfile22 = File::Spec->catfile($tmpdir, "test22.$dev");
$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => $tmpfile22);
$pl->bargraph([map { sprintf ("2002.%03d", $_) } (1..100)], 100*random(100), COLOR => 'BLUE');
$pl->close;
ok (-s $tmpfile22 > 0, "Bar graph");

my $tmpfile23 = File::Spec->catfile($tmpdir, "test23.$dev");
$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => $tmpfile23);
my @labels = ((map { sprintf ("2001.%03d", $_) } (240..365)), (map { sprintf ("2002.%03d", $_) } (1..100)));
$pl->bargraph(\@labels, 100*random(scalar(@labels)), COLOR => 'GREEN');
$pl->close;
ok (-s $tmpfile23 > 0, "Bar graph part 2");

my $tmpfile23a = File::Spec->catfile($tmpdir, "test23a.$dev");
$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => $tmpfile23a);
@labels = ((map { sprintf ("2001.%03d", $_) } (240..365)), (map { sprintf ("2002.%03d", $_) } (1..100)));
$pl->bargraph(\@labels, 100*random(scalar(@labels)), COLOR => 'GREEN', MAXBARLABELS => 30);
$pl->close;
ok (-s $tmpfile23a > 0, "Bar graph part 3");

my $tmpfile23b = File::Spec->catfile($tmpdir, "test23b.$dev");
$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => $tmpfile23b);
@labels = ((map { sprintf ("2001.%03d", $_) } (240..365)), (map { sprintf ("2002.%03d", $_) } (1..100)));
$pl->bargraph(\@labels, 100*random(scalar(@labels)), COLOR => 'GREEN', TEXTPOSITION => ['tv', 0.5, 0.0, 0.0]);
$pl->close;
ok (-s $tmpfile23b > 0, "Bar graph part 4");

my $tmpfile23c = File::Spec->catfile($tmpdir, "test23c.$dev");
$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => $tmpfile23c);
@labels = ((map { sprintf ("2001.%03d", $_) } (240..365)), (map { sprintf ("2002.%03d", $_) } (1..100)));
$pl->bargraph(\@labels, 100*random(scalar(@labels)), UNFILLED_BARS => 1, COLOR => 'GREEN', TEXTPOSITION => ['tv', 0.5, 0.0, 0.0]);
$pl->close;
ok (-s $tmpfile23c > 0, "Bar graph part 5, unfilled boxes");

my $tmpfile24 = File::Spec->catfile($tmpdir, "test24.$dev");
$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => $tmpfile24);
$x  = sequence(10);
$y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE', XERRORBAR => ones(10)*0.5, XTICK => 2,  NXSUB => 5,
                                        YERRORBAR => $y*0.1,       YTICK => 20, NYSUB => 10,
                                        MINTICKSIZE => 2, MAJTICKSIZE => 3);
$pl->close;
ok (-s $tmpfile24 > 0, "Setting error bars and tick size");

my $tmpfile25 = File::Spec->catfile($tmpdir, "test25.$dev");
$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => $tmpfile25);
$x1  = sequence(20);
my $y1  = $x1**2;

$x2  = sequence(22);
my $y2  = sqrt($x2);

my $x3  = sequence(30);
my $y3  = $x3**3;

my $xs  = [$x1, $x2, $x3];
my $ys  = [$y1, $y2, $y3];

$pl->stripplots($xs, $ys, PLOTTYPE => 'LINE', TITLE => 'functions', YLAB => ['x**2', 'sqrt(x)', 'x**3']);
$pl->close;
ok (-s $tmpfile25 > 0, "Basic stripplots");

my $tmpfile26 = File::Spec->catfile($tmpdir, "test26.$dev");
$pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => $tmpfile26);
$x1  = sequence(20);
$y1  = $x1**2;

$x2  = sequence(18);
$y2  = sqrt($x2);

$x3  = sequence(24);
$y3  = $x3**3;

my $x4  = sequence(27);
$a  = ($x4/20) * 2 * $pi;
my $y4  = sin($a);

$xs  = [$x1, $x2, $x3, $x4];
$ys  = [$y1, $y2, $y3, $y4];
$pl->stripplots($xs, $ys, PLOTTYPE => 'LINE', TITLE => 'functions',
                YLAB => ['x**2', 'sqrt(x)', 'x**3', 'sin(x/20*2pi)'],
                         COLOR => ['GREEN', 'DEEPSKYBLUE', 'DARKORCHID1', 'DEEPPINK'], XLAB => 'X label');
$pl->close;
ok (-s $tmpfile26 > 0, "Multi-color stripplots");

# test opening/closing of more than 100 streams (100 is the max number of plplot streams, close should
# reuse plplot stream numbers).
my $count = 0;
for my $i (1 .. 120) {
  my $pltfile = File::Spec->catfile($tmpdir, "test27.$dev");
  my $win = PDL::Graphics::PLplot->new(DEV => $dev, FILE => $pltfile, PAGESIZE => [300, 300]);
  $win->xyplot(pdl(0,1), pdl(0,1));
  # print "Stream = ", plgstrm(), " Stream in object = ", $win->{STREAMNUMBER}, "\n";
  $win->close();
  if (-s $pltfile > 0) { $count++; unlink $pltfile }
}
ok ($count == 120, "Opening/closing of > 100 streams");


SKIP: {
  skip 'Not compiled with POSIX threads', 1 if (($PDL::Config{WITH_POSIX_THREADS} == 0) || ($^O =~/darwin/i));

  my $pltfile = File::Spec->catfile($tmpdir, "test28.$dev");
  my $pid;
  if($^O =~ /MSWin32/i) {
     system "$^X", '-Mblib -e "do \"t/plplot_no_fork.win32\""';
  }
  else {
    if($pid = fork()) {
      $a = waitpid($pid,0);
    } else {

      # Breakage seems to be a function of the grid size. For me, 34 did the trick.
      # You may need to fiddle with it to reproduce trouble, so it's read from the
      # command-line.
      my $grid_size = 34;

      # PThreads settings, uncomment to break:
      set_autopthread_targ($grid_size); # large number to increase likelihood of trouble
      set_autopthread_size(0);  # zero ensures we get threading

      # Add DEV unless you want it to prompt you:
      my $pl = PDL::Graphics::PLplot->new(DEV => $dev, FILE => $pltfile);

      # Some simple sequential data
      my $xs = sequence($grid_size);
      my $ys = sequence($grid_size)->transpose;

      # Plot data so that increasing y-values have different colors:
      $pl->xyplot($xs, $ys, PLOTTYPE => 'POINTS', COLORMAP => $ys);

      $pl->close;
      exit(0);
    }
  }

  # If pthreads are working wrongly, the .svg file is messed up and much larger than usual
  ok( (-s $pltfile <= 600_000) && (($? & 0xff ) == 0), "Fails to crash with POSIX threads");

}

# Local Variables:
# mode: cperl
# End:
