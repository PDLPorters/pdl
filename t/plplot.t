# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

use PDL;
use Test;

BEGIN{
  eval " use PDL::Graphics::PLplot; ";
  unless ($@){
    plan tests => 21;
    print "ok 1\n";
  }
  else {
    plan tests => 1;
    print "ok 1 # Skipped: PDL::Graphics::PLplot not installed\n";
    exit;
  }
}

sub testok ($$) {
  my $bool = shift;
  my $n    = shift;
  if ($bool) {
    print "ok $n\n";
  } else {    print "not ok $n\n";
  }
}


######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

# Use xfig driver because it should always be installed.

my $pl = PDL::Graphics::PLplot->new (DEV => "xfig",
				     FILE => "test2.xfig",
				     BACKGROUND => [255,255,255]);
my $x  = sequence(10);
my $y  = $x**2;
$pl->xyplot($x, $y,
	    BOX => [-5,10,0,200],
	    PLOTTYPE => 'LINE');
$pl->close;
testok -s "test2.xfig" > 0, 2;

my $pl = PDL::Graphics::PLplot->new (DEV => "xfig", FILE => "test3.xfig", 
				       BACKGROUND => 'WHITE');
$pl->xyplot($x, $y, PLOTTYPE => 'POINTS', COLOR => 'BLUEVIOLET', SYMBOL => 1, SYMBOLSIZE => 4);
$pl->close;
testok -s "test3.xfig" > 0, 3;

my $pl = PDL::Graphics::PLplot->new (DEV => "xfig", FILE => "test4.xfig", FRAMECOLOR => 'BLUE');
$pl->xyplot($x, $y, PLOTTYPE => 'LINEPOINTS', COLOR => [50,230,30]);
$pl->close;
testok -s "test4.xfig" > 0, 4;

my $y = sequence(30)+1;
my $m = (50* (exp(1/$y**2) - 1) * random (30,20))->xchg(0,1);
my ($mean, $rms) = statsover($m);
my $x1 = $mean + $rms;
my $x2 = $mean - $rms;
my $n  = 500 - exp($y/5);

#my $pl = PDL::Graphics::PLplot->new (DEV => "xwin", FILE => "trillian.cosmic.ucar.edu:0");

# Setting text to 1 like this does not work.  text is hard coded in ps.c ;(
#my $pl = PDL::Graphics::PLplot->new (DEV => "psc", FILE => "test5.ps", OPTS => {'text' => '1'});

my $pl = PDL::Graphics::PLplot->new (DEV => "xfig", FILE => "test5.xfig");
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
testok -s "test5.xfig" > 0, 5;

# test of setting page size.
my $pl = PDL::Graphics::PLplot->new (DEV => "xfig",
				       FILE => "test6.xfig",
				       PAGESIZE => [50,80]);
my $x  = sequence(10);
my $y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE');
$pl->close;
testok -s "test6.xfig" > 0, 6;

# test of lines with gaps (plgapline)
my $pl = PDL::Graphics::PLplot->new (DEV => "xfig",
				       FILE => "test7.xfig");
my $x  = sequence(10);
my $y  = $x**2;
$x->inplace->setbadat(5); # insert gap
$y->inplace->setbadat(5); # insert gap
$pl->xyplot($x, $y, PLOTTYPE => 'LINE');
$pl->close;
testok -s "test7.xfig" > 0, 7;

# test of setting JUSTify = 1
my $pl = PDL::Graphics::PLplot->new (DEV => "xfig", FILE => "test8.xfig");
my $x  = sequence(10);
my $y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINEPOINTS', JUST => 1);
$pl->close;
testok -s "test8.xfig" > 0, 8;

my $pl = PDL::Graphics::PLplot->new (DEV  => 'xfig', FILE => "test9.xfig");

$pl->text("Test string outside of window", TEXTPOSITION => ['T', 1, 0, 0]);
$pl->text("Test string inside window",     TEXTPOSITION => [0, 0, 0.5, 0.5, 0]);
$pl->close;
testok -s "test9.xfig" > 0, 9;

# test rainbow point plotting with color key
my $pl = PDL::Graphics::PLplot->new (DEV => 'xfig', FILE => "test10.xfig");

my $pi = atan2(1,1)*4;
my $a  = (sequence(20)/20) * 2 * $pi;
my $b  = sin($a);
my $c  = cos($a);

$pl->xyplot ($a, $b, SYMBOL => 850, SYMBOLSIZE => 1.5, PALETTE => 'RAINBOW', PLOTTYPE => 'POINTS', COLORMAP => $c);
$pl->colorkey ($c, 'v', VIEWPORT => [0.93, 0.96, 0.15, 0.85]);
$pl->colorkey ($c, 'h', VIEWPORT => [0.15, 0.85, 0.92, 0.95]);
$pl->close;
testok -s "test10.xfig" > 0, 10;

# Test plot and color key (low level interface)
plsdev ("xfig");
plsfnam ("test11.xfig");
plspage (0,0, 600,600, 0,0);
plinit();
pladv (0);
plvsta();
plwind (0, 1, 0, 1);
plvpor(0.1,0.85,0.1,0.9);
plwind (0, 10, 0, 100);
plcol0(1);
plbox (0, 0, 0, 0, 'BCNST', 'BCNST');
plpoin(10, $x, $y, 2);
plvpor(0.86,0.90,0.1,0.9);
plwind (0, 10, 0, 100);
plbox (0, 0, 0, 0, '', 'TM');
plscmap1l (0, 2, PDL->new(0,1), PDL->new(0,360), PDL->new(0.5, 0.5), PDL->new(1,1), PDL->new(0));
for (my $i=0;$i<10;$i++) {
  plcol1($i/10);
  plfill (4, PDL->new(0,10,10,0), PDL->new($i*10,$i*10,($i+1)*10,($i+1)*10));
}
plend();

testok -s "test11.xfig" > 0, 11;

# Test shade plotting (low level interface)
plsdev ("xfig");
plsfnam ("test12.xfig");
plspage (0,0, 600,600, 0,0);
plinit();
pladv (0);
plvpor(0.1, 0.9, 0.1, 0.9); 
plwind (-1, 1, -1, 1);
plpsty(0); 

my $nx = 35;
my $ny = 46;
my $x = (sequence($nx) - ($nx/2))/($nx/2);
my $y = (sequence($ny) - ($ny/2))/(($ny/2) - 1.0);
my $xv = $x->dummy(0, $y->nelem);
my $yv = $y->dummy(1, $x->nelem);
my $z = -sin(7*$xv) * cos (7*$yv) + $xv**2 - $yv**2;
my $nsteps = 15;
my ($zmin, $zmax) = $z->minmax;
my $clevel = ((sequence($nsteps)*(($zmax - $zmin)/($nsteps-1))) + $zmin);
my $fill_width = 2;
my $cont_color = 0;
my $cont_width = 0;
my $xmap = ((sequence($nx)*(2/($nx-1))) + -1); # map X coords linearly to -1 to 1
my $ymap = ((sequence($ny)*(2/($ny-1))) + -1);
plshades($z, -1, 1, -1, 1,
         $clevel, $fill_width,
         $cont_color, $cont_width, 1, $xmap, $ymap);
plend();

testok -s "test12.xfig" > 0, 12;

# test shade plots with higher level interface. 
my $pl = PDL::Graphics::PLplot->new (DEV => 'xfig', FILE => "test13.xfig");
$pl->shadeplot ($z, $nsteps, BOX => [-1, 1, -1, 1], PALETTE => 'RAINBOW'); 
$pl->colorkey ($z, 'v', VIEWPORT => [0.93, 0.96, 0.15, 0.85]);
$pl->close;
testok -s "test13.xfig" > 0, 13;

# Test histogram plotting (low level interface)
plsdev ("xfig");
plsfnam ("test14.xfig");
plspage (0,0, 600,600, 0,0);
plinit();
pladv (0);
plvpor(0.1, 0.9, 0.1, 0.9); 
my $x = random(100)*100;
my ($min, $max) = $x->minmax;
my $nbins = 15;
my $oldwin = 1; # dont call plenv

plwind ($min, $max, 0, 100);
plbox (0, 0, 0, 0, 'bcnst', 'bcnst');

plhist (100, $x, $min, $max, $nbins, $oldwin);
plend();

testok -s "test14.xfig" > 0, 14;

# test histograms with higher level interface. 
my $pl = PDL::Graphics::PLplot->new (DEV => 'xfig', FILE => "test15.xfig");
$pl->histogram ($x, $nbins, BOX => [$min, $max, 0, 100]); 
$pl->close;
testok -s "test15.xfig" > 0, 15;

# Test multiple plots per page (low level interface)
plsdev ("xfig");
plsfnam ("test16.xfig");
plspage (0,0, 300,600, 0,0);
plssub (1,2);
plinit();
pladv (1);
plvpor(0.1, 0.9, 0.1, 0.9); 
my $x = random(100)*100;
my ($min, $max) = $x->minmax;
my $nbins = 15;
my $oldwin = 1; # dont call plenv
plwind ($min, $max, 0, 100);
plbox (0, 0, 0, 0, 'bcnst', 'bcnst');
plhist (100, $x, $min, $max, $nbins, $oldwin);

pladv (2);
plvpor(0.1, 0.9, 0.1, 0.9); 
my $x = random(200)*100;
my ($min, $max) = $x->minmax;
my $nbins = 15;
my $oldwin = 1; # dont call plenv

plwind ($min, $max, 0, 100);
plbox (0, 0, 0, 0, 'bcnst', 'bcnst');

plhist (100, $x, $min, $max, $nbins, $oldwin);

plend();

testok -s "test16.xfig" > 0, 16;

# test multiple pages per plot (high level interface)
my $pl = PDL::Graphics::PLplot->new (DEV => 'xfig', FILE => "test17.xfig", SUBPAGES => [1,2]);
$pl->histogram ($x, $nbins, BOX => [$min, $max, 0, 100]); 
$pl->histogram ($x, $nbins, BOX => [$min, $max, 0, 100], SUBPAGE => 2); 
$pl->close;
testok -s "test17.xfig" > 0, 17;

my $pl = PDL::Graphics::PLplot->new (DEV => 'xfig', FILE => "test18.xfig");
my $x  = sequence(10);
my $y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE', LINESTYLE => 2);
$pl->close;
testok -s "test18.xfig" > 0, 18;

# test setting plot orientation
my $pl = PDL::Graphics::PLplot->new (DEV => 'xfig', FILE => "test19.xfig", ORIENTATION => 1);
my $x  = sequence(10);
my $y  = $x**2;
$pl->xyplot($x, $y, PLOTTYPE => 'LINE', LINESTYLE => 2);
$pl->close;
testok -s "test19.xfig" > 0, 19;

# test symbol plotting
my $pl = PDL::Graphics::PLplot->new (DEV => 'xfig', FILE => "test20.xfig");
$pl->setparm (BOX => [0,200,0,200]);
for (my $x=0;$x<20;$x++) {
  for (my $y=0;$y<20;$y++) {
    my $xp = pdl(10*$x);
    my $yp = pdl(10*$y);
    $pl->xyplot($xp, $yp, PLOTTYPE => 'POINTS', SYMBOL => 20*$x+$y);
  }
}
$pl->close;
testok -s "test20.xfig" > 0, 20;

# test label plotting in multiple subpage plots
my $pl = PDL::Graphics::PLplot->new(DEV => 'xfig', 
				      FILE => "test21.xfig", 
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
testok -s "test21.xfig" > 0, 21;
unlink glob ("test*.xfig");










