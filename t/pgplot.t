BEGIN{
	  # Set perl to not try to resolve all symbols at startup
	  # The default behavior causes some problems because 
	  # the PGPLOT code defines interfaces for all PGPLOT functions
	  # whether or not they are linked.
	  $ENV{'PERL_DL_NONLAZY'}=0;
}

use PDL;

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

unless($ENV{'DISPLAY'}) {
	print "1\n";
	print "ok 1 # All tests skipped: DISPLAY environment var not set\n";
	exit 0;
}

print "1..12\n";

print STDERR '

PGPLOT test includes interactive components.  Press <RETURN> within 
10 seconds to not skip the interactive part of the test.
';
$timer = 10; 
eval {
	local $SIG{ALRM} = sub { print STDERR (--$timer,": ") if($timer > 0);
	                         alarm 1 if($timer > 0);
				 die "alarm\n" if($timer<=0);
  	                       };
	alarm 1;
	$a = <STDIN>;
	alarm 0;
};
$interactive = ($@ ne "alarm\n");

print STDERR "\nSkipping interactive tests...\n"
unless($interactive);	

eval 'use PDL::Graphics::PGPLOT; use PDL::Graphics::PGPLOT::Window;';
ok(1,!$@);

eval '$w = new PDL::Graphics::PGPLOT::Window(Dev=>"/xw",Size=>[6,4],NX=>2,NY=>2,Ch=>2.5,HardCH=>2.5);';
ok(2,!$@);

{ no warnings;
  $a = rfits('m51.fits');
}

##############################
# Page 1
eval '$w->imag($a,{Title=>"\$w->imag(\$a);"} );';
ok(3,!$@);
eval '$w->fits_imag($a,{Title=>"\$w->fits_imag(\$a);"});';
ok(4,!$@);
eval '$w->imag($a,{J=>1,Title=>"\$w->imag(\$a,{J=>1});"});';
ok(5,!$@);
eval '$w->fits_imag($a,{J=>1,Title=>"\$w->imag(\$a,{J=>1});"});';
ok(6,!$@);

if($interactive) {
print STDERR <<'EOD'
PGPLOT X device... you should see a 6 inch (153 mm) x 4 inch (102 mm)
X window with four plots in it.  All four images should have tick marks 
on the outside of the axes.

[ Scaled image of m51; scale        [Square image of m51 with scale from
  in pixels on both axes ]           -3.5 - +3.5 arcmin on both axes, 
				     with cal. wedge, centered in rect. frame]

[ Square image of m51; scale        [Square image of m51 with scale as above,
  in pixels on both axes;            ``shrink-wrapped'']
  ``shrinkwrapped'' ]

EOD
."Does this look OK? :";
$_ = <STDIN>;
ok(7, ! m/n/i);
} else {
print "ok 7 # Skipped: non-interactive\n";
}

##############################
# Page 2
eval '$w->imag($a,{Pitch=>200,Align=>LB,Title=>"\$w->imag(\$a,{Pitch=>200,Align=>LB})"});';
ok(8,!$@);
eval '$w->imag($a,{J=>.5,Pitch=>200,Align=>LB,Title=>"\$w->imag(\$a,{J=>.5,Pitch=>200,Align=>LB})"});';
ok(9,!$@);
eval '$w->imag($a,{Pitch=>200,Align=>RT,Title=>"\$w->imag(\$a,{Pitch=>200,Align=>RT})"});';
ok(10,!$@);
eval '$w->imag($a,{J=>2,Pitch=>400,Align=>RT,Title=>"\$w->imag(\$a,{J=>1,Pitch=>400,Align=>RT})                     ."});';
ok(11,!$@);

if($interactive) {
print STDERR <<'EOD'
==============================================================

You should see four plots demonstrating pitch setting, justification,
and alignment:

[ Square image of m51 scaled to         [Short, squat image of m51 with
200 ppi (1.25 inches wide), aligned      aspect ratio 1:2, width 1.25 inch,
to bottom left corner of rect. plot      and height 0.625 inch, shrinkwrapped
box and cropped at the top.        ]     and placed at lower left of plot rgn]

[ Square image of m51 scaled to         [Tall, narrow image of m51 with
200 ppi (1.25 inches wide), aligned      aspect ratio 2:1, width 0.625 inch,
to upper right corner of rect. plot      and height 1.25 inch, shrinkwrapped
box and cropped at the bottom.     ]     and placed at upper right of plot rgn]

EOD
."Does this look OK? :";
$_ = <STDIN>;
ok(12,! m/n/i);
} else {
 print("ok 12 # Skipped (non-interactive)\n");
}



