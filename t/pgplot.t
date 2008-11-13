# -*-perl-*-
BEGIN{
	  # Set perl to not try to resolve all symbols at startup
	  # The default behavior causes some problems because 
	  # the PGPLOT code defines interfaces for all PGPLOT functions
	  # whether or not they are linked.
	  $ENV{'PERL_DL_NONLAZY'}=0;
}

use strict;

use PDL;
use Test::More;

BEGIN{
   eval "use PDL::Graphics::PGPLOT; use PDL::Graphics::PGPLOT::Window;";
   if ($@) {
      plan skip_all => "Skipped: PDL::Graphics::PGPLOT not installed";
   } elsif (!exists($ENV{'DISPLAY'})) {
      # We have this after the PGPLOT module is loaded so that we test whether the
      # module will at least load, even if we do not test it's
      # functionality.
      #
      plan tests => 1;
      print "ok 1 # skip -- DISPLAY environment variable not set\n";
      exit;
   } else {
      plan tests => 12;
   }
}

sub get_answer () {
    print STDERR "Does this look OK (y/n, y is default)? :";
    my $answer = <STDIN>;
    return $answer !~ m/n/i;
}

sub interactive ($$) {
    my $flag = shift;
    my $num  = shift;
    return unless $flag; # ie not interactive

    if (1 == $num) {
    print STDERR <<'EOD';
PGPLOT X device... you should see a 6 inch (153 mm) x 4 inch (102 mm)
X window with four plots in it.  All four images should have tick marks 
on the outside of the axes.

[ Scaled image of m51; scale        [Scaled image of m51 with scale from
  in pixels on both axes ]           X=[-1.8, 2.0],Y=[-1.9, 1.9] arcmin, 
				     with cal. wedge, centered in rect. frame]

[ Square image of m51; scale        [Square image of m51 with scale as above,
  in pixels on both axes;            ``shrink-wrapped'']
  ``shrinkwrapped'' ]

EOD
    } elsif (2 == $num) {
    print STDERR <<'EOD';
==============================================================

You should see four plots demonstrating pitch setting, justification,
and alignment:

[ Square image of m51 scaled to         [Short, squat image of m51 with
300 ppi (1.25 inches wide), aligned      aspect ratio 1:2, width 1.25 inch,
to bottom left corner of rect. plot      and height 0.625 inch, shrinkwrapped
box and cropped at the top.        ]     and placed at lower left of plot rgn]

[ Square image of m51 scaled to         [Tall, narrow image of m51 with
300 ppi (1.25 inches wide), aligned      aspect ratio 2:1, width 0.625 inch,
to upper right corner of rect. plot      and height 1.25 inch, shrinkwrapped
box and cropped at the bottom.     ]     and placed at upper right of plot rgn]

EOD
    } else {
      die "Internal error: unknown test number $num for interactive()!\n";
    }
    return get_answer();
}

my $interactive = exists($ENV{'PDL_INT'});
my $skip_interactive_msg = "interactive tests not run since environment var PDL_INT not set";
my $interactive_ctr = 0;

###
### Test code
###

my $dev = $ENV{'PGPLOT_DEV'} ? $ENV{'PGPLOT_DEV'} : "/xw";

my $w = PDL::Graphics::PGPLOT::Window->new(
					   Dev => $dev,
					   Size=> [6,4],
                                           NX=>2, NY=>2,
                                           Ch=>2.5, HardCH=>2.5);
ok( UNIVERSAL::isa($w, "PDL::Graphics::PGPLOT::Window") );

my $a = rfits('m51.fits');

##############################
# Page 1
#
foreach my $str ( (
    '$w->imag($a,{Title=>"\$w->imag(\$a);"} );',
    '$w->fits_imag($a,{Title=>"\$w->fits_imag(\$a);"});',
    '$w->imag($a,{J=>1,Title=>"\$w->imag(\$a,{J=>1});"});',
    '$w->fits_imag($a,{J=>1,Title=>"\$w->fits_imag(\$a,{J=>1});"});'
    ) ) {
    eval $str;
    ok (!$@);
}

$interactive_ctr++;
SKIP: {
   skip $skip_interactive_msg, 1 unless $interactive;
   ok(interactive($interactive, $interactive_ctr), "interactive tests");
}
  
##############################
# Page 2
#
foreach my $str ( (
    '$w->imag($a,{Pitch=>300,Align=>"LB",Title=>"\$w->imag(\$a,{Pitch=>300,Align=>LB})"});',
    '$w->imag($a,{J=>.5,Pitch=>300,Align=>"LB",Title=>"\$w->imag(\$a,{J=>.5,Pitch=>300,Align=>LB})"});',
    '$w->imag($a,{Pitch=>300,Align=>"RT",Title=>"\$w->imag(\$a,{Pitch=>300,Align=>RT})"});',
    '$w->imag($a,{J=>2,Pitch=>600,Align=>"RT",Title=>"\$w->imag(\$a,{J=>2,Pitch=>600,Align=>RT})                     ."});',
    ) ) {
    eval $str;
    ok (!$@);
}

$interactive_ctr++;
SKIP: {
   skip $skip_interactive_msg, 1 unless $interactive;
   ok(interactive($interactive, $interactive_ctr), "interactive tests");
}
  
eval '$w->close';
ok (!$@);

# End


