
=head1 NAME

PDL::Graphics::PGPLOT - PGPLOT enhanced interface for PDL

=head1 SYNOPSIS

   perldl> $a = pdl [1..100]
   perldl> $b = sqrt($a)
   perldl> line $b
   perldl> hold
   Graphics on HOLD
   perldl> $c = sin($a/10)*2 + 4
   perldl> line $c

=head1 DESCRIPTION

PDL::Graphics::PGPLOT is an interface to the PGPLOT graphical libraries.

=head1 FUNCTIONS

Current display commands:

  imag       -  Display an image (uses pgimag()/pggray() as appropriate)
  ctab       -  Load an image colour table
  line       -  Plot vector as connected points
  points     -  Plot vector as points
  errb       -  Plot error bars
  cont       -  Display image as contour map
  bin        -  Plot vector as histogram (e.g. bin(hist($data)) )
  hi2d       -  Plot image as 2d histogram (not very good IMHO...)
  poly       -  Draw a polygon
  vect       -  Display 2 images as a vector field

Device manipulation commands:

  hold       -  Hold current plot window range - allows overlays etc.
  release    -  Release back to autoscaling of new plot window for each 
                command
  rel        -  short alias for 'release'
  env        -  Define a plot window, put on 'hold'
  dev        -  Explicitly set a new PGPLOT graphics device

The actual PGPLOT module is loaded only when the first of these
commands is executed.

Notes: C<$transform> for image/cont etc. is used in the same way as the
C<TR()> array in the underlying PGPLOT FORTRAN routine but is, fortunately,
zero-offset.

For completeness: The transformation array connect the pixel index to a
world coordinate such that:

   X = tr[0] + tr[1]*i + tr[2]*j
   Y = tr[3] + tr[4]*i + tr[5]*j

=head2 Variable passing and extensions

In general variables are passed to the pgplot routines by using
C<get_dataref>
to get the reference to the values. Before passing to pgplot routines
however, the data are checked to see if they are in accordance with the
format (typically dimensionality) required by the PGPLOT routines.
This is done using the routine C<checkarg> (internal to PGPLOT). This routine
checks the dimensionality of the input data. If there are superfluous
dimensions of size 1 they will be trimmed away until the dimensionality
is correct. Example:

Assume a piddle with dimensions (1,100,1,1) is passed to C<line>, which
expects its inputs to be vectors. C<checkarg> will then return a piddle
with dimensions (100). If instead the same piddle was passed to C<imag>,
which requires 2D piddles as output, C<checkarg> would return a piddle
with dimensionality (100, 1) (Dimensions are removed from the I<start>)

Thus, if you want to provide support for another PGPLOT function, the
structure currently look like this (there are plans to use the Options
package to simplify the options parsing):

  # Extract the hash(es) on the commandline
  ($arg, $opt)=_extract_hash(@_); 
  <Check the number of input parameters>
  <deal with $arg>
  checkarg($x, 3); # For a hypothetical 3D routine.
  ...
  pgcube($n, $x->get_dataref);
  1;

=head2 Setting options

All routines in this package take a hash with options as an optional
input. This options hash can be used to set parameters for the
subsequent plotting without going via the PGPLOT commands.

This is implemented such that the plotting settings (such as line width,
line style etc.) are affected only for that plot, any global changes made,
say, with pgslw() are preserved.

=head2 Alphabetical listing of standard options

The following options are always parsed. Whether they have any importance
depend on the routine invoked - e.g. line style is irrelevant for C<imag>, 
or the C<justify> option is irrelevant if the display is on 'hold'.
This is indicated in the help text for the commands below.

The options are not case sensitive and will match for unique substrings,
but this is not encouraged as obscure options might invalidate what
you thought was a unique substring.

In the listing below examples are given of each option. The actual
option can then be used in a plot command by specifying it as an argument
to the function wanted. (It can be placed anywhere in the command list).

E.g:
      $opt={COLOR=>2};
      line $x, $y, $opt; # This will plot a line with red color

=over 4

=item arrow

This options allows you to set the arrow shape, and optionally size for
arrows for the vect routine. The arrow shape is specified as a hash
with the key FS to set fill style, ANGLE to set the opening angle of
the arrow head, VENT to set how much of the arrow head is cut out and
SIZE to set the arrowsize.

The following

   $opt = {ARROW => {FS=>1, ANGLE=>60, VENT=>0.3, SIZE=>5}};

will make a broad arrow of five times the normal size.

Alternatively the arrow can be specified as a set of numbers
corresponding to an extention to the syntax for pgsah. The equivalent to
the above is

   $opt = {ARROW => pdl([1, 60, 0.3, 5})};

For the latter the arguments must be in the given order, and if any are
not given the default values of 1, 45, 0.3 and 1.0 respectively will
be used.

=item arrowsize

The arrowsize can be specified separately using this option to the
options hash. It is useful if an arrowstyle has been set up and one
wants to plot the same arrow with several sizes.

   $opt = {ARROWSIZE => 2.5};

=item axis

Set the axis value (see L</env>).
It can either be specified as a number, or by one of the following names:

 EMPTY  (-2) draw no box, axes or labels
 BOX    (-1) draw box only
 NORMAL (0)  draw box and label it with coordinates
 AXES   (1)  same as NORMAL, but also draw (X=0,Y=0) axes
 GRID   (2)  same as AXES, but also draw grid lines
 LOGX   (10) draw box and label X-axis logarithmically
 LOGY   (20) draw box and label Y-axis logarithmically
 LOGXY  (30) draw box and label both axes logarithmically

=item border

Normally the limits are
chosen so that the plot just fits; with this option you can increase
(or decrease) the limits by either a relative 
(ie a fraction of the original axis width) or an absolute amount.
Either specify a hash array, where the keys are C<TYPE> (set to 
'relative' or 'absolute') and C<VALUE> (the amount to change the limits
by), or set to 1, which is equivalent to
C<BORDER =E<gt> { TYPE =E<gt> 'rel', VALUE =E<gt> 0.05 }>.

=item charsize

Set the character/symbol size as a multiple of the standard size.

   $opt = {CHARSIZE => 1.5}

=item colour (or color)

Set the colour to be used for the subsequent plotting. This can be
specified as a number, and the most used colours can also be specified
with name, according to the following table (note that this only works for
the default colour map):

 0 - WHITE    1 - BLACK      2 - RED       3 - GREEN     4 - BLUE
 5 - CYAN     6 - MAGENTA    7 - YELLOW    8 - ORANGE   14 - DARKGRAY
16 - LIGHTGRAY

=item filltype

Set the fill type to be used by L<poly|/poly>. 
The fill can either be specified using numbers or name, according to the 
following table, where the recognised name is shown in capitals - it is 
case-insensitive, but the whole name must be specified.

   1 - SOLID
   2 - OUTLINE
   3 - HATCHED
   4 - CROSS_HATCHED

   $opt = {FILLTYPE => 'SOLID'};

(see below for an example of hatched fill)

=item font

Set the character font. This can either be specified as a number following
the PGPLOT numbering or name as follows (name in capitals):
   1 - NORMAL
   2 - ROMAN
   3 - ITALIC
   4 - SCRIPT

(Note that in a string, the font can be changed using the escape sequences
C<\fn>, C<\fr>, C<\fi> and C<\fs> respectively)

   $opt = {FONT => 'ROMAN'};

gives the same result as

   $opt = {FONT => 2};


=item hatching

Set the hatching to be used if either fillstyle 3 or 4 is selected
(see above) The specification is similar to the one for specifying
arrows.  The arguments for the hatching is either given using a hash
with the key ANGLE to set the angle that the hatch lines will make
with the horizontal, SEPARATION to set the spacing of the hatch lines
in units of 1% of C<min(height, width)> of the view surface, and PHASE to
set the offset the hatching. Alternatively this can be specified as a
1x3 piddle C<$hatch=pdl[$angle, $sep, $phase]>.

   $opt = {FILLTYPE => 'HATCHED', 
           HATCHING => {ANGLE=>30, SEPARATION=>4}};

Can also be specified as

   $opt = {FILL=> 'HATCHED', HATCH => pdl [30,4,0.0]};

For another example of hatching, see L</poly>.

=item justify

A boolean value which, if true, causes both axes to drawn
to the same scale; see
the PGPLOT C<pgenv()> command for more information.

=item linestyle

Set the line style. This can either be specified as a number following
the PGPLOT numbering (1 - SOLID line, 2 - DASHED, 3 - DOT-DASH-dot-dash,
4 - DOTTED, 5 - DASH-DOT-DOT-dot) or using name (as given in capitals above).
Thus the following two specifications both specify the line to be dotted:

   $opt = {LINESTYLE => 4};
   $varopt = {LINESTYLE => 'DOTTED'};

The names are not case sensitive, but the full name is required.

=item linewidth

Set the line width. It is specified as a integer multiple of 0.13 mm.

   $opt = {LINEWIDTH => 10}; # A rather fat line

=back

A more detailed listing of the functions and their usage follows. For
all functions we specify which options take effect and what other options
exist for the given function.

=head2 dev

=for ref

Open PGPLOT graphics device

=for usage

  Usage: dev $device, [$nx,$ny];

C<$device> is a PGPLOT graphics device such as "/xserve" or "/ps",
if omitted defaults to last used device (or value of env
var PGPLOT_DEV if first time).
C<$nx>, C<$ny> specify sub-panelling.

=head2 env

=for ref

Define a plot window, and put graphics on 'hold'

=for usage

  Usage: env $xmin, $xmax, $ymin, $ymax, [$justify, $axis];
         env $xmin, $xmax, $ymin, $ymax, [$options];

C<$xmin>, C<$xmax>, C<$ymin>, C<$ymax> are the plot boundaries.  
C<$justify> is a boolean value (default is 0);
if true the axes scales will be the same (see L</justify>).
C<$axis> describes how the axes should be drawn (see
L</axis>) and defaults to 0.

If the second form is used, $justify and $axis can be set in the options
hash, for example:

  env 0, 100, 0, 50, {JUSTIFY => 1, AXIS => 'GRID', CHARSIZE => 0.7};

=head2 imag

=for ref

Display an image (uses C<pgimag()>/C<pggray()> as appropriate)

=for usage

  Usage: imag ( $image,  [$min, $max, $transform], [$opt] )

Notes: C<$transform> for image/cont etc. is used in the same way as the
C<TR()> array in the underlying PGPLOT FORTRAN routine but is, 
fortunately, zero-offset.

Options recognised:

       ITF - the image transfer function applied to the pixel values. It
             may be one of 'LINEAR', 'LOG', 'SQRT' (lower case is 
             acceptable). It defaults to 'LINEAR'.
      MIN  - Sets the minimum value to be used for calculation of the
             display stretch
      MAX  - Sets the maximum value for the same
 TRANSFORM - The transform 'matrix' as a 6x1 vector for display

    The following standard options influence this command:
    AXIS, BORDER, JUSTIFY

=head2 ctab

=for ref

Load an image colour table. 

Usage:

=for usage

   ctab ( $name, [$contrast, $brightness] ) # Builtin col table
   ctab ( $ctab, [$contrast, $brightness] ) # $ctab is Nx4 array
   ctab ( $levels, $red, $green, $blue, [$contrast, $brightness] )
   ctab ( '', $contrast, $brightness ) # use last color table

Note: See L<PDL::Graphics::LUT> for access to a large
number of colour tables.

Options recognised:

   Currently no options are implemented for this command.

=head2 line

=for ref

Plot vector as connected points

If the 'MISSING' option is specified, those points in the $y vector
which are equal to the MISSING value are not plotted, but are skipped
over.  This allows one to quickly draw multiple lines with one call to
'line', for example to draw coastlines for maps.


=for usage

 Usage: line ( [$x,] $y, [$opt] )

 Options recognised:

    The following standard options influence this command:
    AXIS, BORDER, COLO(U)R, JUSTIFY, LINESTYLE, LINEWIDTH, MISSING

=for example

 $x = sequence(10)/10.;
 $y = sin($x)**2;
 # Draw a red dot-dashed line
 line $x, $y, {COLOR => 'RED', LINESTYLE=>3}; 

=head2 points

=for ref

Plot vector as points

=for usage

 Usage: points ( [$x,] $y, [$symbol(s)], [$opt] )

 Options recognised:

    SYMBOL - Either a piddle with the same dimensions as $x, containing
             the symbol associated to each point or a number specifying
             the symbol to use for every point, or a name specifying the
             symbol to use according to the following (recognised name in
	     capital letters):

              0 - SQUARE   1 - DOT     2 - PLUS     3 - ASTERISK
              4 - CIRCLE   5 - CROSS   7 - TRIANGLE 8 - EARTH
              9 - SUN     11 - DIAMOND 12- STAR

  PLOTLINE - If this is >0 a line will be drawn through the points.

    The following standard options influence this command:
    AXIS, BORDER, CHARSIZE, COLOUR, JUSTIFY, LINESTYLE, LINEWIDTH

=for example

  $y = sequence(10)**2+random(10);
  # Plot blue stars with a solid line through:
  points $y, {PLOTLINE => 1, COLOUR => BLUE, SYMBOL => STAR};

=head2 errb

=for ref

Plot error bars (using C<pgerrb()>)

Usage:

=for usage

   errb ( $y, $yerrors, [$opt] )
   errb ( $x, $y, $yerrors, [$opt] )
   errb ( $x, $y, $xerrors, $yerrors, [$opt] )
   errb ( $x, $y, $xloerr, $xhierr, $yloerr, $yhierr, [$opt])

 Options recognised:

    TERM - Length of terminals in multiples of the default length
  SYMBOL - Plot the datapoints using the symbol value given, either
           as name or number - see documentation for 'points'

    The following standard options influence this command:
    AXIS, BORDER, CHARSIZE, COLOUR, JUSTIFY, LINESTYLE, LINEWIDTH

=for example

  $y = sequence(10)**2+random(10);
  $sigma=0.5*sqrt($y);
  errb $y, $sigma, {COLOUR => RED, SYMBOL => 18};


=head2 cont

=for ref

Display image as contour map

=for usage

 Usage: cont ( $image,  [$contours, $transform, $misval], [$opt] )

Notes: C<$transform> for image/cont etc. is used in the same way as the
C<TR()> array in the underlying PGPLOT FORTRAN routine but is, 
fortunately, zero-offset.

 Options recognised:

    CONTOURS - A piddle with the contour levels
      FOLLOW - Follow the contour lines around (uses pgcont rather than
               pgcons) If this is set >0 the chosen linestyle will be
               ignored and solid line used for the positive contours
               and dashed line for the negative contours.
      LABELS - An array of strings with labels for each contour
 LABELCOLOUR - The colour of labels if different from the draw colour
               This will not interfere with the setting of draw colour
               using the colour keyword.
     MISSING - The value to ignore for contouring
   NCONTOURS - The number of contours wanted for automatical creation,
               overridden by CONTOURS
   TRANSFORM - The pixel-to-world coordinate transform vector

    The following standard options influence this command:
    AXIS, BORDER, COLOUR, JUSTIFY, LINESTYLE, LINEWIDTH

=for example

   $x=sequence(10,10);
   $ncont = 4;
   $labels= ['COLD', 'COLDER', 'FREEZING', 'NORWAY']
   # This will give four blue contour lines labelled in red.
   cont $x, {NCONT => $ncont, LABELS => $labels, LABELCOLOR => RED,
             COLOR => BLUE}


=head2 bin

=for ref

Plot vector as histogram ( e.g. C<bin(hist($data))> )

=for usage

   Usage: bin ( [$x,] $data )

 Options recognised:

    CENTRE - if true, the x values denote the centre of the bin 
             otherwise they give the lower-edge (in x) of the bin
    CENTER - as CENTRE

    The following standard options influence this command:
    AXIS, BORDER, COLOUR, JUSTIFY, LINESTYLE, LINEWIDTH


=head2 hi2d

=for ref

Plot image as 2d histogram (not very good IMHO...)

=for usage

   Usage: hi2d ( $image, [$x, $ioff, $bias], [$opt] )

 Options recognised:

    IOFFSET - The offset for each array slice. >0 slants to the right
                                               <0 to the left.
       BIAS - The bias to shift each array slice up by.

    The following standard options influence this command:
    AXIS, BORDER, JUSTIFY

Note that meddling with the ioffset and bias often will require you to
change the default plot range somewhat. It is also worth noting that if
you have TriD working you will probably be better off using C<mesh3d> or
a similar command - see C<help TriD>.

=for example

     $r=sequence(100)/50-1.0;
     $y=exp(-$r**2)*transpose(exp(-$r**2))
     hi2d $y, {IOFF => 1.5, BIAS => 0.07};

=head2 poly

=for ref

Draw a polygon

=for usage

   Usage: poly ( $x, $y )

 Options recognised:

    The following standard options influence this command:
    AXIS, BORDER, COLOUR, FILLTYPE, HATCHING, JUSTIFY, LINESTYLE, 
    LINEWIDTH

=for example

  # Fill with hatching in two different colours
  $x=sequence(10)/10;
  # First fill with cyan hatching
  poly $x, $x**2, {COLOR=>5, FILL=>3};
  hold;
  # Then do it over again with the hatching offset in phase:
  poly $x, $x**2, {COLOR=>6, FILL=>3, HATCH=>{PHASE=>0.5}};
  release;

=head2 vect

=for ref

Display 2 images as a vector field

=for usage

   Usage: vect ( $a, $b, [$scale, $pos, $transform, $misval] )

Notes: C<$transform> for image/cont etc. is used in the same way as the
C<TR()> array in the underlying PGPLOT FORTRAN routine but is, 
fortunately, zero-offset.

This routine will plot a vector field. C<$a> is the horizontal component
and C<$b> the vertical component.

 Options recognised:

    SCALE - Set the scale factor for vector lengths.
      POS - Set the position of vectors.
            <0 - vector head at coordinate
            >0 - vector base at coordinate
            =0 - vector centered on the coordinate
TRANSFORM - The pixel-to-world coordinate transform vector
  MISSING - Elements with this value are ignored.

    The following standard options influence this command:
    ARROW, ARROWSIZE, AXIS, BORDER, CHARSIZE, COLOUR, JUSTIFY, 
    LINESTYLE, LINEWIDTH

=for example

 $a=rvals(11,11,{Centre=>[5,5]});
 $b=rvals(11,11,{Centre=>[0,0]});
 vect $a, $b, {COLOR=>YELLOW, ARROWSIZE=>0.5, LINESTYLE=>dashed};

=head1 AUTHOR

Karl Glazebrook [kgb@aaoepp.aao.gov.au] modified by Jarle Brinchmann
(jarle@ast.cam.ac.uk), docs mangled by Tuomas J. Lukka
(lukka@fas.harvard.edu) and Christian Soeller (c.soeller@auckland.ac.nz).

All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut


# Graphics functions for the PDL module, this module
# requires the PGPLOT module be previously installed.
# PGPLOT functions are also made available to the caller.

package PDL::Graphics::PGPLOT;

# Just a plain function exporting package

@EXPORT = qw( dev hold release rel env bin cont errb line points
	      imag image ctab ctab_info hi2d poly vect CtoF77coords
);

use PDL::Core qw/:Func :Internal/;    # Grab the Core names
use PDL::Basic;
use PDL::Primitive;
use PDL::Types;
use SelfLoader;
use Exporter;
use PDL::Dbg;
use PGPLOT;

use vars qw($AXISCOLOUR $SYMBOL $ERRTERM $HARD_LW $HARD_CH $HARD_FONT);

require DynaLoader;

@ISA = qw( Exporter SelfLoader DynaLoader );

bootstrap PDL::Graphics::PGPLOT;

*rel = *release; # Alias
*image = *imag;

############################################################################

# Global variables for customisation, defaults are:

$AXISCOLOUR = 3;   # Axis colour
$SYMBOL     = 17;  # Plot symbol for points
$COLOUR     = 5;   # Colour for plots
$ERRTERM    = 1;   # Size of error bar terminators
$HARD_LW    = 4;   # Line width for hardcopy devices
$HARD_CH    = 1.4; # Character height for hardcopy devices
$HARD_FONT  = 2;   # Font for hardcopy devices

# Standard colour tables (args to ctab())
# see also PDL::Graphics::LUT

%CTAB = ();
$CTAB{Grey}    = [ pdl([0,1],[0,1],[0,1],[0,1]) ];
$CTAB{Igrey}   = [ pdl([0,1],[1,0],[1,0],[1,0]) ];
$CTAB{Fire}    = [ pdl([0,0.33,0.66,1],[0,1,1,1],[0,0,1,1],[0,0,0,1]) ];
$CTAB{Gray}    = $CTAB{Grey};  # Alias
$CTAB{Igray}   = $CTAB{Igrey}; # Alias
$CTAB        = undef;         # last CTAB used
$DEV  = $ENV{"PGPLOT_DEV"} if defined $ENV{"PGPLOT_DEV"};
$DEV  = "?" if !defined($DEV) || $DEV eq ""; # Safe default

%LINES = ('SOLID' => 1, 'DASHED' => 2, 'DOT-DASH' => 3, 'DOTTED' => 4,
	  'DASH-DOT-DOT' => 5);
%FONTS = (NORMAL=>1, ROMAN =>2, ITALIC=>3, SCRIPT=>4);
%FILL=(SOLID=>1, OUTLINE=>2, HATCHED=>3, CROSS_HATCHED=>4);
%COLORS=(WHITE=>0, BLACK=>1, RED=>2, GREEN=>3, BLUE=>4, CYAN=>5,
	MAGENTA=>6, YELLOW=>7, ORANGE=>8, DARKGRAY=>14, DARKGREY=>14,
	LIGHTGRAY=>15, LIGHTGREY=>15);
%SYMBOLS=(SQUARE=>0, DOT=>1, PLUS=>2, ASTERISK=>3, CIRCLE=>4, CROSS=>5,
	 TRIANGLE=>7, EARTH=>8, SUN=>9, DIAMOND=>11, STAR=>12);

%ITF = ( LINEAR => 0, LOG => 1, SQRT => 2 );

# valid values for axis parameter (eg env())
%_axis = 
    ( -2 => -2, EMPTY => -2, -1 => -1, BOX => -1,
      0 => 0, NORMAL => 0, 1 => 1, AXES => 1, 2 => 2, GRID => 2,
      10 => 10, LOGX => 10, 20 => 20, LOGY => 20, 30 => 30, LOGXY => 30 );

# corresponding values for calls to pgbox()
%_pgbox =
    ( -2 => '', -1 => 'BC', 0 => 'BCNST', 1 => 'ABCNST', 2 => 'ABCGNST',
      10 => [ 'BCLNST', 'BCNST' ], 20 => [ 'BCNST', 'BCLNST' ], 
      30 => [ 'BCLNST', 'BCLNST' ] );

END { # Destructor to close plot when perl exits
  my ($state,$len);
  pgqinf('STATE',$state,$len);
  pgend() if $state eq "OPEN";
}


############ Local functions #################

sub checkarg {  # Check/alter arguments utility
    my ($arg,$dims,$type) = @_;
    $type = $PDL_F unless defined $type;
    $arg = topdl($arg); # Make into a pdl
    $arg = convert($arg,$type) if $arg->get_datatype != $type;
    if (($arg->getndims > $dims)) {
      # Get the dimensions, find out which are == 1. If it helps
      # chuck these off and return trimmed piddle.
      my $n=nelem(which(pdl($arg->dims)==1));
      barf "Data is >".$dims."D" if ($arg->getndims-$n) > $dims;
      my $count=0;      my $qq;
      my $s=join ',',
      map {if ($_ == 1 && $count<$arg->getndims-$dims) {$qq='(0)'; $count++}
         else {$qq= ''}; $qq} $arg->dims;
      $arg=$arg->slice($s);
    }
    $_[0] = $arg; # Alter
1;}

sub pgdefaults{    # Set up defaults
    local($hcopy, $len);
    pgask(0);
    pgqinf("HARDCOPY",$hcopy,$len);
    if ($hcopy eq "YES") {
       pgslw($HARD_LW); pgsch($HARD_CH);
       pgscf($HARD_FONT);
    }
    pgsci($COLOUR); pgask(0);
}

sub initdev{  # Ensure a device is open
     local ($state,$len);
     pgqinf('STATE',$state,$len);
     dev() if ($state eq "CLOSED");
1;}

# initenv( $xmin, $xmax, $ymin, $ymax, $just, $axis )
# initenv( $xmin, $xmax, $ymin, $ymax, $just )
# initenv( $xmin, $xmax, $ymin, $ymax, \%opt )
#
# \%opt can be supplied but not be defined
# we parse the JUSTIFY, AXIS, and BORDER options here,
# rather than have a multitude of checks below
#
sub initenv{ # Default box
    my ($col); initdev(); pgqci($col); pgsci($AXISCOLOUR);
    my @opts = @_; 
    my $hashref = undef;
    if ( ref($opts[4]) eq "HASH" ) {
	# parse options
	$hashref = $opts[4];

	$opts[5] = 0; # axis
	$opts[4] = 0; # justify
	while (my ($key, $val) = each %{$hashref}) {
	    $key = uc($key);
	    if    ($key =~ m/^JUST/) { $opts[4] = $val; } # JUSTIFY
	    elsif ($key =~ m/^AXIS/) { $opts[5] = $val; } # AXIS
	    elsif ($key =~ m/^BORD/ and $val != 0 ) {
		my $type  = "REL";
		my $delta = 0.05;
		if ( ref($val) eq "HASH" ) {
		    while (my ($bkey, $bval) = each %{$val}) {
			$bkey = uc($bkey);
			if    ($bkey =~ m/^TYP/) { $type = uc $bval; }
			elsif ($bkey =~ m/^VAL/) { $delta = $bval; }
		    } # while: (bkey,bval)
		} # if: ref($val) eq "HASH"

		if ( $type =~ m/^REL/ ) {
		    my $sep = ( $opts[1] - $opts[0] ) * $delta;
		    $opts[0] -= $sep; $opts[1] += $sep;
		    $sep = ( $opts[3] - $opts[2] ) * $delta;
		    $opts[2] -= $sep; $opts[3] += $sep;
		} elsif ( $type =~ m/^ABS/ ) {
		    $opts[0] -= $delta; $opts[1] += $delta;
		    $opts[2] -= $delta; $opts[3] += $delta;
		} else {
		    print "Warning: unknown BORDER/TYPE option '$type'.\n";
		}
	    } # if: ... $key
	} # while: (key,val)
    } elsif ( $#opts == 4 ) {
	$opts[4] = 0 unless defined $opts[4]; # \%opt supplied but not defined
	$opts[5] = 0;
    }
    # ensure we have a numeric value for the axis option
    my $axopt = $opts[5];
    $opts[5] = $_axis{ uc($axopt) };
    barf "Unknown axis option '$axopt'." unless defined($opts[5]);

    pgenv(@opts); 
    pgsci($col);
    @last = (@opts);
1;}

sub redraw_axes {
    pgqci($col); pgsci($AXISCOLOUR);
    my $axval = $last[5]; $axval = 0 unless defined $axval; # safety check
    $labs = $_pgbox{$axval};
    barf "Unknown axis value '$axval'." unless defined $labs;
    unless ( $hold ) {
	if ( ref($labs) ) {
	    pgbox($$labs[0],0,0,$$labs[1],0,0);
	} else {
	    pgbox($labs,0,0,$labs,0,0);
	}
    }
    pgsci($col);
}

sub CtoF77coords{  # convert a transform array from zero-offset to unit-offset images
    my $tr = pdl(shift); # Copy
    set($tr, 0, at($tr,0)-at($tr,1)-at($tr,2));
    set($tr, 3, at($tr,3)-at($tr,4)-at($tr,5));
    return $tr;
}

sub save_status {
  pgqinf('STATE',$state,$len);
  pgsave if $state eq 'OPEN';
}

sub restore_status {
  pgqinf('STATE',$state,$len);
  pgunsa if $state eq 'OPEN';
}

sub _extract_hash {
  my @opt=@_;
  #
  # Given a list, returns a list of hash references and all the rest.
  #
  my $count=0;
  my $hashes=[];
  foreach (@opt) {
    push @$hashes, splice(@opt, $count, 1) if ref($_) eq 'HASH';
    $count++
  }

  return (\@opt, $$hashes[0]);

}


sub _check_and_exec_name {
  my ($name, $hash, $function, $val)=@_;
  if ($val=~m/^\s*\d/) {&$function($val);} else {
    exists($$hash{uc($val)}) ? &$function($$hash{uc($val)}) :
      warn "Unknown $name $val \n";
  }
}

sub standard_options_parser {
  #
  # Parses a hash with options..
  #
  my ($opt)=@_;
  my $arrowsize=1; # Default arrowsize
  while (my ($key, $val) = each %{$opt}) {
    if ($key =~ m/^COL/i) {
      _check_and_exec_name('color', \%COLORS, \&pgsci, $val);
    } elsif ($key =~ m/^LINES/i) {
      _check_and_exec_name('line-style', \%LINES, \&pgsls, $val);
    } elsif ($key =~ m/^LINEW/i) {
      pgslw($val);
    } elsif ($key =~ m/^FONT/i) {
      _check_and_exec_name('font', \%FONTS, \&pgscf, $val);
    } elsif ($key =~ m/^CHAR/i) {
      pgsch($val);
    } elsif ($key =~ m/^FILL/i) {
      _check_and_exec_name('fill type', \%FILL, \&pgsfs, $val);
    } elsif ($key =~ m/^ARROWS/i) {
      $arrowsize=$val;
      pgsch($arrowsize);
    } elsif ($key =~ m/^ARROW/i) {
      #
      # Set the arrow. The size can be set either independently
      # using ARROWSIZE or in the hash
      #
      my ($fs, $angle, $vent)=(1,45.0,0.3);
      if (ref($val) eq 'HASH') {
	while (my ($var, $value)=each %$val) {
	  $fs=$value if $var =~ m/^F/i;
	  $angle=$value if $var =~ m/^A/i;
	  $vent=$value if $var =~ m/^V/i;
	  $arrowsize=$value if $var =~ m/^S/i;
	}
      } else {

	$fs=$val[0] if defined $val[0];
	$angle=$val[1] if defined $val[1];
	$vent=$val[2] if defined $val[2];
	$arrowsize=$val[3] if defined $val[3];
      }
      pgsch($arrowsize);
      pgsah($fs, $angle, $vent);
    } elsif ($key =~ m/^HATCH/i) {
      if (!defined($val) || lc($val) eq 'default') {
	pgshs(); # Default values are either specfied by HATCH=>undef or HATCH=>'default'
      } else {
	#
	# Can either be specified as numbers or as a hash...
	#
	my ($angle, $separation, $phase)=(45.0,1.0,0.0);
	if (ref($val) eq 'HASH') {
	  while (my ($var, $value) = each %{$val}) {
	    $angle=$value if $var =~ m/^A/i;
	    $separation=$value if $var =~ m/^S/i;
	    $phase=$value if $var =~ m/^P/i;
	  }
	} else {
	  $angle=$val[0] if defined($val[0]);
	  $separation=$val[1] if defined($val[1]);
	  $phase=$val[2] if defined($val[2]);
	}
	if ($separation==0) {
	  warn "The separation of hatch lines cannot be zero, the default of 1 is used!\n";
	  $separation=1;
	}
	pgshs($angle,$separation, $phase);
      }
    }
  }

  1;
}



############ Exported functions #################

# Open/reopen the graphics device

sub dev {
    local ($dev,$nx,$ny) = @_;
    $dev = $DEV if !defined $dev || $dev eq "";
    $nx = 1 unless defined $nx;
    $ny = 1 unless defined $ny;
    local ($state,$len);
    pgqinf('STATE',$state,$len);
    pgend() if ($state eq "OPEN");
    pgbegin(0,$dev,$nx,$ny);
    $DEV = $dev;
    pgdefaults();
1;}

# hold/release functions for overlays

$hold = 0;
sub hold    { $hold=1; print "Graphics on HOLD\n" if $PDL::verbose;};
sub release { $hold=0; print "Graphics RELEASED\n" if $PDL::verbose;};

# set the envelope for plots and put auto-axes on hold

sub env {
    barf "Usage: env ( $xmin, $xmax, $ymin, $ymax, [$just, $axis] )"
       if ($#_==-1 && !defined(@last)) || ($#_>=0 && $#_<=2) || $#_>5;
    my(@args);
    @args = $#_==-1 ? @last : @_;         # No args - use previous
    $args[4] = 0 unless defined $args[4]; # $just
    $args[5] = 0 unless defined $args[5]; # $axis
    initdev();
    initenv( @args );
    hold;
1;}


# Plot a histogram with pgbin()

sub bin {
    ($in, $opt)=_extract_hash(@_);
    barf 'Usage: bin ( [$x,] $data, [$options] )' if $#$in<0 || $#$in>2;
    my ($x, $data)=@$in;

    checkarg($x,1);

    my $n = nelem($x);
    if ($#$in==1) {
       checkarg($data,1); barf '$x and $y must be same size' if $n!=nelem($data);
     } else {
       $data = $x; $x = float(sequence($n));
     }

    unless ( $hold ) {
	my ($xmin, $xmax)=minmax($x); my ($ymin, $ymax)=minmax($data);
	initenv( $xmin, $xmax, $ymin, $ymax, $opt );
    }
    save_status();

    # look for specific options
    my $centre = 1;
    while (my ($key, $val) = each %{$opt}) {
	$key = uc($key); 
	if ($key =~ m/^CENT/) { $centre = $val ? 1 : 0; } # CENT(RE|ER)
    }

    # Let's also parse the options if any.
    standard_options_parser($opt); 
    pgbin($n, $x->get_dataref, $data->get_dataref, $centre);
    restore_status();
1;}

# display a contour map of an image using pgconb()

sub cont {
  ($in, $opt)=_extract_hash(@_);
  barf 'Usage: cont ( $image, %options )' if $#$in<0;

  # Parse input
  my ($image, $contours, $tr, $misval) = @$in;
  checkarg($image,2);
  my($nx,$ny) = $image->dims;
  my ($ncont)=9; # The number of contours by default

  initenv( 0,$nx-1, 0, $ny-1, $opt ) unless ( $hold );

  my($minim, $maxim)=minmax($image);

  # First save the present status
  save_status();
  # Then parse the common options
  standard_options_parser($opt);
  my ($labelcolour);
  pgqci($labelcolour); # Default let the labels have the chosen colour.
  # Parse the options particular to this routine
  my ($labels);
  my $usepgcont = 0;
  while (($key, $value)=each %$opt) {
  SWITCH: {
      if ($key =~ m/^CON/i) {$contours=$value; last SWITCH;}
      if ($key =~ m/^NCONT/i) {$ncont=$value; last SWITCH;}
      if ($key =~ m/^MISS/i) {$misval=$value; last SWITCH}
      if ($key =~ m/^TR/i) {$tr=$value;last SWITCH;}
      if ($key =~ m/^LABELC/i) {$labelcolour=$value; last SWITCH;}
      if ($key =~ m/^LAB/i) {$labels=$value; last SWITCH;}
      if ($key =~ m/^FOLLOW/i) {$usepgcont= ($value >0) ? 1 : -1; last SWITCH}
    }
  }

  $contours = xlinvals(zeroes($ncont), $minim, $maxim)
    unless defined $contours;
  checkarg($contours,1);

  if (defined($tr)) {
    checkarg($tr,1);
    barf '$transform incorrect' if nelem($tr)!=6;
  } else {
    $tr = float [0,1,0, 0,0,1];
  }

  $tr = CtoF77coords($tr);
  print "Contouring $nx x $ny image from ",min($contours), " to ",
  max($contours), " in ",nelem($contours)," steps\n" if $PDL::verbose;

  if (defined($misval)) {
    pgconb( $image->get_dataref, $nx,$ny,1,$nx,1,$ny,
	    $contours->get_dataref,
	    nelem($contours), $tr->get_dataref, $misval);
  } elsif (abs($usepgcont) == 1) {
    pgcont( $image->get_dataref, $nx,$ny,1,$nx,1,$ny,
	    $contours->get_dataref,
	    $usepgcont*nelem($contours), $tr->get_dataref);
  } else {
    pgcons( $image->get_dataref, $nx,$ny,1,$nx,1,$ny,
	    $contours->get_dataref, nelem($contours), $tr->get_dataref);
  }

  # Finally label the contours.
  if (defined($labels) && $#$labels+1==nelem($contours)) {

    my $label=undef;
    my $count=0;
    my $minint=long($nx/10)+1; # At least stretch a tenth of the array
    my $intval=long($nx/3)+1; #

    pgqci($dum);
    _check_and_exec_name('labelcolor', \%COLORS, \&pgsci, $labelcolour);
    foreach $label (@{$labels}) {
      pgconl( $image->get_dataref, $nx,$ny,1,$nx,1,$ny,
	      $contours->slice("($count)"),
	      $tr->get_dataref, $label, $intval, $minint);
      $count++;
    }
    pgsci($dum);
  } elsif (defined($labels)) {
    #
    #  We must have had the wrong number of labels
    #
    warn <<EOD
  You must specify the same number of labels as contours.
  Labelling has been ignored.
EOD

}

  # Restore attributes
  restore_status();
  1;}


# Plot errors with pgerrb()

sub errb {
  my ($in, $opt)=_extract_hash(@_);
  barf <<'EOD' if $#$in<1 || $#$in==4 || $#$in>5;
Usage: errb ( $y, $yerrors [, $options] )
       errb ( $x, $y, $yerrors [, $options] )
       errb ( $x, $y, $xerrors, $yerrors [, $options])
       errb ( $x, $y, $xloerr, $xhierr, $yloerr, $yhierr [, $options])
EOD

  my @t=@$in;
  my $i=0; my $n;
  for (@t) {
    checkarg($_, 1);
    $n = nelem($_) if $i++ == 0;
    barf "Args must have same size" if nelem($_)!=$n;
  }
  my $x = $#t==1 ? float(sequence($n)) : $t[0];
  my $y = $#t==1 ? $t[0] : $t[1];
  
  unless( $hold ) {
      my ($xmin, $xmax)=minmax($x); my ($ymin, $ymax)=minmax($y);
      initenv( $xmin, $xmax, $ymin, $ymax, $opt );
  }
  save_status();
  # Let us parse the options if any.
  my $term=$ERRTERM;
  my $symbol;
  my $plot_points=0; # We won't normally plot the points
  while (my ($key, $val) = each %{$opt}) {
    if ($key =~ m/^TERM/i) {$term=$val;};
    if ($key =~ m/^SYM/i) {
      if (ref($val) eq 'PDL' || $val =~ m/^\d/) {
	$symbol=$val;
      } else {
	# The user has specified a symbol using name presumably
	if (!exists($SYMBOLS{uc($val)})) {
	  warn "Symbol $val does not exist - using points instead\n";
	  $symbol=1;
	} else {
	  $symbol=$SYMBOLS{uc($val)};
	}
      }
      $plot_points=1;
    }
  }
  standard_options_parser($opt);
  if ($#t==1) {
    pgerrb(6,$n,$x->get_dataref,$y->get_dataref,$t[1]->get_dataref,$term);
  } elsif ($#t==2) {
    pgerrb(6,$n,$x->get_dataref,$y->get_dataref,$t[2]->get_dataref,$term);
  } elsif ($#t==3) {
    pgerrb(5,$n,$x->get_dataref,$y->get_dataref,$t[2]->get_dataref,$term);
    pgerrb(6,$n,$x->get_dataref,$y->get_dataref,$t[3]->get_dataref,$term);
  } elsif ($#t==5) {
    pgerrb(1,$n,$x->get_dataref,$y->get_dataref,$t[3]->get_dataref,$term);
    pgerrb(2,$n,$x->get_dataref,$y->get_dataref,$t[5]->get_dataref,$term);
    pgerrb(3,$n,$x->get_dataref,$y->get_dataref,$t[2]->get_dataref,$term);
    pgerrb(4,$n,$x->get_dataref,$y->get_dataref,$t[4]->get_dataref,$term);
  }
  if ($plot_points) {
    $symbol=long($symbol);
    $ns=nelem($symbol);
    pgpnts($n, $x->get_dataref, $y->get_dataref, $symbol->get_dataref, $ns)
  }

  restore_status();
  1;
}

# Plot a line with pgline()

sub line {
  ($in, $opt)=_extract_hash(@_);
  barf 'Usage: line ( [$x,] $y, [$options] )' if $#$in<0 || $#$in>2;
  my($x,$y) = @$in;
  checkarg($x,1);
  my $n = nelem($x);

  if ($#$in==1) {
    checkarg($y,1); barf '$x and $y must be same size' if $n!=nelem($y);
  } else {
    $y = $x; $x = float(sequence($n));
  }

  unless ( $hold ) {

    # Make sure the missing value is used as the min or max value
    my ($ymin, $ymax, $xmin, $xmax);
    if (exists $$opt{MISSING}) {
      ($ymin, $ymax)=minmax($y->where($y != $$opt{MISSING}));
      ($xmin, $xmax)=minmax($x->where($x != $$opt{MISSING}));
    } else {
      ($ymin, $ymax)=minmax($y);
      ($xmin, $xmax)=minmax($x); 
    }

    initenv( $xmin, $xmax, $ymin, $ymax, $opt );
  }
  save_status();
  standard_options_parser($opt);

  # If there is a missing value specified, use pggapline
  # to break the line around missing values.
  if (exists $$opt{MISSING}) {
    pggapline ($n, $$opt{MISSING}, $x->get_dataref, $y->get_dataref);
  } else {
    pgline($n, $x->get_dataref, $y->get_dataref);
  }
  restore_status();
1;}

# Plot points with pgpnts()

sub points {
  ($in, $opt)=_extract_hash(@_);
  barf 'Usage: points ( [$x,] $y, $sym, [$options] )' if $#$in<0 || $#$in>2;
  my ($x, $y, $sym)=@$in;
  checkarg($x,1);
  my $n=nelem($x);

  if ($#$in>=1) {
    checkarg($y,1); barf '$x and $y must be same size' if $n!=nelem($y);
  }else{
    $y = $x; $x = float(sequence($n));
  }

  #
  # Save some time for large datasets.
  #
  unless ( $hold ) {
      my ($xmin, $xmax)=minmax($x); my ($ymin, $ymax)=minmax($y);
      initenv( $xmin, $xmax, $ymin, $ymax, $opt );
  }
  save_status();

  my $plot_line=0;

  standard_options_parser($opt);
  # Additional functionality for points..
  while (my ($key, $val) = each %{$opt}) {
    if ($key =~ m/^SYM/i) {
      if (ref($val) eq 'PDL' || $val =~ m/^\d/) {
	$sym=$val;
      } else {
	# The user has specified a symbol using name presumably
	if (!exists($SYMBOLS{uc($val)})) {
	  warn "Symbol $val does not exist - using points instead\n";
	  $sym=1;
	} else {
	  $sym=$SYMBOLS{uc($val)};
	}
      }
    }
    if ($key =~ m/^PLOTLINE/i) { $plot_line=$val;};
  }

  $sym = $SYMBOL if !defined($sym);
  checkarg($sym,1); my $ns = nelem($sym); $sym = long($sym);

  pgpnts($n, $x->get_dataref, $y->get_dataref, $sym->get_dataref, $ns);

  #
  # Sometimes you would like to plot a line through the points straight
  # away.
  pgline($n, $x->get_dataref, $y->get_dataref) if $plot_line>0;

  restore_status();
  1;}

# display an image using pgimag()/pggray() as appropriate

sub imag {
  ($in, $opt)=_extract_hash(@_);

  # There is little use for options here, but I could add support
  # for a contoured overlay.

  barf 'Usage: imag ( $image,  [$min, $max, $transform] )' if $#$in<0 || $#$in>3;
  my ($image,$min,$max,$tr) = @$in;
  checkarg($image,2);
  my($nx,$ny) = $image->dims;

  my $itf = 0;

  # Parse for options input instead of calling convention
  while (my ($key, $val) = each %{$opt}) {
      $key = uc($key);
    if ($key =~ m/^TRAN/) {$tr=$val;}
    elsif ($key =~ m/^MIN/) { $min=$val;}
    elsif ($key =~ m/^MAX/) {$max=$val;}
    elsif ($key =~ m/ITF/ ) {
      defined( $itf = $ITF{uc $val} )
      or barf ( "illegal ITF value `$val'. use one of: " .
                join(',', keys %ITF) );
    }
  }

  $min = min($image) unless defined $min;
  $max = max($image) unless defined $max;

  if (defined($tr)) {
    checkarg($tr,1);
    barf '$transform incorrect' if nelem($tr)!=6;
  }else{
    $tr = float [0,1,0, 0,0,1];
  }
  $tr = CtoF77coords($tr);

  initenv(
    at($tr,0) + 0.5 * $tr->slice('1:2')->sum,
    at($tr,0) + ($nx + 0.5) * $tr->slice('1:2')->sum,
    at($tr,3) + 0.5 * $tr->slice('4:5')->sum,
    at($tr,3) + ($ny + 0.5) * $tr->slice('4:5')->sum,
    $opt
  ) unless $hold;
  print "Displaying $nx x $ny image from $min to $max ...\n" if $PDL::verbose;

  pgsitf( $itf );
  pgqcir($i1, $i2);          # Colour range - if too small use pggray dither algorithm
  pgqinf('TYPE',$dev,$len);  # Device (/ps buggy - force pggray)
  if ($i2-$i1<16 || $dev =~ /^v?ps$/i) {
    pggray( $image->get_dataref, $nx,$ny,1,$nx,1,$ny, $min, $max, $tr->get_dataref);
  }
  else{
    ctab(Grey) unless $CTAB; # Start with grey
    pgimag( $image->get_dataref, $nx,$ny,1,$nx,1,$ny, $min, $max, $tr->get_dataref);
  }
  redraw_axes unless $hold; # Redraw box
1;}

# Load a colour table using pgctab()

sub ctab {
  ($in, $opt)=_extract_hash(@_);

  # First indirect arg list through %CTA
  my(@arg) = @$in;
  if ($#arg>=0 && !ref($arg[0])) { # First arg is a name not an object
    # if first arg is undef or empty string, means use last CTAB.
    # preload with Grey if no prior CTAB
    $arg[0] = 'Grey' unless $arg[0] || $CTAB;

    # now check if we're using the last one specified
    if ( ! $arg[0] ) {
      shift @arg;
      unshift @arg, @{$CTAB->{ctab}};
      $brightness = $CTAB->{brightness};
      $contrast = $CTAB->{contrast};
    } else {
      my $name = ucfirst(lc(shift @arg)); # My convention is $CTAB{Grey} etc...
      barf "$name is not a standard colour table" unless defined $CTAB{$name};
      unshift @arg, @{$CTAB{$name}};
    }
  }


  if ($#arg<0 || $#arg>5) {
    my @std = keys %CTAB;
    barf <<"EOD";
Usage: ctab ( \$name, [\$contrast, $\brightness] ) # Builtin col table
            [Builtins: @std]
       ctab ( \$ctab, [\$contrast, \$brightness] ) # $ctab is Nx4 array
       ctab ( \$levels, \$red, \$green, \$blue, [\$contrast, \$brightness] )
EOD
}

  my($ctab, $levels, $red, $green, $blue, $contrast, $brightness, @t, $n);

  # Parse for options input instead of calling convention
  while (my ($key, $val) = each %{$opt}) {
    # This is not yet implemented!!
  }

  if ($#arg<3) {
    ($ctab, $contrast, $brightness) = @arg;
    @t = $ctab->dims; barf 'Must be a Nx4 array' if $#t != 1 || $t[1] != 4;
    $n = $t[0];
    $ctab   = float($ctab) if $ctab->get_datatype != $PDL_F;
    my $nn = $n-1;
    $levels = $ctab->slice("0:$nn,0:0");
    $red    = $ctab->slice("0:$nn,1:1");
    $green  = $ctab->slice("0:$nn,2:2");
    $blue   = $ctab->slice("0:$nn,3:3");
  }
  else {
    ($levels, $red, $green, $blue, $contrast, $brightness) = @arg;
    checkarg($levels,1);  $n = nelem($levels);
    for($red,$green,$blue) {
      checkarg($_,1); barf 'Arguments must have same size' unless nelem($_) == $n;
    }
  }

  # Now load it

  $contrast   = 1   unless defined $contrast;
  $brightness = 0.5 unless defined $brightness;
  initdev();

  pgctab( $levels->get_dataref, $red->get_dataref, $green->get_dataref,
	  $blue->get_dataref, $n, $contrast, $brightness );
  $CTAB = { ctab => [ $levels, $red, $green, $blue ],
          brightness => $brightness,
          contrast => $contrast
        }; # Loaded
  1;}

# get information on last CTAB load
sub ctab_info {
  ($in, $opt)=_extract_hash(@_);
  barf 'Usage: ctab_info( )' if $#$in> -1;

  return () unless $CTAB;
  return @{$CTAB->{ctab}}, $CTAB->{contrast}, $CTAB->{brightness};
}


# display an image using pghi2d()

sub hi2d {
  ($in, $opt)=_extract_hash(@_);
  barf 'Usage: hi2d ( $image, [$x, $ioff, $bias] [, $options] )' if $#$in<0 || $#$in>3;
  my ($image, $x, $ioff, $bias) = @$in;
  checkarg($image,2);
  my($nx,$ny) = $image->dims;
  if (defined($x)) {
    checkarg($x,1);
    barf '$x incorrect' if nelem($x)!=$nx;
  }else{
    $x = float(sequence($nx));
  }

  # Parse for options input instead of calling convention
  while (my ($key, $val) = each %{$opt}) {
      $key = uc($key);
    if ($key =~ m/^IOF/) {$ioff=$val;}
    elsif ($key =~ m/^BIAS/) {$bias=$val;}
  }

  $ioff = 1 unless defined $ioff;
  $bias = 5*max($image)/$ny unless defined $bias;
  $work = float(zeroes($nx));

  initenv( 0 ,2*($nx-1), 0, 10*max($image), $opt ) unless $hold;
  pghi2d($image->get_dataref, $nx, $ny, 1,$nx,1,$ny, $x->get_dataref, $ioff,
	 $bias, 1, $work->get_dataref);
  1;}


# Plot a polygon with pgpoly()

sub poly {
  ($in, $opt)=_extract_hash(@_);
  barf 'Usage: poly ( $x, $y [, $options] )' if $#$in<0 || $#$in>2;
  my($x,$y) = @$in;
  checkarg($x,1);
  checkarg($y,1);

  unless ( $hold ) {
      my ($xmin, $xmax)=minmax($x); my ($ymin, $ymax)=minmax($y);
      initenv( $xmin, $xmax, $ymin, $ymax, $opt );
  }
  save_status();
  standard_options_parser($opt);
  my $n = nelem($x);
  pgpoly($n, $x->get_dataref, $y->get_dataref);
  restore_status();
1;}


# display a vector map of 2 images using pgvect()

sub vect {
  ($in, $opt)=_extract_hash(@_);
  barf 'Usage: vect ( $a, $b, [$scale, $pos, $transform, $misval] )' if $#$in<1 || $#$in>5;
  my ($a, $b, $scale, $pos, $tr, $misval) = @$in;
  checkarg($a,2); checkarg($b,2);
  my($nx,$ny) = $a->dims;
  my($n1,$n2) = $b->dims;
  barf 'Dimensions of $a and $b must be the same' unless $n1==$nx && $n2==$ny;

  # Parse for options input instead of calling convention
  while (my ($key, $val) = each %{$opt}) {
      $key = uc($key);
    if ($key =~ m/^SCAL/) {$scale=$val;}
    elsif ($key =~ m/^POS/) {$pos=$val;}
    elsif ($key =~ m/^TR/) {$tr=$val;}
    elsif ($key =~ m/^MIS/) {$misval=$val;}
  }


  $scale = 0 unless defined $scale;
  $pos   = 0 unless defined $pos;

  if (defined($tr)) {
    checkarg($tr,1);
    barf '$transform incorrect' if nelem($tr)!=6;
  }else{
    $tr = float [0,1,0, 0,0,1];
  }
  $tr = CtoF77coords($tr);

  initenv( 0, $nx-1, 0, $ny-1, $opt ) unless $hold;
  print "Vectoring $nx x $ny images ...\n" if $PDL::verbose;

  save_status();
  standard_options_parser($opt); # For arrowtype and arrowhead
  pgvect( $a->get_dataref, $b->get_dataref, $nx,$ny,1,$nx,1,$ny, $scale, $pos,
	  $tr->get_dataref, $misval);
  restore_status();
  1;}


1;# Exit with OK status

