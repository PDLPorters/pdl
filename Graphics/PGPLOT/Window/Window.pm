=head1 NAME

PDL::Graphics::PGPLOT::Window - A OO interface to PGPLOT windows

=head1 SYNOPSIS

 perldl> use PDL::Graphics::PGPLOT::Window
 perldl> $win = PDL::Graphics::PGPLOT::Window->new(Device => '/xs');
 perldl> $a = pdl [1..100]
 perldl> $b = sqrt($a)
 perldl> $win->line($b)
 perldl> $win->hold()
 perldl> $c = sin($a/10)*2 + 4
 perldl> $win->line($c)

In the following documentation the commands are not shown in their OO
versions. This is for historical reasons and should not cause too much
trouble.

=head1 DESCRIPTION

This package offers a OO interface to the PGPLOT plotting package. This
is intended to replace the traditional interface in
L<PDL::Graphics::PGPLOT|PDL::Graphics::PGPLOT>
and contains interfaces to a large number of PGPLOT routines. Below the
usage examples for each function tend to be given in the non-OO version for
historical reasons. This will slowly be changed, but in the meantime refer
to the section on OO-interface below to see how to convert the usage
information below to OO usage (it is totally trivial).

PDL::Graphics::PGPLOT::Window is an interface to the PGPLOT graphical
libraries.


The list of currently availably methods:

 imag       -  Display an image (uses pgimag()/pggray() as appropriate)
 ctab       -  Load an image colour table
 ctab_info  -  Get information about currently loaded colour table
 line       -  Plot vector as connected points
 points     -  Plot vector as points
 errb       -  Plot error bars
 cont       -  Display image as contour map
 bin        -  Plot vector as histogram (e.g. bin(hist($data)) )
 hi2d       -  Plot image as 2d histogram (not very good IMHO...)
 poly       -  Draw a polygon
 vect       -  Display 2 images as a vector field
 text       -  Write text in the plot area
 label_axes -  Print axis titles
 legend     -  Create a legend with different texts, linestyles etc.
 cursor     -  Interactively read cursor positions.
 circle     -  Draw a circle
 ellipse    -  Draw an ellipse.

Device manipulation commands:

 new          -  Constructor for a new PGPLOT output device.
 close        -  Close a PGPLOT output device.
 focus        -  Set focus to the given device. This should normally be
                 done behind the scenes.
 hold         -  Hold current plot window range - allows overlays etc.
 release      -  Release back to autoscaling of new plot window for each
                 command.
 held         -  Returns true if the graphics is held on the current
                 device.
 env          -  Define a plot window, put on 'hold'.
 panel        -  Move to a specified plot panel when several panels are
                 defined.
 erase        -  Erase the current window (or panel).

 options      -  Get the options set for the present output device.
 id           -  The ID for the device.
 device       -  The device type.
 name         -  The window name.

Notes: C<$transform> for image/cont etc. is used in the same way as the
C<TR()> array in the underlying PGPLOT FORTRAN routine but is, fortunately,
zero-offset. The L<transform()|/transform> routine can be used to create this piddle.

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
say, with C<pgslw()> are preserved. Some modifications apply when using
the OO interface, see below.

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
to the function wanted (it can be placed anywhere in the command list).

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
wants to plot the same arrow with several sizes. Please note that it is
B<not> possible to set arrowsize and character size in the same call to
a plotting function. This should not be a problem in most cases.

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

 BORDER => { TYPE => 'rel', VALUE => 0.05 }

=item charsize

Set the character/symbol size as a multiple of the standard size.

 $opt = {CHARSIZE => 1.5}

=item colour (or color)

Set the colour to be used for the subsequent plotting. This can be
specified as a number, and the most used colours can also be specified
with name, according to the following table (note that this only works for
the default colour map):

  0 - WHITE    1 - BLACK     2 - RED      3 - GREEN    4 - BLUE
  5 - CYAN     6 - MAGENTA   7 - YELLOW   8 - ORANGE  14 - DARKGRAY
 16 - LIGHTGRAY

However there is a much more flexible mechanism to deal with colour.
The colour can be set as a 3 or 4 element anonymous array (or piddle)
which gives the RGB colours. If the array has four elements the first
element is taken to be the colour index to change. For normal work you
might want to simply use a 3 element array with R, G and B values and
let the package deal with the details. The R,G and B values go from 0
to 1.

In addition the package will also try to interpret non-recognised
colour names using the default X11 lookup table, normally using the
C<rgb.txt> that came with PGPLOT.

For more details on the handling of colour it is best that the user
consults the PGPLOT documentation. Further details on the handling of
colour can be found in the documentation for the internal routine
L<_set_colour|/_set_colour>.

=item filltype

Set the fill type to be used by L<poly|/poly>, L<circle|/circle>,
L<ellipse|/ellipse>, and L<rectangle|/rectangle>
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
the PGPLOT numbering:

 1 - SOLID line
 2 - DASHED
 3 - DOT-DASH-dot-dash
 4 - DOTTED
 5 - DASH-DOT-DOT-dot

or using name (as given in capitals above).
Thus the following two specifications both specify the line to be dotted:

 $opt = {LINESTYLE => 4};
 $varopt = {LINESTYLE => 'DOTTED'};

The names are not case sensitive, but the full name is required.

=item linewidth

Set the line width. It is specified as a integer multiple of 0.13 mm.

 $opt = {LINEWIDTH => 10}; # A rather fat line

=item plotting range

Explicitly set the plot range in x and y. X-range and Y-range are set
separately via the aptly named options C<Xrange> and C<Yrange>. If omitted
PGPLOT selects appropriate defaults (minimum and maximum of the data range
in general). These options are ignored if the window is on hold.

  line $x, $y, {xr => [0,5]}; # y-range uses default
  line $x, $y, {Xrange => [0,5], Yrange => [-1,3]}; # fully specified range

=back

=head1 OBJECT-ORIENTED INTERFACE

This section will briefly describe how the PDL::Graphics::PGPLOT::Window
package can be used in an object-oriented (OO) approach and what the
advantages of this would be. We will start with the latter

=over

=item Multiple windows.

For the common user it is probably most interesting to use the OO interface
when handling several open devices at the same time. If you have one
variable for each plot device it is easier to distribute commands to the
right device at the right time. This is the angle we will take in the rest
of this description.

=item Coding and abstraction

At a more fundamental level it is desirable to approach a situation where
it is possible to have a generic plotting interface which gives access
to several plotting libraries, much as PGPLOT gives access to different
output devices. Thus in such a hypothetical package one would say:

  my $win1 = Graphics::new('PGPLOT', {Device => '/xs'});
  my $win2 = Graphics::new('gnuplot', {Background => 'Gray'};

From a more practical point of of view such abstraction also comes in
handy when you write a large program package and you do not want to import
routines nilly-willy in which case an OO approach with method calls is a
lot cleaner.


=back

Anyway, enough philosophizing, let us get down to Earth and give some
examples of the use of OO PGPLOT. As an example we will take Odd (which
happens to be a common Norwegian name) who is monitoring the birth of
rabbits in O'Fib-o-nachy's farm (alternatively he can of course be
monitoring processes or do something entirely different). Odd wants the
user to be able to monitor both the birth rates and accumulated number
of rabbits and the spatial distribution of the births. Since these are
logically different he chooses to have two windows open:

  $rate_win = PDL::Graphics::PGPLOT::Window->new(Device => '/xw',
              Aspect => 1, WindowWidth => 5, NXPanel => 2);

  $area_win = PDL::Graphics::PGPLOT::Window->new(Device => '/xw',
              Aspect => 1, WindowWidth => 5);

See the documentation for L<new|/new> below for a full overview of the
options you can pass to the constructor.

Next, Odd wants to create plotting areas for subsequent plots and maybe
show the expected theoretical trends

  $rate_win->env(0, 10, 0, 1000, {XTitle => 'Days', YTitle => '#Rabbits'});
  $rate_win->env(0, 10, 0, 100, {Xtitle=>'Days', Ytitle => 'Rabbits/day'});

  $area_win->env(0, 1, 0, 1, {XTitle => 'Km', Ytitle => 'Km'});
  # And theoretical prediction.
  $rate_win->line(sequence(10), fibonacci(10), {Panel => [1, 1]});

That is basically it. The commands should automatically focus the relevant
window. Due to the limitations of PGPLOT this might however lead you to
plot in the wrong panel... The package tries to be smart and do this
correctly, but might get it wrong at times.


=head1 STATE and RECORDING

A new addition to the graphics interface is the ability to record plot
commands. This can be useful when you create a nice-looking plot on the
screen that you want to re-create on paper for instance. Or if you want
to redo it with slightly changed variables for instance. This is still
under development and views on the interface are welcome.

The functionality is somewhat detached from the plotting functions
described below so I will discuss them and their use here.

Recording is off by default. To turn it on when you create a new
device you can set the C<Recording> option to true, or you can set
the C<$PDL::Graphics::PGPLOT::RECORDING> variable to 1. I recommend doing the
latter in your C<.perldlrc> file at least since you will often have use
for recording in the perldl script.

=head2 Use of recording

The recording is meant to help you recreate a plot with new data or
to a different device. The most typical situation is that you have
created a beautiful plot on screen and want to have a Postscript file
with it. In the dreary old world you needed to go back and execute all
commands manually, but with this wonderful new contraption, the recorder,
you can just replay your commands:

  dev '/xs', {Recording => 1}
  $x = sequence(10)
  line $x, $x**2, {Linestyle => 'Dashed'}
  $s = retrieve_state() # Get the current tape out of the recorder.
  dev '/cps'
  replay $s

This should result in a C<pgplot.ps> file with a parabola drawn with a
dashed line. Note the command C<retrieve_state> which retrieves the current
state of the recorder and return an object (of type PDL::Graphics::State)
that is used to replay commands later.

=head2 Controlling the recording

Like any self-respecting recorder you can turn the recorder on and off
using the C<turn_on_recording> and C<turn_off_recording> respectively.
Likewise you can clear the state using the C<clear_state> command.

  $w=PDL::Graphics::PGPLOT::Window->new(Device => '/xs');
  $w->turn_on_recording;
  $x=sequence(10); $y=$x*$x;
  $w->line($x, $y);
  $w->turn_off_recording;
  $w->line($y, $x);
  $w->turn_on_recording;
  $w->line($x, $y*$x);
  $state = $w->retrieve_state();

We can then replay C<$state> and get a parabola and a cubic plot.

  $w->replay($state);

=head2 Tips and Gotchas!

The data are stored in the state object as references to the real
data. This leads to one good and one potentially bad consequence:

=over

=item The good is that you can create the plot and then subsequently
redo the same plot using a different set of data. This is best explained
by an example. Let us first create a simple gradient image and get
a copy of the recording:

  $im = sequence(10,10)
  imag $im
  $s=retrieve_state

Now this was a rather dull plot, and in reality we wanted to show an
image using C<rvals>. Instead of re-creating the plot (which of course
here would be the simplest option) we just change C<$im>:

  $im -= sequence(10,10)
  $im += rvals(10,10)

Now replay the commands

  replay $s

And hey presto! A totally different plot. Note however the trickery
required to avoid losing reference to C<$im>

=item This takes us immediately to the major problem with the recording
though. Memory leakage! Since the recording keeps references to the data
it can keep data from being freed (zero reference count) when you expect
it to be. For instance, in this example, we lose totally track of the
original $im variable, but since there is a reference to it in the state
it will not be freed

  $im = sequence(1000,1000)
  imag $im
  $s = retrieve_state
  $im = rvals(10,10)

Thus after the execution of these commands we still have a reference to
a 1000x1000 array which takes up a lot of memory...

The solution is to call C<clear> on the state variable:

  $s->clear()

(This is done automatically if the variable goes out of scope). I forsee
this problem to most acute when working on the C<perldl> command line,
but since this is exactly where the recording is most useful the best
advice is just to be careful and call clear on state variables.

If you are working with scripts and use large images for instance I would
instead recommend that you do not turn on recording unless you need it.

=back




=head1 FUNCTIONS

A more detailed listing of the functions and their usage follows. For
all functions we specify which options take effect and what other options
exist for the given function. The function descriptions below are all
given for the non-OO usage for historical reasons, but since the conversion
to an OO method is trivial there is no major need for concern. Whenever you
see a function example of the form

  Usage: a_simple_function($x, $y, $z [, $opt]);

and you wish to use the OO version, just let your mind read the above line
as:

  Usage: $win->a_simple_function($x, $y, $z [, $opt]);

where C<$win> is a PDL::Graphics::PGPLOT::Window object. That is all.


=head2 Window control functions.

=for ref Internal

=head2 new

=for ref

Constructor for PGPLOT object/device/plot window.

=for usage

  Usage: PDL::Graphics::PGPLOT::Window->new($opt);
  Usage: PDL::Graphics::PGPLOT::Window->new($option=>$value,...);

Options to new() can either be specified via a reference to a hash

  $win = PDL::Graphics::PGPLOT::Window->new({Dev=>'/xserve',ny=>2});

or directly, as an array

  # NOTE: no more {} !
  $win = PDL::Graphics::PGPLOT::Window->new(Dev=>'/xserve',ny=>2);

The following lists the recognised options:

=over

=item AspectRatio

The aspect ratio of the image, in the sense vertical/horizontal.  If both
this and WindowWidth are set to zero, the default view surface is used.
(This is the default case).

=item Device

The type of device to use. The syntax of this is the one used by PGPLOT.

=item Hold

Hold the plot window so that subsequent plots can plot over existing plots.
This can be adjusted with the C<hold()> and C<release()> methods.

=item NXPanel

The number of panels in the X-direction

=item NYPanel

The number of panels in the Y-direction

=item WindowName

The name to give to the window. No particular use is made of this at present.
It would be great if it was possible to change the title of the window frame.

=item WindowWidth

The width of the window in inches. If this is set to 0.0, the biggest window
consistent with the AspectRatio setting will be chosen. If both this and
AspectRatio are set to zero, the default view surface is used.
(This is the default case).

=item WindowXSize and WindowYSize

These two options allow an alternative setting of WindowWidth and AspectRatio.
Their values are actually not parsed here, but rather subsequently in the
L<_setup_window|/_setup_window> routine below.

=back

An important point to note is that the default values of most options can be
specified by passing these to the constructor. All general options (common to
several functions) can be adjusted in such a way, but function specific
options can not be set in this way (this is a design limitation which is
unlikely to be changed).

Thus the following call will set up a window where the default axis colour
will be yellow and where plot lines normally have red colour and dashed
linestyle.

  $win = PDL::Graphics::PGPLOT::Window->new(Device => '/xs',
          AxisColour => 'Yellow', Colour => 'Red', LineStyle => 'Dashed');


=head2 close

=for ref

Close a plot window

=for usage

  Usage: $win->close()

Close the current window. This does not necessarily mean that the
window is removed from your screen, but it does ensure that the
device is closed.

A message will be printed to STDOUT giving the name of the 
file created if the plot was made to a hardcopy device and
C<$PDL::verbose> is true.

=head2 held

=for ref

Check if a window is on hold

=for usage

  $is_held = $win->held();

Function to check whether the window is held or not.


=head2 hold

=for ref

Hold the present window.

=for usage

 Usage: $win->hold()

Holds the present window so that subsequent plot commands overplots.


=head2 panel

=for ref

Switch to a different panel

=for usage

  $win->panel(<num>);

Move to a different panel on the plotting surface. Note that you will need
to erase it manually if that is what you require.

=head2 release

=for ref

Release a plot window.

=for usage

   $win->release()

Release a plot window so that subsequent plot commands move to the next
panel or erase the plot and create a new plot.

=head2 erase

=for ref

Erase plot

=for usage

  $win->erase($opt);

Erase a plot area. This accepts the option C<Panel> or alternatively a number
or array reference which makes it possible to specify the panel to erase when
working with several panels.

=head2 Plotting functions

=for ref Internal

=head2 env

=for ref

Define a plot window, and put graphics on 'hold'

=for usage

 $win->env( $xmin, $xmax, $ymin, $ymax, [$justify, $axis] );
 $win->env( $xmin, $xmax, $ymin, $ymax, [$options] );

C<$xmin>, C<$xmax>, C<$ymin>, C<$ymax> are the plot boundaries.
C<$justify> is a boolean value (default is B<0>);
if true the axes scales will be the same (see C<justify>).
C<$axis> describes how the axes should be drawn (see
C<axis>) and defaults to B<0>.

If the second form is used, $justify and $axis can be set in the options
hash, for example:

 $win->env( 0, 100, 0, 50, {JUSTIFY => 1, AXIS => 'GRID', 
			    CHARSIZE => 0.7} );

In addition the following options can also be set for C<env>:

=over

=item PlotPosition

The position of the plot on the page relative to the view surface in
normalised coordinates as an anonymous array. The array should contain
the lower and upper X-limits and then the lower and upper Y-limits. To
place two plots above each other with no space between them you could do

  $win->env(0, 1, 0, 1, {PlotPosition => [0.1, 0.5, 0.1, 0.5]});
  $win->env(5, 9, 0, 8, {PlotPosition => [0.1, 0.5, 0.5, 0.9]});

=item Axis, Justify, Border

See the description of general options for these options.

=item AxisColour

Set the colour of the coordinate axes.

=item XTitle, YTitle, Title, Font, CharSize

Axes titles and the font and size to print them.

=back

=head2 label_axes

=for ref

Label plot axes

=for usage

  $win->label_axes(<xtitle>, <ytitle>, <plot title>, $options);

Draw labels for each axis on a plot.

=head2 imag

=for ref

Display an image (uses C<pgimag()>/C<pggray()> as appropriate)

=for usage

 $win->imag ( $image,  [$min, $max, $transform], [$opt] )

Notes: C<$transform> for image/cont etc. is used in the same way as the
C<TR()> array in the underlying PGPLOT FORTRAN routine but is,
fortunately, zero-offset. The L<transform()|/transform> routine can be used to
create this piddle.

There are several options related to scaling.  By default, the image
is scaled to fit the PGPLOT default viewport on the screen.  Scaling,
aspect ratio preservation, and 1:1 pixel mapping are available.
(1:1 pixel mapping GREATLY increases the speed of pgimag, and is useful
for, eg, movie display; but it's not recommended for final output as
it's not device-independent.)

To draw a colour bar (or wedge), either use the C<DrawWedge> option,
or the C<draw_wedge()> routine (once the image has been drawn).

Options recognised:

       ITF - the image transfer function applied to the pixel values. It
             may be one of 'LINEAR', 'LOG', 'SQRT' (lower case is 
             acceptable). It defaults to 'LINEAR'.
      MIN  - Sets the minimum value to be used for calculation of the
             display stretch
      MAX  - Sets the maximum value for the same
 TRANSFORM - The transform 'matrix' as a 6x1 vector for display
      PIX  - Sets the image pixel aspect ratio.  By default, imag
             stretches the image pixels so that the final image aspect
             ratio fits the viewport exactly.  Setting PIX=>1 causes
             the image aspect ratio to be preserved.  (the image is
             scaled to avoid cropping, unless you specify scaling 
             manually).  Larger numbers yield "landscape mode" pixels.
     PITCH - Sets the number of image pixels per screen unit, in the Y
             direction.  The X direction is determined by PIX, which 
             defaults to 1 if PITCH is specified and PIX is not.  PITCH 
             causes UNIT to default to "inches" so that it is easy to say 
             100dpi by specifying {PITCH=>100}.  Larger numbers yield 
             higher resolution (hence smaller appearing) images.
      UNIT - Sets the screen unit used for scaling.  Must be one of the
             PGPLOT supported units (inch, mm, pixel, normalized).  You
             can refer to them by name or by number.  Defaults to pixels
             if not specified.
     SCALE - Syntactic sugar for the reciprocal of PITCH.  Makes the
             UNIT default to "pixels" so you can say "{SCALE=>1}"
             to see your image in device pixels.   Larger SCALEs lead
             to larger appearing images.
 DrawWedge - set to 1 to draw a colour bar (default is 0)
     Wedge - see the draw_wedge() routine

The following standard options influence this command:

 AXIS, BORDER, JUSTIFY

=for example

   To see an image with maximum size in the current window, but square
   pixels, say:
         $win->imag( $a, { PIX=>1 } );
   An alternative approach is to try:
         $win->imag( $a, { JUSTIFY=>1 } );
   To see the same image, scaled 1:1 with device pixels, say:
         $win->imag( $a, { SCALE=>1 } );
   To see an image made on a device with 1:2 pixel aspect ratio, with 
   X pixels the same as original image pixels, say
         $win->imag( $a, { PIX=>0.5, SCALE=>2 } );
   To display an image at 100 dpi on any device, say:
         $win->imag( $a, { PITCH=>100 } );
   To display an image with 100 micron pixels, say:
         $win->imag( $a, { PITCH=>10, UNIT=>'mm' } );

=head2 imag1

=for ref

Display an image with correct aspect ratio 

=for usage

 $win->imag1 ( $image, [$min, $max, $transform], [$opt] )

This is syntactic sugar for 

  $win->imag( { PIX=>1 } );

=head2 draw_wedge

=for ref

Add a wedge (colour bar) to an image.

=for usage

 $win->draw_wedge( [$opt] )

Adds a wedge - shows the mapping between colour and value for a pixel - to
the current image.  This can also be achieved by setting C<DrawWedge> to 1
when calling the C<imag> routine.

The colour and font size are the same as used to draw the image axes
(although this will probably fail if you did it yourself).  To control the size
and location of the wedge, use the C<Wedge> option, giving it a hash reference
containing any of the following:

=over 4

=item Side

Which side of the image to draw the wedge: can be one of 'B', 'L', 'T', or
'R'. Default is B<'R'>.

=item Displacement

How far from the egde of the image should the wedge be drawn, in units of character
size. To draw within the image use a negative value. Default is B<2>.

=item Width

How wide should the wedge be, in units of character size.  Default is B<3>. 

=item Label

A text label to be added to the wedge.  If set, it is probably worth
increasing the C<Width> value by about 1 to keep the text readable.
Default is B<''>.

=item ForeGround (synonym Fg)

The pixel value corresponding to the "maximum" colour.  If C<undef>, uses the 
value used by C<imag> (recommended choice).  Default is C<undef>.

=item BackGround (synonym Bg)

The pixel value corresponding to the "minimum" colour.  If C<undef>, uses the 
value used by C<imag> (recommended choice).  Default is C<undef>.

=back

=for example

 $a = rvals(50,50);
 $win = PDL::Graphics::PGPLOT::Window->new();
 $win->imag( $a, { Justify => 1, ITF => 'sqrt' } );
 $win->draw_wedge( { Wedge => { Width => 4, Label => 'foo' } } );
 # although the following might be more sensible
 $win->imag( $a, { Justify => 1, ITF => 'sqrt', DrawWedge => 1,
     Wedge => { Width => 4, Label => 'foo'} } );

=head2 ctab

=for ref

Load an image colour table.

 Usage:

=for usage

   ctab ( $name, [$contrast, $brightness] ) # Builtin col table
   ctab ( $ctab, [$contrast, $brightness] ) # $ctab is Nx4 array
   ctab ( $levels, $red, $green, $blue, [$contrast, $brightness] )
   ctab ( '', $contrast, $brightness ) # use last color table

Note: See L<PDL::Graphics::LUT|PDL::Graphics::LUT> for access to a large
number of colour tables.

=head2 line

=for ref

Plot vector as connected points

If the 'MISSING' option is specified, those points in the C<$y> vector
which are equal to the MISSING value are not plotted, but are skipped
over.  This allows one to quickly draw multiple lines with one call to
C<line>, for example to draw coastlines for maps.

=for usage

 Usage: line ( [$x,] $y, [$opt] )

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

C<SymbolSize> allows to adjust the symbol size, it defaults to CharSize.

The C<ColorValues> option allows one to plot XYZ data with the
Z axis mapped to a color value.  For example:

 use PDL::Graphics::LUT;
 ctab(lut_data('idl5')); # set up color palette to 'idl5' 
 points ($x, $y, {ColorValues => $z});

=for example

 $y = sequence(10)**2+random(10);
 # Plot blue stars with a solid line through:
 points $y, {PLOTLINE => 1, COLOUR => BLUE, symbol => STAR}; # case insensitive

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
fortunately, zero-offset. The L<transform()|/transform> routine can be used to
create this piddle.

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

Plot vector as histogram (e.g. C<bin(hist($data))>)

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

Note that meddling with the C<ioffset> and C<bias> often will require you to
change the default plot range somewhat. It is also worth noting that if
you have TriD working you will probably be better off using
L<mesh3d|PDL::Graphics::TriD/mesh3d> or
a similar command - see the L<PDL::Graphics::TriD|PDL::Graphics::TriD>
module.

=for example

 $r=sequence(100)/50-1.0;
 $y=exp(-$r**2)*transpose(exp(-$r**2))
 hi2d $y, {IOFF => 1.5, BIAS => 0.07};

=head2 arrow

=for ref

Plot an arrow

=for usage

 Usage: arrow($x1, $y1, $x2, $y2, [, $opt]);

Plot an arrow from C<$x1, $y1> to C<$x2, $y2>. The arrow shape can be
set using the option C<Arrow>. See the documentation for general options
for details about this option (and the example below):

=for example

Example:

  arrow(0, 1, 1, 2, {Arrow => {FS => 1, Angle => 60, Vent => 0.3, Size => 5}});

which draws a broad, large arrow from (0, 1) to (1, 2).

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

=head2 circle

=for ref

Plot a circle on the display using the fill setting.

=for usage

 Usage: circle($x, $y, $radius [, $opt]);

All arguments can alternatively be given in the options hash using the
following options:

=over

=item XCenter and YCenter

The position of the center of the circle

=item Radius

The radius of the circle.

=back

=head2 ellipse

=for ref

Plot an ellipse, optionally using fill style.

=for usage

 Usage: ellipse($x, $y, $a, $b, $theta [, $opt]);

All arguments can alternatively be given in the options hash using the
following options:

=over

=item MajorAxis

The major axis of the ellipse - this must be defined or C<$a> must be given.

=item MinorAxis

The minor axis, like A this is required.

=item Theta (synonym Angle)

The orientation of the ellipse - defaults to 0.0. This is given in
radians.

=item XCenter and YCenter

The coordinates of the center of the ellipse. These must be specified or
C<$x> and C<$y> must be given.

=item NPoints

The number of points used to draw the ellipse. This defaults to 100 and
might need changing in the case of very large ellipses.

=back

The routine also recognises the same standard options as
accepted by L<poly|/poly>.

=head2 rectangle

=for ref

Draw a rectangle.

=for usage

 Usage: rectangle($xcenter, $ycenter, $xside, $yside, [, $angle, $opt]);

This routine draws a rectangle with the chosen fill style. Internally
it calls L<poly|/poly> which is somewhat slower than C<pgrect> but which
allows for rotated rectangles as well. The routine recognises the same
options as C<poly> and in addition the following:

=over

=item XCenter and YCenter

The position of the center of the rectangle. XCentre and YCentre are
valid synonyms.

=item XSide and YSide

The length of the X and Y sides. If only one is specified the
shape is taken to be square with that as the side-length, alternatively
the user can set Side

=item Side

The length of the sides of the rectangle (in this case a square) - syntactic
sugar for setting XSide and YSide identical. This is overridden by XSide
or YSide if any of those are set.

=item Angle (synonym Theta)

The angle at which the rectangle is to be drawn. This defaults to 0.0 and
is given in radians.


=back


=head2 vect

=for ref

Display 2 images as a vector field

=for usage

 Usage: vect ( $a, $b, [$scale, $pos, $transform, $misval] )

Notes: C<$transform> for image/cont etc. is used in the same way as the
C<TR()> array in the underlying PGPLOT FORTRAN routine but is,
fortunately, zero-offset. The L<transform()|/transform> routine can be used to
create this piddle.

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


=head2 transform

=for ref

Create transform array for contour and image plotting

=for usage

 $win->transform([$xdim,$ydim], $options);

This function creates a transform array in the format required by the image
and contouring routines. You must call it with the dimensions of your image
as arguments or pass these as an anonymous hash - see the example below.

=over

=item Angle

The rotation angle of the transform

=item ImageDimensions

The dimensions of the image the transform is required for. The dimensions
should be passed as a reference to an array.

=item Pixinc

The increment in output coordinate per pixel.

=item ImageCenter (or ImageCentre)

The centre of the image as an anonymous array B<or> as a scalar. In the
latter case the x and y value for the center will be set equal to this
scalar. This is particularly useful in the common case  when the center
is (0, 0).

=item RefPos (or ReferencePosition)

If you wish to set a pixel other than the image centre to a given
value, use this option. It should be supplied with a reference to an array
containing 2 2-element array references, e.g.

 RefPos => [ [ $xpix, $ypix ], [ $xplot, $yplot ] ]

This will label pixel C<($xpix,$ypix)> as being at position
C<($xplot,$yplot)>. The C<ImageCentre> option can be considered
to be a special case of this option, since the following are identical
(although one is a lot easier to type ;)

 ImageCentre => [ $xc, $yc ]
 RefPos      => [ [($nx-1)/2,($ny-1)/2], [ $xc, $yc ] ]

The values supplied in C<ImageCentre> are used 
if I<both> C<ImageCentre> and C<RefPos> are supplied in the
options list.

=back

Example:

   $im = rvals(100, 100);
   $w = PDL::Graphics::PGPLOT::Window->new(Device => '/xs');
   $t = $w->transform(dims($im), {ImageCenter => 0,  Pixinc => 5});
   $w->imag($im, {Transform => $t});

=head2 tline

=for ref

Threaded line plotting

=for usage

 $win->tline($x, $y, $options);

This is a threaded interface to C<line>. This is convenient if you have
a 2D array and want to plot out every line in one go. The routine will
apply any options you apply in a "reasonable" way. In the sense that it
will loop over the options wrapping over if there are less options than
lines.

Example:

  $h={Colour => ['Red', '1', 4], Linestyle => ['Solid' ,'Dashed']};
  $tx=zeroes(100,5)->xlinvals(-5,5);
  $ty = $tx + $tx->yvals;
  $win->tline($tx, $ty, $h);

=head2 tpoints

=for ref

A threaded interface to points

=for usage

 Usage: tpoints($x, $y, $options);

This is a threaded interface to C<points>. This is convenient if you have
a 2D array and want to plot out every line in one go. The routine will
apply any options you apply in a "reasonable" way. In the sense that it
will loop over the options wrapping over if there are less options than
lines.

Example:

  $h={Colour => ['Red', '1', 4], Linestyle => ['Solid' ,'Dashed']};
  $tx=zeroes(100,5)->xlinvals(-5,5);
  $ty = $tx + $tx->yvals;
  tpoints($tx, $ty, $h);


=head2 Text routines

=for ref Internal


=head2 text

=for ref

Write text in a plot window at a specified position.

=for usage

 Usage: text ($text, $x, $y [, $opt])

Options recognised:

=over

=item C<ANGLE>

The angle in degrees between the baseline of the text and
the horisontal (increasing counter-clockwise). This defaults to 0.

=item C<JUSTIFICATION>

The justification of the text relative to the position specified. It
defaults to 0.0 which gives left-justified text. A value of 0.5 gives
centered text and a value of 1.0 gives right-justified text.

=item C<XPos>, C<YPos>, C<Text>

These gives alternative ways to specify the text and position.

=item C<BackgroundColour>

This sets the background colour for the text in case an opaque background
is desired. You can also use the synonyms C<Bg> and C<BackgroundColor>.

=back

The following standard options influence this command:

   COLOUR, CHARSIZE

=for example

  line sequence(10), sequence(10)**2;
  text 'A parabola', 3, 9, {Justification => 1, Angle=>atan2(6,1)};


=head2 legend

=for ref

Add a legend to a plot

=for usage

 Usage: legend($text, $x, $y, [, $width], $opt]);

This function adds a legend to an existing plot. The action is primarily
controlled by information in the options hash, and the basic idea is that
C<$x> and C<$y> determines the upper left hand corner of the box in which
the legend goes. If the width is specified either as an argument or as
an option in the option hash this is used to determine the optimal character
size to fit the text into part of this width (defaults to 0.5 - see the
description of C<TextFraction> below). The rest of the width is filled out with
either lines or symbols according to the content of the C<LineStyle>,
C<Symbol>, C<Colour> and C<LineWidth> options.

The local options recognised are as follows:

=over

=item C<Text>

An anonymous array of annotations, can also be specified directly.

=item C<XPos> and C<YPos>

The X and Y position of the upper left-hand corner of the text.

=item C<Width> and C<Height>

The width and/or height of each line (including symbol/line). This is
used to determine the character size. If any of these are set to 'Automatic'
the current character size will be used.

=item C<TextFraction>

The text and the symbol/line is set inside a box. C<TextFraction>
determines how much of this box should be devoted to text. This
defaults to 0.5. You can also use C<Fraction> as a synonym to this.

=item C<TextShift>

This option allows for fine control of the spacing between the text and the
start of the line/symbol. It is given in fractions of the total width of the
legend box. The default value is 0.1.

=item C<VertSpace> or C<VSpace>

By default the text lines are separated by one character height (in the sense that
if the separation were 0 then they would lie on top of each other). The
C<VertSpace> option allows you to increase (or decrease) this gap in units of
the character height; a value of 0.5 would add half a character height to the
gap between lines, and -0.5 would remove the same distance.
The default value is 0.

=item C<BackgroundColour>

This sets the background colour for the text in case an opaque background
is desired. You can also use the synonyms C<Bg> and C<BackgroundColor>.


=back

=for example

  line $x, $y, {Color => 'Red', LineStyle => 'Solid'};
  line $x2, $y2, {Color => 'Blue', 'LineStyle' => 'Dashed', LineWidth => 10};

  legend ['A red line', 'A blue line'], 5, 5,
      {LineStyle => ['Solid', 'Dashed'], Colour => ['Red', 'Blue']
       LineWidth => [undef, 10]}; # undef gives default.


=head2 Cursor routines

=for ref Internal

=head2 cursor

=for ref

Interactively read cursor positions.

=for usage

 Usage: ($x, $y, $ch, $xref, $yref) = cursor($opt)

This routine has no standard input parameters, but the type of cursor
can be set by setting the option C<Type> as a key in the anonymous hash
C<$opt>. The first three return values from the function are always
defined and gives the position selected by the user and the character
pressed.

Depending on the cursor type selected the last two arguments might also
be defined and these give a reference position. For instance if the cursor
is selected to be C<Rectangle> then the reference position gives one of
the corners of the rectangle and C<$x> and C<$y> the diagonally opposite
one.

Options recognised:

=over

=item XRef, YRef

The reference position to be used

=item Type

The type of cursor. This can be selected using a number between 0 and 7 as
in PGPLOT, or alternatively you can specify these as, C<Default> (0),
C<RadialLine> (1), C<Rectangle> (2), C<TwoHorizontalLines> (3),
C<TwoVerticalLines> (4), C<HorizontalLine> (5), C<VerticalLine> (6)
and C<CrossHair> (7) respectively. The default cursor is just the normal
mouse cursor.

For the C<RadialLine> you I<must> specify the reference point, whereas for
the C<Two(Vertical|Horizontal)Lines> cursor the X or Y reference point,
respectively, must be specified.

=back

=for example

To select a region on a plot, use the rectangle cursor:

  ($x, $y, $ch, $xref, $yref) = cursor({Type => 'Rectangle'});
  poly pdl($x, $xref, $xref, $x, $x), pdl($y, $y, $yref, $yref, $y);

To select a region of the X-axis:

  ($x1, $y1, $ch) = cursor({Type => 'VerticalLine'});
  ($x2, $y2, $ch) = cursor({Type => 'TwoVerticalLines', XRef => $x1});


=head2 Internal routines

=for ref Internal

=cut




#'

package PDL::Graphics::PGPLOT::Window;

use PDL::Core qw/:Func :Internal/; # Grab the Core names
use PDL::Basic;
use PDL::Ufunc;
use PDL::Primitive;
use PDL::Types;
use PDL::Options;
use PDL::Graphics::State;
use PDL::Graphics::PGPLOTOptions qw(default_options);
use SelfLoader;
use Exporter;
use PGPLOT;

require DynaLoader;

@ISA = qw( Exporter SelfLoader DynaLoader );

bootstrap PDL::Graphics::PGPLOT::Window;
$PDL::Graphics::PGPLOT::RECORDING = 0; # By default recording is off..


#
# Note: Here the general and window creation specific options are read in
# from PGPLOTOptions. The $GeneralOptions variable is most importantly
# used in the new() routine to set the general options for the window.
#
# These are somewhat confusingly named perhaps. The WindowOptions are the
# options that affect window creation and setup such as width, shape etc.
# The GeneralOptions are options that affect all function calls in the package
# (or at least most) since it affects the default colour, character size etc.
# The problematic aspect here is the treatment of hardcopy settings. For
# historical reasons these are set in the WindowOptions variable but they
# should affect settings in the GeneralOptions variable...
# Ideally this should be re-coded, but to save some time I have instead opted
# for a patchy solution where they are specially treated in the new_window
# routine.
#
# Added 28/9/01 JB
# Delay the intialization of the window options so that it is possible
# to set the defaults in the .perldlrc file
my ($GeneralOptions, $WindowOptions) = (undef, undef);


my $PREVIOUS_DEVICE = undef;
my $PI = 4*atan2(1,1);
my $PREVIOUS_ENV = undef;

my $AUTOLOG = 0;

sub autolog {
  my $class = shift;
  my $ret;
  if (ref $class) {
    $ret = $class->{Autolog} || $AUTOLOG;
    $class->{Autolog} = shift if @_ > 0;
  } else {
    my $ret = $AUTOLOG;
    $AUTOLOG = shift if @_ > 0;
  }
  return $ret;
}

sub checklog {
  my ($self,$x,$y) = @_;
  $x = $x->log10->float if defined $x && $self->autolog && $self->{Logx};
  $y = $y->log10->float if defined $y && $self->autolog && $self->{Logy};
  # print STDERR "Logx: ",$self->{Logx},"\n";
  # print STDERR "Logy: ",$self->{Logy},"\n";
  return ($x,$y);
}

sub new {

  my $type = shift;

  # Set the default options!
  ($GeneralOptions, $WindowOptions) = default_options();
  # Turn off warnings for missing options...
  $GeneralOptions->warnonmissing(0);
  $WindowOptions->warnonmissing(0);

  # options are either given in a hash reference, or as a list
  # (which is converted to a hash reference to make the code easier)
  my $u_opt;
  if ( ref($_[0]) eq "HASH" ) { $u_opt = shift; }
  else                        { $u_opt = { @_ }; }
#  $u_opt={} unless defined($u_opt);

  my $opt = $WindowOptions->options($u_opt);
  $WindowOptions->full_options(0);
  my $user_options = $WindowOptions->current();
  $WindowOptions->full_options(1);

  # If the user set DEVICE then that overrides anything else...
  if (exists $user_options->{Device}) {
    $dev = $opt->{Device}
  } elsif (!defined($dev) || $dev eq "") {
    # Fall back on the default if first time or use $DEV otherwise..
    $dev = $PREVIOUS_DEVICE || $opt->{Device};
  }
  $PREVIOUS_DEVICE = $dev;


  my $this_opt = PDL::Options->new($opt);
  my $t=$WindowOptions->translation();
  $this_opt->translation($t);
  my $s=$WindowOptions->synonyms();
  $this_opt->synonyms($s);
  $this_opt->warnonmissing(0);

  # This is the setup for the plot options - which also can
  # be set on a per-window basis by the user.
  my $popt = $GeneralOptions->options($u_opt);
  my $this_plotopt = PDL::Options->new($popt);
  $t = $GeneralOptions->translation();
  $this_plotopt->translation($t);
  $s = $GeneralOptions->synonyms();
  $this_plotopt->synonyms($s);
  $this_plotopt->warnonmissing(0);

  # Modified 7/4/02 JB to add CTAB as an aspect of the window.
  my $self = {
	      'Options'	      => $this_opt,
	      'PlotOptions'   => $this_plotopt,
	      'Hold'	      => $opt->{Hold}		  || 0,
	      'Name'	      => $opt->{WindowName}	  || '',
	      'ID'	      => undef,
	      'AspectRatio'   => $opt->{AspectRatio},
	      'WindowWidth'   => $opt->{WindowWidth},
	      'NX'	      => $opt->{NXPanel}	  || 1,
	      'NY'	      => $opt->{NYPanel}	  || 1,
	      'Device'	      => $opt->{Device}		  || $DEV,
	      'CurrentPanel'  => 0,
	      '_env_options'  => undef,
	      'State'         => undef,
	      'Recording'     => $opt->{Recording}        || $PDL::Graphics::PGPLOT::RECORDING,
	      'CTAB'          => undef, # The default colour table
	     };

  if (defined($self->{Options})) {
    # Turn off warnings about missing options
    $self->{Options}->warnonmissing(0);
  }

  bless $self, ref($type) || $type;

  $self->_open_new_window($opt);
  # This weird setup is required to create the object.

  # We always have to create a state variable to avoid undefined errors.
  $self->{State}=PDL::Graphics::State->new();

  return $self;

}


#
# Graphics windows should be closed when they go out of scope.
# Thanks to Doug Burke for pointing this out.
#
sub DESTROY {
  my $self=shift;

  $self->close() unless !defined($self->{ID});
}


=head2 _open_new_window

Open a new window. This sets the window ID, which is the one used when
accessing a window later using C<pgslct>. It also sets the window name
to something easily remembered if it has not been set before.

=cut

sub _open_new_window {

  my $self = shift;

  my $window_nr = pgopen($self->{Device});
  if ($window_nr < 0) {
    barf("Opening new window (pgopen) failed: $window_nr\n");
  }
  $self->{ID} = $window_nr;
  $self->{Name} = "Window$window_nr" if $self->{Name} eq "";

  $self->_setup_window(@_);

}


=head2 _setup_window

This routine sets up a new window with its shape and size. This is also where
the two options C<WindowXSize> and C<WindowYSize> are parsed. These are then
forgotten (well, they are stored in $self->{Options}, but forget that) and
the corresponding aspect ratio and window width is stored.
Alternatively, the C<AspectRatio> and C<WindowWidth> options are used to
set the view surface.  The logic goes somewhat like this:

=over 8

=item 1

If C<WindowXSize> and C<WindowYSize> are both non-zero, then those are used.

=item 2

If C<AspectRatio> is non-zero and C<WindowWidth> is zero, 
C<WindowWidth> is calculated such that it a region with the given
aspect ratio will fit within the device's view surface.

=item 3

If both C<AspectRatio> and C<WindowWidth> are zero, the device's
default C<AspectRatio> and C<WindowWidth> is used.

=back

Finally the subpanels are set up using C<pgsubp> and colours and linewidth
are adjusted according to whether we have a hardcopy device or not.

=cut

sub _setup_window {
  my $self = shift;
  my $opt = shift;

  my $aspect = $self->{AspectRatio};
  my $width = $self->{WindowWidth};

  # Now some error-checking..
  if (defined($opt->{WindowXSize}) && defined($opt->{WindowYSize})) {
    if ($opt->{WindowXSize} == 0 || $opt->{WindowYSize}==0) {
      warn "A window can not have zero size in any direction!\n";
      print "The size options are ignored!\n";
    } else {
      # Check this!
      $aspect = $opt->{WindowXSize}/$opt->{WindowYSize};
      $width = $opt->{WindowXSize};
    }
  }
#  $self->{AspectRatio}=$aspect;
#  $self->{WindowWidth}=$width;

  # stop perl complaining when 'use strict' is on.
  # I am assuming that setting them to 0 is correct in this case
  # (Doug 03/14/01)
  $aspect = 0 unless defined $aspect;
  $width  = 0 unless defined $width;

  # grab whatever width fits the aspect ratio.
  # for PGPLOT, pgpap() does this automatically.
  # this is a place holder so we don't forget this case
  if ( $aspect && 0 == $width )
  {
  }

  # use the current view surface.  unfortunately, this isn't the
  # default view surface, but the *current* view surface.
  # PGPLOT doesn't provide a public routine to get the default view surface.
  elsif ( 0 == $aspect &&  0 == $width )
  {
    my ( $vs_x1, $vs_x2, $vs_y1, $vs_y2 );
    pgqvsz( 1, $vs_x1, $vs_x2, $vs_y1, $vs_y2 );

    $width = $vs_x2 - $vs_x1;
    $aspect = ( $vs_y2 - $vs_y1 ) / $width;
  }

  elsif ( 0 == $aspect && $width )
  {
    warn "The aspect must be specified if the window width is specified\n";
    $aspect = 0.618;
  }

  $self->{AspectRatio} = $aspect;
  $self->{WindowWidth} = $width;

  # Ok - call pgpap to set the size.
  # print "Window opened with Width=$width and AspectRatio=$aspect\n";
  pgpap($width, $aspect);

  # if just the AspectRatio was set, we don't know the WindowWidth;
  # grab that here.  don't do it by default to avoid resetting
  # values (due to round off) and confusing the user
  if ( $self->{AspectRatio} &&  0 == $self->{WindowWidth} )
  {
    my ( $vs_x1, $vs_x2, $vs_y1, $vs_y2 );
    pgqvsz( 1, $vs_x1, $vs_x2, $vs_y1, $vs_y2 );

    $self->{WindowWidth} = $vs_x2 - $vs_x1;
  }


  # Now do the sub-division into panels.
  my $nx = $self->{NX};
  my $ny = $self->{NY};
  if ($nx < 0) {
    warn "We do not support the alternative numbering of panels of PGPLOT!\n";
    $nx = abs($nx);
    $self->{NX}=abs($self->{NX});
  }
  pgsubp($nx, $ny);

  # Setup the colours
  my $o = $self->{Options}->current();
  pgask(0);
  pgqinf("HARDCOPY",$hcopy,$len);
  if ($hcopy eq "YES") {
    # This has changed to set the defaults instead.
    pgslw($o->{HardLW});
    pgsch($o->{HardCH});
    pgscf($o->{HardFont});
    # To change defaults you first need to read them out and then
    # adjust them and set them again
    my $temp_wo = $self->{PlotOptions}->defaults();
    $temp_wo->{Font}= $o->{HardFont};
    $temp_wo->{CharSize}= $o->{HardCH};
    $temp_wo->{LineWidth}= $o->{HardLW};
    $temp_wo->{Colour}= $o->{HardColour};
    $self->{PlotOptions}->defaults($temp_wo);
    my $temp_o=$self->{Options}->defaults();
    $temp_o->{AxisColour}=$o->{HardAxisColour};
    $temp_o->{CharSize}=$o->{HardCH};
    $self->{Options}->defaults($temp_o);
  } else {
    # Set the global properties as for the hardcopy device.
    pgsch($o->{CharSize});
    my $wo = $self->{PlotOptions}->defaults();
    pgscf($wo->{Font});
    pgslw($wo->{LineWidth});
  }
  my ($hcopy, $len);
  my $wo = $self->{PlotOptions}->defaults();

  $self->_set_colour($wo->{Colour});
  pgask(0);

}

sub _set_defaults {		# Set up defaults

  # Now check if this is a hardcopy device, in which case we
  # set a variety of properties differently.
  my $self = shift;

}




=head2 _status

This routine checks the status of the window. It returns OPEN if the window
is open and CLOSED if it is closed.

=cut

sub _status {

  my $self=shift;
  $self->focus();
  my ($state, $len);
  pgqinf('STATE',$state,$len);

  return $state;

}

=head2 _reopen

This functions reopens a window. Since this is an internal function it does
not have a lot of error-checking. Make sure the device is closed I<before>
calling this routine.

There is an unfortunate problem which pops up viz. that the window name
cannot be changed at this point since we are offering that to the rest of
the world. That might be sensible, but it means that the window name will
not reflect the id of the window - use C<id()> for that (this is also why
we do not call C<open_new_window> )

=cut

sub _reopen {

  my $self = shift;
  my $window_nr = pgopen($self->{Device});
  if ($window_nr < 0) {
    barf("Opening new window (pgopen) failed: $window_nr\n");
  }
  $self->{ID} = $window_nr;

  $self->_setup_window(@_);

}


=head2 _advance_panel

This routine advances one plot panel, updating the CurrentPanel as well.
If the advance will proceed past the page the page will be erased. Also
note that when you advance one panel the hold value will be changed.

=cut

sub _advance_panel {
  my $self = shift;

  my $new_panel = $self->{CurrentPanel}+1;
  if ($new_panel > ($self->{NX}*$self->{NY})) {
    # We are at the end of the page..
    $new_panel = 1;
    $self->clear_state();
    pgpage();
#    $self->{_env_set}=[];
  }

  $self->panel($new_panel);
  if ($self->held()) {
    $self->{Hold}=0;
    print "Graphic released (panel move)\n" if $PDL::verbose;
  }

}


=head2 _check_move_or_erase

This routine is a utility routine which checks if we need to move panel,
and if so will do this. It also checks if it is necessary to advance panels,
and whether they need to be erased.

=cut

sub _check_move_or_erase {

  my $self=shift;
  my ($panel, $erase)=@_;

  if (defined($panel)) {
    $self->panel($panel);
  } elsif (!$self->held()) {
    # If no hold has been set.
    $self->_advance_panel();
  }

  $self->erase() if $erase;

}


=head2 _thread_options

This function is a cludgy utility function that expands an options hash
to an array of hashes looping over options. This is mainly of use for
"threaded" interfaces to standard plotting routines.

=cut


sub _thread_options {
  my ($n, $h) = @_;

  # Loop over each option.
  my @hashes=(); # One for each option.
  my @keys = keys %$h;
  foreach my $k (@keys) {
    my @vals=();
    my $v=$h->{$k};
    $v = [$v] if ref($v) ne 'ARRAY';
    while ($#vals+1 < $n) {
      splice(@vals, @vals, 0, @$v);
    }
    for (my $i=0; $i<$n; $i++) {
      $hashes[$i]->{$k}=$vals[$i];
    }
  }
  return \@hashes;
}

############################
# Replay related functions #
############################

my $DEBUGSTATE = 0;

sub debug_state {
  $DEBUGSTATE = !$DEBUGSTATE;
}

sub replay {
  my $self = shift;
  my $state = shift || $self->{State};

  if (!defined($state)) {
    die "A state object must be defined to play back commands!\n";
  }

  my @list = $state->get();


  if ($#list < 0) {
    # If there are no commands, then the user might have forgotten to
    # turn on recording, let us remind him/her

    warn "Replaying an empty state - did you turn on recording?\n";
    print "Hint: Put PDL::Graphics::PGPLOT::RECORDING=1 in your .perldlrc file\n"
  }

  foreach my $arg (@list) {
    my ($command, $commandname, $arg, $opt)=@$arg;
    &$command($self, @$arg, $opt);
  }
}


sub clear_state {
  my $self = shift;
  print "Clearing state!\n" if $DEBUGSTATE;
  $self->{State}->clear();
}

sub turn_off_recording {
  my $self=shift;
  # Turning off does _NOT_ clear the state at the moment!
   $self->{Recording} =0;
  print "Turning off state!\n" if $DEBUGSTATE;
}
sub turn_on_recording {
  my $self=shift;
  # Previous calls are not recorded of course..
  print "Turning on state!\n" if $DEBUGSTATE;
  $self->{Recording} = 1;
  $self->{State}=PDL::Graphics::State->new() unless defined($self->{State});
}

sub _add_to_state {
  my $self=shift;
  my ($func, $arg, $opt)=@_;
  my ($pkg, $fname, $line, $funcname, $hasargs, $wantarray,
      $evaltext, $isrequire, $hints, $bitmask)=caller(1);
  # We only add if recording has been turned on.
  print "Adding to state ! $func, $arg, $opt\n" if $DEBUGSTATE;
  print "State = ".$self->{State}."\n" if $DEBUGSTATE;
  $self->{State}->add($func, $funcname, $arg, $opt) if $self->{Recording};
}

sub retrieve_state {
  my $self=shift;
  my $state_copy = $self->{State}->copy();
  print "Retriving state!\n" if $DEBUGSTATE;
  return $state_copy;
}


#####################################
# Window related "public" routines. #
#####################################

sub close {
  my $self=shift;
  # let the user know that we've created a file
  if ( $self->_status() eq 'OPEN' ) {
      my @info = $self->info( 'HARDCOPY', 'FILE' );
      print "Created: $info[1]\n" if $info[0] eq 'YES' and $PDL::verbose;
      pgclos();
  }
  $self->{ID}=undef;
  $self->clear_state();
}

=head2 options

Access the options used when I<originally> opening the window. At the moment
this is not updated when the window is changed later.

=cut

sub options {
  return $_[0]->{Options};
}

=head2 id

Access the window ID that PGPLOT uses for the present window.

=cut

sub id {
  return $_[0]->{ID};
}

=head2 device

This function returns the device type of the present window.

=cut

sub device {
  return $_[0]->{Device};
}

=head2 name

Accessor to set and examine the name of a window.

=cut

sub name {
  my $self=shift;
  if ($#_>=0) {
    $self->{Name}=$_[0];
  }
  return $self->{Name};
}

=head2 focus

Set focus for subsequent PGPLOT commands to this window.

=cut

sub focus {

  my $self=shift;
  return if !defined($self->{ID});
  my $sid; pgqid($sid);
  # Only perform a pgslct if necessary.
  pgslct($self->{ID}) unless $sid == $self->{ID};

}


sub hold {
  my $self=shift;
  $self->{Hold}=1;
  $self->_add_to_state(\&hold);
  return $self->{Hold};
}


sub release {
  my $self=shift;
  $self->{Hold}=0;
  $self->_add_to_state(\&release);
  return $self->{Hold};
}


sub held {
  my $self = shift;
  return $self->{Hold};
}




=head2 info

=for ref

Get general information about the PGPLOT environment.

=for usage

 @ans = $self->info( @item );

The valid values of C<@item> are as below, where case is not
important:

  VERSION     - What PGPLOT version is in use.
  STATE       - The status of the output device, this is returns 'OPEN'.
                if the device is open and 'CLOSED' otherwise.
  USER        - The username of the owner of the spawning program.
  NOW         - The current date and time in the format
                'dd-MMM-yyyy hh:mm'. Most people are likely to use Perl
                functions instead.
  DEVICE    * - The current PGPLOT device or file, see also device().
  FILE      * - The filename for the current device.
  TYPE      * - And the device type for the current device.
  DEV/TYPE  * - This combines DEVICE and TYPE in a form that can be used
                as input to new.
  HARDCOPY  * - This is flag which is set to 'YES' if the current device is
                a hardcopy device and 'NO' otherwise.
  TERMINAL  * - This flag is set to 'YES' if the current device is the
                user's terminal and 'NO' otherwise.
  CURSOR    * - A flag ('YES' or 'NO') to inform whether the current device
                has a cursor.

Those items marced with a C<*> only return a valid answer if
the window is open.  A question mark (C<?>) is returned
if the item is not recognised or the information is not available.

=cut

#'

sub info {
    my $self = shift;
    my @inq;
    if ( wantarray() ) { @inq = @_; }
    else               { push @ing, $_[0]; }

    $self->focus();
    my @ans;
    foreach my $inq ( @inq ) {
	my ( $state, $len );
	pgqinf( uc($inq), $state, $len );
	push @ans, $state;
    }
    return wantarray() ? @ans : $ans[0];
} # info()






sub panel {

  my $self = shift;

  $self->focus();
  my ($xpos, $ypos);
  if ($#_ == 1) {
    # We have gotten $x and $y..
    ($xpos, $ypos)=@_;
  } elsif ($#_ == 0 && ref($_[0]) eq 'ARRAY' ) {
    ($xpos, $ypos)=@{$_[0]};
  } elsif ($#_ == 0) {
    # We have been given a single number... This can be converted
    # to a X&Y position with a bit of calculation. The code is taken
    # from one2nd.
    my $i=$_[0]-1;		# The code is 0 offset..
    $xpos = $i % $self->{NX};
    $i = long($i/$self->{NX});
    $ypos=$i % $self->{NY};
    $xpos++; $ypos++;		# Because PGPLOT starts at 1..
  } else {
    barf <<'EOD'
 Usage: panel($xpos, $ypos);   or
        panel([$xpos, $ypos]); or
        panel($index);
EOD
  }

  # We do not subtract 1 from X because we would need to add it again to
  # have a 1-offset numbering scheme.
  $self->{CurrentPanel} = ($ypos-1)*$self->{NX}+($xpos);
  $self->_add_to_state(\&panel, $xpos, $ypos);
  pgpanl($xpos, $ypos);


}


{
  # To save space and time..
  my $erase_options = undef;
  sub erase {
    my $self = shift;

    # Parse options
    my $u_opt = shift;
    if (defined($u_opt) && ref($u_opt) eq 'HASH') {
      $erase_options = PDL::Options->new({Panel => undef}) if
	!defined($erase_options);
      my $o = $erase_options->options($u_opt);
      # Change panel if requested
      $self->panel($o->{Panel}) if defined($o->{Panel});
    } elsif (defined($u_opt)) {
      # The user has passed a number of reference to array..
      $self->panel($u_opt);
    }

    $self->focus();
    # What should I do with the state here????
    pgeras();
    $self->_add_to_state(\&erase, [], $u_opt);
    # Remove hold.
    $self->{Hold}=0;
  }

}


##
## Utility functions
##

=head2 _extract_hash

This routine takes and array and returns the first hash reference found as
well as those elements that are I<not> hashes. Note the latter point because
all other references to hashes in the array will be lost.

=cut

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

=head2 _parse_options

This is a convenience routine for parsing a set of options. It returns
both the full set of options and those that the user has set.

=cut

sub _parse_options {

  my $self=shift;
  my ($opt, $oin)=@_;

  ## Should do something sensible if $opt is no options object f.i.
  if (defined($oin) && ref($oin) ne 'HASH') {
    my ($package, $file, $line, $sub)=caller(1);
    barf "_parse_options called by $sub with non-hash options element!";
  } elsif (!defined($oin)) {
    my ($package, $file, $line, $sub)=caller(1);
    warn "_parse_options called by $sub without an options hash! - continuing\n";
    $oin = {};
  }
  my $o=$opt->options($oin);
  $opt->full_options(0);
  my $uo=$opt->current();
  $opt->full_options(1);

  $opt->clear_current();

  return ($o, $uo);

}


################################################################
#
#    GRAPHICS FUNCTIONS below!
#
################################################################

############ Local functions #################

=head2 _save_status

Saves the PGPLOT state so that changes to settings can be made and then
the present state restored by C<_restore_status>.

=cut

sub _save_status {
  my $self=shift;
  pgsave if $self->_status() eq 'OPEN';
}

=head2 _restore_status

Restore the PGPLOT state. See L</_save_status>.

=cut

sub _restore_status {
  my $self=shift;
  pgunsa if $self->_status() eq 'OPEN';
}



=head2 _checkarg

This routine checks and optionally alters the arguments given to it.

=cut

sub _checkarg {			# Check/alter arguments utility
  my $self = shift;
  my ($arg,$dims,$type,$nobarf) = @_;
  $type = $PDL_F unless defined $type;

  # nobarf added so the end-user can choose whether to die or not..x
  $nobarf = 0 unless defined($nobarf);
  my $ok = 1;

  $arg = topdl($arg);		# Make into a pdl
  $arg = convert($arg,$type) if $arg->get_datatype != $type;
  if (($arg->getndims > $dims)) {
    # Get the dimensions, find out which are == 1. If it helps
    # chuck these off and return trimmed piddle.
    my $n=nelem(which(pdl($arg->dims)==1));
    if (($arg->getndims-$n) > $dims) {
      $ok = 0;
      barf "Data is >".$dims."D" unless $nobarf;
    } else {
      my $count=0;      my $qq;
      my $s=join ',',
	map {if ($_ == 1 && $count<$arg->getndims-$dims) {$qq='(0)'; $count++}
	     else {
	       $qq= '';
	     }
	     ; $qq} $arg->dims;
      $arg=$arg->slice($s);
    }
  }
  $_[0] = $arg if $ok;	# Alter

  return $ok;
}

# a hack to store information in the object.  
# Currently only used by imag() for storing information
# useful to draw_wedge().  
#
# This routine needs changing:
#  . store values using PDL::Options, so you can update rather than overwrite
#  . associate the information with a particular window/panel/whatever
#  . clear information when plot erased (correct for current use by imag(), 
#    but maybe not in more general cases?)
#   
# The API is liable to change: you have been warned (Doug Burke)
#
sub _store {
    my $self = shift;
    barf 'Usage: _store( $self, $name, $item )' unless $#_ == 1;

    my $name   = shift;
    my $object = shift;

    # create storage space, if needed
    $self->{_horrible_storage_space} = {} 
    unless defined $self->{_horrible_storage_space};

    # store data
    $self->{_horrible_storage_space}{$name} = $object;


} # sub: _store()

# retrieve information from storage space
# - same caveats as with _store()
#
sub _retrieve {
    my $self = shift;
    barf 'Usage: _retrieve( $self, $name )' unless $#_ == 0;

    my $name = shift;

    barf "Internal error: no storage space in object"
	unless exists $self->{_horrible_storage_space};

    if ( exists $self->{_horrible_storage_space}{$name} ) {
	return $self->{_horrible_storage_space}{$name};
    } else {
	return undef;
    }

} # sub: _retrieve()

##################
# Options parser #
##################



=head2 _set_colour

This is an internal routine that encapsulates all the nastiness of
setting colours depending on the different PGPLOT colour models (although
HLS is not supported).

The routine works in the following way:

=over 8

=item *

At initialisation of the plot device the work colour index is set
to 16. The work index is the index the routine will modify unless the
user has specified something else.

=item *

The routine should be used after standard interpretation and synonym
matching has been used. So if the colour is given as input is an integer
that colour index is used.

=item *

If the colour is a reference the routine checks whether it is an
C<ARRAY> or a C<PDL> reference. If it is not an error message is given.
If it is a C<PDL> reference it will be converted to an array ref.

=item *

If the array has four elements the first element is interpreted
as the colour index to modify and this overrules the setting for the
work index used internally. Otherwise the work index is used and incremented
until the maximum number of colours for the output device is reached
(as indicated by C<pgqcol>). Should you wish to change that you need
to read the PGPLOT documentation - it is somewhat device dependent.

=item *

When the array has been recognised the R,G and B colours of the
user-set index or work index is set using the C<pgscr> command and we
are finished.

=item *

If the input colour instead is a string we try to set the colour
using the PGPLOT routine C<pgscrn> with no other error-checking. This
should be ok,  as that routine returns a rather sensible error-message.

=back

=cut

{
  my $work_ci = 16;

  sub _set_colour {
    my $self = shift;
    my ($col, $is_textbg) = @_;
    $is_textbg = 0 if !defined($is_textbg);

    # The colour index to use for user changes.
    # This is increased until the max of the colour map.
    # I don't know if this can change, but let's not take any
    # chances.
    my ($min_col, $max_col);
    pgqcol($min_col, $max_col);

    #
    # Extended treatment of colours - added 2/10/01 JB.
    #
    if (ref($col)) {
      if ((ref($col) eq 'PDL') or (ref($col) eq 'ARRAY')) {
	my @colvals = (ref($col) eq 'PDL' ? list($col) : @{$col});
	my ($r, $g, $b)=@colvals;
	my $index = $work_ci;
	if ($#colvals == 3) {
	  # This is a situation where the first element is interpreted
	  # as a PGPLOT colour index, otherwise we'll use our own
	  # strategy to step through indices.
	  ($index, $r, $g, $b)=@colvals;
	} else {
  	  $work_ci += 1;
	  # NB this does not work on devices with < 16 colours.
	  $work_ci = 16 if $work_ci > $max_col;
	}
	pgscr($index, $r, $g, $b);

	if ($is_textbg) {
	  pgstbg($index);
	} else {
	  pgsci($index);
	}
      } else {
	warn "The colour option must be a number, string, array or PDL!\n";
      }
    } else {
      # Now check if this is a name that could be recognised by pgscrn.
      # To simplify the logic we first check if $col is a digit.
      if ($col =~ m/^\s*\d+\s*$/) { 
	if ($is_textbg) {
	  pgstbg($col);
	} else {
	  pgsci($col);
	}
      } else {
	#
	# Ok, we either have an untranslated colour name or something
	# bogus - let PGPLOT deal with that!
	#
	my $ier;
	pgscrn($work_ci, $col, $ier);
	if ($is_textbg) {
	  pgstbg($work_ci);
	} else {
	  pgsci($work_ci);
	}
	$work_ci += 1;
	# NB this does not work on devices with < 16 colours.
	$work_ci = 16 if $work_ci > $max_col;
      }
    }


  }

}


=head2 _standard_options_parser

This internal routine is the default routine for parsing options. This
routine deals with a subset of options that most routines will accept.

=cut

sub _standard_options_parser {
  #
  # Parse the options and act on the values set.
  #
  my $self=shift;
  my ($o)=@_;

  #
  # The input hash has to contain the options _set by the user_
  #
  $self->_set_colour($o->{Colour}) if (exists($o->{Colour}));
  pgsls($o->{LineStyle})  if exists($o->{LineStyle});
  pgslw($o->{LineWidth})  if exists($o->{LineWidth});
  pgscf($o->{Font})	  if exists($o->{Font});
  pgsch($o->{CharSize})	  if exists($o->{CharSize});
  pgsfs($o->{Fill})	  if exists($o->{Fill});
#  pgsch($o->{ArrowSize})  if exists($o->{ArrowSize});
  # Two new options..


  my $wo = $self->{PlotOptions}->defaults(); # Window defaults - for some routines below

  # We just need special treatment of the Arrow and Hatch options,
  # and they are complex for historical reasons...

  if (exists($o->{Arrow})) {
    #
    # Set the arrow. The size can be set either independently
    # using ARROWSIZE or in the hash
    #
    # Note the use of $wo to get the true default values here!
    my ($fs, $angle, $vent)=($wo->{Arrow}{FS}, $wo->{Arrow}{Angle},
			     $wo->{Arrow}{Vent});
    my $arrowsize = $o->{CharSize}; # Default to the character size..
    if (ref($o->{Arrow}) eq 'HASH') {
      while (my ($var, $value)=each %{$o->{Arrow}}) {
	$fs=$value if $var =~ m/^F/i;
	$angle=$value if $var =~ m/^A/i;
	$vent=$value if $var =~ m/^V/i;
	$arrowsize=$value if $var =~ m/^S/i;
      }
    } else {
      $fs=$o->{Arrow}[0] if defined $o->{Arrow}[0];
      $angle=$o->{Arrow}[1] if defined $o->{Arrow}[1];
      $vent=$o->{Arrow}[2] if defined $o->{Arrow}[2];
      $arrowsize=$o->{Arrow}[3] if defined $o->{Arrow}[3];
    }
    pgsch($arrowsize) if defined($arrowsize);
    pgsah($fs, $angle, $vent);
  }

  if (exists($o->{Hatch})) {
    my $val = $o->{Hatch};
    if (!defined($val) || lc($val) eq 'default') {
      pgshs();			# Default values are either specfied by HATCH=>undef or HATCH=>'default'
    } else {
      #
      # Can either be specified as numbers or as a hash...
      #
      # Note the use of $wo to get the true default values!!
      #
      my ($angle, $separation, $phase)=
	($wo->{Hatch}{Angle}, $wo->{Hatch}{Separation}, $wo->{Hatch}{Phase});

      if (ref($val) eq 'HASH') {
	while (my ($var, $value) = each %{$val}) {
	  $angle=$value if $var =~ m/^A/i;
	  $separation=$value if $var =~ m/^S/i;
	  $phase=$value if $var =~ m/^P/i;
	}
      } else {
	$angle=$$val[0] if defined($$val[0]);
	$separation=$$val[1] if defined($$val[1]);
	$phase=$$val[2] if defined($$val[2]);
      }
      if ($separation==0) {
	warn "The separation of hatch lines cannot be zero, the default of".
	  $wo->{Hatch}{Separation} . " is used!\n";
	$separation=$wo->{Hatch}{Separation};
      }
      pgshs($angle,$separation, $phase);
    }
  }
}



# initenv( $xmin, $xmax, $ymin, $ymax, $just, $axis )
# initenv( $xmin, $xmax, $ymin, $ymax, $just )
# initenv( $xmin, $xmax, $ymin, $ymax, \%opt )
#
# \%opt can be supplied but not be defined
# we parse the JUSTIFY, AXIS, and BORDER options here,
# rather than have a multitude of checks below
#


sub initenv{
  my $self = shift;		# Default box

  # We must check the status of the object, and if not ready it must
  # be re-opened...
  $self->_status();

  my ($in, $u_opt)=_extract_hash(@_);
  my ($xmin, $xmax, $ymin, $ymax, $just, $axis)=@$in;
  $u_opt={} unless defined($u_opt);

  # If the user specifies $just or $axis these values will
  # override any options given. This actually changes the behaviour
  # of the previous initenv() function when $just and/or $axis was
  # specified in conjunction with an options hash.
  $u_opt->{Justify} = $just if defined($just);
  $u_opt->{Axis} = "$axis" if defined($axis);

  # Now parse the input options.
  my $o = $self->{Options}->options($u_opt); # Merge in user options...

  if ($self->autolog) {
    # print STDERR "Options: ",$o->{Axis},"\n";
    # use Data::Dumper; print Dumper $o->{Axis};
    $self->{Logx} = ($o->{Axis} == 10 || $o->{Axis} == 30 ||
		    $o->{Axis}[0] =~ /BCLNST/) ? 1 : 0;
    $self->{Logy} = ($o->{Axis} == 20 || $o->{Axis} == 30 ||
		    $o->{Axis}[1] =~ /BCLNST/) ? 1 : 0;
    ($xmin,$xmax) = map {
      barf "plot boundaries not positive in logx-mode" if $_ <= 0;
      log($_)/log(10) } ($xmin,$xmax)
	if $self->{Logx};
    ($ymin,$ymax) = map { 
      barf "plot boundaries not positive in logy-mode" if $_ <= 0;
      log($_)/log(10) } ($ymin,$ymax)
	if $self->{Logy};
  }

  # Save current colour and set the axis colours
  my ($col);
  pgqci($col);
  $self->_set_colour($o->{AxisColour});
  # Save current font size and set the axis character size.
  my ($chsz);
  pgqch($chsz);
  pgsch($o->{CharSize});

  if (ref($o->{Border}) eq 'HASH' || $o->{Border} != 0) {
    my $type  = "REL";
    my $delta = 0.05;
    if ( ref($o->{Border}) eq "HASH" ) {
      while (my ($bkey, $bval) = each %{$o->{Border}}) {
	$bkey = uc($bkey);
	if ($bkey =~ m/^TYP/) {
	  $type = uc $bval;
	} elsif ($bkey =~ m/^VAL/) {
	  $delta = $bval;
	}
      }				# while: (bkey,bval)
    }				# if: ref($val) eq "HASH"

    if ( $type =~ m/^REL/ ) {
      my $sep = ( $xmax - $xmin ) * $delta;
      $xmin -= $sep; $xmax += $sep;
      $sep = ( $ymax - $ymin ) * $delta;
      $ymin -= $sep; $ymax += $sep;
    } elsif ( $type =~ m/^ABS/ ) {
      $xmin -= $delta; $xmax += $delta;
      $ymin -= $delta; $ymax += $delta;
    } else {
      print "Warning: unknown BORDER/TYPE option '$type'.\n";
    }
  }

  #
  # This part of the code has changed from the previous PGPLOT
  # installation. The reason is that when we use several panels
  # and want to jump from one to another we cannot use pgenv since
  # that makes an annoying call to pgpage which we want to have
  # control over....
  #

  # A particular difficulty arises with the use of several panels.
  # It is here hopeless to do a pgpage since that will jump to the
  # next panel and that was exactly the reason why I have scrapped
  # pgenv.
  #
  # To give a consistent system and to tidy up we will call
  # erase if there are several panels, and pgpage otherwise.
  if (defined($o->{Erase}) && $o->{Erase}) {
    if ($self->{NX}*$self->{NY} > 1) {
      pgeras();
      $self->clear_state(); # Added to deal with new pages.
    } else {
      $self->clear_state(); # Added to deal with new pages.
      pgpage();
    }
  }

  if (!defined($o->{PlotPosition}) || $o->{PlotPosition} eq 'Default') {
    # Set standard viewport
    pgvstd();
  } else {
    barf "The PlotPosition must be given as an array reference!" unless
      ref($o->{PlotPosition}) eq 'ARRAY';
    my ($x1, $x2, $y1, $y2)=@{$o->{PlotPosition}};
    pgsvp ($x1, $x2, $y1, $y2);
  }

  # This behaviour is taken from the PGPLOT manual.
  if ($o->{Justify} == 1) {
    pgwnad($xmin, $xmax, $ymin, $ymax);
    if (ref($o->{Axis}) eq 'ARRAY') {
      pgtbox($o->{Axis}[0], 0.0, 0, $o->{Axis}[1], 0.0, 0);
    } else {
      pgtbox($o->{Axis}, 0.0, 0, $o->{Axis}, 0.0, 0);
    }
  } else {
    pgswin($xmin, $xmax, $ymin, $ymax);
    if (ref($o->{Axis}) eq 'ARRAY') {
      pgtbox($o->{Axis}[0], 0.0, 0, $o->{Axis}[1], 0.0, 0);
    } else {
      pgtbox($o->{Axis}, 0.0, 0, $o->{Axis}, 0.0, 0);
    }
  }
  $self->_set_env_options($xmin, $xmax, $ymin, $ymax, $o);
  $self->label_axes($u_opt);

  #  pgenv($xmin, $xmax, $ymin, $ymax, $o->{Justify}, $o->{Axis});
  $self->_set_colour($col);
  pgsch($chsz);

#  $self->{_env_set}[$self->{CurrentPanel}]=1;
  1;
}

# This is a tidy little routine to set the env options and update the global
# variable.
sub _set_env_options {
  my $self=shift;
  my @opt=@_;

  $self->{_env_options} = [@opt];
  $PREVIOUS_ENV = [@opt];
}

sub redraw_axes {
  my $self = shift;
  my $col;
  pgqci($col);
  my $o;
  if (defined($self->{_env_options})) {
    # Use the previous settings for the plot box.
    my $e = $self->{_env_options};
    $o=$$e[4];
  } else {
    $o=$self->{Options}->defaults();
  }
  $self->_set_colour($o->{AxisColour});
  my ($chsz);
  pgqch($chsz);
  pgsch($o->{CharSize});
  my $axval = $o->{Axis};	# Using the last for this window...
  $axval = 0 unless defined $axval; # safety check
  unless ( $self->{Hold} ) {
    if ( ref($axval) ) {
      pgtbox($$axval[0],0,0,$$axval[1],0,0);
    } else {
      pgtbox($axval,0,0,$axval,0,0);
    }
  }
  $self->_set_colour($col);
  pgsch($chsz);

  $self->_add_to_state(\&redraw_axes);


}


sub label_axes {

  my $self = shift;
  my ($in, $opt)=_extract_hash(@_);
  # :STATE RELATED:
  # THIS WILL PROBABLY NOT WORK as label_axes can be called both by
  # the user directly and by env... Let's see.
  $self->_add_to_state(\&label_axes, $in, $opt);


  barf 'Usage: label_axes( [$xtitle, $ytitle, $title], [$opt])' if $#$in > 2;

  my ($xtitle, $ytitle, $title)=@$in;

  $opt = {} if !defined($opt); # For safety.

  # Now the titles are set per plot so we use the general options to
  # parse the options (if they were set per window we would use
  # $self->{Options}
  my ($o, $u_opt) = $self->_parse_options($self->{PlotOptions}, $opt);

  # Added 25/8/01 JB to check whether label_axes is called before env..
  # This is not fool-proof though... And it will give a warning if the
  # user creates her/his env box outside of this package.
  warn "label_axes called before env - weird results might occur!\n" unless
    defined($self->{_env_options});

  $self->_save_status();
  $self->_standard_options_parser($u_opt);
  $o->{Title}=$title if defined($title);
  $o->{XTitle}=$xtitle if defined($xtitle);
  $o->{YTitle}=$ytitle if defined($ytitle);
  pglab($o->{XTitle}, $o->{YTitle}, $o->{Title});
  $self->_restore_status;


}


############ Exported functions #################

# Open/reopen the graphics device

################ Supports two new options::
## NewWindow and WindowName


sub CtoF77coords{		# convert a transform array from zero-offset to unit-offset images
  my $self = shift;
  my $tr = pdl(shift);		# Copy
  set($tr, 0, at($tr,0)-at($tr,1)-at($tr,2));
  set($tr, 3, at($tr,3)-at($tr,4)-at($tr,5));
  return $tr;
}



# set the envelope for plots and put auto-axes on hold

sub env {
  my $self=shift;

  # Inserted 28/2/01 - JB to avoid having to call release whenever
  # you want to move to the next panel after using env.
  $self->release() if $self->held();
  # The following is necessary to advance the panel if wanted...
  my ($in, $opt)=_extract_hash(@_);
  $opt = {} if !defined($opt);
  my $o = $self->{PlotOptions}->options($opt);

  #
  # Inserted 06/08/01 - JB to be able to determine whether the user has
  # specified a particular PlotPosition in which case we do _not_ call
  # _check_move_or_erase...
  #
  my $o2 = $self->{Options}->options($opt);
  if (!defined($o2->{PlotPosition}) || $o2->{PlotPosition} eq 'Default') {
      $self->_check_move_or_erase($o->{Panel}, $o->{Erase});
  }

  barf 'Usage: env ( $xmin, $xmax, $ymin, $ymax, [$just, $axis, $opt] )'
    if ($#_==-1 && !defined($self->{_env_options}) && !defined($PREVIOUS_ENV)) || 
      ($#_>=0 && $#_<=2) || $#_>6;
  my(@args);

  # Set the args. The logic here was extended 13/8 by JB to use the
  # previous setting of the plot env variables regardless of device
  # if the current device does not have a setting for env etc.
  if ($#_ == -1) {
    if (defined(@{$self->{_env_options}})) {
      @args = @{$self->{_env_options}};
    } elsif (defined($PREVIOUS_ENV)) {
      @args = @{$PREVIOUS_ENV};
    } else {
      @args = ();
    }
  } else {
    @args = @_;
  }
  $self->initenv( @args );
  ## The adding to state has to take place here to avoid being cleared
  ## buy the call to initenv...
  $self->_add_to_state(\&env, $in, $opt);
  $self->hold();


  1;
}

# Plot a histogram with pgbin()

{
  my $bin_options = undef;


  sub bin {
    my $self = shift;
    if (!defined($bin_options)) {
      $bin_options = $self->{PlotOptions}->extend({Centre => 1});
      $bin_options->add_synonym({Center => 'Centre'});
    }
    my ($in, $opt)=_extract_hash(@_);
    $self->_add_to_state(\&bin, $in, $opt);


    barf 'Usage: bin ( [$x,] $data, [$options] )' if $#$in<0 || $#$in>2;
    my ($x, $data)=@$in;

    $self->_checkarg($x,1);

    my $n = nelem($x);
    if ($#$in==1) {
      $self->_checkarg($data,1); barf '$x and $y must be same size' if $n!=nelem($data);
    } else {
      $data = $x; $x = float(sequence($n));
    }

    # Parse options
    $opt={} unless defined($opt);
    my ($o, $u_opt) = $self->_parse_options($bin_options,$opt);

    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});
    unless ( $self->held() ) {
      my ($xmin, $xmax)=ref $o->{Xrange} eq 'ARRAY' ?
	   @{$o->{Xrange}} : minmax($x);
      my ($ymin, $ymax)=ref $o->{Yrange} eq 'ARRAY' ?
	   @{$o->{Yrange}} : minmax($data);
      if ($xmin == $xmax) { $xmin -= 0.5; $xmax += 0.5; }
      if ($ymin == $ymax) { $ymin -= 0.5; $ymax += 0.5; }
      $self->initenv( $xmin, $xmax, $ymin, $ymax, $opt );
    }
    $self->_save_status();

    my $centre = $o->{Centre};

    # For the standard parser we only want the options that the user set!
    # $bin_options->full_options(0);
    # my $u_opt = $bin_options->current();
    # $bin_options->full_options(1);

    # Let's also parse the options if any.
    $self->_standard_options_parser($u_opt);
    pgbin($n, $x->get_dataref, $data->get_dataref, $centre);
    $self->_restore_status();
    1;
  }
}




{
    use strict;
    my $transform_options = undef;

    sub transform {
	# Compute the transform array needed in contour and image plotting
	my $self = shift;

	if (!defined($transform_options)) {
	  $transform_options = 
	    $self->{PlotOptions}->extend({Angle => undef,
					  ImageDims => undef,
					  Pixinc => undef,
					  ImageCenter => undef,
					  RefPos => undef,
					  });
	  $transform_options->synonyms({
	      ImageDimensions => 'ImageDims',
	      ImageCentre => 'ImageCenter',
	      ReferencePosition => 'RefPos',
	      });
	}

	# parse the input
	my ($in, $opt)=_extract_hash(@_);
	my ($x_pix, $y_pix)= @$in; 

	# handle options
	$opt = {} if !defined($opt);
	my ($o, $u_opt) = $self->_parse_options($transform_options, $opt);
	$self->_standard_options_parser($u_opt);

	my ($angle, $x_pixinc, $y_pixinc, $xref_pix, $yref_pix, $xref_wrld, $yref_wrld);
	if (defined($o->{Angle})) {
	    $angle = $o->{Angle};
	}
	else {
	    $angle = 0;
	}

	if (defined($o->{Pixinc})) {
	    if (ref($o->{Pixinc}) eq 'ARRAY') {
		($x_pixinc, $y_pixinc) = @{$o->{Pixinc}};
	    }
	    else {
		$x_pixinc = $y_pixinc = $o->{Pixinc};
	    }
	}
	else {
	    $x_pixinc = $y_pixinc = 1;
	}

	if ( defined $o->{ImageDims} ) {
	    if ( ref($o->{ImageDims}) eq 'ARRAY' ) {
		($x_pix, $y_pix) = @{$o->{ImageDims}};
	    }
	    else {
		barf "Image dimensions must be given as an array reference!";
	    }
	} 
	
	# The user has to pass the dimensions of the image somehow, so this
	# is a good point to check whether he/she/it has done so.
	unless (defined($x_pix) && defined($y_pix)) {
	  barf "You must pass the image dimensions to the transform routine\n";
	}

	# The RefPos option gives more flexibility than
	# ImageCentre, since ImageCentre => [ a, b ] is the same 
	# as PosReference => [ [(nx-1)/2,(ny-1/2)], [a,b] ].
	# We use ImageCentre in preference to PosReference
	#
	if (defined $o->{ImageCenter}) {
	    print "transform() ignoring RefPos as seen ImageCentre\n"
		if defined $o->{RefPos} and $PDL::verbose;
	    my $ic = $o->{ImageCenter};
	    if (ref($ic) eq 'ARRAY') {
	        ($xref_wrld, $yref_wrld) = @{$ic};
	    }
	    else {
		$xref_wrld = $yref_wrld = $ic;
	    }
	    $xref_pix = ($x_pix - 1)/2;
	    $yref_pix = ($y_pix - 1)/2;
	}
	elsif ( defined $o->{RefPos} ) {
	    my $aref = $o->{RefPos};
	    barf "RefPos option must be sent an array reference.\n"
		unless ref($aref) eq 'ARRAY';
	    barf "RefPos must be a 2-element array reference\n"
		unless $#$aref == 1;
	    my $pixref  = $aref->[0];
	    my $wrldref = $aref->[1];
	    barf "Elements of RefPos must be 2-element array references\n"
		unless $#$pixref == 1 and $#$wrldref == 1;

	    ($xref_pix,  $yref_pix)  = @{$pixref};
	    ($xref_wrld, $yref_wrld) = @{$wrldref};
	}
	else {
	    $xref_wrld = $yref_wrld = 0;
	    $xref_pix = ($x_pix - 1)/2;
	    $yref_pix = ($y_pix - 1)/2;
	}

	# The elements of the transform piddle,
	# here labelled t0 to t5, relate to the
	# following maxtix equation:
	#   
	#   world = zp + matrix * pixel
	#
	# world  - the position of the point in the world, 
	#          ie plot, coordinate system
	# pixel  - the position of the point in pixel
	#          coordinates (bottom-left is 0,0 pixel)
	# zp     - (t0)
	#          (t3)
	# matrix - (t1 t2)
	#          (t4 t5)
	#
	my $ca = cos( $angle );
	my $sa = sin( $angle );
	my $t1 = $x_pixinc * $ca;
	my $t2 = $y_pixinc * $sa;
	my $t4 = - $x_pixinc * $sa;
	my $t5 = $y_pixinc * $ca;

	return pdl( 
		   $xref_wrld - $t1 * $xref_pix - $t2 * $yref_pix,
		   $t1, $t2,
		   $yref_wrld - $t4 * $xref_pix - $t5 * $yref_pix,
		   $t4, $t5
		   ); 
    }
}


# display a contour map of an image using pgconb()

{

  my $cont_options = undef;


  sub cont {
    my $self=shift;
    if (!defined($cont_options)) {
      $cont_options = $self->{PlotOptions}->extend({Contours => undef,
						    Follow => 0,
						    Labels => undef,
						    LabelColour => undef,
						    Missing => undef,
						    NContours => undef,
						    FillContours => undef});
      my $t = {
	       LabelColour => {
			       'White' => 0, 'Black' => 1, 'Red' => 2,
			       'Green' => 3, 'Blue' => 4, 'Cyan' => 5,
			       'Magenta' => 6, 'Yellow' => 7, 'Orange' => 8,
			       'DarkGray' => 14, 'DarkGrey' => 14,
			       'LightGray' => 15, 'LightGrey' => 15
			      }
	      };
      $cont_options->add_translation($t);
    }

    my ($in, $opt)=_extract_hash(@_);
    $self->_add_to_state(\&cont, $in, $opt);

    barf 'Usage: cont ( $image, %options )' if $#$in<0;

    # Parse input
    my ($image, $contours, $tr, $misval) = @$in;
    $self->_checkarg($image,2);
    my($nx,$ny) = $image->dims;
    my ($ncont)=9;		# The number of contours by default

    # First save the present status
    $self->_save_status();


    # Then parse the common options
    #
    # These will be all options.
    $opt = {} if !defined($opt);
    my ($o, $u_opt) = $self->_parse_options($cont_options, $opt);
    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});


    $self->_standard_options_parser($u_opt);
    my ($labelcolour);
    pgqci($labelcolour);	# Default let the labels have the chosen colour.


    my ($labels, $fillcontours, $angle);
    my $usepgcont = 0;

    $contours = $o->{Contours} if defined($o->{Contours});
    $ncont = $o->{NContours} if defined($o->{NContours});
    $misval = $o->{Missing} if defined($o->{Missing});
    $tr = $o->{Transform} if defined($o->{Transform});
    $labelcolour = $o->{LabelColour} if defined($o->{LabelColour});
    $labels = $o->{Labels} if defined($o->{Labels});
    $usepgcont = $o->{Follow} if defined($o->{Follow});
    $fillcontours = $o->{FillContours} if defined($o->{FillContours});

    if (defined($tr)) {
      $self->_checkarg($tr,1);
      barf '$transform incorrect' if nelem($tr)!=6;
    } else {
      $tr = float [0,1,0, 0,0,1];
    }

    $tr = $self->CtoF77coords($tr);

    if (!$self->held()) {
	# Scale the image correctly even with rotation by calculating the new 
	# corner points
	$self->initenv(($tr->slice("0:2")*pdl[
					      [1, 0, 0],
					      [1, 0, $nx],
					      [1, $nx, 0],
					      [1, $nx, $nx]])->sumover->minmax,
		       ($tr->slice("3:5")*pdl[
					      [1, 0, 0],
					      [1, 0, $ny],
					      [1, $ny, 0],
					      [1, $ny, $ny]])->sumover->minmax,
		       $opt);
    }

    if (!defined($contours)) {
      my($minim, $maxim)=minmax($image);
      $contours = xlinvals(zeroes($ncont), $minim, $maxim)
    }
    else {
	$ncont = nelem($contours);
    }

    $self->_checkarg($contours,1);

    print "Contouring $nx x $ny image from ",min($contours), " to ",
      max($contours), " in ",nelem($contours)," steps\n" if $PDL::verbose;

    if (defined($fillcontours)) {
      pgbbuf();
      if (ref $fillcontours ne PDL) {
	$fillcontours = zeroes($ncont - 1)->xlinvals(0,1)->dummy(0,3);
      } elsif ($fillcontours->getndims == 1) {
	$fillcontours = $fillcontours->dummy(0,3);
      } elsif (($fillcontours->getdim(1) != $ncont - 1) ||
	       ($fillcontours->getdim(0) != 3)) {
	barf "Argh, wrong dims in filled contours!";
      }
      my ($cr, $cg, $cb, $i);
      pgqcr(16, $cr, $cg, $cb); # Save color index 16
      # Loop over filled contours (perhaps should be done in PP for speed)
      # Do not shade negative and 0-levels
      for ($i = 0; $i < ($ncont - 1); $i++) {
	pgscr(16, list $fillcontours->slice(":,$i"));
	pgsci(16);
	pgconf($image->get_dataref, $nx, $ny,
               1, $nx, 1, $ny,
	       list($contours->slice($i.':'.($i + 1))), $tr->get_dataref);
      }
      pgscr(16, $cr, $cg, $cb); # Restore color index 16
      pgebuf();
    } elsif (defined($misval)) {
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
      my $intval=long($nx/3)+1;	#

      my $dum;
      pgqci($dum);
      $self->_set_colour($labelcolour);
      foreach $label (@{$labels}) {
	pgconl( $image->get_dataref, $nx,$ny,1,$nx,1,$ny,
		$contours->slice("($count)"),
		$tr->get_dataref, $label, $intval, $minint);
	$count++;
      }
      $self->_set_colour($dum);
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
      $self->redraw_axes unless $self->held(); # Redraw box
      $self->_restore_status();
    1;
  }
}

# Plot errors with pgerrb()

{

  my $errb_options = undef;

  sub errb {
    my $self = shift;
    if (!defined($errb_options)) {
      $errb_options = $self->{PlotOptions}->extend({Term => 1});
      $errb_options->add_synonym({Terminator => 'Term'});
    }
    my ($in, $opt)=_extract_hash(@_);
    $self->_add_to_state(\&bin, $in, $opt);

    $opt = {} if !defined($opt);
    barf <<'EOD' if $#$in<1 || $#$in==4 || $#$in>5;
 Usage: errb ( $y, $yerrors [, $options] )
	errb ( $x, $y, $yerrors [, $options] )
	errb ( $x, $y, $xerrors, $yerrors [, $options])
	errb ( $x, $y, $xloerr, $xhierr, $yloerr, $yhierr [, $options])
EOD

    my @t=@$in;
    my $i=0; my $n;
    for (@t) {
      $self->_checkarg($_, 1);
      $n = nelem($_) if $i++ == 0;
      barf "Args must have same size" if nelem($_)!=$n;
    }
    my $x = $#t==1 ? float(sequence($n)) : $t[0];
    my $y = $#t==1 ? $t[0] : $t[1];

    my ($o, $u_opt) = $self->_parse_options($errb_options, $opt);
    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});
    unless( $self->held() ) {
      # Allow for the error bars
      my ( $xmin, $xmax, $ymin, $ymax );

      if ($#t==1) {
	($xmin,$xmax) = $x->minmax($x);
	$ymin = min( $y - $t[1] ); $ymax = max( $y + $t[1] );
      } elsif ($#t==2) {
	($xmin, $xmax ) = $x->minmax($x);
	$ymin = min( $y - $t[2] ); $ymax = max( $y + $t[2] );
      } elsif ($#t==3) {
	$xmin = min( $x - $t[2] ); $xmax = max( $x + $t[2] );
	$ymin = min( $y - $t[3] ); $ymax = max( $y + $t[3] );
      } elsif ($#t==5) {
	$xmin = min( $x - $t[2] ); $xmax = max( $x + $t[3] );
	$ymin = min( $y - $t[4] ); $ymax = max( $y + $t[5] );
      }
      ($xmin,$xmax) = @{$o->{Xrange}} if ref $o->{Xrange} eq 'ARRAY';
      ($ymin,$ymax) = @{$o->{Yrange}} if ref $o->{Yrange} eq 'ARRAY';
      if ($xmin == $xmax) { $xmin -= 0.5; $xmax += 0.5; }
      if ($ymin == $ymax) { $ymin -= 0.5; $ymax += 0.5; }
      $self->initenv( $xmin, $xmax, $ymin, $ymax, $opt );
    }
    $self->_save_status();
    # Let us parse the options if any.

    my $term=$o->{Term};
    my $symbol;
    my $plot_points=0;		# We won't normally plot the points

    if (defined($u_opt->{Symbol})) {
      $symbol = $u_opt->{Symbol};
      $plot_points=1;
    }

    # Parse other standard options.
    $self->_standard_options_parser($u_opt);
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
       if (exists($opt->{SymbolSize})) { # Set symbol size (2001.10.22 kwi)
           pgsch($opt->{SymbolSize});
       }
      $symbol=long($symbol);
      my $ns=nelem($symbol);
      pgpnts($n, $x->get_dataref, $y->get_dataref, $symbol->get_dataref, $ns)
    }

    $self->_restore_status();
    1;
  }
}

#
# A "threaded" line - I cannot come up with a more elegant way of doing
# this without re-coding bits of thread_over but it might very well be
# that you may :)
#

my $line_options = undef;
sub tline {

  my $self = shift;
  my ($in, $opt)=_extract_hash(@_);
  $self->_add_to_state(\&tline, $in, $opt);
  $opt={} if !defined($opt);

  barf 'Usage tline ([$x], $y, [, $options])' if $#$in < 0 || $#$in > 2;
  my ($x, $y)=@$in;
  if (!defined($line_options)) {
    $line_options=$self->{PlotOptions}->extend({Missing => undef});
  }

  if ($#$in==0) {
    $y = $x; $x = $y->xvals();
  }

  # This is very very cludgy, but it was the best way I could find..
  my $o = _thread_options($y->getdim(1), $opt);
  # We need to keep track of the current status of hold or not since
  # the tline function automatically enforces a hold to allow for overplots.
  my $tmp_hold = $self->held();
  unless ( $self->held() ) {
    my ($o, $u_opt) = $self->_parse_options($line_options,$opt);
    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});
    
    # use Data::Dumper;
    # print Dumper $o;
    # print Dumper $u_opt;

    my ($ymin, $ymax, $xmin, $xmax);
    # Make sure the missing value is used as the min or max value
    if (defined $o->{Missing} ) {
      ($ymin, $ymax)=ref $o->{Yrange} eq 'ARRAY' ? 
	@{$o->{Yrange}} : minmax($y->where($y != $o->{Missing}));
      ($xmin, $xmax)=ref $o->{Xrange} eq 'ARRAY' ?
	@{$o->{Xrange}} : minmax($x->where($x != $o->{Missing}));
    } else {
      ($ymin, $ymax)=ref $o->{Yrange} eq 'ARRAY' ? @{$o->{Yrange}} :
	minmax($y);
      ($xmin, $xmax)=ref $o->{Xrange} eq 'ARRAY' ? @{$o->{Xrange}} :
	minmax($x);
    }
    if ($xmin == $xmax) { $xmin -= 0.5; $xmax += 0.5; }
    if ($ymin == $ymax) { $ymin -= 0.5; $ymax += 0.5; }
    # use Data::Dumper;
    # print "tline options: ", Dumper($opt), "\n";
    $self->initenv( $xmin, $xmax, $ymin, $ymax, $opt);
    $self->hold; # we hold for the duration of the threaded plot
  }
  _tline($x, $y, PDL->sequence($y->getdim(1)), $self, $o);
  $self->release unless $tmp_hold;

}


PDL::thread_define('_tline(a(n);b(n);ind()), NOtherPars => 2',
  PDL::over {
    my ($x, $y, $ind, $self, $opt)=@_;
    # use Data::Dumper;
    # print Dumper $opt->[$ind->at(0)];
    $self->line($x, $y,$opt->[$ind->at(0)] || {}); #
});


#
# A "threaded" point - I cannot come up with a more elegant way of doing
# this without re-coding bits of thread_over but it might very well be
# that you may :)
#

my $points_options = undef;
sub tpoints {

  my $self = shift;
  my ($in, $opt)=_extract_hash(@_);
  $self->_add_to_state(\&tpoints, $in, $opt);
  $opt={} if !defined($opt);

  barf 'Usage tpoints ([$x], $y, [, $options])' if $#$in < 0 || $#$in > 2;
  my ($x, $y)=@$in;


  if ($#$in==0) {
    $y = $x; $x = $y->xvals();
  }

  # This is very very cludgy, but it was the best way I could find..
  my $o = _thread_options($y->getdim(1), $opt);
  # We need to keep track of the current status of hold or not since
  # the tline function automatically enforces a hold to allow for overplots.
  my $tmp_hold = $self->held();
  unless ( $self->held() ) {
    if (!defined($points_options)) {
      $points_options = $self->{PlotOptions}->extend({PlotLine => 0});
    }
    my ($o, $u_opt) = $self->_parse_options($points_options,$opt);
    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});

    # use Data::Dumper;
    # print Dumper $o;
    # print Dumper $u_opt;

    my ($ymin, $ymax, $xmin, $xmax);
    # Make sure the missing value is used as the min or max value
    if (defined $o->{Missing} ) {
      ($ymin, $ymax)=ref $o->{Yrange} eq 'ARRAY' ? 
	@{$o->{Yrange}} : minmax($y->where($y != $o->{Missing}));
      ($xmin, $xmax)=ref $o->{Xrange} eq 'ARRAY' ?
	@{$o->{Xrange}} : minmax($x->where($x != $o->{Missing}));
    } else {
      ($ymin, $ymax)=ref $o->{Yrange} eq 'ARRAY' ? @{$o->{Yrange}} :
	minmax($y);
      ($xmin, $xmax)=ref $o->{Xrange} eq 'ARRAY' ? @{$o->{Xrange}} :
	minmax($x);
    }
    if ($xmin == $xmax) { $xmin -= 0.5; $xmax += 0.5; }
    if ($ymin == $ymax) { $ymin -= 0.5; $ymax += 0.5; }
    $self->initenv( $xmin, $xmax, $ymin, $ymax, $opt);
    $self->hold; # we hold for the duration of the threaded plot
  }
  _tpoints($x, $y, PDL->sequence($y->getdim(1)), $self, $o);
  $self->release unless $tmp_hold;

}


PDL::thread_define('_tpoints(a(n);b(n);ind()), NOtherPars => 2',
  PDL::over {
    my ($x, $y, $ind, $self, $opt)=@_;
    $self->points($x, $y, $opt->[$ind->at(0)] || {});
});



# Plot a line with pgline()

{
  my $line_options = undef;


  sub line {
    my $self = shift;
    if (!defined($line_options)) {
      $line_options=$self->{PlotOptions}->extend({Missing => undef});
    }
    my ($in, $opt)=_extract_hash(@_);

    barf 'Usage: line ( [$x,] $y, [$options] )' if $#$in<0 || $#$in>2;
    my($x,$y) = @$in;
    $self->_checkarg($x,1);
    my $n = nelem($x);

    my ($is_1D, $is_2D);
    if ($#$in==1) {
      $is_1D = $self->_checkarg($y,1,undef,1);
      if (!$is_1D) {
	$is_2D = $self->_checkarg($y,2,undef,1);
	barf '$y must be 1D (or 2D for threading!)'."\n" if !$is_2D;
	
	# Ok, let us use the threading possibility.
	$self->tline(@$in, $opt);
	
	return;
      } else {
	barf '$x and $y must be same size' if $n!=nelem($y);
      }
    } else {
      $y = $x; $x = float(sequence($n));
    }

    # Let us parse the options if any.
    $opt = {} if !defined($opt);
    my ($o, $u_opt) = $self->_parse_options($line_options, $opt);
    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});

    unless ( $self->held() ) {

      # Make sure the missing value is used as the min or max value
      my ($ymin, $ymax, $xmin, $xmax);
      if (defined $o->{Missing} ) {
	($ymin, $ymax)=ref $o->{Yrange} eq 'ARRAY' ? 
	  @{$o->{Yrange}} : minmax($y->where($y != $o->{Missing}));
	($xmin, $xmax)=ref $o->{Xrange} eq 'ARRAY' ?
	   @{$o->{Xrange}} : minmax($x->where($x != $o->{Missing}));
      } else {
	($ymin, $ymax)=ref $o->{Yrange} eq 'ARRAY' ? @{$o->{Yrange}} :
	  minmax($y);
	($xmin, $xmax)=ref $o->{Xrange} eq 'ARRAY' ? @{$o->{Xrange}} :
	  minmax($x);
      }
      if ($xmin == $xmax) { $xmin -= 0.5; $xmax += 0.5; }
      if ($ymin == $ymax) { $ymin -= 0.5; $ymax += 0.5; }
      $self->initenv( $xmin, $xmax, $ymin, $ymax, $opt);
    }
    $self->_save_status();
    $self->_standard_options_parser($u_opt);

    ($x,$y) = $self->checklog($x,$y) if $self->autolog;

    # If there is a missing value specified, use pggapline
    # to break the line around missing values.
    if (defined $o->{Missing}) {
      pggapline ($n, $o->{Missing}, $x->get_dataref, $y->get_dataref);
    } else {
      pgline($n, $x->get_dataref, $y->get_dataref);
    }
    $self->_restore_status();
    $self->_add_to_state(\&line, $in, $opt);

    1;
  }
}
# Plot points with pgpnts()



sub arrow {

  my $self = shift;

  my ($in, $opt)=_extract_hash(@_);
  $opt = {} if !defined($opt);

  barf 'Usage: arrow($x1, $y1, $x2, $y2 [, $options])' if $#$in != 3;

  my ($x1, $y1, $x2, $y2)=@$in;

  my ($o, $u_opt) = $self->_parse_options($self->{PlotOptions}, $opt);
  $self->_check_move_or_erase($o->{Panel}, $o->{Erase});
  unless ($self->held()) {
    $self->initenv($x1, $x2, $y1, $y2, $opt);
  }

  $self->_save_status();
  $self->_standard_options_parser($u_opt);
  pgarro($x1, $y1, $x2, $y2);
  $self->_restore_status();
  $self->_add_to_state(\&arrow, $in, $opt);

}



{
  my $points_options = undef;

  sub points {

    my $self = shift;
    if (!defined($points_options)) {
      $points_options = $self->{PlotOptions}->extend({PlotLine => 0});
    }
    my ($in, $opt)=_extract_hash(@_);
    barf 'Usage: points ( [$x,] $y, $sym, [$options] )' if $#$in<0 || $#$in>2;
    my ($x, $y, $sym)=@$in;
    $self->_checkarg($x,1);
    my $n=nelem($x);

    my ($is_1D, $is_2D);
    if ($#$in>=1) {
      $is_1D = $self->_checkarg($y,1,undef,1);
      if (!$is_1D) {
	$is_2D = $self->_checkarg($y,2,undef,1);
	barf '$y must be 1D (or 2D for threading!)'."\n" if !$is_2D;
	
	# Ok, let us use the threading possibility.
	$self->tpoints(@$in, $opt);
	return;

      } else {
	barf '$x and $y must be same size' if $n!=nelem($y);
      }
    } else {
      $y = $x; $x = float(sequence($n));
    }

    # Let us parse the options if any.
    $opt = {} if !defined($opt);
    my ($o, $u_opt) = $self->_parse_options($points_options, $opt);
    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});

    #
    # Save some time for large datasets.
    #
    unless ( $self->held() ) {
      my ($xmin, $xmax)=ref $o->{Xrange} eq 'ARRAY' ?
	   @{$o->{Xrange}} : minmax($x);
      my ($ymin, $ymax)=ref $o->{Yrange} eq 'ARRAY' ?
	   @{$o->{Yrange}} : minmax($y);
      if ($xmin == $xmax) { $xmin -= 0.5; $xmax += 0.5; }
      if ($ymin == $ymax) { $ymin -= 0.5; $ymax += 0.5; }
      $self->initenv( $xmin, $xmax, $ymin, $ymax, $opt );
    }
    $self->_save_status();
    $self->_standard_options_parser($u_opt);

    if (exists($opt->{SymbolSize})) { # Set symbol size (2001.10.22 kwi)
       pgsch($opt->{SymbolSize});
    }

    if (exists($opt->{ColorValues})) {
      my $sym ||= $o->{Symbol} || 0;
      my $z   = $opt->{ColorValues};
      $self->_checkarg($z,1);    # make sure this is a float PDL
      pgcolorpnts($n, $x->get_dataref, $y->get_dataref, $z->get_dataref, $sym);
    } else {

      # Set symbol if specified in the options hash.
      ## $sym ||= $o->{Symbol};
      $sym = $o->{Symbol} unless defined $sym;
      
      $self->_checkarg($sym,1); my $ns = nelem($sym); $sym = long($sym);
      
      pgpnts($n, $x->get_dataref, $y->get_dataref, $sym->get_dataref, $ns);

    }

    #
    # Sometimes you would like to plot a line through the points straight
    # away.
    pgline($n, $x->get_dataref, $y->get_dataref) if $o->{PlotLine}>0;

    $self->_restore_status();
    $self->_add_to_state(\&points, $in, $opt);
    1;
  }
}

# add a "wedge" to the image
# - since this can be called from imag() as well as by the user,
#   we make all parameters defined as options
#
#   Wedge => { 
#              Side         => one of B L T R,
#              Displacement => default = 2,
#              Width        => default = 3,
#              Fg/Bg        => default, values used by imag()
#              Label        => default ''
#            }
#
# - uses horrible _store()/_retrieve() routines, which need to 
#   know (but don't) about changing window focus/erasing/...
#
# Want to be able to specify a title (optional)
# - also, by default want to use the axes colour/size, but want to be able to 
#   over-ride this
#
# initial version by Doug Burke (11/20/00 ish)

{
    my $wedge_options = undef;

    sub draw_wedge {
	my $self = shift;
	if ( !defined($wegde_options) ) {
	    $wedge_options = 
		$self->{PlotOptions}->extend({
		    Side => 'R',
		    Displacement => 2.0,
		    Width => 3.0,
		    Label => '',
		    ForeGround => undef,
		    BackGround => undef,
		});
	    $wedge_options->synonyms({ Fg => 'ForeGround', Bg => 'BackGround' });
	}

	my ( $in, $opt ) = _extract_hash(@_);
	$opt = {} unless defined($opt);
	barf 'Usage: $win->draw_wedge( [$options] )'
	    unless $#$in == -1;

	# check imag has been called, and get information
	# - this is HORRIBLE
	my $iref = $self->_retrieve( 'imag' );
	barf 'draw_wedge() can only be called after a call to imag()'
	    unless defined $iref;

	# Let us parse the options if any.
	# - not convinced I know what I'm doing
	my $o;
	if ( defined $opt->{Wedge} ) {
	    $o = $wedge_options->options( $opt->{Wedge} );
	} else {
	    $o = $wedge_options->current();
	}
	$o->{ForeGround} = $$iref{max} unless defined( $o->{ForeGround} );
	$o->{BackGround} = $$iref{min} unless defined( $o->{BackGround} );

	# do we really want this?
	# - (03/15/01 DJB) removed since I assume that draw_wedge()
	#   will be called before the focus has been changed.
	#   Not ideal, but I don't think the current implementation will
	#   handle such cases anyway (ie getting the correct min/max values
	#   for the wedge).
#	$self->_check_move_or_erase($o->{Panel}, $o->{Erase});

	# get the options used to draw the axes
	# note: use the window object, not the options hash, though we
	# probably could/should do that
	my $wo = $self->{_env_options}[4];

	# Save current status
	$self->_save_status();

	# we use the colour/size of the axes here
	$self->_set_colour($wo->{AxisColour});
	pgsch($wo->{CharSize});

	# draw the wedge
	my $side = $o->{Side} . $$iref{routine};
	pgwedg( $side, $o->{Displacement}, $o->{Width}, $o->{BackGround}, $o->{ForeGround}, $o->{Label} );

	# restore character colour & size before returning
	$self->_restore_status();
	$self->_add_to_state(\&draw_wedge, $in, $opt);

	1;
    } # sub: draw_wedge()
}

# display an image using pgimag()/pggray() as appropriate


{
  # The ITF is in the general options - since other functions might want
  # it too.
  #
  # There is some repetetiveness in the code, but this is to allow the
  # user to set global defaults when opening a new window.
  #

  my $im_options = undef;


  sub imag1 {
    my $self = shift;
    my ($in,$opt)=_extract_hash(@_);

    if (!defined($im_options)) {
      $im_options = $self->{PlotOptions}->extend({
						  PIX => undef,
						  Min => undef,
						  Max => undef,
						  Scale => undef,
						  Pitch => undef,
						  Unit => undef,
						  DrawWedge => 0,
						  Wedge => undef,
						 });
    }

    # Let us parse the options if any.
    $opt = {} if !defined($opt);
    my ($o, $u_opt) = $self->_parse_options($im_options, $opt);

    barf 'Usage: im ( $image, [$min, $max, $transform] )' if $#$in<0 || $#$in>3;
    $u_opt->{'PIX'}=1 unless defined $u_opt->{'PIX'};
    # Note that passing $u_opt is ok here since the two routines accept the
    # same options!
    $self->imag (@$in,$u_opt);
    # This is not added to the state, because the imag command does that
  }

  sub imag {

    my $self = shift;
    if (!defined($im_options)) {
      $im_options = $self->{PlotOptions}->extend({
						  PIX => undef,
						  Min => undef,
						  Max => undef,
						  Scale => undef,
						  Pitch => undef,
						  Unit => undef,
						  DrawWedge => 0,
						  Wedge => undef,
						 });
    }

    my ($in, $opt)=_extract_hash(@_);
    # Let us parse the options if any.
    $opt = {} if !defined($opt);
    my ($o, $u_opt) = $self->_parse_options($im_options, $opt);


    barf 'Usage: imag ( $image,  [$min, $max, $transform] )' if $#$in<0 || $#$in>3;
    my ($image,$min,$max,$tr) = @$in;
    $self->_checkarg($image,2);
    my($nx,$ny) = $image->dims;

    my($pix,$pitch,$unit,$scale);
    my $itf = 0;

    $tr = $u_opt->{Transform} if exists($u_opt->{Transform});
    $min = $u_opt->{Min} if exists($u_opt->{Min});
    $max = $u_opt->{Max} if exists($u_opt->{Max});
    $itf = $u_opt->{ITF} if exists($u_opt->{ITF});

    # Check on ITF value hardcoded in.
    barf ( "illegal ITF value `$val'") if $itf > 2 || $itf < 0;

    $min = min($image) unless defined $min;
    $max = max($image) unless defined $max;

    if (defined($tr)) {
      $self->_checkarg($tr,1);
      barf '$transform incorrect' if nelem($tr)!=6;
    } else {
      $tr = float [0,1,0, 0,0,1];
    }
    $tr = $self->CtoF77coords($tr);

    ##############################
    # Set up coordinate transformation in the output window.

    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});
    if (!$self->held()) {
      #########
      # Parse out scaling options - this is pretty long because
      # the defaults for each value change based on the others.
      # (e.g. specifying "SCALE" and no unit gives pixels; but
      # specifying "PITCH" and no unit gives dpi).
      #
      local $_;
      my ($pix,$pitch,$unit);

      if ($u_opt->{'Scale'}) {
	($pix,$pitch,$unit)=(1,1.0/$u_opt->{'Scale'},3);
      }
      if ($u_opt->{'Pitch'}) {
	($pix,$pitch,$unit) = (1,$u_opt->{'Pitch'},1);
      }
      if (defined ($_ = $u_opt->{'Unit'})) {
	undef $unit;
	if (m/^d/ && $_ <= 4) {	# Numeric data type spec
	  $unit = $_;
	} else { 
	  my @c = ('n','i','m','p');
	  my ($i, $c);
	  for ($i=0;defined($c=shift(@c));$i++) {
	    m/^$c/ || next;
	    $unit=$i; 
	    last;
	  }
	}
	barf ('If you specify UNIT, it has to be one of (normal,inches,millimeters,pixels)!') unless defined($unit);
      }

      $pix = $u_opt->{'PIX'} if defined $u_opt->{'PIX'};

      ##############################
      ## Do the initial scaling setup.  If $pix is defined, then
      ## handle the scaling locally, else use initenv.
      ## [ The PIX, SCALE, and UNIT options could in principle be fed to
      ## initenv instead of doing it here... ]

      if (defined $pix) {
	my ( $x0,$x1,$y0,$y1 );

	if (!defined($pitch)) {
	  ## Set scaling parameters automagically.

	  pgqvsz(1,$x0,$x1,$y0,$y1);
	  print "x0=$x0, x1=$x1, y0=$y0, y1=$y1\n" if $PDL::verbose;
	  ($unit,$pitch) = (1, max(pdl( $pix * $nx / ($x1-$x0)  , 
					$ny / ($y1-$y0)   )));
	  print "imag: defined pitch & unit automagically\n" if $PDL::verbose;
	}

	print "imag: unit='$unit', pitch='$pitch'\n" if $PDL::verbose;


	my($col); pgqci($col);
	my $wo = $self->{Options}->options($opt);
	print "Axis colour set to $$wo{AxisColour}\n";
	if ($self->{NX}*$self->{NY} > 1) {
	  $self->clear_state();
	  pgeras();
	} else {
	  $self->clear_state();
	  pgpage();
	}
	$self->_set_colour($wo->{AxisColour});
	pgvstd;			## Change this to use the margins for display!

	## Set the window to the correct number of pixels for the
	## viewport size and the specified $pitch.
	pgqvsz($unit,$x0,$x1,$y0,$y1);
	pgswin(0,($x1-$x0)*$pitch/$pix,0,($y1-$y0)*$pitch);

	$self->_set_env_options(0, ($x1-$x0)*$pitch/$pix, 0, 
				 ($y1-$y0)*$pitch, 
				 $self->{Options}->options($opt));
#	$self->{_env_set}[$self->{CurrentPanel}]=1;
	$self->_set_colour($col);
      } else {
	# Scale the image correctly even with rotation by calculating the new
	# corner points
	# - we respect the sense of the input axis by swapping the min/max
	#   values if the pixel size is negative (DJB 01/10/15)
        #
        my @xvals = ($tr->slice("0:2")*pdl[
					   [1, 0.5, 0.5],
					   [1, 0.5, $nx+0.5],
					   [1, $nx+0.5, 0.5],
					   [1, $nx+0.5, $nx+0.5]])->sumover->minmax;
        my @yvals = ($tr->slice("3:5")*pdl[
					   [1, 0.5, 0.5],
					   [1, 0.5, $ny+0.5],
					   [1, $ny+0.5, 0.5],
					   [1, $ny+0.5, $ny+0.5]])->sumover->minmax;
        if ( $tr->at(1) < 0 ) { @xvals = ( $xvals[1], $xvals[0] ); }
        if ( $tr->at(5) < 0 ) { @yvals = ( $yvals[1], $yvals[0] ); }

	$self->initenv( $xvals[0], $xvals[1], $yvals[0], $yvals[1], $opt);
      }
    }				# if ! hold

    print "Displaying $nx x $ny image from $min to $max ...\n" if $PDL::verbose;

    pgsitf( $itf );
    my ($i1, $i2);
    pgqcir($i1, $i2);		# Colour range - if too small use pggray dither algorithm

    # Why is the PS output disabled in the following if statement??
    if ($i2-$i1<16 || $self->{Device} =~ /^v?ps$/i) {
      pggray( $image->get_dataref, $nx,$ny,1,$nx,1,$ny, $min, $max, $tr->get_dataref);
      $self->_store( imag => { routine => "G", min => $min, max => $max } );
    } else {
      $self->ctab('Grey') unless $self->_ctab_set(); # Start with grey
      pgimag( $image->get_dataref, $nx,$ny,1,$nx,1,$ny, $min, $max, $tr->get_dataref);
      $self->_store( imag => { routine => "I", min => $min, max => $max } );
    }
    $self->redraw_axes unless $self->held(); # Redraw box

    # draw the wedge, if requested
    if ( $u_opt->{DrawWedge} ) {
	my $hflag = $self->held();
	$self->hold();
	$self->draw_wedge( $u_opt );
	$self->release() unless $hflag;
    }

    $self->_add_to_state(\&imag, $in, $opt);
    1;

  } # sub: imag()

}

# Load a colour table using pgctab()

#
# Modified 7/4/02 JB - having the last colour table as a variable in here
# did not work. So it is now moved to the $self hash.
{
  # This routine doesn't really have any options at the moment, but
  # it uses the following standard variables
  my %CTAB = ();
  $CTAB{Grey}    = [ pdl([0,1],[0,1],[0,1],[0,1]) ];
  $CTAB{Igrey}   = [ pdl([0,1],[1,0],[1,0],[1,0]) ];
  $CTAB{Fire}    = [ pdl([0,0.33,0.66,1],[0,1,1,1],[0,0,1,1],[0,0,0,1]) ];
  $CTAB{Gray}    = $CTAB{Grey};	# Alias
  $CTAB{Igray}   = $CTAB{Igrey}; # Alias

  # It would be easy to add options though..
  sub _ctab_set {
    my $self = shift;
    return defined($self->{CTAB});
  }

  sub ctab {
    my $self = shift;
    my ($in, $opt)=_extract_hash(@_);


    # No arguments -- print list of tables
    if (scalar(@$in) == 0) {
      print "Available 'standard' color tables are:\n",join(",",sort keys %CTAB)
	,"\n";
      return;
    }
    # No arguments -- print list of tables
    if (scalar(@$in) == 0) {
      print "Available 'standard' color tables are:\n",join(",",sort keys %CTAB)
	,"\n";
      return;
    }

    # First indirect arg list through %CTAB
    my(@arg) = @$in;

    my($ctab, $levels, $red, $green, $blue, $contrast, $brightness, @t, $n);

    if ($#arg>=0 && !ref($arg[0])) {       # First arg is a name not an object
      # if first arg is undef or empty string, means use last CTAB.
      # preload with Grey if no prior CTAB
      $arg[0] = 'Grey' unless $arg[0] || $self->{CTAB};

      # now check if we're using the last one specified
      if ( ! $arg[0] ) {
	shift @arg;
	unshift @arg, @{$self-{CTAB}->{ctab}};
	$brightness = $self->{CTAB}->{brightness};
	$contrast = $self->{CTAB}->{contrast};
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
    } else {
      ($levels, $red, $green, $blue, $contrast, $brightness) = @arg;
      $self->_checkarg($levels,1);  $n = nelem($levels);
      for ($red,$green,$blue) {
	$self->_checkarg($_,1); barf 'Arguments must have same size' unless nelem($_) == $n;
      }
    }

    # Now load it

    $contrast   = 1   unless defined $contrast;
    $brightness = 0.5 unless defined $brightness;

    pgctab( $levels->get_dataref, $red->get_dataref, $green->get_dataref,
	    $blue->get_dataref, $n, $contrast, $brightness );
    $self->{CTAB} = { ctab => [ $levels, $red, $green, $blue ],
	      brightness => $brightness,
	      contrast => $contrast
	    };			# Loaded
    $self->_add_to_state(\&ctab, $in, $opt);

    1;
  }

  # get information on last CTAB load
  sub ctab_info {
    my $self = shift;
    my ($in, $opt)=_extract_hash(@_);
    barf 'Usage: ctab_info( )' if $#$in> -1;

    return () unless $self->{CTAB};
    return @{$self->{CTAB}->{ctab}}, $self-{CTAB}->{contrast},
      $self->{CTAB}->{brightness};
  }
}

# display an image using pghi2d()

{

  my $hi2d_options = undef;

  sub hi2d {
    my $self = shift;
    if (!defined($hi2d_options)) {
      $hi2d_options = $self->{PlotOptions}->extend({
					       Ioff => undef,
					       Bias => undef
					      });
    }
    my ($in, $opt)=_extract_hash(@_);
    $opt = {} if !defined($opt);

    barf 'Usage: hi2d ( $image, [$x, $ioff, $bias] [, $options] )' if $#$in<0 || $#$in>3;
    my ($image, $x, $ioff, $bias) = @$in;
    $self->_checkarg($image,2);
    my($nx,$ny) = $image->dims;

    # Let us parse the options if any.
    my ($o, $u_opt) = $self->_parse_options($hi2d_options, $opt);
    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});

    if (defined($x)) {
      $self->_checkarg($x,1);
      barf '$x incorrect' if nelem($x)!=$nx;
    } else {
      $x = float(sequence($nx));
    }

    # Parse for options input instead of calling convention
    $ioff = $o->{Ioff} || 1 unless defined($ioff);
    $bias = $o->{Bias} if defined($o->{Bias});

    $bias = 5*max($image)/$ny unless defined $bias;
    my $work = float(zeroes($nx));

    $self->_save_status();
    $self->_standard_options_parser($u_opt);

    $self->initenv( 0 ,2*($nx-1), 0, 10*max($image), $opt ) unless $self->held();

    pghi2d($image->get_dataref, $nx, $ny, 1,$nx,1,$ny, $x->get_dataref, $ioff,
	   $bias, 1, $work->get_dataref);

    $self->_restore_status();
    $self->_add_to_state(\&hi2d, $in, $opt);
    1;
  }
}

# Plot a polygon with pgpoly()

sub poly {
  my $self = shift;
  my ($in, $opt)=_extract_hash(@_);
  barf 'Usage: poly ( $x, $y [, $options] )' if $#$in<0 || $#$in>2;
  my($x,$y) = @$in;
  $self->_checkarg($x,1);
  $self->_checkarg($y,1);
  my ($o, $u_opt) = $self->_parse_options($self->{PlotOptions}, $opt);
  $self->_check_move_or_erase($o->{Panel}, $o->{Erase});

  unless ( $self->held() ) {
      my ($xmin, $xmax)=ref $o->{Xrange} eq 'ARRAY' ?
	   @{$o->{Xrange}} : minmax($x);
      my ($ymin, $ymax)=ref $o->{Yrange} eq 'ARRAY' ?
	   @{$o->{Yrange}} : minmax($y);
      if ($xmin == $xmax) { $xmin -= 0.5; $xmax += 0.5; }
      if ($ymin == $ymax) { $ymin -= 0.5; $ymax += 0.5; }
    $self->initenv( $xmin, $xmax, $ymin, $ymax, $opt );
  }

  $self->_save_status();
  $self->_standard_options_parser($u_opt);
  my $n = nelem($x);
  pgpoly($n, $x->get_dataref, $y->get_dataref);
  $self->_restore_status();
  $self->_add_to_state(\&poly, $in, $opt);
  1;
}

# Plot a circle using pgcirc




{
  my $circle_options = undef;

  sub circle {
    my $self = shift;
    if (!defined($circle_options)) {
      $circle_options = $self->{PlotOptions}->extend({Radius => undef,
						 XCenter => undef,
						 YCenter => undef});
    }
    my ($in, $opt)=_extract_hash(@_);
    $opt = {} if !defined($opt);
    my ($x, $y, $radius)=@$in;

    my ($o, $u_opt) = $self->_parse_options($circle_options, $opt);
    $o->{XCenter}=$x if defined($x);
    $o->{YCenter}=$y if defined($y);
    $o->{Radius} = $radius if defined($radius);

    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});

    $self->_save_status();
    $self->_standard_options_parser($u_opt);
    pgcirc($o->{XCenter}, $o->{YCenter}, $o->{Radius});
    $self->_restore_status();
    $self->_add_to_state(\&circle, $in, $opt);
  }
}

# Plot an ellipse using poly.

{
  my $ell_options = undef;

  sub ellipse {
    my $self = shift;
    if (!defined($ell_options)) {
      $ell_options = $self->{PlotOptions}->extend({
					      MajorAxis=>undef,
					      MinorAxis=>undef,
					      Theta => 0.0,
					      XCenter => undef,
					      YCenter => undef,
					      NPoints => 100
						  });
      $ell_options->synonyms({Angle => 'Theta'});
    }
    my ($in, $opt)=_extract_hash(@_);
    $opt = {} unless defined $opt;
    my ($x, $y, $a, $b, $theta)=@$in;

    my $o = $ell_options->options($opt);
    $o->{XCenter}=$x if defined($x);
    $o->{YCenter}=$y if defined($y);
    $o->{MajorAxis} = $a if defined($a);
    $o->{MinorAxis} = $b if defined($b);
    $o->{Theta}=$theta if defined($theta);

    if (!defined($o->{MajorAxis}) || !defined($o->{MinorAxis}) || !defined($o->{XCenter})
       || !defined($o->{YCenter})) {
      barf "The major and minor axis and the center coordinates must be given!";
    }

    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});

    my $t = 2*$PI*sequence($o->{NPoints})/($o->{NPoints}-1);
    my ($xtmp, $ytmp) = ($o->{MajorAxis}*cos($t), $o->{MinorAxis}*sin($t));

    # Rotate the ellipse and shift it.
    my ($costheta, $sintheta)=(cos($o->{Theta}), sin($o->{Theta}));
    $x = $o->{XCenter}+$xtmp*$costheta-$ytmp*$sintheta;
    $y = $o->{YCenter}+$xtmp*$sintheta+$ytmp*$costheta;

    $self->_add_to_state(\&ellipse, $in, $opt);
    # Now turn off recording so we don't get this one twice..
    $self->turn_off_recording();
    $self->poly($x, $y, $opt);
    $self->turn_on_recording();

  }

}


{
  my $rect_opt = undef;
  sub rectangle {
    my $self = shift;
    my $usage='Usage: rectangle($xcenter, $ycenter, $xside, $yside, [, $angle, $opt])';
    if (!defined($rect_opt)) {
      # No need to use $self->{PlotOptions} here since we
      # pass control to poly below.
      $rect_opt = PDL::Options->new({XCenter => undef, YCenter => undef,
				     XSide => undef, YSide => undef,
				     Angle => 0, Side => undef});
      $rect_opt->synonyms({XCentre => 'XCenter', YCentre => 'YCenter',
			  Theta => 'Angle'});
      $rect_opt->warnonmissing(0);
    }
    my ($in, $opt)=_extract_hash(@_);
    $opt={} if !defined($opt);
    my ($xc, $yc, $xside, $yside, $angle)=@$in;
    my $o=$rect_opt->options($opt);

    $o->{XCenter}=$xc if defined($xc);
    $o->{YCenter}=$yc if defined($yc);
    $o->{XSide}=$xside if defined($xside);
    $o->{YSide}=$yside if defined($yside);
    $o->{Angle}=$angle if defined($angle);

    ##
    # Now do some error checking and checks for squares.
    ##
    if (defined($o->{XSide}) || defined($o->{YSide})) {
      # At least one of these are set - let us ignore Side.
      $o->{XSide}=$o->{YSide} if !defined($o->{XSide});
      $o->{YSide}=$o->{XSide} if !defined($o->{YSide});
    } elsif (defined($o->{Side})) {
      $o->{XSide}=$o->{Side};
      $o->{YSide}=$o->{Side};
    } else {
      print "$usage\n";
      barf 'The sides of the rectangle must be specified!';
    }

    unless (defined($o->{XCenter}) && defined($o->{YCenter})) {
      print "$usage\n";
      barf 'The center of the rectangle must be specified!';
    }

    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});

    # Ok if we got this far it is about time to do something useful,
    # namely construct the piddle that contains the sides of the rectangle.

    # We make it first parallell to the coordinate axes around origo
    # and rotate it subsequently (ala the ellipse routine above).
    my ($dx, $dy)=(0.5*$o->{XSide}, 0.5*$o->{YSide});
    my $xtmp = pdl(-$dx, $dx, $dx, -$dx, -$dx);
    my $ytmp = pdl(-$dy, -$dy, $dy, $dy, -$dy);

    my ($costheta, $sintheta)=(cos($o->{Angle}), sin($o->{Angle}));
    my $x = $o->{XCenter}+$xtmp*$costheta-$ytmp*$sintheta;
    my $y = $o->{YCenter}+$xtmp*$sintheta+$ytmp*$costheta;

    $self->_add_to_state(\&rectangle, $in, $opt);
    # Turn off recording temporarily.
    $self->turn_off_recording();
    $self->poly($x, $y, $opt);
    $self->turn_on_recording();

  }
}


# display a vector map of 2 images using pgvect()

{
  my $vect_options = undef;

  sub vect {
    my $self = shift;
    if (!defined($vect_options)) {
      $vect_options = $self->{PlotOptions}->extend({
					       Scale => 0,
					       Position => 0,
					       Missing => undef
					      });
      $vect_options->add_synonym({Pos => 'Position'});
    }
    my ($in, $opt)=_extract_hash(@_);
    barf 'Usage: vect ( $a, $b, [$scale, $pos, $transform, $misval] )' if $#$in<1 || $#$in>5;
    my ($a, $b, $scale, $pos, $tr, $misval) = @$in;
    $self->_checkarg($a,2); $self->_checkarg($b,2);
    my($nx,$ny) = $a->dims;
    my($n1,$n2) = $b->dims;
    barf 'Dimensions of $a and $b must be the same' unless $n1==$nx && $n2==$ny;

    my ($o, $u_opt) = $self->_parse_options($vect_options, $opt);
    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});

    # Parse for options input instead of calling convention
    $scale = $o->{Scale} if exists($u_opt->{Scale});
    $pos = $o->{Position} if exists($u_opt->{Scale});
    $tr = $o->{Transform} if exists($u_opt->{Transform});
    $misval = $o->{Missing} if exists($u_opt->{Missing});

    $scale = 0 unless defined $scale;
    $pos   = 0 unless defined $pos;

    if (defined($tr)) {
      $self->_checkarg($tr,1);
      barf '$transform incorrect' if nelem($tr)!=6;
    } else {
      $tr = float [0,1,0, 0,0,1];
    }
    $tr = $self->CtoF77coords($tr);

    $self->initenv( 0, $nx-1, 0, $ny-1, $opt ) unless $self->held();
    print "Vectoring $nx x $ny images ...\n" if $PDL::verbose;

    $self->_save_status();
    $self->_standard_options_parser($u_opt); # For arrowtype and arrowhead
    pgvect( $a->get_dataref, $b->get_dataref, $nx,$ny,1,$nx,1,$ny, $scale, $pos,
	    $tr->get_dataref, $misval);
    $self->_restore_status();
    $self->_add_to_state(\&vect, $in, $opt);
    1;
  }
}

# ############ Text routines #############



{
  # Do not create this object unless necessary.
  my $text_options = undef;

  sub text {
    my $self = shift;

    if (!defined($text_options)) {
      # This is the first time this routine is called so we
      # have to initialise the options object.
      $text_options = $self->{PlotOptions}->extend({
					       Angle => 0.0,
					       Justification => 0.0,
					       Text => '',
					       XPos => undef,
					       YPos => undef
					      });
      $text_options->add_synonym({Justify => 'Justification'});
      $text_options->add_synonym({Bg => 'BackgroundColour'});
    }

    # Extract the options hash and separate it from the other input
    my ($in, $opt)=_extract_hash(@_);
    $opt = {} if !defined($opt);
    barf 'Usage: text ($text, $x, $y, [,$opt])' if 
      (!defined($opt) && $#$in < 2) || ($#$in > 3) || ($#$in < 0);
    my ($text, $x, $y)=@$in;

    # Next - parse options
    my ($o, $u_opt) = $self->_parse_options($text_options, $opt);
    # Check for change of panel or request to erase the panel
    $self->_check_move_or_erase($o->{Panel}, $o->{Erase});
    # Parse standard options such as colour
    $self->_save_status();

    $self->_standard_options_parser($u_opt);

    # Finally do what the routine needs to do.
    $o->{Text}=$text if defined($text);
    $o->{XPos}=$x if defined($x);
    $o->{YPos}=$y if defined($y);
    barf "text: You must specify the X-position!\n" if !defined($o->{XPos});
    barf "text: You must specify the Y-position!\n" if !defined($o->{YPos});

    # Added support for different background colours..
    # 2/10/01 JB - To avoid -w noise we use a reg-exp..
    if ($o->{BackgroundColour} !~ m/^-?\d+$/) {
      # Do this unless a negative integer..
      $self->_set_colour($o->{BackgroundColour}, 1);
    }
    pgptxt($o->{XPos}, $o->{YPos}, $o->{Angle}, $o->{Justification},
	   $o->{Text});
#
    $self->_restore_status();
    $self->_add_to_state(\&text, $in, $opt);

    1;
  }
}



{
  my $legend_options = undef;

  sub legend {

    my $self = shift;
    if (!defined($legend_options)) {
      $legend_options = $self->{PlotOptions}->extend({
						 Text	   => undef,
						 XPos	   => undef,
						 YPos	   => undef,
						 Width     => 'Automatic',
						 Height    => 'Automatic',
						 TextFraction  => 0.5,
						 TextShift => 0.1,
						 VertSpace => 0
						     });
      $legend_options->synonyms({ VSpace => 'VertSpace' });
      $legend_options->synonyms({ Fraction => 'TextFraction' });
      $legend_options->add_synonym({Bg => 'BackgroundColour'});
    }
    my ($in, $opt)=_extract_hash(@_);
    $opt = {} if !defined($opt);
    my ($o, $u_opt) = $self->_parse_options($legend_options, $opt);

    #
    # In this function there are several options that we do not want
    # parsed by the standard options parsers so we deal with these
    # here - we translate the linestyles, symbols and colours below
    #
    my $linestyle=$u_opt->{LineStyle}; delete $u_opt->{LineStyle};
    $linestyle=[$linestyle] if !ref($linestyle) eq 'ARRAY';
    my $linewidth=$u_opt->{LineWidth}; delete $u_opt->{LineWidth};
    $linewidth=[$linewidth] if !ref($linewidth) eq 'ARRAY';
    my $color = $u_opt->{Colour}; delete $u_opt->{Colour};
    $color=[$color] if !ref($color) eq 'ARRAY';
    my $symbol = $u_opt->{Symbol}; delete $u_opt->{Symbol};
    $symbol=[$symbol] if !ref($symbol) eq 'ARRAY';

    my ($text, $x, $y, $width)=@$in;
    $o->{Text} = $text if defined($text);
    $o->{XPos} = $x if defined($x);
    $o->{YPos} = $y if defined($y);
    $o->{Width} = $width if defined($width);

    # We could keep accessing $o but this is more succint.
    $text = $o->{Text};

    if (!defined($o->{XPos}) || !defined($o->{YPos}) || !defined($o->{Text})) {
      barf 'Usage: legend $text, $x, $y [,$width, $opt] (styles are given in $opt)';
    }
    $self->_save_status();

#    print "Setting character size to: ".$u_opt->{CharSize}."\n"
#      if defined $u_opt->{CharSize};
    $self->_standard_options_parser($u_opt); # Set font, charsize, colour etc.

    # Ok, introductory stuff has been done, lets get down to the gritty
    # details. First let us save the current character size.
    my $chsz; pgqch($chsz);

#    print "I found a character size of $chsz\n";
    # In the following we want to deal with an array of text.
    $text = [$text] unless ref($text) eq 'ARRAY';

    ## Now, set the background colour of the text before getting further.
    ## Added 2/10/01 - JB - test as a regexp to avoid -w noise.
    if ($o->{BackgroundColour} !~ m/^-?\d+$/) {
      # Do this unless a negative integer..
      $self->_set_colour($o->{BackgroundColour}, 1);
    }

    # The size of the legend can be specified by giving the width or the
    # height so to calculate the required text size we need to find the
    # minimum required (since text in PGPLOT cannot have variable width
    # and height.
    # Get the window size.
    my ($xmin, $xmax, $ymax, $ymin);
    pgqwin($xmin, $xmax, $ymin, $ymax);

    # note: VertSpace is assumed to be a scalar
    my $vspace = $o->{VertSpace};

    my $required_charsize=$chsz*9000;
    if ($o->{Width} eq 'Automatic' && $o->{Height} eq 'Automatic') {
      # Ok - we just continue with the given character size.
      $required_charsize = $chsz;
      # We still need to calculate the width and height of the legend
      # though. Fixed 20/3/01

      my $t_width = -1; # Very short text...
      my $t_height = -1; # And very low
      foreach my $t (@$text) {
	# Find the bounding box of left-justified text
	my ($xbox, $ybox);
	pgqtxt($xmin, $ymin, 0.0, 0.0, $t, $xbox, $ybox);
	if (($$xbox[2]-$$xbox[0]) > $t_width) {
	  # This is now the longest line.
	  $t_width = $$xbox[2]-$$xbox[0];
	}
	if (($$ybox[2]-$$ybox[0]) > $t_height) {
	  $t_height = $$ybox[2]-$$ybox[0];
	}
      }

      $o->{Width} = $t_width/$o->{TextFraction};
      # we include an optional vspace (which is given as a fraction of the
      # height of a line)
      $o->{Height} = $t_height*(1+$vspace)*($#$text+1); # The height of all lines..
    } else {
      # We have some constraint on the size.
      my ($win_width, $win_height)=($xmax-$xmin, $ymax-$ymin);

      # If either the width or the height is set to automatic we set
      # the width/height here to be 2 times the width/height of the
      # plot window - thus ensuring not too large a text size should the
      # user have done something stupid, but still large enough to
      # detect an error.
      $o->{Width}=2*$win_width/$o->{TextFraction} if $o->{Width} eq 'Automatic';
      $o->{Height}=2*$win_height if $o->{Height} eq 'Automatic';

      my $n_lines = $#$text+1; # The number of lines.
      foreach my $t (@$text) {
	# Find the bounding box of left-justified text
	my ($xbox, $ybox);
	pgqtxt($xmin, $ymin, 0.0, 0.0, $t, $xbox, $ybox);

	# Find what charactersize is required to fit the height
	# (accounting for vspace) or fraction*width:
	my $t_width= $o->{TextFraction}*$o->{Width}/($$xbox[2]-$$xbox[0]);
	my $t_height = $o->{Height}/(1+$vspace)/$n_lines/($$ybox[2]-$$ybox[0]); # XXX is (1+$vspace) correct

	$t_chsz = ($t_width < $t_height ? $t_width*$chsz : $t_height*$chsz);
#	print "For text = $t the optimal size is $t_chsz ($t_width, $t_height)\n";
	$required_charsize = $t_chsz if $t_chsz < $required_charsize;

	pgsch($required_charsize*$chsz); # Since we measured relative to $chsz
      }
    }

    #
    # Ok, $required_charsize should now contain the optimal size for the
    # text. The next step is to create the legend. We can set linestyle,
    # linewidth, colour and symbol for each of these texts.
    #
    my ($xpos, $ypos) = ($o->{XPos}, $o->{YPos});
    my ($xstart, $xend)=($o->{XPos}+$o->{TextFraction}*$o->{Width}+
			 $o->{TextShift}*$o->{Width}, $o->{XPos}+$o->{Width});

    my $n_lines=$#$text+1;

    # step size in y
    my $ystep = $o->{Height} / $n_lines;

    foreach (my $i=0; $i<=$#$text; $i++) {
      $self->text($text->[$i], $xpos, $ypos);
      # Since the parsing of options does not go down array references
      # we need to create a temporary PDL::Options object here to do the
      # parsing..
      my $t_o = $self->{PlotOptions}->options({
					Symbol => $symbol->[$i],
					LineStyle => $linestyle->[$i],
					LineWidth => $linewidth->[$i],
					Colour => $color->[$i]
				      });
      my $col; pgqci($col);
      $self->_set_colour($t_o->{Colour}) if defined($color->[$i]);
      my ($lw, $ls);
      pgqls($ls); pgqlw($lw);

      # Use the following to get the lines/symbols centered on the
      # text.
      my ($xbox, $ybox);
      pgqtxt($xpos, $ypos, 0.0, 0.0, $text->[$i], $xbox, $ybox);
      if (defined($$symbol[$i])) {
#	print "I will be using symbol $$o{Symbol}\n";
	my ($xsym, $ysym)=(0.5*($xstart+$xend), 0.5*($$ybox[2]+$$ybox[0]));

	pgpt(1, $xsym, $ysym, $t_o->{Symbol});
      } else {
#	print "I will be drawing a line with colour $$o{Colour} and style $$o{LineStyle}\n";
	my $yline=0.5*($$ybox[2]+$$ybox[0]);
	pgsls($t_o->{LineStyle}) if defined($linestyle->[$i]);
	pgslw($t_o->{LineWidth}) if defined($linewidth->[$i]);
	pgline(2, [$xstart, $xend], [$yline, $yline]);
      }
      $self->_set_colour($col); # Reset colour after each line so that the text comes
      # in a sensible colour
      pgsls($ls); # And line style
      pgslw($lw); # And line width
      $ypos -= $ystep;
    }


    $self->_restore_status();
    $self->_add_to_state(\&legend, $in, $opt);
  }

}





############## Cursor routine ##################



{
  $cursor_options = undef;
  sub cursor {

    my $self = shift;
    # Let us check if this is a hardcopy device, in which case we will return
    # with a warning and undefined values.
    my ($hcopy, $len);
    pgask(0);
    pgqinf("HARDCOPY",$hcopy,$len);
    if ($hcopy eq 'YES') {
      warn "cursor called on a hardcopy device - returning!\n";
      return (undef, undef, undef, undef, undef);
    }

    if (!defined($cursor_options)) {
      $cursor_options = PDL::Options->new(
					  {
					   'XRef' => undef,
					   'YRef' => undef,
					   'Type' => 0
					  });
      $cursor_options->translation({Type=>{
				   'Default'                  => 0,
				   'RadialLine'		      => 1,
				   'Rectangle'		      => 2,
				   'TwoHorizontalLines'	      => 3,
				   'TwoVerticalLines'	      => 4,
				   'HorizontalLine'	      => 5,
				   'VerticalLine'	      => 6,
				   'CrossHair'		      => 7
				  }});
    }

    my ($opt)=@_;

    $opt = {} unless defined($opt);
    my $place_cursor=1; # Since X&Y might be uninitialised.
    my $o = $cursor_options->options($opt);

    my ($x, $y, $ch);

    # The window needs to be focussed before using the cursor commands.
    # Added 08/08/01 by JB after bug report from Brad Holden.
    $self->focus();

    if ($o->{Type} eq 'Rectangle' && !defined($o->{XRef})) {
      #
      # We use pgcurs to get a first position.
      #
      print "Please select a corner of the rectangle\n";
      pgcurs($x, $y, $ch);
      $o->{XRef}=$x;
      $o->{YRef}=$y;
    }

    if ($o->{Type} > 7 || $o->{Type} < 0) {
      print "Unknown type of cursor $$o{Type} - using Default\n";
      $o->{Type}=0;
    }
    my ($xmin, $xmax, $ymax, $ymin);
    pgqwin($xmin, $xmax, $ymin, $ymax);

    $x = 0.5*($xmin+$xmax) if !defined($x);
    $y = 0.5*($ymin+$ymax) if !defined($y);

    my ($got_xref, $got_yref)=(defined($o->{XRef}), defined($o->{YRef}));
    if (!$got_xref || !$got_yref) {
      # There is a little bit of gritty error-checking
      # for the users convenience here.
      if ($o->{Type}==1 || $o->{Type}==2) {
	barf "When specifying $$o{Type} as cursor you must specify the reference point";
      } elsif ($o->{Type}==3 && !$got_yref) {
	barf "When specifying two horizontal lines you must specify the Y-reference";
      } elsif ($o->{Type}==4 && !$got_xref ) {
	barf "When specifying two vertical lines you must specify the X-reference";
      }

      # Ok so we have some valid combination of type and reference point.
      $o->{XRef}=$xmin if !$got_xref;
      $o->{YRef}=$ymin if !$got_yref;

    }


    $ch = ''; # To silence -w
    my $istat = pgband($o->{Type}, $place_cursor, $o->{XRef},
		       $o->{YRef}, $x, $y, $ch);

    $self->_add_to_state(\&cursor, [], $opt);
    return ($x, $y, $ch, $o->{XRef}, $o->{YRef});

  }
}


=head1 INTERNAL

The coding tries to follow reasonable standards, so that all functions
starting with an underscore should be considered as internal and should
not be called from outside the package. In addition most routines have
a set of options. These are encapsulated and are not accessible outside
the routine. This is to avoid collisions between different variables.


=head1 AUTHOR

Karl Glazebrook [kgb@aaoepp.aao.gov.au] modified by Jarle Brinchmann
(jarle@astro.ox.ac.uk) who is also responsible for the OO interface,
docs mangled by Tuomas J. Lukka (lukka@fas.harvard.edu) and
Christian Soeller (c.soeller@auckland.ac.nz). Further contributions and
bugfixes from Kaj Wiik, Doug Burke and many others.

All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

=cut

#

1;

__DATA__

