=head1 NAME

PDL::Graphics::PGPLOTOptions - Setting PGPLOT options

=head1 SYNOPSIS

use PGPLOTOptions qw('default_options');

=head1 DESCRIPTION

This package contains one function (at present) which returns PDL::Option
objects for default settings for plot windows and plot commands. This
should be complemented by functions that could affect this such as
file reading commands etc.

=head1 OPTIONS

The following is a listing of options that are set in this file and what
they do and what their default value is

=head2 Window specfic options

These options modify the appearance of windows and can also modify the
default settings for creation of plot axes etc.

=over

=item Device

The default PGPLOT device to use. The default value is set to the PGPLOT_DEV
environment variable if set, otherwise to '?'.

=item AxisColour

The colour with which to draw axes

=item HardLW, HardCH, HardFont, HardAxisColour

The linewidth, character height, font and axis colour to use on hardcopy
devices.

=item Axis

The axis style to use. See the L<PDL::Graphics::PGPLOT::Window> documentation
for details. It defaults to 'Normal' which is a labelled box. Valid arguments
are 'Empty', 'Box', 'Normal', 'Axes', 'Grid', 'LogX', 'LogY', 'LogXY'.

=item AspectRatio

The aspect ratio of the output device it defaults to 0.618.

=item WindowWidth

The width of the output window in inches and defaults to as big as possible.

=item WindowXSize and WindowYSize

These are alternatives to AspectRatio and WindowWidth.

=item WindowName

The name of the window - can later be retrieved using name(). It defaults
to 'Window'+Window ID.

=item NXPanel

The number of panels in the X-direction - defaults to 1

=item NYPanel

The number of panels in the Y-direction - defaults to 1

=item Justify

A boolean value which, if true, causes both axes to drawn
to the same scale; see
the PGPLOT C<pgenv()> command for more information.

=item Border

Adjust the spacing around the plot. See the documentation in
L<PDL::Graphics::PGPLOT::Window> for details.

=item CharSize

The default charsize for the plot - used when annotating the axes for
instance.

=item PlotPosition

The position of the plot in normalised coordinates.

=item NoErase

Allows plotting multiple panels on one plot, normally used with 'PlotPosition'.

=back

=head2 Plot specific options


For the moment see the C<PDL::Graphics::PGPLOT::Window> documentation for
these.


=cut

package PDL::Graphics::PGPLOTOptions;


# use PDL::Core qw/:Func :Internal/;
use Exporter;
use strict;
use vars qw(@ISA @EXPORT_OK);

@ISA = ('Exporter');
@EXPORT_OK = qw(default_options);

#
# At the moment this does not have the option of setting default values
# for options externally. This should be resolved.
#

sub default_options {

  my $DEV=undef;
  $DEV  = $ENV{"PGPLOT_DEV"} if defined $ENV{"PGPLOT_DEV"};
  $DEV  = "?" if !defined($DEV) || $DEV eq ""; # Safe default

  # Options specific (primarily) to window creation
  my $wo = {
	    Device => $DEV, ### Tidy this up.
	    AxisColour  => 3,	 # Axis colour
	    HardLW      => 4,	 # Line width for hardcopy devices,
	    HardCH      => 1.4,	 # Character height for hardcopy devices
	    HardFont    => 2,	 # For for hardcopy devices
	    HardAxisColour => 1,     # Black colour as default on hardcopy devices.
	    Axis        => 'BCNST',# The type of box
	    AspectRatio => 0.618, # The aspect ratio of the plot window.
	    WindowWidth => 10,    # The width of the plot window in inches.
	    WindowXSize => undef, # The X&Y size of a window, these will be
	    WindowYSize => undef, # used to give the aspect ratio if defined.
	    WindowName  => '',    # The window name given
	    NXPanel     => 1,     # The number of plotting panels
	    NYPanel     => 1,     # Ditto.
	    Justify     => 0,
	    Border      => 0,
	    CharSize    => 1,     # Character size for annotation
	    NoErase     => 0,
	    PlotPosition => 'Default' # The position of the plot on the page.
	   };


  # Options specific to plotting commands
  my $o = {
	   Symbol      => 17,	 # Symbol for points
	   Colour      => 5,	 # Colour for plots
	   CharSize    => 1,     # Character height
	   ErrTerm     => 1,	 # Size of error-bar terminators
           Erase       => 0,     # Whether to erase a panel when switching.
	   Panel       => undef, # What panel to switch to.
	   LineStyle   => 1,	 # Solid linestyle
	   Font	       => 1,	 # Normal font
	   Fill	       => 1,	 # Solid fill
	   ITF	       => 0,	 # Linear ITF
	   Axis	       => 0,	 # Standard axis-type
	   Transform   => undef, # The transform used for plots.
	   LineWidth   => 1,
	   # The following two should really be implemented as an Options
	   # object, but that will make I/O of options somewhat difficult.
	   # Note that the arrowsize is implemented as a synonym for the
	   # charsize this should not cause any problems but might be worth
	   # noting...
	   # In addition to this the arrowsize below is also set to be undefined
	   # by default which will automatically use the character size.
	   # All these problems are historical..
	   Arrow       => {FS => 1, Angle => 45.0, Vent => 0.3,
			   ArrowSize => undef},
	   Hatch       => {Angle => 45.0, Separation => 1.0, Phase => 0.0},
	   XTitle      => '',    # Label for X-axis
	   YTitle      => '',    # Label for Y-axis
	   Title       => '',    # Title for plot
	  };


  # Now for the synonyms
  my $s = {Color => 'Colour', 'Line-style' => 'LineStyle',
	   'Line-width' => 'LineWidth', 'Hatching' => 'Hatch',
	   FillType => 'Fill', 'ArrowSize' => 'CharSize'};
  #
  # And now for the lookup tables..
  #
  my $t = {
	   Colour => {
			'White' => 0, 'Black' => 1, 'Red' => 2,
			'Green' => 3, 'Blue' => 4, 'Cyan' => 5,
			'Magenta' => 6,	'Yellow' => 7, 'Orange' => 8,
			'DarkGray' => 14, 'DarkGrey' => 14,
			'LightGray' => 15, 'LightGrey' => 15
		       },
	   Symbol => {
		       'Square' => 0, 'Dot' => 1, 'Plus' => 2,
		       'Asterisk' => 3, 'Circle' => 4, 'Cross' => 5,
		       'Triangle' => 7, 'Earth' => 8, 'Sun' => 9,
		       'Diamond' => 11, 'Star' => 12, Default => 17
		      },
	   ITF => {
		   'Linear' => 0, 'Log' => 1, 'Sqrt' => 2
		  },
	   LineStyle => {
			 'Solid' => 1, 'Dashed' => 2, 'Dot-Dash' => 3,
			 'Dotted' => 4, 'Dash-Dot-Dot' => 5,
			 '-' => 1, '--' => 2, '.-' => 3, '.' => 4,
			 '-..' => 5
			},
	   Font => {
		    Normal => 1, Roman => 2,Italic => 3, Script => 4
		   },
	   Fill => {
		    Solid => 1, Outline => 2, Hatched => 3,
		    Cross_Hatched => 4, CrossHatched => 4
		   },
	  };

    my $wt = {
	      # valid values for axis parameter (eg env())
	      Axis => {
		       Empty => '', Box => 'BC', Normal => 'BCNST', 
		       Axes => 'ABCNST', Grid => 'ABCGNST', 
		       LogX => ['BCLNST', 'BCNST'],
		       LogY => ['BCNST', 'BCLNST'],
		       LogXY => ['BCLNST', 'BCLNST'],
		       '-2' => '', '-1' => 'BC', '0' => 'BCNST', 
		       '1' => 'ABCNST', '2' => 'ABCGNST',
		       '10' => ['BCLNST', 'BCNST'],
		       '20' => ['BCNST', 'BCLNST'],
		       '30' => ['BCLNST', 'BCLNST']
		      },
 	      AxisColour => {
			     'White' => 0, 'Black' => 1, 'Red' => 2,
			     'Green' => 3, 'Blue' => 4, 'Cyan' => 5,
			     'Magenta' => 6,	'Yellow' => 7, 'Orange' => 8,
			     'DarkGray' => 14, 'DarkGrey' => 14,
			     'LightGray' => 15, 'LightGrey' => 15
			    },
	      HardFont => {
		       Normal => 1, Roman => 2,Italic => 3, Script => 4
		      },
 	      HardAxisColour => {
			     'White' => 0, 'Black' => 1, 'Red' => 2,
			     'Green' => 3, 'Blue' => 4, 'Cyan' => 5,
			     'Magenta' => 6,	'Yellow' => 7, 'Orange' => 8,
			     'DarkGray' => 14, 'DarkGrey' => 14,
			     'LightGray' => 15, 'LightGrey' => 15
			    }
	     };


  # Set up the two primary sets of options for PGPLOT commands.
  my $window_options = PDL::Options->new($wo);
  $window_options->translation($wt);

  my $general_options = PDL::Options->new($o);
  $general_options->translation($t);
  $general_options->synonyms($s);

  return ($general_options, $window_options);

}


1;
