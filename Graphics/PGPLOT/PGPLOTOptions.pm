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

The colour with which to draw axes. Default value=3 (Green)

=item HardLW, HardCH, HardFont, HardAxisColour, HardColour

The linewidth, character height, font and axis colour to use on hardcopy
devices. The default values are HardLW=1, HardCH=1.4, HardFont=2 (Roman),
HardAxisColour=1 (Black) and HardColour=1 as well. The latter is the default
plot colour to use on hardcopy devices.

=item Axis

The axis style to use. See the L<PDL::Graphics::PGPLOT::Window> documentation
for details. It defaults to 'Normal' which is a labelled box. Valid arguments
are 'Empty', 'Box', 'Normal', 'Axes', 'Grid', 'LogX', 'LogY', 'LogXY'.

=item AspectRatio

The aspect ratio of the output device. The default value is device dependent.

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

=item TightLabels

Boolean value which, if true, causes axis labels to be pulled
slightly closer to the main viewport than usual.  That's handy
for making multi-panel plots.  Undef (the default) is equivalent
to 0 for panels with NYPanels <= 1 and 1 for panels with NYPanels > 1.

=item TitleSize

The relative size of a plot or image title, compared to other annotations.
Defaults to 1.0 (original behavior) but can be set to, e.g., 1.5 to 
emphasize graph titles in a multipanel plot.

=item Border

Adjust the spacing around the plot. See the documentation in
L<PDL::Graphics::PGPLOT::Window> for details.

=item CharSize

The default charsize for the plot - used when annotating the axes for
instance. It defaults to 1.

=item PlotPosition

The position of the plot in normalised coordinates.

=item Erase

Explicitly erase the plotting surface, normally required when making new
plots with PlotPosition.

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
@EXPORT_OK = qw(default_options set_pgplot_options);

#
# To be able to set options outside of PGPLOT in the .perldlrc I will
# have to define these local variables.
#
my %options = (
	       Device => undef,
	       AxisColour => 3,
	       BackgroundColour => -1, # Text background colour
	       HardLW => 1,
	       HardCH => 1.4,
	       HardFont => 2,
	       HardAxisColour => 1,
	       HardColour => 1,
	       Axis => 'BCNST', # see kludge in Window::imag if you change this
	       AspectRatio => undef,
	       WindowWidth => undef,
	       WindowXSize => undef,
	       WindowYSize => undef,
               Size => undef,
               Unit=> undef,
	       WindowName => '',
	       NXPanel => 1,
	       NYPanel => 1,
	       Justify => 0,   # Justification of boxes & axes
               Scale=> undef,  # device pixels per data pixel
               Pitch=> undef,  # Horizontal data pixels per <unit>
               Unit => undef,  # Unit for pitch 
               Pix => undef,   # Pixel aspect ratio
 	       Align => undef, # Alignment of viewport within plot area
	       Border => 0,
	       CharSize => 1,
	       Symbol => 17,
	       Colour => 5,
	       ErrTerm => 1,
	       LineStyle => 1,
	       Font => 1,
	       Fill => 1,
	       ITF => 0,
	       Transform => undef,
	       LineWidth => 1,
	       XRange => undef,
	       YRange => undef,
	       Arrow => {FS => 1, Angle => 45.0, Vent => 0.3,
			   ArrowSize => undef},
	       Hatch => {Angle => 45.0, Separation => 1.0, Phase => 0.0},
	       XTitle => '',
	       YTitle => '',
	       Title => '',
	      );


sub default_options {


  my $DEV=undef;
  # Use the standard PGPLOT environment variable.
  $DEV  = $ENV{"PGPLOT_DEV"} if defined $ENV{"PGPLOT_DEV"};
  # However if the user has specified the Perl-ish variable use that.
  $DEV  = $options{Device} if defined($options{Device});
  $DEV  = "?" if !defined($DEV) || $DEV eq ""; # Safe default

  # Options specific (primarily) to window creation
  my $wo = {
	    Device => $DEV, ### Tidy this up.
	    AxisColour  => $options{AxisColour}, # Axis colour
	    HardLW      => $options{HardLW}, # Line width for hardcopy devices,
	    HardCH      => $options{HardCH}, # Character height for hardcopy devices
	    HardFont    => $options{HardFont}, # For for hardcopy devices
	    HardAxisColour => $options{HardAxisColour},	# Black colour as default on hardcopy devices.
	    HardColour => $options{HardColour},	# Black as default plot colour on hardcopy devices.
	    Axis        => $options{Axis}, # The type of box
 	    AspectRatio => $options{AspectRatio}, # The aspect ratio of the plot window.
 	    WindowWidth => $options{WindowWidth}, # The width of the plot window in inches.
	    WindowXSize => $options{WindowXSize}, # The X&Y size of a window, these will be
	    WindowYSize => $options{WindowYSize}, # used to give the aspect ratio if defined.
	    Size        => $options{Size},        # alternative window size spec
	    Unit        => $options{Unit},        # Units for size spec
	    WindowName  => $options{WindowName}, # The window name given
	    NXPanel     => $options{NXPanel}, # The number of plotting panels
	    NYPanel     => $options{NYPanel}, # Ditto.
	    TightLabels => undef,
	    TitleSize   => 1.0,
	    Justify     => $options{Justify}, # Justification of boxes & axes
	    Scale       => $options{Justify}, # device pixels per data pixel
	    Pitch       => $options{Pitch},   # Horizontal data pixels per unit
	    Unit        => $options{Unit},    # PGPLOT unit for pitch
	    Pix         => $options{Pix},     # Pixel aspect ratio
	    Align       => $options{Align},   # Alignment of vp in plot area
	    Border      => $options{Border},
	    CharSize    => $options{CharSize}, # Character size for annotation
	    Erase       => 0,
	    Recording   => 0,	# Off by default.
	    PlotPosition => 'Default' # The position of the plot on the page.
	   };

  # Options specific to plotting commands
  my $o = {
	   Symbol      => $options{Symbol},	 # Symbol for points
	   Colour      => $options{Colour},	 # Colour for plots
	   CharSize    => $options{CharSize},     # Character height
	   ErrTerm     => $options{ErrTerm},	 # Size of error-bar terminators
           Erase       => 0,     # Whether to erase a panel when switching.
	   Panel       => undef, # What panel to switch to.
	   LineStyle   => $options{LineStyle},	 # Solid linestyle
	   Font	       => $options{Font},	 # Normal font
	   Fill	       => $options{Fill},	 # Solid fill
	   ITF	       => $options{ITF},	 # Linear ITF
	   Axis	       => $options{Axis},	 # Standard axis-type

	   Transform   => $options{Transform},   # The transform used for plots.
           Justify     => $options{Justify}, # Justification of boxes & axes
	   Scale       => $options{Justify}, # device pixels per data pixel
	   Pitch       => $options{Pitch},   # Horizontal data pixels per unit
	   Unit        => $options{Unit},    # PGPLOT unit for pitch
	   Pix         => $options{Pix},     # Pixel aspect ratio
	   Align       => $options{Align},   # Alignment of vp in plot area

	   LineWidth   => $options{LineWidth},
	   TightLabels => $options{TightLabels},
	   TitleSize   => $options{TitleSize},
	   XRange      => $options{XRange},
	   YRange      => $options{YRange},
	   BackgroundColour => $options{BackgroundColour},
	   # The following two should really be implemented as an Options
	   # object, but that will make I/O of options somewhat difficult.
	   # Note that the arrowsize is implemented as a synonym for the
	   # charsize this should not cause any problems but might be worth
	   # noting...
	   # In addition to this the arrowsize below is also set to be undefined
	   # by default which will automatically use the character size.
	   # All these problems are historical..
	   Arrow       => $options{Arrow},
	   Hatch       => $options{Hatch},
	   XTitle      => $options{XTitle},    # Label for X-axis
	   YTitle      => $options{YTitle},    # Label for Y-axis
	   Title       => $options{Title},    # Title for plot
	  };


  # Now for the synonyms
  my $s = {Color => 'Colour', 'Line-style' => 'LineStyle',
	   'Line-width' => 'LineWidth', 'Hatching' => 'Hatch',
	   FillType => 'Fill', 'ArrowSize' => 'CharSize',
	   AxisColor => 'AxisColour', HardAxisColor => 'HardAxisColour',
	   HardColor => 'HardColor', BackgroundColor => 'BackgroundColour'};
  #
  # And now for the lookup tables..
  #
  my $t = {
	   Colour => {
			'White' => 0, 'Black' => 1, 'Red' => 2,
			'Green' => 3, 'Blue' => 4, 'Cyan' => 5,
			'Magenta' => 6,	'Yellow' => 7, 'Orange' => 8,
			'DarkGray' => 14, 'DarkGrey' => 14,
			'LightGray' => 15, 'LightGrey' => 15,
		      'CosmicSpectrum' => [0.269, 0.388, 0.342]
		       },
	   BackgroundColour => {
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


=head2 set_pgplot_options

This function allows the user to set the default PGPLOT options. It
is particularly useful in the C<.perldlrc> file since one can do

  use PDL::Graphics::PGPLOTOptions ('set_pgplot_options');
  set_pgplot_options('Device' => '/xs', 'HardLW' => 3);

for instance to set the default values. The main drawback is that the
routine is rather unflexible with no synonyms or case-insensitivity.

=cut

sub set_pgplot_options {
  my %o;
  if (ref($_[0]) eq 'HASH') {
    %o = %{$_[0]};
  } else {
    %o = @_;
  }

  foreach my $k (keys %o) {
    if (exists($options{$k})) {
      $options{$k} = $o{$k};
    } elsif ($k =~ /Color/) {
      my $knew = $k;
      $knew =~ s/Color/Colour/;
      if (!exists($options{$knew})) {
	warn "Option $k is not recognised!\n";
      } else {
	$options{$knew} = $o{$k};
      }
    } else {
      warn "Option $k is not recognised!\n";
    }
  }

}


1;
