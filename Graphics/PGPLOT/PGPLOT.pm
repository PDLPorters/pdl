# Graphics functions for the PDL module, this module
# requires the PGPLOT module be previously installed.
# PGPLOT functions are also made available to the caller.

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

C<PDL::Graphics::PGPLOT> is a convenience interface to the PGPLOT commands,
implemented using the object oriented PGPLOT plotting package in
L<PDL::Graphics::PGPLOT::Window>. See the documentation for that package
for in-depth information about the usage of these commands and the options
they accept.

The list of currently availably commands:

 imag       -  Display an image (uses pgimag()/pggray() as appropriate)
 im         -  Shorthand to display an image with aspect ratio of 1
 fits_imag  -  Display a FITS image with appropriate transforms & labels
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

 hold         -  Hold current plot window range - allows overlays etc.
 release      -  Release back to autoscaling of new plot window for each 
                 command
 rel          -  short alias for 'release'
 env          -  Define a plot window, put on 'hold'
 dev          -  Explicitly set a new PGPLOT graphics device
 new_window   -  Create a new plot window (use of dev is recommended)
 focus_window -  Change focus to a new window
 window_list  -  Get a list of currently exisiting plot windows
 close_window -  Close an open window


=head1 FUNCTIONS

The following is a list of the functions that are private to this package,
for the other functions please read the L<PDL::Graphics::PGPLOT::Window>
documentation.

=head2 dev

=for ref

Open PGPLOT graphics device

=for usage

 Usage: dev $device, [$nx,$ny, $opt];

C<$device> is a PGPLOT graphics device such as "/xserve" or "/ps",
if omitted defaults to last used device (or value of env
var C<PGPLOT_DEV> if first time).
C<$nx>, C<$ny> specify sub-panelling. The function returns the id of
the newly created window - this can subsequently be used as argument to
C<focus_window> to select the window.

The result of this command can be modified using options. The options
recognised are the same as for C<new_window> - which primarily and in
addition it is possible to set the default values for a window that are
defined in L<PDL::PGPLOTOptions>, see this for details but see below for
a synopsis.

In addition C<dev> recognises the option C<NewWindow> which allows the
user to specify that a C<dev> command is to create a new window rather than
closing the previous. This allows a large number of output destinations to
be open at the same time, which occasionally can be convenient.

Here is a quick summary of the most useful additional options that can
be given:

=over

=item Device

Alternative to C<$device>.

=item AspectRatio

The aspect ratio of the output window

=item WindowWidth

The width of the plot window in inches

=item AxisColour

The axis colour to be used as default for plots in this window. In the same
way it is possible to set the default character size (C<CharSize>) and axis
and box styles. See L<PDL::Graphics::PGPLOTOptions> for details.

=item WindowName

The name of a window. This name can subsequently be used to refer to the
window instead of its ID, making interactive use somewhat more intuitive.

=back

=for example

To open a X-window output that will stay on screen:

  $win = dev('/xs');

To open two windows, one small and square, one large and wide:

  $win1 = dev('/xs', {Aspect => 1, WindowWidth => 4});
  $win2 = dev('/xs', {Aspect => 0.5, WindowWidth => 10});

=cut

package PDL::Graphics::PGPLOT;

# Just a plain function exporting package

use PDL::Core qw/:Func :Internal/; # Grab the Core names
use PDL::Graphics::PGPLOTOptions qw(default_options);
use PDL::Graphics::PGPLOT::Window;
use PGPLOT;
use Exporter;

use strict;

use vars qw (@ISA @EXPORT);

@ISA = ('Exporter');

@EXPORT = qw( dev hold release rel env bin cont errb line points
	      fits_imag imag imag1 draw_wedge ctab ctab_info hi2d poly vect CtoF77coords
	      new_window focus_window window_list close_window
	      label_axes text legend cursor circle ellipse rectangle
	      tpoints tline retrieve_state replay turn_off_recording
	      turn_on_recording clear_state autolog get_current_window
              transform
	    );

*rel = *release;		# Alias
*image = *imag;

############################################################################

#############################################################
# This is a new version of PGPLOT which uses PDL::Options for
# option parsing.
#############################################################

# Option explanation:
#
#   Each routine has a set of options, and there is also a set of
#   global options that may or may not affect a particular routine.
#   The global options are defined here in the start of the code.
#
#   This require a minor adjustment to the PDL::Options code since
#   otherwise we would need to define the global options everywhere.
#
#   The actual setting of default parameters is split off in a separate
#   file PDL::Graphics::PGPLOTOptions - which also exports default_options
#   used below.

# The list of default global opttions, synonyms and translations.
#

END {				# Destructor to close plot when perl exits
  _close_windows();
}

#############################################################
# We now want to be able to have several plotting windows   #
# The current set of windows is stored in these             #
# variables - accessed by the local subs.  Added JB 12/7/00 #
#                                                           #
# This has lead to a substantial rewrite of the code since  #
# all the real work is now done in the Window object which  #
# this routine only provides a convenient (and backwards    #
# compatible) interface to.                                 #
#############################################################




my @_WINDOWS=();		# The list of windows to access - the value is the options.
my %_WINDOWNAMES = ();		# A map of names for each window to their number.
my $CW = undef;


=head2 new_window

=for ref

Open a PGPLOT graphics device

=for usage

  $win = new_window($dev, $nx, $ny, $opt);

This function is identical to L<dev> except that it always creates a new
window. This means that the user is required to close all windows
explicitly using L<close_window>. All functionality is otherwise like C<dev>
so see the documentation for L<dev> for details of use.

=cut

sub new_window {
  my ($dev, $nx, $ny, $opt)=@_;

  if (ref($dev) eq 'HASH') {
    $opt = $dev;
    ($dev, $nx, $ny)=(undef, undef, undef);
  } elsif (ref($nx) eq 'HASH') {
    $opt = $nx;
    ($nx, $ny)=(undef, undef);
  }

  $opt={}  unless defined($opt);

  # This will cause problems if people both pass dev, nx & ny _and_
  # passes them in an options hash with poor spelling - don't do that..
  $opt->{Device}=$dev if defined($dev);
  $opt->{NXPanel}=$nx if defined($nx);
  $opt->{NYPanel}=$ny if defined($ny);

  # Now insert the necessary information in the variables above.
  #    barf "Options must be an anonymous hash!\n" if defined($_[0]) && 
  #      ref($_[0]) ne 'HASH';
  my $win = PDL::Graphics::PGPLOT::Window->new($opt);
  my ($name, $id) = ($win->name(), $win->id());
  $_WINDOWS[$id] = $name;
  $_WINDOWNAMES{$name}=$win;
  $_WINDOWNAMES{$id}=$name;	# Reverse lookup for speed

  $CW = $win;
  if (wantarray) {
    return ($id, $name, $win);
  } else {
    return $id;
  }
}


# Close all windows.
sub _close_windows {
  close_window({All=>1});	# Do all windows...
}



=head2 close_window

=for ref

Close a PGPLOT output device

=for usage

 Usage: close_window($id)

This function closes a PGPLOT output device created with C<dev> or
C<new_window>. It requires the id of the window to close. If C<$id> is
left undefined, the currently focussed window is deleted and focus is
transferred to the lowest numbered window in existence. If many windows
have been created and deleted this might not be what you expect, so
it is recommended to make an explicit call to L<focus_window> after
any call to C<close_window>.

=cut

sub close_window {
  my ($name)=@_;

  if (ref($name) eq 'HASH') {	# Hack - to avoid checking window names..
    for (my $id=0; $id<=$#_WINDOWS; $id++) {
      next unless defined($_WINDOWS[$id]);
      my $n = $_WINDOWS[$id];
      $_WINDOWNAMES{$n}->close();
      delete $_WINDOWNAMES{$n};
      delete $_WINDOWNAMES{$id};
    }
    @_WINDOWS=();  # Remove all windows.
    $CW = undef;   # No current window
  } else {
    #
    # Delete a specific window
    #
    my $id = _get_windownumber($name);
    my $CWid = $CW->id();

    my $n= $_WINDOWNAMES{$id};	# In case the name was not passed..
    $_WINDOWNAMES{$n}->close();
    delete $_WINDOWNAMES{$n};
    delete $_WINDOWNAMES{$id};
    $_WINDOWS[$id]=undef; #splice(@_WINDOWS, $id, 1);


    if ($CWid == $id) {
      # Now determine the current window, viz the lowest numbered
      # window existing.

      $CW = undef;
      for (my $i=0; $i<=$#_WINDOWS; $i++) {
	if (defined($_WINDOWS[$i])) {
	  $CW = $_WINDOWNAMES{$_WINDOWS[$i]};
	  last;
	}
      }
    }

    # Since we set the corresponding array elements to undef - we
    # have to check if we have in fact deleted the whole shebang in
    # which case @_WINDOWS should be reset.
    @_WINDOWS=() if (!defined($CW));
  }
}

# Utility function - allowing both numbers and names to be used.

=head2 _get_windownumber

Internal function to obtain the ID of a window. This allows the user
to refer to a window with its name.

=cut


sub _get_windownumber {
  my ($name)=@_;
  if (!defined($name) || $name eq '') {
    return -1 unless defined($CW);
    return $CW->id();
  }
  my $windownumber = -1;
  if (!exists($_WINDOWNAMES{$name}) || !ref($_WINDOWNAMES{$name})) {
    # Then it ought to be a number
    if ($name =~ m/^\d+$/) {
      $windownumber = $name;
    } else {
      print "Valid window names: \n";
      foreach my $k (keys %_WINDOWNAMES) {
	print "$k\n";
      }
      barf ("I cannot switch to window $name - no such name\n");
    }
  } else {
    $windownumber = $_WINDOWNAMES{$name}->id();
  }
  barf("Invalid windownumber ($name)\n") if
    !defined($_WINDOWS[$windownumber]);

  return $windownumber;
}



{
  my $dev_options = undef; 

  sub dev {
    # This delayed creation of the options variable is for increased speed.
    # Although for dev() this is unnecessary...
    if (!defined($dev_options)) {
      $dev_options = PDL::Options->new({NewWindow => 0});
      $dev_options->warnonmissing(0); # Turn off warnings.
    }

    # Get the input options.
    my ($dev, $nx, $ny, $u_opt)=@_;

    if (ref($nx) eq 'HASH') {
      $u_opt = $nx;
      ($nx, $ny)=(undef, undef);
    }
    $u_opt = {} if !defined($u_opt);
    my $opt = $dev_options->options($u_opt);

    # Then we want to close the current one before opening a new one.
    if (!$opt->{NewWindow}) {
      my ($state,$len);
      pgqinf('STATE',$state,$len);
      close_window() if $state eq 'OPEN';
    }

    my $win=new_window(@_);
    return $win;
  }

}

=head2 focus_window

=for ref

Switch to another output window.

=for usage

 Usage: focus_window($id);

This command is used to switch output focus to another window created by
L<dev> or L<new_window>. The window can be referred to either by its
ID or by its name.

=for example

  $win1 = dev('/xs', {WindowName => 'X-output'});
  $win2 = dev('test.ps/ps', {WindowName => 'PS-output'});

  focus_window('X-output');  # Or focus_window($win1);
  <.. Commands ..>
  focus_window($win2);       # Or focus_window('PS-output');
  <.. Commands ..>

=cut


# Switch to a new window.
sub focus_window {
  my ($name)=@_;
  my $windownumber = _get_windownumber($name);
  die "No such window ($name)\n" if $windownumber < 0;
  $CW = $_WINDOWNAMES{$_WINDOWS[$windownumber]};
  print "Window focus switched to Window nr $windownumber ($_WINDOWNAMES{$windownumber})\n" if $PDL::verbose;
  $CW->focus();
}


=head2 window_list

=for ref

Return a list of ID numbers and names of the windows currently opened using
L<dev> or C<new_window>.

=for usage

 Usage: ($numbers, $names)=window_list();

C<$numbers> and C<$names> are anonymous arrays giving the ID numbers and
names of the open windows respectively.

=cut

# And getting a list of windows
sub window_list {
  my @numbers=();
  my @names=();
  foreach (keys %_WINDOWNAMES) {
    if (ref($_WINDOWNAMES{$_}) eq 'PDL::Graphics::PGPLOT::Window') {
      push @names, $_;
    } else {
      push @numbers, $_;
    }
  }
  return (wantarray ? (\@numbers, \@names) : \@numbers);
}


sub label_axes {
  # We do not label axes when there is no plot window.
  return if !defined($CW);
  $CW->label_axes(@_);
}

sub turn_on_recording {
  if (!defined($CW)) {
    warn "You can only turn on recording when you have a device open!\n";
    return;
  }
  $CW->turn_on_recording();
}

sub turn_off_recording {
  if (!defined($CW)) {
    warn "You can only turn off recording when you have a device open!\n";
    return;
  }
  $CW->turn_off_recording();
}

sub retrieve_state {
  if (!defined($CW)) {
    warn "You can only retrieve the state when a device is open\n";
    return;
  }
  $CW->retrieve_state();
}


sub replay {
  if (!defined($CW)) {
    warn "You can only replay plotting commands when a device is open!\n";
    return;
  }
  $CW->replay(@_);
}

sub clear_state {
  if (!defined($CW)) {
    warn "You can only clear the state when a device is open\n";
    return;
  }
  $CW->clear_state();
}


sub autolog { # for this one we use the class method to set autolog globally
  dev() if !defined($CW);
  PDL::Graphics::PGPLOT::Window->autolog(@_);
}
sub env {
  dev() if !defined($CW);
  $CW->env(@_);
}
sub bin {
  dev() if !defined($CW);
  $CW->bin(@_);
}
sub cont {
  dev() if !defined($CW);
  $CW->cont(@_);
}
sub errb {
  dev() if !defined($CW);
  $CW->errb(@_);
}
sub line {
  dev() if !defined($CW);
  $CW->line(@_);
}
sub tline {
  dev() if !defined($CW);
  $CW->tline(@_);
}
sub points {
  dev() if !defined($CW);
  $CW->points(@_);
}
sub tpoints {
  dev() if !defined($CW);
  $CW->tpoints(@_);
}
sub imag {
  dev() if !defined($CW);
  $CW->imag(@_);
}
sub fits_imag {
  dev() if !defined($CW);
  $CW->fits_imag(@_);
}
sub imag1 {
  dev() if !defined($CW);
  $CW->imag1(@_);
}
sub draw_wedge {
  barf 'Open a plot window first!' if !defined($CW);
  $CW->draw_wedge(@_);
}
sub ctab {
  dev() if !defined($CW);
  $CW->ctab(@_);
}
sub ctab_info {
  dev() if !defined($CW);
  $CW->ctab_info(@_);
}
sub hi2d {
  dev() if !defined($CW);
  $CW->hi2d(@_);
}
sub poly {
  dev() if !defined($CW);
  $CW->poly(@_);
}
sub vect {
  dev() if !defined($CW);
  $CW->vect(@_);
}
sub CtoF77coords {
  dev() if !defined($CW);
  $CW->CtoF77coords(@_);
}

sub text {
  barf 'Open a plot window first!' if !defined($CW);
  $CW->text(@_);
}

sub cursor {
  barf 'Open a plot window first!' if !defined($CW);
  $CW->cursor(@_);
}

sub legend {
  barf 'Open a plot window first!' if !defined($CW);
  $CW->legend(@_);
}

sub circle {
  dev(@_) if !defined($CW);
  $CW->circle(@_);
}

sub ellipse {
  dev(@_) if !defined($CW);
  $CW->ellipse(@_);
}

sub rectangle {
  dev(@_) if !defined($CW);
  $CW->rectangle(@_);
}

sub transform {
  barf 'Open a plot window first!' if !defined($CW);
  $CW->transform(@_);
}

sub hold    { return if !defined($CW); $CW->hold(); print "Graphics on HOLD\n" if $PDL::verbose;};
sub release { return if !defined($CW); $CW->release(); print "Graphics RELEASED\n" if $PDL::verbose;};
#sub held { return 0 if !defined($CW); return $CW->held()};
#sub current_device { return $CW->device(); };

sub get_current_window { return $CW; }  # return current window or undef if none exists

1;				# Exit with OK status




