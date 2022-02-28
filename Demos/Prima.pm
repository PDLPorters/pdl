package PDL::Demos::Prima;

use strict;
use warnings;
use PDL::Graphics::Prima 0.13;

sub info {('prima', 'Prima graphics (requires PDL::Graphics::Prima)')}
sub demo {[actnw => q|
  # starting up the Prima GUI demo app
  |.__PACKAGE__.q|::run();
|]}

use PDL;

=head1 NAME

PDL::Demos::Prima - PDL demo for PDL::Graphics::Prima

=head1 SYNOPSIS

You can enjoy this demo in any number of ways. First, you can invoke the
demo from the command line by saying

 perl -MPDL::Demos::Prima

Second, you can invoke the demo from with the pdl shell by saying

 pdl> demo prima

Finally, all of the content is in the pod documentation, so you can simply
read this, though it won't be quite so interactive. :-)

 perldoc PDL::Demos::Prima
 podview PDL::Demos::Prima

=head1 DESCRIPTION

The documentation in this module is meant to give a short, hands-on
introduction to L<PDL::Graphics::Prima>, a plotting
library written on top of the L<Prima> GUI toolkit.

=cut

# Pull the pod apart into the following sort of array structure
# @demo_data = (
#   'Introduction' => $first_paragraph => $first_code,
#   'Introduction' => $second_paragraph => $second_code,
#     ...
#   'First steps'  => $first_paragraph => $first_code,
#     ...
# );
my (@demo_data, $curr_section, $curr_par, $curr_code);
while(my $line = <DATA>) {
	# Only =head2s in this documentation
	last if $line =~ /=head1/;
	if ($line =~ /^=head2 (.*)/) {
		$curr_section = $1;
	}
	elsif ($line =~ /^\n/) {
		if (defined $curr_par and defined $curr_code) {
			push @demo_data, [$curr_section, $curr_par, $curr_code];
			$curr_par = $curr_code = undef;
		}
	}
	elsif (not defined $curr_par) {
		$curr_par = $line;
	}
	elsif (not defined $curr_code and $line !~ /^\s/) {
		$curr_par .= $line;
	}
	elsif ($line =~ /^\s/) {
		# Accumulate code lines, stripping off the leading space
		$line =~ s/^\s//;
		$curr_code .= $line;
	}
}

# Add some extra content for Prima viewing only
unshift @demo_data, ['Introduction',
'This is the demo for L<PDL::Graphics::Prima>. Explanatory
text will appear here; code samples will appear below. Tip: you can modify and
re-run the code samples. When you are done, simply close the window.',
'### HEY, EDIT ME! ###
use Prima::MsgBox;
Prima::MsgBox::message( "Hello, this is the PDL::Graphics::Prima demo.", mb::Ok);'];

##################################
# The command that runs the demo #
##################################

# These are widgets I will need across multiple functions, so they are globals.
our %GUI;
sub run {
	setup_gui(\%GUI);
	$GUI{window}->bring_to_front;
	setup_slide(@{$demo_data[0]}, 0, \%GUI);
	# Run this sucker
	local $@;
	eval { $::application->go };
	$GUI{help_window}->close if defined $GUI{help_window} and $GUI{help_window}->alive;
}

sub setup_gui {
	my ($gui) = @_;
	# Note that by the time we reach here, $::application is defined.
	require PDL::Graphics::Prima::Simple;
	PDL::Graphics::Prima::Simple->import();
	require Prima::Application;
	Prima::Application->import();
	require Prima::Label;
	require Prima::PodView;
	require Prima::Buttons;
	require Prima::Utils;
	require Prima::Edit;

	my $current_slide = 0;

	# ---( Build the Demo Window )--- #

																	# Window
	$gui->{window} = Prima::Window->create(
		place => {
			relx => 0.15, relwidth => 0.7, relheight => 0.7, rely => 0.15,
			anchor => 'sw',
		},
		sizeMax => [600, 800],
		sizeMin => [600, 800],
		text => 'PDL::Graphics::Prima Demo',
		onDestroy => sub {
			require Prima::Utils;
			# Throw an exception after destruction is complete so that we
			# break out of the $::application->go loop.
			Prima::Utils::post(sub { die 'time to exit the event loop' });
		},
		onKeyUp => \&keypress_handler,
	);
	$gui->{window}->font->size(12);
																		# Title
	# ---( Build list of windows that we don't want to close )---
	my @dont_touch = $::application->get_widgets;

	my $title_height = 50;
	$gui->{section_title_label} = $gui->{window}->insert(Label =>
		place => {
			x => 0, relwidth => 1, anchor => 'sw',
			y => -$title_height, rely => 1, height => $title_height,
		},
		text => '',
		height => $title_height,
		alignment => ta::Center(),
		valignment => ta::Center(),
		backColor => cl::White(),
		font => {
			size => 24,
		},
		onKeyUp => \&keypress_handler,
	);
																	# Buttons
	my $button_height = 35;
	$gui->{prev_button} = $gui->{window}->insert(Button =>
		place => {
			x => 0, relwidth => 0.333, anchor => 'sw',
			y => 0, height => $button_height,
		},
		height => $button_height,
		text => 'Previous',
		enabled => 0,
		onClick => sub {
			$current_slide-- unless $current_slide == 0;
			setup_slide(@{$demo_data[$current_slide]}, _slide_posn($current_slide, scalar @demo_data), $gui);
		},
	);
	$gui->{run_button} = $gui->{window}->insert(Button =>
		place => {
			relx => 0.333, relwidth => 0.333, anchor => 'sw',
			y => 0, height => $button_height,
		},
		height => $button_height,
		text => 'Run',
		onClick => sub {
			# Clear out old windows
			for my $curr_window ($::application->get_widgets) {
				next if grep { $curr_window == $_ } @dont_touch
					or defined $gui->{help_window} and $curr_window == $gui->{help_window};
				$curr_window->destroy;
			}

			# Disable the buttons
			my $prev_state = $gui->{prev_button}->enabled;
			$gui->{prev_button}->enabled(0);
			$gui->{run_button}->enabled(0);
			my $next_state = $gui->{next_button}->enabled;
			$gui->{next_button}->enabled(0);

			# Run the eval
			eval 'no strict; no warnings; ' . $gui->{code_eval}->text;
			if ($@ and $@ !~ /time to exit the event loop/		) {
				warn $@;
				Prima::MsgBox::message($@);
			}

			$gui->{prev_button}->enabled($prev_state);
			$gui->{run_button}->enabled(1);
			$gui->{next_button}->enabled($next_state);
		},
	);
	$gui->{next_button} = $gui->{window}->insert(Button =>
		place => {
			relx => 0.666, relwidth => 0.333, anchor => 'sw',
			y => 0, height => $button_height,
		},
		height => $button_height,
		text => 'Next',
		onClick => sub {
			$current_slide++ unless $current_slide == @demo_data;
			setup_slide(@{$demo_data[$current_slide]}, _slide_posn($current_slide, scalar @demo_data), $gui);
		},
	);
																	# Text
	my $par_container = $gui->{window}->insert(Widget =>
		place => {
			x => 0, relwidth => 1, anchor => 'sw',
			rely => 0.6, relheight => 0.4, height => -$title_height-1,
		},
		backColor => cl::White(),
	);
	my $padding = 10;
	$gui->{text_pod} = $par_container->insert(PodView =>
		place => {
			x => $padding, relwidth => 1, width => -2*$padding,
			y => $padding, relheight => 1, height => -2*$padding - 15,
			anchor => 'sw',
		},
		# This Event does not appear to be documented!!! Beware!!!
		# Modify link clicking so that it opens the help window instead
		# of following the link.
		onLink => sub {
			my ($self, $link) = @_;
			# $link is a reference to the link that should be opened; deref
			$::application->open_help($$link);
			# Store the help window so we can close it on exit later
			$gui->{help_window} = $::application->get_active_window;
			# Bring the help window to the fore
			$::application->get_active_window->bring_to_front
				if $::application->get_active_window;
			# Clear the event so that it doesn't follow the link in this
			# renderer
			$self->clear_event;
		},
		backColor => cl::White(),
		borderWidth => 0,
		autoVScroll => 1,
		onKeyUp => \&keypress_handler,
	);

																		# Code
	my $code_container = $gui->{window}->insert(Widget =>
		place => {
			x => 0, relwidth => 1, anchor => 'sw',
			y => $button_height+1, relheight => 0.6, height => -$button_height-2,
		},
		backColor => cl::White(),
	);
	$gui->{code_eval} = $code_container->insert(Edit =>
		place => {
			x => $padding, relwidth => 1, width => -2*$padding,
			y => $padding, relheight => 1, height => -2*$padding,
			anchor => 'sw',
		},
		borderWidth => 0,
		backColor => cl::White(),
		tabIndent => 4,
		syntaxHilite => 1,
		wantTabs => 1,
		wantReturns => 1,
		wordWrap => 0,
		autoIndent => 1,
		cursorWrap => 1,
		font => { name => 'monospace', size => 12 },
	);
}

sub keypress_handler {
	my ($self, $code, $key, $mod) = @_;
	if ($key == kb::Down() or $key == kb::Right() or $key == kb::PgDn()) {
		$GUI{next_button}->notify('Click');
	}
	elsif ($key == kb::Up() or $key == kb::Left() or $key == kg::PgUp()) {
		$GUI{prev_button}->notify('Click');
	}
	else {
		$GUI{code_eval}->notify('KeyUp', $code, $key, $mod);
	}
}


#############################################################
# Function that transitions between paragraphs and sections #
#############################################################

# posn 0=first, 1=mid, 2=penultimate, 3=last
sub _slide_posn {
  my ($number, $total) = @_;
  return 0 if ($number//0) <= 0;
  return 2 if $number == $total - 1;
  return 3 if $number >= $total;
  1;
}
sub setup_slide {
	my ($section, $text, $code, $posn, $gui) = @_; # see above for posn
	if ($posn == 0) {
		$gui->{prev_button}->enabled(0);
	}
	else {
		$gui->{prev_button}->enabled(1);
	}
	if ($posn == 2) {
		$gui->{next_button}->enabled(1);
		$gui->{next_button}->text('Finish');
	}
	elsif ($posn == 3) {
		# Close the window
		$gui->{window}->notify('Destroy');
		return;
	}
	else {
		$gui->{next_button}->enabled(1);
		$gui->{next_button}->text('Next');
	}
	# Set the section title and code
	$gui->{section_title_label}->text($section);
	$gui->{code_eval}->text($code);
	# Load the pod
	$gui->{text_pod}->open_read;
	$gui->{text_pod}->read("=pod\n\n$text\n\n=cut");
	$gui->{text_pod}->close_read;
	# Run the demo
	$gui->{run_button}->notify('Click');
}

# This way, it can be invoked as "perl -MPDL::Demos::Prima" or as
# "perl path/to/Prima.pm"
if ($0 eq '-' or $0 eq __FILE__) {
	run;
	exit;
}

1;

__DATA__

=head2 use PDL::Graphics::Prima::Simple

To get started, you will want to use
L<PDL::Graphics::Prima::Simple>. This
module provides a set of friendly wrappers for simple, first-cut data
visualization. L<PDL::Graphics::Prima>, the underlying
library, is a general-purpose 2D plotting library built as a widget in the
L<Prima GUI toolkit|Prima>, but we don't need the full functionality for
the purposes of this demo.

 use PDL::Graphics::Prima::Simple;
 my $x = sequence(100)/10;
 line_plot($x, $x->sin);

=head2 More than just lines!

In addition to numerous ways to plot x/y data, you can also plot
distributions and images. The best run-down of the simple plotting routines
can be found in
L<the Synopsis for PDL::Graphics::Prima::Simple|PDL::Graphics::Prima::Simple/SYNOPSIS>.

 $distribution = grandom(100);
 hist_plot($distribution);
 
 $x = sequence(100)/10;
 cross_plot($x, $x->sin);
 
 $image = rvals(100, 100);
 matrix_plot($image);

=head2 Mouse Interaction

Plots allow for
L<mouse interaction|PDL::Graphics::Prima::Simple/"Interactive Features">,
herein referred to as twiddling. You can resize the window, zoom with the
scroll wheel, or click and drag the canvas around. There is also a
right-click zoom-rectangle, and a right-click context menu.

 hist_plot(grandom(100));
 
 # Run this, then try using your mouse

In your Perl scripts, and in the PDL shell for some operating systems and
some versions of L<Term::ReadLine>, twiddling will cause your script to pause
when you create a new plot. To resume your script or return execution to the
shell, either close the window or press 'q'.

 # If your PDL shell supports simultaneous
 # input and plot interaction, running this
 # should display both plots simultaneously:
 
 $x = sequence(100)/10;
 cross_plot($x, $x->sin);
 line_plot($x, $x->cos);

=head2 Multiple plots without blocking

The blocking behavior just discussed is due to what is called autotwiddling.
To turn this off, simply send a boolean false value to auto_twiddle. Then,
be sure to invoke twiddling when you're done creating your plots.

 auto_twiddle(0);
 hist_plot(grandom(100));
 matrix_plot(rvals(100, 100));
 twiddle();

Once turned off, autotwiddling will remain off until you turn it back on.

 # autotwiddling still off
 hist_plot(grandom(100));
 matrix_plot(rvals(100, 100));
 twiddle();

=head2 Adding a title and axis labels

Functions like 
L<hist_plot|PDL::Graphics::Prima::Simple/hist_plot>,
L<cross_plot|PDL::Graphics::Prima::Simple/cross_plot>, and
L<matrix_plot|PDL::Graphics::Prima::Simple/matrix_plot> actually create and
return plot objects which you can subsequently modify. For example,
adding a title and axis labels are pretty easy. For titles, you call the
L<title method on the plot object|PDL::Graphics::Prima/title>. For axis
labels, you call the
L<label method on the axis objects|PDL::Graphics::Prima::Axis/label>.

 # Make sure autotwiddling is off in your script
 auto_twiddle(0);
 
 # Build the plot
 my $x = sequence(100)/10;
 my $plot = line_plot($x, $x->sin);
 
 # Add the title and labels
 $plot->title('Harmonic Oscillator');
 $plot->x->label('Time [s]');
 $plot->y->label('Displacement [cm]');
 
 # Manually twiddle once everything is finished
 twiddle();

=head2 Saving to a file

L<PDL::Graphics::Prima::Simple> excels at user interaction, but you can save
your plots to a file using L<save_to_file|PDL::Graphics::Prima/save_to_file>
or L<save_to_postscript|PDL::Graphics::Prima/save_to_postscript> methods, or
by right-clicking and selecting the appropriate menu option.

 auto_twiddle(0);
 $x = sequence(100)/10;
 line_plot($x, $x->sin)->save_to_postscript;
 
 # You can supply a filename to the method if you like.
 # Also available is save_to_file, which saves to raster
 # file formats. Expect save_to_postscript to be merged
 # into save_to_file in the future.

=head2 Adding additional data to the plot

Once you have created a plot, you can
L<add additional data to it|PDL::Graphics::Prima/dataSets>. You
achieve this by adding a new
L<DataSet|PDL::Graphics::Prima::DataSet> with the data you want displayed.

 auto_twiddle(0);
 my $plot = hist_plot(grandom(100));
 
 # Add a Gaussian curve that "fits" the data
 use PDL::Constants qw(PI);
 my $fit_xs = zeroes(100)->xlinvals(-2, 2);
 my $fit_ys = exp(-$fit_xs**2 / 2) / sqrt(2*PI);
 $plot->dataSets->{fit_curve} = ds::Pair($fit_xs, $fit_ys);
 
 twiddle();

The default L<plot type|PDL::Graphics::Prima::PlotType/> for
L<pairwise data|PDL::Graphics::Prima::DataSet/Pair> is
L<Diamonds|PDL::Graphics::Prima::PlotType/ppair::Diamonds>. You can choose a
L<different pairwise plot type|PDL::Graphics::Prima::PlotType/Pairs>, or
even mix and match L<multiple pairwise plot types|PDL::Graphics::Prima::PlotType/SYNOPSIS>.

 auto_twiddle(0);
 my $plot = hist_plot(grandom(100));
 
 # Add a Gaussian curve that "fits" the data
 use PDL::Constants qw(PI);
 my $fit_xs = zeroes(200)->xlinvals(-5, 5);
 my $fit_ys = exp(-$fit_xs**2 / 2) / sqrt(2*PI);
 $plot->dataSets->{fit_curve} = ds::Pair($fit_xs, $fit_ys,
     # Use lines
     plotTypes => [
         ppair::Lines(
             # with a thickness of three pixels
             lineWidth => 3,
             # And the color red
             color => cl::LightRed,
         ),
         ppair::Diamonds,
     ],
 );
 
 twiddle();

=head2 The plot command

If you want to specify everything in one command, you can use the plot
function. This lets you put everything together that we've already discussed,
including multiple DataSets in a single command, title specification, and
x and y axis options.

 # Generate some data:
 my $xs = sequence(100)/10 + 0.1;
 my $ys = $xs->sin + $xs->grandom / 10;
 my $y_err = $ys->grandom/10;
 
 # Plot the data and the fit
 plot(
     -data => ds::Pair($xs, $ys,
         plotTypes => [
             ppair::Triangles(filled => 1),
             ppair::ErrorBars(y_err => $y_err),
         ],
     ),
     -fit  => ds::Func(\&PDL::sin,
         lineWidth => 3,
         color => cl::LightRed,
     ),
     -note => ds::Note(
         pnote::Text('Incoming Signal',
             x => 0.2,
             y => sin(0.2) . '-3em',
         ),
     ),
     title => 'Noisey Sine Wave',
     x => {
         label => 'Time [s]',
         scaling => sc::Log,
     },
     y => { label => 'Measurement [Amp]' },
 );

=head2 Enjoy PDL::Graphics::Prima!

I hope you've enjoyed the tour, and I hope you find
L<PDL::Graphics::Prima|PDL::Graphics::Prima/> to be a useful plotting tool!

 # Thanks!

=head1 AUTHOR

David Mertens C<dcmertens.perl@gmail.com>

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2013, David Mertens. All righs reserved.

This module is free software; you can redistribute it and/or modify it under the
same terms as Perl itself. See L<perlartistic>.

=cut
