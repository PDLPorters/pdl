use strict;
use warnings;

############################################################################
                         package PDL::Demos::Prima;
############################################################################

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
introduction to L<PDL::Graphics::Prima|PDL::Graphics::Prima/>, a plotting
library written on top of the L<Prima|Prima/> GUI toolkit.

=cut

##############################
# Check load status of Prima #
##############################

my $min_version = 0.13;
my $loaded_prima = eval {
	require PDL::Graphics::Prima;
	return 0 if $PDL::Graphics::Prima::VERSION < $min_version;
	require PDL::Graphics::Prima::Simple;
	PDL::Graphics::Prima::Simple->import();
	require Prima::Application;
	Prima::Application->import();
	1;
};

###########################################
# Pull the demo pod into a data structure #
###########################################

# Pull the pod apart into the following sort of array structure
# @demo = (
#   'Introduction' => $first_paragraph => $first_code,
#   'Introduction' => $second_paragraph => $second_code,
#     ...
#   'First steps'  => $first_paragraph => $first_code,
#     ...
# );

my (@demo, $curr_section, $curr_par, $curr_code);
my $curr_state = 'section_title';
while(my $line = <DATA>) {
	# Only =head2s in this documentation
	last if $line =~ /=head1/;
	if ($line =~ /^=head2 (.*)/) {
		# Add the current section's name and an empty arrayref
		$curr_section = $1;
	}
	elsif ($line =~ /^\n/) {
		if (defined $curr_par and defined $curr_code) {
			push @demo, $curr_section, $curr_par, $curr_code;
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
if ($loaded_prima) {
	unshift @demo, 'Introduction',
'This is the demo for L<PDL::Graphics::Prima|PDL::Graphics::Prima/>. Explanatory
text will appear here; code samples will appear below. Tip: you can modify and 
re-run the code samples. When you are done, simply close the window.',
'### HEY, EDIT ME! ###
use Prima::MsgBox;
Prima::MsgBox::message( "Hello, this is the PDL::Graphics::Prima demo.", mb::Ok);'
}

##################################
# The command that runs the demo #
##################################

# These are widgts I will need across multiple functions, so they are globals.
my ($section_title_label, $text_pod, $code_eval, $prev_button, $next_button,
	$run_button, $help_window, $window, $is_evaling);
sub run {
	
	# Make sure they have it. Otherwise, bail out.
	if (not $loaded_prima) {
		my $reason =
"I couldn't load the library, either because it's not installed on your
machine or it's broken.";
		$reason = 
"your version of PDL::Graphics::Prima (v$PDL::Graphics::Prima::VERSION) is out of date. This demo
requires at least v$min_version." if defined $loaded_prima;
		print <<SORRY;

Thanks for trying to learn more about PDL::Graphics::Prima. Unfortunately,
$reason

If you really want to get this working, the fastest way to get help is to
join the live chat on the PDL irc channel. If you have an IRC client, check
out

  irc.perl.org#pdl

If you don't have an IRC client, you can join the discussion via mibbit:

  http://www.mibbit.com/chat/?url=irc://irc.perl.org/pdl

If you would rather, you can send an email to the mailing list:

  http://pdl.perl.org/?page=mailing-lists

For more information about PDL::Graphics::Prima, check out

  http://p3rl.org/PDL::Graphics::Prima.


Thanks, and keep trying! I promise it's worth it.

SORRY
		return;
	}
	
	# Note that by the time we reach here, $::application is defined.
	require Prima::Label;
	require Prima::PodView;
	require Prima::Buttons;
	require Prima::Utils;
	require Prima::Edit;
	
	my $current_slide = 0;
	
	# ---( Build the Demo Window )--- #
	
																	# Window
	$window = Prima::Window->create(
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
	$window->font->size(12);
																		# Title
	# ---( Build list of windows that we don't want to close )---
	my @dont_touch = $::application->get_widgets;
	
	my $title_height = 50;
	$section_title_label = $window->insert(Label =>
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
	$prev_button = $window->insert(Button =>
		place => {
			x => 0, relwidth => 0.333, anchor => 'sw',
			y => 0, height => $button_height,
		},
		height => $button_height,
		text => 'Previous',
		enabled => 0,
		onClick => sub {
			$current_slide-- unless $current_slide == 0;
			setup_slide($current_slide);
		},
	);
	$run_button = $window->insert(Button =>
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
					or defined $help_window and $curr_window == $help_window;
				$curr_window->destroy;
			}
			
			# Disable the buttons
			my $prev_state = $prev_button->enabled;
			$prev_button->enabled(0);
			$run_button->enabled(0);
			my $next_state = $next_button->enabled;
			$next_button->enabled(0);
			
			# Run the eval
			eval 'no strict; no warnings; ' . $code_eval->text;
			if ($@ and $@ !~ /time to exit the event loop/		) {
				warn $@;
				Prima::MsgBox::message($@);
			}
			
			$prev_button->enabled($prev_state);
			$run_button->enabled(1);
			$next_button->enabled($next_state);
		},
	);
	$next_button = $window->insert(Button =>
		place => {
			relx => 0.666, relwidth => 0.333, anchor => 'sw',
			y => 0, height => $button_height,
		},
		height => $button_height,
		text => 'Next',
		onClick => sub {
			$current_slide++ unless $current_slide == @demo/3;
			setup_slide($current_slide);
		},
	);
																	# Text
	my $par_container = $window->insert(Widget =>
		place => {
			x => 0, relwidth => 1, anchor => 'sw',
			rely => 0.6, relheight => 0.4, height => -$title_height-1,
		},
		backColor => cl::White(),
	);
	my $padding = 10;
	$text_pod = $par_container->insert(PodView =>
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
			$help_window = $::application->get_active_window;
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
	my $code_container = $window->insert(Widget =>
		place => {
			x => 0, relwidth => 1, anchor => 'sw',
			y => $button_height+1, relheight => 0.6, height => -$button_height-2,
		},
		backColor => cl::White(),
	);
	$code_eval = $code_container->insert(Edit =>
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
	
	$window->bring_to_front;
	setup_slide(0);
	
	# Run this sucker
	local $@;
	eval { $::application->go };
	$help_window->close if defined $help_window and $help_window->alive;
}

sub keypress_handler {
	my ($self, $code, $key, $mod) = @_;
	if ($key == kb::Down() or $key == kb::Right() or $key == kb::PgDn()) {
		$next_button->notify('Click');
	}
	elsif ($key == kb::Up() or $key == kb::Left() or $key == kg::PgUp()) {
		$prev_button->notify('Click');
	}
	else {
		$code_eval->notify('KeyUp', $code, $key, $mod);
	}
}


#############################################################
# Function that transitions between paragraphs and sections #
#############################################################

sub setup_slide {
	my $number = shift;
	if ($number == 0) {
		$prev_button->enabled(0);
	}
	else {
		$prev_button->enabled(1);
	}
	if ($number == @demo/3 - 1) {
		$next_button->enabled(1);
		$next_button->text('Finish');
	}
	elsif ($number == @demo/3) {
		# Close the window
		$window->notify('Destroy');
		return;
	}
	else {
		$next_button->enabled(1);
		$next_button->text('Next');
	}
	
	$number *= 3;
	# Set the section title and code
	$section_title_label->text($demo[$number]);
	$code_eval->text($demo[$number+2]);
	
	# Load the pod
	$text_pod->open_read;
	$text_pod->read("=pod\n\n$demo[$number+1]\n\n=cut");
	$text_pod->close_read;
	
	# Run the demo
	$run_button->notify('Click');
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
L<PDL::Graphics::Prima::Simple|PDL::Graphics::Prima::Simple/>. This
module provides a set of friendly wrappers for simple, first-cut data
visualization. L<PDL::Graphics::Prima|PDL::Graphics::Prima/>, the underlying
library, is a general-purpose 2D plotting library built as a widget in the
L<Prima GUI toolkit|Prima/>, but we don't need the full functionality for
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
