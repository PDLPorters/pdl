package PDL::Graphics2D;

use Exporter 'import'; # gives you Exporter's import() method directly
@EXPORT = qw(imag2d imag2d_update twiddle);     # symbols to export on request
@EXPORT_OK = qw(imag2d imag2d_update twiddle);  # symbols to export on request

=head1 NAME

PDL::Graphics2D - An object oriented interface to PDL graphics

=head1 SYNOPSIS

 use PDL::Graphics2D;
 $win = PDL::Graphics2D->new(<Interface>, <Options>);

 $w = imag2d( $image, 'Title Here', ... );

=head1 DESCRIPTION

This is an umbrella class allowing for a simple interface to all plotting
routines in PDL. On its own it does not do any work it merely passes
information to the appropriate class. Ideally this should probably offer
a uniform interface to a variety of packages.

This requires a lot more work before it is useful I feel, but it can be
used already.

=head1 CONSTRUCTORS

=head2 new

=for ref

Create a 2-D graphics object with the requested interface type

=cut


{
  my %lookup=(
              'PGPLOT' => 'PDL::Graphics::PGPLOT::Window'
             );
  sub new {

    my $type=shift;
    my $interface=shift;

    #
    # Translate the interface name to the appropriate class name.
    #
    $interface=uc($interface);
    die "Interface $interface is not known!\n" if !exists($lookup{$interface});
    my $class = $lookup{$interface};
    eval "require $class;";
    return $class->new(@_);
  }
}

use strict;

my $debug = 0;

use PDL::Lite;
use PDL::NiceSlice;

#------------------------------------------------------------------------
# PDL constants used by imag2d
#------------------------------------------------------------------------
#
#   PDL_B
#   PDL_D
#   PDL_F
#   PDL_L
#   PDL_S
#   PDL_US
#
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# PDL methods used by imag2d
#------------------------------------------------------------------------
#
#   .=
#   dim
#   float
#   get_dataref
#   ndims
#   sever
#   type->symbol
#
#------------------------------------------------------------------------

use OpenGL qw( :all );

#------------------------------------------------------------------------
# opengl/glut constants used by imag2d
#------------------------------------------------------------------------
#
#   GLUT_ACTION_CONTINUE_EXECUTION
#   GLUT_ACTION_ON_WINDOW_CLOSE
#   GLUT_DOUBLE
#   GLUT_RGBA
#   GLUT_WINDOW_HEIGHT
#   GLUT_WINDOW_WIDTH
#
#   GL_COLOR_BUFFER_BIT
#   GL_FLOAT
#   GL_INT
#   GL_LUMINANCE
#   GL_LUMINANCE_ALPHA
#   GL_MODELVIEW
#   GL_PROJECTION
#   GL_RGB
#   GL_RGBA
#   GL_SHORT
#   GL_UNPACK_ALIGNMENT
#   GL_UNSIGNED_BYTE
#   GL_UNSIGNED_INT_8_8_8_8
#   GL_UNSIGNED_SHORT
#
#------------------------------------------------------------------------


#------------------------------------------------------------------------
# opengl/glu/glut routines used by imag2d
#------------------------------------------------------------------------
# 
#   OpenGL::done_glutInit
#
#   glutAddMenuEntry
#   glutAttachMenu
#   glutCreateMenu
#   glutCreateWindow
#   glutDestroyWindow
#   glutDisplayFunc
#   glutGet
#   glutGetWindow
#   glutInit
#   glutInitDisplayMode
#   glutInitWindowPosition
#   glutInitWindowSize
#   glutKeyboardFunc
#   glutLeaveMainLoop
#   glutMouseFunc
#   glutPostRedisplay
#   glutReshapeFunc
#   glutReshapeWindow
#   glutSetOption
#   glutSwapBuffers
#
#   glClear
#   glClearColor
#   glDrawPixels_s
#   glFlush
#   glLoadIdentity
#   glMatrixMode
#   glPixelStorei
#   glPixelZoom
#   glRasterPos2i
#   glViewport
#
#   gluOrtho2D
#
#------------------------------------------------------------------------


our $draw_overlay;

my $finished_glutInit = 0;
my $cur_fig_num = 0;
my $imag2d_keep_twiddling;
my $imag2d_is_twiddling;

my $show_overlay = 1;
our $is_paused = 0;
our $do_step = 0;
our $step_count = 1;
our $go_forward = 1;
our $go_backward = 0;

our @imag2d_list = ();

#------------------------------------------------------------------------
# glutMouseFunc callback
#------------------------------------------------------------------------
sub mouse_click {
   my ($button, $state, $x, $y) = @_;

   my $window_id = glutGetWindow();
   my $width = glutGet(GLUT_WINDOW_WIDTH);
   my $height = glutGet(GLUT_WINDOW_HEIGHT);
   my $img;

   # search for image corresponding to window
   foreach my $entry ( @imag2d_list ) {
      if ( $entry->{window_id} == $window_id ) {
         $img = $entry->{img};  # 2D piddle for now
         last;
      }
   }

   die "mouse_click: callback could not find image window\n" unless defined $img;

   # calculate zoom/aspect ratio factors
   my $glds = 0; $glds = 1 if $img->dim(0) < 5;  # hack, need verify consistency

   my $zoom_x = $width / $img->dim($glds+0);
   my $zoom_y = $height / $img->dim($glds+1);
   my $zoom = ($zoom_x < $zoom_y) ? $zoom_x : $zoom_y;

   # calculate the offset to the image use for centering
   my ($hshift, $vshift) = (0,0);

   if ( $zoom == $zoom_x ) {
      # shift down
      $vshift = ($height - $zoom * $img->dim($glds+1)) / 2.0;
   } else {
      # shift right
      $hshift = ($width - $zoom * $img->dim($glds+0)) / 2.0;
   }

   my ($im_x, $im_y);
   $im_x = sprintf "%.1f", ($x - $hshift) / $zoom;
   $im_y = sprintf "%.1f", ($y - $vshift) / $zoom;

   if ( $state
         and (-1 < $im_x)
         and (-1 < $im_y)
         and ($im_x < $img->dim($glds+0)+1)
         and ($im_y < $img->dim($glds+1)+1) ) {
      printf STDERR "b_%01d: pixel=(%d,%d), im pt=(%.1f,%.1f),", $button, $x, $y, $im_x, $im_y;
      printf STDERR " im val=%s, glds=$glds, winID=$window_id\n", $glds ? $img->(:,(int($im_x)),(int($im_y))) : $img->((int($im_x)),(int($im_y)));
   }
};

#------------------------------------------------------------------------
# glutReshapeFunc callback
#------------------------------------------------------------------------
sub resize_window {
   my ($width, $height) = @_;

   print STDERR "resize_window: call with new dims ($width, $height)\n" if $debug;

   my $window_id = glutGetWindow();
   my $img;

   return unless scalar(@imag2d_list);

   # search for image corresponding to window
   foreach my $entry ( @imag2d_list ) {
      if ( $entry->{window_id} == $window_id ) {
         $img = $entry->{img};  # 2D piddle for now
         last;
      }
   }

   die "resize_window: callback could not find image window\n" unless defined $img;

   # calculate zoom/aspect ratio factors
   my $glds = 0; $glds = 1 if $img->dim(0) < 5;  # hack, need verify consistency

   my $zoom_x = $width / $img->dim($glds+0);
   my $zoom_y = $height / $img->dim($glds+1);
   my $zoom = ($zoom_x < $zoom_y) ? $zoom_x : $zoom_y;

   glViewport( 0, 0, $width, $height );

   # set coordinate frame for graphics in window
   glMatrixMode( GL_PROJECTION );
   glLoadIdentity();

   gluOrtho2D( 0, $width, $height, 0 );

   glMatrixMode( GL_MODELVIEW );
   glLoadIdentity();

   # set zoom factors for image display
   glPixelZoom( $zoom, -$zoom );

   ## glutReshapeWindow($zoom*$img->dim($glds+0), $zoom*$img->dim($glds+1));

   # offset the image in the window to keep centered
   my ($hshift, $vshift) = (0,0);

   if ( $zoom == $zoom_x ) {
      # shift down
      $vshift = ($height - $zoom * $img->dim($glds+1)) / 2.0;
   } else {
      # shift right
      $hshift = ($width - $zoom * $img->dim($glds+0)) / 2.0;
   }

   glRasterPos2i( int($hshift), int($vshift) );

   my ($do_reshape) = 0;
   my ($new_width, $new_height) = ($zoom*$img->dim($glds+0),  $zoom*$img->dim($glds+1));

   # handle integer rounding problems in resize
   if (abs($new_width - $width ) <= 2) {
      $new_width = $width;
   } else {
      $do_reshape++;
   }
   if (abs($new_height - $height ) <= 2) {
      $new_height = $height;
   } else {
      $do_reshape++;
   }

   glutReshapeWindow($new_width,  $new_height) if $do_reshape;
};

#------------------------------------------------------------------------
# glutDisplayFunc callback
#------------------------------------------------------------------------
sub display_image {
   my $window_id = glutGetWindow();
   my $img;
   my ($gldrawformat, $gldrawtype, $glds);

   return unless scalar(@imag2d_list);

   # search for image corresponding to window
   foreach my $entry ( @imag2d_list ) {
      if ( $entry->{window_id} == $window_id ) {
         $img = $entry->{img};  # 2D piddle for now
         last;
      }
   }

   die "display_window: callback could not find image window\n" unless defined $img;

   # determine display pixel format to use
   if ($img->ndims > 2 && $img->dim(0) == 4) {
      $gldrawformat = GL_RGBA;
      $glds = 1;
   } elsif ($img->ndims > 2 && $img->dim(0) == 3) {
      $gldrawformat = GL_RGB;
      $glds = 1;
   } elsif ($img->ndims > 2 && $img->dim(0) == 2) {
      $gldrawformat = GL_LUMINANCE_ALPHA;
      $glds = 1;
   } elsif ($img->ndims > 2 && $img->dim(0) == 1) {
      $gldrawformat = GL_LUMINANCE;
      $glds = 1;
   } else {
      $gldrawformat = GL_LUMINANCE;
      $glds = 0;
   };

   # convert to float if double for display
   if ($img->type->symbol eq 'PDL_D') {         # clean up code
      $img = $img->float;
   }

   # determine display pixel type to use
   if ($img->type->symbol eq 'PDL_F') {
      $gldrawtype = GL_FLOAT;
   } elsif ($img->type->symbol eq 'PDL_B') {
      $gldrawtype = GL_UNSIGNED_BYTE;
   } elsif ($img->type->symbol eq 'PDL_S') {
      $gldrawtype = GL_SHORT;
   } elsif ($img->type->symbol eq 'PDL_US') {
      $gldrawtype = GL_UNSIGNED_SHORT;
   } elsif ($img->type->symbol eq 'PDL_L') {
      $gldrawtype = ( $gldrawformat == GL_RGBA ) ? GL_UNSIGNED_INT_8_8_8_8 : GL_INT;
   } else {
      die "display_image: unsupported data type '", $img->type->symbol, "' for image display\n";
   }

   my ($sizeX, $sizeY) = ($img->dim($glds+0), $img->dim($glds+1));
   # print STDERR "...  calculated image size is ($sizeX, $sizeY)\n";

   # display image
   glClear(GL_COLOR_BUFFER_BIT);
   # glRasterPos2i( 0, 0 );
   glDrawPixels_s( $sizeX, $sizeY, $gldrawformat, $gldrawtype, $img->get_dataref );

   &{$draw_overlay}($img, $sizeX, $sizeY) if $show_overlay and defined($draw_overlay);

   #draw_hough_lines($img, $sizeX, $sizeY);

   glutSwapBuffers();
   glFlush();
}

my $RELEASE=99;

#------------------------------------------------------------------------
# glutCreateMenu callback
#------------------------------------------------------------------------
sub ModeMenu {
   my $entry = shift;
   my $img;

   if ($entry == $RELEASE) {
      my ($window_id) = glutGetWindow();

      # search for image corresponding to window
      foreach my $listentry ( @imag2d_list ) {
         if ( $listentry->{window_id} == $window_id ) {
            $img = $listentry->{img};  # 2D piddle for now
            last;
         }
      }

      die "ModeMenu: callback could not find image window\n" unless defined $img;

      glutLeaveMainLoop();
      # glutDestroyWindow($window_id);

   } else {
      die "ModeMenu: illegal menu entry '$entry'\n";
   }
}

#------------------------------------------------------------------------
# glutKeyboardFunc callback
#------------------------------------------------------------------------
sub key_ops {
   my ($key, $x, $y) = @_;
   my $win_id = glutGetWindow();

   # handle keypress events (defaults first)
   # print STDERR "Got keypress for keypress=$key\n";

   # stop twiddling
   if ($key == ord('Q') or $key == ord('q')) {
      $imag2d_is_twiddling = 0;
      warn "Stop twiddling command, key '" . chr($key) . "', detected.\n";
      return;
   }

   # exit program
   if ($key == 27 or $key == 3) {          # ESC or Ctrl-C
      warn "Exit program command, key '" . (($key == 27) ? 'ESC' : 'Ctrl-C') . "', detected.\n";
      if (defined $PERLDL::TERM) {         # don't exit if in the perldl or pdl2 shell
         $imag2d_is_twiddling = 0;
         warn "PDL shell in use, stop twiddling instead of exit...\n";
         return;
      } else {
         exit; 
      }
   }

   # toggle overlay
   if ($key == ord('O') or $key == ord('o')) {
      $show_overlay = (($show_overlay) ? 0 : 1);
      warn "Toggle overlay command, key '" . chr($key) . "', detected.\n";
      return;
   }

   # lock windows sizes together
   if ($key == ord('L') or $key == ord('l')) {
      ## $lock_sizes = (($lock_sizes) ? 0 : 1);
      ## warn "Setting \$lock_sizes to $lock_sizes, (window=$win_id)\n";
      warn "Lock window sizes command, key '" . chr($key) . "', not implemented.\n";
      return;
   }

   # toggle image histogram equalization
   if ($key == ord('H') or $key == ord('h')) {
      ## $hist_equalize = (($hist_equalize) ? 0 : 1);
      ## warn "Setting \$hist_equalize to $hist_equalize\n";
      warn "Toggle image histogram equalization command, key '" . chr($key) . "', not implemented.\n";
      return;
   }

   # resize current window (last clicked?) to 1:1
   if ($key == ord('1')) {
   ##    resize_window(-1,-1);     # Special (w,h) args mean set zoom to 1
   ##    glutPostRedisplay();
      warn "Resize current window to 1:1 scale command, key '" . chr($key) . "', not implemented.\n";
      return;
   }
   
   # resize other image windows to this one
   if ($key == ord('=')) {
   ##    warn "Resize other images to this one not yet implemented, (window=$win_id)\n";
      warn "Resize other windows to this one command, key '" . chr($key) . "', not implemented.\n";
      return;
   }

   # pause/run with space bar
   if ($key == 32) {    # SPACE
      if ($is_paused) {
         $is_paused = 0;
      } else {
         $is_paused = 1;
         $step_count = 1;
         $do_step = 1;
      }
      # warn "Pause/Run command, key 'SPACE', detected\n";
      return;
   } 

   # toggle verbose output
   if ($key == ord('v') or $key == ord('V')) {
      ##    $be_verbose = (($be_verbose) ? 0 : 1);
      warn "Toggle verbose output command, key '" . chr($key) . "', not implemented.\n";
      return;
   }

   # change direction or step in direction
   if ($key == 46 or $key == 62) {          # . or >
      $go_forward = 1;
      $go_backward = 0;
      if ($is_paused) {
         $do_step = 1;
         $step_count = 1;
      } else {
         $step_count++;
         $step_count = 1 if $step_count == 0;
      }
      # warn "Change Direction/Step forward command, key '" . (($key == 46) ? '.' : '>') . "', detected.\n";
      return;
   };

   if ($key == 44 or $key == 60) { ;        # , or <
      $go_forward = 0;
      $go_backward = 1;
      if ($is_paused) {
         $do_step = 1;
         $step_count = -1;
      } else {
         $step_count--;
         $step_count = -1 if $step_count == 0;
      }

      # warn "Change Direction/Step backward command, key '" . (($key == 44) ? ',' : '<') . "', detected.\n";
      return;
   }

   warn "No handler for key " . chr($key) . ", (window=$win_id)\n";
}

#------------------------------------------------------------------------
# Create a new OpenGL context window for image display
#------------------------------------------------------------------------
sub display_new_window {
   my ($height, $width, $zoom, $name, $off_r, $off_c, $window_id) = @_;

   my ($window_width, $window_height);
   my ($zoom_x, $zoom_y);

   if ( $width <= 0 || $height <= 0 || $zoom == 0.0 )
   {
      die "display_new_window: invalid arguments!\n";
   }

   $window_width  = int($zoom*$width  + 0.5);
   $window_height = int($zoom*$height + 0.5);

   # compute zoom factors to make graphics overlay the image precisely
   $zoom_x = $window_width/$width;
   $zoom_y = $window_height/$height;

   # create display window
   if (! $finished_glutInit ) {
      glutInit() unless OpenGL::done_glutInit();
      glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE);
      glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE,GLUT_ACTION_CONTINUE_EXECUTION) if OpenGL::_have_freeglut();
      $finished_glutInit = 1;
   }
   glutInitWindowSize( $window_width, $window_height );
   glutInitWindowPosition( $off_r, $off_c );
   $window_id = glutCreateWindow( $name );

   # set some standard defaults
   glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
   glClearColor( 0.0, 0.0, 0.0, 0.0 );
   glViewport( 0, 0, $window_width, $window_height );

   # set coordinate frame for graphics in window
   glMatrixMode( GL_PROJECTION );
   glLoadIdentity();

   gluOrtho2D( 0, $width, $height, 0 );

   glMatrixMode( GL_MODELVIEW );
   glLoadIdentity();

   # set zoom factors for image display
   glPixelZoom( $zoom_x, -$zoom_y );

   # set origin for drawing images as the top-left corner of the window
   glRasterPos2i( 0, 0 );

   # success
   return $window_id;
};

#------------------------------------------------------------------------
# Display piddle as 2-D image in window using OpenGL
#------------------------------------------------------------------------

=head1 FUNCTIONS

=head2 imag2d

=for ref

Display a 2-D image in a figure window

imag2d() creates a plain FreeGLUT OpenGL window and displays
the input image with 1:1 aspect ratio for pixels.  The window
resize is constrained to the actual ratio of the image
dimensions.  The initial display size is currently a 200x200
window to prevent things from being too small by default.

The image to display can have dimensions ($c,$M,$N) where for
$c==4 the display is in GL_RGBA, for $c==3 the display is GL_RGB,
for $c==2 the display is GL_LUMINANCE_ALPHA, and for $c==1 or for
for dimensions ($M,$N) then the display is GL_LUMINANCE.

This routine does not yet thread but multiple images may be
viewed at the same time in separate windows by multiple
calls to imag2d().  TriD graphics visualization windows and the
imag2d() windows may be created and used independently.

NOTE: If you are twiddling a TriD window, the imag2d()
windows are active as well.  If you call twiddle()
the sub, only the imag2d() windows will update correctly.

=for usage

  $window_id = imag2d($image, $name, $zoom, $x_off, $y_off);
    
    creates a new image figure window from the input piddle
    with the given title, zoom factor, and position (if possible)
    
    $window_id - may be used to refer to the figure window
    
    $image - 2D image piddle with at least 2 or 3 dimensions
             e.g. [M,N], [1,M,N], [2,M,N], [3,M,N], [4,M,N]
    
    $name - the name to use for the figure window (optional)
    
    $zoom - desired (float) pixel zoom factor     (optional)
    
    ($x_off, $y_off) - desired window pixel position (optional)
                       with (0,0) as the top left pixel of the
                       display

=for example

  use PDL::Graphics2D;     # imports imag2d() and twiddle()

  $x = sequence(64,48,3);  # make test RGB image
  $x = $x->mv(2,0);        # color must be dim(0) with size [0..4]
  $x /= $x->max;           # pixel values in [0.0,1.0]
  $x = sin(10*$x);
  $w1 = imag2d($x);        # with parens...
  $w2 = imag2d $x->sqrt;   # or without
  $w3 = imag2d $x**2;


=head2 imag2d_update

=for ref

Update an existing imag2d window with new piddle data

=for usage

  $image = random(3,64,48)/2 + 0.25;  # random pixel image
  $win = imag2d($image);              # create original image display

  imag2d_update($win, $image->sequence/$image->nelem);  # update data

C<imag2d_update> allows one to update an C<imag2d> display window
by replacing the associated image data with new contents.  The
new image data must be the same type and shape as the previous.

Eventually, we would like to implement this via some sort of
dataflow that would be transparent to the user.

=cut

=head2 twiddle

=for ref

Enable GUI interaction with a FreeGLUT display window.  With an argument, it sets
the default value for the auto-twiddling state. C< 0 > will disable the automatic
twiddling and C< 1 >, or true, will enable twiddling.

=for usage

  twiddle();     # same as twiddle(undef)

    Runs the FreeGLUT event loop so window GUI operations
    such as resize, expose, mouse click,.. work

  twiddle(0);  # disables twiddle looping for next twiddle() call
  twiddle(1);  # re-enables default twiddle looping for next twiddle() call
    
=cut


sub imag2d {
   my ($img, $name, $zoom, $off_r, $off_c) = (undef,"Figure $cur_fig_num", undef, 0, 0);

   # need to add error checking here
   $img = (shift)->copy;
   $name  = shift if scalar(@_);
   $zoom  = shift if scalar(@_);
   $off_r = shift if scalar(@_);
   $off_c = shift if scalar(@_);

   my $window_id;
   my ($gldrawformat, $gldrawtype, $glds);

   # determine display pixel format and type to use
   if ($img->ndims > 2 && $img->dim(0) == 4) {
      $gldrawformat = GL_RGBA;
      $glds = 1;
   } elsif ($img->ndims > 2 && $img->dim(0) == 3) {
      $gldrawformat = GL_RGB;
      $glds = 1;
   } elsif ($img->ndims > 2 && $img->dim(0) == 2) {
      $gldrawformat = GL_LUMINANCE_ALPHA;
      $glds = 1;
   } elsif ($img->ndims > 2 && $img->dim(0) == 1) {
      $gldrawformat = GL_LUMINANCE;
      $glds = 1;
   } else {
      $gldrawformat = GL_LUMINANCE;
      $glds = 0;
   };

   # convert to float if double for display
   if ($img->type->symbol eq 'PDL_D') {         # clean up code
      $img = $img->float;
   }

   # determine display pixel type to use
   if ($img->type->symbol eq 'PDL_F') {
      $gldrawtype = GL_FLOAT;
   } elsif ($img->type->symbol eq 'PDL_B') {
      $gldrawtype = GL_UNSIGNED_BYTE;
   } elsif ($img->type->symbol eq 'PDL_S') {
      $gldrawtype = GL_SHORT;
   } elsif ($img->type->symbol eq 'PDL_US') {
      $gldrawtype = GL_UNSIGNED_SHORT;
   } elsif ($img->type->symbol eq 'PDL_L') {
      $gldrawtype = ( $gldrawformat == GL_RGBA ) ? GL_UNSIGNED_INT_8_8_8_8 : GL_INT;
   } else {
      die "display_image: unsupported data type '", $img->type->symbol, "' for image display\n";
   }

   # create display window
   my ($im_height, $im_width);
   $im_height = $img->dim($glds+1);
   $im_width  = $img->dim($glds+0);
   if ( !defined($zoom)  and ( $im_width < 200 or $im_height < 200 ) ) {
      # adjust zoom to make initial window bigger than 200px
      my $mindim = ($im_width < $im_height) ? $im_width : $im_height;
      my $zoomest = int( 200 / $mindim );
      $zoomest += 1 unless $zoomest == 200/$mindim;
      print STDERR "imag2d: estimated zoom factor is $zoomest\n" if $debug;
      $zoom = $zoomest;
   }
   $zoom = defined($zoom) ? $zoom : 1.0;

   print STDERR "imag2d: calling display_new_window( "
   . $img->dim($glds+1) . ", "
   . $img->dim($glds+0) . ", $zoom, $name, $off_r, $off_c )" if $debug;

   if ( ! defined( $window_id = display_new_window( $img->dim($glds+1), $img->dim($glds+0), $zoom, $name, $off_r, $off_c ) ) ) {
      print STDERR "imag2d: failure\n";
      return;
   }

   # add GLUT window id to title
   glutSetWindowTitle($name . ":  WinID $window_id");
   $cur_fig_num++;

   # set callback function for image display
   glutDisplayFunc ( \&display_image );

   # set callback function for keypress events
   glutKeyboardFunc ( \&key_ops );

   # set callback for mouse clicks 
   glutMouseFunc( \&mouse_click );

   # set callback for image window resize
   glutReshapeFunc( \&resize_window );

   glutCreateMenu( \&ModeMenu );
   glutAddMenuEntry( "End MainLoop", $RELEASE );
   glutAttachMenu(GLUT_RIGHT_BUTTON);

   # add image and window to list
   push @imag2d_list, { window_id => $window_id, img => $img };

   # set callback for image window close
   glutCloseFunc( \&close_imag2d_window );

   # success
   glRasterPos2i( 0, 0 );
   glDrawPixels_s( $img->dim($glds+0), $img->dim($glds+1), $gldrawformat, $gldrawtype,         $img->get_dataref );
   glFlush();

   # we don't twiddle if in PDL shell and glutRunning is on
   {
      no warnings 'once';
      twiddle() unless defined $PERLDL::TERM and ref $Term::ReadLine::toloop;
   }

   return $window_id;
}

#------------------------------------------------------------------------
# Update imag2d() window image data
#------------------------------------------------------------------------
sub imag2d_update {
   my ($win_id, $image) = @_;
   my $img;

   return unless scalar(@imag2d_list);

   # search for image corresponding to window
   foreach my $entry ( @imag2d_list ) {
      if ( $entry->{window_id} == $win_id ) {
         $img = $entry->{img};  # 2D piddle for now
         last;
      }
   }

   die "imag2d_update: callback could not find image window\n" unless defined $img;

   # update display window
   # TODO: do we need to save and restore the current window?
   # For now: calling imag2d_update makes that window current
   glutSetWindow($win_id);

   $img .= $image->sever;
   glutPostRedisplay();

   # update display but don't force twiddle()
   glutMainLoopEvent();

   return $win_id;
}

#------------------------------------------------------------------------
# Close a specific imag2d window
#------------------------------------------------------------------------
sub close_imag2d_window {

   my $win_id = glutGetWindow();

   if ( ! scalar(@imag2d_list) ) {
      $imag2d_is_twiddling = 0;
      return;
   }

   # search for image corresponding to window
   my ($entry, $found_it);
   foreach $entry ( @imag2d_list ) {
      if ($entry->{window_id} == $win_id) {
         $found_it = 1;
         last;
      }
   }

   print STDERR "close_imag2d_window: started with  " . scalar(@imag2d_list) . " windows.\n" if $debug;
   if ($found_it) {
      @imag2d_list = grep { $_->{window_id} != $win_id } @imag2d_list;
   } else {
      warn "close_imag2d_window: could not find open window\n";
   }
   print STDERR "close_imag2d_window: finished with " . scalar(@imag2d_list) . " windows.\n" if $debug;
}

#------------------------------------------------------------------------
# Close all imag2d windows
#------------------------------------------------------------------------
sub close_imag2d {

   return unless scalar(@imag2d_list);

   # process all image windows
   foreach my $entry ( @imag2d_list ) {
      glutDestroyWindow($entry->{window_id});
   }

   @imag2d_list = ();
}

#------------------------------------------------------------------------
# Simple twiddle for perldl (use [qQ] to exit)
#------------------------------------------------------------------------
sub twiddle {
   my ($keeptwiddling) = @_;

   if (defined $keeptwiddling) {
      $imag2d_keep_twiddling = $keeptwiddling;
      return;
   }

   $imag2d_is_twiddling = (defined $imag2d_keep_twiddling) ? $imag2d_keep_twiddling : 1;

   if ( $imag2d_is_twiddling ) {
      print STDERR "Type Q or q to stop twiddling...\n";
      while ($imag2d_is_twiddling && scalar(@imag2d_list)) {
         glutMainLoopEvent();
      }
      print STDERR "Stopped twiddle-ing!\n";
   }
   glutMainLoopEvent();
}

#------------------------------------------------------------------------
# Threaded image display as tiles (code from PDL::Graphics::TriD::Image)
#------------------------------------------------------------------------

# N-D piddle -> 2-D
sub flatten {
   my ($this,$bin_align) = @_;

   my @dims = $this->dims;
   my $imdim0 = shift @dims; # get rid of the '3'

   my $xd = $dims[0]; my $yd = $dims[1];
   my $xdr = $xd; my $ydr = $yd;

   # Calculate the whole width of the image.
   my $ind = 0;
   my $xm = 0; my $ym = 0;
   for (@dims[2..$#dims]) {
      if ($ind % 2 == 0) {
         $xd ++; # = $dims[$ind-2];
         $xd *= $_;
         $xdr ++;
         $xdr *= $_;
         $xm++;
      } else {
         $yd ++; # = $dims[$ind-2];
         $yd *= $_;
         $ydr ++;
         $ydr *= $_;
         $ym++;
      }
      $ind++;
   }
   $xd -= $xm; $yd -= $ym;

   # R because the final texture must be 2**x-aligned ;(
   my ($txd ,$tyd, $xxd, $yyd);
   if ($bin_align) {
      for ($txd = 0; $txd < 12 and 2**$txd < $xdr; $txd++) {};
      for ($tyd = 0; $tyd < 12 and 2**$tyd < $ydr; $tyd++) {};
      $txd = 2**$txd; $tyd = 2**$tyd;
      $xxd = ($xdr > $txd ? $xdr : $txd);
      $yyd = ($ydr > $tyd ? $ydr : $tyd);
   } else {
      $xxd=$txd=$xdr; $yyd=$tyd=$ydr;
   }

   my $p = PDL->zeroes(PDL::float(),$imdim0,$xxd,$yyd);

   # # object is PDL not PDL::Graphics::TriD::Image
   # if(defined $this->{Opts}{Bg}) {
   #     $p .= $this->{Opts}{Bg};
   # }

   # print "MKFOOP\n";
   my $foop = $p->slice(":,0:".($xdr-1).",0:".($ydr-1));

   $ind = $#dims;
   my $firstx = 1;
   my $firsty = 1;
   my $spi;
   for (@dims[reverse(2..$#dims)]) {
      $foop->make_physdims();
      # print "FOOP: \n"; $foop->dump;
      if ($ind % 2 == 0) {
         $spi = $foop->getdim(1)/$_;
         $foop = $foop->splitdim(1,$spi)->slice(":,0:-2")->mv(2,3);
      } else {
         $spi = $foop->getdim(2)/$_;
         $foop = $foop->splitdim(2,$spi)->slice(":,:,0:-2");
      }
      # print "IND+\n";
      $ind++; # Just to keep even/odd correct
   }
   # $foop->dump;
   print "ASSGNFOOP!\n" if $PDL::debug;

   $foop .= $this->{Im};
   # print "P: $p\n";
   return wantarray() ? ($p,$xd,$yd,$txd,$tyd) : $p;
}

sub toimage {
   # initially very simple implementation
   my ($this) = @_;
   return $this->flatten(0);
}

1;

