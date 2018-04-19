package PDL::Graphics::OpenGL::Perl::OpenGL;

BEGIN {
   use PDL::Config;
   if ($PDL::Config{USE_POGL}) {
      eval "use OpenGL $PDL::Config{POGL_VERSION} qw()";
      use OpenGL::Config;
   }
}

BEGIN {
   eval 'OpenGL::ConfigureNotify()';
   if ($@) {
      # Set up some X11 and GLX constants for fake XEvent emulation
      {
         no warnings 'redefine';
         eval "sub OpenGL::GLX_DOUBLEBUFFER    () { 5 }";
         eval "sub OpenGL::GLX_RGBA            () { 4 }";
         eval "sub OpenGL::GLX_RED_SIZE        () { 8 }";
         eval "sub OpenGL::GLX_GREEN_SIZE      () { 9 }";
         eval "sub OpenGL::GLX_BLUE_SIZE       () { 10 }";
         eval "sub OpenGL::GLX_DEPTH_SIZE      () { 12 }";
         eval "sub OpenGL::KeyPressMask        () { (1<<0 ) }";
         eval "sub OpenGL::KeyReleaseMask      () { (1<<1 ) }";
         eval "sub OpenGL::ButtonPressMask     () { (1<<2 ) }";
         eval "sub OpenGL::ButtonReleaseMask   () { (1<<3 ) }";
         eval "sub OpenGL::PointerMotionMask   () { (1<<6 ) }";
         eval "sub OpenGL::Button1Mask         () { (1<<8 ) }";
         eval "sub OpenGL::Button2Mask         () { (1<<9 ) }";
         eval "sub OpenGL::Button3Mask         () { (1<<10) }";
         eval "sub OpenGL::Button4Mask         () { (1<<11) }";  # scroll wheel
         eval "sub OpenGL::Button5Mask         () { (1<<12) }";  # scroll wheel
         eval "sub OpenGL::ButtonMotionMask    () { (1<<13) }";
         eval "sub OpenGL::ExposureMask        () { (1<<15) }";
         eval "sub OpenGL::StructureNotifyMask    { (1<<17) }";
         eval "sub OpenGL::KeyPress            () { 2 }";
         eval "sub OpenGL::KeyRelease          () { 3 }";
         eval "sub OpenGL::ButtonPress         () { 4 }";
         eval "sub OpenGL::ButtonRelease       () { 5 }";
         eval "sub OpenGL::MotionNotify        () { 6 }";
         eval "sub OpenGL::Expose              () { 12 }";
         eval "sub OpenGL::GraphicsExpose      () { 13 }";
         eval "sub OpenGL::NoExpose            () { 14 }";
         eval "sub OpenGL::VisibilityNotify    () { 15 }";
         eval "sub OpenGL::ConfigureNotify     () { 22 }";
      }
   }
}

use warnings;
use strict;

=head1 NAME

PDL::Graphics::OpenGL::Perl::OpenGL - PDL TriD OpenGL interface using POGL

=head1 VERSION

Version 0.01_10

=cut

our $VERSION = '0.01_10';
$VERSION = eval $VERSION;


=head1 SYNOPSIS

This module provides the glue between the Perl
OpenGL functions and the API defined by the internal
PDL::Graphics::OpenGL one. It also supports any
miscellaneous OpenGL or GUI related functionality to
support PDL::Graphics::TriD refactoring.

You should eventually be able to replace:

    use PDL::Graphics::OpenGL
by
    use PDL::Graphics::OpenGL::Perl::OpenGL;

This module also includes support for FreeGLUT and
GLUT instead of X11+GLX as mechanism for creating
windows and graphics contexts.

=head1 EXPORT

See the documentation for the OpenGL module.
More details to follow as the refactored TriD module
interface and build environment matures

=head1 FUNCTIONS

=head2 TBD

=cut

*glpOpenWindow = \&OpenGL::glpOpenWindow;

*glpcOpenWindow = \&OpenGL::glpcOpenWindow;


=head2 TBD

=cut

package PDL::Graphics::OpenGL::OO;
use PDL::Graphics::TriD::Window qw();
use PDL::Options;
use strict;
my $debug = 0;
my (@fakeXEvents) = ();
my (@winObjects) = ();
#
# This is a list of all the fields of the opengl object
#
#use fields qw/Display Window Context Options GL_Vendor GL_Version GL_Renderer/;

=head2 new($class,$options,[$window_type])

Returns a new OpenGL object with attributes specified in the options
field, and of the 3d window type, if specified.  These attributes are:

=for ref

  x,y - the position of the upper left corner of the window (0,0)
  width,height - the width and height of the window in pixels (500,500)
  parent - the parent under which the new window should be opened (root)
  mask - the user interface mask (StructureNotifyMask)
  attributes - attributes to pass to glXChooseVisual

Allowed 3d window types, case insensitive, are:

=for ref

  glut - use Perl OpenGL bindings and GLUT windows (no Tk)
  x11  - use Perl OpenGL (POGL) bindings with X11 (disabled)

=cut

sub new {
   my($class_or_hash,$options,$window_type) = @_;

   my $isref = ref($class_or_hash);  
   my $p;
#  OpenGL::glpSetDebug(1);

   if($isref and defined $class_or_hash->{Options}){
      $p = $class_or_hash->{Options};
   }else{
      my $opt = new PDL::Options(default_options());
      $opt->incremental(1);
      $opt->options($options) if(defined $options);
      $p = $opt->options;
   }

   # Use GLUT windows and event handling as the TriD default
   $window_type ||= $PDL::Config{POGL_WINDOW_TYPE};
   # $window_type ||= 'x11';       # use X11 default until glut code is ready

   my $self;
   if ( $window_type =~ /x11/i ) {       # X11 windows
      print STDERR "Creating X11 OO window\n" if $debug;
      $self =  OpenGL::glpcOpenWindow(
         $p->{x},$p->{y},$p->{width},$p->{height},
         $p->{parent},$p->{mask}, $p->{steal}, @{$p->{attributes}});
   } else {                              # GLUT or FreeGLUT windows
      print STDERR "Creating GLUT OO window\n" if $debug;
      OpenGL::glutInit() unless OpenGL::done_glutInit();        # make sure glut is initialized
      OpenGL::glutInitWindowPosition( $p->{x}, $p->{y} );
      OpenGL::glutInitWindowSize( $p->{width}, $p->{height} );      
      OpenGL::glutInitDisplayMode( OpenGL::GLUT_RGBA() | OpenGL::GLUT_DOUBLE() | OpenGL::GLUT_DEPTH() );        # hardwire for now
      if ($^O ne 'MSWin32' and not $OpenGL::Config->{DEFINE} =~ /-DHAVE_W32API/) { # skip these MODE checks on win32, they don't work
         if (not OpenGL::glutGet(OpenGL::GLUT_DISPLAY_MODE_POSSIBLE()))
         {
            warn "glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH | GLUT_ALPHA) not possible";
            warn "...trying without GLUT_ALPHA";
            # try without GLUT_ALPHA
            OpenGL::glutInitDisplayMode( OpenGL::GLUT_RGBA() | OpenGL::GLUT_DOUBLE() | OpenGL::GLUT_DEPTH() );
            if ( not OpenGL::glutGet( OpenGL::GLUT_DISPLAY_MODE_POSSIBLE() ) )
            {
               die "display mode not possible";
            }
         }
      }

      my($glutwin) = OpenGL::glutCreateWindow( "GLUT TriD" );
      OpenGL::glutSetWindowTitle("GLUT TriD #$glutwin");        # add GLUT window id to title

      $self = { 'glutwindow' => $glutwin, 'xevents' => \@fakeXEvents, 'winobjects' => \@winObjects };

      OpenGL::glutReshapeFunc( \&_pdl_fake_ConfigureNotify );
      OpenGL::glutCloseFunc( \&_pdl_fake_exit_handler );
      OpenGL::glutKeyboardFunc( \&_pdl_fake_KeyPress );
      OpenGL::glutMouseFunc( \&_pdl_fake_button_event );
      OpenGL::glutMotionFunc( \&_pdl_fake_MotionNotify );
      OpenGL::glutDisplayFunc( \&_pdl_display_wrapper );

      OpenGL::glutSetOption(OpenGL::GLUT_ACTION_ON_WINDOW_CLOSE(), OpenGL::GLUT_ACTION_GLUTMAINLOOP_RETURNS()) if OpenGL::_have_freeglut();

      OpenGL::glutMainLoopEvent();       # pump event loop so window appears
   }
   if(ref($self) ne 'HASH'){
      die "Could not create OpenGL window";
   }

#  psuedo-hash style see note above  
#  no strict 'refs';
#  my $self = bless [ \%{"$class\::FIELDS"}], $class;
   #
   $self->{Options} = $p;
   $self->{window_type} = $window_type;
   if($isref){
      if(defined($class_or_hash->{Options})){
         return bless $self,ref($class_or_hash);
      }else{
         foreach(keys %$self){
            $class_or_hash->{$_} = $self->{$_};
         }
         return $class_or_hash;
      }
   }
   bless $self,$class_or_hash;
}

=head2 default GLUT callbacks

These routines are set as the default GLUT callbacks for when GLUT windows
are used for PDL/POGL.  Their only function at the moment is to drive an
fake XEvent queue to feed the existing TriD GUI controls.  At some point,
the X11 stuff will the deprecated and we can rewrite this more cleanly.

=cut

sub _pdl_display_wrapper {
   my ($win) = OpenGL::glutGetWindow();
   if ( defined($win) and defined($winObjects[$win]) ) {
      $winObjects[$win]->display();
   }
}

sub _pdl_fake_exit_handler {
   my ($win) = shift;
   print "_pdl_fake_exit_handler: clicked for window $win\n" if $debug;
   # Need to clean up better and exit/transition cleanly
}

sub _pdl_fake_ConfigureNotify {
   print "_pdl_fake_ConfigureNotify: got (@_)\n" if $debug;
   OpenGL::glutPostRedisplay();
   push @fakeXEvents, [ 22, @_ ];
}

sub _pdl_fake_KeyPress {
   print "_pdl_fake_KeyPress: got (@_)\n" if $debug;
   push @fakeXEvents, [ 2, chr($_[0]) ];
}

{
   my @button_to_mask = (1<<8, 1<<9, 1<<10, 1<<11, 1<<12);
   my $fake_mouse_state = 16;  # default have EnterWindowMask set;
   my $last_fake_mouse_state;

   sub _pdl_fake_button_event {
      print "_pdl_fake_button_event: got (@_)\n" if $debug;
      $last_fake_mouse_state = $fake_mouse_state;
      if ( $_[1] == 0 ) {       # a press
         $fake_mouse_state |= $button_to_mask[$_[0]];
         push @fakeXEvents, [ 4, $_[0]+1, @_[2,3], -1, -1, $last_fake_mouse_state ];
      } elsif ( $_[1] == 1 ) {  # a release
         $fake_mouse_state &= ~$button_to_mask[$_[0]];
         push @fakeXEvents, [ 5, $_[0]+1 , @_[2,3], -1, -1, $last_fake_mouse_state ];
      } else {
         die "ERROR: _pdl_fake_button_event got unexpected value!";
      }
   }

   sub _pdl_fake_MotionNotify {
      print "_pdl_fake_MotionNotify: got (@_)\n" if $debug;
      push @fakeXEvents, [ 6, $fake_mouse_state, @_ ];
   }

}

=head2 default_options

default options for object oriented methods

=cut

sub default_options{
   {  'x'     => 0,
      'y'     => 0,
      'width' => 500,
      'height'=> 500,
      'parent'=> 0,
      'mask'  => eval '&OpenGL::StructureNotifyMask',
      'steal' => 0,
      'attributes' => eval '[ &OpenGL::GLX_DOUBLEBUFFER, &OpenGL::GLX_RGBA ]',
   }	
}


=head2 XPending()

OO interface to XPending

=cut

sub XPending {
   my($self) = @_;
   if ( $self->{window_type} eq 'glut' ) {
      # monitor state of @fakeXEvents, return number on queue
      print STDERR "OO::XPending: have " .  scalar( @{$self->{xevents}} ) . " xevents\n" if $debug > 1;
      scalar( @{$self->{xevents}} );
   } else {
      OpenGL::XPending($self->{Display});
   }
}


=head2 XResizeWindow(x,y)

OO interface to XResizeWindow

=cut

sub XResizeWindow {
  my($self,$x,$y) = @_;
  OpenGL::glpResizeWindow($x,$y,$self->{Window},$self->{Display});
}


=head2 glpXNextEvent()

OO interface to glpXNextEvent

=cut


sub glpXNextEvent {
   my($self) = @_;
   if ( $self->{window_type} eq 'glut' ) {
      while ( !scalar( @{$self->{xevents}} ) ) {
         # If no events, we keep pumping the event loop
         OpenGL::glutMainLoopEvent();
      }
      # Extract first event from fake event queue and return
      return @{ shift @{$self->{xevents}} };
   } else {
      return OpenGL::glpXNextEvent($self->{Display});
   }
}


=head2 glpRasterFont()

OO interface to the glpRasterFont function

=cut

sub glpRasterFont{
   my($this,@args) = @_;
   OpenGL::glpRasterFont($args[0],$args[1],$args[2],$this->{Display});
}


=head2 AUTOLOAD

If the function is not prototyped in OO we assume there is
no explicit mention of the three identifying parameters (Display, Window, Context)
and try to load the OpenGL function.

=cut

sub AUTOLOAD {
  my($self,@args) = @_;
  use vars qw($AUTOLOAD);
  my $sub = $AUTOLOAD; 
  return if($sub =~ /DESTROY/);
  $sub =~ s/.*:://;
  $sub = "OpenGL::$sub";
  if(defined $debug){
    print "In AUTOLOAD: $sub at ",__FILE__," line ",__LINE__,".\n";
  }
  no strict 'refs';
  return(&{$sub}(@args));
}


=head2 glXSwapBuffers

OO interface to the glXSwapBuffers function

=cut

sub glXSwapBuffers {
	my($this,@args) = @_;
	OpenGL::glXSwapBuffers($this->{Window},$this->{Display});  # Notice win and display reversed [sic]
}


=head1 AUTHOR

Chris Marshall, C<< <devel dot chm dot 01 at gmail.com> >>

=head1 BUGS

Bugs and feature requests may be submitted through the PDL GitHub
project page at L<https://github.com/PDLPorters/pdl/issues> .


=head1 SUPPORT

PDL uses a mailing list support model.  The Perldl mailing list
is the best for questions, problems, and feature discussions with
other PDL users and PDL developers.

To subscribe see the page at L<http://pdl.perl.org/?page=mailing-lists>



=head1 ACKNOWLEDGEMENTS

TBD including PDL TriD developers and POGL developers...thanks to all.

=head1 COPYRIGHT & LICENSE

Copyright 2009 Chris Marshall.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of PDL::Graphics::OpenGL::Perl::OpenGL
