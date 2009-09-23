package PDL::Graphics::OpenGL::Perl::OpenGL;

use OpenGL 0.58003 qw();
use PDL::Graphics::OpenGL::Perl::OpenGL;

use warnings;
use strict;

=head1 NAME

PDL::Graphics::OpenGL::Perl::OpenGL - PDL TriD OpenGL interface using POGL

=head1 VERSION

Version 0.01_07

=cut

our $VERSION = '0.01_07';


=head1 SYNOPSIS

This module provides the glue between the Perl
OpenGL functions and the API defined by the internal
PDL::Graphics::OpenGL one. It also supports any
miscellaneous OpenGL or GUI related functionality to
support PDL::Graphics::TriD refactoring.

You should be able to replace:

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

OpenGL::glpSetDebug(1);

*glpOpenWindow = \&OpenGL::glpOpenWindow;

*glpcOpenWindow = \&OpenGL::glpcOpenWindow;


=head2 TBD

=cut

package PDL::Graphics::OpenGL::OO;
use PDL::Options;
use strict;
my $debug;
#
# This is a list of all the fields of the opengl object and one could create a 
# psuedo hash style object but I want to use multiple inheritence with Tk...
#
#use fields qw/Display Window Context Options GL_Vendor GL_Version GL_Renderer/;

=head2 new($class,$options)

Returns a new OpenGL object with attributes specified in the options
field.  These attributes are:

=for ref

  x,y - the position of the upper left corner of the window (0,0)
  width,height - the width and height of the window in pixels (500,500)
  parent - the parent under which the new window should be opened (root)
  mask - the user interface mask (StructureNotifyMask)
  attributes - attributes to pass to glXChooseVisual

=cut

sub new {
  my($class_or_hash,$options) = @_;

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

  my $self =  OpenGL::glpcOpenWindow(
     $p->{x},$p->{y},$p->{width},$p->{height},
     $p->{parent},$p->{mask}, $p->{steal}, @{$p->{attributes}});

	if(ref($self) ne 'HASH'){
	  die "Could not create OpenGL window";
   }

#  psuedo-hash style see note above  
#  no strict 'refs';
#  my $self = bless [ \%{"$class\::FIELDS"}], $class;
#
  $self->{Options} = $p;
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

=head2 default_options

default options for object oriented methods

=cut

sub default_options{
   {  'x'     => 0,
      'y'     => 0,
      'width' => 500,
      'height'=> 500,
      'parent'=> 0,
      'mask'  => &OpenGL::StructureNotifyMask,
      'steal' => 0,
      'attributes' => [ &OpenGL::GLX_DOUBLEBUFFER, &OpenGL::GLX_RGBA ],
   }	
}

=head2 XPending()

OO interface to XPending

=cut

sub XPending {
   my($self) = @_;
   OpenGL::XPending($self->{Display});
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
   OpenGL::glpXNextEvent($self->{Display});
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

Bugs and feature requests may be submitted through the PDL sourceforge
project page at L<http://sourceforge.net/tracker/?group_id=612> .


=head1 SUPPORT

PDL uses a mailing list support model.  The Perldl mailing list
is the best for questions, problems, and feature discussions with
other PDL users and PDL developers.

To subscribe see the page at L<http://mailman.jach.hawaii.edu/mailman/listinfo/perldl>



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
