#!/usr/bin/perl 
#
#  PDL::Graphics::TriD::Tk - A Tk widget interface to the PDL::Graphics::TriD
#  visualization package:  $Revision$  
#
#  James P. Edwards
#  Instituto Nacional de Meteorologia
#  Brasilia, DF, Brasil
#  jedwards@inmet.gov.br  
#
#  This distribution is free software; you can
#  redistribute it and/or modify it under the same terms as Perl itself.
#  

=head1 NAME

PDL::Graphics::TriD::Tk - A Tk widget interface to the PDL::Graphics::TriD.

=head1 SYNOPSIS

=for usage

 #
 # Opens a Tk window with an embedded TriD window - that's all
 # see Demos/TkTriD_demo.pm for a better example
 # 
 use PDL;
 use PDL::Graphics::TriD;
 use PDL::Graphics::TriD::GL;
 use Tk;
 use PDL::Graphics::TriD::Tk;

 my $MW = MainWindow->new();
 my $TriDW = $MW->Tk( )->pack(-expand=>1, -fill=>'both');
 $TriDW->MainLoop;

=head1 DESCRIPTION

The widget is composed of a Frame and the Display device of the TriD output.
It inherits all of the attributes of a Tk Frame.  All of the events associated 
with this window are handled through Tk with the exception of the <expose> event
which must be handled by TriD because the Frame is never exposed.  
Default Mouse bindings, defined for button1 and button3, 
control TriD object orientation and size respectively.  

=cut

package PDL::Graphics::TriD::Tk;
use Tk;
use PDL::Core;
use PDL::Graphics::TriD;
use PDL::Graphics::OpenGL;
use strict;


@PDL::Graphics::TriD::Tk::ISA = qw(Tk::Frame);

$PDL::Graphics::TriD::Tk::verbose=0;

Tk::Widget->Construct('Tk');

#$PDL::Graphics::TriD::Tk::VERSION = '$Revision$ ' ;
#$PDL::Graphics::TriD::Tk::VERSION =~ s/\$Revision$\s*$/$1/;
#sub Version {return $PDL::Graphics::TriD::Tk::VERSION;}

=head1 FUNCTIONS

=head2 Populate

=for ref

Used for widget initialization by Tk, this function should never be called directly

=cut

sub Populate {
  my($TriD, $args) = @_;

  if(defined $PDL::Graphics::TriD::cur){
	 print "Current code limitations prevent TriD:Tk from being loaded after ";
    print "another TriD graphics window has been defined.  If you are running the ";
	 print "PDL demo package, please start it again and run this demo first.\n";
	 exit;
  }

  $args->{-height}=300 unless defined $args->{-height};
  $args->{-width}=300 unless defined $args->{-width};

  $TriD->SUPER::Populate($args);
  # This bind causes GL to be initialized after the 
  # Tk frame is ready to accept it
  $TriD->bind("<Configure>", [ \&GLinit ]);
  print "Populate complete\n" if($PDL::Graphics::TriD::Tk::verbose);
}

=head2 MainLoop

=for ref

Should be used in place of the Tk MainLoop.  Handles all of the Tk 
callbacks and calls the appropriate TriD display functions.  

=cut



sub MainLoop
{
  my ($self) = @_;
 
  unless ($Tk::inMainLoop)
  {
    local $Tk::inMainLoop = 1;
    while (Tk::MainWindow->Count)
    {
      DoOneEvent(Tk::DONT_WAIT());
      

      if(defined $self->{GLwin}){
	if( &XPending()){
	  my @e = &glpXNextEvent();
#	  if($e[0] == &ConfigureNotify) {
#	    print "CONFIGNOTIFE\n" if($PDL::Graphics::TriD::verbose);
#	    $self->reshape($e[1],$e[2]);
#	  }

	  $self->refresh();
	}
	my $job=shift(@{$self->{WorkQue}});
	if(defined $job){
	  my($cmd,@args) = @$job;
	  &{$cmd}(@args);
	}
      }
    }
  }
}

=head2 GLinit

=for ref

GLinit is called internally by a Configure callback in Populate.  This insures 
that the required Tk::Frame is initialized before the TriD::GL window that will go inside.

=cut

sub GLinit{
  my($self,@args) = @_;
  
  if(defined $self->{GLwin}){
#    print "OW= ",$self->width," OH= ",$self->height,"\n";
#    $self->update;
#    print "NW= ",$self->width," NH= ",$self->height,"\n";
	 $self->{GLwin}{_GLObject}->XResizeWindow($self->width ,$self->height);

    $self->{GLwin}->reshape($self->width,$self->height);
    $self->refresh();
  }else{
# width and height represent the largest size on my screen so that the
# graphics window always fills the frame.
    my $options={parent=> ${$self->WindowId},
                 width=> $self->width,
                 height=>$self->height};
    $options->{mask} = ( ExposureMask );

    $self->{GLwin} = PDL::Graphics::TriD::get_current_window($options);

    $self->{GLwin}->reshape($self->width,$self->height);

#
# This is an array for future expansion beyond the twiddle call.
# 
    $self->{WorkQue}= [];
    $self->refresh();

    $self->bind("<Button1-Motion>",[ \&buttonmotion, 1, Ev('x'),Ev('y')]);
    $self->bind("<Button2-Motion>",[ \&buttonmotion, 2, Ev('x'),Ev('y')]);
    $self->bind("<Button3-Motion>",[ \&buttonmotion, 3, Ev('x'),Ev('y')]);
  }

}

=head2 refresh

=for ref

refresh() causes a display event to be put at the top of the TriD work que.  
This should be called at the end of each user defined TriD::Tk callback. 

=cut

sub refresh{
  my($self) = @_;
  return unless defined $self->{GLwin};
# put a redraw command at the top of the work queue
  my $dcall=ref($self->{GLwin})."::display";
  unshift(@{$self->{WorkQue}}, [\&{$dcall},$self->{GLwin}]);
}

=head2 AUTOLOAD

=for ref 

Trys to find a subroutine in PDL::Graphics::TriD when it is 
not found in this package.  

=cut

#
#  This AUTOLOAD allows the PDL::Graphics::TriD::Tk object to act as the PDL::Graphics::TriD
#  object which it contains.  It seems slow and may not be a good idea.
#

sub AUTOLOAD {
  my ($self,@args)=@_;
  use vars qw($AUTOLOAD);
  my $sub = $AUTOLOAD;
  # get subroutine name

#  print "In AutoLoad $self $sub\n";
  if(defined($self->{GLwin})){
    $sub =~ s/.*:://;
    return($self->{GLwin}->$sub(@args));
  }
}




=head2 buttonmotion

=for ref

Default bindings for mousemotion with buttons 1 and 3

=cut



sub buttonmotion{
  my($self,$but,$x,$y)=@_;

  $but--;

  foreach my $vp (@{$self->viewports()}){
#    use Data::Dumper;        
#    my $out = Dumper($vp);
#    print "$out\n";
#    exit;
    next unless $vp->{Active};
	 next unless defined $vp->{EHandler}{Buttons}[$but];
	 $vp->{EHandler}{Buttons}[$but]->mouse_moved($vp->{EHandler}{X},
																$vp->{EHandler}{Y},
																$x,$y);
	 $vp->{EHandler}{X} = $x;
	 $vp->{EHandler}{Y} = $y;
  }
  
  $self->refresh();
}




=head1 Author

B<James P. Edwards, Instituto Nacional de Meteorologia Brasil>

jedwards@inmet.gov.br

=cut

1;

