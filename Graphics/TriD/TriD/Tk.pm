#!/usr/bin/perl 
#
#  Tk::TriD - a Tk widget interface to the PDL::Graphics::TriD 
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

=head1 DESCRIPTION

The widget is composed of a Frame and the Display device of the TriD output.

=head1 Author

B<James P. Edwards, Instituto Nacional de Meteorologia>

jedwards@inmet.gov.br

=cut

package PDL::Graphics::TriD::Tk;
use Tk;
use PDL::Core;
use PDL::Graphics::TriD;
use PDL::Graphics::OpenGL;
use strict;
#use Data::Dumper;

@PDL::Graphics::TriD::Tk::ISA = qw(Tk::Frame);

$PDL::Graphics::TriD::Tk::verbose=0;

Tk::Widget->Construct('Tk');

#$PDL::Graphics::TriD::Tk::VERSION = '$Revision$ ' ;
#$PDL::Graphics::TriD::Tk::VERSION =~ s/\$Revision$\s*$/$1/;
#sub Version {return $PDL::Graphics::TriD::Tk::VERSION;}

sub Populate {
  my($TriD, $args) = @_;
  $TriD->SUPER::Populate($args);
  $TriD->bind("<Configure>", [ \&GLinit ]);
  print "Populate complete\n" if($PDL::Graphics::TriD::Tk::verbose);
  $TriD->parent->bind("<Expose>",sub{print "Here is an MW Expose\n"; $TriD->refresh()});
  $TriD->bind("<Expose>",sub{print "Here is an Trid Expose\n"; $TriD->refresh()});
}


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
	my $job=shift(@{$self->{WorkQue}});
	if(defined $job){
	  my($cmd,@args) = @$job;
	  &{$cmd}(@args);
	}
      }
    }
  }
}



sub GLinit{
  my($self,@args) = @_;
  
  if(defined $self->{GLwin}){
    $self->{GLwin}->reshape($self->width,$self->height);
    $self->refresh();
  }else{
# width and height represent the largest size on my screen so that the
# graphics window always fills the frame.
    my $options={parent=> ${$self->WindowId},
                 width=> $self->width,
                 height=>$self->height};
    $options->{mask} = ( NoEventMask );

    $self->{GLwin} = PDL::Graphics::TriD::get_current_window($options);

    $self->{GLwin}->clear_viewports();

    $self->{Viewport}[0]=$self->{GLwin}->new_viewport(0,0,1,1);

    $self->{Viewport}[0]->clear_objects();

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


sub refresh{
  my($self) = @_;
  return unless defined $self->{GLwin};
# put a redraw command at the top of the work queue
  my $dcall=ref($self->{GLwin})."::display";
  unshift(@{$self->{WorkQue}}, [\&{$dcall},$self->{GLwin}]);
}



#
#  This AUTOLOAD allows the PDL::Graphics::TriD::Tk object to act as the PDL::Graphics::TriD
#  object which it contains.  It seems slow and may not be a good idea.
#

sub AUTOLOAD {
  my ($self,@args)=@_;
  use vars qw($AUTOLOAD);
  my $sub = $AUTOLOAD;
  # get subroutine name

  print "In AutoLoad $self $sub\n";
  if(defined($self->{GLwin})){
    $sub =~ s/.*:://;
    return($self->{GLwin}->$sub(@args));
  }
}


sub buttonmotion{
  my($self,$but,$x,$y)=@_;

  $but--;

  return unless defined $self->{GLwin}{EHandler}{Buttons}[$but];

  $self->{GLwin}{EHandler}{Buttons}[$but]->mouse_moved($self->{GLwin}{EHandler}{X},
							$self->{GLwin}{EHandler}{Y},
							$x,$y);
  $self->{GLwin}{EHandler}{X} = $x;
  $self->{GLwin}{EHandler}{Y} = $y;
  
  $self->refresh();
}



1;

