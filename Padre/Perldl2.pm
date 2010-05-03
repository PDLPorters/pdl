package Devel::REPL::Profile::Perldl2;
#
# Created on: Sun 25 Apr 2010 03:09:34 PM
# Last saved: Mon 03 May 2010 01:35:15 PM 
#

use Moose;
use namespace::clean -except => [ 'meta' ];

with 'Devel::REPL::Profile';

sub plugins {
   qw( Commands CompletionDriver::Globals CompletionDriver::INC CompletionDriver::Keywords CompletionDriver::LexEnv
       CompletionDriver::Methods DDS History Interrupt LexEnv MultiLine::PPI NiceSlice Packages);
}

sub apply_profile {
   my ($self, $repl) = @_;

   $repl->load_plugin($_) for $self->plugins;

   # these plugins don't work on win32
   unless ($^O =~ m/win32/i) {
      $repl->load_plugin('Interrupt');
      $repl->load_plugin('ReadLineHistory');
   }


   # do perldl stuff here
   $repl->eval('use PDL');
   $repl->eval('use PDL::Dbg');
   $repl->eval('use PDL::Doc::Perldl');
   $repl->eval('use PDL::IO::Dumper');
   $repl->eval('use PDL::IO::FlexRaw');
   $repl->eval('use PDL::IO::Pic');
   $repl->eval('use PDL::Image2D');
   $repl->eval('use PDL::AutoLoader');

   $repl->eval('no strict qw(vars)');
   $repl->prompt("perldl2> ");
}

1;
