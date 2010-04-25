package Devel::REPL::Profile::Perldl2;
#
# Created on: Sun 25 Apr 2010 03:09:34 PM
# Last saved: Sun 25 Apr 2010 06:36:55 PM
#

use Moose;
use namespace::clean -except => [ 'meta' ];

with 'Devel::REPL::Profile';

sub plugins {
   qw(History LexEnv DDS Packages Commands MultiLine::PPI NiceSlice CompletionDriver::Methods CompletionDriver::INC);
}

sub apply_profile {
   my ($self, $repl) = @_;

   $repl->load_plugin($_) for $self->plugins;

   # do perldl stuff here
   $repl->eval('use PDL');
   $repl->eval('use PDL::Dbg');
   $repl->eval('use PDL::Doc::Perldl');
   $repl->eval('use PDL::IO::Dumper');
   $repl->eval('use PDL::IO::FlexRaw');
   $repl->eval('use PDL::IO::Pic');
   $repl->eval('use PDL::Image2D');
   $repl->eval('use PDL::AutoLoader');

   $repl->prompt('perldl2> ');
}

1;
