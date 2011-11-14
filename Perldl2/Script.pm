package PDL::Perldl2::Script;

use Moose;

use namespace::clean -except => [ qw(meta) ];

extends 'Devel::REPL::Script';

sub _startup_def {
   return "PDL/default.pdl" if $^O =~ /win32/i;
   return "PDL/default.perldlrc";
}

sub load_rcfile {
   my ($self, $rc_file) = @_;

   my $HOME = $ENV{HOME};
   if ($^O =~ /win32/i and
	   (! defined($HOME)) or
	   (defined($HOME) and $HOME eq "")) {
      $HOME = $ENV{USERPROFILE};
      $HOME =~ s/\\/\//g;
   }
   print STDERR "load_rcfile: got \$HOME = $HOME\n";

   # get rc file name
   my $startup_file = _startup_def();
   foreach my $startup_suffix (qw( .pdlrc .perldlrc )) {
      if ( -e "$HOME/$startup_suffix" ) {
         $startup_file = "$HOME/$startup_suffix";
         last;
      }
   }
   print STDERR "load_rcfile: loading $startup_file\n";
   $self->apply_script($startup_file);

   # load local.perldlrc if it exists
   foreach my $local_startup_file (qw( local.pdlrc local.perldlrc )) {
      if ( -e $local_startup_file ) {
         print STDERR "load_rcfile: loading $local_startup_file\n";
         $self->apply_script($local_startup_file);
         last;
      }
   }
}

# Global and local startup

1;
