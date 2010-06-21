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
   if ($^O =~ /win32/i and $HOME eq ""){
      $HOME = $ENV{USERPROFILE};
      $HOME =~ s/\\/\//g;
   }
   print STDERR "load_rcfile: got \$HOME = $HOME\n";

   # get rc file name
   my $startup_file = -e "$HOME/.perldlrc" ? "$HOME/.perldlrc" : _startup_def();

   # plain name => ~/.re.pl/${rc_file}
   # if ($rc_file !~ m!/!) {
   #   $rc_file = File::Spec->catfile(File::HomeDir->my_home, $rc_file);
   # }

   print STDERR "load_rcfile: loading $startup_file\n";
   $self->apply_script($startup_file);
   print STDERR "load_rcfile: ...done\n";

   foreach my $local_startup_file qw( local.pdlrc local.perldlrc ) {
      if ( -e $local_startup_file ) {
         print STDERR "load_rcfile: loading $local_startup_file\n";
         $self->apply_script($local_startup_file);
         print STDERR "load_rcfile: ...done\n";
         last;
      }
   }
}

# Global and local startup

1;
