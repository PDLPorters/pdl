package PDL::Perldl2::Profile::Perldl2;

use strict;
use warnings;
use Moose;
use namespace::clean -except => [ 'meta' ];

our $VERSION = 0.008;
$PERLDL::PROMPT = $PERLDL::PROMPT; # suppress warning

with 'Devel::REPL::Profile';

my %plugin2deps = (
  'Completion' => [qw(PPI)],
  'CompletionDriver::INC' => [qw(File::Next)],
  'CompletionDriver::Keywords' => [qw(B::Keywords)],
  'CompletionDriver::LexEnv' => [qw(Lexical::Persistence)],
  'DDS' => [qw(Data::Dump::Streamer)],
  'Interrupt' => [qw(Sys::SigAction)],
  'LexEnv' => [qw(Lexical::Persistence)],
  'MultiLine::PPI' => [qw(PPI)],
);
sub plugins {
   qw(
      CleanErrors
      Commands
      Completion
      CompletionDriver::INC
      CompletionDriver::Keywords
      CompletionDriver::LexEnv
      CompletionDriver::Methods
      DDS
      History
      Interrupt
      LexEnv
      MultiLine::PPI
      Packages
      NiceSlice
      PrintControl
      ReadLineHistory
      PDLCommands
   ); # CompletionDriver::Globals
}

sub apply_profile {
   my ($self, $repl) = @_;

   # check for Term::ReadLine::Stub
   if ($repl->term->ReadLine =~ /Stub/) {
      $repl->print("WARNING:\n Term::ReadLine::Stub does not support pdl2 features.\n");
      $repl->print(" Please install either Term::ReadLine::Perl or Term::ReadLine::Gnu.\n");
      $repl->print(" Falling back to perldl in the meantime...\n");
      $repl->print("------------------------------------------\n\n");
      exec 'perldl';
   }

   # add PDL::Perldl2 for plugin search
   push @{$repl->_plugin_app_ns}, 'PDL::Perldl2';

   foreach my $plug ($self->plugins) {
     if (my $deps = $plugin2deps{$plug}) {
       next if grep !eval "require $_; 1", @$deps;
     }
     $repl->load_plugin($plug);
   }

   # enable Term::ReadLine file expansion by default
   $repl->do_readline_filename_completion(1) if $repl->can('do_readline_filename_completion');

   # do perldl stuff here
   $repl->eval('package main');

   $repl->eval('use PDL');
   $repl->eval('use PDL::Config');
   $repl->eval('use PDL::Dbg');
   $repl->eval('use PDL::Doc::Perldl');
   $repl->eval('use PDL::IO::Dumper');
   $repl->eval('use PDL::IO::FlexRaw');
   $repl->eval('use PDL::IO::Pic');
   $repl->eval('use PDL::Image2D');
   $repl->eval('use PDL::AutoLoader');
   $repl->eval('no strict qw(vars)');

   # declare PERLDL package variables
   # most are not used but they are here if needed
   $repl->eval( q[
      @PERLDL::AUTO = ();                                 # code string/refs to run after user enters a new line
      $PERLDL::ESCAPE = '#';                              # Default shell escape character
      $PERLDL::HISTFILESIZE =  $ENV{PERLREPL_HISTLEN};    # Number of lines to keep in history
      $PERLDL::MULTI  = 1;                                # Enable multi-lines by default
      $PERLDL::NO_EOF = 1;                                # Enable EOF protection by default
      $PERLDL::PAGE   = 0;
      $PERLDL::PAGER  = ((exists $ENV{PAGER}) ? $ENV{PAGER} : 'more');
      $PERLDL::PAGING = 0;
      $PERLDL::PROMPT = "pdl> ";                          # string or code reference
      $PERLDL::PREFIX_RE = qr(^\s*(?:pdl|perldl)>\s*);    # RE for shell prompts
      $PERLDL::TERM = $_REPL->term;
      ] );

   #autoflush STDOUT
   $repl->eval('$|=1;');
   # p command (NOTE: this is not an alias for print)
   $repl->eval('sub p { local $, = " "; print @_,"\n" };');

   # list history command
   $repl->eval('sub l {
      my $n = $#_ > -1 ? shift : 20;
      my @h = $_REPL->term->GetHistory();
      my $min = $#h < $n-1 ? 0 : $#h-$n+1;
      map { printf "%d: %s\n", $_+1, $h[$_] } ($min..$#h);
      #map {print  "$_: $h[$_]\n"} ($min..$#h);
      };');

   $repl->eval( q{
    sub with_time (&) {
      require Time::HiRes;
      my @t = Time::HiRes::gettimeofday();
      &{$_[0]}();
      printf "%g ms\n", Time::HiRes::tv_interval(\@t) * 1000;
    }
   } );

   $repl->eval( q{
    sub gv {
      my ($pdl, $file) = @_;
      my $format = (split '.', $file)[-1];
      my $g = PDL::Core::pdumpgraph(PDL::Core::pdumphash($pdl));
      require GraphViz2;
      my $gv = GraphViz2->from_graph(PDL::Core::pdumpgraphvizify($g));
      $gv->run(format => $format, output_file => $file);
    }
   } );

   $repl->eval( q{
    sub x {
      require Data::Dumper;
      local $Data::Dumper::Indent = 1;
      local $Data::Dumper::Sortkeys = 1;
      local $Data::Dumper::Terse = 1;
      print Data::Dumper::Dumper(@_);
    }
   } );

   $repl->eval( q{
    use PDL::Demos;
    sub demo {
      if (!$_[0]) {
        require List::Util;
        my @kw = sort grep $_ ne 'pdl', PDL::Demos->keywords;
        my $maxlen = List::Util::max(map length, @kw);
        print "Use:\n";
        printf "   demo %-${maxlen}s # %s\n", @$_[0,1] for map [PDL::Demos->info($_)], 'pdl', @kw;
        return;
      }
      no strict;
      PDL::Demos->init($_[0]);
      &{$_->[0]}($_->[1]) for PDL::Demos->demo($_[0]);
      PDL::Demos->done($_[0]);
    }
   } );

   if ($repl->can('do_print')) {
      $repl->eval('sub do_print { $_REPL->do_print(@_) };');
   }

   if ($repl->can('exit_repl')) {
      $repl->eval('sub quit { $_REPL->exit_repl(1) };');
   } else {
      $repl->eval('sub quit { $_REPL->print("Use Ctrl-D or exit to quit" };');
   }

   $repl->prompt($PERLDL::PROMPT);  # new prompt

   if ( defined $ENV{TERM} and $ENV{TERM} eq 'dumb' ) {
      $repl->print("\n");
      $repl->print("******************************************\n");
      $repl->print("* Warning: TERM type is dumb!            *\n");
      $repl->print("* Limited ReadLine functionality will be *\n");
      $repl->print("* available.  Please unset TERM or use a *\n");
      $repl->print("* different terminal type.               *\n");
      $repl->print("******************************************\n");
      $repl->print("\n");
   }

   $repl->print("Perldl2 Shell v$PDL::Perldl2::Profile::Perldl2::VERSION
      PDL comes with ABSOLUTELY NO WARRANTY. For details, see the file
      'COPYING' in the PDL distribution. This is free software and you
      are welcome to redistribute it under certain conditions, see
      the same file for details.\n");

   $repl->print("Loaded plugins:\n");
   {
      my @plugins = ();
      foreach my $pl ( $repl->_plugin_locator->plugins ) {
         # print names of ones that have been loaded
         my $plug = $pl;
         $plug =~ s/^.*Plugin::/  /;
         push @plugins, $plug if $repl->does($pl);
      }
      # Need smarter display of plugins, fill out the line
      # and list CompletionDrivers under Completion
      $repl->print(join "\n", sort(@plugins));
      $repl->print("\n");
   }

   $repl->print("Type 'help' for online help\n");
   $repl->print("Type Ctrl-D or quit to exit\n");
   $repl->print("Loaded PDL v$PDL::VERSION\n");
}

1;

__END__

=head1 NAME

PDL::Perldl2::Profile::Perldl2 - profile for Perldl2 shell

=head1 SYNOPSIS

    system> re.pl --profile=PDL::Perldl2::Profile::Perldl2  # unix-ish shell
    system> re    --profile=PDL::Perldl2::Profile::Perldl2  # win32 CMD shell

    Perldl2 Shell v0.008
          PDL comes with ABSOLUTELY NO WARRANTY. For details, see the file
          'COPYING' in the PDL distribution. This is free software and you
          are welcome to redistribute it under certain conditions, see
          the same file for details.
    
    Loaded plugins:
      CleanErrors
      Commands
      Completion
      CompletionDriver::INC
      CompletionDriver::Keywords
      CompletionDriver::LexEnv
      CompletionDriver::Methods
      DDS
      FindVariable
      History
      Interrupt
      LexEnv
      MultiLine::PPI
      NiceSlice
      PDLCommands
      Packages
      PrintControl
      ReadLineHistory
    
    
    Type 'help' for online help
    
    Type Ctrl-D or quit to exit
    
    Loaded PDL v2.006
    
    pdl> 


=head1 DESCRIPTION

This profile is for development of the new PDL shell (version 2).
The preferred method to start the new shell is via the C<pdl2>
command.  This documentation is provided for C<Devel::REPL> coders
that may wish to use this profile directly for their development.

=head1 SEE ALSO

C<Devel::REPL>, C<Devel::REPL::Profile>, and C<PDL::Perldl>.

=head1 AUTHOR

Chris Marshall, C<< <chm at cpan dot org> >>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2010 by Christopher Marshall

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
