package PDL::Perldl2::Plugin::PDLCommands;

use Devel::REPL::Plugin;

use namespace::clean -except => [ 'meta' ];

# The atomic option---need to deconflict Turtle command injection
# using qr{\#} and perldl's usage for command escapes.  Just
# exclude for now to get things working
excludes 'Turtles';

around 'read' => sub {

   my $orig = shift;
   my ($self, @args) = @_;

   # using $lines here because that is the usage from perldl
   # and I want to cut and paste existing code if possible
   my $lines = $self->$orig(@args);

   # Execute the list of auto-code  (TODO)
   ## for my $c (@PERLDL::AUTO) {
   ##     my $mess = eval_and_report($c);
   ##     warn $mess if $mess;
   ## }

   # Filter out PDL shell prefixes from cut-n-pasted lines
   if ( defined($lines) and $lines =~ s/$PERLDL::PREFIX_RE// ) {
      if ($PERLDL::TERM->can('GetHistory') and $PERLDL::TERM->can('SetHistory')) {
         my @hist = $PERLDL::TERM->GetHistory();
         foreach my $entry (@hist) { $entry =~ s/$PERLDL::PREFIX_RE//; }
         $PERLDL::TERM->SetHistory(@hist);
      }
   }

   return $lines unless defined $lines;

   # print STDERR "PDLCommands: got '$lines'\n";
   if ( lc $lines eq 'q' || lc $lines eq 'x' || lc $lines eq 'exit' ) { return "quit"; };

   $lines =~ s/^\s*\?\?\s*/apropos /; # Make '??' = 'apropos'
   $lines =~ s/^\s*\?\s*/help /;      # Make lone '?' = 'help'

   if ( $lines =~ /^\s*(help|usage|apropos|sig|badinfo|demo)\s+/) { # Allow help foo (no quotes)
      my @t = split(/\s+/,$lines);
      my $x;
      foreach $x(@t) { $x=~s/^["']+//; $x=~s/['"]+$//; };
      $t[1] = "'".$t[1]."'"  if ($#t == 1 && !($t[1] =~ /^\$/));
      $lines = join(' ',@t);
   }

   $PERLDL::ESCAPE = $PERLDL::ESCAPE if defined $PERLDL::ESCAPE;
   if (substr($lines,0,1) eq substr($PERLDL::ESCAPE,0,1) and
      substr($lines,0,2) ne '#!') {  # Allow escapes, avoid shebang
      my @lines = split /\n/, $lines;
      system(substr(shift @lines,1)); # Shell escape
      $lines = join("\n",@lines);
   }

   return $lines;
};

1;

__END__

=head1 NAME

PDL::Perldl2::Plugin::PDLCommands - implement perldl aliases/escapes

=head1 DESCRIPTION


This plugin implements the various convenience features of the
perldl shell which correspond, roughly, to aliases and some
structured pre-processing of the command line entered:

=over 4

=item q|x|exit|quit as shortcuts to quit the shell

=item ?? as an alias for apropos

=item ? as an alias for help

=item Autoquoting for arguments to help|usage|apropos|sig|badinfo|demo

=item C<$PERLDL::ESCAPE> at the start of a command line to escape to the shell, defaults to C<#>

=back

=head1 SEE ALSO

C<PDL::Perldl>, C<Devel::REPL>

=head1 AUTHOR

Chris Marshall, C<< <chm at cpan dot org> >>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2010 by Christopher Marshall

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
