package PDL::SFilter;

# replace all occurences of the form
#
#   $pdl(args);
# or
#   $pdl->(args);
# with
#
#   $pdl->mslice(processed_args);
#

$PDL::SFilter::VERSION = 0.2;

use Text::Balanced; # used to find parenthesis-delimited blocks 

# a call stack for error processing
my @callstack = ('stackbottom');
sub curarg {
  return $callstack[-1]; # return top element of stack
}

my @srcstr = (); # stack for refs to current source strings
my $offset = 1;  # this is temporary

sub line {
  die __PACKAGE__." internal error: can't determine line number"
    if $#srcstr < 0;
  my $pretext = substr ${$srcstr[0]}, 0, pos(${$srcstr[0]})-1;
  return ($pretext =~ tr/\n/\n/)+$offset;
}

sub filterdie {
  my ($msg) = @_;
  die "$msg near line ".
    line()." (line count may start at 'use PDL::SFilter')\n"
}

# a pattern that finds occurences of the form
#
#  $avar(
#
# and
#
#  ->(
#
# used as the prefix pattern for findslice
my $prefixpat = qr/.*?  # arbitrary leading stuff
                   ((?<!&)\$\w+  # $varname not preceded by '&'
                    |->)         # or just '->'
                   (?=\()/smx;   # directly followed by open '(' (look ahead)

# translates a single arg into corresponding
# mslice format
sub onearg ($) {
  my ($arg) = @_;
  return 'X' if $arg =~ /^\s*$/;     # empty arg
  # recursively process args for slice syntax
  $arg = findslice($arg) if $arg =~ $prefixpat;
  # no doubles colon are matched to avoid confusion with Perl's C<::>
  return "[$arg]" if $arg =~ s/(?<!:):(?!:)/,/g; # replace single ':' with ','
  # the (pos) syntax, i.e. 0D slice
  return "[$arg,0,0]" if $arg =~ s/^\s*\((.*)\)\s*$/$1/; # use the new [x,x,0]
  # we don't allow [] syntax (although that's what mslice uses)
  filterdie "invalid slice expression containing '[', expression was '".
    curarg()."'" if $arg =~ /^\s*\[/;
  # this must be a simple position, wrap in [] for mslice
  return "[$arg]";  # otherwise just a simple arg wrapper
}

# non-bracketed prefix matching regexp
my $prebrackreg = qr/^([^\(\{\[]*)/;

# split comma separated arglist
# but ignore bracket-protected commas
# (i.e. commas that are within matched brackets)
sub splitarglist ($) {
  my ($txt) = @_;
  my ($got,$pre) = (1,'');
  my @chunks = ('');
  my $ct = 0; # infinite loop protection
  while ($got && $txt =~ /[({\[]/ && $ct++ < 1000) {
    # print "iteration $ct\n";
    ($got,$txt,$pre) =
      Text::Balanced::extract_bracketed($txt,'{}()[]',$prebrackreg);
    my @partialargs = split ',', $pre, -1;
    $chunks[-1] .= shift @partialargs if @partialargs;
    push @chunks, @partialargs;
    $chunks[-1] .= $got;
  }
  filterdie "possible infinite parse loop, slice arg '".curarg()."'"
			   if $ct == 1000;
  my @partialargs = split ',', $txt, -1;
  $chunks[-1] .= shift @partialargs if @partialargs;
  push @chunks, @partialargs;
  return @chunks;
}

# process the arg list
sub procargs {
  my ($txt) = @_;
  $txt =~ s/\((.*)\)/$1/;
  push @callstack, $txt; # for later error reporting
  my $args = $txt =~ /^\s*$/ ? '' :
    join ',', map {onearg $_} splitarglist $txt;
  $args =~ s/\s//g; # get rid of whitespace
  pop @callstack; # remove from call stack
  return "($args)";
}

# this is the real workhorse that translates occurences
# of $a(args) into $args->mslice(processed_arglist)
#
sub findslice {
  my ($src,$verb) = @_;
  push @srcstr, \$src;
  $verb = 0 unless defined $verb;
  my $processed = '';
  my $ct=0; # protect against infinite loop
  my ($found,$prefix,$dummy);
  while ( (($found,$dummy,$prefix) = 
	   Text::Balanced::extract_bracketed($src,'()',$prefixpat))[0]
	  && $ct++ < 1000) {
    print STDERR "pass $ct: found slice expr $found at line ".line()."\n"
      if $verb;
    $processed .= "$prefix". ($prefix =~ /->$/ ? '' : '->').
      'mslice'.procargs($found);
  }
  pop @srcstr; # clear stack
  $processed .= substr $src, pos($src); # append the remaining text portion
}


# save eval of findslice that should be used within perldl as a preprocessor
sub perldlpp {
 my ($txt) = @_;
 my $new;
 eval '$new = PDL::SFilter::findslice $txt';
 return "print q|preprocessor error: $@|" if $@;
 return $new;
}

# use Damian Conway's simplified source filter interface
use Filter::Simple sub {
   $_ = findslice $_;
 };

=head1 NAME

PDL::SFilter - towards a less insane slice syntax

=head1 SYNOPSYS

  use PDL::SFilter;

  $a(1:4) .= 2;
  print $b((0),1:$end);
  $a->xchg(0,1)->(($pos-1)) .= 0;

=head1 DESCRIPTION

C<PDL>'s C<slice> syntax sucks. This module attempts to reduce the
suction. How does it work? We can't change Perl's syntax limitations.
However, Damian Conway has shown how to move forward within the
framework of Perl 5 syntax restrictions. The answer lies in the murky
area of source filters. Source filters can be thought of as a very
powerful macro facility. Finally Perl will rival Lisp in
obscurity. The possibility for confusion is virtually infinite and
only matched by the power of the approach ;).

I<NOTE>: C<PDL::SFilter> relies on modules from CPAN that make source filtering
and parsing easier. In particular it requires F<Filter::Simple> (which
in turn requires a module from the F<Filter> distribution) and
F<Text::Balanced>.

=head1 New slicing syntax

=head2 Parentheses on a scalar variable

An arglist in parentheses following directly after a scalar variable
name that is I<not> preceded by C<&> will be translated into an
approriate call to C<mslice>, e.g.

  $a(1:4) .= 2;

is equivalent to

  $a->mslice([1,4]) .= 2

However,

  &$a(4,5);

will be left untouched to not interfere with the current subref syntax.

=head2 The I<default method> syntax

The second syntax that will be recognized is what I called the
I<default method> syntax. It is the method arrow C<-E<gt>> directly
followed by an open parenthesis C<(>, e.g.

  $a->xchg(0,1)->(($pos)) .= 0;

corresponds to

  $a->xchg(0,1)->mslice([$pos,$pos,0]) .= 0; # really the ($pos) 
                                             # format of slice

Note that this conflicts with the use of code refs, since you can write
in plain Perl

  $sub = sub { print join ',', @_ };
  $sub->(1,'a');

NOTE: Once C<use PDL::SFilter> is in effect the preprocessor will incorrectly
replace the above call to $sub with an invocation of the slicing method.
I am not sure if this bothers anybody; a simple workaround is to use the
C<&>-way of calling subrefs, e.g.:

  $sub = sub { print join ',', @_ };
  &$sub(1,'a');

Why two different ways to invoke slicing? The first syntax C<$a(args)>
doesn't work with chained method calls. E.g.

  $a->xchg(0,1)(0);

won't work. Instead, use the I<default method> syntax:

  $a->xchg(0,1)->(0);

Similarly, if you have a list of piddles C<@pdls>:

  $b = $pdls[5]->(0:-1);

=head2 argument list syntax

Note that perl variables and function invocations can be used in the
argument list:

  $a($pos-1:$end,myfunc(1,3)) .= 5;

There can even be other slicing commands in the arglist:

  $a(0:-1:$pdl($step)->at(0)) *= 2;

You can access ranges using the usual C<:> format:

  $a($start:$stop:$step) *= 4;

Note that you can omit the trailing step. C<::> is not allowed to
avoid clashes with Perl's namespace syntax. A position in parentheses
C<($pos)> selects that position and removes the dim from the result piddle
(just like C<slice> does):

  print $a((0));

An empty argument selects the whole dimension:

  print $a(,(0));

Trailing dims can be omitted as usual and will be selected as a whole.

  $a = sequence 5, 5, 5;
  $b = $a(0); # slice of shape [1,5,5]

Anyway, that's the theory.  This whole thing is probably not
yet foolproof but a start.

=head1 Use in scripts and C<perldl> shell

In a script or module write
something like

  use PDL::SFilter;

  # this code will be translated

  no PDL::SFilter;

  # this code won't

See also L<Filter::Simple> and F<test.pl> in this distribution for
an example.

To use the filter in the C<perldl> shell
add the following two lines to your F<.perldlrc> file:

   use PDL::SFilter;
   $PERLDL::PREPROCESS = \&PDL::SFilter::perldlpp;

A more complete tool box of commands for experimentation is
in the file F<local.perldlrc> in the C<PDL::SFilter> source
directory.

Error checking is not yet foolproof. Please send comments
and bug reports to the pdl-porters list <pdl-porters@jach.hawaii.edu>.

=head1 Dependencies

This module relies on the latest version of mslice to work properly.
These are included below for the moment. If your installed PDL has
a different mslice version just include the following in your F<.perldlrc>
(or even better include the contents of the file F<local.perldlrc> that
is part of this dist):

   # called for colon-less args	
   # preserves parens if present	
   sub intpars { $_[0] =~ /\(.*\)/ ? '('.int($_[0]).')' : int $_[0] }
   sub PDL::mslice {
	   my($pdl) = shift;
	   return $pdl->slice(join ',',(map {
			   $_ eq "X" ? ":" :
			   ref $_ eq "ARRAY" ? $#$_ > 1 && @$_[2] == 0 ? 
			   "(".int(@$_[0]).")" : join ':', map {int $_} @$_ :
			   !ref $_ ? intpars $_ :
			   die "INVALID SLICE DEF $_"
		   } @_));
   }


=head1 BUGS

Undoubtedly C<;)>. The module is still highly experimental.
Feedback and bug reports are welcome.

=head1 COPYRIGHT

Copyright (c) 2001, Christian Soeller. All Rights Reserved.
This module is free software. It may be used, redistributed
and/or modified under the same terms as PDL itself
(see http://pdl.perl.org).

=cut

1;
