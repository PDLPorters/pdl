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

$PDL::SFilter::VERSION = 0.4;

require PDL::Version; # get PDL version number
if ("$PDL::Version::VERSION" !~ /cvs$/ and
    "$PDL::Version::VERSION" lt '2.2.2') { # make sure we got uptodate version
                                           # of nslice
eval << 'EOH';
  {
    package PDL;
    sub _intpar { ref $_[0] ? UNIVERSAL::isa($_[0],'PDL') ?
		    $_[0]->nelem == 1 ? $_[0]->flat->at(0) :
		      die "multielement piddle where only one allowed" :
			die "non piddle ref '".ref $_[0]."'"
			  : int $_[0] }
    sub PDL::flat { # fall through if < 2D
      return $_[0]->getndims > 1 ? $_[0]->clump(-1) : $_[0];
    }
    sub PDL::nslice {
      my($pdl) = shift;
      my @args = @_;
      my $i = 0;
      for (@args) {
	if (UNIVERSAL::isa($_,'PDL')) {
	  if ($_->nelem > 1) {
	    PDL::Core::barf 'piddle must be <= 1D' if $_->getndims > 1;
	    # dice this axis
	    $pdl = $pdl->dice_axis($i,$_);
	    # and keep resulting dim fully in slice
	    $_ = 'X'; } 
	  else { $_ = $_->flat->at(0) } # reduce this one-element piddle
	  # to a scalar for 'slice'
	}
	$i++;
      }
      # print STDERR 'processed arglist: ',join(',',@args);
      my $slstr = join ',',(map {
	!ref $_ && $_ eq "X" ? ":" :
	  ref $_ eq "ARRAY" ? $#$_ > 1 && @$_[2] == 0 ? 
	    "("._intpar(@$_[0]).")" : join ':', map {_intpar $_} @$_ :
	      _intpar $_
	    } @args);
      # print STDERR "slicestr: $slstr\n";
      return $pdl->slice($slstr);
    }
  }
EOH
}

use Text::Balanced; # used to find parenthesis-delimited blocks 

# a call stack for error processing
my @callstack = ('stackbottom');
sub curarg {
  return $callstack[-1]; # return top element of stack
}

my @srcstr = (); # stack for refs to current source strings
my $offset = 1;  # line offset
my $file   = 'unknown';

sub line {
  die __PACKAGE__." internal error: can't determine line number"
    if $#srcstr < 0;
  my $pretext = substr ${$srcstr[0]}, 0, pos(${$srcstr[0]})-1;
  return ($pretext =~ tr/\n/\n/)+$offset;
}

sub filterdie {
  my ($msg) = @_;
  die "$msg\n\t at $file near line ".
    line().", slice expression '".curarg()."'\n";
}

# non-bracketed prefix matching regexp
my $prebrackreg = qr/^([^\(\{\[]*)/;

# split regex $re separated arglist
# but ignore bracket-protected bits
# (i.e. text that is within matched brackets)
sub splitprotected ($$) {
  my ($txt,$re) = @_;
  my ($got,$pre) = (1,'');
  my @chunks = ('');
  my $ct = 0; # infinite loop protection
  while ($got && $txt =~ /[({\[]/ && $ct++ < 1000) {
    # print "iteration $ct\n";
    ($got,$txt,$pre) =
      Text::Balanced::extract_bracketed($txt,'{}()[]',$prebrackreg);
    my @partialargs = split $re, $pre, -1;
    $chunks[-1] .= shift @partialargs if @partialargs;
    push @chunks, @partialargs;
    $chunks[-1] .= $got;
  }
  filterdie "possible infinite parse loop, slice arg '".curarg()."'"
			   if $ct == 1000;
  my @partialargs = split $re, $txt, -1;
  $chunks[-1] .= shift @partialargs if @partialargs;
  push @chunks, @partialargs;
  return @chunks;
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
  return q|'X'| if $arg =~ /^\s*:??\s*$/;     # empty arg or just colon
  # recursively process args for slice syntax
  $arg = findslice($arg) if $arg =~ $prefixpat;
  # no doubles colon are matched to avoid confusion with Perl's C<::>
  if ($arg =~ /(?<!:):(?!:)/) {
    my @args = splitprotected $arg, '(?<!:):(?!:)';
    filterdie "invalid range in slice expression '".curarg()."'"
      if @args > 3;
    $args[0] = 0 if !defined $args[0] || $args[0] =~ /^\s*$/;
    $args[1] = -1 if !defined $args[1] || $args[1] =~ /^\s*$/;
    $args[2] = 1  if !defined $args[2] || $args[2] =~ /^\s*$/;
    return "[".join(',',@args)."]"; # replace single ':' with ','
  }
  # the (pos) syntax, i.e. 0D slice
  return "[$arg,0,0]" if $arg =~ s/^\s*\((.*)\)\s*$/$1/; # use the new [x,x,0]
  # we don't allow [] syntax (although that's what mslice uses)
  filterdie "invalid slice expression containing '[', expression was '".
    curarg()."'" if $arg =~ /^\s*\[/;
  # this must be a simple position, leave as is
  return "$arg";
}

# process the arg list
sub procargs {
  my ($txt) = @_;
  $txt =~ s/\((.*)\)/$1/;
  push @callstack, $txt; # for later error reporting
  my $args = $txt =~ /^\s*$/ ? '' :
    join ',', map {onearg $_} splitprotected $txt, ',';
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
      'nslice'.procargs($found);
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

# well it is not quite that simple ;)
#  since Filter::Simple doesn't have all the bells and whistles yet
#
# some symbol table wizardry to get file name and offset
local $^W = 0; # shut up warnings about redefining import
my $class = __PACKAGE__;
my $sub = \&{"${class}::import"}; # get the import meth created by F::Simple
*{"${class}::import"} = sub {
  ($file,$offset) = (caller)[1,2];
  $offset++;
  # now chain the import sub that was generated by Filter::Simple
  &$sub(@_);
};

=head1 NAME

PDL::SFilter - towards a less insane slice syntax

=head1 SYNOPSYS

  use PDL::SFilter;

  $a(1:4) .= 2;
  print $b((0),1:$end);
  $a->xchg(0,1)->(($pos-1)) .= 0;

=head1 DESCRIPTION

C<PDL>'s C<slice> syntax sucks. This module tries to rectify the
situation to some degree. Using Perl's ability to do I<source
filtering> (which you can think of as a very powerful macro facility,
see L<perlfilter>) it introduces a more reasonable syntax for slicing
of PDL objects (AKA piddles).  C<PDL::SFilter> implements the source
filter that provides this new feature.

I<NOTE>: C<PDL::SFilter> relies on several modules from CPAN that make
source filtering and parsing easier. In particular it requires
F<Filter::Simple> (which in turn requires a module from the F<Filter>
distribution) and F<Text::Balanced>. To make your life easier it is
recommended that you use the F<CPAN> module to resolve dependencies
during installation automatically. Using the cpan installation shell
installation should be as easy as this:

   $ perl -MCPAN -e shell
   cpan> i PDL::SFilter

=head1 The new slicing syntax

Using C<PDL::SFilter> slicing piddles becomes so much easier since, first of
all, you don't need to make explicit method calls any more. No

  $pdl->slice(....);

calls, etc. Instead, C<PDL::SFilter> introduces two ways in which to
slice piddles without too much typing:

=over 2

=item *

using parentheses directly following a scalar variable name,
for example

   $c = $b(0:-3:4,(0));

=item *

using the so called I<default method> invocation in which the
piddle object is treated as if it were a reference to a
subroutine (see also L<perlref>). Take this example that slices
a piddle that is part of a perl list C<@b>:

  $c = $b[0]->(0:-3:4,(0));

=back

The format of the argument list is the same for both types of
invocation and will be explained in more detail below.

=head2 Parentheses following a scalar variable name

An arglist in parentheses following directly after a scalar variable
name that is I<not> preceded by C<&> will be resolved as a slicing
command, e.g.

  $a(1:4) .= 2;         # only use this syntax on piddles
  $sum += $a(,(1));

However, if the variable name is immediately preceded by a C<&>,
for example

  &$a(4,5);

it will not be interpreted as a slicing expression. Rather, to avoid
interfering with the current subref syntax, it will be treated as an
invocation of the code reference C<$a> with argumentlist C<(4,5)>.

=head2 The I<default method> syntax

The second syntax that will be recognized is what I called the
I<default method> syntax. It is the method arrow C<-E<gt>> directly
followed by an open parenthesis C<(>, e.g.

  $a->xchg(0,1)->(($pos)) .= 0;

Note that this conflicts with the use of normal code references, since you
can write in plain Perl

  $sub = sub { print join ',', @_ };
  $sub->(1,'a');

NOTE: Once C<use PDL::SFilter> is in effect (see below how to switch it off
again in a source file) the preprocessor will incorrectly
replace the above call to C<$sub> with an invocation of the slicing method.
This is one of the pitfalls of using a source filter that doesn't know
anything about the runtime type of a variable (cf. the
Implementation section).

This shouldn't be a major problem in practice; a simple workaround is to use
the C<&>-way of calling subrefs, e.g.:

  $sub = sub { print join ',', @_ };
  &$sub(1,'a');

Why are there two different ways to invoke slicing?
The first syntax C<$a(args)> doesn't work with chained method calls. E.g.

  $a->xchg(0,1)(0);

won't work. It can I<only> be used directly following a valid perl variable
name. Instead, use the I<default method> syntax in such cases:

  $a->xchg(0,1)->(0);

Similarly, if you have a list of piddles C<@pdls>:

  $b = $pdls[5]->(0:-1);

=head2 The argument list

The argument list is a comma separated list. Each argument determines
how the corresponding dimension in the piddle is sliced. In contrast
to usage of C<slice> arguments should not be quoted. Rather freely mix literals
(1,3,etc), perl variabales and function invocations, e.g.

  $a($pos-1:$end,myfunc(1,3)) .= 5;

There can even be other slicing commands in the arglist:

  $a(0:-1:$pdl($step)) *= 2;

NOTE: If you use function calls in the arglist make sure that
you use parentheses around the argument lists. Otherwise the
source filter will get confused since it splits the argument
list on commas that are not protected by parentheses. Take
the following example:

  sub myfunc { return 5*$_[0]+$_[1] }
  $a = sequence 10;
  $sl = $a(0:myfunc 1, 2);
  print $sl;
 PDL barfed: Error in slice:Too many dims in slice
 Caught at file /usr/local/bin/perldl, line 232, pkg main


The simple fix is

  $sl = $a(0:myfunc(1, 2));
  print $sl;
 [0 1 2 3 4 5 6 7]

Note that using prototypes in the definition of myfunc does not help.
At this stage the source filter is simply not intelligent enough to
make use of this information. So beware of this subtlety.

=head2 Argument formats

You can use ranges and secondly, piddles as 1D index lists.

=over 2

=item * ranges

You can access ranges using the usual C<:> separated format:

  $a($start:$stop:$step) *= 4;

Note that you can omit the trailing step which then defaults to 1.  Double
colons (C<::>) are not allowed to avoid clashes with Perl's namespace
syntax. So if you want to use steps different from the default
you have to also at least specify the stop position.
Examples:

  $a(::2);   # this won't work (in the way you probably intended)
  $a(:-1:2); # this will select every 2nd element in the 1st dim

Just as with C<slice> negative indices count from the end of the dimension
backwards with C<-1> being the last element. If the start index is smaller
than the stop index the resulting piddle will have the elements in reverse
between those indices:

  print $a(-2:0:2);
 [8 6 4 2 0]

A single index just selects the given index in the slice

  print $a(5);
 [5]

Note, however, that the corresponding dimension is not removed from
the resulting piddle but rather reduced to size 1:

  print $a(5)->info
 PDL: Double D [1]

If you want to get completely rid of that dimension enclose the index
in parentheses (again similar to the C<slice> syntax):

  print $a((5));
 5

In this particular example a 0D piddle results. Note that this syntax is
only allowed with a single index. All these will be errors:

  print $a((0,4));  # will work but not in the intended way
  print $a((0:4));  # compile time error

An empty argument selects the whole dimension, in this example
all of the first dimension:

  print $a(,(0));

Alternative ways to select a whole dimension are

  $a = sequence 5, 5; 
  print $a(:,(0));
  print $a(0:-1,(0));
  print $a(:-1,(0));
  print $a(0:,(0));

Arguments for trailing dimensions can be omitted. In that case
these dimensions will be fully kept in the sliced piddle:

  $a = random 3,4,5;
  print $a->info;
 PDL: Double D [3,4,5]
  print $a((0))->info;
 PDL: Double D [4,5]
  print $a((0),:,:)->info;  # a more explicit way
 PDL: Double D [4,5]
  print $a((0),,)->info;    # similar
 PDL: Double D [4,5]

=item * piddle index lists

=back

=head2 piddles as indices

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
