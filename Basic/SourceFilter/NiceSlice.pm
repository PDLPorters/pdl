package PDL::NiceSlice;

# replace all occurences of the form
#
#   $pdl(args);
# or
#   $pdl->(args);
# with
#
#   $pdl->mslice(processed_args);
#

$PDL::NiceSlice::VERSION = 0.5;

require PDL::Version; # get PDL version number
if ("$PDL::Version::VERSION" !~ /cvs$/ and
    "$PDL::Version::VERSION" lt '2.2.2') { # make sure we got uptodate version
                                           # of nslice
eval << 'EOH';
  {
    package PDL;
    sub _intpar ($) { ref $_[0] ? UNIVERSAL::isa($_[0],'PDL') ?
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
	  ref $_ eq "ARRAY" ? $#$_ > 1 && _intpar @$_[2] == 0 ? 
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
 eval '$new = PDL::NiceSlice::findslice $txt';
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

PDL::NiceSlice - toward a nicer slicing syntax for PDL

=head1 SYNOPSYS

  use PDL::NiceSlice;

  $a(1:4) .= 2;             # concise syntax for ranges
  print $b((0),1:$end);
  $a->xchg(0,1)->(($pos-1)) .= 0;
  
  $idx = long 1, 7, 3, 0;   # a piddle of indices
  $a(-3:2:2,$idx) += 3;     # mix explicit indexing and ranges

=head1 DESCRIPTION

C<PDL>'s L<slice|PDL::Slices/slice> syntax sucks. This module tries to rectify the
situation to some degree. Using Perl's ability to use I<source
filtering> (which you can think of as a very powerful macro facility,
see L<perlfilter>) it introduces a more reasonable syntax
for slicing PDL objects (AKA piddles).  C<PDL::NiceSlice> implements
the source filter that provides this new feature.

I<NOTE>: C<PDL::NiceSlice> relies on several modules from CPAN that make
source filtering and parsing easier. In particular it requires
F<Filter::Simple> (which in turn requires a module from the F<Filter>
distribution) and F<Text::Balanced>. To make your life easier it is
recommended that you use the F<CPAN> module to resolve dependencies
during installation automatically. Using the cpan installation shell
installation should be as easy as this:

   $ perl -MCPAN -e shell
   cpan> install PDL::NiceSlice

=head1 The new slicing syntax

Using C<PDL::NiceSlice> slicing piddles becomes so much easier since, first of
all, you don't need to make explicit method calls. No

  $pdl->slice(....);

calls, etc. Instead, C<PDL::NiceSlice> introduces two ways in which to
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
followed by an open parenthesis, e.g.

  $a->xchg(0,1)->(($pos)) .= 0;

Note that this conflicts with the use of normal code references, since you
can write in plain Perl

  $sub = sub { print join ',', @_ };
  $sub->(1,'a');

NOTE: Once C<use PDL::NiceSlice> is in effect (see below how to switch it off
again in a source file) the source filter will incorrectly
replace the above call to C<$sub> with an invocation of the slicing method.
This is one of the pitfalls of using a source filter that doesn't know
anything about the runtime type of a variable (cf. the
Implementation section).

This shouldn't be a major problem in practice; a simple workaround is to use
the C<&>-way of calling subrefs, e.g.:

  $sub = sub { print join ',', @_ };
  &$sub(1,'a');

=head2 When to use which syntax?

Why are there two different ways to invoke slicing?
The first syntax C<$a(args)> doesn't work with chained method calls. E.g.

  $a->xchg(0,1)(0);

won't work. It can I<only> be used directly following a valid perl variable
name. Instead, use the I<default method> syntax in such cases:

  $a->xchg(0,1)->(0);

Similarly, if you have a list of piddles C<@pdls>:

  $b = $pdls[5]->(0:-1);

=head2 The argument list

The argument list is a comma separated list. Each argument specifies
how the corresponding dimension in the piddle is sliced. In contrast
to usage of L<slice|PDL::Slices/slice> the arguments should not be
quoted. Rather freely mix literals (1,3,etc), perl variabales and
function invocations, e.g.

  $a($pos-1:$end,myfunc(1,3)) .= 5;

There can even be other slicing commands in the arglist:

  $a(0:-1:$pdl($step)) *= 2;

NOTE: If you use function calls in the arglist make sure that
you use parentheses around their argument lists. Otherwise the
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

Just as with L<slice|PDL::Slices/slice> negative indices count from the end of the dimension
backwards with C<-1> being the last element. If the start index is larger
than the stop index the resulting piddle will have the elements in reverse
order between these limits:

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
in parentheses (again similar to the L<slice|PDL::Slices/slice> syntax):

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

The second way to select indices from a dimension is via 1D piddles
of indices. A simple example:

  $a = random 10;
  $idx = long 3,4,7,0;
  $b = $a($idx);

This way of selecting indices was previously only possible using
L<dice|PDL::Slices/dice> (C<PDL::NiceSlice> attempts to unify the
C<slice> and C<dice> interfaces). Note that the indexing piddles must
be 1D or 0D. Higher dimensional piddles as indices will raise an error:

  $a = sequence 5, 5;
  $idx2 = ones 2,2;
  $sum = $a($idx2)->sum;
 piddle must be <= 1D at /home/XXXX/.perldlrc line 93

Note that using index piddles is not as efficient as using ranges.
If you can represent the indices you want to select using a range
use that rather than an equivalent index piddle. In particular,
memory requirements are increased with index piddles (and execution
time I<may> be longer). That said, if an index piddle is the way to
go use it!

=back

As you might have expected ranges and index piddles can be freely
mixed in slicing expressions:

  $a = random 5, 5;
  $b = $a(-1:2,pdl(3,0,1));

=head2 piddles as indices

You can use piddles to specify indices in ranges. No need to
turn them into proper perl scalars with the new slicing syntax.
However, make sure they contain not more than one element! Otherwise
a runtime error will be triggered. First a few examples that illustrate
proper usage:

  $a = sequence 5, 5;
  $rg = pdl(1,-1,3);
  print $a($rg(0):$rg(1):$rg(2),2);
 [
  [11 14]
 ]
  print $a($rg+1,:$rg(0));
 [
  [2 0 4]
  [7 5 9]
 ]

The next one raises an error 

  print $a($rg+1,:$rg(0:1));
 multielement piddle where only one allowed at XXX/Core.pm line 1170.

The problem is caused by using the 2-element piddle C<$rg(0:1)> as the
stop index in the second argument C<:$rg(0:1)> that is interpreted as
a range by C<PDL::NiceSlice>. You I<can> use multielement piddles as
index piddles as described above but not in ranges. And
C<PDL::NiceSlice> treats any expression with unprotected C<:>'s as a
range.  I<Unprotected> means as usual 
I<"not occurring between matched parentheses">.

=head1 Use in scripts and C<perldl> shell

Source filtering can be switched on and off in scripts
and perl modules by using or unloading C<PDL::NiceSlice>.
Everything after C<use PDL::NiceSlice> will be translated
and you can use the snew slicing syntax. Source filtering
will continue until the end of the file is encountered.
You can stop sourcefiltering before the end of the file
by issuing a C<no PDL::NiceSlice> statement.

Here is an example:

  use PDL::NiceSlice;

  # this code will be translated
  # and you can use the new slicing syntax

  no PDL::NiceSlice;

  # this code won't
  # and the new slicing syntax will raise errors!

See also L<Filter::Simple> and F<example> in this distribution for
further examples.

To use the filter in the C<perldl> shell you need to
add the following two lines to your F<.perldlrc> file:

   use PDL::NiceSlice;
   $PERLDL::PREPROCESS = \&PDL::NiceSlice::perldlpp;

A more complete tool box of commands for experimentation is
in the file F<local.perldlrc> in the C<PDL::NiceSlice> source
directory. Just include the code in that file in your usual
F<~/.perldlrc> and you can switch source filtering with
PDL::NiceSlice on and off by typing C<trans> and C<notrans>,
respectively. To see what and how your commands are translated
switch reporting on:

  perldl> report 1;

Similarly, switch reporting off as needed

  perldl> report 0;

Note that these commands will only work if you included
the contents of F<local.perldlrc> in your perldl startup file.

=head1 Implementation

C<PDL::NiceSlice> exploits the ability of Perl to use source filtering
(see also L<perlfilter>). A source filter basically filters (or
rewrites) your perl code before it is seen by the
compiler. C<PDL::NiceSlice> searches through your Perl source code and when
it finds the new slicing syntax it rewrites the argument list
appropriately and splices a call to the C<nslice> method using the
modified arg list into your perl code. You can see how this works in
the L<perldl|perldl> shell by switching on reporting (see above how to do
that).

The C<nslice> method is an extended version of L<mslice|PDL::Core/mslice> that
knows how to deal with index piddles (and therefore combines
slicing and dicing). Full documentation of C<nslice> will
be in the next PDL release.

=head1 BUGS

Error checking is probably not yet foolproof.
Feedback and bug reports are welcome. Please include an example
that demonstrates the problem. Log bug reports in the PDL
bug database at

  http://sourceforge.net/bugs/?group_id=612

or send them to the pdl-porters mailing list
E<lt>pdl-porters@jach.hawaii.eduE<gt>.


=head1 COPYRIGHT

Copyright (c) 2001, Christian Soeller. All Rights Reserved.
This module is free software. It may be used, redistributed
and/or modified under the same terms as PDL itself
(see http://pdl.perl.org).

=cut

1;
