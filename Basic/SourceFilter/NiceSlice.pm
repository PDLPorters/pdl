package PDL::NiceSlice;

# replace all occurences of the form
#
#   $pdl(args);
# or
#   $pdl->(args);
# with
#
#   $pdl->nslice(processed_args);
#
#
# Modified 2-Oct-2001: don't modify $var(LIST) if it's part of a
# "for $var(LIST)" or "foreach $var(LIST)" statement.  CED.

$PDL::NiceSlice::VERSION = '0.99.2';
$PDL::NiceSlice::debug = 0;
# the next one is largely stolen from Regexp::Common
my $RE_cmt = qr'(?:(?:\#)(?:[^\n]*)(?:\n))';

require PDL::Version; # get PDL version number
if ("$PDL::Version::VERSION" !~ /cvs$/ and
    "$PDL::Version::VERSION" lt '2.2.2') { # make sure we got uptodate version
                                           # of nslice

eval << 'EOF' unless PDL->can('flat');
    sub PDL::flat { # fall through if < 2D
      return $_[0]->dims == 1 ? $_[0] : $_[0]->clump(-1);
    }
EOF

eval << 'EOH';
  {
    package PDL;
    sub _intpar ($) { ref $_[0] ? UNIVERSAL::isa($_[0],'PDL') ?
		    $_[0]->nelem == 1 ? $_[0]->flat->at(0) :
		      die "multielement piddle where only one allowed" :
			die "non piddle ref '".ref $_[0]."'"
			  : int $_[0] }
    sub PDL::nslice {
      my($pdl) = shift;
      my @args = @_;
      my ($i,$noslice) = (0,0);
      for (@args) {
	if (UNIVERSAL::isa($_,'PDL')) {
	  if ($_->nelem > 1) {
	    if ($_->getndims > 1) {
	      # allow one multi-D arg which will imply flat addressing
	      PDL::Core::barf 'piddle must be <= 1D' if @args > 1;
	      $pdl = $pdl->flat->index($_);
	      $noslice = 1;
	    } else {
	      # dice this axis
	      $pdl = $pdl->dice_axis($i,$_);
	      # and keep resulting dim fully in slice
	      $_ = 'X'; 
	    }
	  } else { $_ = $_->flat->at(0) } # reduce this one-element piddle
					# to a scalar for 'slice'
	}
	$i++;
      }
      return $pdl if $noslice;
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

# mark as lvalue sub if 5.6.x and above
eval 'sub PDL::nslice : lvalue;' if ($^V and $^V ge v5.6.0);
}

use Text::Balanced; # used to find parenthesis-delimited blocks 

# a call stack for error processing
my @callstack = ('stackbottom');
sub curarg {
  my $arg = $callstack[-1]; # return top element of stack
  $arg =~ s/\((.*)\)/$1/s;
  return $arg;
}
sub savearg ($) {push @callstack,$_[0]}
sub poparg () {pop @callstack}

my @srcstr = (); # stack for refs to current source strings
my $offset = 1;  # line offset
my $file   = 'unknown';

my $mypostfix = '';

sub autosever {
  my ($this,$arg) = @_;
  $arg = 1 unless defined $arg;
  if ($arg) {$mypostfix = '->sever'} else
    {$mypostfix = ''}
}

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
  my ($re,$txt) = @_;
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
                    (\s|$RE_cmt)* # ignore comments
		    \s*          # more whitespace
                   (?=\()/smx;   # directly followed by open '(' (look ahead)

# translates a single arg into corresponding
# mslice format
sub onearg ($) {
  my ($arg) = @_;
  print STDERR "processing arg $arg\n" if $PDL::NiceSlice::debug;
  return q|'X'| if $arg =~ /^\s*:??\s*$/;     # empty arg or just colon
  # recursively process args for slice syntax
  $arg = findslice($arg,$PDL::debug) if $arg =~ $prefixpat;
  # no doubles colon are matched to avoid confusion with Perl's C<::>
  if ($arg =~ /(?<!:):(?!:)/) { # a start:stop:delta range
    my @args = splitprotected '(?<!:):(?!:)', $arg;
    filterdie "invalid range in slice expression '".curarg()."'"
      if @args > 3;
    $args[0] = 0 if !defined $args[0] || $args[0] =~ /^\s*$/;
    $args[1] = -1 if !defined $args[1] || $args[1] =~ /^\s*$/;
    $args[2] = 1  if !defined $args[2] || $args[2] =~ /^\s*$/;
    return "[".join(',',@args)."]"; # replace single ':' with ','
  }
  # the (pos) syntax, i.e. 0D slice
  return "[$arg,0,0]" if $arg =~ s/^\s*\((.*)\)\s*$/$1/; # use the new [x,x,0]
  # we don't allow [] syntax (although that's what nslice internally uses)
  filterdie "invalid slice expression containing '[', expression was '".
    curarg()."'" if $arg =~ /^\s*\[/;
  # this must be a simple position, leave as is
  return "$arg";
}

# process the arg list
sub procargs {
  my ($txt) = @_;
  print STDERR "procargs: got $txt\n" if $PDL::NiceSlice::debug;
  # $txt =~ s/^\s*\((.*)\)\s*$/$1/s; # this is now done by findslice
  # push @callstack, $txt; # for later error reporting
  my $args = $txt =~ /^\s*$/s ? '' :
    join ',', map {onearg $_} splitprotected ',', $txt;
  $args =~ s/\s//sg; # get rid of whitespace
  # pop @callstack; # remove from call stack
  return "($args)";
}

# this is the real workhorse that translates occurences
# of $a(args) into $args->nslice(processed_arglist)
#
sub findslice {
  my ($src,$verb) = @_;
  push @srcstr, \$src;
  $verb = 0 unless defined $verb;
  my $processed = '';
  my $ct=0; # protect against infinite loop
  my ($found,$prefix,$dummy);
  while ( $src =~ m/\G($prefixpat)/ && (($found,$dummy,$prefix) =
	   Text::Balanced::extract_bracketed($src,'()',$prefixpat))[0]
	  && $ct++ < 1000) {
    print STDERR "pass $ct: found slice expr $found at line ".line()."\n"
      if $verb;

#  Do final check for "for $avar(LIST)" and "foreach $avar(LIST)" syntax. 
#  Process into an 'nslice' call only if it's not that.

    if ($prefix =~ m/for(each)?(\s+(my|our))?\s+\$\w+(\s|$RE_cmt)*$/s ||
      # foreach statement: Don't translate
	$prefix =~ m/->\s*\$\w+$/s) # e.g. $a->$method(args)
      # method invocation via string, don't translate either
    {
      $processed .= "$prefix".$found;
    } else {      # statement is a real slice and not a foreach

      my ($call,$pre,$post,$arg);

      # the following section got an overhaul in v0.99
      # to fix modifier parsing and allow >1 modifier
      # this code still needs polishing
      savearg $found; # error reporting
      print STDERR "findslice: found $found\n" if $PDL::NiceSlice::debug;
      $found =~ s/^\s*\((.*)\)\s*$/$1/s;
      my ($slicearg,@mods) = splitprotected ';', $found;
      filterdie "more than 1 modifier group: @mods" if @mods > 1;
      # filterdie "invalid modifier $1"
      #	if $found =~ /(;\s*[[:graph:]]{2,}?\s*)\)$/;
      print STDERR "MODS: @mods\n" if $PDL::NiceSlice::debug;
      my @post = (); # collects all post nslice operations
      my @pre = ();
      if (@mods) {
	(my $mod = $mods[0]) =~ s/\s//sg; # eliminate whitespace
	my @modflags = split '', $mod;
	print STDERR "MODFLAGS: @modflags\n" if $PDL::NiceSlice::debug;
	filterdie "more than 1 modifier incompatible with ?: @modflags"
	  if @modflags > 1 && grep (/\?/, @modflags); # only one flag with where
	my %seen = ();
	if (@modflags) {
	  for my $mod1 (@modflags) {
	    if ($mod1 eq '?') {
	      $seen{$mod1}++ && filterdie "modifier $mod1 used twice or more";
	      $call = 'where';
	      $arg = "($slicearg)";
	      # $post = ''; # no post action required
	    } elsif ($mod1 eq '_') {
	      $seen{$mod1}++ && filterdie "modifier $mod1 used twice or more";
	      push @pre, 'flat->';
	      $call ||= 'nslice';       # do only once
	      $arg = procargs($slicearg);
	      # $post = ''; # no post action required
	    } elsif ($mod1 eq '|') {
	      $seen{$mod1}++ && filterdie "modifier $mod1 used twice or more";
	      $call ||= 'nslice';
	      $arg ||= procargs($slicearg);
	      push @post, '->sever';
	    } elsif ($mod1 eq '-') {
	      $seen{$mod1}++ && filterdie "modifier $mod1 used twice or more";
	      $call ||= 'nslice';
	      $arg ||= procargs($slicearg);
	      push @post, '->reshape(-1)';
	    } else {
	      filterdie "unknown modifier $mod1";
	    }
	  }
	} else { # empty modifier block
	  $call = 'nslice';
	  $arg = procargs($slicearg);
	  # $post = '';
	}
      } else { # no modifier block
	$call = 'nslice';
	$arg = procargs($slicearg);
	# $post = '';
      }
      $pre = join '', @pre;
      # assumption here: sever should be last
      # and order of other modifiers doesn't matter
      $post = join '', sort @post; # need to ensure that sever is last
      $processed .= "$prefix". ($prefix =~ /->(\s*$RE_cmt*)*$/ ? 
				'' : '->').
	$pre.$call.$arg.$post.$mypostfix;
    }

  } # end of while loop

  poparg;      # clean stack
  pop @srcstr; # clear stack
  # append the remaining text portion
  #     use substr only if we have had at least one pass
  #     through above loop (otherwise pos is uninitialized)
  $processed .= $ct > 0 ? substr $src, pos($src) : $src;
}


# save eval of findslice that should be used within perldl as a preprocessor
sub perldlpp {
 my ($txt) = @_;
 my $new;
 eval '$new = PDL::NiceSlice::findslice $txt';
 if ($@) {
   my $err = $@;
   for (split '','#!|\'"%~/') {
     return "print q${_}NiceSlice error: $err${_}"
       unless $err =~ m{[$_]};
    }
   return "print q{NiceSlice error: $err}"; # if this doesn't work
                                               # we're stuffed
 }
 return $new;
}

use Filter::Util::Call;

sub import {
  my ($class) = @_;
  ($file,$offset) = (caller)[1,2];  # for error reporting
  $offset++;
  my $terminator = qr/^\s*no\s+$class\s*;\s*(#.*)*$/; # allow trailing comments
	filter_add(
		sub {
			my ($status, $off, $end);
			my $count = 0;
			my $data = "";
			while ($status = filter_read()) {
				return $status if $status < 0;
				if ($terminator && m/$terminator/) {
					$off=1;
					last;
				}
				if (m/^\s*(__END__|__DATA__)\s*$/) {
				  $end=$1; $off = 1;
				  last;
				}
				$data .= $_;
				$count++;
				$_ = "";
			}
			$_ = $data;
			$_ = findslice $_ unless $status < 0; # the actual filter
			$_ .= "no $class;\n" if $off;
			$_ .= "$end\n" if $end;
			return $count;
		}
	);
}

sub unimport {
  filter_del();
}

=head1 NAME

PDL::NiceSlice - toward a nicer slicing syntax for PDL

=head1 SYNOPSYS

  use PDL::NiceSlice;

  $a(1:4) .= 2;             # concise syntax for ranges
  print $b((0),1:$end);     # use variables in the slice expression
  $a->xchg(0,1)->(($pos-1)) .= 0; # default method syntax

  $idx = long 1, 7, 3, 0;   # a piddle of indices
  $a(-3:2:2,$idx) += 3;     # mix explicit indexing and ranges
  $a->clump(1,2)->(0:30);   # 'default method' syntax
  $a(myfunc(0,$var),1:4)++; # when using functions in slice expressions
                            # use parentheses around args!

  # modifiers are specified in a ;-separated trailing block
  $a($a!=3;?)++;            # short for $a->where($a!=3)++
  $a(0:1114;_) .= 0;        # short for $a->flat->(0:1114)
  $b = $a(0:-1:3;|);        # short for $a(0:-1:3)->sever
  $n = sequence 3,1,4,1;
  $b = $n(;-);              # drop all dimensions of size 1 (AKA squeeze)
  $b = $n(0,0;-|);          # squeeze *and* sever
  $c = $a(0,3,0;-);         # more compact way of saying $a((0),(3),(0))

  # Use with perldl versions < v1.31 (or include these lines in .perldlrc)
  perldl> use PDL::NiceSlice;
  # next one is required, see below
  perldl> $PERLDL::PREPROCESS = \&PDL::NiceSlice::perldlpp;
  perldl> $a(4:5) .= xvals(2);

=head1 DESCRIPTION

Slicing is a basic, extremely common operation, and PDL's
L<slice|PDL::Slices/slice> method would be cumbersome to use in many
cases.  C<PDL::NiceSlice> rectifies that by incorporating new slicing
syntax directly into the language via a perl I<source filter> (see
L<perlfilter|the perlfilter man page>).  NiceSlice adds no new functionality, only convenient
syntax.

NiceSlice is loaded automatically in the perldl shell, but (to avoid
conflicts with other modules) must be loaded automatically in standalone
perl/PDL scripts (see below).  If you prefer not to use a prefilter on
your standalone scripts, you can use the L<slice> method in those scripts,
rather than the more compact NiceSlice constructs.

=head1 Use in scripts and C<perldl> shell

The new slicing syntax can be switched on and off in scripts
and perl modules by using or unloading C<PDL::NiceSlice>.

Note: this will I<not> work in the L<perldl|perldl> shell E<lt> v1.31.
Because the perldl shell uses evals, and NiceSlice is a perl source filter,
you have to set a special variable to use it within perldl. See below
how to enable the new slicing syntax within older L<perldl|perldl>.

But now back to scripts and modules.
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

NOTE: Unlike "normal" modules you need to include a
C<use PDL::NiceSlice> call in each and every file that
contains code that uses the new slicing syntax. Imagine
the following situation: a file F<test0.pl>

   # start test0.pl
   use PDL;
   use PDL::NiceSlice;

   $a = sequence 10;
   print $a(0:4),"\n";

   require 'test1.pl';
   # end test0.pl

that C<require>s a second file F<test1.pl>

   # begin test1.pl
   $aa = sequence 11;
   print $aa(0:7),"\n";
   1;
   # end test1.pl

Following conventional perl wisdom everything should be alright
since we C<use>d C<PDL> and C<PDL::NiceSlice> already from within
F<test0.pl> and by the time F<test1.pl> is C<require>d things should
be defined and imported, etc. A quick test run will, however, produce
something like the following:

  perl test0.pl
 [0 1 2 3 4]
 syntax error at test1.pl line 3, near "0:"
 Compilation failed in require at test0.pl line 7.

This can be fixed by adding the line

  use PDL::NiceSlice;

C<before> the code in F<test1.pl> that uses the
new slicing syntax (to play safe just include the line
near the top of the file), e.g.

   # begin corrected test1.pl
   use PDL::NiceSlice;
   $aa = sequence 11;
   print $aa(0:7),"\n";
   1;
   # end test1.pl

Now things proceed more smoothly

  perl test0.pl
 [0 1 2 3 4]
 [0 1 2 3 4 5 6 7]

Note that we don't need to issue C<use PDL> again.
C<PDL::NiceSlice> is a somewhat I<funny> module in
that respect. It is a consequence of the way source
filtering works in Perl (see also the IMPLEMENTATION
section below).

=head2 Usage with perldl

I<NOTE>: This information only applies to versions of
L<perldl|perldl> earlier than 1.31 . From v1.31 onwards
niceslicing is enabled by default, i.e.
I<it should just work>. See L<perldl> for details.

For pre v1.31 C<perldl>s you need to
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
In C<perldl> v1.31 and later these commands are available by
default.

=head2 evals and C<PDL::NiceSlice>

Due to C<PDL::NiceSlice> being a source filter it won't work
in the usual way within evals. The following will I<not> do what
you want:

  $a = sequence 10;
  eval << 'EOE';

  use PDL::NiceSlice;
  $b = $a(0:5);

  EOE
  print $b;

Instead say:

  use PDL::NiceSlice;
  $a = sequence 10;
  eval << 'EOE';

  $b = $a(0:5);

  EOE
  print $b;

Source filters I<must> be executed at compile time to be effective. And
C<PDL::NiceFilter> is just a source filter (although it is not
necessarily obvious for the casual user).

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

The $a(ARGS) syntax collides in a minor way with the perl syntax.  In
particular, ``foreach $avar(LIST)'' appears like a PDL slicing call.  
NiceSlice avoids translating the ``for $avar(LIST)'' and 
``foreach $avar(LIST)'' constructs for this reason.  Since you
can't use just any old lvalue expression in the 'foreach' 'for'
constructs -- only a real perl scalar will do -- there's no 
functionality lost.  If later versions of perl accept 
``foreach <lvalue-expr> (LIST)'', then you can use the code ref
syntax, below, to get what you want.

=head2 The I<default method> syntax

The second syntax that will be recognized is what I called the
I<default method> syntax. It is the method arrow C<-E<gt>> directly
followed by an open parenthesis, e.g.

  $a->xchg(0,1)->(($pos)) .= 0;

Note that this conflicts with the use of normal code references, since you
can write in plain Perl

  $sub = sub { print join ',', @_ };
  $sub->(1,'a');

NOTE: Once C<use PDL::NiceSlice> is in effect (you can always switch it off with
a line C<no PDL::NiceSlice;> anywhere in the script) the source filter will incorrectly
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
to usage of the L<slice|PDL::Slices/slice> method the arguments should
I<not> be quoted. Rather freely mix literals (1,3,etc), perl
variables and function invocations, e.g.

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

Another pitfall to be aware of: currently, you can't use the conditional
operator in slice expressions (i.e., C<?:>, since the parser confuses them
with ranges). For example, the following will cause an error:

  $a = sequence 10;
  $b = rand > 0.5 ? 0 : 1; # this one is ok
  print $a($b ? 1 : 2);    # error !
 syntax error at (eval 59) line 3, near "1,

For the moment, just try to stay clear of the conditional operator
in slice expressions (or provide us with a patch to the parser to
resolve this issue ;).

=head2 Modifiers

Following a suggestion originally put forward by Karl Glazebrook the
latest versions of C<PDL::NiceSlice> implement I<modifiers> in slice
expressions. Modifiers are convenient shorthands for common variations
on PDL slicing. The general syntax is

    $pdl(<slice>;<modifier>)

Four modifiers are currently implemented:

=over

=item *

C<_> : I<flatten> the piddle before applying the slice expression. Here
is an example

   $b = sequence 3, 3;
   print $b(0:-2;_); # same as $b->flat->(0:-2)
 [0 1 2 3 4 5 6 7]

which is quite different from the same slice expression without the modifier

   print $b(0:-2);
 [
  [0 1]
  [3 4]
  [6 7]
 ]

=item *

C<|> : L<sever|PDL::Core/sever> the link to the piddle, e.g.

   $a = sequence 10;
   $b = $a(0:2;|)++;  # same as $a(0:2)->sever++
   print $b;
 [1 2 3]
   print $a; # check if $a has been modified
 [0 1 2 3 4 5 6 7 8 9]

=item *

C<?> : short hand to indicate that this is really a
L<where|PDL::Primitive/where> expression

As expressions like

  $a->where($a>5)

are used very often you can write that shorter as

  $a($a>5;?)

With the C<?>-modifier the expression preceding the modifier is I<not>
really a slice expression (e.g. ranges are not allowed) but rather an
expression as required by the L<where|PDL::Primitive/where> method.
For example, the following code will raise an error:

  $a = sequence 10;
  print $a(0:3;?);
 syntax error at (eval 70) line 3, near "0:"

That's about all there is to know about this one.

=item *

C<-> : I<squeeze> out any singleton dimensions. In less technical terms:
reduce the number of dimensions (potentially) by deleting all
dims of size 1. It is equivalent to doing a L<reshape|PDL::Core/reshape>(-1).
That can be very handy if you want to simplify
the results of slicing operations:

  $a = ones 3, 4, 5;
  $b = $a(1,0;-); # easier to type than $a((1),(0))
  print $b->info;
 PDL: Double D [5]

It also provides a unique opportunity to have smileys in your code!
Yes, PDL gives new meaning to smileys.

=back

=head2 Combining modifiers

Several modifiers can be used in the same expression, e.g.

  $c = $a(0;-|); # squeeze and sever

Other combinations are just as useful, e.g. C<;_|> to flatten and
sever. The sequence in which modifiers are specified is not important.

A notable exception is the C<where> modifier (C<?>) which must not
be combined with other flags (let me know if you see a good reason
to relax this rule).

Repeating any modifier will raise an error:

  $c = $a(-1:1;|-|); # will cause error
 NiceSlice error: modifier | used twice or more

Modifiers are still a new and experimental feature of
C<PDL::NiceSlice>. I am not sure how many of you are actively using
them. I<Please do so and experiment with the syntax>. I think
modifiers are very useful and make life a lot easier.  Feedback is
welcome as usual. The modifier syntax will likely be further tuned in
the future but we will attempt to ensure backwards compatibility
whenever possible.

=head2 Argument formats

In slice expressions you can use ranges and secondly,
piddles as 1D index lists (although compare the description
of the C<?>-modifier above for an exception).

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

=head2 piddles as indices in ranges

You can use piddles to specify indices in ranges. No need to
turn them into proper perl scalars with the new slicing syntax.
However, make sure they contain not more than one element! Otherwise
a runtime error will be triggered. First a couple of examples that
illustrate proper usage:

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

=head1 IMPLEMENTATION

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

=head2 Conditional operator

The conditional operator can't be used in slice expressions (see
above).

=head2 The C<DATA> file handle

I<Note>: To avoid clobbering the C<DATA> filehandle C<PDL::NiceSlice>
switches itself off when encountering the C<__END__> or C<__DATA__> tokens.
This should not be a problem for you unless you use C<SelfLoader> to load
PDL code including the new slicing from that section. It is even desirable
when working with L<Inline::Pdlpp|Inline::Pdlpp>, see below.

=head2 Possible interaction with L<Inline::Pdlpp|Inline::Pdlpp>

There is currently an undesired interaction between C<PDL::NiceSlice>
and the new L<Inline::Pdlpp|Inline::Pdlpp> module (currently only in 
PDL CVS). Since PP code generally
contains expressions of the type C<$var()> (to access piddles, etc)
C<PDL::NiceSlice> recognizes those I<incorrectly> as
slice expressions and does its substitutions. This is not a problem
if you use the C<DATA> section for your Pdlpp code -- the recommended
place for Inline code anyway. In that case
C<PDL::NiceSlice> will have switched itself off before encountering any
Pdlpp code (see above):

    # use with Inline modules
  use PDL;
  use PDL::NiceSlice;
  use Inline Pdlpp;

  $a = sequence(10);
  print $a(0:5);

  __END__

  __Pdlpp__

  ... inline stuff


Otherwise switch C<PDL::NiceSlice> explicitly off around the
Inline::Pdlpp code:

  use PDL::NiceSlice;

  $a = sequence 10;
  $a(0:3)++;
  $a->inc;

  no PDL::NiceSlice; # switch off before Pdlpp code
  use Inline Pdlpp => "Pdlpp source code";

The cleaner solution is to always stick with the
C<DATA> way of including your C<Inline> code as
in the first example. That way you keep your nice Perl
code at the top and all the ugly Pdlpp stuff etc at
the bottom.

=head2 Bug reports

Feedback and bug reports are welcome. Please include an example
that demonstrates the problem. Log bug reports in the PDL
bug database at

  http://sourceforge.net/bugs/?group_id=612

or send them to the pdl-porters mailing list
E<lt>pdl-porters@jach.hawaii.eduE<gt>.


=head1 COPYRIGHT

Copyright (c) 2001, 2002 Christian Soeller. All Rights Reserved.
This module is free software. It may be used, redistributed
and/or modified under the same terms as PDL itself
(see http://pdl.perl.org).

=cut

1;
