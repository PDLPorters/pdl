package PDL::Demos;

use strict;
use warnings;
use Carp;
use Exporter;
require File::Spec;

our @ISA="Exporter";
our @EXPORT = qw/comment act actnw output/;

sub comment($) {
   local $SIG{__DIE__} = \&Carp::confess;
   print "----\n";
   print $_[0];
   my $prompt = "---- (press enter)";
   defined($PERLDL::TERM) ? $PERLDL::TERM->readline($prompt) : ( print $prompt, <> );
}

sub act($) {
   local $SIG{__DIE__} = \&Carp::confess;
   actnw($_[0], (caller)[0]);
   my $prompt = "---- (press enter)";
   defined($PERLDL::TERM) ? $PERLDL::TERM->readline($prompt) : ( print $prompt, <> );
}

sub _eval_pkg {
  my ($txt, $pack) = @_;
  eval "package $pack; no strict; use PDL; $txt";
}
sub actnw($) {
   local $SIG{__DIE__} = \&Carp::confess;
   my ($script, $pack) = @_;
   $script =~ s/^(\s*)output/$1print/mg;
   print "---- Code:";
   print $script;
   print "---- Output:\n";
   _eval_pkg($script, $pack // (caller)[0]);
   print "----\n";
   print "----\nOOPS!!! Something went wrong, please make a bug report!: $@\n----\n" if $@;
}

sub output {local $SIG{__DIE__} = \&Carp::confess; print @_;}

my ($searched, @found);
my @d = qw(PDL Demos);
sub list {
  return @found if $searched;
  $searched = 1;
  my %found_already;
  foreach my $path ( @INC ) {
    next if !-d (my $dir = File::Spec->catdir( $path, @d ));
    my @c = do { opendir my $dirfh, $dir or die "$dir: $!"; grep !/^\./, readdir $dirfh };
    for my $f (grep /\.pm$/ && -f File::Spec->catfile( $dir, $_ ), @c) {
      $f =~ s/\.pm//;
      my $found_mod = join "::", @d, $f;
      next if $found_already{$found_mod}++;
      push @found, $found_mod;
    }
    for my $t (grep -d $_->[1], map [$_, File::Spec->catdir( $dir, $_ )], @c) {
      my ($subname, $subd) = @$t;
      # one extra level
      my @c = do { opendir my $dirfh, $subd or die "$subd: $!"; grep !/^\./, readdir $dirfh };
      for my $f (grep /\.pm$/ && -f File::Spec->catfile( $subd, $_ ), @c) {
        $f =~ s/\.pm//;
        my $found_mod = join "::", @d, $subname, $f;
        next if $found_already{$found_mod}++;
        push @found, $found_mod;
      }
    }
  }
  @found;
}

my ($kw_loaded, %kw2info); # info = [kw, description, module]
sub _load_keywords {
  return if $kw_loaded;
  $kw_loaded = 1;
  # || do {warn "\n\n\n\nerror: $@";0}
  my @modules = grep eval "require $_; 1", __PACKAGE__->list;
  my %mod2i = map +($_ => [$_->info]), grep $_->can('info'), @modules;
  %kw2info = map +($mod2i{$_}[0] => [@{$mod2i{$_}}, $_]), keys %mod2i;
}
sub keywords { _load_keywords(); keys %kw2info; }
sub info {
  _load_keywords();
  my $info = $kw2info{$_[1]} || die "unknown demo $_[1]\n";
  @$info;
}
sub demo {
  my $pkg = ($_[0]->info($_[1]))[2];
  $pkg->can('demo') ? $pkg->demo : [comment=>"No demo data found for $_[1]\n"];
}
sub _proxy {
  my $method = shift;
  my $pkg = ($_[0]->info($_[1]))[2];
  return if !$pkg->can($method);
  _eval_pkg($pkg->$method, (caller 1)[0]);
}
sub init { _proxy('init', @_) }
sub done { _proxy('done', @_) }

=head1 NAME

PDL::Demos - PDL demo infrastructure

=head1 SYNOPSIS

  # in a demo, if text-orientated
  package PDL::Demos::Blah;
  sub info { ('blah', 'Longer description of demo') }
  sub init { 'use PDL::Graphics::PGPLOT;' }
  my @demo = (
    [comment => "Welcome to the Blah demo"],
    [act => <<'EOF'],
  output "PDL can make n-dimensional sequences:\n";
  output $x = sequence(2,3);
  EOF
  );
  sub demo { @demo }
  sub done { "# return things to previous state\n" }

  # a GUI-orientated one
  package PDL::Demos::GUIBlah;
  use GUIBlah; # so demo won't show up in list if GUIBlah not installed
  sub info { ('blahgui', 'GUIBlah demo') }
  sub demo {[actnw => q|
    # starting up the GUI demo app
    |.__PACKAGE__.q|::run();
  |]}
  sub run { # this is just a convention, but a good one
    # ...
  }

  # iterate a demo of your own module - call it PDL::Demos::(something)
  make && perl -Mblib -S perldl # run "demo" and it will see your demo

  # in a CLI or REPL
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
    $_->[0]->($_->[1]) for PDL::Demos->demo($_[0]);
    PDL::Demos->done($_[0]);
  }

=head1 DESCRIPTION

Provides utilities to make demos for PDL modules.

PDL demos should be in the C<PDL::Demos::*> namespace so that they can
be auto-discovered.

Please ensure that your demo module is included in a CPAN distribution
and add it to the appropriate metadata (e.g. C<Makefile.PL> and
C<MANIFEST>).

=head1 METHODS

=head2 list

Class method; goes through C<@INC> finding all modules starting with
C<PDL::Demos::> (with up to two C<::>-separated words). Cached after
first run. Does not distinguish demo modules that did not load.

=head2 keywords

Returns the list of keywords (first element of C<info> return-list)
of all found modules that loaded successfully and implement an C<info>
method. Caches results.

=head2 info

Given a keyword, returns the result of calling C<info> on the relevant
module plus the module name (three elements) or throws exception if
unknown keyword.

=head2 init

Given a keyword, C<eval>s the result of calling C<init> on the relevant
module if it has one, or throws exception if unknown keyword.

=head2 demo

Given a keyword, returns the result of calling C<demo> on the relevant
module or throws exception if unknown keyword.

=head2 done

Given a keyword, C<eval>s the result of calling C<done> on the relevant
module if it has one, or throws exception if unknown keyword.

=head1 DEMO MODULE METHODS

Each demo module must provide these class methods:

=over

=item info

Return a two-element list of strings: a single keyword (probably
lower-case), and a short description of the demo.  Both will be displayed
when a user enters C<demo> without giving a name.

=item demo

Returns a list of array-refs of two elements: a L<function|/FUNCTIONS>
provided by this module, and an argument for it.

=item init

Return a string of Perl code which will be evaluated in the package
running the demo. Use this e.g. for C<use> statements that import
functions needed in your demo.

=back

=head1 FUNCTIONS

These are all exported.

=head2 comment

Prints its argument, prompts user to press enter before returning.

=head2 output

Prints its argument (best for use in C<actnw> etc).

=head2 actnw

The argument must be a string containing valid Perl code.  The string
is printed with a separator, then evaluated as Perl code in the
package running the demo, with C<PDL> loaded. Doesn't prompt, so use
this for e.g. GUI demos that return when the user tells them to.

Multiline code string should start with a newline.

=head2 act

As above, but prompts before returning.

=head1 ERROR HANDLING

Check the prerequisites (e.g. optional Perl modules) for your demo in
your demo module and not only in the code string you pass to the
C<init> routine.  If the code in your demo module dies, then the demo
will not be offered in the demo overview.  Fatal errors in the init
routine will be printed and mess up the output layout.  Also, error
messages might be difficult to understand if users just want to run
the demo.

If you want to show the demo in the overview though it can't run in
the current situation, then make sure that your C<demo> method informs
the user what is missing, and where they can obtain it.

=head1 AUTHOR

Copyright (C) 1998 Tuomas J. Lukka.
Tweaks by Ed J for PDL 2.077, 2022.

=cut

1;
