package PDL::Demos;

use strict;
use warnings;
use Carp;
use Exporter;
require File::Spec;

our @ISA="Exporter";
our @EXPORT = qw/comment act actnw output/;

sub home() {
   if (-e '/usr/bin/tput') {
      system 'tput clear';
   } elsif ( $^O eq 'MSWin32' ) {
      system 'cls';
   }
}

sub comment($) {
   local $SIG{__DIE__} = \&Carp::confess;
   home();
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
   home();
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
  foreach my $path ( @INC ) {
    next if !-d (my $dir = File::Spec->catdir( $path, @d ));
    my @c = do { opendir my $dirfh, $dir or die "$dir: $!"; readdir $dirfh };
    my @files = grep /\.pm$/ && -f File::Spec->catfile( $dir, $_ ), @c;
    s/\.pm//, push(@found, "PDL::Demos::$_") for @files;
    for my $subd (grep !/^\./ && -d File::Spec->catdir( $dir, $_ ), @c) {
      # one extra level
      my @c = do { open my $dirfh, $dir or die "$dir: $!"; readdir $dirfh };
      my @files = grep /\.pm$/ && -f File::Spec->catfile( $dir, $_ ), @c;
      s/\.pm//, push(@found, "PDL::Demos::$subd\::$_") for @files;
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

  # in a demo
  package PDL::Demos::Blah;
  use PDL::Demos;
  my @demo = (
    [comment => "Welcome to the Blah demo"],
    [act => <<'EOF'],
  output "PDL can make n-dimensional sequences:\n";
  output $x = sequence(2,3);
  EOF
  );
  sub info { ('blah', 'Longer description of demo') }
  sub demo { @demo }
  sub init { 'use PDL::Graphics::PGPLOT;' }

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

Return a two-element list: a single keyword (probably lower-case),
and a short description of the demo.

=item demo

Returns a list of array-refs of two elements: a function provided by this
module, and an argument for it.

=item init

Return a string which will be evaluated in the package running the
demo. Use this for C<use> statements that import functions needed in
your demo.

=back

=head1 FUNCTIONS

These are all exported.

=head2 comment

Prints its argument, prompts user to press enter before returning.

=head2 output

Prints its argument (best for use in C<actnw> etc).

=head2 actnw

Prints its argument with a separator, then evaluates it as Perl code
with C<PDL> loaded. Doesn't prompt, so use this for e.g. GUI demos that
return when the user tells them to.

=head2 act

As above, but prompts before returning.

=head1 AUTHOR

Copyright (C) 1998 Tuomas J. Lukka.
Tweaks by Ed J for PDL 2.077, 2022.

=cut

1;
