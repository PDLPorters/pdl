package Devel::REPL;

use Term::ReadLine;
use Moose;
use namespace::clean -except => [ 'meta' ];
use 5.008001; # backwards compat, doesn't warn like 5.8.1

our $VERSION = '1.003009_02'; # 1.3.9_02

with 'MooseX::Object::Pluggable';

use Devel::REPL::Error;

has 'term' => (
  is => 'rw', required => 1,
  default => sub { Term::ReadLine->new('Perl REPL') }
);

has 'prompt' => (
  is => 'rw', required => 1,
  default => sub { '$ ' }
);

has 'out_fh' => (
  is => 'rw', required => 1, lazy => 1,
  default => sub { shift->term->OUT || \*STDOUT; }
);

has 'exit_repl' => (
  is => 'rw', required => 1,
  default => sub { 0 }
);

sub run {
  my ($self) = @_;
  while ($self->run_once_safely) {
    # keep looping unless we want to exit REPL
    last if $self->exit_repl;
  }
}

sub run_once_safely {
  my ($self, @args) = @_;

  my $ret = eval { $self->run_once(@args) };

  if ($@) {
    my $error = $@;
    eval { $self->print("Error! - $error\n"); };
    return 1;
  } else {
    return $ret;
  }
}

sub run_once {
  my ($self) = @_;

  my $line = $self->read;
  return unless defined($line);     # undefined value == EOF

  my @ret = $self->formatted_eval($line);

  $self->print(@ret) unless $self->exit_repl;

  return 1;
}

sub formatted_eval {
  my ( $self, @args ) = @_;

  my @ret = $self->eval(@args);

  return $self->format(@ret);
}

sub format {
  my ( $self, @stuff ) = @_;

  if ( $self->is_error($stuff[0]) ) {
    return $self->format_error(@stuff);
  } else {
    return $self->format_result(@stuff);
  }
}

sub format_result {
  my ( $self, @stuff ) = @_;

  return @stuff;
}

sub format_error {
  my ( $self, $error ) = @_;
  return $error->stringify;
}

sub is_error {
  my ( $self, $thingy ) = @_;
  blessed($thingy) and $thingy->isa("Devel::REPL::Error");
}

sub read {
  my ($self) = @_;
  return $self->term->readline($self->prompt);
}

sub eval {
  my ($self, $line) = @_;
  my $compiled = $self->compile($line);
  return $compiled unless defined($compiled) and not $self->is_error($compiled);
  return $self->execute($compiled);
}

sub compile {
  my ( $_REPL, @args ) = @_;
  my $compiled = eval $_REPL->wrap_as_sub(@args);
  return $_REPL->error_return("Compile error", $@) if $@;
  return $compiled;
}

sub wrap_as_sub {
  my ($self, $line, %args) = @_;
  return qq!sub {\n!. ( $args{no_mangling} ? $line : $self->mangle_line($line) ).qq!\n}\n!;
}

sub mangle_line {
  my ($self, $line) = @_;
  return $line;
}

sub execute {
  my ($self, $to_exec, @args) = @_;
  my @ret = eval { $to_exec->(@args) };
  return $self->error_return("Runtime error", $@) if $@;
  return @ret;
}

sub error_return {
  my ($self, $type, $error) = @_;
  return Devel::REPL::Error->new( type => $type, message => $error );
}

sub print {
  my ($self, @ret) = @_;
  my $fh = $self->out_fh;
  no warnings 'uninitialized';
  print $fh "@ret";
  print $fh "\n" if $self->term->ReadLine =~ /Gnu/;
}

=head1 NAME

Devel::REPL - a modern perl interactive shell

=head1 SYNOPSIS

  my $repl = Devel::REPL->new;
  $repl->load_plugin($_) for qw(History LexEnv);
  $repl->run

Alternatively, use the 're.pl' script installed with the distribution

  system$ re.pl

=head1 DESCRIPTION

This is an interactive shell for Perl, commonly known as a REPL - Read,
Evaluate, Print, Loop. The shell provides for rapid development or testing
of code without the need to create a temporary source code file.

Through a plugin system, many features are available on demand. You can also
tailor the environment through the use of profiles and run control files, for
example to pre-load certain Perl modules when working on a particular project.

=head1 USAGE

To start a shell, follow one of the examples in the L</"SYNOPSIS"> above.

Once running, the shell accepts and will attempt to execute any code given. If
the code executes successfully you'll be shown the result, otherwise an error
message will be returned. Here are a few examples:

 $_ print "Hello, world!\n"
 Hello, world!
 1
 $_ nosuchfunction
 Compile error: Bareword "nosuchfunction" not allowed while "strict subs" in use at (eval 130) line 5.
  
 $_ 

In the first example above you see the output of the command (C<Hello,
world!>), if any, and then the return value of the statement (C<1>). Following
that example, an error is returned when the execution of some code fails.

Note that the lack of semicolon on the end is not a mistake - the code is
run inside a Block structure (to protect the REPL in case the code blows up),
which means a single statement doesn't require the semicolon. You can add one
if you like, though.

If you followed the first example in the L</"SYNOPSIS"> above, you'll have the
History and LexEnv plugins loaded (and there are many more available).
Although the shell might support "up-arrow" history, the History plugin adds
"bang" history to that so you can re-execute chosen commands (with e.g.
C<!53>). The LexEnv plugin ensures that lexical variables declared with the
C<my> keyword will automatically persist between statements executed in the
REPL shell.

When you C<use> any Perl module, the C<import()> will work as expected - the
exported functions from that module are available for immediate use:

 $_ carp "I'm dieeeing!\n"
 String found where operator expected at (eval 129) line 5, near "carp "I'm dieeeing!\n""
         (Do you need to predeclare carp?)
 Compile error: syntax error at (eval 129) line 5, near "carp "I'm dieeeing!\n""
 BEGIN not safe after errors--compilation aborted at (eval 129) line 5.
 
 $_ use Carp 
  
 $_ carp "I'm dieeeing!\n"
 I'm dieeeing!
  at /usr/share/perl5/Lexical/Persistence.pm line 327
 1
 $_ 

To quit from the shell, hit C<control+d> or C<control+c>.

=head2 Run Control Files

For particular projects you might well end up running the same commands each
time the REPL shell starts up - loading Perl modules, setting configuration,
and so on. A run control file lets you have this done automatically, and you
can have multiple files for different projects.

By default the C<re.pl> program looks for C<< $HOME/.re.pl/repl.rc >>, and
runs whatever code is in there as if you had entered it at the REPL shell
yourself.

To set a new run control file that's also in that directory, pass it as a
filename like so:

 system$ re.pl --rcfile myproject.pc

If the filename happens to contain a forwardslash, then it's used absolutely,
or realive to the current working directory:

 system$ re.pl --rcfile /path/to/my/project/repl.rc

Within the run control file you might want to load plugins. This is covered in
L</"The REPL shell object"> section, below.

=head2 Profiles

To allow for the sharing of run control files, you can fashion them into a
Perl module for distribution (perhaps via the CPAN). For more information on
this feature, please see the L<Devel::REPL::Profile> manual page.

A default profile ships with C<Devel::REPL>; it loads the following plugins:

=over 4

=item *

L<Devel::REPL::Plugin::History>

=item *

L<Devel::REPL::Plugin::LexEnv>

=item *

L<Devel::REPL::Plugin::DDS>

=item *

L<Devel::REPL::Plugin::Packages>

=item *

L<Devel::REPL::Plugin::Commands>

=item *

L<Devel::REPL::Plugin::MultiLine::PPI>

=back

=head2 Plugins

Plugins are a way to add funcionality to the REPL shell, and take advantage of
C<Devel::REPL> being based on the L<Moose> object system for Perl 5. This
means it's simple to 'hook into' many steps of the R-E-P-L process. Plugins
can change the way commands are interpreted, or the way their results are
output, or even add commands to the shell environment.

A number of plugins ship with C<Devel::REPL>, and more are available on the
CPAN. Some of the shipped plugins are loaded in the default profile, mentioned
above.

Writing your own plugins is not difficult, and is discussed in the
L<Devel::REPL::Plugin> manual page, along with links to the manual pages of
all the plugins shipped with C<Devel::REPL>.

=head2 The REPL shell object

From time to time you'll want to interact with or manipulate the
C<Devel::REPL> shell object itself; that is, the instance of the shell you're
currently running.

The object is always available through the C<$_REPL> variable. One common
requirement is to load an additional plugin, after your profile and run
control files have already been executed:

 $_ $_REPL->load_plugin('Timing');
 1
 $_ print "Hello again, world!\n"
 Hello again, world!
 Took 0.00148296356201172 seconds.
 1
 $_

=head1 REQUIREMENTS

In addition to the contents of the standard Perl distribution, you will need
the following:

=over 4

=item *

L<Moose> >= 0.64

=item *

L<MooseX::Object::Pluggable> >= 0.0009

=item *

L<MooseX::Getopt> >= 0.15

=item *

L<MooseX::AttributeHelpers> >= 0.14

=item *

L<namespace::clean>

=item *

L<File::HomeDir>

=item *

L<Task::Weaken>

=back

Optionally, some plugins if installed will require the following modules:

=over 4

=item *

L<PPI>

=item *

L<Data::Dump::Streamer>

=item *

L<File::Next>

=item *

L<B::Keywords>

=item *

L<Lexical::Persistence>

=item *

L<App::Nopaste>

=item *

L<Module::Refresh>

=back

=head1 AUTHOR

Matt S Trout - mst (at) shadowcatsystems.co.uk (L<http://www.shadowcatsystems.co.uk/>)

=head1 CONTRIBUTORS

=over 4

=item Stevan Little - stevan (at) iinteractive.com

=item Alexis Sukrieh - sukria+perl (at) sukria.net

=item epitaph

=item mgrimes - mgrimes (at) cpan dot org

=item Shawn M Moore - sartak (at) gmail.com

=item Oliver Gorwits - oliver on irc.perl.org

=item Andrew Moore - C<< <amoore@cpan.org> >>

=item Norbert Buchmuller C<< <norbi@nix.hu> >>

=item Dave Houston C<< <dhouston@cpan.org> >>

=back

=head1 LICENSE

This library is free software under the same terms as perl itself

=cut

1;
