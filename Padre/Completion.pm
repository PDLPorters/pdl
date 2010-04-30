package Devel::REPL::Plugin::Completion;
use Devel::REPL::Plugin;
use Scalar::Util 'weaken';
use PPI;
use namespace::clean -except => [ 'meta' ];

has current_matches => (
  is => 'rw',
  isa => 'ArrayRef',
  lazy => 1,
  default => sub { [] },
);

has match_index => (
  is => 'rw',
  isa => 'Int',
  lazy => 1,
  default => sub { 0 },
);

has no_term_class_warning => (
  isa => "Bool",
  is  => "rw",
  default => 0,
);

before 'read' => sub {
  my ($self) = @_;

  if ((!$self->term->isa("Term::ReadLine::Gnu") and !$self->term->isa("Term::ReadLine::Perl"))
        and !$self->no_term_class_warning) {
     warn "Term::ReadLine::Gnu or Term::ReadLine::Perl is required for the Completion plugin to work";
     $self->no_term_class_warning(1);
  }

  my $weakself = $self;
  weaken($weakself);

  $self->term->Attribs->{attempted_completion_function} = sub {
    $weakself->_completion(@_);
  };

  $self->term->Attribs->{completion_function} = sub {
    $weakself->_completion(@_);
  };
};

sub _completion {
  my $is_trp = scalar(@_) == 4 ? 1 : 0;
  my ($self, $text, $line, $start, $end) = @_;
  $end = $start+length($text) if $is_trp;

  # we're discarding everything after the cursor for completion purposes
  # we can't just use $text because we want all the code before the cursor to
  # matter, not just the current word
  substr($line, $end) = '';

  my $document = PPI::Document->new(\$line);
  return unless defined($document);

  $document->prune('PPI::Token::Whitespace');

  my @matches = $self->complete($text, $document);

  # iterate through the completions
  if ($is_trp) {
     return @matches;
  } else {
     return $self->term->completion_matches($text, sub {
           my ($text, $state) = @_;

           if (!$state) {
              $self->current_matches(\@matches);
              $self->match_index(0);
           }
           else {
              $self->match_index($self->match_index + 1);
           }

           return $self->current_matches->[$self->match_index];
        });
  }
}

sub complete {
  return ();
}

# recursively find the last element
sub last_ppi_element {
  my ($self, $document, $type) = @_;
  my $last = $document;
  while ($last->can('last_element') && defined($last->last_element)) {
    $last = $last->last_element;
    return $last if $type && $last->isa($type);
  }
  return $last;
}

1;

__END__

=head1 NAME

Devel::REPL::Plugin::Completion - Extensible tab completion

=head1 AUTHOR

Shawn M Moore, C<< <sartak at gmail dot com> >>

=cut

