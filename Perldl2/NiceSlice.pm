package Devel::REPL::Plugin::NiceSlice;

use Devel::REPL::Plugin;

use namespace::clean -except => [ 'meta' ];

use PDL::Lite;
use PDL::NiceSlice;

my $preproc = sub {
   my ($txt) = @_;
   my $new = PDL::NiceSlice::perldlpp('main',$txt);
   return $new;
};

around 'compile' => sub {

  my ($orig, $self) = (shift, shift);
  my ($lines, @args) = @_;

  no PDL::NiceSlice;
  $lines = $preproc->($lines);

  $self->$orig($lines, @args);
};

1;
