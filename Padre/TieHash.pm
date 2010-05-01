package Term::ReadLine::Perl::TieHash;

sub TIEHASH { bless {} }
sub DESTROY {}

sub STORE {
  my ($self, $name) = (shift, shift);
  $ {'readline::rl_' . $name} = shift;
}

sub FETCH {
  my ($self, $name) = (shift, shift);
  $ {'readline::rl_' . $name};
}

sub EXISTS {
   my ($self, $name) = @_;
   return exists $readline::{'rl_' . $name};
}

{
   my (@rl_keys);

   sub FIRSTKEY {
      @rl_keys = sort grep { m/^rl_\w/ } keys %readline:: ;
      my $key = substr shift(@rl_keys), 3;
      return wantarray ? ($key, ${'readline::rl_' . $key}) : $key;
   }

   sub NEXTKEY {
      my $key = substr shift(@rl_keys), 3;
      return wantarray ? ($key, ${'readline::rl_' . $key}) : $key;
   }
}

sub SCALAR {}

1;
