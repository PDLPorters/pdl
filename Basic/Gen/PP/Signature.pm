=head1 NAME

PDL::PP::Signature - Internal module to handle signatures

=head1 DESCRIPTION

Internal module to handle signatures

=head1 SYNOPSIS

 use PDL::PP::Signature;


=cut

package PDL::PP::Signature;
use PDL::PP::PdlParObj;
use PDL::PP::Dims;
use Carp;
use SelfLoader;

@ISA = qw/ SelfLoader /;

# we pass on $bvalflag to the PdlParObj's created by parse
# (a hack for PdlParObj::get_xsdatapdecl() which should
# disappear when (if?) things are done sensibly)
#
sub new {
  my ($type,$str,$bvalflag) = @_;
  $bvalflag ||= 0;
  my ($namep,$objp) = parse($str,$bvalflag);
  return bless {Names => $namep, Objects => $objp},$type;
}

*with = \&new;

1;

=head1 AUTHOR

Copyright (C) Tuomas J. Lukka 1997 (lukka@husc.harvard.edu) and by Christian
Soeller (c.soeller@auckland.ac.nz).
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut

__DATA__

# Eliminate whitespace entries
sub nospacesplit {map {/^\s*$/?():$_} split $_[0],$_[1]}


sub names {
  my $this = shift;
  return $this->{Names};
}

sub objs {
  my $this = shift;
  return $this->{Objects};
}

# Pars -> ParNames, Parobjs
sub parse {
	my($str,$bvalflag) = @_;
	my @entries = nospacesplit ';',$str;
	my $number = 0;
	my %objs; my @names; my $obj;
	for (@entries) {
		$obj = PDL::PP::PdlParObj->new($_,"PDL_UNDEF_NUMBER",$bvalflag);
		push @names,$obj->name;
		$objs{$obj->name} = $obj;
	}
	return (\@names,\%objs,1);
}


sub realdims {
  my $this = shift;
  my @rds = map { scalar @{$this->{Objects}->{$_}->{RawInds}}}
         @{$this->{Names}};
#  print "Realdims are ".join(',',@rds)."\n";
  return \@rds;
}

sub creating {
  my $this = shift;
#  my @creat = map { $this->{Objects}->{$_}->{FlagCreat} ? 1:0 }
#   @{$this->{Names}};
#  print "Creating is ".join(',',@creat)."\n";
  croak "you must perform a checkdims before calling creating"
    unless defined $this->{Create};
  return $this->{Create};
}

sub getinds {
  my $this = shift;
  $this->{Dims} = new PDL::PP::PdlDimsObj;
  for (@{$this->{Names}}) {
    $this->{Objects}->{$_}->add_inds($this->{Dims});
  }
}

sub resetinds {
  my $this = shift;
  for (keys %{$this->{Dims}}) {$this->{Dims}->{$_}->{Value} = undef;}
}
sub checkdims {
  my $this = shift;
  $this->getinds;  # we have to recreate to keep defaults currently
  my $n = @{$this->{Names}};
  croak "not enough pdls to match signature" unless $#_ >= $n-1;
  my @pdls = @_[0..$n-1];
  if ($PDL::debug) { print "args: ".
		     join(' ,',map { "[".join(',',$_->dims)."]," } @pdls)
		       . "\n"}
  my $i = 0;
  my @creating = map $this->{Objects}->{$_}->perldimcheck($pdls[$i++]),
         @{$this->{Names}};
  $i = 0;
  for (@{$this->{Names}}) {
    push @creating, $this->{Objects}->{$_}->getcreatedims
      if $creating[$i++];
  }
  $this->{Create} = \@creating;
  $i = 0;
  my $corr = 0;
  for (@{$this->{Names}}) {
    $corr = $this->{Objects}->{$_}->finalcheck($pdls[$i++]);
    next unless $#$corr>-1;
    my ($j,$str) = (0,"");
    for (@$corr) {$str.= ":,"x($_->[0]-$j)."(0),*$_->[1],";
			$j=$_->[0]+1 }
    chop $str;
    $_[$i-1] = $pdls[$i-1]->slice($str);
  }
}
