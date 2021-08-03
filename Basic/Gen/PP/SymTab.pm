# For making sure that no conflicts occur

package PDL::PP::SymTab;
use Carp;

sub new {
	my($type,%ids) = @_;
	my($this) = bless {
		Id2Sym => {},
		IsPar => {},
	}, $type;
	$this->add_ids(%ids);
	$this;
}

sub add_ids {
	my($this,%hash) = @_;
	@{$this->{Id2Sym}}{keys %hash} = values %hash;
}

sub add_params {
	my($this,%hash) = @_;
	$this->add_ids(%hash);
	for(keys %hash) {
		$this->{IsPar}{$_} = 1;
	}
}

sub decl_locals {
	my($this) = @_;
	my $str;
	for(keys %{$this->{Id2Sym}}) {
		if(!$this->{IsPar}{$_}) {
			$str .= $this->{Id2Sym}{$_}[1]
				   ->get_decl($this->{Id2Sym}{$_}[0]).";";
		}
	}
	$str;
}

sub get_params {
}

sub get_symname {
	my($this,$id) = @_;
	confess "Symbol not found: $id\n" if(!defined($this->{Id2Sym}{$id}));
	return $this->{Id2Sym}{$id}[0];
}

1;
