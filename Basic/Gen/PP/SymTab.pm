# For making sure that no conflicts occur

package SymTab;
use Carp;

sub new {
	my($type,%ids) = @_;
	my($this) = bless {
		Id2Sym => {},
		Sym2Id => {},
		IsPar => {},
	}, $type;
	$this->add_ids(%ids);
	$this;
}

sub add_ids {
	my($this,%hash) = @_;
	for(keys %hash) {
		$this->{Id2Sym}{$_} = $hash{$_};
		
		# This usually sets the 'undef' key to whatever is in $_, because the
		# object in $hash{$_} is usually a scalar, not an array. I know this
		# becuase this function is called by AddArgsyms in PDL::PP, which
		# conructs the %hash to be
		# 
		#   sym_name => sym_name
		# 
		# The only other place that invokes this code is the constructor,
		# which itself is called by MkDefSyms in PDL::PP. That invocation is
		# called with %hash set as
		# 
		#   _PDL_ThisTrans => ["__privtrans",C::Type->new(undef,"$_[0] *foo")]
		# 
		# AFAIK, Sym2Id is never used anywhere in the code generation, and
		# the setting of undef throws warning messages, so I am going to
		# comment-out this line for now.  --David Mertens, 12-12-2011
		#$this->{Sym2Id}{$hash{$_}->[0]} = $_;
	}
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
