##############################################
package PDL::PP::PdlDimsObj; # Hold more dims
use Carp;

sub new {
	my($type) = @_;
	bless {},$type;
}

sub get_indobj_make {
	my($this,$expr) = @_;
	$expr =~ /^([a-zA-Z0-9]+)(?:=([0-9]+))?$/ or confess "Invalid index expr '$expr'\n";
	my $name = $1; my $val = $2;
	my $indobj;
	if(defined $this->{$name}) {
		$indobj = $this->{$name};
	} else {
		$indobj = PDL::PP::Ind->new($name);
		$this->{$name}=$indobj;
	}
	if(defined $val) { $indobj->add_value($val); }
	return $indobj;
}

#####################################################################
#
# Encapsulate one index.

package PDL::PP::Ind;
use Carp;

sub new {
	my($type,$name) = @_;
	bless {Name => $name},$type;
}

# set the value of an index, also used by perl level threading
sub add_value {
	my($this,$val) = @_;
	croak("index values for $this->{Name} must be positive")
	  unless $val > 0;
	return $this->{Value} = $val if
		!defined $this->{Value} or
		$this->{Value} == -1 or
		$this->{Value} == 1;
	croak "For index $this->{Name} conflicting values $this->{Value} and $val given\n" if $val != 1 && $val != $this->{Value};
}

# This index will take its size value from outside parameter ...
sub set_from { my($this,$otherpar) = @_;
	$this->{From} = $otherpar;
}

sub name {$_[0]->{Name}}

sub get_decldim { "PDL_Indx ".$_[0]->get_priv.";"; }

sub get_initdim { my($this) = @_;
	my $init = $this->{Value} //
	  ($this->{From} ? "\$COMP(".$this->{From}{ProtoName}.")" : '-1');
	$this->get_size." = $init;"
}

sub get_size { my($this) = @_;
	"\$PRIV(".$this->get_priv.")"
}

sub get_priv { my($this) = @_;
	"__$this->{Name}_size"
}

1;
