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
	my $this = bless {Name => $name},$type;
	return $this;
}

# set the value of an index, also used by perl level threading
sub add_value {
	my($this,$val) = @_;
	croak("index values for $this->{Name} must be positive")
	  unless $val > 0;
	if(defined $this->{Value}) {
	  if ($this->{Value} == -1 || $this->{Value} == 1)
	    { $this->{Value} = $val }
	  elsif($val != 1 && $val != $this->{Value}) {
	    croak("For index $this->{Name} conflicting values $this->{Value} and $val given\n");
		}
	} else {
		$this->{Value} = $val;
	}
}

# This index will take its size value from outside parameter ...
sub set_from { my($this,$otherpar) = @_;
	$this->{From} = $otherpar;
}

sub name {return (shift)->{Name}}

sub get_decldim { my($this) = @_;
	return "PDL_Long __$this->{Name}_size;";
}

sub get_initdim { my($this) = @_;
	my $init = '-1';
	$init = "\$COMP(".$this->{From}->{ProtoName}.")"
	  if $this->{From};
	$init = $this->{Value} if defined $this->{Value};
	"\$PRIV(__$this->{Name}_size) = $init;"
}

sub get_copydim { my($this,$fromsub,$tosub) = @_;
	my($iname) = "__$this->{Name}_size";
	&$tosub($iname) ."=". &$fromsub($iname) .";" ;
}

sub get_size { my($this) = @_;
	"\$PRIV(__$this->{Name}_size)"
}

1;
