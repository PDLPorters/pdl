# Represent any C type.
# Type contains the size of arrays, which is either constant
# or resolved (into an object) from resolveobj.

package C::Type;
use Carp;

# new C::Type(resolveobj,str)

sub new {
	my $this = bless {},shift;
	$this->{Resolve} = shift;
	if(@_) {
		$this->parsefrom(shift);
	}
	return $this;
}

sub stripptrs {
	my($this,$str) = @_;
	if($str =~ /^\s*\w+\s*$/) {
		$str =~ s/\s//g;
		$this->{ProtoName} = $str;
		return [];
	} else {
# Now, recall the different C syntaxes. First priority is a pointer:
		my $decl;
		if($str =~ /^\s*\*(.*)$/) {
			$decl = $this->stripptrs($1);
			unshift @$decl,"PTR";
		} elsif($str =~ /^\s*\(.*\)\s*$/) {
# XXX Should try to see if a funccall.
			return $this->stripptrs($1);
		} elsif($str =~ /^(.*)\[([^]]+)\]\s*$/) {
			my $siz = $2;
			print "ARR($str): ($siz)\n" if $::PP_VERBOSE;
			$decl = $this->stripptrs($1);
			unshift @$decl,"ARR($siz)";
			print "ARR($str): ($siz)\n" if $::PP_VERBOSE;
		} else {
			die("Invalid C type '$str'");
		}
		return $decl;
	}
}

# XXX Correct to *real* parsing. This is only a subset.
sub parsefrom {
	my($this,$str) = @_;
# First, take the words in the beginning
	$str =~ /^\s*((?:\w+\b\s*)+)([^[].*)$/;
	my $base = $1; my $decl = $2;
	my $foo = $this->stripptrs($decl);
	$this->{Base} = $base;
	$this->{Chain} = $foo;
}

sub get_decl {
	my($this,$name,$opts) = @_;
	for(@{$this->{Chain}}) {
		if($_ eq "PTR") {$name = "*$name"}
		elsif($_ =~/^ARR\((.*)\)$/) {
			if($opts->{VarArrays2Ptrs}) {
				$name = "*$name";
			} else {
				$name = "($name)[$1]";
			}
		}
		else { confess("Invalid decl") }
	}
	return "$this->{Base} $name";
}

# Useful when parsing argument decls
sub protoname { return shift->{ProtoName} }

sub get_copy {
	my($this,$from,$to) = @_;
	my ($prev,$close);
	if($#{$this->{Chain}} >= 0) {
		# strdup loses portability :(
		return "($to) = malloc(strlen($from)+1); strcpy($to,$from);"
		 if $this->{Base} =~ /^\s*char\s*$/;
                return "($to) = newSVsv($from);"
                 if $this->{Base} =~ /^\s*SV\s*$/;
		my $code = $this->get_malloc($to,$from);
		my ($deref0,$deref1) = ($from,$to);
		for(@{$this->{Chain}}) {
			if($_ eq "PTR") {confess("Cannot alloc pointer, must be array");}
			elsif($_ =~/^ARR\((.*)\)$/) {
				$no++;
				$prev .= "
				  if(!$deref0) {$deref1=0;}
				  else {int __malloc_ind_$no;
					for(__malloc_ind_$no = 0;
						__malloc_ind_$no < $1;
						__malloc_ind_$no ++) {";
				$deref0 = $deref0."[__malloc_ind_$no]";
				$deref1 = $deref1."[__malloc_ind_$no]";
				$close .= "}}";
			} else { confess("Invalid decl $_") }
		}
		$code .= "$prev $deref1 = $deref0; $close";
		return $code;
	}
	return "($to) = ($from);";
}

sub get_free {
	my($this,$from) = @_;
	my ($prev,$close);
	if($#{$this->{Chain}} >= 0) {
		return "free($from);"
		 if $this->{Base} =~ /^\s*char\s*$/;
                return "SvREFCNT_dec($from);"
                 if $this->{Base} =~ /^\s*SV\s*$/;
		my @mallocs;
		my $str = "{";
		my $deref = "$from";
		my $prev = undef;
		my $close = undef;
		my $no = 0;
		for(@{$this->{Chain}}) {
			$no++;
			if($no > 1) {croak("Can only free one layer!\n");}
#			if($_ eq "PTR") {confess("Cannot free pointer, must be array ;) (FIX CType.pm)");}
			return "free($from);\n ";
		}
	} else {
		"";
	}
}

sub need_malloc {
	my($this) = @_;
	return scalar grep /(ARR|PTR)/,(@{$this->{Chain}})
}

# Just returns with the array string.
sub get_malloc {
	my($this,$assignto) = @_;
	my $str = "{";
	my $deref = "$assignto";
	my $prev = undef;
	my $close = undef;
	my $no = 0;
	for(@{$this->{Chain}}) {
		if($_ eq "PTR") {confess("Cannot alloc pointer, must be array");}
		elsif($_ =~/^ARR\((.*)\)$/) {
			$str .= "$prev $assignto =
				malloc(sizeof(* $assignto) * $1);
				";
			$no++;
			$prev = "{int __malloc_ind_$no;
				for(__malloc_ind_$no = 0;
					__malloc_ind_$no < $1;
					__malloc_ind_$no ++) {";
			$deref = $deref."[__malloc_ind_$no]";
			$close .= "}}";
		} else { confess("Invalid decl $_") }
	}
	$str .= "}";
	return $str;
}

sub getvar {
}

# Determine if everything constant and can just declare
sub need_alloc {
}

sub alloccode {
}

sub copycode {
}

sub freecode {
}

1;
