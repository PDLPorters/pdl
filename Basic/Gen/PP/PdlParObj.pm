package PDL::PP::PdlParObj;

use strict;
use warnings;
use Carp;
use PDL::Types ':All';

our %INVALID_PAR = map +($_=>1), qw(
  I
);

my $typeregex = join '|', map $_->ppforcetype, types;
my $complex_regex = join '|', qw(real complex);
our $sqbr_re = qr/\[([^]]*)\]/x;
our $pars_re = qr/^
	\s*(?:($complex_regex|$typeregex)\b([+]*)|)\s*	# $1,2: first option then plus
	(?:$sqbr_re)?\s*	# $3: The initial [option] part
	(\w+)			# $4: The name
	\(([^)]*)\)		# $5: The indices
/x;
my %flag2info = (
  io => [[qw(FlagW)]],
  nc => [[qw(FlagNCreat)]],
  o => [[qw(FlagOut FlagCreat FlagW)]],
  oca => [[qw(FlagOut FlagCreat FlagW FlagCreateAlways)]],
  t => [[qw(FlagTemp FlagCreat FlagW)]],
  phys => [[qw(FlagPhys)]],
  real => [[qw(FlagReal)]],
  complex => [[qw(FlagComplex)]],
  (map +($_->ppforcetype => [[qw(FlagTyped)], 'Type']), types),
);
my %flag2c = qw(
  FlagReal PDL_PARAM_ISREAL
  FlagComplex PDL_PARAM_ISCOMPLEX
  FlagTyped PDL_PARAM_ISTYPED
  FlagTplus PDL_PARAM_ISTPLUS
  FlagCreat PDL_PARAM_ISCREAT
  FlagCreateAlways PDL_PARAM_ISCREATEALWAYS
  FlagOut PDL_PARAM_ISOUT
  FlagTemp PDL_PARAM_ISTEMP
  FlagW PDL_PARAM_ISWRITE
  FlagPhys PDL_PARAM_ISPHYS
  FlagIgnore PDL_PARAM_ISIGNORE
);
sub new {
	my($type,$string,$badflag,$sig) = @_;
	$badflag ||= 0;
	my $this = bless {Number => "PDL_UNDEF_NUMBER", BadFlag => $badflag, Sig => $sig},$type;
	# Parse the parameter string. Note that the regexes for this match were
	# originally defined here, but were moved to PDL::PP for FullDoc parsing.
	$string =~ $pars_re
		 or confess "Invalid pdl def $string (regex $pars_re)\n";
	my($opt1,$opt_plus,$sqbr_opt,$name,$inds) = map $_ // '', $1,$2,$3,$4,$5;
	print "PDL: '$opt1$opt_plus', '$sqbr_opt', '$name', '$inds'\n"
		  if $::PP_VERBOSE;
	croak "Invalid Pars name: $name"
	  if $INVALID_PAR{$name};
# Set my internal variables
	$this->{Name} = $name;
	$this->{Flags} = [(split ',',$sqbr_opt),($opt1?$opt1:())];
	for(@{$this->{Flags}}) {
		confess("Invalid flag $_ given for $string\n")
			unless my ($set, $store) = @{ $flag2info{$_} || [] };
		$this->{$store} = $_ if $store;
		$this->{$_} = 1 for @$set;
	}
	if ($this->{FlagTyped} && $opt_plus) {
	  $this->{FlagTplus} = 1;
	}
	$this->{Type} &&= PDL::Type->new($this->{Type});
	if($this->{FlagNCreat}) {
		delete $this->{FlagCreat};
		delete $this->{FlagCreateAlways};
	}
	$this->{RawInds} = [map{
		s/\s//g; 		# Remove spaces
		$_;
	} split ',', $inds];
	return $this;
}

sub cflags {
  my ($this) = @_;
  map $flag2c{$_}, grep $this->{$_}, sort keys %flag2c;
}

sub name {return (shift)->{Name}}

sub add_inds {
	my($this,$dimsobj) = @_;
	$this->{IndObjs} = [map {$dimsobj->get_indobj_make($_)}
		@{$this->{RawInds}}];
	my %indcount;
	$this->{IndCounts} = [
		map {
			0+($indcount{$_->name}++);
		} @{$this->{IndObjs}}
	];
	$this->{IndTotCounts} = [
		map {
			($indcount{$_->name});
		} @{$this->{IndObjs}}
	];
}


# do the dimension checking for perl level broadcasting
# assumes that IndObjs have been created
sub perldimcheck {
  my ($this,$pdl) = @_;
  croak ("can't create ".$this->name) if $pdl->isnull &&
    !$this->{FlagCreat};
  return 1 if $pdl->isnull;
  my $rdims = @{$this->{RawInds}};
  croak ("not enough dimensions for ".$this->name)
    if ($pdl->broadcastids)[0] < $rdims;
  my @dims = $pdl->dims;
  my ($i,$ind) = (0,undef);
  for $ind (@{$this->{IndObjs}}) {
    $ind->add_value($dims[$i++]);
  }
  return 0; # not creating
}

sub finalcheck {
  my ($this,$pdl) = @_;
  return [] if $pdl->isnull;
  my @corr = ();
  my @dims = $pdl->dims;
  my ($i,$ind) = (0,undef);
  for $ind (@{$this->{IndObjs}}) {
    push @corr,[$i-1,$ind->{Value},$dims[$i-1]] if $dims[$i++] != $ind->{Value};
  }
  return [@corr];
}

# get index sizes for a parameter that has to be created
sub getcreatedims {
  my $this = shift;
  return map
    { croak "can't create: index size ".$_->name." not initialised"
	if !defined($_->{Value}) || $_->{Value} < 1;
      $_->{Value} } @{$this->{IndObjs}};
}

sub adjusted_type {
  my ($this, $generic) = @_;
  confess "adjusted_type given undefined generic type\n" if !defined $generic;
  return $generic->realversion if $this->{FlagReal};
  return $generic->complexversion if $this->{FlagComplex};
  return $generic unless $this->{FlagTyped};
  return $this->{Type}->numval > $generic->numval
    ? $this->{Type} : $generic
    if $this->{FlagTplus};
  $this->{Type};
}

sub get_nname{ my($this) = @_;
	"(\$PRIV(pdls[$this->{Number}]))";
}

sub get_nnflag { my($this) = @_;
	"(\$PRIV(vtable->per_pdl_flags[$this->{Number}]))";
}

sub get_incname {
	my($this,$ind,$for_local) = @_;
	return "inc_sizes[PDL_INC_ID(__privtrans->vtable,$this->{Number},$ind)]" if !$for_local;
	if($this->{IndTotCounts}[$ind] > 1) {
	    "__inc_".$this->{Name}."_".($this->{IndObjs}[$ind]->name).$this->{IndCounts}[$ind];
	} else {
	    "__inc_".$this->{Name}."_".($this->{IndObjs}[$ind]->name);
	}
}

sub get_incregisters {
	my($this) = @_;
	if(scalar(@{$this->{IndObjs}}) == 0) {return "";}
	(join '',map {
		my $x = $_;
		my ($name, $for_local) = map $this->get_incname($x, $_), 0, 1;
		"register PDL_Indx $for_local = __privtrans->$name; (void)$for_local;\n";
	} (0..$#{$this->{IndObjs}}) )
}

# Print an access part.
sub do_access {
	my($this,$inds,$context) = @_;
	my $pdl = $this->{Name};
# Parse substitutions into hash
	my %subst = map
	 {/^\s*(\w+)\s*=>\s*(\S*)\s*$/ or confess "Invalid subst $_ in ($inds) (no spaces in => value)\n"; ($1,$2)}
		PDL::PP::Rule::Substitute::split_cpp($inds);
# Generate the text
	my $text;
	$text = "(${pdl}_datap)"."[";
	$text .= join '+','0',map {
		$this->do_indterm($pdl,$_,\%subst,$context);
	} (0..$#{$this->{IndObjs}});
	$text .= "]";
# If not all substitutions made, the user probably made a spelling
# error. Barf.
	if(scalar(keys %subst) != 0) {
		confess("Substitutions left: ".(join ',',sort keys %subst)."\n");
	}
       $text;
}

sub do_pdlaccess {
	my($this) = @_;
	PDL::PP::pp_line_numbers(__LINE__-1, '$PRIV(pdls['.$this->{Number}.'])');
}

sub do_pointeraccess {
	my($this) = @_;
	return $this->{Name}."_datap";
}

sub do_physpointeraccess {
	my($this) = @_;
	return $this->{Name}."_physdatap";
}

sub do_indterm { my($this,$pdl,$ind,$subst,$context) = @_;
# Get informed
	my $indname = $this->{IndObjs}[$ind]->name;
	my $indno = $this->{IndCounts}[$ind];
	my $indtot = $this->{IndTotCounts}[$ind];
# See if substitutions
	my $substname = ($indtot>1 ? $indname.$indno : $indname);
	my $incname = $indname.($indtot>1 ? $indno : "");
	my $index;
	if(defined $subst->{$substname}) {$index = delete $subst->{$substname};}
	else {
# No => get the one from the nearest context.
		for(reverse @$context) {
			if($_->[0] eq $indname) {$index = $_->[1]; last;}
		}
	}
	if(!defined $index) {confess "Access Index not found: $pdl, $ind, $indname
		On stack:".(join ' ',map {"($_->[0],$_->[1])"} @$context)."\n" ;}
       return "(".($this->get_incname($ind,1))."*".
               "PP_INDTERM(".$this->{IndObjs}[$ind]->get_size().", $index))";
}

sub get_xsdatapdecl { 
    my($this,$ctype,$nulldatacheck) = @_;
    my $pdl = $this->get_nname;
    my $flag = $this->get_nnflag;
    my $name = $this->{Name};
    my $macro = "PDL_DECLARE_PARAMETER".($this->{BadFlag} ? "_BADVAL" : "");
    "$macro($ctype, $flag, $name, $pdl, $nulldatacheck)";
}

1;
