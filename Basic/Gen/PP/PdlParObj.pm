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
	\((.*)\)		# $5: The indices
	\s*\Z			# that's all
/x;
my %flag2info = (
  io => [[qw(FlagW)]],
  o => [[qw(FlagOut FlagCreat FlagW)]],
  oca => [[qw(FlagOut FlagCreat FlagW FlagCreateAlways)]],
  t => [[qw(FlagTemp FlagCreat FlagW)]],
  phys => [[qw(FlagPhys)]],
  real => [[qw(FlagTypeOverride FlagReal)]],
  complex => [[qw(FlagTypeOverride FlagComplex)]],
  (map +($_->ppforcetype => [[qw(FlagTypeOverride FlagTyped)], 'Type']), types),
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
my $calc_re = qr{
  (\w+)\s*=\s*      # paren group 1 (dim name) - from perlre/PARNO
  CALC
  (                 # paren group 2 (parens)
    \(
      (             # paren group 3 (contents of parens)
      (?:
       (?> [^()]+ ) # Non-parens without backtracking
      |
       (?2)         # Recurse to start of paren group 2
      )*
      )
    \)
  )
}xo;
sub new {
  my ($type,$string,$badflag,$sig) = @_;
  $badflag ||= 0;
  my $this = bless {Number => "PDL_UNDEF_NUMBER", BadFlag => $badflag, Sig => $sig},$type;
  $string =~ $pars_re or croak "pp_def($this->{Sig}{OpName}): Invalid pdl def $string (regex $pars_re)\n";
  my($opt1,$opt_plus,$sqbr_opt,$name,$inds) = map $_ // '', $1,$2,$3,$4,$5;
  print "PDL: '$opt1$opt_plus', '$sqbr_opt', '$name', '$inds'\n"
    if $::PP_VERBOSE;
  croak "pp_def($this->{Sig}{OpName}): Invalid Pars name: $name" if $INVALID_PAR{$name};
# Set my internal variables
  $this->{Name} = $name;
  $this->{Flags} = [(split ',',$sqbr_opt),($opt1?$opt1:())];
  for(@{$this->{Flags}}) {
    croak("pp_def($this->{Sig}{OpName}): Invalid flag $_ given for $string\n")
      unless my ($set, $store) = @{ $flag2info{$_} || [] };
    $this->{$store} = $_ if $store;
    $this->{$_} = 1 for @$set;
  }
  $this->{FlagTplus} = 1 if $this->{FlagTyped} && $opt_plus;
  $this->{Type} &&= PDL::Type->new($this->{Type});
  $this->{Ind2Calc} = \my %ind2calc;
  $ind2calc{$1} = $3 while $inds =~ s#$calc_re#$1#;
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

sub name {$_[0]{Name}}

sub add_inds {
	my($this,$dimsobj) = @_;
	$this->{IndObjs} = [my @objs = map $dimsobj->get_indobj_make($_, $this->{Ind2Calc}{$_}), @{$this->{RawInds}}];
	my %indcount;
	$this->{IndCounts} = [ map 0+($indcount{$_->name}++), @objs ];
	$this->{IndTotCounts} = [ map $indcount{$_->name}, @objs ];
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
  my $i = 0;
  for my $ind (@{$this->{IndObjs}}) {
    push @corr,[$i-1,$ind->{Value},$dims[$i-1]] if $dims[$i++] != $ind->{Value};
  }
  return \@corr;
}

# get index sizes for a parameter that has to be created
sub getcreatedims {
  my $this = shift;
  return map
    { croak "pp_def($this->{Sig}{OpName}): can't create: index size ".$_->name." not initialised"
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
	"(\$PRIV(pdls)[$this->{Number}])";
}

sub get_substname {
  my($this,$ind) = @_;
  $this->{IndObjs}[$ind]->name.($this->{IndTotCounts}[$ind] > 1 ? $this->{IndCounts}[$ind] : '');
}

sub get_incname {
  my($this,$ind,$for_local) = @_;
  return "inc_sizes[PDL_INC_ID(__privtrans->vtable,$this->{Number},$ind)]" if !$for_local;
  "__inc_$this->{Name}_".$this->get_substname($ind);
}

sub get_incregisters {
	my($this) = @_;
	return '' if scalar(@{$this->{IndObjs}}) == 0;
	join '', map {
		my $x = $_;
		my ($name, $for_local) = map $this->get_incname($x, $_), 0, 1;
		"register PDL_Indx $for_local = __privtrans->$name; (void)$for_local;";
	} 0..$#{$this->{IndObjs}};
}

# Print an access part.
sub do_access {
  my($this,$inds,$context) = @_;
  my $pdl = $this->{Name};
  my %subst = map {
    if (!/^\s*(\w+)\s*=>\s*(\S*)\s*$/) {
      my $msg = "Invalid subst '$_' in \$$pdl($inds):";
      $msg .= " no '=>' seen" if !/=>/;
      $msg .= " invalid dim name '$1'" if /^\s*([^\w]*?)\s*=>/;
      $msg .= " (no spaces in => value)" if /=>\s*\S\s*\S/;
      croak "pp_def($this->{Sig}{OpName}): $msg\n";
    }
    ($1,$2)
  } PDL::PP::Rule::Substitute::split_cpp($inds);
  my $text = "(${pdl}_datap)[" .
    join('+','0', map $this->do_indterm($pdl,$_,\%subst,$context), 0..$#{$this->{IndObjs}})
  . "]";
  # If not all substitutions made, the user probably made a spelling error
  croak "pp_def($this->{Sig}{OpName}): Substitutions left for \$$pdl($inds): ".(join ',',sort keys %subst)."\n"
    if keys(%subst) != 0;
  $text;
}

sub do_pdlaccess {
	my($this) = @_;
	'$PRIV(pdls)['.$this->{Number}.']';
}

sub do_pointeraccess {
	my($this) = @_;
	return $this->{Name}."_datap";
}

sub do_indterm { my($this,$pdl,$ind,$subst,$context) = @_;
  my $substname = $this->get_substname($ind);
# See if substitutions
  my $index = delete($subst->{$substname}) //
# No => get the one from the nearest context.
    (grep $_ eq $substname, map $_->[1], reverse @$context)[0];
  croak "pp_def($this->{Sig}{OpName}): Access Index not found: $pdl, $ind, @{[$this->{IndObjs}[$ind]->name]}
	  On stack:".(join ' ',map {"($_->[0],$_->[1])"} @$context)."\n"
	  if !defined $index;
  return "(".($this->get_incname($ind,1))."*($index))";
}

sub get_xsdatapdecl { 
    my($this,$ctype,$nulldatacheck,$ppsym) = @_;
    my $pdl = $this->get_nname;
    my $name = $this->{Name};
    my $macro = "PDL_DECLARE_PARAMETER".($this->{BadFlag} ? "_BADVAL" : "");
    "$macro($ctype, $name, $pdl, $nulldatacheck, $ppsym)";
}

1;
