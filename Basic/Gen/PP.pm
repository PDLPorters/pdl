# $PDL::PP::deftbl is an array-ref of
#   PDL::PP::Rule->new("Name1", "Name2", $ref_to_sub)
# where Name1 represents the target of the rule, Name2 the condition,
# and the subroutine reference is the routine called when the rule is
# applied.
#
# If there is no condition, the argument can be left out of the call
# (unless there is a doc string), so
#   PDL::PP::Rule->new("Name1", $ref_to_sub)
#
# The target and conditions can also be an array reference, so
#   PDL::PP::Rule->new("Name1", ["Name2","Name3"], $ref_to_sub)
#   PDL::PP::Rule->new(["Name1","Name2"], "Name3", $ref_to_sub)
#   PDL::PP::Rule->new(["Name1","Name2"], ["Name3","Name4], $ref_to_sub)
#
# If a doc string exists then the condition must also
# be supplied, even if it is just [] (ie no condition).
#
# There are specialized rules for common situations. The rules for the
# target, condition, and doc arguments hold from the base class (ie
# whether scalar or array values are used, ...)
#
# Return a constant:
#
# PDL::PP::Rule::Returns->new($targets [,$conditions [,$doc]], $value)
# is used to return a constant. So
#   PDL::PP::Rule::Returns->new("Name1", "foo")
#
# This class is specialized since there are some common return values:
#   PDL::PP::Rule::Returns::Zero->new($targets [,$conditions [,$doc]])
#   PDL::PP::Rule::Returns::One->new($targets [,$conditions [,$doc]])
#   PDL::PP::Rule::Returns::EmptyString->new($targets [,$conditions [,$doc]])
#   PDL::PP::Rule::Returns::NULL->new($targets [,$conditions [,$doc]])
# which return 0, 1, "", and "NULL" respectively
#
# The InsertName class exists to allow you to return something like
#   "foo<routine name>bar"
# e.g.
#  PDL::PP::Rule::InsertName->new("Foo", '_pdl_${name}_bar')
#  PDL::PP::Rule::InsertName->new("Foo", "Arg2", '_pdl_${name}_bar')
# Note that the Name argument is automatically used as a condition, so
# it does not need to be supplied, and the return value should be
# given as a single-quoted string and use the $name variable
#
# The Substitute rule replaces dollar-signed macros ($P(), $ISBAD(), etc)
# with the low-level C code to perform the macro.
#   PDL::PP::Rule::Substitute("NewXSCoerceMustSubs", "NewXSCoerceMustSub1")
# PDL::PP::Rule::Substitute->new($target,$condition)
#   $target and $condition must be scalars.
#   Implicit conditions are NewXSSymTab and Name
#
# PDL::PP::Rule::Substitute::Usual->new($target, $condition)
#   $target and $condition must be scalars.
#   Implicit conditions are NewXSSymTab and Name
#
# The MakeComp rule creates the compiled representation accessed by $COMP()
#  PDL::PP::Rule::MakeComp->new("MakeCompiledRepr", ["MakeComp","CompObj"],
#		      "COMP")
# PDL::PP::Rule::MakeComp->new($target,$conditions,$symbol)
#   $target and $symbol must be scalars.

# Notes:
#   Substitute, Substitute::Usual, MakeComp classes feel a bit
#   ugly. See next point. Also the get_std_childparent method is
#   a bit of a hack.

package PDL::PP::Rule;

use strict;

use Carp;
our @CARP_NOT;

use overload ("\"\"" => \&PDL::PP::Rule::stringify);
sub stringify {
    my $self = shift;

    my $str = ref $self;
    if ("PDL::PP::Rule" eq $str) {
	$str = "Rule";
    } else {
	$str =~ s/PDL::PP::Rule:://;
    }
    $str = "($str) ";
    $str .= exists $self->{doc} ?
       $self->{doc} : join(",", @{$self->{targets}});
    return $str;
}

# Takes two args: the calling object and the message, but we only care
# about the message:
sub report ($$) { print $_[1] if $::PP_VERBOSE; }

# Very limited error checking.
# Allow scalars for targets and conditions to be optional
#
# At present you have to have a conditions argument if you supply
# a doc string
my $rule_usage = "Usage: PDL::PP::Rule->new(\$targets[,\$conditions[,\$doc],] [,\$ref])\n";
sub new {
    die $rule_usage if @_ < 2 or @_ > 5;
    my $class = shift;
    my $self = bless {}, $class;
    my $targets = shift;
    $targets = [$targets] unless ref $targets eq "ARRAY";
    $self->{targets} = $targets;
    return $self if !@_;
    $self->{ref} = pop if ref $_[-1] eq "CODE";
    my $conditions = shift // [];
    $conditions = [$conditions] unless ref $conditions eq "ARRAY";
    $self->{conditions} = $conditions;
    $self->{doc} = shift if defined $_[0];
    $self;
}

# $rule->any_targets_exist($pars);
#
# Returns 1 if any of the targets exist in $pars, 0 otherwise.
# A return value of 1 means that the rule should not be applied.
sub any_targets_exist {
    my $self = shift;
    my $pars = shift;

    my $targets = $self->{targets};

    foreach my $target (@$targets) {
	if (exists $pars->{$target}) {
	    $self->report("--skipping since TARGET $target exists\n");
	    return 1;
	}
    }
    return 0;
}

# $rule->all_conditions_exist($pars);
#
# Returns 1 if all of the required conditions exist in $pars, 0 otherwise.
# A return value of 0 means that the rule should not be applied.
sub all_conditions_exist {
    my $self = shift;
    my $pars = shift;

    my $conditions = $self->{conditions};

    foreach my $condition (@$conditions) {

	# skip if not a required condition
	next if substr($condition,0,1) eq "_";

	unless (exists $pars->{$condition}) {
	    $self->report("--skipping since CONDITION $condition does not exist\n");
	    return 0;
	}
    }

    return 1;
}

# $rule->should_apply($pars);
#
# Returns 1 if the rule should be applied (ie no targets already
# exist in $pars and all the required conditions exist in $pars),
# otherwise 0.
#
sub should_apply {
    my $self = shift;
    my $pars = shift;
    return 0 if $self->any_targets_exist($pars);
    return 0 unless $self->all_conditions_exist($pars);
    return 1;
}

# my @args = $self->extract_args($pars);
sub extract_args {
    my ($self, $pars) = @_;
    @$pars{ map {
        (my $condition = $_) =~ s/^_//; # initial _ = optional condition
        $condition;
    } @{ $self->{conditions} } };
}

# Apply the rule using the supplied $pars hash reference.
#
sub apply {
    my $self = shift;
    my $pars = shift;

    carp "Unable to apply rule $self as there is no subroutine reference!"
      unless exists $self->{ref};

    my $targets = $self->{targets};
    my $conditions = $self->{conditions};
    my $ref = $self->{ref};

    $self->report("Applying: $self\n");

    return unless $self->should_apply($pars);

    # Create the argument array for the routine.
    #
    my @args = $self->extract_args($pars);

    # Run this rule's subroutine:
    my @retval = $ref->(@args);

    # Check for any inconsistencies:
    confess "Internal error: rule '$self' returned " . (1+$#retval)
      . " items and expected " . (1+$#$targets)
		unless $#retval == $#$targets;

    $self->report("--setting:");
    foreach my $target (@$targets) {
		$self->report(" $target");
		confess "Cannot have multiple meanings for target $target!"
		  if exists $pars->{$target};
		my $result = shift @retval;

		# The following test suggests that things could/should be
		# improved in the code generation.
		#
		if (defined $result and $result eq 'DO NOT SET!!') {
			$self->report (" is 'DO NOT SET!!'");
		} else {
			$pars->{$target} = $result;
		}
	}
	$self->report("\n");
}


package PDL::PP::Rule::Croak;

# Croaks if all of the input variables are defined. Use this to identify
# incompatible arguments.
our @ISA = qw(PDL::PP::Rule);
use Carp;
our @CARP_NOT;


sub new {
    croak('Usage: PDL::PP::Rule::Croak->new(["incompatible", "arguments"], "Croaking message")')
		unless @_ == 3;
    shift->SUPER::new([], @_);
}

sub apply {
    my ($self, $pars) = @_;
    croak($self->{doc}) if $self->should_apply($pars);
}

package PDL::PP::Rule::Returns;

use strict;

use Carp;
our @CARP_NOT;

our @ISA = qw (PDL::PP::Rule);

# This class does not treat return values of "DO NOT SET!!"
# as special.
#
sub new {
    my $class = shift;
    my $value = pop;
    my $self  = $class->SUPER::new(@_);
    $self->{"returns.value"} = $value;
    my $targets = $self->{targets};
    croak "There can only be 1 target for a $self, not " . (1+$#$targets) . "!"
      unless $#$targets == 0;
    return $self;
}

sub apply {
    my $self = shift;
    my $pars = shift;

    carp "Unable to apply rule $self as there is no return value!"
      unless exists $self->{"returns.value"};

    my $target = $self->{targets}->[0];

    $self->report("Applying: $self\n");

    return unless $self->should_apply($pars);

    # Set the value
    #
    $self->report ("--setting: $target\n");
    $pars->{$target} = $self->{"returns.value"};
}

package PDL::PP::Rule::Returns::Zero;

use strict;

our @ISA = qw (PDL::PP::Rule::Returns);

sub new {
    shift->SUPER::new(@_,0);
}

package PDL::PP::Rule::Returns::One;

use strict;

our @ISA = qw (PDL::PP::Rule::Returns);

sub new {
    shift->SUPER::new(@_,1);
}

package PDL::PP::Rule::Returns::EmptyString;

use strict;

our @ISA = qw (PDL::PP::Rule::Returns);

sub new {
    shift->SUPER::new(@_,"");
}

package PDL::PP::Rule::Returns::NULL;

use strict;

our @ISA = qw (PDL::PP::Rule::Returns);

sub new {
    shift->SUPER::new(@_,"NULL");
}

package PDL::PP::Rule::InsertName;

use strict;

use Carp;
our @CARP_NOT;

our @ISA = qw (PDL::PP::Rule);

# This class does not treat return values of "DO NOT SET!!"
# as special.
#
sub new {
    my $class = shift;

    my $value = pop;

    my @args  = @_;
    my $self  = $class->SUPER::new(@args);
    $self->{"insertname.value"} = $value;

    # Generate a defaul doc string
    unless (exists $self->{doc}) {
        $self->{doc} = 'Sets ' . $self->{targets}->[0]
            . ' to "' . $value . '"';
    }

    my $targets = $self->{targets};
    croak "There can only be 1 target for a $self, not " . (1+$#$targets) . "!"
      unless $#$targets == 0;

    # we add "Name" as the first condition
    #
    my $conditions = $self->{conditions};
    unshift @$conditions, "Name";

    return $self;
}

sub apply {
    my $self = shift;
    my $pars = shift;

    carp "Unable to apply rule $self as there is no return value!"
      unless exists $self->{"insertname.value"};

    $self->report("Applying: $self\n");

    return unless $self->should_apply($pars);

    # Set the value
    #
    my $target = $self->{targets}->[0];
    my $name   = $pars->{Name};
    $self->report ("--setting: $target (name=$name)\n");
    $pars->{$target} = eval "return \"" . $self->{"insertname.value"} . "\";";
}

#   PDL::PP::Rule->new("NewXSCoerceMustSubs", ["NewXSCoerceMustSub1","NewXSSymTab","Name"],
#	 	      \&dosubst),
#
# PDL::PP::Rule::Substitute->new($target,$condition)
#   $target and $condition must be scalars.
#
#   Implicit conditions are NewXSSymTab and Name
#
package PDL::PP::Rule::Substitute;

use strict;

use Carp;
our @CARP_NOT;

our @ISA = qw (PDL::PP::Rule);

# Probably want this directly in the apply routine but leave as is for now
#
sub dosubst_private {
    my ($src,$sname,$pname,$name) = @_;
    my $ret = (ref $src ? $src->[0] : $src);
    my %syms = (
		((ref $src) ? %{$src->[1]} : ()),
		PRIV => sub {return "$sname->$_[0]"},
		COMP => sub {return "$pname->$_[0]"},
		CROAK => sub {PDL::PP::pp_line_numbers(__LINE__-1, "PDL->pdl_barf(\"Error in $name:\" $_[0])")},
		NAME => sub {return $name},
		MODULE => sub {return $::PDLMOD},
		SETPDLSTATEBAD  => sub { PDL::PP::pp_line_numbers(__LINE__-1, "$_[0]\->state |= PDL_BADVAL") },
		SETPDLSTATEGOOD => sub { PDL::PP::pp_line_numbers(__LINE__-1, "$_[0]\->state &= ~PDL_BADVAL") },
		ISPDLSTATEBAD   => sub { PDL::PP::pp_line_numbers(__LINE__-1, "(($_[0]\->state & PDL_BADVAL) > 0)") },
		ISPDLSTATEGOOD  => sub { PDL::PP::pp_line_numbers(__LINE__-1, "(($_[0]\->state & PDL_BADVAL) == 0)") },
		BADFLAGCACHE    => sub { PDL::PP::pp_line_numbers(__LINE__-1, "badflag_cache") },
	       );
    while(
	  $ret =~ s/\$(\w+)\(([^()]*)\)/
	  (defined $syms{$1} or
	   confess("$1 not defined in '$ret'!")) and
	  (&{$syms{$1}}($2))/ge
	 ) {};
    $ret;
}

sub new {
    my $class = shift;

    die "Usage: PDL::PP::Rule::Substitute->new(\$target,\$condition);"
      unless $#_ == 1;

    my $target = shift;
    my $condition = shift;

    die "\$target must be a scalar for PDL::PP::Rule::Substitute" if ref $target;
    die "\$condition must be a scalar for PDL::PP::Rule::Substitute" if ref $condition;

    $class->SUPER::new($target, [$condition, "StructName", "ParamStructName", "Name"],
				  \&dosubst_private);
}

#   PDL::PP::Rule->new("CacheBadFlagInit", ["CacheBadFlagInitNS","NewXSSymTab","Name"],
#		      \&dousualsubsts),
#
# PDL::PP::Rule::Substitute::Usual->new($target, $condition)
#   $target and $condition must be scalars.
#
#   Implicit conditions are NewXSSymTab and Name
#
# Need to think about @std_childparent as it is also used by
# other bits of code. At the moment provide a class method
# to access the array but there has to be better ways of
# doing this.
#
package PDL::PP::Rule::Substitute::Usual;

use strict;

use Carp;
our @CARP_NOT;

our @ISA = qw (PDL::PP::Rule::Substitute);

# This is a copy of the main one for now. Need a better solution.
#
my @std_childparent = (
	CHILD => sub {PDL::PP::pp_line_numbers(__LINE__-1, '$PRIV(pdls[1]->'.(join ',',@_).")")},
	PARENT => sub {PDL::PP::pp_line_numbers(__LINE__-1, '$PRIV(pdls[0]->'.(join ',',@_).")")},
	CHILD_PTR => sub {PDL::PP::pp_line_numbers(__LINE__-1, '$PRIV(pdls[1])')},
	PARENT_PTR => sub {PDL::PP::pp_line_numbers(__LINE__-1, '$PRIV(pdls[0])')},
);

sub get_std_childparent { return @std_childparent; }

# We modify the arguments from the conditions to include the
# extra information
#
# We simplify the base-class version since we assume that all
# conditions are required here.
#
sub extract_args {
    my $self = shift;
    my $pars = shift;

    # The conditions are [<code>, NewXSSymTab, Name]
    #
    my $code   = $pars->{$self->{conditions}[0]};
    my $sname = $pars->{$self->{conditions}[1]};
    my $name   = $pars->{$self->{conditions}[2]};

    return ([$code,{@std_childparent}],$sname,$name);
}

# PDL::PP::Rule::MakeComp->new($target,$conditions,$symbol)
#   $target and $symbol must be scalars.
#
package PDL::PP::Rule::MakeComp;

use strict;

use Carp;
our @CARP_NOT;

our @ISA = qw (PDL::PP::Rule);

# This is a copy of the main one for now. Need a better solution.
#
my @std_redodims = (
  SETNDIMS => sub {PDL::PP::pp_line_numbers(__LINE__-1, "PDL->reallocdims(__it,$_[0])")},
  SETDIMS => sub {PDL::PP::pp_line_numbers(__LINE__-1, "PDL->setdims_careful(__it)")},
  SETDELTATHREADIDS => sub {PDL::PP::pp_line_numbers(__LINE__, <<EOF)},
{int __ind; PDL->reallocthreadids(\$CHILD_PTR(), \$PARENT(nthreadids));
for(__ind=0; __ind<\$PARENT(nthreadids); __ind++)
  \$CHILD(threadids[__ind]) = \$PARENT(threadids[__ind]) + ($_[0]);
}
EOF
);

# Probably want this directly in the apply routine but leave as is for now
#
sub subst_makecomp_private {
	my($which,$mc,$cobj) = @_;
	my ($cn,$co) = !$cobj ? () : map $cobj->$_, qw(othernames otherobjs);
	return [$mc,{
		PDL::PP::Rule::Substitute::Usual::get_std_childparent(),
		($cn ?
			(('DO'.$which.'ALLOC') => sub {join('',
				map $$co{$_}->get_malloc("\$$which($_)"),
				    grep $$co{$_}->need_malloc, @$cn)}) :
			()
		),
		($which eq "PRIV" ?
			@std_redodims : ()),
		},
	];
}

sub new {
    my $class = shift;

    die "Usage: PDL::PP::Rule::MakeComp->new(\$target,\$conditions,\$symbol);"
      unless $#_ == 2;

    my $target = shift;
    my $condition = shift;
    my $symbol = shift;

    die "\$target must be a scalar for PDL::PP::Rule->MakeComp" if ref $target;
    die "\$symbol must be a scalar for PDL::PP::Rule->MakeComp" if ref $symbol;

    my $self = $class->SUPER::new($target, $condition,
				  \&subst_makecomp_private);
    $self->{"makecomp.value"} = $symbol;

    return $self;
}

# We modify the arguments from the conditions to include the
# extra information
#
# We simplify the base-class version since we assume that all
# conditions are required here.
#
sub extract_args {
    my $self = shift;
    my $pars = shift;
    ($self->{"makecomp.value"}, @$pars{@{$self->{conditions}}});
}

package PDL::PP;

use strict;

our $VERSION = "2.3";
$VERSION = eval $VERSION;

our $macros_xs = <<'EOF';
#define PDL_XS_PREAMBLE \
  char *objname = "PDL"; /* XXX maybe that class should actually depend on the value set \
                            by pp_bless ? (CS) */ \
  HV *bless_stash = 0; \
  SV *parent = 0; \
  int   nreturn;

#define PDL_XS_PACKAGEGET \
  PDL_COMMENT("Check if you can get a package name for this input value.  ") \
  PDL_COMMENT("It can be either a PDL (SVt_PVMG) or a hash which is a     ") \
  PDL_COMMENT("derived PDL subclass (SVt_PVHV)                            ") \
  if (SvROK(ST(0)) && ((SvTYPE(SvRV(ST(0))) == SVt_PVMG) || (SvTYPE(SvRV(ST(0))) == SVt_PVHV))) { \
    parent = ST(0); \
    if (sv_isobject(parent)){ \
	bless_stash = SvSTASH(SvRV(ST(0))); \
	objname = HvNAME((bless_stash));  PDL_COMMENT("The package to bless output vars into is taken from the first input var") \
    } \
  }

#define PDL_XS_PERLINIT(name, to_push, method) \
  if (strcmp(objname,"PDL") == 0) { PDL_COMMENT("shortcut if just PDL") \
     name ## _SV = sv_newmortal(); \
     name = PDL->null(); \
     PDL->SetSV_PDL(name ## _SV, name); \
     if (bless_stash) name ## _SV = sv_bless(name ## _SV, bless_stash); \
  } else { \
     PUSHMARK(SP); \
     XPUSHs(to_push); \
     PUTBACK; \
     perl_call_method(#method, G_SCALAR); \
     SPAGAIN; \
     name ## _SV = POPs; \
     PUTBACK; \
     name = PDL->SvPDLV(name ## _SV); \
  }

#define PDL_XS_RETURN(clause1) \
    if (nreturn) { \
      if (nreturn > 0) EXTEND (SP, nreturn); \
      clause1; \
      XSRETURN(nreturn); \
    } else { \
      XSRETURN(0); \
    }
EOF

our $header_c = pp_line_numbers(__LINE__, <<'EOF');
/*
 * THIS FILE WAS GENERATED BY PDL::PP! Do not modify!
 */

#define PDL_COMMENT(comment)
PDL_COMMENT("This preprocessor symbol is used to add commentary in the PDL  ")
PDL_COMMENT("autogenerated code. Normally, one would use typical C-style    ")
PDL_COMMENT("multiline comments (i.e. /* comment */). However, because such ")
PDL_COMMENT("comments do not nest, it's not possible for PDL::PP users to   ")
PDL_COMMENT("comment-out sections of code using multiline comments, as is   ")
PDL_COMMENT("often the practice when debugging, for example. So, when you   ")
PDL_COMMENT("see something like this:                                       ")
PDL_COMMENT("                                                               ")
                PDL_COMMENT("Memory access")
PDL_COMMENT("                                                               ")
PDL_COMMENT("just think of it as a C multiline comment like:                ")
PDL_COMMENT("                                                               ")
PDL_COMMENT("   /* Memory access */                                         ")

#define PDL_XS_INPLACE(in, out) \
    if (in->state & PDL_INPLACE && (out != in)) { \
	in->state &= ~PDL_INPLACE; PDL_COMMENT("unset") \
	out = in; \
	PDL->SetSV_PDL(out ## _SV,out); \
    }

#define PDL_XS_INPLACE_CHECK(in) \
    if (in->state & PDL_INPLACE) barf("inplace input but output given");

#define PDL_FREE_CODE(trans, comp_free_code, ntpriv_free_code) \
    PDL_TR_CLRMAGIC(trans); \
    comp_free_code \
    if ((trans)->dims_redone) { \
	ntpriv_free_code \
    }

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "pdl.h"
#include "pdlcore.h"
#define PDL %s
extern Core* PDL; PDL_COMMENT("Structure hold core C functions")
static int __pdl_boundscheck = 0;
static SV* CoreSV;       PDL_COMMENT("Gets pointer to perl var holding core structure")

#if ! %s
# define PP_INDTERM(max, at) at
#else
# define PP_INDTERM(max, at) (__pdl_boundscheck? PDL->safe_indterm(max,at, __FILE__, __LINE__) : at)
#endif
EOF
our $header_xs = pp_line_numbers(__LINE__, <<'EOF');

Core* PDL = NULL; PDL_COMMENT("Structure hold core C functions")

MODULE = %1$s PACKAGE = %1$s

PROTOTYPES: ENABLE

int
set_boundscheck(i)
       int i;
       CODE:
       if (! %6$s)
         warn("Bounds checking is disabled for %1$s");
       RETVAL = __pdl_boundscheck;
       __pdl_boundscheck = i;
       OUTPUT:
       RETVAL


MODULE = %1$s PACKAGE = %2$s

%3$s

BOOT:

   PDL_COMMENT("Get pointer to structure of core shared C routines")
   PDL_COMMENT("make sure PDL::Core is loaded")
   %4$s
   %5$s
EOF

use Config;
use Exporter;
use Data::Dumper;

our @ISA = qw(Exporter);

@PDL::PP::EXPORT = qw/pp_addhdr pp_addpm pp_bless pp_def pp_done pp_add_boot
                      pp_add_exported pp_addxs pp_add_isa pp_export_nothing
		      pp_core_importList pp_beginwrap pp_setversion
                      pp_addbegin pp_boundscheck pp_line_numbers
                      pp_deprecate_module/;

$PP::boundscheck = 1;
$::PP_VERBOSE    = 0;

$PDL::PP::done = 0;  # pp_done has not been called yet

use Carp;
our @CARP_NOT;

sub nopm { $::PDLPACK eq 'NONE' } # flag that we don't want to generate a PM

sub import {
	my ($mod,$modname, $packname, $prefix, $callpack, $multi_c) = @_;
	# Allow for users to not specify the packname
	($packname, $prefix, $callpack) = ($modname, $packname, $prefix)
		if ($packname =~ m|/|);

	$::PDLMOD=$modname; $::PDLPACK=$packname; $::PDLPREF=$prefix;
	$::CALLPACK = $callpack || $::PDLMOD;
	$::PDLMULTI_C = $multi_c; # one pp-*.c per function
	$::PDLOBJ = "PDL"; # define pp-funcs in this package
	$::PDLXS="";
	$::PDLBEGIN="";
	$::PDLPMROUT="";
 	for ('Top','Bot','Middle') { $::PDLPM{$_}="" }
	@::PDLPMISA=('PDL::Exporter', 'DynaLoader');
	@::PDL_IFBEGINWRAP = ('','');
	$::PDLVERSIONSET = '';
	$::PDLMODVERSION = undef;
	$::DOCUMENTED = 0;
	$::PDLCOREIMPORT = "";  #import list from core, defaults to everything, i.e. use Core
				#  could be set to () for importing nothing from core. or qw/ barf / for
				# importing barf only.
	@_=("PDL::PP");
	goto &Exporter::import;
}

sub list_functions {
  my ($file) = @_;
  my @funcs;
  local *PDL::PP::pp_def = sub { push @funcs, (_pp_parsename($_[0]))[0]};
  local *PDL::PP::pp_done = sub {};
  require File::Spec::Functions;
  do ''.File::Spec::Functions::rel2abs($file);
  die $@ if $@;
  @funcs;
}

# query/set boundschecking
# if on the generated XS code will have optional boundschecking
# that can be turned on/off at runtime(!) using
#   __PACKAGE__::set_boundscheck(arg); # arg should be 0/1
# if off code is speed optimized and no runtime boundschecking
# can be performed
# ON by default
sub pp_boundscheck {
  my $ret = $PP::boundscheck;
  $PP::boundscheck = $_[0] if $#_ > -1;
  return $ret;
}

sub pp_beginwrap {
	@::PDL_IFBEGINWRAP = ('BEGIN {','}');
}

sub pp_setversion {
	my ($ver) = @_;
	$ver = qq{'$ver'} if $ver !~ /['"]/;
	$::PDLMODVERSION = '$VERSION';
	$::PDLVERSIONSET = "our \$VERSION = $ver;";
}

sub pp_addhdr {
	my ($hdr) = @_;
	$::PDLXSC .= $hdr;
	$::PDLXSC_header .= $hdr if $::PDLMULTI_C;
}

sub pp_addpm {
 	my $pm = shift;
 	my $pos;
 	if (ref $pm) {
 	  my $opt = $pm;
 	  $pm = shift;
 	  croak "unknown option" unless defined $opt->{At} &&
 	    $opt->{At} =~ /^(Top|Bot|Middle)$/;
 	  $pos = $opt->{At};
 	} else {
 	  $pos = 'Middle';
 	}
 	$::PDLPM{$pos} .= "$pm\n\n";
}

sub pp_add_exported {
  shift if !$_[0] or $_[0] eq __PACKAGE__;
  $::PDLPMROUT .= join ' ', @_, '';
}

sub pp_addbegin {
	my ($cmd) = @_;
	if ($cmd =~ /^\s*BOOT\s*$/) {
		pp_beginwrap;
	} else {
		$::PDLBEGIN .= $cmd."\n";
	}
}

#  Sub to call to export nothing (i.e. for building OO package/object)
sub pp_export_nothing {
	$::PDLPMROUT = ' ';
}

sub pp_add_isa {
	push @::PDLPMISA,@_;
}

sub pp_add_boot {
	my ($boot) = @_;
	$boot =~ s/^\s*\n//gm; # XS doesn't like BOOT having blank lines
	$::PDLXSBOOT .= $boot;
}

sub pp_bless{
   my($new_package)=@_;
   $::PDLOBJ = $new_package;
}

# sub to call to set the import list from core on the 'Use Core' line in the .pm file.
#   set to '()' to not import anything from Core, or 'qw/ barf /' to import barf.
sub pp_core_importList{
   $::PDLCOREIMPORT = $_[0];
}

sub printxs {
	shift;
	$::PDLXS .= join'',@_;
}

sub pp_addxs {
	PDL::PP->printxs("\nMODULE = $::PDLMOD PACKAGE = $::CALLPACK\n\n",
                         @_,
                         "\nMODULE = $::PDLMOD PACKAGE = $::PDLOBJ\n\n");
}

# inserts #line directives into source text. Use like this:
#   ...
#   FirstKey => ...,
#   Code => pp_line_numbers(__LINE__, $x . $y . $c),
#   OtherKey => ...
sub pp_line_numbers ($$) {
	my ($line, $string) = @_;
	# The line needs to be incremented by one for the bookkeeping to work
	$line++;
	# Get the source filename using caller()
	my (undef, $filename) = caller;
	# Escape backslashes:
	$filename =~ s/\\/\\\\/g;
	my @to_return = "\nPDL_LINENO_START $line \"$filename\"\n";
	# Look for threadloops and loops and add # line directives
	foreach (split (/\n/, $string)) {
		# Always add the current line.
		push @to_return, "$_\n";
		# If we need to add a # line directive, do so after incrementing
		$line++;
		if (/%\{/ or /%}/) {
			push @to_return, "PDL_LINENO_END\n";
			push @to_return, "PDL_LINENO_START $line \"$filename\"\n";
		}
	}
	push @to_return, "PDL_LINENO_END\n";
	return join('', @to_return);
}
sub _pp_linenumber_fill {
  my ($file, $text) = @_;
  my (@stack, @to_return) = [$file, 1];
  foreach (split (/\n/, $text)) {
    $_->[1]++ for @stack;
    push(@to_return, $_), next if !/^(\s*)PDL_LINENO_(?:START (\S+) "(.*)"|(END))$/;
    my ($ci, $new_line, $new_file, $is_end) = ($1, $2, $3, $4);
    if ($is_end) {
      pop @stack;
      push @to_return, qq{$ci#line $stack[-1][1] "$stack[-1][0]"};
    } else {
      push @stack, [$new_file, $new_line-1];
      push @to_return, qq{$ci#line @{[$stack[-1][1]+1]} "$stack[-1][0]"};
    }
  }
  join '', map "$_\n", @to_return;
}

sub _file_same {
  my ($from_text, $to_file) = @_;
  require File::Map;
  File::Map::map_file(my $to_map, $to_file, '<');
  s/^\s*#line.*?$//gm for $from_text, (my $to_text = $to_map);
  $from_text eq $to_text;
}
sub _write_file {
  my ($file, $text) = @_;
  $text = _pp_linenumber_fill($file, $text);
  return if -f $file && length($text) == -s $file && _file_same($text, $file);
  open my $fh, '>', $file or confess "open $file: $!";
  binmode $fh; # to guarantee length will be same for same contents
  print $fh $text;
}

sub printxsc {
  (undef, my $file) = (shift, shift);
  my $text = join '',@_;
  if (defined $file) {
    (my $mod_underscores = $::PDLMOD) =~ s#::#_#g;
    $text = join '', sprintf($PDL::PP::header_c, $mod_underscores, $PP::boundscheck), $::PDLXSC_header, $text;
    _write_file($file, $text);
  } else {
    $::PDLXSC .= $text;
  }
}

sub pp_done {
        return if $PDL::PP::done; # do only once!
        $PDL::PP::done = 1;
	print "DONE!\n" if $::PP_VERBOSE;
	print "Inline running PDL::PP version $PDL::PP::VERSION...\n" if nopm();
        require PDL::Core::Dev;
        my $pdl_boot = PDL::Core::Dev::PDL_BOOT('PDL', $::PDLMOD);
        (my $mod_underscores = $::PDLMOD) =~ s#::#_#g;
        my $text = join '',
          sprintf($PDL::PP::header_c, $mod_underscores, $PP::boundscheck),
          $::PDLXSC,
          $PDL::PP::macros_xs, sprintf($PDL::PP::header_xs,
            $::PDLMOD, $::PDLOBJ, $::PDLXS,
            $pdl_boot, $::PDLXSBOOT, $PP::boundscheck,
          );
        _write_file("$::PDLPREF.xs", $text);

unless (nopm) {
	$::PDLPMISA = "'".join("','",@::PDLPMISA)."'";
	$::PDLBEGIN = "BEGIN {\n$::PDLBEGIN\n}"
		unless $::PDLBEGIN =~ /^\s*$/;
        $::FUNCSPOD = $::DOCUMENTED ? "\n\n=head1 FUNCTIONS\n\n=cut\n\n" : '';
        _write_file("$::PDLPREF.pm", <<EOF);
#
# GENERATED WITH PDL::PP! Don't modify!
#
package $::PDLPACK;

our \@EXPORT_OK = qw($::PDLPMROUT);
our %EXPORT_TAGS = (Func=>\\\@EXPORT_OK);

use PDL::Core$::PDLCOREIMPORT;
use PDL::Exporter;
use DynaLoader;

$::PDL_IFBEGINWRAP[0]
   $::PDLVERSIONSET
   our \@ISA = ( $::PDLPMISA );
   push \@PDL::Core::PP, __PACKAGE__;
   bootstrap $::PDLMOD $::PDLMODVERSION;
$::PDL_IFBEGINWRAP[-1]

$::PDLBEGIN

$::PDLPM{Top}

$::FUNCSPOD

$::PDLPM{Middle};

$::PDLPM{Bot}

# Exit with OK status

1;
EOF
      }  # unless (nopm)
} # end pp_done

sub _pp_parsename {
  my ($name) = @_;
  # See if the 'name' is multiline, in which case we extract the
  # name and add the FullDoc field
  return ($name, undef) if $name !~ /\n/;
  my $fulldoc = $name;
  # See if the very first thing is a word. That is going to be the
  # name of the function under consideration
  if ($fulldoc =~ s/^(\w+)//) {
    $name = $1;
  } elsif ($fulldoc =~ /=head2 (\w+)/) {
    $name = $1;
  } else {
    croak('Unable to extract name');
  }
  ($name, $fulldoc);
}

sub pp_def {
	require PDL::Core::Dev;
	require PDL::Types;
	require PDL::PP::PdlParObj;
	require PDL::PP::Signature;
	require PDL::PP::Dims;
	require PDL::PP::CType;
	require PDL::PP::PDLCode;
	PDL::PP::load_deftable() if !$PDL::PP::deftbl;
	my($name,%obj) = @_;
	print "*** Entering pp_def for $name\n" if $::PP_VERBOSE;
	($name, my $fulldoc) = _pp_parsename($name);
	$obj{FullDoc} = $fulldoc if defined $fulldoc;
	$obj{Name} = $name;
	croak("ERROR: pp_def=$name given empty GenericTypes!\n")
	  if exists $obj{GenericTypes} and !@{ $obj{GenericTypes} || [] };
	foreach my $rule (@$PDL::PP::deftbl) {
	    $rule->apply(\%obj);
	}
	print "Result of translate for $name:\n" . Dumper(\%obj) . "\n"
	  if exists $obj{Dump} and $obj{Dump} and $::PP_VERBOSE;

	croak("ERROR: No FreeFunc for pp_def=$name!\n")
	  unless exists $obj{FreeFunc};

	my $ctext = join("\n\n",@obj{'StructDecl','RedoDimsFunc',
		'ReadDataFunc','WriteBackDataFunc',
		'FreeFunc',
		'VTableDef','RunFunc',
		}
		);
	if ($::PDLMULTI_C) {
	  PDL::PP->printxsc(undef, <<EOF);
extern pdl_transvtable $obj{VTableName};
$obj{RunFuncHdr};
EOF
	  PDL::PP->printxsc("pp-$obj{Name}.c", $ctext);
	} else {
	  PDL::PP->printxsc(undef, $ctext);
	}
	PDL::PP->printxs($obj{NewXSCode});
	pp_add_boot($obj{BootSetNewXS});
	PDL::PP->pp_add_exported($name);
	PDL::PP::pp_addpm("\n".$obj{PdlDoc}."\n") if $obj{PdlDoc};
	PDL::PP::pp_addpm($obj{PMCode});
	PDL::PP::pp_addpm($obj{PMFunc}."\n");

	print "*** Leaving pp_def for $name\n" if $::PP_VERBOSE;
}

# marks this module as deprecated. This handles the user warnings, and adds a
# notice into the documentation. Can take a {infavor => "newmodule"} option
sub pp_deprecate_module
{
  my $options;
  if( ref $_[0] eq 'HASH' )  { $options = shift;  }
  else                       { $options = { @_ }; }

  my $infavor;

  if( $options && ref $options eq 'HASH' && $options->{infavor} )
  {
    $infavor = $options->{infavor};
  }

  my $mod = $::PDLMOD;
  my $envvar = 'PDL_SUPPRESS_DEPRECATION_WARNING__' . uc $mod;
  $envvar =~ s/::/_/g;

  my $warning_main =
    "$mod is deprecated.";
  $warning_main .=
    " Please use $infavor instead." if $infavor;

  my $warning_suppression_runtime =
    "This module will be removed in the future; please update your code.\n" .
    "Set the environment variable $envvar\n" .
    "to suppress this warning\n";

  my $warning_suppression_pod =
    "A warning will be generated at runtime upon a C<use> of this module\n" .
    "This warning can be suppressed by setting the $envvar\n" .
    "environment variable\n";

  my $deprecation_notice = <<EOF ;
XXX=head1 DEPRECATION NOTICE

$warning_main
$warning_suppression_pod

XXX=cut

EOF
  $deprecation_notice =~ s/^XXX=/=/gms;
  pp_addpm( {At => 'Top'}, $deprecation_notice );

  pp_addpm {At => 'Top'}, <<EOF;
warn \"$warning_main\n$warning_suppression_runtime\" unless \$ENV{$envvar};
EOF


}

# Worst memleaks: not freeing things at redodims or
# final free time (thread, dimmed things).

use Carp;
$SIG{__DIE__} = \&Carp::confess if $::PP_VERBOSE;

$|=1;

#
# This is ripped from xsubpp to ease the parsing of the typemap.
#
our $proto_re = "[" . quotemeta('\$%&*@;[]') . "]" ;

sub ValidProtoString ($)
{
    my($string) = @_ ;

    if ( $string =~ /^$proto_re+$/ ) {
        return $string ;
    }

    return 0 ;
}

sub C_string ($)
{
    my($string) = @_ ;

    $string =~ s[\\][\\\\]g ;
    $string ;
}

sub TrimWhitespace
{
    $_[0] =~ s/^\s+|\s+$//go ;
}
sub TidyType
{
    local ($_) = @_ ;

    # rationalise any '*' by joining them into bunches and removing whitespace
    s#\s*(\*+)\s*#$1#g;
    s#(\*+)# $1 #g ;

    # change multiple whitespace into a single space
    s/\s+/ /g ;

    # trim leading & trailing whitespace
    TrimWhitespace($_) ;

    $_ ;
}



#------------------------------------------------------------------------------
# Typemap handling in PP.
#
# This subroutine does limited input typemap conversion.
# Given a variable name (to set), its type, and the source
# for the variable, returns the correct input typemap entry.
# Original version: D. Hunt 4/13/00  - Current version J. Brinchmann (06/05/05)
#
# The code loads the typemap from the Perl typemap using the loading logic of
# xsubpp. Do note that I  made the assumption that
# $Config{installprivlib}/ExtUtils was the right root directory for the search.
# This could break on some systems?
#
# Also I do _not_ parse the Typemap argument from ExtUtils::MakeMaker because I don't
# know how to catch it here! This would be good to fix! It does look for a file
# called typemap in the current directory however.
#
# The parsing of the typemap is mechanical and taken straight from xsubpp and
# the resulting hash lookup is then used to convert the input type to the
# necessary outputs (as seen in the old code above)
#
# JB 06/05/05
#
sub typemap {
  my $oname  = shift;
  my $type   = shift;
  my $arg    = shift;

  # Modification to parse Perl's typemap here.
  #
  # The default search path for the typemap taken from xsubpp. It seems it is
  # necessary to prepend the installprivlib/ExtUtils directory to find the typemap.
  # It is not clear to me how this is to be done.
  #
  my ($typemap, $mode, $junk, $current, %input_expr,
      %proto_letter, %output_expr, %type_kind);

  # according to MM_Unix 'privlibexp' is the right directory
  #     seems to work even on OS X (where installprivlib breaks things)
  my $_rootdir = $Config{privlibexp}.'/ExtUtils/';

  # First the system typemaps..
  my @tm = ($_rootdir.'../../../../lib/ExtUtils/typemap',
	    $_rootdir.'../../../lib/ExtUtils/typemap',
	    $_rootdir.'../../lib/ExtUtils/typemap',
	    $_rootdir.'../../../typemap',
	    $_rootdir.'../../typemap', $_rootdir.'../typemap',
	    $_rootdir.'typemap');
  # Note that the OUTPUT typemap is unlikely to be of use here, but I have kept
  # the source code from xsubpp for tidiness.
  push @tm, &PDL::Core::Dev::PDL_TYPEMAP, 'typemap';
  carp "**CRITICAL** PP found no typemap in $_rootdir/typemap; this will cause problems..."
      unless my @typemaps = grep -f $_ && -T _, @tm;
  foreach $typemap (@typemaps) {
    open(my $fh, $typemap)
      or warn("Warning: could not open typemap file '$typemap': $!\n"), next;
    $mode = 'Typemap';
    $junk = "" ;
    $current = \$junk;
    local $_; # else get "Modification of a read-only value attempted"
    while (<$fh>) {
	next if /^\s*#/;
        my $line_no = $. + 1;
	if (/^INPUT\s*$/)   { $mode = 'Input';   $current = \$junk;  next; }
	if (/^OUTPUT\s*$/)  { $mode = 'Output';  $current = \$junk;  next; }
	if (/^TYPEMAP\s*$/) { $mode = 'Typemap'; $current = \$junk;  next; }
	if ($mode eq 'Typemap') {
	    chomp;
	    my $line = $_ ;
            TrimWhitespace($_) ;
	    # skip blank lines and comment lines
	    next if /^$/ or /^#/ ;
	    my($t_type,$kind, $proto) = /^\s*(.*?\S)\s+(\S+)\s*($proto_re*)\s*$/ or
		warn("Warning: File '$typemap' Line $. '$line' TYPEMAP entry needs 2 or 3 columns\n"), next;
            $t_type = TidyType($t_type) ;
	    $type_kind{$t_type} = $kind ;
            # prototype defaults to '$'
            $proto = "\$" unless $proto ;
            warn("Warning: File '$typemap' Line $. '$line' Invalid prototype '$proto'\n")
                unless ValidProtoString($proto) ;
            $proto_letter{$t_type} = C_string($proto) ;
	}
	elsif (/^\s/) {
	    $$current .= $_;
	}
	elsif ($mode eq 'Input') {
	    s/\s+$//;
	    $input_expr{$_} = '';
	    $current = \$input_expr{$_};
	}
	else {
	    s/\s+$//;
	    $output_expr{$_} = '';
	    $current = \$output_expr{$_};
	}
    }
    close $fh;
  }

  #
  # Do checks...
  #
  # First reconstruct the type declaration to look up in type_kind
  my $full_type=TidyType($type->get_decl('', {VarArrays2Ptrs=>1})); # Skip the variable name
  die "The type =$full_type= does not have a typemap entry!\n" unless exists($type_kind{$full_type});
  my $typemap_kind = $type_kind{$full_type};
  # Look up the conversion from the INPUT typemap. Note that we need to do some
  # massaging of this.
  my $input = $input_expr{$typemap_kind};
  $input =~ s/^(.*?)=\s*//s; # Remove all before =
  $input =~ s/\$(var|\{var\})/$oname/g;
  $input =~ s/\$(arg|\{arg\})/$arg/g;
  $input =~ s/\$(type|\{type\})/$full_type/g;
  return ($input);
}

sub wrap_vfn {
  my (
    $code,$rout,$func_header,
    $all_func_header,$sname,$pname,$ptype,
  ) = @_;
  my $str = join "\n", grep $_, $all_func_header, $func_header, $code;
  PDL::PP::pp_line_numbers(__LINE__, <<EOF);
void $rout(pdl_trans *$sname) {
@{[$ptype ? "  $ptype *$pname = $sname->params;" : ""]}
$str}
EOF
}
my @vfn_args_always = qw(_AllFuncHeader StructName ParamStructName ParamStructType);
sub make_vfn_args {
  my ($which) = @_;
  ("${which}Func",
    ["${which}CodeSubd","${which}FuncName","_${which}FuncHeader",
      @vfn_args_always
    ],
    sub {$_[1] eq 'NULL' ? '' : wrap_vfn(@_,lc $which)}
  );
}

my @xscode_args_always = (
  "_NewXSCHdrs",
  "RunFuncCall",
);
sub make_xs_code {
  my($xscode_before,$xscode_after,$hdr,
    $xs_c_headers,
    @bits) = @_;
  my($str,$boot,$prelude) = $hdr;
  if($xs_c_headers) {
    $prelude = join '' => ($xs_c_headers->[0], @bits, $xs_c_headers->[1]);
    $boot = $xs_c_headers->[2];
    $str .= "\n";
  } else {
    my $xscode = join '' => @bits;
    $str .= " $xscode_before\n $xscode$xscode_after\n\n";
  }
  $str =~ s/(\s*\n)+/\n/g;
  ($str,$boot,$prelude)
}

sub indent($$) {
    my ($text,$ind) = @_;
    $text =~ s/^(.*)$/$ind$1/mg;
    return $text;
}

# This subroutine generates the XS code needed to call the perl 'initialize'
# routine in order to create new output PDLs
sub callPerlInit {
    my $names = shift; # names of variables to initialize
    my $ci    = shift; # current indenting
    my $callcopy = $#_ > -1 ? shift : 0;
    my $ret = '';
    foreach my $name (@$names) {
	my ($to_push, $method) = $callcopy
	    ? ('parent', 'copy')
	    : ('sv_2mortal(newSVpv(objname, 0))', 'initialize');
	$ret .= PDL::PP::pp_line_numbers(__LINE__-1, "PDL_XS_PERLINIT($name, $to_push, $method)\n");
    }
    indent($ret,$ci);
} #sub callPerlInit()

###########################################################
# Name       : extract_signature_from_fulldoc
# Usage      : $sig = extract_signature_from_fulldoc($fulldoc)
# Purpose    : pull out the signature from the fulldoc string
# Returns    : whatever is in parentheses in the signature, or undef
# Parameters : $fulldoc
# Throws     : never
# Notes      : the signature must have the following form:
#            : 
#            : =for sig
#            : <blank>
#            :   Signature: (<signature can
#            :                be multiline>)
#            : <blank>
#            : 
#            : The two spaces before "Signature" are required, as are
#            : the parentheses.
sub extract_signature_from_fulldoc {
	my $fulldoc = shift;
	if ($fulldoc =~ /=for sig\n\n  Signature: \(([^\n]*)\n/g) {
		# Extract the signature and remove the final parenthesis
		my $sig = $1;
		$sig .= $1 while $fulldoc =~ /\G\h+([^\n]*)\n/g;
		$sig =~ s/\)\s*$//;
		return $sig;
	}
	return;
}

# function to be run by real pp_def so fake pp_def can do without other modules
sub load_deftable {
# Build the valid-types regex and valid Pars argument only once. These are
# also used in PDL::PP::PdlParObj, which is why they are globally available.
my $pars_re = $PDL::PP::PdlParObj::pars_re;

# Set up the rules for translating the pp_def contents.
#
$PDL::PP::deftbl =
  [
   PDL::PP::Rule->new(
      [qw(RedoDims EquivCPOffsCode HandleBad P2Child TwoWay)],
      ["Identity"],
      "something to do with dataflow between CHILD & PARENT, I think.",
      sub {
        (PDL::PP::pp_line_numbers(__LINE__-1, '
          int i;
          $SETNDIMS($PARENT(ndims));
          for(i=0; i<$CHILD(ndims); i++) {
            $CHILD(dims[i]) = $PARENT(dims[i]);
          }
          $SETDIMS();
          $SETDELTATHREADIDS(0);
          $PRIV(dims_redone) = 1;
        '),
        # NOTE: we use the same bit of code for all-good and bad data -
        #  see the Code rule
        # we can NOT assume that PARENT and CHILD have the same type,
        # hence the version for bad code
        #
        # NOTE: we use the same code for 'good' and 'bad' cases - it's
        # just that when we use it for 'bad' data, we have to change the
        # definition of the EQUIVCPOFFS macro - see the Code rule
        PDL::PP::pp_line_numbers(__LINE__,
            'PDL_Indx i;
             for(i=0; i<$CHILD_P(nvals); i++)  {
                $EQUIVCPOFFS(i,i);
             }'),
        1, 1, 1);
      }),

   # used as a flag for many of the routines
   # ie should we bother with bad values for this routine?
   # 1     - yes,
   # 0     - no, maybe issue a warning
   PDL::PP::Rule->new("BadFlag", "_HandleBad",
		      "Sets BadFlag based upon HandleBad key",
		      sub { $_[0] }),

   ####################
   # FullDoc Handling #
   ####################

   # Error processing: does FullDoc contain BadDoc, yet BadDoc specified?
   PDL::PP::Rule::Croak->new(['FullDoc', 'BadDoc'],
       'Cannot have both FullDoc and BadDoc defined'),
   PDL::PP::Rule::Croak->new(['FullDoc', 'Doc'],
       'Cannot have both FullDoc and Doc defined'),
   # Note: no error processing on Pars; it's OK for the docs to gloss over
   # the details.

   # Add the Pars section based on the signature of the FullDoc if the Pars
   # section doesn't already exist
   PDL::PP::Rule->new('Pars', 'FullDoc',
      'Sets the Pars from the FullDoc if Pars is not explicitly specified',
      # Purpose    : extract the Pars from the signature from the fulldoc string,
      #            : the part of the signature that specifies the ndarrays
      # Returns    : a string appropriate for the Pars key
      # Parameters : $fulldoc
      # Throws     : if there is no signature 
      #            : if there is no extractable Pars section
      #            : if some PDL arguments come after the OtherPars arguments start
      # Notes      : This is meant to be used directly in a Rule. Therefore, it
      #            : is only called if the Pars key does not yet exist, so if it
      #            : is not possible to extract the Pars section, it dies.
      sub {
        my $fulldoc = shift;
        # Get the signature or die
        my $sig = extract_signature_from_fulldoc($fulldoc)
          or confess('No Pars specified and none could be extracted from FullDoc');
        # Everything is semicolon-delimited
        my @args = split /\s*;\s*/, $sig;
        my @pars;
        my $switched_to_other_pars = 0;
        for my $arg (@args) {
          confess('All PDL args must come before other pars in FullDoc signature')
            if $switched_to_other_pars and $arg =~ $pars_re;
          if ($arg =~ $pars_re) {
            push @pars, $arg;
          } else {
            $switched_to_other_pars = 1;
          }
        }
        # Make sure there's something there
        confess('FullDoc signature contains no PDL arguments') if @pars == 0;
        # All done!
        return join('; ', @pars);
      }
   ),
   PDL::PP::Rule->new('OtherPars', 'FullDoc',
      'Sets the OtherPars from the FullDoc if OtherPars is not explicitly specified',
      # Purpose    : extract the OtherPars from the signature from the fulldoc
      #            : string, the part of the signature that specifies non-ndarray
      #            : arguments
      # Returns    : a string appropriate for the OtherPars key
      # Parameters : $fulldoc
      # Throws     : if some OtherPars arguments come before the last PDL argument
      # Notes      : This is meant to be used directly in a Rule. Therefore, it
      #            : is only called if the OtherPars key does not yet exist.
      sub {
        my $fulldoc = shift;
        # Get the signature or do not set
        my $sig = extract_signature_from_fulldoc($fulldoc)
                or return 'DO NOT SET!!';
        # Everything is semicolon-delimited
        my @args = split /\s*;\s*/, $sig;
        my @otherpars;
        for my $arg (@args) {
          confess('All PDL args must come before other pars in FullDoc signature')
            if @otherpars > 0 and $arg =~ $pars_re;
          if ($arg !~ $pars_re) {
            push @otherpars, $arg;
          }
        }
        # All done!
        return 'DO NOT SET!!'if @otherpars == 0;
        return join('; ', @otherpars);
      }
   ),

   ################################
   # Other Documentation Handling #
   ################################
   
   # no docs by default
   PDL::PP::Rule::Returns->new("Doc", [], 'Sets the default doc string',
    "\n=for ref\n\ninfo not available\n"),
   
   # try and automate the docs
   # could be really clever and include the sig to see about
   # input/output params, for instance
   
   PDL::PP::Rule->new("BadDoc", ["BadFlag","Name","_CopyBadStatusCode"],
              'Sets the default documentation for handling of bad values',
      sub {
         my ( $bf, $name, $code ) = @_;
         my $str;
         if ( not defined($bf) ) {
            $str = "$name does not process bad values.\n";
         } elsif ( $bf ) {
            $str = "$name processes bad values.\n";
         } else {
            $str = "$name ignores the bad-value flag of the input ndarrays.\n";
         }
         if ( not defined($code) ) {
            $str .= "It will set the bad-value flag of all output ndarrays if " .
            "the flag is set for any of the input ndarrays.\n";
         } elsif (  $code eq '' ) {
            $str .= "The output ndarrays will NOT have their bad-value flag set.\n";
         } else {
            $str .= "The state of the bad-value flag of the output ndarrays is unknown.\n";
         }
      }
   ),

   # Default: no otherpars
   PDL::PP::Rule::Returns::EmptyString->new("OtherPars"),

   # the docs
   PDL::PP::Rule->new("PdlDoc", "FullDoc", sub {
         my $fulldoc = shift;
         # Append a final cut if it doesn't exist due to heredoc shinanigans
         $fulldoc .= "\n\n=cut\n" unless $fulldoc =~ /\n=cut\n*$/;
         # Make sure the =head1 FUNCTIONS section gets added
         $::DOCUMENTED++;
         return $fulldoc;
      }
   ),
   PDL::PP::Rule->new("PdlDoc", ["Name","_Pars","OtherPars","Doc","_BadDoc"],
      sub {
        my ($name,$pars,$otherpars,$doc,$baddoc) = @_;
        return '' if !defined $doc # Allow explcit non-doc using Doc=>undef
            or $doc =~ /^\s*internal\s*$/i;
        # If the doc string is one line let's have to for the
        # reference card information as well
        $doc = "=for ref\n\n".$doc if $doc !~ /\n/;
        $::DOCUMENTED++;
        $pars = "P(); C()" unless $pars;
        # Strip leading whitespace and trailing semicolons and whitespace
        $pars =~ s/^\s*(.+[^;])[;\s]*$/$1/;
        $otherpars =~ s/^\s*(.+[^;])[;\s]*$/$1/ if $otherpars;
        my $sig = "$pars".( $otherpars ? "; $otherpars" : "");
        $doc =~ s/\n(=cut\s*\n)+(\s*\n)*$/\n/m; # Strip extra =cut's
        if ( defined $baddoc ) {
                # Strip leading newlines and any =cut markings
            $baddoc =~ s/\n(=cut\s*\n)+(\s*\n)*$/\n/m;
            $baddoc =~ s/^\n+//;
            $baddoc = "=for bad\n\n$baddoc";
        }
        my $baddoc_function_pod = <<"EOD" ;

XXX=head2 $name

XXX=for sig

  Signature: ($sig)

$doc

$baddoc

XXX=cut

EOD
        $baddoc_function_pod =~ s/^XXX=/=/gms;
        return $baddoc_function_pod;
      }
   ),

   ##################
   # Done with Docs #
   ##################

   # Notes
   # Suffix 'NS' means, "Needs Substitution". In other words, the string
   # associated with a key that has the suffix "NS" must be run through a
   # Substitute or Substitute::Usual
   # The substituted version should then replace "NS" with "Subd"
   # So: FreeCodeNS -> FreeCodeSubd

   PDL::PP::Rule::Returns->new("StructName", "__privtrans"),
   PDL::PP::Rule::Returns->new("ParamStructName", "__params"),

   PDL::PP::Rule::Croak->new([qw(P2Child GenericTypes)],
       'Cannot have both P2Child and GenericTypes defined'),
   PDL::PP::Rule->new([qw(Pars HaveThreading CallCopy GenericTypes DefaultFlow AllFuncHeader RedoDimsFuncHeader)],
		      ["P2Child","Name","StructName"],
      sub {
        my (undef,$name,$sname) = @_;
        ("PARENT(); [oca]CHILD();",0,0,[PDL::Types::ppdefs_all()],1,
          pp_line_numbers(__LINE__-1,"\tpdl *__it = $sname->pdls[1];\n\tpdl *__parent = $sname->pdls[0];\n"),
          pp_line_numbers(__LINE__-1,"PDL->hdr_childcopy($sname);\n$sname->dims_redone = 1;\n"),
        );
      }),

# Question: where is ppdefs defined?
# Answer: Core/Types.pm
#
   PDL::PP::Rule->new("GenericTypes", [],
       'Sets GenericTypes flag to all real types known to PDL::Types',
       sub {[PDL::Types::ppdefs()]}),

   PDL::PP::Rule->new("ExtraGenericLoops", "FTypes",
       'Makes ExtraGenericLoops identical to FTypes if the latter exists and the former does not',
       sub {return $_[0]}),
   PDL::PP::Rule::Returns->new("ExtraGenericLoops", [],
		'Sets ExtraGenericLoops to an empty hash if it does not already exist', {}),

   PDL::PP::Rule::InsertName->new("VTableName", 'pdl_${name}_vtable'),

   PDL::PP::Rule::Returns->new("Priv", "AffinePriv", 'PDL_Indx incs[$CHILD(ndims)];PDL_Indx offs; '),
   PDL::PP::Rule::Returns->new("IsAffineFlag", "AffinePriv", "PDL_ITRANS_ISAFFINE"),
   PDL::PP::Rule::Returns::Zero->new("IsAffineFlag"),
   PDL::PP::Rule::Returns->new("TwoWayFlag", "TwoWay", "PDL_ITRANS_TWOWAY"),
   PDL::PP::Rule::Returns::Zero->new("TwoWayFlag"),
   PDL::PP::Rule::Returns->new("DefaultFlowFlag", "DefaultFlow", "PDL_ITRANS_DO_DATAFLOW_F|PDL_ITRANS_DO_DATAFLOW_B"),
   PDL::PP::Rule::Returns::Zero->new("DefaultFlowFlag"),

   PDL::PP::Rule->new("RedoDims", ["EquivPDimExpr","_EquivDimCheck"],
      sub {
        my($pdimexpr,$dimcheck) = @_;
        $pdimexpr =~ s/\$CDIM\b/i/g;
        PDL::PP::pp_line_numbers(__LINE__-1, '
          int i,cor;
          '.$dimcheck.'
          $SETNDIMS($PARENT(ndims));
          $DOPRIVALLOC();
          $PRIV(offs) = 0;
          for(i=0; i<$CHILD(ndims); i++) {
            cor = '.$pdimexpr.';
            $CHILD(dims[i]) = $PARENT(dims[cor]);
            $PRIV(incs[i]) = $PARENT(dimincs[cor]);
          }
          $SETDIMS();
          $SETDELTATHREADIDS(0);
          $PRIV(dims_redone) = 1;
        ');
      }),

   PDL::PP::Rule->new("Code", ["EquivCPOffsCode","BadFlag"],
      "create Code from EquivCPOffsCode",
      # NOTE: EQUIVCPOFFS and EQUIVCPTRUNC both suffer from the macro-block
      # wart of C preprocessing.  They look like statements but sometimes
      # process into blocks, so if/then/else constructs can get broken.
      # Either (1) use blocks for if/then/else, or (2) get excited and
      # use the "do {BLOCK} while(0)" block-to-statement conversion construct
      # in the substitution.  I'm too Lazy. --CED 27-Jan-2003
      sub {
        my $good  = shift;
        my $bflag = shift;
        my $bad = $good;
        # parse 'good' code
        $good =~ s/\$EQUIVCPOFFS\(([^()]+),([^()]+)\)/\$PP(CHILD)[$1] = \$PP(PARENT)[$2]/g;
        $good =~ s/\$EQUIVCPTRUNC\(([^()]+),([^()]+),([^()]+)\)/\$PP(CHILD)[$1] = ($3) ? 0 : \$PP(PARENT)[$2]/g;
        return $good if !$bflag;
        # parse 'bad' code
        $bad  =~ s/\$EQUIVCPOFFS\(([^()]+),([^()]+)\)/if( \$PPISBAD(PARENT,[$2]) ) { \$PPSETBAD(CHILD,[$1]); } else { \$PP(CHILD)[$1] = \$PP(PARENT)[$2]; }/g;
        $bad =~ s/\$EQUIVCPTRUNC\(([^()]+),([^()]+),([^()]+)\)/ if( ($3) || \$PPISBAD(PARENT,[$2]) ) { \$PPSETBAD(CHILD,[$1]); } else {\$PP(CHILD)[$1] = \$PP(PARENT)[$2]; }/g;
        PDL::PP::pp_line_numbers(__LINE__-1, 'if ( $PRIV(bvalflag) ) { ' . $bad . ' } else { ' . $good . '}');
      }),

   PDL::PP::Rule->new("BackCode", ["EquivCPOffsCode","BadFlag"],
      "create BackCode from EquivCPOffsCode",
      # If there is an EquivCPOffsCode and:
      #    no bad-value support ==> use that
      #    bad value support ==> write a bit of code that does
      #      if ( $PRIV(bvalflag) ) { bad-EquivCPOffsCode }
      #      else                   { good-EquivCPOffsCode }
      #
      #  Note: since EquivCPOffsCode doesn't (or I haven't seen any that
      #  do) use 'loop %{' or 'threadloop %{', we can't rely on
      #  PDLCode to automatically write code like above, hence the
      #  explicit definition here.
      #
      #  Note: I *assume* that bad-Equiv..Code == good-Equiv..Code *EXCEPT*
      #        that we re-define the meaning of the $EQUIVCPOFFS macro to
      #        check for bad values when copying things over.
      #        This means having to write less code.
      #
      # Since PARENT & CHILD need NOT be the same type we cannot just copy
      # values from one to the other - we have to check for the presence
      # of bad values, hence the expansion for the $bad code
      #
      # Some operators (notably range) also have an out-of-range flag; they use
      # the macro EQUIVCPTRUNC instead of EQUIVCPOFFS.
      # $EQUIVCPTRUNC does the same as EQUIVCPOFFS but accepts a
      # child-out-of-bounds flag.  If the out-of-bounds flag is set, the
      # forward code puts BAD/0 into the child, and reverse code refrains
      # from copying.
      #                    --CED 27-Jan-2003
      #
      # this just reverses PARENT & CHILD in the expansion of
      # the $EQUIVCPOFFS macro (ie compared to Code from EquivCPOffsCode)
      sub {
        my ($good, $bflag) = @_;
        my $bad  = $good;
        # parse 'good' code
        $good =~ s/\$EQUIVCPOFFS\(([^()]+),([^()]+)\)/\$PP(PARENT)[$2] = \$PP(CHILD)[$1]/g;
        $good =~ s/\$EQUIVCPTRUNC\(([^()]+),([^()]+),([^()]+)\)/if(!($3)) \$PP(PARENT)[$2] = \$PP(CHILD)[$1] /g;
        return $good if !$bflag;
        # parse 'bad' code
        $bad  =~ s/\$EQUIVCPOFFS\(([^()]+),([^()]+)\)/if( \$PPISBAD(CHILD,[$1]) ) { \$PPSETBAD(PARENT,[$2]); } else { \$PP(PARENT)[$2] = \$PP(CHILD)[$1]; }/g;
        $bad =~ s/\$EQUIVCPTRUNC\(([^()]+),([^()]+),([^()]+)\)/if(!($3)) { if( \$PPISBAD(CHILD,[$1]) ) { \$PPSETBAD(PARENT,[$2]); } else { \$PP(PARENT)[$2] = \$PP(CHILD)[$1]; } } /g;
        PDL::PP::pp_line_numbers(__LINE__-1, 'if ( $PRIV(bvalflag) ) { ' . $bad . ' } else { ' . $good . '}');
      }),

   PDL::PP::Rule::Returns::Zero->new("Affine_Ok", "EquivCPOffsCode"),
   PDL::PP::Rule::Returns::One->new("Affine_Ok"),

   PDL::PP::Rule::Returns::NULL->new("ReadDataFuncName", "AffinePriv"),
   PDL::PP::Rule::Returns::NULL->new("WriteBackDataFuncName", "AffinePriv"),

   PDL::PP::Rule::InsertName->new("NewXSName", '_${name}_int'),

   PDL::PP::Rule::Returns::One->new("HaveThreading"),

# Parameters in the 'a(x,y); [o]b(y)' format, with
# fixed nos of real, unthreaded-over dims.
# Also "Other pars", the parameters which are usually not pdls.
   PDL::PP::Rule->new("SignatureObj", ["Pars","BadFlag","OtherPars"],
      sub { PDL::PP::Signature->new(@_) }),

 # Set CallCopy flag for simple functions (2-arg with 0-dim signatures)
 #   This will copy the $object->copy method, instead of initialize
 #   for PDL-subclassed objects
 #
   PDL::PP::Rule->new("CallCopy", ["SignatureObj", "Name"],
      sub {
	  my ($sig, $Name, $hasp2c) = @_;
	  my $noDimmedArgs = $sig->dims_count;
	  my $noArgs = @{$sig->names};
	  # Check for 2-arg function with 0-dim signatures
	  return 0 if !($noDimmedArgs == 0 and $noArgs == 2);
	  # Check to see if output arg is _not_ explicitly typed:
	  !$sig->objs->{$sig->names->[1]}{FlagTyped};
      }),

   PDL::PP::Rule::Returns->new("PMCode", undef),

   PDL::PP::Rule->new(["InplaceCode","InplaceCheck"], ["SignatureObj","Inplace"],
		      'Insert code (just after HdrCode) to ensure the routine can be done inplace',
      # insert code, after the autogenerated xs argument processing code
      # produced by VarArgsXSHdr and AFTER any in HdrCode
      # - this code flags the routine as working inplace,
      #
      # Inplace can be supplied several values
      #   => 1
      #     assumes fn has an input and output ndarray (eg 'a(); [o] b();')
      #   => [ 'a' ]
      #     assumes several input ndarrays in sig, so 'a' labels which
      #     one is to be marked inplace
      #   => [ 'a', 'b' ]
      #     input ndarray is a(), output ndarray is 'b'
      sub {
        my ( $sig, $arg ) = @_;
        return '' if !$arg;
        confess "Inplace array-ref (@$arg) > 2 elements" if ref($arg) eq "ARRAY" and @$arg > 2;
        # find input and output ndarrays
        my @out = $sig->names_out;
        my @in = $sig->names_in;
        my $in = @in == 1 ? $in[0] : undef;
        my $out = @out == 1 ? $out[0] : undef;
        if ( ref($arg) eq "ARRAY" and @$arg) {
          $in = $$arg[0];
          $out = $$arg[1] if @$arg > 1;
        }
        confess "ERROR: Inplace does not know name of input ndarray\n"
            unless defined $in;
        confess "ERROR: Inplace does not know name of output ndarray\n"
            unless defined $out;
        (
         PDL::PP::pp_line_numbers(__LINE__-1, "PDL_XS_INPLACE($in, $out)\n"),
         PDL::PP::pp_line_numbers(__LINE__-1, "PDL_XS_INPLACE_CHECK($in)\n"),
        );
      }),
   PDL::PP::Rule::Returns::EmptyString->new("InplaceCode", []),
   PDL::PP::Rule::Returns::EmptyString->new("InplaceCheck", []),

   PDL::PP::Rule::Returns::EmptyString->new("HdrCode", [],
					    'Code that will be inserted at the end of the autogenerated xs argument processing code VargArgsXSHdr'),

   # globalnew implies internal usage, not XS
   PDL::PP::Rule::Returns->new("VarArgsXSHdr","GlobalNew",undef),
   PDL::PP::Rule->new("VarArgsXSHdr",
      ["Name","SignatureObj",
       "PMCode","HdrCode","InplaceCode","InplaceCheck","_CallCopy","_Bitwise"],
      'XS code to process arguments on stack based on supplied Pars argument to pp_def; GlobalNew has implications how/if this is done',
      # This subroutine operates when no 'PMCode' exists.
      # This writes an XS header which handles variable argument lists,
      # thus avoiding the perl layer in calling the routine. D. Hunt 4/11/00
      #
      # The use of 'DO NOT SET!!' looks ugly.
      sub {
        my($name,$sig,
           $pmcode,$hdrcode,$inplacecode,$inplacecheck,$callcopy,$bitwise) = @_;
        # Don't do var args processing if the user has pre-defined pmcode
        return 'DO NOT SET!!' if $pmcode;
        my $ci = '  ';  # current indenting
        my $pars = join "\n",map "$ci$_;", $sig->alldecls(1, 0);
        my @args = $sig->alldecls(0, 1);
        my %out = map +($_=>1), $sig->names_out_nca;
        my %outca = map +($_=>1), $sig->names_oca;
        my %tmp = map +($_=>1), $sig->names_tmp;
        my $optypes = $sig->otherobjs(1);
        my %other  = map { $_ => exists($$optypes{$_}) } @args;
        # remember, othervars *are* input vars
        my $nout   = (grep { $_ } values %out);
        my $noutca = (grep { $_ } values %outca);
        my $nother = (grep { $_ } values %other);
        my $ntmp   = (grep { $_ } values %tmp);
        my $ntot   = @args;
        my $nmaxonstack = $ntot - $noutca;
        my $nin    = $ntot - ($nout + $noutca + $ntmp);
        my $ninout = $nin + $nout;
        my $nallout = $nout + $noutca;
        my $usageargs = join (",", @args);
        $ci = '  ';  # Current indenting
        # Generate declarations for SV * variables corresponding to pdl * output variables.
        # These are used in creating output and temp variables.  One variable (ex: SV * outvar1_SV;)
        # is needed for each output and output create always argument
        my $svdecls = join "\n", map "${ci}SV *${_}_SV = NULL;", sort(keys %tmp), $sig->names_out;
        my $clause_inputs = ''; my %already_read; my $cnt = 0;
        foreach my $x (@args) {
            last if $out{$x} || $tmp{$x} || $outca{$x} || $other{$x};
            $already_read{$x} = 1;
            $clause_inputs .= "$ci$x = PDL->SvPDLV(ST($cnt));\n";
            $cnt++;
        }
        my @create = ();  # The names of variables which need to be created by calling
                          # the 'initialize' perl routine from the correct package.
        $ci = '    ';  # Current indenting
        # clause for reading in all variables
        my $clause1 = $inplacecheck; $cnt = 0;
        foreach my $x (@args) {
            if ($other{$x}) {  # other par
                $clause1 .= "$ci$x = " . typemap($x, $$optypes{$x}, "ST($cnt)") . ";\n";
                $cnt++;
            } elsif ($outca{$x}) {
                push (@create, $x);
            } else {
                $clause1 .= "$ci$x = PDL->SvPDLV(".
		  (($out{$x}||$tmp{$x}) ? "${x}_SV = " : '').
		  "ST($cnt));\n" if !$already_read{$x};
                $cnt++;
            }
        }
        # Add code for creating output variables via call to 'initialize' perl routine
        $clause1 .= callPerlInit (\@create, $ci, $callcopy);
        @create = ();
        # clause for reading in input and output vars and creating temps
        my $clause2;
        # skip this clause if there are no temps
        if ($nmaxonstack == $ninout) {
            $clause2 = '';
        } else {
            $clause2 = "\n  else if (items == $ninout) { PDL_COMMENT(\"all but temps on stack, read in output, create temps\")" .
                "    nreturn = $noutca;\n";
            $cnt = 0;
            foreach my $x (@args) {
                if ($other{$x}) {
                    $clause2 .= "$ci$x = " . typemap($x, $$optypes{$x}, "ST($cnt)") . ";\n";
                    $cnt++;
                } elsif ($tmp{$x} || $outca{$x}) {
                    # a temporary or always create variable
                    push (@create, $x);
                } else { # an input or output variable
                    $clause2 .= "$ci$x = PDL->SvPDLV(ST($cnt));\n" if !$already_read{$x};
                    $cnt++;
                }
            }
            # Add code for creating output variables via call to 'initialize' perl routine
            $clause2 .= callPerlInit (\@create, $ci, $callcopy);
            $clause2 .= "}\n";
            @create = ();
        }
        # clause for reading in input and creating output and temp vars
        my $clause3 = '';
        $cnt = 0;
        foreach my $x (@args) {
            if ($other{$x}) {
                $clause3 .= "$ci$x = " . typemap($x, $$optypes{$x}, "ST($cnt)") . ";\n";
                $cnt++;
            } elsif ($out{$x} || $tmp{$x} || $outca{$x}) {
                push (@create, $x);
            } else {
                $clause3 .= "$ci$x = PDL->SvPDLV(ST($cnt));\n" if !$already_read{$x};
                $cnt++;
            }
        }
        # Add code for creating output variables via call to 'initialize' perl routine
        $clause3 .= callPerlInit (\@create, $ci, $callcopy); @create = ();
        # Bitwise ops may get five args
        my $bitwise_cond = $bitwise ? " || items == 5" : '';
        $clause3 = <<EOF . $clause3;
  else if (items == $nin$bitwise_cond) { PDL_COMMENT("only input variables on stack, create outputs and temps")
    nreturn = $nallout;
EOF
        $clause3 = '' if $nmaxonstack == $nin and !$bitwise;
        my $clause3_coda = $clause3 ? '}' : '';
        PDL::PP::pp_line_numbers(__LINE__, <<END);

void
$name(...)
 PREINIT:
  PDL_XS_PREAMBLE
$svdecls
$pars
 PPCODE:
  if (items != $nmaxonstack && !(items == $nin$bitwise_cond) && items != $ninout)
    croak (\"Usage:  PDL::$name($usageargs) (you may leave temporaries or output variables out of list)\");
  PDL_XS_PACKAGEGET
$clause_inputs
  if (items == $nmaxonstack) { PDL_COMMENT("all variables on stack, read in output and temp vars")
    nreturn = $noutca;
$clause1
  }
$clause2
$clause3$clause3_coda
$hdrcode
$inplacecode
END
      }),

   # globalnew implies internal usage, not XS
   PDL::PP::Rule::Returns->new("VarArgsXSReturn","GlobalNew",undef),
   PDL::PP::Rule->new("VarArgsXSReturn",
      ["SignatureObj"],
      "Generate XS trailer to return output variables or leave them as modified input variables",
      sub {
        my @outs = $_[0]->names_out; # names of output variables (in calling order)
        my $clause1 = join ';', map "ST($_) = $outs[$_]_SV", 0 .. $#outs;
        PDL::PP::pp_line_numbers(__LINE__-1, "PDL_XS_RETURN($clause1)");
      }),

   PDL::PP::Rule->new("NewXSHdr", ["NewXSName","SignatureObj"],
      sub {
        my($name,$sig) = @_;
        my $shortpars = join ',', $sig->alldecls(0, 1);
        my $longpars = join "\n", map "\t$_", $sig->alldecls(1, 1);
        return<<END;

void
$name($shortpars)
$longpars
END
      }),
   PDL::PP::Rule::InsertName->new("RunFuncName", 'pdl_${name}_run'),
   PDL::PP::Rule->new("NewXSCHdrs", ["RunFuncName","SignatureObj","GlobalNew"],
      sub {
        my($name,$sig,$gname) = @_;
        my $longpars = join ",", $sig->alldecls(1, 0);
        return ["void $name($longpars) {","}",
                "PDL->$gname = $name;"];
      }),
   PDL::PP::Rule->new(["RunFuncCall","RunFuncHdr"],["RunFuncName","SignatureObj"], sub {
        my ($func_name,$sig) = @_;
        my $shortpars = join ',', $sig->alldecls(0, 0);
        my $longpars = join ",", $sig->alldecls(1, 0);
        (PDL::PP::pp_line_numbers(__LINE__-1, "$func_name($shortpars);"),
          "void $func_name($longpars)");
      }),

   PDL::PP::Rule->new("NewXSMakeNow", ["SignatureObj"],
      sub { join '', map PDL::PP::pp_line_numbers(__LINE__-1, "$_ = PDL->make_now($_);\n"), @{ $_[0]->names } }),
   PDL::PP::Rule->new("IgnoreTypesOf", ["FTypes","SignatureObj"], sub {
      my ($ftypes, $sig) = @_;
      my ($pnames, $pobjs) = ($sig->names_sorted, $sig->objs);
      $_->{FlagIgnore} = 1 for grep $ftypes->{$_->{Name}}, @$pobjs{@$pnames};
      +{map +($_,1), keys %$ftypes};
   }),
   PDL::PP::Rule::Returns->new("IgnoreTypesOf", {}),

   PDL::PP::Rule->new("NewXSCoerceMustNS", "FTypes",
      sub {
        my($ftypes) = @_;
        join '', map
          PDL::PP::pp_line_numbers(__LINE__, "$_->datatype = $ftypes->{$_};"),
          sort keys %$ftypes;
      }),
   PDL::PP::Rule::Substitute::Usual->new("NewXSCoerceMustSubd", "NewXSCoerceMustNS"),

   PDL::PP::Rule->new("NewXSTypeCoerceNS", ["StructName"],
      sub {
        PDL::PP::pp_line_numbers(__LINE__-1, "PDL->type_coerce($_[0]);");
      }),
   PDL::PP::Rule::Substitute::Usual->new("NewXSTypeCoerceSubd", "NewXSTypeCoerceNS"),

   PDL::PP::Rule->new("NewXSSetTransPDLs", ["SignatureObj","StructName"], sub {
      my($sig,$trans) = @_;
      my $no=0;
      join '',
        map PDL::PP::pp_line_numbers(__LINE__, "$trans->pdls[".($no++)."] = $_;\n"),
        @{ $sig->names_sorted };
   }),

   PDL::PP::Rule->new("NewXSExtractTransPDLs", ["SignatureObj","StructName"], sub {
      my($sig,$trans) = @_;
      my $no=0;
      join '',
        map PDL::PP::pp_line_numbers(__LINE__, "$_ = $trans->pdls[".($no++)."];\n"),
        @{ $sig->names_sorted };
   }),

   PDL::PP::Rule->new("NewXSRunTrans", ["StructName"], sub {
      my($trans) = @_;
      PDL::PP::pp_line_numbers(__LINE__,
      "PDL->make_trans_mutual($trans);\n");
   }),

   PDL::PP::Rule->new(PDL::PP::Code::make_args("Code"),
		      sub { PDL::PP::Code->new(@_, undef, undef); }),
   PDL::PP::Rule->new(PDL::PP::Code::make_args("BackCode"),
		      sub { PDL::PP::Code->new(@_, undef, 1); }),

# Compiled representations i.e. what the RunFunc function leaves
# in the params structure. By default, copies of the parameters
# but in many cases (e.g. slice) a benefit can be obtained
# by parsing the string in that function.
# If the user wishes to specify their own MakeComp code and Comp content,
# The next definitions allow this.
   PDL::PP::Rule->new("CompObj", ["BadFlag","Comp"],
      sub { PDL::PP::Signature->new('', @_) }),
   PDL::PP::Rule->new("CompObj", "SignatureObj", sub { @_ }), # provide default
   PDL::PP::Rule->new("MakeCompOther", "SignatureObj", sub { $_[0]->getcopy }),
   PDL::PP::Rule->new("MakeCompTotal", [qw(MakeCompOther _MakeComp)], sub { join "\n", grep $_, @_ }),
   PDL::PP::Rule->new("CompStructOther", "SignatureObj", sub {$_[0]->getcomp}),
   PDL::PP::Rule->new("CompStructComp", [qw(CompObj Comp)], sub {$_[0]->getcomp}),
   PDL::PP::Rule->new("CompStruct", [qw(CompStructOther _CompStructComp)], sub { join "\n", grep $_, @_ }),
   PDL::PP::Rule::MakeComp->new("MakeCompiledReprNS", ["MakeCompTotal","CompObj"],
				"COMP"),
   PDL::PP::Rule->new("CompFreeCodeOther", "SignatureObj", sub {$_[0]->getfree("COMP")}),
   PDL::PP::Rule->new("CompFreeCodeComp", [qw(CompObj Comp)], sub {$_[0]->getfree("COMP")}),
   PDL::PP::Rule->new("CompFreeCode", [qw(CompFreeCodeOther _CompFreeCodeComp)], sub { join "\n", grep $_, @_ }),

   PDL::PP::Rule->new(["StructDecl","ParamStructType"],
      ["CompStruct","Name"],
      sub {
        my($comp,$name) = @_;
        return ('', '') if !$comp;
        my $ptype = "pdl_params_$name";
        (PDL::PP::pp_line_numbers(__LINE__-1, qq{typedef struct $ptype {\n$comp} $ptype;}),
        $ptype);
      }),

   PDL::PP::Rule::Substitute->new("MakeCompiledReprSubd", "MakeCompiledReprNS"),

   PDL::PP::Rule->new("DefaultRedoDims",
      ["StructName"],
      sub { "PDL->redodims_default($_[0]);" }),

   PDL::PP::Rule->new("DimsSetters",
      ["SignatureObj"],
      sub { join "\n", sort map $_->get_initdim, $_[0]->dims_values }),

   PDL::PP::Rule->new("RedoDimsFuncName", [qw(Name _RedoDims _RedoDimsCode DimsSetters)],
      sub { (scalar grep $_ && /\S/, @_[1..$#_]) ? "pdl_$_[0]_redodims" : 'NULL'}),
   PDL::PP::Rule::Returns->new("RedoDimsCode", [],
			       'Code that can be inserted to set the size of output ndarrays dynamically based on input ndarrays; is parsed',
			       ''),
   PDL::PP::Rule->new(PDL::PP::Code::make_args("RedoDimsCode"),
      'makes the parsed representation from the supplied RedoDimsCode',
      sub { return '' if !$_[0]; PDL::PP::Code->new(@_, 1, undef); }),
   PDL::PP::Rule->new("RedoDims",
      ["DimsSetters","ParsedRedoDimsCode","DefaultRedoDims"],
      'makes the redodims function from the various bits and pieces',
      sub { join "\n", grep $_ && /\S/, @_ }),

   PDL::PP::Rule::Returns::EmptyString->new("Priv"),

   PDL::PP::Rule->new("PrivObj", ["BadFlag","Priv"],
      sub { PDL::PP::Signature->new('', @_) }),
   PDL::PP::Rule->new("NTPrivFreeCode", "PrivObj", sub {$_[0]->getfree("PRIV")}),

   PDL::PP::Rule->new("FreeCodeNS",
      ["StructName","CompFreeCode","NTPrivFreeCode"],
      sub {
	  (grep $_, @_[1..$#_]) ? PDL::PP::pp_line_numbers(__LINE__-1, "PDL_FREE_CODE($_[0], $_[1], $_[2])"): ''}),

   PDL::PP::Rule::Substitute::Usual->new("FreeCodeSubd", "FreeCodeNS"),

   PDL::PP::Rule::Returns::EmptyString->new("NewXSCoerceMustSubd"),

   PDL::PP::Rule::MakeComp->new("NewXSCoerceMustCompNS", "NewXSCoerceMustSubd", "FOO"),
   PDL::PP::Rule::Substitute->new("NewXSCoerceMustCompSubd", "NewXSCoerceMustCompNS"),

   PDL::PP::Rule->new("NewXSFindBadStatusNS", "FindBadStatusCode", sub {@_}),
   PDL::PP::Rule->new("NewXSFindBadStatusNS", ["StructName"],
      "Rule to find the bad value status of the input ndarrays",
      sub {
        PDL::PP::pp_line_numbers(__LINE__-1, "char \$BADFLAGCACHE() = PDL->trans_badflag_from_inputs($_[0]);\n");
      }),

   PDL::PP::Rule->new("NewXSCopyBadStatusNS",
      ["CopyBadStatusCode"],
      "Use CopyBadStatusCode if given",
      sub {
        my ($badcode) = @_;
        confess "PDL::PP ERROR: CopyBadStatusCode contains '\$PRIV(bvalflag)'; replace with \$BADFLAGCACHE()"
          if $badcode =~ m/\$PRIV(bvalflag)/;
        $badcode;
      }),
   PDL::PP::Rule->new("NewXSCopyBadStatusNS",
      ["SignatureObj"],
      "Rule to copy the bad value status to the output ndarrays",
      # note: this is executed before the trans_mutual call
      # is made, since the state may be changed by the
      # Code section
      sub {
        my ( $sig ) = @_;
        return '' if @{$sig->names} == (my @outs = $sig->names_out); # no input pdls, no badflag copying needed
        PDL::PP::pp_line_numbers(__LINE__, join '',
          "if (\$BADFLAGCACHE()) {\n",
          (map "  \$SETPDLSTATEBAD($_);\n", @outs),
          "}\n");
      }),

 # expand macros in ...BadStatusCode
 #
   PDL::PP::Rule::Substitute::Usual->new("NewXSFindBadStatusSubd", "NewXSFindBadStatusNS"),
   PDL::PP::Rule::Substitute::Usual->new("NewXSCopyBadStatusSubd", "NewXSCopyBadStatusNS"),

   PDL::PP::Rule->new("NewXSStructInit0",
		      ["StructName","VTableName","ParamStructName","ParamStructType"],
		      "Rule to create and initialise the private trans structure",
      sub {
        my( $sname, $vtable, $pname, $ptype ) = @_;
        PDL::PP::pp_line_numbers(__LINE__, <<EOF);
if (!PDL) croak("PDL core struct is NULL, can't continue");
pdl_trans *$sname = PDL->create_trans(&$vtable);
@{[$ptype ? "  $ptype *$pname = $sname->params;" : ""]}
EOF
      }),

   PDL::PP::Rule->new(["RunFunc"],
      ["RunFuncHdr",
        "NewXSStructInit0",
        "NewXSSetTransPDLs",
        "NewXSFindBadStatusSubd",
        #     NewXSMakeNow, # this is unnecessary since families never got implemented
        "NewXSTypeCoerceSubd",
        "NewXSExtractTransPDLs",
        "MakeCompiledReprSubd",
        "NewXSCoerceMustCompSubd",
        "NewXSRunTrans",
        "NewXSCopyBadStatusSubd",
      ],
      "Generate C function with idiomatic arg list to maybe call from XS",
      sub {
        my ($xs_c_header, @bits) = @_;
        PDL::PP::pp_line_numbers __LINE__-1, join '', "$xs_c_header {\n", @bits, "}\n";
      }),

 # Generates XS code with variable argument list.  If this rule succeeds, the next rule
 # will not be executed. D. Hunt 4/11/00
 #
   PDL::PP::Rule->new(["NewXSCode","BootSetNewXS","NewXSInPrelude"],
      ["VarArgsXSHdr",@xscode_args_always,
       "VarArgsXSReturn"
      ],
      "Rule to print out XS code when variable argument list XS processing is enabled",
      sub {make_xs_code('','',@_)}),

 # This rule will fail if the preceding rule succeeds
 # D. Hunt 4/11/00
 #
   PDL::PP::Rule->new(["NewXSCode","BootSetNewXS","NewXSInPrelude"],
      ["NewXSHdr",@xscode_args_always],
      "Rule to print out XS code when variable argument list XS processing is disabled",
      sub {make_xs_code('CODE:',' XSRETURN(0);',@_)}),

   PDL::PP::Rule::MakeComp->new("RedoDims-PostComp",
      ["RedoDims", "PrivObj"], "PRIV"),

   # The RedoDimsCodeNS rule takes in the RedoDims target
   # directly as well as via RedoDims-PostComp for better error-reporting
   PDL::PP::Rule->new("RedoDimsCodeNS",
      ["RedoDims", "RedoDims-PostComp", "_DimsObj"],
      sub {
        my ($redodims, $result, $dimobjs) = @_;
        $result->[1]{"SIZE"} = sub {
          my ($dimname) = @_;
          unless (defined $dimobjs->{$dimname}) {
            eval { require PDL::IO::Dumper };
            croak "can't get SIZE of undefined dimension (RedoDims=$redodims).\nredodims is $redodims\ndimobjs is ".PDL::IO::Dumper::sdump($dimobjs)."\n"
          }
          return $dimobjs->{$dimname}->get_size();
        };
        return $result;
      }),
   PDL::PP::Rule::Substitute->new("RedoDimsCodeSubd", "RedoDimsCodeNS"),
   PDL::PP::Rule->new(make_vfn_args("RedoDims")),

   PDL::PP::Rule::MakeComp->new("ReadDataCodeNS", "ParsedCode", "FOO"),
   PDL::PP::Rule::Substitute->new("ReadDataCodeSubd", "ReadDataCodeNS"),
   PDL::PP::Rule::InsertName->new("ReadDataFuncName", 'pdl_${name}_readdata'),
   PDL::PP::Rule->new(make_vfn_args("ReadData")),

   PDL::PP::Rule::MakeComp->new("WriteBackDataCodeNS", "ParsedBackCode", "FOO"),
   PDL::PP::Rule::Substitute->new("WriteBackDataCodeSubd", "WriteBackDataCodeNS"),

   PDL::PP::Rule::InsertName->new("WriteBackDataFuncName", "BackCode", 'pdl_${name}_writebackdata'),
   PDL::PP::Rule::Returns::NULL->new("WriteBackDataFuncName", "Code"),

   PDL::PP::Rule->new(make_vfn_args("WriteBackData")),

   PDL::PP::Rule->new("FreeFuncName",
		      ["FreeCodeSubd","Name"],
		      sub {$_[0] ? "pdl_$_[1]_free" : 'NULL'}),
   PDL::PP::Rule->new(make_vfn_args("Free")),

   PDL::PP::Rule::Returns::Zero->new("NoPthread"), # assume we can pthread, unless indicated otherwise
   PDL::PP::Rule->new("VTableDef",
      ["VTableName","ParamStructType","RedoDimsFuncName","ReadDataFuncName",
       "WriteBackDataFuncName","FreeFuncName",
       "SignatureObj","Affine_Ok","HaveThreading","NoPthread","Name",
       "GenericTypes","IsAffineFlag","TwoWayFlag","DefaultFlowFlag",
       "BadFlag"],
      sub {
        my($vname,$ptype,$rdname,$rfname,$wfname,$ffname,
           $sig,$affine_ok,$havethreading, $noPthreadFlag, $name, $gentypes,
           $affflag, $revflag, $flowflag, $badflag) = @_;
        my ($pnames, $pobjs) = ($sig->names_sorted, $sig->objs);
        my $nparents = 0 + grep {! $pobjs->{$_}->{FlagW}} @$pnames;
        my $aff = ($affine_ok ? "PDL_TPDL_VAFFINE_OK" : 0);
        my $npdls = scalar @$pnames;
        my $join_flags = join(", ",map {$pobjs->{$pnames->[$_]}->{FlagPhys} ?
                                          0 : $aff} 0..$npdls-1) || '0';
        my @op_flags;
        push @op_flags, 'PDL_TRANS_DO_THREAD' if $havethreading;
        push @op_flags, 'PDL_TRANS_BADPROCESS' if $badflag;
        push @op_flags, 'PDL_TRANS_BADIGNORE' if defined $badflag and !$badflag;
        push @op_flags, 'PDL_TRANS_NO_PARALLEL' if $noPthreadFlag;
        my $op_flags = join('|', @op_flags) || '0';
        my $iflags = join('|', grep $_, $affflag, $revflag, $flowflag) || '0';
        my $gentypes_txt = join(", ", (map PDL::Type->new($_)->sym, @$gentypes), '-1');
        my @realdims = map 0+@{$_->{IndObjs}}, @$pobjs{@$pnames};
        my $realdims = join(", ", @realdims) || '0';
        my $parnames = join(",",map qq|"$_"|, @$pnames) || '""';
        my $parflags = join(",\n  ",map join('|', $_->cflags)||'0', @$pobjs{@$pnames}) || '0';
        my $partypes = join(", ", map defined()?$_->sym:-1, map $_->{Type}, @$pobjs{@$pnames}) || '-1';
        my $i = 0; my @starts = map { my $ci = $i; $i += $_; $ci } @realdims;
        my $realdim_ind_start = join(", ", @starts) || '0';
        my @rd_inds = map $_->get_index, map @{$_->{IndObjs}}, @$pobjs{@$pnames};
        my $realdim_inds = join(", ", @rd_inds) || '0';
        my @indnames = $sig->ind_names_sorted;
        my $indnames = join(",", map qq|"$_"|, @indnames) || '""';
        my $sizeof = $ptype ? "sizeof($ptype)" : '0';
        PDL::PP::pp_line_numbers(__LINE__, <<EOF);
static pdl_datatypes ${vname}_gentypes[] = { $gentypes_txt };
static char ${vname}_flags[] = {
  $join_flags
};
static PDL_Indx ${vname}_realdims[] = { $realdims };
static char *${vname}_parnames[] = { $parnames };
static short ${vname}_parflags[] = {
  $parflags
};
static pdl_datatypes ${vname}_partypes[] = { $partypes };
static PDL_Indx ${vname}_realdims_starts[] = { $realdim_ind_start };
static PDL_Indx ${vname}_realdims_ind_ids[] = { $realdim_inds };
static char *${vname}_indnames[] = { $indnames };
pdl_transvtable $vname = {
  $op_flags, $iflags, ${vname}_gentypes, $nparents, $npdls, ${vname}_flags,
  ${vname}_realdims, ${vname}_parnames,
  ${vname}_parflags, ${vname}_partypes,
  ${vname}_realdims_starts, ${vname}_realdims_ind_ids, @{[scalar @rd_inds]},
  @{[scalar @indnames]}, ${vname}_indnames,
  $rdname, $rfname, $wfname,
  $ffname,
  $sizeof,"$::PDLMOD\::$name"
};
EOF
      }),

   PDL::PP::Rule->new('PMFunc', 'Name',
     'Sets PMFunc to default symbol table manipulations',
     sub {
         my ($name) = @_;
         $::PDL_IFBEGINWRAP[0].'*'.$name.' = \&'.$::PDLOBJ.
                   '::'.$name.";\n".$::PDL_IFBEGINWRAP[1]
     }
   ),

];
}

1;
