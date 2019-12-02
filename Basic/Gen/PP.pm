#####################################################################
#####################################################################
##
##
## Here starts the actual thing.
##
## This is way too messy and uncommented. Still. :(
#
# DJB August 24 2006
#   begin cleaning up the code so that it all runs under use strict
# DJB August 31 2006
#   moved to use objects for the rule table (ie defvar) in the
#   hope it's more declarative (since the addition of "::" to
#   a statement makes it so much-more meaningful :-)
#

# How to convert from the old deftbl format to the new, object-based,
# system:
#
# What used to be, in $PDL::PP::deftbl
#   [["Name1"], ["Name2"], $ref_to_sub]]
# is now
#   PDL::PP::Rule->new("Name1", "Name2", $ref_to_sub)
# where Name1 represents the target of the rule, Name2 the condition,
# and the subroutine reference is the routine called when the rule is
# applied.
#
# If their is no condition, the argument can be left out of the call
# (unless there is a doc string), so
#   [["Name1"], [], $ref_to_sub]]
# becomes
#   PDL::PP::Rule->new("Name1", $ref_to_sub)
#
# The target and conditions can also be an array reference, so
#   [["Name1"], ["Name2","Name3"], $ref_to_sub]]
#   [["Name1","Name2"], ["Name3"], $ref_to_sub]]
#   [["Name1","Name2"], ["Name3","Name4"], $ref_to_sub]]
# become, respectively
#   PDL::PP::Rule->new("Name1", ["Name2","Name3"], $ref_to_sub)
#   PDL::PP::Rule->new(["Name1","Name2"], "Name3", $ref_to_sub)
#   PDL::PP::Rule->new(["Name1","Name2"], ["Name3","Name4], $ref_to_sub)
#
# If the old rule had a document string, this is placed between
# the condition and the subroutine reference. To make processing
# simpler, if a doc string exists then the condition must also
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
#   [["Name1"], [], sub { "foo" }]
# becomes
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
# The old rules
#  [["Foo"], ["Name"], sub { return "_pdl_$_[0]_bar"; }]
#  [["Foo"], ["Name","Arg2"], sub { return "_pdl_$_[0]_bar"; }]
# become
#  PDL::PP::Rule::InsertName->new("Foo", '_pdl_${name}_bar')
#  PDL::PP::Rule::InsertName->new("Foo", "Arg2", '_pdl_${name}_bar')
# Note that the Name argument is automatically used as a condition, so
# it does not need to be supplied, and the return value should be
# given as a single-quoted string and use the $name variable
#
# The Substitute rule replaces dollar-signed macros ($P(), $ISBAD(), etc)
# with the low-level C code to perform the macro.
#
# The Substitute class replaces the dosubst rule. The old rule
#   [["NewXSCoerceMustSubs"], ["NewXSCoerceMustSub1","NewXSSymTab","Name"],
#	 	      \&dosubst]
# becomes
#   PDL::PP::Rule::Substitute("NewXSCoerceMustSubs", "NewXSCoerceMustSub1")
#
# PDL::PP::Rule::Substitute->new($target,$condition)
#   $target and $condition must be scalars.
#
#   Implicit conditions are NewXSSymTab and Name
#
# The Substitute:Usual class replaces the dousualsubsts rule. The old rule
#   [["CacheBadFlagInit"], ["CacheBadFlagInitNS","NewXSSymTab","Name"],
#		      \&dousualsubsts],
# becomes
#   PDL::PP::Rule::Substitute::Usual->new("CacheBadFlagInit", "CacheBadFlagInitNS")
#
# PDL::PP::Rule::Substitute::Usual->new($target, $condition)
#   $target and $condition must be scalars.
#
#   Implicit conditions are NewXSSymTab and Name
#
# The MakeComp rule replaces the subst_makecomp routine. The old rule
#  [["MakeCompiledRepr"], ["MakeComp","CompNames","CompObjs"],
#		      sub {subst_makecomp("COMP",@_)}]
# becomes
#  PDL::PP::Rule::MakeComp->new("MakeCompiledRepr", ["MakeComp","CompNames","CompObjs"],
#		      "COMP")
# PDL::PP::Rule::MakeComp->new($target,$conditions,$symbol)
#   $target and $symbol must be scalars.
#

# Notes:
#   InsertName could become a subclass of Insert since there are
#   a few rules that just insert conditions into a text string.
#
#   Substitute, Substitute::Usual, MakeComp classes feel a bit
#   ugly. See next point. Also the get_std_childparent method is
#   a bit of a hack.
#
#   DJB thinks that the code fragments themselves could be objects
#   since they should 'know' what needs doing to them (eg the
#   substitutions). Not sure whether it would really clarify things.
#
# To do:
#   wrap_vfn could propbably be moved into a class.
#
#   move the PDL::PP::Rule and subclasses into their own file?
#

package PDL::PP::Rule;

use strict;
require PDL::Core::Dev;

use Carp;
our @CARP_NOT;

my $INVALID_OTHERPARS_RE = qr/^(?:magicno|flags|vtable|freeproc|bvalflag|has_badvalue|badvalue|pdls|__datatype)\z/;

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
#
# It seems strange to make the subroutine reference an optional
# argument but this is being used to transition to a slightly-different
# object design
#
sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    my $usage = "Usage: PDL::PP::Rule->new(\$targets[,\$conditions[,\$doc],] [,\$ref])\n";

    # handle arguments
    my $nargs = $#_;
    die $usage if $nargs < 0 or $nargs > 3;

    my $targets = shift;
    $targets = [$targets] unless ref $targets eq "ARRAY";
    $self->{targets} = $targets;

    if ($#_ != -1) {
        if (ref $_[-1] eq "CODE") {
            $self->{ref} = pop;
        }

        my ($conditions,$doc) = @_;

        if (defined $conditions) {
            $conditions = [$conditions] unless ref $conditions eq "ARRAY";
        } else {
            $conditions = [];
        }
        $self->{conditions} = $conditions;
        $self->{doc} = $doc if defined $doc;
    }

    return $self;
}

# $rule->check_if_targets_exist($pars);
#
# Returns 1 if any of the targets exist in $pars, 0 otherwise.
# A return value of 1 means that the rule should not be applied.
#
# Not 100% happy with use of report here. Needs re-thinking.
#
sub check_if_targets_exist {
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

# $rule->check_if_conditions_exist($pars);
#
# Returns 1 if all of the required conditions exist in $pars, 0 otherwise.
# A return value of 0 means that the rule should not be applied.
#
# Not 100% happy with use of report here. Needs re-thinking.
#
sub check_if_conditions_exist {
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

# $rule->is_valid($pars);
#
# Returns 1 if the rule should be applied (ie no targets already
# exist in $pars and all the required conditions exist in $pars),
# otherwise 0.
#
sub is_valid {
    my $self = shift;
    my $pars = shift;

    return 0 if $self->check_if_targets_exist($pars);
    return 0 unless $self->check_if_conditions_exist($pars);
    return 1;
}

# my @args = $self->extract_args($pars);
#
# If this method is called we assume that
#   $self->check_if_conditions_exist($pars)
# returns 1.
#
sub extract_args {
    my $self = shift;
    my $pars = shift;

    my $conditions = $self->{conditions};

    my @args;
    foreach (@$conditions) {
		# make a copy of each condition so that any changes to it are not
		# also made to the original array!
		my $condition = $_;
		# Remove any possible underscores (which indicate optional conditions):
		$condition =~ s/^_//;

		# Note: This will *not* create $pars->{$condition} if it did not already
		# exist:
		push @args, $pars->{$condition};
    }

    return @args;
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

    # Is the rule valid?
    #
    return unless $self->is_valid($pars);

    # Create the argument array for the routine.
    #
    my @args = $self->extract_args($pars);

    # Run this rule's subroutine:
    my @retval = $self->{ref}(@args);

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
    croak('Usage: PDL::PP::Ruel::Croak->new(["incompatible", "arguments"], "Croaking message")')
		unless @_ == 3;
    
    my $class = shift;
    my $self  = $class->SUPER::new([], @_);
    return bless $self, $class;
}

sub apply {
    my ($self, $pars) = @_;
    croak($self->{doc}) if $self->is_valid($pars);
}

package PDL::PP::Rule::Returns;

use strict;

use Carp;
our @CARP_NOT;

##use PDL::PP::Rule;
our @ISA = qw (PDL::PP::Rule);

# This class does not treat return values of "DO NOT SET!!"
# as special.
#
sub new {
    my $class = shift;

    my $value = pop;

    my @args  = @_;
    my $self  = $class->SUPER::new(@args);
    bless $self, $class;
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

    # Is the rule valid?
    #
    return unless $self->is_valid($pars);

    # Set the value
    #
    $self->report ("--setting: $target\n");
    $pars->{$target} = $self->{"returns.value"};
}

package PDL::PP::Rule::Returns::Zero;

use strict;

##use PDL::PP::Rule::Returns;
our @ISA = qw (PDL::PP::Rule::Returns);

sub new {
    my $class = shift;
    my @args  = @_;
    my $self  = $class->SUPER::new(@args,0);
    bless $self, $class;
    return $self;
}

package PDL::PP::Rule::Returns::One;

use strict;

##use PDL::PP::Rule::Returns;
our @ISA = qw (PDL::PP::Rule::Returns);

sub new {
    my $class = shift;
    my @args  = @_;
    my $self  = $class->SUPER::new(@args,1);
    bless $self, $class;
    return $self;
}

package PDL::PP::Rule::Returns::EmptyString;

use strict;

##use PDL::PP::Rule::Returns;
our @ISA = qw (PDL::PP::Rule::Returns);

sub new {
    my $class = shift;
    my @args  = @_;
    my $self  = $class->SUPER::new(@args,"");
    bless $self, $class;
    return $self;
}

package PDL::PP::Rule::Returns::NULL;

use strict;

##use PDL::PP::Rule::Returns;
our @ISA = qw (PDL::PP::Rule::Returns);

sub new {
    my $class = shift;
    my @args  = @_;
    my $self  = $class->SUPER::new(@args,"NULL");
    bless $self, $class;
    return $self;
}

package PDL::PP::Rule::InsertName;

use strict;

use Carp;
our @CARP_NOT;

##use PDL::PP::Rule;
our @ISA = qw (PDL::PP::Rule);

# This class does not treat return values of "DO NOT SET!!"
# as special.
#
sub new {
    my $class = shift;

    my $value = pop;

    my @args  = @_;
    my $self  = $class->SUPER::new(@args);
    bless $self, $class;
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

    # Is the rule valid?
    #
    return unless $self->is_valid($pars);

    # Set the value
    #
    my $target = $self->{targets}->[0];
    my $name   = $pars->{Name};
    $self->report ("--setting: $target (name=$name)\n");
    $pars->{$target} = eval "return \"" . $self->{"insertname.value"} . "\";";
}

# Poor name. This is the old "dosubst" routine
#
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

##use PDL::PP::Rule;
our @ISA = qw (PDL::PP::Rule);

# Probably want this directly in the apply routine but leave as is for now
#
sub dosubst_private {
    my ($src,$symtab,$name) = @_;
    my $ret = (ref $src ? $src->[0] : $src);
    my %syms = (
		((ref $src) ? %{$src->[1]} : ()),
		PRIV => sub {return "".$symtab->get_symname('_PDL_ThisTrans').
			       "->$_[0]"},
		CROAK => sub {PDL::PP::pp_line_numbers(__LINE__, "PDL->pdl_barf(\"Error in $name:\" $_[0])")},
		NAME => sub {return $name},
		MODULE => sub {return $::PDLMOD},

		SETPDLSTATEBAD  => sub { PDL::PP::pp_line_numbers(__LINE__, "$_[0]\->state |= PDL_BADVAL") },
		SETPDLSTATEGOOD => sub { PDL::PP::pp_line_numbers(__LINE__, "$_[0]\->state &= ~PDL_BADVAL") },
		ISPDLSTATEBAD   => sub { PDL::PP::pp_line_numbers(__LINE__, "(($_[0]\->state & PDL_BADVAL) > 0)") },
		ISPDLSTATEGOOD  => sub { PDL::PP::pp_line_numbers(__LINE__, "(($_[0]\->state & PDL_BADVAL) == 0)") },
		BADFLAGCACHE    => sub { PDL::PP::pp_line_numbers(__LINE__, "badflag_cache") },

		SETREVERSIBLE => sub {
		    PDL::PP::pp_line_numbers(__LINE__, "if($_[0]) \$PRIV(flags) |= PDL_ITRANS_REVERSIBLE;\n" .
		      "   else \$PRIV(flags) &= ~PDL_ITRANS_REVERSIBLE;\n")
		  },
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

    die "\$target must be a scalar for PDL::PP::Rule->Substitute" if ref $target;
    die "\$condition must be a scalar for PDL::PP::Rule->Substitute" if ref $condition;

    my $self = $class->SUPER::new($target, [$condition, "NewXSSymTab", "Name"],
				  \&dosubst_private);
    bless $self, $class;

    return $self;
}

# Poor name. This is the old "dousualsubsts" routine
#
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

##use PDL::PP::Rule;
our @ISA = qw (PDL::PP::Rule::Substitute);

# This is a copy of the main one for now. Need a better solution.
#
my @std_childparent = (
	CHILD => sub {PDL::PP::pp_line_numbers(__LINE__, '$PRIV(pdls[1]->'.(join ',',@_).")")},
	PARENT => sub {PDL::PP::pp_line_numbers(__LINE__, '$PRIV(pdls[0]->'.(join ',',@_).")")},
	CHILD_P => sub {PDL::PP::pp_line_numbers(__LINE__, '$PRIV(pdls[1]->'.(join ',',@_).")")},
	PARENT_P => sub {PDL::PP::pp_line_numbers(__LINE__, '$PRIV(pdls[0]->'.(join ',',@_).")")},
	CHILD_PTR => sub {PDL::PP::pp_line_numbers(__LINE__, '$PRIV(pdls[1])')},
	PARENT_PTR => sub {PDL::PP::pp_line_numbers(__LINE__, '$PRIV(pdls[0])')},
	COMP => sub {PDL::PP::pp_line_numbers(__LINE__, '$PRIV('.(join ',',@_).")")}
);

sub get_std_childparent { return @std_childparent; }

sub new {
    my $class = shift;

    my @args = @_;
    my $self = $class->SUPER::new(@args);
    bless $self, $class;

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

    # The conditions are [<code>, NewXSSymTab, Name]
    #
    my $code   = $pars->{$self->{conditions}[0]};
    my $symtab = $pars->{$self->{conditions}[1]};
    my $name   = $pars->{$self->{conditions}[2]};

    return ([$code,{@std_childparent}],$symtab,$name);
}

# Poor name. This is the old "subst_makecomp" routine
#
#  PDL::PP::Rule->new("MakeCompiledRepr", ["MakeComp","CompNames","CompObjs"],
#		      sub {subst_makecomp("COMP",@_)}),
#
# PDL::PP::Rule::MakeComp->new($target,$conditions,$symbol)
#   $target and $symbol must be scalars.
#
package PDL::PP::Rule::MakeComp;

use strict;

use Carp;
our @CARP_NOT;

##use PDL::PP::Rule;
our @ISA = qw (PDL::PP::Rule);

# This is a copy of the main one for now. Need a better solution.
#
my @std_redodims = (
		    SETNDIMS => sub {PDL::PP::pp_line_numbers(__LINE__, "PDL->reallocdims(__it,$_[0])")},
		    SETDIMS => sub {PDL::PP::pp_line_numbers(__LINE__, "PDL->setdims_careful(__it)")},
		    SETDELTATHREADIDS => sub {PDL::PP::pp_line_numbers(__LINE__, '
		{int __ind; PDL->reallocthreadids($CHILD_PTR(),
			$PARENT(nthreadids));
		for(__ind=0; __ind<$PARENT(nthreadids)+1; __ind++) {
			$CHILD(threadids[__ind]) =
				$PARENT(threadids[__ind]) + ('.$_[0].');
		}
		}
		')});

##sub get_std_redodims { return @std_redodims; }

# Probably want this directly in the apply routine but leave as is for now
#
sub subst_makecomp_private {
	my($which,$mc,$cn,$co) = @_;
	return [$mc,{
#		@::std_childparent,
		PDL::PP::Rule::Substitute::Usual::get_std_childparent(),
		($cn ?
			(('DO'.$which.'DIMS') => sub {PDL::PP::pp_line_numbers(__LINE__, join '',
				map{$$co{$_}->need_malloc ?
				    $$co{$_}->get_malloc('$PRIV('.$_.')') :
				    ()} @$cn)}) :
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
    bless $self, $class;
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

    # The conditions are [<symbol>, conditions...]
    # - could use slicing here
    #
    my @args = ($self->{"makecomp.value"});
    foreach my $condition (@{$self->{conditions}}) {
      push @args, $pars->{$condition};
    }
    return @args;
}

package PDL::PP;

use strict;

our $VERSION = "2.3";
$VERSION = eval $VERSION;

use PDL::Types ':All';
use Config;
use FileHandle;
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

END {
    #you can uncomment this for testing, but this should remain
    #commented in production code. This causes pp_done to be called
    #even when a .pd file aborts with die(), potentially bypassing
    #problem code when build is re-attempted. Having this commented
    #means we are a bit more strict: a module must call pp_done in
    #order to have .xs and .pm files written.
#  pp_done() unless $PDL::PP::done;
}

use Carp;
our @CARP_NOT;

# check for bad value support
use PDL::Config;
my $bvalflag = $PDL::Config{WITH_BADVAL} || 0;

my $ntypes = $#PDL::Types::names;

sub nopm { $::PDLPACK eq 'NONE' } # flag that we don't want to generate a PM

sub import {
	my ($mod,$modname, $packname, $prefix, $callpack) = @_;
	# Allow for users to not specify the packname
	($packname, $prefix, $callpack) = ($modname, $packname, $prefix)
		if ($packname =~ m|/|);

	$::PDLMOD=$modname; $::PDLPACK=$packname; $::PDLPREF=$prefix;
	$::CALLPACK = defined $callpack ? $callpack : $::PDLMOD;
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
	$::PDLMODVERSION = '$VERSION';
	$::PDLVERSIONSET = "\$$::PDLPACK\::VERSION = $ver;";
}

sub pp_addhdr {
	my ($hdr) = @_;
	$::PDLXSC .= $hdr;
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
	# my ($this,$exp) = @_;
        my $exp = join ' ', @_; # get rid of this silly $this argument
	$::PDLPMROUT .= $exp." ";
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
	$::PDLXSBOOT .= $boot." ";
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
	my @to_return = "\n#line $line \"$filename\"\n";

	# Look for threadloops and loops and add # line directives
	foreach (split (/\n/, $string)) {
		# Always add the current line.
		s/^=/ =/; # so doesn't look like POD
		push @to_return, "$_\n";
		# If we need to add a # line directive, do so after incrementing
		$line++;
		if (/%\{/ or /%}/) {
			push @to_return, "#line $line \"$filename\"\n";
		}
	}

	return join('', @to_return);
}

sub printxsc {
	shift;
	$::PDLXSC .= join '',@_;
}

sub pp_done {
        return if $PDL::PP::done; # do only once!
        $PDL::PP::done = 1;
        $::FUNCSPOD = $::DOCUMENTED ? "\n\n=head1 FUNCTIONS\n\n\n\n=cut\n\n\n"
	  : '';
	print "DONE!\n" if $::PP_VERBOSE;
	print "Inline running PDL::PP version $PDL::PP::VERSION...\n" if nopm();
	(my $fh = FileHandle->new(">$::PDLPREF.xs")) or die "Couldn't open xs file\n";
        my $pdl_boot = PDL::Core::Dev::PDL_BOOT('PDL', $::PDLMOD); # don't hardcode in more than one place

$fh->print(pp_line_numbers(__LINE__, qq%
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

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "pdl.h"
#include "pdlcore.h"
static Core* PDL; PDL_COMMENT("Structure hold core C functions")
static int __pdl_debugging = 0;
static int __pdl_boundscheck = 0;
static SV* CoreSV;       PDL_COMMENT("Gets pointer to perl var holding core structure")

#if ! $PP::boundscheck
# define PP_INDTERM(max, at) at
#else
# define PP_INDTERM(max, at) (__pdl_boundscheck? PDL->safe_indterm(max,at, __FILE__, __LINE__) : at)
#endif

$::PDLXSC

MODULE = $::PDLMOD PACKAGE = $::PDLMOD

PROTOTYPES: ENABLE

int
set_debugging(i)
	int i;
	CODE:
	RETVAL = __pdl_debugging;
	__pdl_debugging = i;
	OUTPUT:
	RETVAL

int
set_boundscheck(i)
       int i;
       CODE:
       if (! $PP::boundscheck)
         warn("Bounds checking is disabled for $::PDLMOD");
       RETVAL = __pdl_boundscheck;
       __pdl_boundscheck = i;
       OUTPUT:
       RETVAL


MODULE = $::PDLMOD PACKAGE = $::PDLOBJ

$::PDLXS

BOOT:

   PDL_COMMENT("Get pointer to structure of core shared C routines")
   PDL_COMMENT("make sure PDL::Core is loaded")
   $pdl_boot
   $::PDLXSBOOT
%));

unless (nopm) {
	$::PDLPMISA = "'".join("','",@::PDLPMISA)."'";
	$::PDLBEGIN = "BEGIN {\n$::PDLBEGIN\n}"
		unless $::PDLBEGIN =~ /^\s*$/;
	($fh = FileHandle->new(">$::PDLPREF.pm")) or die "Couldn't open pm file\n";

	$fh->print(qq%
#
# GENERATED WITH PDL::PP! Don't modify!
#
package $::PDLPACK;

\@EXPORT_OK  = qw( $::PDLPMROUT);
\%EXPORT_TAGS = (Func=>[\@EXPORT_OK]);

use PDL::Core$::PDLCOREIMPORT;
use PDL::Exporter;
use DynaLoader;


$::PDL_IFBEGINWRAP[0]
   $::PDLVERSIONSET
   \@ISA    = ( $::PDLPMISA );
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

		   %);  # end of print
      }  # unless (nopm) {...
} # end pp_done

sub pp_def {
	my($name,%obj) = @_;

	print "*** Entering pp_def for $name\n" if $::PP_VERBOSE;
	
	# See if the 'name' is multiline, in which case we extract the
	# name and add the FullDoc field
	if ($name =~ /\n/) {
		my $fulldoc = $name;
		# See if the very first thing is a word. That is going to be the
		# name of the function under consideration
		if ($fulldoc =~ s/^(\w+)//) {
			$name = $1;
		}
		elsif ($fulldoc =~ /=head2 (\w+)/) {
			$name = $1;
		}
		else {
			croak('Unable to extract name');
		}
		$obj{FullDoc} = $fulldoc;
	}
	
	$obj{Name} = $name;
	translate(\%obj,$PDL::PP::deftbl);

	print "Output of translate for $name:\n" . Dumper(\%obj) . "\n"
	  if exists $obj{Dump} and $obj{Dump} and $::PP_VERBOSE;

	croak("ERROR: No FreeFunc for pp_def=$name!\n")
	  unless exists $obj{FreeFunc}; # and $obj{FreeFunc};

	PDL::PP->printxsc(join "\n\n",@obj{'StructDecl','RedoDimsFunc',
		'CopyFunc',
		'ReadDataFunc','WriteBackDataFunc',
		'FreeFunc',
		'FooFunc',
		'VTableDef','NewXSInPrelude',
		}
		);
	PDL::PP->printxs($obj{NewXSCode});
	pp_add_boot($obj{XSBootCode} . $obj{BootSetNewXS});
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
$SIG{__DIE__} = sub {print Carp::longmess(@_); die;}
  if $::PP_VERBOSE;  # seems to give us trouble with 5.6.1

use PDL::PP::Signature;
use PDL::PP::Dims;
use PDL::PP::CType;
use PDL::PP::XS;
use PDL::PP::SymTab;
use PDL::PP::PDLCode;

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
# This is an extended typemap handler from the one earlier written by
# Doug Hunt. It should work exactly as the older version, but with extensions.
# Instead of handling a few special cases explicitly we now use Perl's
# built-in typemap handling using code taken straight from xsubpp.
#
# I have infact kept the old part of the code here because I belive any
# subsequent hackers might find it very helpful to refer to this code to
# understand what the following does. So here goes:
#
# ------------ OLD TYPEMAP PARSING: ------------------------
#
#   # Note that I now just look at the basetype.  I don't
#   # test whether it is a pointer to the base type or not.
#   # This is done because it is simpler and I know that the otherpars
#   # belong to a restricted set of types.  I know a char will really
#   # be a char *, for example.  I also know that an SV will be an SV *.
#   #    yes, but how about catching syntax errors in OtherPars (CS)?
#   #    shouldn't we really parse the perl typemap (we can steal the code
#   #    from xsubpp)?
#
#   my $OLD_PARSING=0;
#   if ($OLD_PARSING) {
#     my %typemap = (char     => "(char *)SvPV($arg,PL_na)",
# 		   short    => "(short)SvIV($arg)",
# 		   int      => "(int)SvIV($arg)",
# 		   long     => "(long)SvIV($arg)",
# 		   double   => "(double)SvNV($arg)",
# 		   float    => "(float)SvNV($arg)",
# 		   SV       => "$arg",
# 		  );
#     my $basetype = $type->{Base};
#     $basetype =~ s/\s+//g;  # get rid of whitespace
#
#     die "Cannot find $basetype in my (small) typemap" unless exists($typemap{$basetype});
#     return ($typemap{$basetype});
#   }
#
#--------- END OF THE OLD CODE ---------------
#
# The code loads the typemap from the Perl typemap using the loading logic of
# xsubpp. Do note that I  made the assumption that
# $Config{}installprivlib}/ExtUtils was the right root directory for the search.
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

  #
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
  # if this does not work portably we should split out the typemap finding code
  # and make it as complex as necessary + save the typemap location
  # in the PDL::Config hash
  my $_rootdir = $Config{privlibexp}.'/ExtUtils/';
#  print "_rootdir set to '$_rootdir'\n";

  # First the system typemaps..
  my @tm = ($_rootdir.'../../../../lib/ExtUtils/typemap',
	    $_rootdir.'../../../lib/ExtUtils/typemap',
	    $_rootdir.'../../lib/ExtUtils/typemap',
	    $_rootdir.'../../../typemap',
	    $_rootdir.'../../typemap', $_rootdir.'../typemap',
	    $_rootdir.'typemap');
  # Finally tag onto the end, the current directory typemap. Ideally we should here pick
  # up the TYPEMAPS flag from ExtUtils::MakeMaker, but a) I don't know how and b)
  # it is only a slight inconvenience hopefully!
  #
  # Note that the OUTPUT typemap is unlikely to be of use here, but I have kept
  # the source code from xsubpp for tidiness.
  push @tm, 'typemap';
  my $foundtm = 0;
  foreach $typemap (@tm) {
    next unless -f $typemap ;
    # skip directories, binary files etc.
    warn("Warning: ignoring non-text typemap file '$typemap'\n"), next
      unless -T $typemap ;
    $foundtm = 1;
    open(TYPEMAP, $typemap)
      or warn ("Warning: could not open typemap file '$typemap': $!\n"), next;
    $mode = 'Typemap';
    $junk = "" ;
    $current = \$junk;
    while (<TYPEMAP>) {
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
    close(TYPEMAP);
  }
  carp "**CRITICAL** PP found no typemap in $_rootdir/typemap; this will cause problems..."
      unless $foundtm;

  #
  # Do checks...
  #
  # First reconstruct the type declaration to look up in type_kind
  my $full_type=TidyType($type->get_decl('')); # Skip the variable name
  die "The type =$full_type= does not have a typemap entry!\n" unless exists($type_kind{$full_type});
  my $typemap_kind = $type_kind{$full_type};
  # Look up the conversion from the INPUT typemap. Note that we need to do some
  # massaging of this.
  my $input = $input_expr{$typemap_kind};
  # Remove all before =:
  $input =~ s/^(.*?)=\s*//; # This should not be very expensive
  # Replace $arg with $arg
  $input =~ s/\$arg/$arg/;
  # And type with $full_type
  $input =~ s/\$type/$full_type/;

  return ($input);
}


sub identity2priv {
	PDL::PP::pp_line_numbers(__LINE__, '
		int i;
		$SETNDIMS($PARENT(ndims));
		for(i=0; i<$CHILD(ndims); i++) {
			$CHILD(dims[i]) = $PARENT(dims[i]);
		}
		$SETDIMS();
		$SETDELTATHREADIDS(0);
	');
}

sub pdimexpr2priv {
	my($pdimexpr,$hdr,$dimcheck) = @_;
	$pdimexpr =~ s/\$CDIM\b/i/g;
	PDL::PP::pp_line_numbers(__LINE__, '
		int i,cor;
		'.$dimcheck.'
		$SETNDIMS($PARENT(ndims));
		$DOPRIVDIMS();
		$PRIV(offs) = 0;
		for(i=0; i<$CHILD(ndims); i++) {
			cor = '.$pdimexpr.';
			$CHILD(dims[i]) = $PARENT(dims[cor]);
			$PRIV(incs[i]) = $PARENT(dimincs[cor]);

		}
		$SETDIMS();
		$SETDELTATHREADIDS(0);
	');
}

# something to do with copying values between parent and children
#
# we can NOT assume that PARENT and CHILD have the same type,
# hence the version for bad code
#
# NOTE: we use the same code for 'good' and 'bad' cases - it's
# just that when we use it for 'bad' data, we have to change the
# definition of the EQUIVCPOFFS macro - see the Code rule
#
sub equivcpoffscode {
    PDL::PP::pp_line_numbers(__LINE__,
	'PDL_Indx i;
         for(i=0; i<$CHILD_P(nvals); i++)  {
            $EQUIVCPOFFS(i,i);
         }');

} # sub: equivcpoffscode()

# Pars -> ParNames, Parobjs
#
# XXX
# - the need for BadFlag is due to hacked get_xsdatapdecl()
#   in PP/PdlParObj and because the PdlParObjs are created by
#   PDL::PP::Signature (Doug Burke 07/08/00)
sub Pars_nft {
	my($str,$badflag) = @_;
	my $sig = PDL::PP::Signature->new($str,$badflag);
	return ($sig->names,$sig->objs,1);
}

# ParNames,Parobjs -> DimObjs
sub ParObjs_DimObjs {
	my($pnames,$pobjs) = @_;
	my ($dimobjs) = PDL::PP::PdlDimsObj->new();
	for(@$pnames) {
		$pobjs->{$_}->add_inds($dimobjs);
	}
	return ($dimobjs);
}

# Eliminate whitespace entries
sub nospacesplit {map {/^\s*$/?():$_} split $_[0],$_[1]}

sub OtherPars_nft {
    my($otherpars,$dimobjs) = @_;
    my(@names,%types,$type);
    # support 'int ndim => n;' syntax
    for (nospacesplit ';',$otherpars) {
	if (/^\s*([^=]+)\s*=>\s*(\S+)\s*$/) {
	    my ($ctype,$dim) = ($1,$2);
	    $ctype =~ s/(\S+)\s+$/$1/; # get rid of trailing ws
	    print "OtherPars: setting dim '$dim' from '$ctype'\n" if $::PP_VERBOSE;
	    $type = C::Type->new(undef,$ctype);
	    croak "can't set unknown dimension"
		unless defined($dimobjs->{$dim});
	    $dimobjs->{$dim}->set_from($type);
	} elsif(/^\s*pdl\s+\*\s*(\w+)$/) {
	    # It is a piddle -> make it a controlling one.
	    die("Not supported yet");
	} else {
	    $type = C::Type->new(undef,$_);
	}
	my $name = $type->protoname;
        if ($name =~ /$INVALID_OTHERPARS_RE/) {
            croak "Invalid OtherPars name: $name";
        }
	push @names,$name;
	$types{$name} = $type;
    }
    return (\@names,\%types);
}

sub NXArgs {
	my($parnames,$parobjs,$onames,$oobjs) = @_;
	my $pdltype = C::Type->new(undef,"pdl *__foo__");
	my $nxargs = [
		( map {[$_,$pdltype]} @$parnames ),
		( map {[$_,$oobjs->{$_}]} @$onames )
	];
	return $nxargs;
}

# XXX
# - the need for BadFlag is due to hacked get_xsdatapdecl()
#   in PP/PdlParObj and because the PdlParObjs are created by
#   PDL::PP::Signature (Doug Burke 07/08/00)
sub NewParentChildPars {
    my($p2child,$name,$badflag) = @_;
    return (Pars_nft("PARENT(); [oca]CHILD();",$badflag),0,"${name}_NN");
}

# XXX
# - the need for BadFlag is due to hacked get_xsdatapdecl()
#   in PP/PdlParObj and because the PdlParObjs are created by
#   PDL::PP::Signature (Doug Burke 07/08/00)
#
# however, it looks like this isn't being used anymore,
# so commenting out.
#
#sub ParentChildPars {
#	my($p2child,$name,$badflag) = @_;
#	return (Pars_nft("PARENT(); [oca]CHILD();",$badflag),0,"${name}_XX",
#	"
#	*$name = \\&PDL::$name;
#	sub PDL::$name {
#		my \$this = shift;
#		my \$foo=\$this->null;
#		PDL::${name}_XX(\$this,\$foo,\@_);
#		\$foo
#	}
#	");
#}

sub mkstruct {
	my($pnames,$pobjs,$comp,$priv,$name) = @_;
	my $npdls = $#$pnames+1;
	PDL::PP::pp_line_numbers(__LINE__, qq{typedef struct $name {
		PDL_TRANS_START($npdls);
		$priv
		$comp
		char __ddone; PDL_COMMENT("Dims done")
		} $name;});
}

sub def_vtable {
    my($vname,$sname,$rdname,$rfname,$wfname,$cpfname,$ffname,
       $pnames,$pobjs,$affine_ok,$foofname) = @_;
    my $nparents = 0 + grep {! $pobjs->{$_}->{FlagW}} @$pnames;
    my $aff = ($affine_ok ? "PDL_TPDL_VAFFINE_OK" : 0);
    my $npdls = scalar @$pnames;
    my $join_flags = join",",map {$pobjs->{$pnames->[$_]}->{FlagPhys} ?
				      0 : $aff} 0..$npdls-1;
    if($Config{cc} eq 'cl') {
       $join_flags = '""' if $join_flags eq '';
    }
    PDL::PP::pp_line_numbers(__LINE__, "static char ${vname}_flags[] =
	 	{ ". $join_flags . "};
	 pdl_transvtable $vname = {
		0,0, $nparents, $npdls, ${vname}_flags,
		$rdname, $rfname, $wfname,
		$ffname,NULL,NULL,$cpfname,
		sizeof($sname),\"$vname\"
	 };");
}

sub sort_pnobjs {
    my($pnames,$pobjs) = @_;
    my (@nn);
    for(@$pnames) { push ( @nn, $_ ) unless $pobjs->{$_}{FlagW}; }
    for(@$pnames) { push ( @nn, $_ ) if $pobjs->{$_}{FlagW}; }
    my $no = 0;
    for(@nn) { $pobjs->{$_}{Number} = $no++; }
    return (\@nn,$pobjs);
}

# XXX __privtrans explicit :(
sub wrap_vfn {
    my($code,$hdrinfo,$rout,$p2child,$name) = @_;
    my $type = ($name eq "copy" ? "pdl_trans *" : "void");
    my $sname = $hdrinfo->{StructName};
    my $oargs = ($name eq "foo" ? ",int i1,int i2,int i3" : "");

#	print "$rout\_$name: $p2child\n";
    my $p2decl = '';
    # Put p2child in simple boolean context rather than strict numerical equality
    if ( $p2child ) {
	$p2decl =
	    PDL::PP::pp_line_numbers(__LINE__, "pdl *__it = ((pdl_trans_affine *)(__tr))->pdls[1]; pdl *__parent = __tr->pdls[0];");
	if ( $name eq "redodims" ) {
	    $p2decl .= '
	     if (__parent->hdrsv && (__parent->state & PDL_HDRCPY)) {
                  PDL_COMMENT("call the perl routine _hdr_copy.")
                  int count;

                  dSP;
                  ENTER ;
                  SAVETMPS ;
                  PUSHMARK(SP) ;
                  XPUSHs( sv_mortalcopy((SV*)__parent->hdrsv) );
                  PUTBACK ;
                  count = call_pv("PDL::_hdr_copy",G_SCALAR);
                  SPAGAIN ;
                  if(count != 1)
                      croak("PDL::_hdr_copy didn\'t return a single value - please report this bug (B).");

                  { PDL_COMMENT("convenience block for tmp var")
                    SV *tmp = (SV *) POPs ;
		    __it->hdrsv = (void*) tmp;
                    if(tmp != &PL_sv_undef )
                       (void)SvREFCNT_inc(tmp);
                  }

                  __it->state |= PDL_HDRCPY;

                  FREETMPS ;
                  LEAVE ;
             }
        ';
	}
    } # if: $p2child == 1

    qq|$type $rout(pdl_trans *__tr $oargs) {
	int __dim;
	$sname *__privtrans = ($sname *) __tr;
	$p2decl
	{
	    $code
	}
    }
    |;

} # sub: wrap_vfn()

sub makesettrans {
    my($pnames,$pobjs,$symtab) = @_;
    my $trans = $symtab->get_symname('_PDL_ThisTrans');
    my $no=0;
    PDL::PP::pp_line_numbers(__LINE__, (join '',map {
	"$trans->pdls[".($no++)."] = $_;\n"
	} @$pnames).
	    "PDL->make_trans_mutual((pdl_trans *)$trans);\n");
}

sub CopyOtherPars {
	my($onames,$otypes,$symtab) = @_; my $repr;
	my $sname = $symtab->get_symname('_PDL_ThisTrans');
	for(@$onames) {
		$repr .= $otypes->{$_}->get_copy("$_","$sname->$_");
	}
	PDL::PP::pp_line_numbers(__LINE__, $repr);
}

sub mkxscat {
	my($glb,$xs_c_headers,$hdr,@bits) = @_;
	my($boot,$prelude,$str);
	if($glb) {
		$prelude = join '' => ($xs_c_headers->[0], @bits, $xs_c_headers->[1]);
		$boot = $xs_c_headers->[3];
		$str = "$hdr\n";
	} else {
		my $xscode = join '' => @bits;
		$str = "$hdr CODE:\n { $xscode XSRETURN(0);\n}\n\n";
	}
	$str =~ s/(\s*\n)+/\n/g;
	(PDL::PP::pp_line_numbers(__LINE__, $str),$boot,$prelude)
}

sub mkVarArgsxscat {
	my($glb,$xs_c_headers,$hdr,@bits) = @_;
	my($boot,$prelude,$str);
	if($glb) {
		$prelude = join '' => ($xs_c_headers->[0], @bits, $xs_c_headers->[1]);
		$boot = $xs_c_headers->[3];
		$str = "$hdr\n";
	} else {
		my $xscode = join '' => @bits;
		$str = "$hdr \n { $xscode \n}\n\n";
	}
	$str =~ s/(\s*\n)+/\n/g;
	(PDL::PP::pp_line_numbers(__LINE__, $str),$boot,$prelude)
}


sub MakeNows {
    my($pnames, $symtab) = @_;
    my $str = "\n";
    for(@$pnames) { $str .= "$_ = PDL->make_now($_);\n"; }
    PDL::PP::pp_line_numbers(__LINE__, $str);
}

sub Sym2Loc { PDL::PP::pp_line_numbers(__LINE__, $_[0]->decl_locals()) }

sub MkPrivStructInit {
    my( $symtab, $vtable, $affflag, $nopdlthread ) = @_;
    my $sname = $symtab->get_symname('_PDL_ThisTrans');

    my $ci = '   ';
    PDL::PP::pp_line_numbers(__LINE__,
	"\n${ci}$sname = malloc(sizeof(*$sname)); memset($sname, 0, sizeof(*$sname));\n" .
	($nopdlthread ? "" : "${ci}PDL_THR_CLRMAGIC(&$sname->__pdlthread);\n") .
	"${ci}PDL_TR_SETMAGIC($sname);\n" .
	"${ci}$sname->flags = $affflag;\n" .
	"${ci}$sname->__ddone = 0;\n" .
	"${ci}$sname->vtable = &$vtable;\n" .
	"${ci}$sname->freeproc = PDL->trans_mallocfreeproc;\n")

} # sub: MkPrivStructInit()

sub MkDefSyms {
    return SymTab->new(
		       _PDL_ThisTrans => ["__privtrans",C::Type->new(undef,"$_[0] *foo")],
		      );
}

sub AddArgsyms {
	my($symtab,$args) = @_;
	$symtab->add_params(
		map {($_->[0],$_->[0])} @$args
	);
	return $symtab;
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
	unless ($callcopy) { $ret .= << "EOC"}

if (strcmp(objname,"PDL") == 0) { PDL_COMMENT("shortcut if just PDL")
   $name\_SV = sv_newmortal();
   $name = PDL->null();
   PDL->SetSV_PDL($name\_SV,$name);
   if (bless_stash) $name\_SV = sv_bless($name\_SV, bless_stash);
} else {
   PUSHMARK(SP);
   XPUSHs(sv_2mortal(newSVpv(objname, 0)));
   PUTBACK;
   perl_call_method(\"initialize\", G_SCALAR);
   SPAGAIN;
   $name\_SV = POPs;
   PUTBACK;
   $name = PDL->SvPDLV($name\_SV);
}

EOC

    else { $ret .= << "EOD" }

if (strcmp(objname,"PDL") == 0) { PDL_COMMENT("shortcut if just PDL")
   $name\_SV = sv_newmortal();
   $name = PDL->null();
   PDL->SetSV_PDL($name\_SV,$name);
   if (bless_stash) $name\_SV = sv_bless($name\_SV, bless_stash);
} else {
   /* XXX should these commented lines be removed? See also a 8 lines down */
   /* warn("possibly relying on deprecated automatic copy call in derived class\n")
   warn("please modify your initialize method to avoid future problems\n");
   */
   PUSHMARK(SP);
   XPUSHs(parent);
   PUTBACK;
   perl_call_method(\"copy\", G_SCALAR);
   /* perl_call_method(\"initialize\", G_SCALAR); */
   SPAGAIN;
   $name\_SV = POPs;
   PUTBACK;
   $name = PDL->SvPDLV($name\_SV);
}
EOD

  } # doreach: $name

  PDL::PP::pp_line_numbers(__LINE__, indent($ret,$ci));

} #sub callPerlInit()

# This subroutine is called when no 'otherpars' exist.
# This writes an XS header which handles variable argument lists,
# thus avoiding the perl layer in calling the routine. D. Hunt 4/11/00
#
# The use of 'DO NOT SET!!' looks ugly.
#
# Removing useless use of hasp2child in this function. DCM Sept 12, 2011
sub VarArgsXSHdr {
  my($name,$xsargs,$parobjs,$optypes,#$hasp2child,
     $pmcode,$hdrcode,$inplacecode,$globalnew,$callcopy,$bitwise) = @_;

  # Don't do var args processing if the user has pre-defined pmcode
  return 'DO NOT SET!!' if ($pmcode);

  # don't generate a HDR if globalnew is set
  # globalnew implies internal usage, not XS
  return undef if $globalnew;

  my $ci = '  ';  # current indenting
  my $pars = join "\n",map {$ci.$_->[1]->get_decl($_->[0]).";"} @$xsargs;

  my @args   = map { $_->[0] } @$xsargs;
  my %out    = map { $_ => exists($$parobjs{$_})
		       && exists($$parobjs{$_}{FlagOut})
				 && !exists($$parobjs{$_}{FlagCreateAlways})}
		     @args;
  my %outca = map { $_ => exists($$parobjs{$_})
		       && exists($$parobjs{$_}{FlagOut})
				 && exists($$parobjs{$_}{FlagCreateAlways})}
		     @args;
  my %tmp    = map { $_ => exists($$parobjs{$_}) && exists($$parobjs{$_}{FlagTemp}) } @args;
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
  my $svdecls = join ("\n", map { "${ci}SV *${_}_SV;" } grep { $out{$_} || $outca{$_} || $tmp{$_} } @args);

  my @create = ();  # The names of variables which need to be created by calling
                    # the 'initialize' perl routine from the correct package.

  $ci = '    ';  # Current indenting

  # clause for reading in all variables
  my $clause1 = ''; my $cnt = 0;
  foreach my $i ( 0 .. $#args ) {
      my $x = $args[$i];
      if ($other{$x}) {  # other par
	  $clause1 .= "$ci$x = " . typemap($x, $$optypes{$x}, "ST($cnt)") . ";\n";
	  $cnt++;
      } elsif ($outca{$x}) {
	  push (@create, $x);
      } else {
	  $clause1 .= "$ci$x = PDL->SvPDLV(ST($cnt));\n";
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
      foreach my $i ( 0 .. $#args ) {
	  my $x = $args[$i];
	  if ($other{$x}) {
	      $clause2 .= "$ci$x = " . typemap($x, $$optypes{$x}, "ST($cnt)") . ";\n";
	      $cnt++;
	  } elsif ($tmp{$x} || $outca{$x}) {
	      # a temporary or always create variable
	      push (@create, $x);
	  } else { # an input or output variable
	      $clause2 .= "$ci$x = PDL->SvPDLV(ST($cnt));\n";
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
  foreach my $i ( 0 .. $#args ) {
      my $x = $args[$i];
      if ($other{$x}) {
	  $clause3 .= "$ci$x = " . typemap($x, $$optypes{$x}, "ST($cnt)") . ";\n";
	  $cnt++;
      } elsif ($out{$x} || $tmp{$x} || $outca{$x}) {
	  push (@create, $x);
      } else {
	  $clause3 .= "$ci$x = PDL->SvPDLV(ST($cnt));\n";
	  $cnt++;
      }
  }

  # Add code for creating output variables via call to 'initialize' perl routine
  $clause3 .= callPerlInit (\@create, $ci, $callcopy); @create = ();

  # Bitwise ops may get five args
  my $bitwise_cond = $bitwise ? " || items == 5" : '';

  PDL::PP::pp_line_numbers(__LINE__, <<END);

void
$name(...)
 PREINIT:
  char *objname = "PDL"; /* XXX maybe that class should actually depend on the value set
                            by pp_bless ? (CS) */
  HV *bless_stash = 0;
  SV *parent = 0;
  int   nreturn;
$svdecls
$pars

 PPCODE:

{
  PDL_COMMENT("Check if you can get a package name for this input value.  ")
  PDL_COMMENT("It can be either a PDL (SVt_PVMG) or a hash which is a     ")
  PDL_COMMENT("derived PDL subclass (SVt_PVHV)                            ")

  if (SvROK(ST(0)) && ((SvTYPE(SvRV(ST(0))) == SVt_PVMG) || (SvTYPE(SvRV(ST(0))) == SVt_PVHV))) {
    parent = ST(0);
    if (sv_isobject(parent)){
	bless_stash = SvSTASH(SvRV(ST(0)));
	objname = HvNAME((bless_stash));  PDL_COMMENT("The package to bless output vars into is taken from the first input var")
    }
  }
  if (items == $nmaxonstack) { PDL_COMMENT("all variables on stack, read in output and temp vars")
    nreturn = $noutca;
$clause1
  }
$clause2
  else if (items == $nin$bitwise_cond) { PDL_COMMENT("only input variables on stack, create outputs and temps")
    nreturn = $nallout;
$clause3
  }

  else {
    croak (\"Usage:  PDL::$name($usageargs) (you may leave temporaries or output variables out of list)\");
  }
}
{
$hdrcode
$inplacecode
}
END

} # sub: VarArgsXSHdr()

# This subroutine produces the code which returns output variables
# or leaves them as modified input variables.  D. Hunt 4/10/00
sub VarArgsXSReturn {
    my($xsargs, $parobjs, $globalnew ) = @_;

    # don't generate a HDR if globalnew is set
    # globalnew implies internal usage, not XS
    return undef if $globalnew;

    # names of output variables    (in calling order)
    my @outs;

    # beware of existance tests like this:  $$parobjs{$arg->[0]}{FlagOut}  !
    # this will cause $$parobjs{$arg->[0]} to spring into existance even if $$parobjs{$arg->[0]}{FlagOut}
    # does not exist!!
    foreach my $arg (@$xsargs) {
	my $x = $arg->[0];
	push (@outs, $x) if (exists ($$parobjs{$x}) and exists ($$parobjs{$x}{FlagOut}));
    }

    my $ci = '  ';  # Current indenting

    my $clause1 = '';
    foreach my $i ( 0 .. $#outs ) {
	$clause1 .= ($ci x 2) . "ST($i) = $outs[$i]_SV;\n";
    }

PDL::PP::pp_line_numbers(__LINE__, <<"END")
${ci}if (nreturn) {
${ci}  if (nreturn > 0) EXTEND (SP, nreturn );
$clause1
${ci}  XSRETURN(nreturn);
${ci}} else {
${ci}  XSRETURN(0);
${ci}}
END

} # sub: VarArgsXSReturn()


sub XSCHdrs {
	my($name,$pars,$gname) = @_;
	# Hmmm, do we need $shortpars at all?
	#my $shortpars = join ',',map {$_->[0]} @$pars;
	my $longpars = join ",",map {$_->[1]->get_decl($_->[0])} @$pars;
	return ["void $name($longpars) {","}","",
		"PDL->$gname = $name;"];
}

# abstract the access to the bad value status
# - means we can easily change the representation without too
#   many changes
#
# it's also used in one place in PP/PDLCode.pm
# -- there it's hard-coded
#
sub set_badflag   { PDL::PP::pp_line_numbers(__LINE__, '$PRIV(bvalflag) = 1;' . "\n") }
sub clear_badflag { PDL::PP::pp_line_numbers(__LINE__, '$PRIV(bvalflag) = 0;' . "\n") }
sub get_badflag   { PDL::PP::pp_line_numbers(__LINE__, '$PRIV(bvalflag)') }

sub get_badflag_priv { PDL::PP::pp_line_numbers(__LINE__, '$PRIV(bvalflag)') }

sub set_badstate {
    my $pdl = shift;
    PDL::PP::pp_line_numbers(__LINE__, "\$SETPDLSTATEBAD($pdl)")
}

sub clear_badstate {
    my $pdl = shift;
    PDL::PP::pp_line_numbers(__LINE__, "\$SETPDLSTATEGOOD($pdl)")
}

sub get_badstate {
    my $pdl = shift;
    PDL::PP::pp_line_numbers(__LINE__, "\$ISPDLSTATEBAD($pdl)")
}

# checks the input piddles to see if the routine
# is being any data containing bad values
#
# if FindBadStatusCode is set, use it,
# otherwise create the code automatically.
#
# - in the automatic code creation,
# if $badflag is 0, rather than being undefined, then
# we issue a warning if any piddles contain bad values
# (and set the bvalflag to 0)
#
# XXX it looks like output piddles are included in the
# check. I *think* this is just wasted code, but I'm
# not sure.
#
sub findbadstatus {
    my ( $badflag, $badcode, $xsargs, $parobjs, $optypes, $symtab, $name ) = @_;
    return '' unless $bvalflag;

    return PDL::PP::pp_line_numbers(__LINE__, $badcode) if defined $badcode;

    my $sname = $symtab->get_symname('_PDL_ThisTrans');

    my @args   = map { $_->[0] } @$xsargs;
    my %out    = map {
	$_ =>
	    exists($$parobjs{$_}) && exists($$parobjs{$_}{FlagOut})
		&& !exists($$parobjs{$_}{FlagCreateAlways})
	    } @args;
    my %outca = map {
	$_ =>
	    exists($$parobjs{$_}) && exists($$parobjs{$_}{FlagOut})
		&& exists($$parobjs{$_}{FlagCreateAlways})
	    } @args;
    my %tmp    = map {
	$_ =>
	    exists($$parobjs{$_}) && exists($$parobjs{$_}{FlagTemp})
	    } @args;
    my %other  = map { $_ => exists($$optypes{$_}) } @args;

    my $clear_bad = clear_badflag();
    my $set_bad   = set_badflag();
    my $get_bad   = get_badflag();

    my $str = $clear_bad;

    # set the badflag_cache variable if any input piddle has the bad flag set
    #
    my $add = 0;
    my $badflag_str = "  \$BADFLAGCACHE() = ";
    foreach my $i ( 0 .. $#args ) {
	my $x = $args[$i];
	unless ( $other{$x} or $out{$x} or $tmp{$x} or $outca{$x}) {
	    if ($add) { $badflag_str .= " || "; }
	    else      { $add = 1; }
	    $badflag_str .= get_badstate($args[$i]);
	}
    }

    # It is possible, at present, for $add to be 0. I think this is when
    # the routine has no input piddles, such as fibonacci in primitive.pd,
    # but there may be other cases. These routines could/should (?)
    # be marked as NoBadCode to avoid this, or maybe the code here made
    # smarter. Left as is for now as do not want to add instability into
    # the 2.4.3 release if I can help it - DJB 23 Jul 2006
    #
    if ($add != 0) {
	$str .= $badflag_str . ";\n  if (\$BADFLAGCACHE()) ${set_bad}\n";
    } else {
	print "\nNOTE: $name has no input bad piddles.\n\n" if $::PP_VERBOSE;
    }

    if ( defined($badflag) and $badflag == 0 ) {
	$str .=
"  if ( $get_bad ) {
      printf(\"WARNING: $name does not handle bad values.\\n\");
      $clear_bad
  }\n";
	print "\nNOTE: $name does not handle bad values.\n\n" if $::PP_VERBOSE;
    } # if: $badflag

    PDL::PP::pp_line_numbers(__LINE__, $str)

} # sub: findbadstatus


# copies over the bad value state to the output piddles
#
# if CopyBadStatusCode is set, use it,
# otherwise create the code automatically.
#
# note: this is executed before the trans_mutual call
# is made, since the state may be changed by the
# Code section
#
sub copybadstatus {
    my ( $badflag, $badcode, $xsargs, $parobjs, $symtab ) = @_;
##    return '' unless $bvalflag or $badflag == 0;
    return '' unless $bvalflag;

    if (defined $badcode) {
	# realised in 2.4.3 testing that use of $PRIV at this stage is
	# dangerous since it may have been freed. So I introduced the
	# $BFLACACHE variable which stores the $PRIV(bvalflag) value
	# for use here.
	# For now make the substitution automatic but it will likely become an
	# error to use $PRIV(bvalflag) here.
	#
	if ($badcode =~ m/\$PRIV(bvalflag)/) {
	    $badcode =~ s/\$PRIV(bvalflag)/\$BADFLAGCACHE()/;
	    print "\nPDL::PP WARNING: copybadstatus contains '\$PRIV(bvalflag)'; replace with \$BADFLAGCACHE()\n\n";
	}
	return PDL::PP::pp_line_numbers(__LINE__, $badcode);
    }

    # names of output variables    (in calling order)
    my @outs;

    # beware of existance tests like this:  $$parobjs{$arg->[0]}{FlagOut}  !
    # this will cause $$parobjs{$arg->[0]} to spring into existance even if $$parobjs{$arg->[0]}{FlagOut}
    # does not exist!!
    foreach my $arg (@$xsargs) {
	my $x = $arg->[0];
	push (@outs, $x) if (exists ($$parobjs{$x}) and exists ($$parobjs{$x}{FlagOut}));
    }

    my $sname = $symtab->get_symname('_PDL_ThisTrans');
    my $str = '';

# It appears that some code in Bad.xs sets the cache value but then
# this bit of code never gets called. Is this an efficiency issue (ie
# should we try and optimise away those ocurrences) or does it perform
# some purpose?
#
    $str = "if (\$BADFLAGCACHE()) {\n";
    foreach my $arg ( @outs ) {
	$str .= "  " . set_badstate($arg) . ";\n";
    }
    $str .= "}\n";

    PDL::PP::pp_line_numbers(__LINE__, $str);

} # sub: copybadstatus()

# insert code, after the autogenerated xs argument processing code
# produced by VarArgsXSHdr and AFTER any in HdrCode
# - this code flags the routine as working inplace,
#
# Inplace can be supplied several values
#   => 1
#     assumes fn has an inout and output piddle (eg 'a(); [o] b();')
#
#   => [ 'a' ]
#     assumes several input piddles in sig, so 'a' labels which
#     one is to be marked inplace
#
#   => [ 'a', 'b' ]
#     input piddle is a(), output pidle is 'b'
#
sub InplaceCode {
    my ( $ppname, $xsargs, $parobjs, $arg ) = @_;
    return '' unless defined $arg;

    # find input and output piddles
    my ( @in, @out );
    foreach my $arg (@$xsargs) {
	my $name = $arg->[0];
	if ( exists $$parobjs{$name} ) {
	    if ( exists $$parobjs{$name}{FlagOut} ) {
		push @out, $name;
	    } elsif ( ! exists $$parobjs{$name}{FlagTemp} ) {
		push @in, $name;
	    }
	}
    }

    # handle different values of arg
    my ( $in, $out );

    # default vals - only set if we have one input/output piddle
    $in  = $in[0]  if $#in == 0;
    $out = $out[0] if $#out == 0;

    if ( ref($arg) eq "ARRAY" ) {
	my $narg = $#$arg;
	if ( $narg > -1 ) {
	    $in = $$arg[0];
	    $out = $$arg[1] if $narg > 0;
	}
    } elsif ( ref($arg) eq "" ) {
	return '' unless $arg;
	# use default values
    } else {
	die "ERROR: Inplace rule [$ppname] must be sent either an array ref or a scalar.\n";
    }

    die "ERROR: Inplace [$ppname] does not know name of input piddle\n"
	unless defined $in;
    die "ERROR: Inplace [$ppname] does not know name of output piddle\n"
	unless defined $out;

    my $instate = $in . "->state";
    PDL::PP::pp_line_numbers(__LINE__,
	qq{\tif ( $instate & PDL_INPLACE && ($out != $in)) {
              $instate &= ~PDL_INPLACE; PDL_COMMENT("unset")
              $out = $in;             PDL_COMMENT("discard output value, leak ?")
              PDL->SetSV_PDL(${out}_SV,${out});
          }})

} # sub: InplaceCode

# If there is an EquivCPOffsCOde and:
#    no bad-value support ==> use that
#    bad value support ==> write a bit of code that does
#      if ( $PRIV(bvalflag) ) { bad-EquivCPOffsCode }
#      else                   { good-EquivCPOffsCode }
#
#  Note: since EquivCPOffsCOde doesn't (or I haven't seen any that
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
# $EQUIVCPTRUNC does the same as EQUIVCPOFFS but accepts a child-out-of-bounds
# flag.  If the out-of-bounds flag is set, the forward code puts BAD/0 into
# the child, and reverse code refrains from copying.
#                    --CED 27-Jan-2003
#
# sent [EquivCPOffsCode,BadFlag]

#
# NOTE: EQUIVCPOFFS and EQUIVCPTRUNC both suffer from the macro-block
# wart of C preprocessing.  They look like statements but sometimes
# process into blocks, so if/then/else constructs can get broken.
# Either (1) use blocks for if/then/else, or (2) get excited and
# use the "do {BLOCK} while(0)" block-to-statement conversion construct
# in the substitution.  I'm too Lazy. --CED 27-Jan-2003
#
sub CodefromEquivCPOffsCode {
    my $good  = shift;
    my $bflag = shift;

    my $bad = $good;

    # parse 'good' code
    $good =~ s/\$EQUIVCPOFFS\(([^()]+),([^()]+)\)/\$PP(CHILD)[$1] = \$PP(PARENT)[$2]/g;
    $good =~ s/\$EQUIVCPTRUNC\(([^()]+),([^()]+),([^()]+)\)/\$PP(CHILD)[$1] = ($3) ? 0 : \$PP(PARENT)[$2]/g;

    my $str = $good;

    if ( defined $bflag and $bflag ) {
	# parse 'bad' code
	$bad  =~ s/\$EQUIVCPOFFS\(([^()]+),([^()]+)\)/if( \$PPISBAD(PARENT,[$2]) ) { \$PPSETBAD(CHILD,[$1]); } else { \$PP(CHILD)[$1] = \$PP(PARENT)[$2]; }/g;
	$bad =~ s/\$EQUIVCPTRUNC\(([^()]+),([^()]+),([^()]+)\)/ if( ($3) || \$PPISBAD(PARENT,[$2]) ) { \$PPSETBAD(CHILD,[$1]); } else {\$PP(CHILD)[$1] = \$PP(PARENT)[$2]; }/g;

	$str = 'if( $PRIV(bvalflag) ) { ' . $bad . ' } else { ' . $good . '}';
    }

    PDL::PP::pp_line_numbers(__LINE__, $str);

} # sub: CodefromEquivCPOffsCode

# this just reverses PARENT & CHILD in the expansion of
# the $EQUIVCPOFFS macro (ie compared to CodefromEquivCPOffsCode)
#
sub BackCodefromEquivCPOffsCode {
    my $good = shift;
    my $bflag = shift;

    my $bad  = $good;

    # parse 'good' code
    $good =~ s/\$EQUIVCPOFFS\(([^()]+),([^()]+)\)/\$PP(PARENT)[$2] = \$PP(CHILD)[$1]/g;
    $good =~ s/\$EQUIVCPTRUNC\(([^()]+),([^()]+),([^()]+)\)/if(!($3)) \$PP(PARENT)[$2] = \$PP(CHILD)[$1] /g;

    my $str = $good;

    if ( defined $bflag and $bflag ) {
	# parse 'bad' code
	$bad  =~ s/\$EQUIVCPOFFS\(([^()]+),([^()]+)\)/if( \$PPISBAD(CHILD,[$1]) ) { \$PPSETBAD(PARENT,[$2]); } else { \$PP(PARENT)[$2] = \$PP(CHILD)[$1]; }/g;
	$bad =~ s/\$EQUIVCPTRUNC\(([^()]+),([^()]+),([^()]+)\)/if(!($3)) { if( \$PPISBAD(CHILD,[$1]) ) { \$PPSETBAD(PARENT,[$2]); } else { \$PP(PARENT)[$2] = \$PP(CHILD)[$1]; } } /g;

	$str = 'if ( $PRIV(bvalflag) ) { ' . $bad . ' } else { ' . $good . '}';
    }

    PDL::PP::pp_line_numbers(__LINE__, $str);

} # sub: BackCodefromEquivCPOffsCode

sub GenDocs {
  my ($name,$pars,$otherpars,$doc,$baddoc) = @_;

  # Allow explcit non-doc using Doc=>undef

  return '' if $doc eq '' && (!defined $doc) && $doc==undef;
  return '' if $doc =~ /^\s*internal\s*$/i;

  # remove any 'bad' documentation if we're not compiling support
  $baddoc = undef unless $bvalflag;

  # If the doc string is one line let's have to for the
  # reference card information as well
  my @splitRes; # temp split variable to get rid of
                #  'implicit split to @_ is deprecated' messages
  $doc = "=for ref\n\n".$doc if( scalar(@splitRes = split("\n", $doc)) <= 1);

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

sub ToIsReversible {
	my($rev) = @_;
	if($rev eq "1") {
		PDL::PP::pp_line_numbers(__LINE__, '$SETREVERSIBLE(1)')
	} else {
		PDL::PP::pp_line_numbers(__LINE__, $rev)
	}
}

sub make_newcoerce {
	my($ftypes) = @_;
	PDL::PP::pp_line_numbers(__LINE__, join '',map {
		"$_->datatype = $ftypes->{$_}; "
	} keys %$ftypes);
}

# Assuming that, if HASP2Child is true, we only have
# PARENT; CHILD parameters, so we can just take the
# datatype to be that of PARENT (which is set up by
# find_datatype()). Little bit complicated because
# we need to set CHILD's datatype under certain
# circumstances
#
sub coerce_types {
    my($parnames,$parobjs,$ignore,$newstab,$hasp2child) = @_;

    # assume [oca]CHILD();, although there might be an ignore
    if ( $hasp2child ) {
	my $child = $$parnames[1];
	return "" if $ignore->{$child};

	die "ERROR: expected $child to be [oca]\n"
	    unless $parobjs->{$child}{FlagCreateAlways};

	return PDL::PP::pp_line_numbers(__LINE__, "$child\->datatype = \$PRIV(__datatype);\n$child\->has_badvalue = \$PRIV(has_badvalue);\n$child\->badvalue = \$PRIV(badvalue);\n");
    }

    my $str = "";
    foreach ( @$parnames ) {
	next if $ignore->{$_};

	my $po = $parobjs->{$_};

	my $dtype;
	if ( $po->{FlagTyped} ) {
	    $dtype = $po->cenum();
	    $dtype = "PDLMAX($dtype,\$PRIV(__datatype))"
		if $po->{FlagTplus};
	} else {
	    $dtype = "\$PRIV(__datatype)";
	}

	if ( $po->{FlagCreateAlways} ) {
	    $str .= "$_->datatype = $dtype; ";
	} else {
	    $str .=
	 "if( ($_->state & PDL_NOMYDIMS) && $_->trans == NULL ) {
	     $_->datatype = $dtype;
	  } else "
	      if $po->{FlagCreat};
	    $str .= "if($dtype != $_->datatype) {
	     $_ = PDL->get_convertedpdl($_,$dtype);
	  }";
	}
    } # foreach: @$parnames

    PDL::PP::pp_line_numbers(__LINE__, $str);
} # sub: coerce_types()

# First, finds the greatest datatype, then, if not supported, takes
# the largest type supported by the function.
# Not yet optimal.
#
# Assuming that, if HASP2Child is true, we only have
# PARENT; CHILD parameters, so we can just take the
# datatype to be that of PARENT (see also coerce_types())
#
sub find_datatype {
    my($parnames,$parobjs,$ignore,$newstab,$gentypes,$hasp2child) = @_;

    my $dtype = "\$PRIV(__datatype)";

    # TODO XXX
    #  the check can probably be removed, but left in since I don't know
    #  what I'm doing (DJB)
    die "ERROR: gentypes != $ntypes with p2child\n"
	if $hasp2child and $#$gentypes != $ntypes;

    return "$dtype = $$parnames[0]\->datatype;\n\$PRIV(has_badvalue) = $$parnames[0]\->has_badvalue;\n\$PRIV(badvalue) = $$parnames[0]\->badvalue;\n"
	if $hasp2child;

    my $str = "$dtype = 0;";
    foreach ( @$parnames ) {
	my $po = $parobjs->{$_};
	next if $ignore->{$_} or $po->{FlagTyped} or $po->{FlagCreateAlways};

	$str .= "if(";
	$str .= "!(($_->state & PDL_NOMYDIMS) &&
		       $_->trans == NULL) && "
			   if $po->{FlagCreat};
	$str .= "$dtype < $_->datatype) {
		 	$dtype = $_->datatype;
		    }\n";
    } # foreach: @$parnames

    $str .= join '', map { "if($dtype == PDL_$_) {}\nelse " }(@$gentypes);

    PDL::PP::pp_line_numbers(__LINE__, $str .= "$dtype = PDL_$gentypes->[-1];\n");
} # sub: find_datatype()

sub NT2Decls_p {&NT2Decls__({ToPtrs=>1},@_);}

sub NT2Copies_p {&NT2Copies__({ToPtrs=>1},@_);}

sub NT2Free_p {&NT2Free__({ToPtrs=>1},@_);}

sub NT2Decls {&NT2Decls__({},@_);}

sub NT2Decls__ {
    my($opts,$onames,$otypes) = @_;
    my $decl;
    my $dopts = {};
    $dopts->{VarArrays2Ptrs} = 1 if $opts->{ToPtrs};
    for(@$onames) {
	$decl .= $otypes->{$_}->get_decl($_,$dopts).";";
    }
    PDL::PP::pp_line_numbers(__LINE__, $decl);
}

sub NT2Copies__ {
    my($opts,$onames,$otypes,$copyname) = @_;
    my $decl;
    my $dopts = {};
    $dopts->{VarArrays2Ptrs} = 1 if $opts->{ToPtrs};
    for(@$onames) {
	$decl .= $otypes->{$_}->get_copy("\$PRIV($_)","$copyname->$_",
					 $dopts).";";
    }
    PDL::PP::pp_line_numbers(__LINE__, $decl);
}

sub NT2Free__ {
    my($opts,$onames,$otypes) = @_;
    my $decl;
    my $dopts = {};
    $dopts->{VarArrays2Ptrs} = 1 if $opts->{ToPtrs};
    for(@$onames) {
	$decl .= $otypes->{$_}->get_free("\$PRIV($_)",
					 $dopts).";";
    }
    PDL::PP::pp_line_numbers(__LINE__, $decl);
}

# The undef is just so that PrivIsInc gets set. Is this really
# needed (well, it is since the rule fails if there aren't 2
# return values; what I meant is what does PrivIsInc do for
# us?)
#
sub make_incsizes {
	my($parnames,$parobjs,$dimobjs,$havethreading) = @_;
	my $str = ($havethreading?"pdl_thread __pdlthread; ":"").
	  (join '',map {$parobjs->{$_}->get_incdecls} @$parnames).
	    (join '',map {$_->get_decldim} sort values %$dimobjs);
	return ($str,undef);
}

sub make_incsize_copy {
	my($parnames,$parobjs,$dimobjs,$copyname,$havethreading) = @_;
	PDL::PP::pp_line_numbers(__LINE__,
		($havethreading?
	      "PDL->thread_copy(&(\$PRIV(__pdlthread)),&($copyname->__pdlthread));"
		 : "").
		 (join '',map {$parobjs->{$_}->get_incdecl_copy(sub{"\$PRIV($_[0])"},
								sub{"$copyname->$_[0]"})} @$parnames).
		 (join '',map {$_->get_copydim(sub{"\$PRIV($_[0])"},
							sub{"$copyname->$_[0]"})} sort values %$dimobjs)
	);

}

sub make_incsize_free {
	my($parnames,$parobjs,$dimobjs,$havethreading) = @_;
	$havethreading ?
	  PDL::PP::pp_line_numbers(__LINE__, 'PDL->freethreadloop(&($PRIV(__pdlthread)));')
	: ''
}

sub make_parnames {
	my($pnames,$pobjs,$dobjs) = @_;
	my @pdls = map {$pobjs->{$_}} @$pnames;
	my $npdls = $#pdls+1;
      my $join__parnames = join ",",map {qq|"$_"|} @$pnames;
      my $join__realdims = join ",",map {$#{$_->{IndObjs}}+1} @pdls;
      if($Config{cc} eq 'cl') {
         $join__parnames = '""' if $join__parnames eq '';
         $join__realdims = '0' if $join__realdims eq '';
      }
      PDL::PP::pp_line_numbers(__LINE__, "static char *__parnames[] = {". $join__parnames ."};
		static PDL_Indx __realdims[] = {". $join__realdims . "};
		static char __funcname[] = \"\$MODULE()::\$NAME()\";
		static pdl_errorinfo __einfo = {
			__funcname, __parnames, $npdls
		};
		");
}

##############################
#
# hdrcheck -- examine the various PDLs that form the output PDL,
# and copy headers as necessary.  The last header found with the hdrcpy
# bit set is used.  This used to do just a simple ref copy but now
# it uses the perl routine PDL::_hdr_copy to do the dirty work.  That
# routine makes a deep copy of the header.  Copies of the deep copy
# are distributed to all the names of the piddle that are not the source
# of the header.  I believe that is the Right Thing to do but I could be
# wrong.
#
# It's hard to read this sort of macro stuff so here's the flow:
#   - Check the hdrcpy flag.  If it's set, then check the header
#     to see if it exists.  If it doees, we need to call the
#     perl-land PDL::_hdr_copy routine.  There are some shenanigans
#     to keep the return value from evaporating before we've had a
#     chance to do our bit with it.
#   - For each output argument in the function signature, try to put
#     a reference to the new header into that argument's header slot.
#     (For functions with multiple outputs, this produces multiple linked
#     headers -- that could be Wrong; fixing it would require making
#     yet more explicit copies!)
#   - Remortalize the return value from PDL::_hdr_copy, so that we don't
#     leak memory.
#
#   --CED 12-Apr-2003
#

sub hdrcheck {
  my ($pnames,$pobjs) = @_;

  my $nn = $#$pnames;
  my @names = map { "\$PRIV(pdls[$_])" } 0..$nn;

  # from make_redodims_thread() we know that __creating[] == 0 unless
  # ...{FlagCreat} is true
  #
  my $str = "
{ PDL_COMMENT(\"convenience block\")
  void *hdrp = NULL;
  char propagate_hdrcpy = 0;
  SV *hdr_copy = NULL;
";

  # Find a header among the possible names
  foreach ( 0 .. $nn ) {
    my $aux = $pobjs->{$pnames->[$_]}{FlagCreat} ? "!__creating[$_] && \n" : "";
    $str .= <<"HdRCHECK1"
      if(!hdrp &&
	 $aux     $names[$_]\->hdrsv &&
	 ($names[$_]\->state & PDL_HDRCPY)
	 ) {
	hdrp = $names[$_]\->hdrsv;
	propagate_hdrcpy = (($names[$_]\->state & PDL_HDRCPY) != 0);
      }
HdRCHECK1
  ;
  }

  $str .= << 'DeePcOPY'
if (hdrp) {
  if(hdrp == &PL_sv_undef)
    hdr_copy = &PL_sv_undef;
  else  {  PDL_COMMENT("Call the perl routine _hdr_copy...")
    int count;
    PDL_COMMENT("Call the perl routine PDL::_hdr_copy(hdrp)")
    dSP;
    ENTER ;
    SAVETMPS ;
    PUSHMARK(SP) ;
    XPUSHs( hdrp );
    PUTBACK ;
    count = call_pv("PDL::_hdr_copy",G_SCALAR);
    SPAGAIN ;
    if(count != 1)
	croak("PDL::_hdr_copy didn't return a single value - please report this bug (A).");

    hdr_copy = (SV *)POPs;

    if(hdr_copy && hdr_copy != &PL_sv_undef) {
       (void)SvREFCNT_inc(hdr_copy); PDL_COMMENT("Keep hdr_copy from vanishing during FREETMPS")
    }

    FREETMPS ;
    LEAVE ;


  } PDL_COMMENT("end of callback  block")

DeePcOPY
    ;
# if(hdrp) block is still open -- now reassign all the aliases...

  # Found the header -- now copy it into all the right places.
  foreach ( 0 .. $nn ) {
     $str .= <<"HdRCHECK2"
       if ( $names[$_]\->hdrsv != hdrp ){
	 if( $names[$_]\->hdrsv && $names[$_]\->hdrsv != &PL_sv_undef)
             (void)SvREFCNT_dec( $names[$_]\->hdrsv );
	 if( hdr_copy != &PL_sv_undef )
             (void)SvREFCNT_inc(hdr_copy);
	 $names[$_]\->hdrsv = hdr_copy;
       }
     if(propagate_hdrcpy)
       $names[$_]\->state |= PDL_HDRCPY;
HdRCHECK2

      # QUESTION: what is the following line doing?
      #
      if ( $pobjs->{$pnames->[$_]}{FlagCreat} );
   }

  $str .= '
         if(hdr_copy != &PL_sv_undef)
            SvREFCNT_dec(hdr_copy); PDL_COMMENT("make hdr_copy mortal again")
      } PDL_COMMENT("end of if(hdrp) block")
   } PDL_COMMENT("end of conv. block")
';
  PDL::PP::pp_line_numbers(__LINE__, $str);

} # sub: hdrcheck()

sub make_redodims_thread {
    #my($pnames,$pobjs,$dobjs,$dpars,$pcode ) = @_;
    my($pnames,$pobjs,$dobjs,$dpars,$pcode, $noPthreadFlag) = @_;
    my $str = PDL::PP::pp_line_numbers(__LINE__, '');
    my $npdls = @$pnames;

    $noPthreadFlag = 0 unless( defined $noPthreadFlag ); # assume we can pthread, unless indicated otherwise

    my $nn = $#$pnames;
    my @privname = map { "\$PRIV(pdls[$_])" } ( 0 .. $nn );
    $str .= $npdls ? "PDL_Indx __creating[$npdls];\n" : "PDL_Indx __creating[1];\n";
    $str .= join '',map {$_->get_initdim."\n"} sort values %$dobjs;

    # if FlagCreat is NOT true, then we set __creating[] to 0
    # and we can use this knowledge below, and in hdrcheck()
    # and in PP/PdlParObj (get_xsnormdimchecks())
    #
    foreach ( 0 .. $nn ) {
	$str .= "__creating[$_] = ";
	if ( $pobjs->{$pnames->[$_]}{FlagCreat} ) {
	    $str .= "PDL_CR_SETDIMSCOND(__privtrans,$privname[$_]);\n";
	} else {
	    $str .= "0;\n";
	}
    } # foreach: 0 .. $nn

    $str .= " {\n$pcode\n}\n";
    $str .= " {\n " . make_parnames($pnames,$pobjs,$dobjs) . "
		 PDL->initthreadstruct(2,\$PRIV(pdls),
			__realdims,__creating,$npdls,
                      &__einfo,&(\$PRIV(__pdlthread)),
                        \$PRIV(vtable->per_pdl_flags),
			$noPthreadFlag );
		}\n";
    $str .= join '',map {$pobjs->{$_}->get_xsnormdimchecks()} @$pnames;
    $str .= hdrcheck($pnames,$pobjs);
    $str .= join '',map {$pobjs->{$pnames->[$_]}->
			     get_incsets($privname[$_])} 0..$nn;
    return $str;

} # sub: make_redodims_thread()

sub XSHdr {
	my($xsname,$nxargs) = @_;
	return XS::mkproto($xsname,$nxargs);
}

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


# Build the valid-types regex and valid Pars argument only once. These are
# also used in PDL::PP::PdlParObj, which is why they are globally available.
use PDL::PP::PdlParObj;
my $pars_re = $PDL::PP::PdlParObj::pars_re;

###########################################################
# Name       : build_pars_from_fulldoc
# Usage      : $pars = build_pars_from_fulldoc($fulldoc)
# Purpose    : extract the Pars from the signature from the fulldoc string,
#            : the part of the signature that specifies the piddles
# Returns    : a string appropriate for the Pars key
# Parameters : $fulldoc
# Throws     : if there is no signature 
#            : if there is no extractable Pars section
#            : if some PDL arguments come after the OtherPars arguments start
# Notes      : This is meant to be used directly in a Rule. Therefore, it
#            : is only called if the Pars key does not yet exist, so if it
#            : is not possible to extract the Pars section, it dies.
sub build_pars_from_fulldoc {
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
		}
		else {
			$switched_to_other_pars = 1;
		}
	}
	
	# Make sure there's something there
	confess('FullDoc signature contains no PDL arguments') if @pars == 0;
	
	# All done!
	return join('; ', @pars);
}

###########################################################
# Name       : build_otherpars_from_fulldoc
# Usage      : $otherpars = build_otherpars_from_fulldoc($fulldoc)
# Purpose    : extract the OtherPars from the signature from the fulldoc
#            : string, the part of the signature that specifies non-piddle
#            : arguments
# Returns    : a string appropriate for the OtherPars key
# Parameters : $fulldoc
# Throws     : if some OtherPars arguments come before the last PDL argument
# Notes      : This is meant to be used directly in a Rule. Therefore, it
#            : is only called if the OtherPars key does not yet exist.
sub build_otherpars_from_fulldoc {
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

# Set up the rules for translating the pp_def contents.
#
$PDL::PP::deftbl =
  [
   # used as a flag for many of the routines
   # ie should we bother with bad values for this routine?
   # 1     - yes,
   # 0     - no, maybe issue a warning
   # undef - we're not compiling with bad value support
   #
   PDL::PP::Rule->new("BadFlag", "_HandleBad",
		      "Sets BadFlag based upon HandleBad key and PDL's ability to handle bad values",
		      sub { return (defined $_[0]) ? ($bvalflag and $_[0]) : undef; }),

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
      \&build_pars_from_fulldoc
   ),
   PDL::PP::Rule->new('OtherPars', 'FullDoc',
      'Sets the OtherPars from the FullDoc if OtherPars is not explicitly specified',
      \&build_otherpars_from_fulldoc
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
         return undef unless $bvalflag;
         my ( $bf, $name, $code ) = @_;
         my $str;
         if ( not defined($bf) ) {
            $str = "$name does not process bad values.\n";
         } elsif ( $bf ) {
            $str = "$name processes bad values.\n";
         } else {
            $str = "$name ignores the bad-value flag of the input piddles.\n";
         }
         if ( not defined($code) ) {
            $str .= "It will set the bad-value flag of all output piddles if " .
            "the flag is set for any of the input piddles.\n";
         } elsif (  $code eq '' ) {
            $str .= "The output piddles will NOT have their bad-value flag set.\n";
         } else {
            $str .= "The state of the bad-value flag of the output piddles is unknown.\n";
         }
      }
   ),

   # Default: no otherpars
   PDL::PP::Rule::Returns::EmptyString->new("OtherPars"),

   # the docs
   PDL::PP::Rule->new("PdlDoc", "FullDoc", sub {
         my $fulldoc = shift;
         
         # Remove bad documentation if bad values are not supported
         $fulldoc =~ s/=for bad\n\n.*?\n\n//s unless $bvalflag;
         
         # Append a final cut if it doesn't exist due to heredoc shinanigans
         $fulldoc .= "\n\n=cut\n" unless $fulldoc =~ /\n=cut\n*$/;
         
         # Make sure the =head1 FUNCTIONS section gets added
         $::DOCUMENTED++;
         
         return $fulldoc;
      }
   ),
   PDL::PP::Rule->new("PdlDoc", ["Name","_Pars","OtherPars","Doc","_BadDoc"], \&GenDocs),
   
   ##################
   # Done with Docs #
   ##################
   
   # Notes
   # Suffix 'NS' means, "Needs Substitution". In other words, the string
   # associated with a key that has the suffix "NS" must be run through a
   # Substitute or Substitute::Usual

# some defaults
#
   PDL::PP::Rule::Returns->new("CopyName", [],
       'Sets the CopyName key to the default: __copy', "__copy"),

   PDL::PP::Rule->new("DefaultFlowCodeNS", "_DefaultFlow",
       'Sets the code to handle dataflow flags, if applicable',
		      sub { pp_line_numbers(__LINE__, $_[0] ?
			      '$PRIV(flags) |= PDL_ITRANS_DO_DATAFLOW_F | PDL_ITRANS_DO_DATAFLOW_B;'
				: 'PDL_COMMENT("No flow")') }),

# Question: where is ppdefs defined?
# Answer: Core/Types.pm
#
   PDL::PP::Rule->new("GenericTypes", [],
       'Sets GenericTypes flag to all types known to PDL::Types',
       sub {[ppdefs]}),

   PDL::PP::Rule->new("ExtraGenericLoops", "FTypes",
       'Makes ExtraGenericLoops identical to FTypes if the latter exists and the former does not',
       sub {return $_[0]}),
   PDL::PP::Rule::Returns->new("ExtraGenericLoops", [],
		'Sets ExtraGenericLoops to an empty hash if it does not already exist', {}),

   PDL::PP::Rule::InsertName->new("StructName", 'pdl_${name}_struct'),
   PDL::PP::Rule::InsertName->new("VTableName", 'pdl_${name}_vtable'),

   PDL::PP::Rule->new("FHdrInfo", ["Name","StructName"],
		      sub { return { Name => $_[0], StructName => $_[1], }; }),

# Treat exchanges as affines. Affines assumed to be parent->child.
# Exchanges may, if the want, handle threadids as well.
# Same number of dimensions is assumed, though.
#
   PDL::PP::Rule->new("AffinePriv", "XCHGOnly", sub { return @_; }),
   PDL::PP::Rule::Returns->new("Priv", "AffinePriv", 'PDL_Indx incs[$CHILD(ndims)];PDL_Indx offs; '),
   PDL::PP::Rule::Returns->new("IsAffineFlag", "AffinePriv", "PDL_ITRANS_ISAFFINE"),

   PDL::PP::Rule->new("RedoDims", ["EquivPDimExpr","FHdrInfo","_EquivDimCheck"],
		      \&pdimexpr2priv),
   PDL::PP::Rule->new("RedoDims", ["Identity","FHdrInfo"],
		      \&identity2priv),

 # NOTE: we use the same bit of code for all-good and bad data -
 #  see the Code rule
#
   PDL::PP::Rule->new("EquivCPOffsCode", "Identity",
		      "something to do with dataflow between CHILD & PARENT, I think.",
		      \&equivcpoffscode),

   PDL::PP::Rule->new("Code", ["EquivCPOffsCode","BadFlag"],
		      "create Code from EquivCPOffsCode",
		      \&CodefromEquivCPOffsCode),

   PDL::PP::Rule->new("BackCode", ["EquivCPOffsCode","BadFlag"],
		      "create BackCode from EquivCPOffsCode",
		      \&BackCodefromEquivCPOffsCode),

   PDL::PP::Rule::Returns::Zero->new("Affine_Ok", "EquivCPOffsCode"),
   PDL::PP::Rule::Returns::One->new("Affine_Ok"),

   PDL::PP::Rule::Returns::NULL->new("ReadDataFuncName", "AffinePriv"),
   PDL::PP::Rule::Returns::NULL->new("WriteBackDataFuncName", "AffinePriv"),

   PDL::PP::Rule::InsertName->new("ReadDataFuncName", 'pdl_${name}_readdata'),
   PDL::PP::Rule::InsertName->new("CopyFuncName",     'pdl_${name}_copy'),
   PDL::PP::Rule::InsertName->new("FreeFuncName",     'pdl_${name}_free'),
   PDL::PP::Rule::InsertName->new("RedoDimsFuncName", 'pdl_${name}_redodims'),

   # There used to be a BootStruct rule which just became copied to the XSBootCode
   # rule, so it has been removed.
   #
   PDL::PP::Rule->new("XSBootCode", ["AffinePriv","VTableName"],
		      sub {return "   $_[1].readdata = PDL->readdata_affine;\n" .
			     "   $_[1].writebackdata = PDL->writebackdata_affine;\n"}),

# Parameters in the form 'parent and child(this)'.
# The names are PARENT and CHILD.
#
# P2Child implicitly means "no data type changes".

   PDL::PP::Rule->new(["USParNames","USParObjs","FOOFOONoConversion","HaveThreading","NewXSName"],
		      ["P2Child","Name","BadFlag"],
		      \&NewParentChildPars),

   PDL::PP::Rule::InsertName->new("NewXSName", '_${name}_int'),

   PDL::PP::Rule::Returns->new("EquivPThreadIdExpr", "P2Child",
			       '$CTID-$PARENT(ndims)+$CHILD(ndims)'),

   PDL::PP::Rule::Returns::One->new("HaveThreading"),

# Parameters in the 'a(x,y); [o]b(y)' format, with
# fixed nos of real, unthreaded-over dims.
#
# XXX
# - the need for BadFlag is due to hacked get_xsdatapdecl()
#   in PP/PdlParObj and because the PdlParObjs are created by
#   PDL::PP::Signature (Doug Burke 07/08/00)

   PDL::PP::Rule->new(["USParNames","USParObjs","DimmedPars"], ["Pars","BadFlag"], \&Pars_nft),
   PDL::PP::Rule->new("DimObjs", ["USParNames","USParObjs"], \&ParObjs_DimObjs),

 # Set CallCopy flag for simple functions (2-arg with 0-dim signatures)
 #   This will copy the $object->copy method, instead of initialize
 #   for PDL-subclassed objects
 #
   PDL::PP::Rule->new("CallCopy", ["DimObjs", "USParNames", "USParObjs", "Name", "_P2Child"],
		      sub {
			  my ($dimObj, $USParNames, $USParObjs, $Name, $hasp2c) = @_;
			  return 0 if $hasp2c;
			  my $noDimmedArgs = scalar(keys %$dimObj);
			  my $noArgs = scalar(@$USParNames);
			  if( $noDimmedArgs == 0 and $noArgs == 2  ){
			      # Check for 2-arg functgion with 0-dim signatures
			      # Check to see if output arg is _not_ explicitly typed:
			      my $arg2 = $USParNames->[1];
			      my $ParObj = $USParObjs->{$arg2};
			      if( $ParObj->ctype('generic') eq 'generic'){
				  # print "Calling Copy for function '$Name'\n";
				  return 1;
			      }
			  }
			  return 0;
		      }),

# "Other pars", the parameters which are usually not pdls.

   PDL::PP::Rule->new(["OtherParNames","OtherParTypes"], ["OtherPars","DimObjs"], \&OtherPars_nft),

   PDL::PP::Rule->new(["ParNames","ParObjs"], ["USParNames","USParObjs"], \&sort_pnobjs),

   PDL::PP::Rule->new("DefSyms", "StructName", \&MkDefSyms),
   PDL::PP::Rule->new("NewXSArgs", ["USParNames","USParObjs","OtherParNames","OtherParTypes"],
		      \&NXArgs),

   PDL::PP::Rule::Returns->new("PMCode", undef),

   PDL::PP::Rule->new("NewXSSymTab", ["DefSyms","NewXSArgs"], \&AddArgsyms),

   PDL::PP::Rule->new("InplaceCode", ["Name","NewXSArgs","USParObjs","_Inplace"],
		      'Insert code (just after HdrCode) to ensure the routine can be done inplace',
		      \&InplaceCode),

   PDL::PP::Rule::Returns::EmptyString->new("HdrCode", [],
					    'Code that will be inserted at the end of the autogenerated xs argument processing code VargArgsXSHdr'),


 # Create header for variable argument list.  Used if no 'other pars' specified.
 # D. Hunt 4/11/00
 # make sure it is not used when the GlobalNew flag is set ; CS 4/15/00
   PDL::PP::Rule->new("VarArgsXSHdr",
		      ["Name","NewXSArgs","USParObjs","OtherParTypes",
		       "PMCode","HdrCode","InplaceCode","_GlobalNew","_CallCopy","_Bitwise"],
		      'XS code to process arguments on stack based on supplied Pars argument to pp_def; GlobalNew has implications how/if this is done',
		      \&VarArgsXSHdr),

 ## Added new line for returning (or not returning) variables.  D. Hunt 4/7/00
 # make sure it is not used when the GlobalNew flag is set ; CS 4/15/00
 #
   PDL::PP::Rule->new("VarArgsXSReturn",
		      ["NewXSArgs","USParObjs","_GlobalNew"],
		      "Generate XS trailer for returning output variables",
		      \&VarArgsXSReturn),

   PDL::PP::Rule->new("NewXSHdr", ["NewXSName","NewXSArgs"], \&XSHdr),
   PDL::PP::Rule->new("NewXSCHdrs", ["NewXSName","NewXSArgs","GlobalNew"], \&XSCHdrs),
   PDL::PP::Rule->new("NewXSLocals", "NewXSSymTab", \&Sym2Loc),

   PDL::PP::Rule::Returns::Zero->new("IsAffineFlag"),
   PDL::PP::Rule::Returns::Zero->new("NoPdlThread"),

# hmm, need to check on conditional check here (or rather, other bits of code prob need
# to include it too; see Ops.xs, PDL::assgn)
##
##           sub { return (defined $_[0]) ? "int \$BADFLAGCACHE() = 0;" : ""; } ],
##
## why have I got a "_HandleBad" condition here? it isn't used in the routine
## and isn't required to fire the rule. Or should we actually check the value of
## HandleBad (ie to optimize for code that explicitly doesn't handle bad code)?
## TO DO: Check assgn in ops for this? Not obvious, or at least we need other
## bits of code work with us (eg the checking of $BADFLAGCACHE in some other
## rule)
##
##    PDL::PP::Rule->new("CacheBadFlagInitNS", "_HandleBad",
##		      sub { return $bvalflag ? "\n  int \$BADFLAGCACHE() = 0;\n" : ""; }),
    PDL::PP::Rule->new("CacheBadFlagInitNS",
		      sub { PDL::PP::pp_line_numbers(__LINE__, $bvalflag ? "\n  int \$BADFLAGCACHE() = 0;\n" : "") }),
# The next rule, if done in place of the above, causes Ops.xs to fail to compile
#    PDL::PP::Rule->new("CacheBadFlagInitNS", "BadFlag",
#		      sub { return $_[0] ? "\n  int \$BADFLAGCACHE() = 0;\n" : ""; }),
   PDL::PP::Rule::Substitute::Usual->new("CacheBadFlagInit", "CacheBadFlagInitNS"),

    # need special cases for
    # a) bad values
    # b) bad values + GlobalNew
    # c) bad values + PMCode
    # - perhaps I should have separate rules (but b and c produce the
    #   same output...)
    #
   PDL::PP::Rule->new("NewXSStructInit0",
		      ["NewXSSymTab","VTableName","IsAffineFlag","NoPdlThread"],
		      "Rule to create and initialise the private trans structure",
		      \&MkPrivStructInit),

   PDL::PP::Rule->new("NewXSMakeNow", ["ParNames","NewXSSymTab"], \&MakeNows),
   PDL::PP::Rule->new("IgnoreTypesOf", "FTypes", sub {return {map {($_,1)} keys %{$_[0]}}}),
   PDL::PP::Rule::Returns->new("IgnoreTypesOf", {}),

   PDL::PP::Rule->new("NewXSCoerceMustNS", "FTypes", \&make_newcoerce),
   PDL::PP::Rule::Substitute::Usual->new("NewXSCoerceMust", "NewXSCoerceMustNS"),

   PDL::PP::Rule::Substitute::Usual->new("DefaultFlowCode", "DefaultFlowCodeNS"),

   PDL::PP::Rule->new("NewXSFindDatatypeNS",
		      ["ParNames","ParObjs","IgnoreTypesOf","NewXSSymTab","GenericTypes","_P2Child"],
		      \&find_datatype),
   PDL::PP::Rule::Substitute::Usual->new("NewXSFindDatatype", "NewXSFindDatatypeNS"),

   PDL::PP::Rule::Returns::EmptyString->new("NewXSTypeCoerce", "NoConversion"),

   PDL::PP::Rule->new("NewXSTypeCoerceNS",
		      ["ParNames","ParObjs","IgnoreTypesOf","NewXSSymTab","_P2Child"],
		      \&coerce_types),
   PDL::PP::Rule::Substitute::Usual->new("NewXSTypeCoerce", "NewXSTypeCoerceNS"),

   PDL::PP::Rule::Returns::EmptyString->new("NewXSStructInit1", ["ParNames","NewXSSymTab"]),

   PDL::PP::Rule->new("NewXSSetTrans", ["ParNames","ParObjs","NewXSSymTab"], \&makesettrans),

   PDL::PP::Rule->new("ParsedCode",
		      ["Code","_BadCode","ParNames","ParObjs","DimObjs","GenericTypes",
		       "ExtraGenericLoops","HaveThreading","Name"],
		      sub { return PDL::PP::Code->new(@_); }),
   PDL::PP::Rule->new("ParsedBackCode",
		      ["BackCode","_BadBackCode","ParNames","ParObjs","DimObjs","GenericTypes",
		       "ExtraGenericLoops","HaveThreading","Name"],
		      sub { return PDL::PP::Code->new(@_, undef, undef, 'BackCode2'); }),

# Compiled representations i.e. what the xsub function leaves
# in the trans structure. By default, copies of the parameters
# but in many cases (e.g. slice) a benefit can be obtained
# by parsing the string in that function.

# If the user wishes to specify his own code and compiled representation,
# The next two definitions allow this.
# Because of substitutions that will be there,
# makecompiledrepr et al are array refs, 0th element = string,
# 1th element = hashref of translated names
# This makes the objects: type + ...
#
   PDL::PP::Rule->new(["CompNames","CompObjs"], "Comp", \&OtherPars_nft),
   PDL::PP::Rule->new("CompiledRepr", ["CompNames","CompObjs"], \&NT2Decls_p),
   PDL::PP::Rule::MakeComp->new("MakeCompiledRepr", ["MakeComp","CompNames","CompObjs"],
				"COMP"),

   PDL::PP::Rule->new("CompCopyCode", ["CompNames","CompObjs","CopyName"], \&NT2Copies_p),
   PDL::PP::Rule->new("CompFreeCode", ["CompNames","CompObjs"], \&NT2Free_p),

# This is the default
#
   PDL::PP::Rule->new("MakeCompiledRepr",
		      ["OtherParNames","OtherParTypes","NewXSSymTab"],
		      \&CopyOtherPars),
   PDL::PP::Rule->new("CompiledRepr",
		      ["OtherParNames","OtherParTypes"],
		      \&NT2Decls),
   PDL::PP::Rule->new("CompCopyCode",
		      ["OtherParNames","OtherParTypes","CopyName"],
		      \&NT2Copies_p),
   PDL::PP::Rule->new("CompFreeCode", ["OtherParNames","OtherParTypes"], \&NT2Free_p),

# Threads
#
   PDL::PP::Rule->new(["Priv","PrivIsInc"],
		      ["ParNames","ParObjs","DimObjs","HaveThreading"],
		      \&make_incsizes),
   PDL::PP::Rule->new("PrivCopyCode",
		      ["ParNames","ParObjs","DimObjs","CopyName","HaveThreading"],
		      \&make_incsize_copy),
   PDL::PP::Rule->new("PrivFreeCode",
		      ["ParNames","ParObjs","DimObjs","HaveThreading"],
		      "Frees the thread",
		      \&make_incsize_free),

   PDL::PP::Rule::Returns->new("RedoDimsCode", [],
			       'Code that can be inserted to set the size of output piddles dynamically based on input piddles; is parsed',
			       'PDL_COMMENT("none")'),
   PDL::PP::Rule->new("RedoDimsParsedCode",
		      ["RedoDimsCode","_BadRedoDimsCode","ParNames","ParObjs","DimObjs",
		       "GenericTypes","ExtraGenericLoops","HaveThreading","Name"],
		      'makes the parsed representation from the supplied RedoDimsCode',
		      sub {
			  return 'PDL_COMMENT("no RedoDimsCode")'
			    if $_[0] =~ m|^/[*] none [*]/$|;
			  PDL::PP::Code->new(@_,1); }),
   PDL::PP::Rule->new("RedoDims",
		      ["ParNames","ParObjs","DimObjs","DimmedPars","RedoDimsParsedCode", '_NoPthread'],
		      'makes the redodims function from the various bits and pieces',
		      \&make_redodims_thread),

   PDL::PP::Rule::Returns::EmptyString->new("Priv"),

   PDL::PP::Rule->new(["PrivNames","PrivObjs"], "Priv", \&OtherPars_nft),
   PDL::PP::Rule->new("PrivateRepr", ["PrivNames","PrivObjs"], \&NT2Decls_p),
   PDL::PP::Rule->new("PrivCopyCode", ["PrivNames","PrivObjs","CopyName"], \&NT2Copies_p),

# avoid clash with freecode above?
#
   PDL::PP::Rule->new("NTPrivFreeCode", ["PrivNames","PrivObjs"], \&NT2Free_p),

   PDL::PP::Rule->new("IsReversibleCodeNS", "Reversible", \&ToIsReversible),
   PDL::PP::Rule::Substitute::Usual->new("IsReversibleCode", "IsReversibleCodeNS"),

   # Needs cleaning up. NewXSStructInit2DJB has been added to make use
   # of the PDL::PP::Rule::Substitute class.
   #
   PDL::PP::Rule::Substitute->new("NewXSStructInit2DJB", "MakeCompiledRepr"),
   PDL::PP::Rule->new("NewXSStructInit2", "NewXSStructInit2DJB",
		      sub { PDL::PP::pp_line_numbers(__LINE__, "{".$_[0]."}") }),

   PDL::PP::Rule->new("CopyCodeNS",
		      ["PrivCopyCode","CompCopyCode","StructName","NoPdlThread"],
		      sub {
			  PDL::PP::pp_line_numbers(__LINE__,
			    "$_[2] *__copy = malloc(sizeof($_[2])); memset(__copy, 0, sizeof($_[2]));\n" .
			($_[3] ? "" : "PDL_THR_CLRMAGIC(&__copy->__pdlthread);") .
"			PDL_TR_CLRMAGIC(__copy);
                        __copy->has_badvalue = \$PRIV(has_badvalue);
                        __copy->badvalue = \$PRIV(badvalue);
			__copy->flags = \$PRIV(flags);
			__copy->vtable = \$PRIV(vtable);
			__copy->__datatype = \$PRIV(__datatype);
			__copy->freeproc = NULL;
			__copy->__ddone = \$PRIV(__ddone);
			{int i;
			 for(i=0; i<__copy->vtable->npdls; i++)
				__copy->pdls[i] = \$PRIV(pdls[i]);
			}
			$_[1]
			if(__copy->__ddone) {
				$_[0]
			}
			return (pdl_trans*)__copy;") }),

   PDL::PP::Rule->new("FreeCodeNS",
		      ["PrivFreeCode","CompFreeCode","NTPrivFreeCode"],
		      sub {
			  PDL::PP::pp_line_numbers(__LINE__, "
			PDL_TR_CLRMAGIC(__privtrans);
			$_[1]
			if(__privtrans->__ddone) {
				$_[0]
				$_[2]
			}
			") }),

   PDL::PP::Rule::Substitute::Usual->new("CopyCode", "CopyCodeNS"),
   PDL::PP::Rule::Substitute::Usual->new("FreeCode", "FreeCodeNS"),
   PDL::PP::Rule::Substitute::Usual->new("FooCodeSub", "FooCode"),

   PDL::PP::Rule::Returns::EmptyString->new("NewXSCoerceMust"),

   PDL::PP::Rule::MakeComp->new("NewXSCoerceMustSub1", "NewXSCoerceMust", "FOO"),
   PDL::PP::Rule::Substitute->new("NewXSCoerceMustSub1d", "NewXSCoerceMustSub1"),

   PDL::PP::Rule->new("NewXSClearThread", "HaveThreading",
		      sub {$_[0] ? PDL::PP::pp_line_numbers(__LINE__, "__privtrans->__pdlthread.inds = 0;") : ""}),

   PDL::PP::Rule->new("NewXSFindBadStatusNS",
		      ["BadFlag","_FindBadStatusCode","NewXSArgs","USParObjs","OtherParTypes","NewXSSymTab","Name"],
		      "Rule to find the bad value status of the input piddles",
		      \&findbadstatus),

    # this can be removed once the default bad values are stored in a C structure
    # (rather than as a perl array in PDL::Types)
    # which it now is, hence the comments (DJB 07/10/00)
    # - left around in case we move to per-piddle bad values
    # - NOTE: now we have the experimental per-piddle bad values I need to remember
    #         what I was doing here
# [[NewXSCopyBadValues], [BadFlag,NewXSSymTab],
#    "copybadvalues",
#    "Rule to copy the default bad values into the trnas structure"],

   PDL::PP::Rule->new("NewXSCopyBadStatusNS",
		      ["BadFlag","_CopyBadStatusCode","NewXSArgs","USParObjs","NewXSSymTab"],
		      "Rule to copy the bad value status to the output piddles",
		      \&copybadstatus),

 # expand macros in ...BadStatusCode
 #
   PDL::PP::Rule::Substitute::Usual->new("NewXSFindBadStatus", "NewXSFindBadStatusNS"),
   PDL::PP::Rule::Substitute::Usual->new("NewXSCopyBadStatus", "NewXSCopyBadStatusNS"),

 # Generates XS code with variable argument list.  If this rule succeeds, the next rule
 # will not be executed. D. Hunt 4/11/00
 #
   PDL::PP::Rule->new(["NewXSCode","BootSetNewXS","NewXSInPrelude"],
		      ["_GlobalNew","_NewXSCHdrs","VarArgsXSHdr","NewXSLocals",
		       "CacheBadFlagInit",
		       "NewXSStructInit0",
		       "NewXSFindBadStatus",
		       #     NewXSCopyBadValues,
		       #     NewXSMakeNow,  # this is unnecessary since families never got implemented
		       "NewXSFindDatatype","NewXSTypeCoerce",
		       "NewXSStructInit1",
		       "NewXSStructInit2",
		       "NewXSCoerceMustSub1d","_IsReversibleCode","DefaultFlowCode",
		       "NewXSClearThread",
		       "NewXSSetTrans",
		       "NewXSCopyBadStatus",
		       "VarArgsXSReturn"
		      ],
		      "Rule to print out XS code when variable argument list XS processing is enabled",
		      \&mkVarArgsxscat),

 # This rule will fail if the preceding rule succeeds
 # D. Hunt 4/11/00
 #
   PDL::PP::Rule->new(["NewXSCode","BootSetNewXS","NewXSInPrelude"],
		      ["_GlobalNew","_NewXSCHdrs","NewXSHdr","NewXSLocals",
		       "CacheBadFlagInit",
		       "NewXSStructInit0",
		       "NewXSFindBadStatus",
		       #     NewXSCopyBadValues,
		       #     NewXSMakeNow, # this is unnecessary since families never got implemented
		       "NewXSFindDatatype","NewXSTypeCoerce",
		       "NewXSStructInit1",
		       "NewXSStructInit2",
		       "NewXSCoerceMustSub1d","_IsReversibleCode","DefaultFlowCode",
		       "NewXSClearThread",
		       "NewXSSetTrans",
		       "NewXSCopyBadStatus"
		      ],
		      "Rule to print out XS code when variable argument list XS processing is disabled",
		      \&mkxscat),

   PDL::PP::Rule->new("StructDecl",
		      ["ParNames","ParObjs","CompiledRepr","PrivateRepr","StructName"],
		      \&mkstruct),

   # The RedoDimsSub rule is a bit weird since it takes in the RedoDims target
   # twice (directly and via RedoDims-PostComp). Can this be cleaned up?
   #    [I don't know who put this in, or when -- but I don't understand it.  CED 13-April-2015]
   PDL::PP::Rule->new("RedoDims-PreComp", "RedoDims",
		      sub { PDL::PP::pp_line_numbers(__LINE__, $_[0] . ' $PRIV(__ddone) = 1;') }),
   PDL::PP::Rule::MakeComp->new("RedoDims-PostComp",
				["RedoDims-PreComp", "PrivNames", "PrivObjs"], "PRIV"),

   # RedoDimsSub is supposed to allow you to use $SIZE as an lvalue, to resize things.  It hasn't
   # worked since I can remember (at least since I started messing around with range).  The reason
   # appears to be that the SIZE macro was using the redodims argument instead of its own zeroth
   # argument.  Renaming gone wrong?  Anyway I've fixed it to use $_[0] instead of $redodims in the
   # SIZE closure.   -- CED 13-April-2015
   PDL::PP::Rule->new("RedoDimsSub",
		      ["RedoDims", "RedoDims-PostComp", "_DimObjs"],
		      sub {
			my $redodims = $_[0];
			my $result   = $_[1];
			my $dimobjs  = $_[2];

			$result->[1]{"SIZE"} = sub {
			    eval 'use PDL::IO::Dumper';
			    croak "FOO can't get SIZE of undefined dimension (RedoDims=$redodims).\nredodims is $redodims\ndimobjs is ".sdump($dimobjs)."\n"
			       unless defined $dimobjs->{$_[0]};  # This is the closure's $_[0], not the rule definition's $_[0]
			  return $dimobjs->{$_[0]}->get_size();
			};
			return $result;
		      }),
   PDL::PP::Rule::Substitute->new("RedoDimsSubd", "RedoDimsSub"),
   PDL::PP::Rule->new("RedoDimsFunc",
		      ["RedoDimsSubd","FHdrInfo","RedoDimsFuncName","_P2Child"],
		      sub {wrap_vfn(@_,"redodims")}),

   PDL::PP::Rule::MakeComp->new("ReadDataSub", "ParsedCode", "FOO"),
   PDL::PP::Rule::Substitute->new("ReadDataSubd", "ReadDataSub"),
   PDL::PP::Rule->new("ReadDataFunc",
		      ["ReadDataSubd","FHdrInfo","ReadDataFuncName","_P2Child"],
		      sub {wrap_vfn(@_,"readdata")}),

   PDL::PP::Rule::MakeComp->new("WriteBackDataSub", "ParsedBackCode", "FOO"),
   PDL::PP::Rule::Substitute->new("WriteBackDataSubd", "WriteBackDataSub"),

   PDL::PP::Rule::InsertName->new("WriteBackDataFuncName", "BackCode", 'pdl_${name}_writebackdata'),
   PDL::PP::Rule::Returns::NULL->new("WriteBackDataFuncName", "Code"),

   PDL::PP::Rule->new("WriteBackDataFunc",
		      ["WriteBackDataSubd","FHdrInfo","WriteBackDataFuncName","_P2Child"],
		      sub {wrap_vfn(@_,"writebackdata")}),,

   PDL::PP::Rule->new("CopyFunc",
		      ["CopyCode","FHdrInfo","CopyFuncName","_P2Child"],
		      sub {wrap_vfn(@_,"copy")}),
   PDL::PP::Rule->new("FreeFunc",
		      ["FreeCode","FHdrInfo","FreeFuncName","_P2Child"],
		      sub {wrap_vfn(@_,"free")}),

   PDL::PP::Rule::Returns->new("FoofName", "FooCodeSub", "foomethod"),
   PDL::PP::Rule->new("FooFunc", ["FooCodeSub","FHdrInfo","FoofName","_P2Child"],
		      sub {wrap_vfn(@_,"foo")}),

   PDL::PP::Rule::Returns::NULL->new("FoofName"),

   PDL::PP::Rule->new("VTableDef",
		      ["VTableName","StructName","RedoDimsFuncName","ReadDataFuncName",
		       "WriteBackDataFuncName","CopyFuncName","FreeFuncName",
		       "ParNames","ParObjs","Affine_Ok","FoofName"],
		      \&def_vtable),

   # Maybe accomplish this with an InsertName rule?
   PDL::PP::Rule->new('PMFunc', 'Name',
           'Sets PMFunc to default symbol table manipulations',
           sub {
               my ($name) = @_;
               $::PDL_IFBEGINWRAP[0].'*'.$name.' = \&'.$::PDLOBJ.
                         '::'.$name.";\n".$::PDL_IFBEGINWRAP[1]
           }
    ),

];

sub printtrans {
	my($bar) = @_;
	for (qw/StructDecl RedoDimsFunc ReadDataFunc WriteBackFunc
		VTableDef NewXSCode/) {
		print "\n\n================================================
	$_
=========================================\n",$bar->{$_},"\n" if $::PP_VERBOSE;
	}
}

sub translate {
    my ($pars,$tbl) = @_;

    foreach my $rule (@$tbl) {
	$rule->apply($pars);
    }

#	print Dumper($pars);
    print "GOING OUT!\n" if $::PP_VERBOSE;
    return $pars;
} # sub: translate()

## End
#
