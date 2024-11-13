package PDL::Types;
use strict;
use warnings;
require Exporter;
use Carp;
use Config;

our @ISA    = qw( Exporter );

my @TYPE_CHECK = qw/
  realctype ppforcetype usenan real unsigned integer identifier ctype
/;
my @TYPE_VERBATIM = (@TYPE_CHECK, qw(
  ioname convertfunc defbval shortctype ppsym numval sym floatsuffix
));
my @TYPE_MODIFIED = qw(realversion complexversion isnan isfinite);

sub packtypeof_PDL_Indx {
   if ($Config{'ivsize'} == 8) {
      return 'q*';
   }
   elsif ($Config{'ivsize'} == 4 ) {
      return 'l*';
   }
   else {
      die "Types.pm.PL: packtype for ivsize==$Config{'ivsize'} not handled\n";
   }
}

# Data types *must* be listed in order of complexity!!
# this is critical for type conversions!!!
#
my @HASHES = (
  {
    identifier => 'SB',
    onecharident => 'A',   # only needed if different from identifier
    ctype => 'PDL_SByte',# to be defined in pdl.h
    realctype => 'signed char', # CORE21 change to int8_t
    ppforcetype => 'sbyte', # for some types different from ctype
    usenan => 0,           # do we need NaN handling for this type?
    packtype => 'c*',      # the perl pack type
    defbval => 'SCHAR_MIN',
    real=>1,
    integer=>1,
    unsigned=>0,
  },
  {
    identifier => 'B',
    ctype => 'PDL_Byte',# to be defined in pdl.h
    realctype => 'unsigned char',
    ppforcetype => 'byte', # for some types different from ctype
    usenan => 0,           # do we need NaN handling for this type?
    packtype => 'C*',      # the perl pack type
    defbval => 'UCHAR_MAX',
    real=>1,
    integer=>1,
    unsigned=>1,
  },
  {
    identifier => 'S',
    ctype => 'PDL_Short',
    realctype => 'short',
    ppforcetype => 'short',
    usenan => 0,
    packtype => 's*',
    defbval => 'SHRT_MIN',
    real=>1,
    integer=>1,
    unsigned=>0,
  },
  {
    identifier => 'US',
    onecharident => 'U',   # only needed if different from identifier
    ctype => 'PDL_Ushort',
    realctype => 'unsigned short',
    ppforcetype => 'ushort',
    usenan => 0,
    packtype => 'S*',
    defbval => 'USHRT_MAX',
    real=>1,
    integer=>1,
    unsigned=>1,
  },
  {
    identifier => 'L',
    ctype => 'PDL_Long',
    realctype => 'int32_t',
    ppforcetype => 'int',
    usenan => 0,
    packtype => 'l*',
    defbval => 'INT32_MIN',
    real=>1,
    integer=>1,
    unsigned=>0,
  },
  {
    identifier => 'UL',
    onecharident => 'K',   # only needed if different from identifier
    ctype => 'PDL_ULong',
    realctype => 'uint32_t',
    ppforcetype => 'uint',
    usenan => 0,
    packtype => 'L*',
    defbval => 'UINT32_MAX',
    real=>1,
    integer=>1,
    unsigned=>1,
  },
  {
    identifier => 'IND',
    onecharident => 'N',   # only needed if different from identifier
    ctype => 'PDL_Indx',
    realctype => 'ptrdiff_t',
    ppforcetype => 'indx',
    usenan => 0,
    packtype => &packtypeof_PDL_Indx,
    defbval => 'PTRDIFF_MIN',
    real=>1,
    integer=>1,
    unsigned=>0,
  },
# note that the I/O routines have *not* been updated to be aware of
# such a type yet
  { # this one before LL so last integer is signed, to avoid default-type (last in list) changing to unsigned
    identifier => 'ULL',
    onecharident => 'P',   # only needed if different from identifier
    ctype => 'PDL_ULongLong',
    realctype => 'uint64_t',
    ppforcetype => 'ulonglong',
    usenan => 0,
    packtype => 'Q*',
    defbval => 'UINT64_MAX',
    real=>1,
    integer=>1,
    unsigned=>1,
  },
  {
    identifier => 'LL',
    onecharident => 'Q',   # only needed if different from identifier
    ctype => 'PDL_LongLong',
    realctype => 'int64_t',
    ppforcetype => 'longlong',
    usenan => 0,
    packtype => 'q*',
    defbval => 'INT64_MIN',
    real=>1,
    integer=>1,
    unsigned=>0,
  },
# IMPORTANT:
# PDL_F *must* be the first non-integer type in this list
# as there are many places in the code (.c/.xs/.pm/.pd)
# with tests like this:
#                        if (ndarraytype < PDL_F) { ... }
  {
    identifier => 'F',
    ctype => 'PDL_Float',
    realctype => 'float',
    ppforcetype => 'float',
    usenan => 1,
    packtype => 'f*',
    defbval => '-FLT_MAX',
    real=>1,
    complexversion=> 'G',
    integer=>0,
    unsigned=>0,
    isnan=>'isnan(%1$s)',
    isfinite=>'isfinite(%1$s)',
    floatsuffix=>'f',
  },
  {
    identifier => 'D',
    ctype => 'PDL_Double',
    realctype => 'double',
    ppforcetype => 'double',
    usenan => 1,
    packtype => 'd*',
    defbval => '-DBL_MAX',
    real=>1,
    complexversion=> 'C',
    integer=>0,
    unsigned=>0,
    isnan=>'isnan(%1$s)',
    isfinite=>'isfinite(%1$s)',
    floatsuffix=>'',
  },
  {
    identifier => 'LD',
    onecharident => 'E',   # only needed if different from identifier
    ctype => 'PDL_LDouble',
    realctype => 'long double',
    ppforcetype => 'ldouble',
    usenan => 1,
    packtype => 'D*',
    defbval => '-LDBL_MAX',
    real=>1,
    complexversion=> 'CLD',
    integer=>0,
    unsigned=>0,
    isnan=>'isnan(%1$s)',
    isfinite=>'isfinite(%1$s)',
    floatsuffix=>'l',
  },
# the complex types need to be in the same order as their real
# counterparts, because the "real" ppforcetype relies on a fixed interval
# between each real and complex version
# they also need to occur at the end of the types, as a < PDL_CF
# comparison is done at C level to see if a type is real, analogous to
# the < PDL_F above
  {
    identifier => 'CF',
    onecharident => 'G',   # only needed if different from identifier
    ctype => 'PDL_CFloat',
    realctype => 'complex float',
    ppforcetype => 'cfloat',
    usenan => 1,
    packtype => '(ff)*',
    defbval => '(-FLT_MAX - I*FLT_MAX)',
    real=>0,
    realversion=>'F',
    integer=>0,
    unsigned=>0,
    isnan=>'(isnan(crealf(%1$s)) || isnan(cimagf(%1$s)))',
    isfinite=>'(isfinite(crealf(%1$s)) && isfinite(cimagf(%1$s)))',
    floatsuffix=>'f',
  },
  {
    identifier => 'CD',
    onecharident => 'C',   # only needed if different from identifier
    ctype => 'PDL_CDouble',
    realctype => 'complex double',
    ppforcetype => 'cdouble',
    usenan => 1,
    packtype => '(dd)*',
    defbval => '(-DBL_MAX - I*DBL_MAX)',
    real=>0,
    realversion=>'D',
    integer=>0,
    unsigned=>0,
    isnan=>'(isnan(creal(%1$s)) || isnan(cimag(%1$s)))',
    isfinite=>'(isfinite(creal(%1$s)) && isfinite(cimag(%1$s)))',
    floatsuffix=>'',
  },
  {
    identifier => 'CLD',
    onecharident => 'H',   # only needed if different from identifier
    ctype => 'PDL_CLDouble',
    realctype => 'complex long double',
    ppforcetype => 'cldouble',
    usenan => 1,
    packtype => '(DD)*',
    defbval => '(-LDBL_MAX - I*LDBL_MAX)',
    real=>0,
    realversion=>'LD',
    integer=>0,
    unsigned=>0,
    isnan=>'(isnan(creall(%1$s)) || isnan(cimagl(%1$s)))',
    isfinite=>'(isfinite(creall(%1$s)) && isfinite(cimagl(%1$s)))',
    floatsuffix=>'l',
  },
);

my $i = 0;
my @check_keys = (@TYPE_CHECK, qw(
  identifier packtype defbval
));
for my $type (@HASHES) {
  die "type is not a HASH ref but ".ref($type) unless ref $type eq 'HASH';
  my @missing_keys = grep !exists $type->{$_}, @check_keys;
  die "type hash missing (@missing_keys)" if @missing_keys;
  $type->{shortctype} = $type->{ctype} =~ s/PDL_//r;
  $type->{ioname} = $type->{convertfunc} = lc $type->{shortctype};
  $type->{ppsym} = $type->{onecharident} || $type->{identifier};
  $type->{numval} = $i++;
  $type->{sym} = "PDL_$type->{identifier}";
  $type->{realversion} ||= $type->{ppsym};
  $type->{complexversion} ||= !$type->{real} ? $type->{ppsym} : 'G';
  $type->{floatsuffix} //= 'INVALID';

}

our @EXPORT = (qw(@pack %typehash), my @typevars = map "\$$_->{sym}", @HASHES);
our @EXPORT_OK = (@EXPORT,
  qw/types typesrtkeys mapfld typefld
    ppdefs ppdefs_complex ppdefs_all
  /
);
our %EXPORT_TAGS = (
	All=>\@EXPORT_OK,
);

eval "our ( @{[ join ',', @typevars ]} ) = (0..$#HASHES)";
die if $@;
# Corresponding pack types
our @pack= map $_->{packtype}, @HASHES;
our @names= map $_->{sym}, @HASHES;

our %typehash = map {
  my $type = $_;
  $type->{sym} => +{
    (map +($_ => $type->{$_}), @TYPE_VERBATIM, @TYPE_MODIFIED),
  };
} @HASHES;

# Cross-reference by common names
our %typenames;
for my $h (@HASHES) {
  my $n = $h->{numval};
  $typenames{$_} = $n for $n, @$h{qw(sym ioname ctype ppforcetype ppsym identifier)};
}

=head1 NAME

PDL::Types - define fundamental PDL Datatypes

=head1 SYNOPSIS

 use PDL::Types;

 $pdl = ushort( 2.0, 3.0 );
 print "The actual c type used to store ushort's is '" .
    $pdl->type->realctype() . "'\n";
 The actual c type used to store ushort's is 'unsigned short'

=head1 DESCRIPTION

Internal module - holds all the PDL Type info.  The type info can be
accessed easily using the C<PDL::Type> object returned by
the L<type|PDL::Core/type> method as shown in the synopsis.

Skip to the end of this document to find out how to change
the set of types supported by PDL.

=head1 FUNCTIONS

A number of functions are available for module writers
to get/process type information. These are used in various
places (e.g. C<PDL::PP>, C<PDL::Core>) to generate the
appropriate type loops, etc.

=head2 typesrtkeys

=for ref

Returns an array of keys of typehash sorted in order of type complexity

=for example

 pdl> @typelist = PDL::Types::typesrtkeys;
 pdl> print @typelist;
 PDL_SB PDL_B PDL_S PDL_US PDL_L PDL_UL PDL_IND PDL_ULL PDL_LL PDL_F PDL_D PDL_LD PDL_CF PDL_CD PDL_CLD
=cut

sub typesrtkeys { @names }

=head2 ppdefs

=for ref

Returns an array of pp symbols for all real types. This informs the
default C<GenericTypes> for C<pp_def> functions, making support for
complex types require an "opt-in".

=for example

 pdl> print PDL::Types::ppdefs
 A B S U L K N P Q F D E

=cut

my @PPDEFS = map $_->{ppsym}, grep $_->{real}, @HASHES;
sub ppdefs { @PPDEFS }

=head2 ppdefs_complex

=for ref

Returns an array of pp symbols for all complex types.

=for example

 pdl> print PDL::Types::ppdefs_complex
 G C H

=cut

my @PPDEFS_CPLX = map $_->{ppsym}, grep !$_->{real}, @HASHES;
sub ppdefs_complex { @PPDEFS_CPLX }

=head2 ppdefs_all

=for ref

Returns an array of pp symbols for all types including complex.

=for example

 pdl> print PDL::Types::ppdefs_all
 A B S U L K N P Q F D E G C H

=cut

my @PPDEFS_ALL = map $_->{ppsym}, @HASHES;
sub ppdefs_all { @PPDEFS_ALL }

sub typefld {
  my ($type,$fld) = @_;
  croak "unknown type $type" unless exists $typehash{$type};
  croak "unknown field $fld in type $type"
     unless exists $typehash{$type}->{$fld};
  return $typehash{$type}->{$fld};
}

sub mapfld {
	my ($type,$src,$trg) = @_;
	my @keys = grep {$typehash{$_}->{$src} eq $type} typesrtkeys;
	return @keys > 0 ? $typehash{$keys[0]}->{$trg} : undef;
}

=head2 typesynonyms

=for ref

return type related synonym definitions to be included in pdl.h .
This routine must be updated to include new types as required.
Mostly the automatic updating should take care of the vital
things.

=cut

sub typesynonyms {
  my $add = join "\n",
      map {"#define PDL_".typefld($_,'ppsym')." ".typefld($_,'sym')}
        grep {"PDL_".typefld($_,'ppsym') ne typefld($_,'sym')} typesrtkeys;
  return "$add\n";
}

=head1 PDL TYPES OVERVIEW

As of 2.065, PDL supports these types:

=over

=item SByte

Signed 8-bit value.

=item Byte

Unsigned 8-bit value.

=item Short

Signed 16-bit value.

=item UShort

Unsigned 16-bit value.

=item Long

Signed 32-bit value.

=item ULong

Unsigned 32-bit value.

=item Indx

Signed value, same size as a pointer on the system in use.

=item ULongLong

Unsigned 64-bit value.

=item LongLong

Signed 64-bit value.

=item Float

L<IEEE 754|https://en.wikipedia.org/wiki/IEEE_754> single-precision real
floating-point value.

=item Double

IEEE 754 double-precision real value.

=item LDouble

A C99 "long double", defined as "at least as precise as a double",
but often more precise.

=item CFloat

A C99 complex single-precision floating-point value.

=item CDouble

A C99 complex double-precision floating-point value.

=item CLDouble

A C99 complex "long double" - see above for description.

=back

=head1 PDL::Type OBJECTS

This module declares one class - C<PDL::Type> - objects of this class
are returned by the L<type|PDL::Core/type> method of an ndarray.  It has
several methods, listed below, which provide an easy way to access
type information:

Additionally, comparison and stringification are overloaded so that
you can compare and print type objects, e.g.

  $nofloat = 1 if $pdl->type < float;
  die "must be double" if $type != double;

For further examples check again the
L<type|PDL::Core/type> method.

=over 4

=item enum

Returns the number representing this datatype (see L<get_datatype|PDL::Core/PDL::get_datatype>).

=item symbol

Returns one of 'PDL_SB', 'PDL_B', 'PDL_S', 'PDL_US', 'PDL_L',
'PDL_UL', 'PDL_IND', 'PDL_ULL', 'PDL_LL', 'PDL_F', 'PDL_D', 'PDL_LD',
'PDL_CF', 'PDL_CD', or 'PDL_CLD'.

=item ctype

Returns the macro used to represent this type in C code (eg 'PDL_Long').

=item convertfunc

Synonym for C<ctype>.

=item ppsym

The letter used to represent this type in PP code (eg 'U' for L<ushort|PDL::Core/ushort>).

=item realctype

The actual C type used to store this type.

=item shortctype

The value returned by C<ctype> without the 'PDL_' prefix.

=item badvalue

The special numerical value used to represent bad values for this type.
See L<PDL::Bad/badvalue> for more details.

=item isnan

Given a string representing a C value, will return a C expression for
this type that indicates whether that value is NaN (for complex values,
if I<either> is NaN).

=item isfinite

Given a string representing a C value, will return a C expression for
this type that indicates whether that value is finite (for complex values,
if I<both> are finite).

=item floatsuffix

The string appended to floating-point functions for this floating-point
type. Returns C<INVALID> if called on non-floating-point type.

=item orig_badvalue

The default special numerical value used to represent bad values for this
type. (You can change the value that represents bad values for each type
during runtime.) See the
L<orig_badvalue routine in PDL::Bad|PDL::Bad/orig_badvalue> for more details.

=item bswap

Returns the appropriate C<bswap*> from L<PDL::IO::Misc> for the size of
this type, including a no-op for types of size 1. Note this means a
one-line construction means you must call the return value:

  $pdl->type->bswap->($pdl);

=item real

Returns whether the type is real-only (true) or can hold complex values
(false).

  die "Real data only!" if !$pdl->type->real;

=item unsigned

Returns whether the type can hold signed values (false) or not (true).

=item integer

Returns whether the type can hold non-integer, a.k.a. floating-point,
values (false) or not (true).

=back

=cut

my @CACHED_TYPES = map bless([$_->{numval}, $_], 'PDL::Type'), @HASHES;
# return all known types as type objects
sub types { @CACHED_TYPES }

{
package PDL::Type;
use Carp;
sub new {
  my ($type,$val) = @_;
  return $val if "PDL::Type" eq ref $val;
  if(ref $val and $val->isa('PDL')) {
    PDL::Core::barf("Can't make a type out of non-scalar ndarray $val!")
      if $val->getndims != 0;
    $val = $val->at;
  }
  confess "Can't make a type out of non-scalar $val (".
    (ref $val).")!" if ref $val;
  confess "Unknown type string '$val' (should be one of ".
    join(",",map $PDL::Types::typehash{$_}{ioname}, @names).
    ")\n"
    if !defined $PDL::Types::typenames{$val};
  $CACHED_TYPES[$PDL::Types::typenames{$val}];
}

sub enum { $_[0][0] }
*symbol = \&sym;

sub realversion {
  $CACHED_TYPES[$PDL::Types::typenames{ $_[0][1]{realversion} }];
}
sub complexversion {
  $CACHED_TYPES[$PDL::Types::typenames{ $_[0][1]{complexversion} }];
}
sub isnan { sprintf $_[0][1]{isnan}, $_[1] }
sub isfinite { sprintf $_[0][1]{isfinite}, $_[1] }

my (%bswap_cache, %howbig_cache);
sub bswap {
  PDL::Core::barf('Usage: $type->bswap with no args') if @_ > 1;
  return $bswap_cache{$_[0][0]} if $bswap_cache{$_[0][0]};
  my $size = $_[0]->howbig;
  return $bswap_cache{$_[0][0]} = sub {} if $size < 2;
  require PDL::IO::Misc;
  $bswap_cache{$_[0][0]} =
    $size == 2 ? \&PDL::bswap2 :
    $size == 4 ? \&PDL::bswap4 :
    $size == 8 ? \&PDL::bswap8 :
    $size == 16 ? \&PDL::bswap16 :
    $size == 32 ? \&PDL::bswap32 :
    PDL::Core::barf("bswap couldn't find swap function for $_[0][1]{shortctype}");
}

sub howbig {
  $howbig_cache{$_[0][0]} ||= PDL::Core::howbig($_[0][0]);
}

foreach my $name (@TYPE_VERBATIM) {
  no strict 'refs';
  *$name = sub { $_[0][1]{$name}; };
}

sub badvalue {
  PDL::Bad::_badvalue_int( $_[1], $_[0][0] );
}
sub orig_badvalue {
  PDL::Bad::_default_badvalue_int($_[0][0]);
}

# make life a bit easier
use overload (
  '""'  => sub { lc $_[0]->shortctype },
  "eq"  => sub { my ($self, $other, $swap) = @_; ("$self" eq $other); },
  "cmp" => sub { my ($self, $other, $swap) = @_;
    $swap ? $other cmp "$self" : "$self" cmp $other;
  },
  "<=>" => sub { $_[2] ? $_[1][0] <=> $_[0][0] : $_[0][0] <=> $_[1][0] },
);
} # package: PDL::Type
# Return
1;

__END__

=head1 DEVELOPER NOTES ON ADDING/REMOVING TYPES

You can change the types that PDL knows about by editing entries in
the definition of the variable C<@types> that appears close to the
top of the file F<Types.pm.PL> (i.e. the file from which this module
was generated).

=head2 Format of a type entry

Each entry in the C<@HASHES> array is a hash reference. Here is an example
taken from the actual code that defines the C<ushort> type:

  {
    identifier => 'US',
    onecharident => 'U',   # only needed if different from identifier
    ctype => 'PDL_Ushort',
    realctype => 'unsigned short',
    ppforcetype => 'ushort',
    usenan => 0,
    packtype => 'S*',
    defbval => 'USHRT_MAX',
    real=>1,
    integer=>1,
    unsigned=>1,
  },

Before we start to explain the fields please take this important
message on board:
I<entries must be listed in order of increasing complexity>. This
is critical to ensure that PDL's type conversion works correctly.
Basically, a less complex type will be converted to a more complex
type as required.

=head2 Fields in a type entry

Each type entry has a number of required and optional entry.

A list of all the entries:

=over

=item *

identifier

I<Required>. A short sequence of uppercase letters that identifies this
type uniquely. More than three characters is probably overkill.


=item *

onecharident

I<Optional>. Only required if the C<identifier> has more than one character.
This should be a unique uppercase character that will be used to reference
this type in PP macro expressions of the C<TBSULFD> type - see L<PDL::PP/$T>.

=item *

ctype

I<Required>. The C<typedef>ed name that will be used to access this type
from C code.

=item *

realctype

I<Required>. The C compiler type that is used to implement this type.
For portability reasons this one might be platform dependent.

=item *

ppforcetype

I<Required>. The type name used in PP signatures to refer to this type.

=item *

usenan

I<Required>. Flag that signals if this type has to deal with NaN issues.
Generally only required for floating point types.

=item *

packtype

I<Required>. The Perl pack type used to pack Perl values into the machine representation for this type. For details see C<perldoc -f pack>.

=item *

integer

I<Required>. Boolean - is this an integer type?

=item *

unsigned

I<Required>. Boolean - is this an unsigned type?

=item *

real

I<Required>. Boolean - is this a real (not complex) type?

=item *

realversion

String - the real version of this type (e.g. cdouble -> 'D').

=item *

complexversion

String - the complex version of this type (e.g. double -> 'C').

=back

Also have a look at the entries at the top of F<Types.pm.PL>.

The syntax is not written into stone yet and might change as the
concept matures.

=head2 Other things you need to do

You need to check modules that do I/O (generally in the F<IO>
part of the directory tree). In the future we might add fields to
type entries to automate this. This requires changes to those IO
modules first though.

You should also make sure that any type macros in PP files
(i.e. C<$TBSULFD...>) are updated to reflect the new type. PDL::PP::Dump
has a mode to check for type macros requiring updating. Do something like

    find . -name \*.pd -exec perl -Mblib=. -M'PDL::PP::Dump=typecheck' {} \;

from the PDL root directory I<after> updating F<Types.pm.PL> to check
for such places.

=cut
