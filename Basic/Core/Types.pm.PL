#
# this script is executed directly from the top-level Makefile.PL
# (ie before the standard "loop through the directories" behaviour
#  of the WriteMakefile() call in that file)
#

use strict;
use Config;
use File::Basename qw(&basename &dirname);

my @TYPE_VERBATIM = qw/
  realctype ppforcetype usenan real unsigned integer
/;

# Figure out the 4 byte integer type on this machine

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

sub typeof_PDL_Indx {
   warn "Types.pm.PL: using typedef $Config{'ivtype'} PDL_Indx\n";
   return $Config{'ivtype'} 
}

sub typeof_PDL_Long {
   return 'int'  if $Config{'intsize'}==4;
   return 'long' if $Config{'longsize'}==4;
   die "Can not find an integer datatype of size 4 bytes!!!\n";
}

sub typeof_PDL_i64 {
  return $Config{i64type} or
    die "Can not find an integer 64 bit type";
}

# Data types *must* be listed in order of complexity!!
# this is critical for type conversions!!!
#
my @types = (
	     {
	      identifier => 'B',
	      pdlctype => 'PDL_Byte',# to be defined in pdl.h
	      realctype => 'unsigned char',
	      ppforcetype => 'byte', # for some types different from ctype
	      usenan => 0,           # do we need NaN handling for this type?
	      packtype => 'C*',      # the perl pack type
	      defaultbadval => 'UCHAR_MAX',
	      real=>1,
	      integer=>1,
	      unsigned=>1,
	     },
	     {
	      identifier => 'S',
	      pdlctype => 'PDL_Short',
	      realctype => 'short',
	      ppforcetype => 'short',
	      usenan => 0,
	      packtype => 's*',
	      defaultbadval => 'SHRT_MIN',
	      real=>1,
	      integer=>1,
	      unsigned=>0,
	     },
	     {
	      identifier => 'US',
	      onecharident => 'U',   # only needed if different from identifier
	      pdlctype => 'PDL_Ushort',
	      realctype => 'unsigned short',
	      ppforcetype => 'ushort',
	      usenan => 0,
	      packtype => 'S*',
	      defaultbadval => 'USHRT_MAX',
	      real=>1,
	      integer=>1,
	      unsigned=>1,
	     },
	     {
	      identifier => 'L',
	      pdlctype => 'PDL_Long',
	      realctype => &typeof_PDL_Long,
	      ppforcetype => 'int',
	      usenan => 0,
	      packtype => 'l*',
	      defaultbadval => 'INT_MIN',
	      real=>1,
	      integer=>1,
	      unsigned=>0,
	     },

# The PDL_Indx type will be either the same as PDL_Long or, probably,
# the same as PDL_LongLong depending on the platform.  Will need to
# determine the actual type at build time.
       {
        identifier => 'IND',
        onecharident => 'N',   # only needed if different from identifier
        pdlctype => 'PDL_Indx',
        realctype => &typeof_PDL_Indx,
        ppforcetype => 'indx',
        usenan => 0,
        packtype => &packtypeof_PDL_Indx,
        defaultbadval => 'LONG_MIN',
        real=>1,
        integer=>1,
        unsigned=>0,
       },

# note that the I/O routines have *not* been updated to be aware of
# such a type yet
       {
	identifier => 'LL',
	onecharident => 'Q',   # only needed if different from identifier
	pdlctype => 'PDL_LongLong',
	realctype => &typeof_PDL_i64,
	ppforcetype => 'longlong',
	usenan => 0,
	packtype => 'q*',
	defaultbadval => 'LONG_MIN', # this is far from optimal
		                     # but LLONG_MIN/LLONG_MAX are probably
		                     # nonportable
	                             # on the other hand 2^63 should be the
                                     # value of llong_max which we should be
                                     # able to compute at runtime ?!
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
		  pdlctype => 'PDL_Float',
		  realctype => 'float',
		  ppforcetype => 'float',
		  usenan => 1,
		  packtype => 'f*',
	          defaultbadval => '-FLT_MAX',
	          real=>1,
		  complexversion=> 'G',
	          integer=>0,
	          unsigned=>0,
	          isnan=>'isnan(%1$s)',
	          floatsuffix=>'f',
	      },
	      {
		  identifier => 'D',
		  pdlctype => 'PDL_Double',
		  realctype => 'double',
		  ppforcetype => 'double',
		  usenan => 1,
		  packtype => 'd*',
	          defaultbadval => '-DBL_MAX',
	          real=>1,
		  complexversion=> 'C',
	          integer=>0,
	          unsigned=>0,
	          isnan=>'isnan(%1$s)',
	          floatsuffix=>'',
	      },
# the complex types need to be in the same order as their real
# counterparts, because the "real" ppforcetype relies on a fixed interval
# between real and complex versions
# they also need to occur at the end of the types, as a < PDL_CF
# comparison is done at C level to see if a type is real, analogous to
# the < PDL_F above
	      {
		  identifier => 'CF',
		  onecharident => 'G',   # only needed if different from identifier
		  pdlctype => 'PDL_CFloat',
		  realctype => 'complex float',
		  ppforcetype => 'cfloat',
		  usenan => 1,
		  packtype => '(ff)*',
	          defaultbadval => '(-FLT_MAX - I*FLT_MAX)',
	          real=>0,
	          realversion=>'F',
	          integer=>0,
	          unsigned=>0,
	          isnan=>'(isnan(crealf(%1$s)) || isnan(cimagf(%1$s)))',
	          floatsuffix=>'f',
	      },
	      {
		  identifier => 'CD',
		  onecharident => 'C',   # only needed if different from identifier
		  pdlctype => 'PDL_CDouble',
		  realctype => 'complex double',
		  ppforcetype => 'cdouble',
		  usenan => 1,
		  packtype => '(dd)*',
	          defaultbadval => '(-DBL_MAX - I*DBL_MAX)',
	          real=>0,
	          realversion=>'D',
	          integer=>0,
	          unsigned=>0,
	          isnan=>'(isnan(creal(%1$s)) || isnan(cimag(%1$s)))',
	          floatsuffix=>'',
	      },
	      );

sub checktypehas {
  my ($key,@types) = @_;
  for my $type (@types) {
    die "type is not a HASH ref" unless ref $type eq 'HASH';
    die "type hash doesn't have a key '$key'" unless exists $type->{$key};
  }
}

sub gentypevars {
  my @types = @_;
  checktypehas 'identifier', @types;
  my @ret = map {"\$PDL_$_->{identifier}"} @types;
  return wantarray ? @ret : $ret[0];
}

sub genexports {
  my @types = @_;
  return join ' ', gentypevars @types;
}

sub gentypenames {
  my @types = @_;
  checktypehas 'identifier', @types;
  my @ret = map {"PDL_$_->{identifier}"} @types;
  return wantarray ? @ret : $ret[0];
}

sub genpacktypes {
  my @types = @_;
  checktypehas 'packtype', @types;
  my @ret = map {"$_->{packtype}"} @types;
  return wantarray ? @ret : $ret[0];
}

sub convertfunc {
  my ($type) = @_;
  return $type->{'convertfunc'} if exists $type->{'convertfunc'};
  checktypehas 'pdlctype', $type;
  my $cfunc = $type->{pdlctype};
  $cfunc =~ s/PDL_//;
  return lc $cfunc;
}

sub gentypehashentry ($$) {
  my ($type,$num) = @_;
  checktypehas $_, $type
    for qw/identifier pdlctype defaultbadval/, @TYPE_VERBATIM;
  my $convertfunc = convertfunc($type);
  (my $shortctype = $type->{pdlctype}) =~ s/PDL_//;
  my $ppsym = $type->{onecharident} || $type->{identifier};
  +{
    ctype => $type->{pdlctype},
    ppsym => $ppsym,
    convertfunc => $convertfunc,
    sym => &gentypenames($type),
    numval => $num,
    ioname => $convertfunc,
    defbval => $type->{defaultbadval},
    shortctype => $shortctype,
    realversion => $type->{realversion} || $ppsym,
    complexversion => $type->{complexversion} || (!$type->{real} ? $ppsym : 'G'),
    (map +($_ => $type->{$_}), @TYPE_VERBATIM, qw(isnan floatsuffix)),
  };
}

sub gentypehashcode {
  my @types = @_;
  use Data::Dumper;
  local $Data::Dumper::Terse = 1;
  local $Data::Dumper::Indent = 1;
  local $Data::Dumper::Sortkeys = 1;
  local $Data::Dumper::Pad = "\t\t";
  my $i = 0;
  my $perlcode = '';
  $perlcode .= "our %typehash = (\n";
  for my $type (@types) {
    print STDOUT "making ".gentypenames($type)."...\n";
    $perlcode .= "\t".gentypenames($type)." =>\n";
    $perlcode .= Data::Dumper::Dumper(gentypehashentry($type, $i++));
    $perlcode .= "\t\t,\n";
  }
  $perlcode .= "); # end typehash definition\n";
  return $perlcode;
}

# List explicitly here the variables you want Configure to
# generate.  Metaconfig only looks for shell variables, so you
# have to mention them as if they were shell variables, not
# %Config entries.  Thus you write
#  $startperl
# to ensure Configure will look for $Config{startperl}.

# This forces PL files to create target in same directory as PL file.
# This is so that make depend always knows where to find PL derivatives.
chdir(dirname($0));
my $file;
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//
	if ($Config{'osname'} eq 'VMS' or
	    $Config{'osname'} eq 'OS2');  # "case-forgiving"
open OUT,">$file" or die "Can't create $file: $!";

print "Extracting $file\n";
chmod 0644, $file;

# in the following we generate the type dependent
# parts of Types.pm
# all the required info is extracted from the @types
# array defined above
# the guts how this is done is encapsulated in the subroutines
# that follow the definition of @types

# set up some variables that we will use below
my $typeexports = genexports @types;
my $ntypesm1  = @types - 1; # number of types - 1
my $typevars  = join ', ',gentypevars @types;
my $packtypes = join ' ', genpacktypes @types;
my $typenames = join ' ', gentypenames @types;

print OUT <<'!NO!SUBS!';

### Generated from Types.pm.PL automatically - do not modify! ###

package PDL::Types;
use strict;
use warnings;
require Exporter;
use Carp;

!NO!SUBS!

print OUT qq{
our \@EXPORT = qw( $typeexports
	       \@pack \%typehash );
};

print OUT <<'!NO!SUBS!';

our @EXPORT_OK = (@EXPORT,
  qw/types typesrtkeys mapfld typefld
    ppdefs ppdefs_complex ppdefs_all
  /
);
our %EXPORT_TAGS = (
	All=>[@EXPORT,@EXPORT_OK],
);

our @ISA    = qw( Exporter );

!NO!SUBS!

print OUT qq{

# Data types/sizes (bytes) [must be in order of complexity]
# Enum
our ( $typevars ) = (0..$ntypesm1);
# Corresponding pack types
our \@pack= qw/$packtypes/;
our \@names= qw/$typenames/;

};

# generate the typehash output
print OUT gentypehashcode @types;

print OUT <<'!NO!SUBS!';

# Cross-reference by common names
my @HASHES = sort {$a->{numval} <=> $b->{numval}} values %typehash;
my @RTKEYS = map $_->{sym}, @HASHES;
our %typenames;
for my $h (@HASHES) {
  my $n = $h->{numval};
  $typenames{$_} = $n for $n, @$h{qw(sym ioname ctype ppforcetype ppsym)};
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
the L<type|PDL::Core/type> method.

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
 PDL_B PDL_S PDL_US PDL_L PDL_IND PDL_LL PDL_F PDL_D

=cut

sub typesrtkeys { @RTKEYS }

=head2 ppdefs

=for ref

Returns an array of pp symbols for all real types. This informs the
default C<GenericTypes> for C<pp_def> functions, making support for
complex types require an "opt-in".

=for example

 pdl> print PDL::Types::ppdefs
 B S U L N Q F D

=cut

my @PPDEFS = map $_->{ppsym}, grep $_->{real}, @HASHES;
sub ppdefs { @PPDEFS }

=head2 ppdefs_complex

=for ref

Returns an array of pp symbols for all complex types.

=for example

 pdl> print PDL::Types::ppdefs_complex
 G C

=cut

my @PPDEFS_CPLX = map $_->{ppsym}, grep !$_->{real}, @HASHES;
sub ppdefs_complex { @PPDEFS_CPLX }

=head2 ppdefs_all

=for ref

Returns an array of pp symbols for all types including complex.

=for example

 pdl> print PDL::Types::ppdefs_all
 B S U L N Q F D G C

=cut

my @PPDEFS_ALL = map $_->{ppsym}, @HASHES;
sub ppdefs_all { @PPDEFS_ALL }

=head2 typefld

=for ref

Returns specified field (C<$fld>) for specified type (C<$type>)
by querying type hash

=for usage

PDL::Types::typefld($type,$fld);

=for example

 pdl> print PDL::Types::typefld('PDL_IND',realctype)
 long

=cut

sub typefld {
  my ($type,$fld) = @_;
  croak "unknown type $type" unless exists $typehash{$type};
  croak "unknown field $fld in type $type"
     unless exists $typehash{$type}->{$fld};
  return $typehash{$type}->{$fld};
}

=head2 mapfld

Map a given source field to the corresponding target field by
querying the type hash. This gives you a way to say, "Find the type
whose C<$in_key> is equal to C<$value>, and return that type's value
for C<$out_key>. For example:

 # Does byte type use nan?
 $uses_nan = PDL::Types::mapfld(byte => 'ppforcetype', 'usenan');
 # Equivalent:
 $uses_nan = byte->usenan;
 
 # What is the actual C type for the value that we call 'long'?
 $type_name = PDL::Types::mapfld(long => 'convertfunc', 'realctype');
 # Equivalent:
 $type_name = long->realctype;

As you can see, the equivalent examples are much shorter and legible, so you
should only use mapfld if you were given the type index (in which case the
actual type is not immediately obvious):

 $type_index = 4;
 $type_name = PDL::Types::mapfld($type_index => numval, 'realctype');

=cut

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
  print "adding...\n$add\n";
  return "$add\n";
}

=head2 datatypes_header

=for ref

return C header text for F<pdl.h> and F<pdlsimple.h>.

=cut

sub datatypes_header {
    require Config;
    warn "Using new 64bit index support\n" if $Config::Config{'ivsize'}==8;

    my $enum = join ', ', 'PDL_INVALID=-1', map $_->{sym}, @HASHES;
    my $typedefs = join '', map "typedef $_->{realctype} $_->{ctype};\n", @HASHES;
    $typedefs .= "typedef struct {\n  pdl_datatypes type;\n  union {\n";
    $typedefs .= join '', map "  $_->{ctype} $_->{ppsym};\n", @HASHES;
    $typedefs .= "  } value;\n} PDL_Anyval;\n";

    my $indx_type = typefld('PDL_IND','realctype');
    $typedefs .= '#define IND_FLAG ';
    if ($indx_type eq 'long'){
	$typedefs .= qq|"ld"|;
    } elsif ($indx_type eq 'long long'){
	$typedefs .= qq|"lld"|;
    } else {
	$typedefs .= qq|"d"|;
    }
    $typedefs .= "\n";

    <<EOD . "\n".typesynonyms()."\n";

/*****************************************************************************/
/*** This section of .h file generated automatically by ***/
/*** PDL::Types::datatypes_header() - don't edit manually ***/

/* Data types/sizes [must be in order of complexity] */

typedef enum { $enum } pdl_datatypes;

/* Define the pdl data types */

$typedefs

/*****************************************************************************/

EOD
}

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

Returns one of 'PDL_B', 'PDL_S', 'PDL_US', 'PDL_L', 'PDL_IND', 'PDL_LL',
'PDL_F' or 'PDL_D'.

=item ctype

Returns the macro used to represent this type in C code (eg 'PDL_Long').

=item ppsym

The letter used to represent this type in PP code (eg 'U' for L<ushort|PDL::Core/ushort>).

=item realctype

The actual C type used to store this type.

=item shortctype

The value returned by C<ctype> without the 'PDL_' prefix.

=item badvalue

The special numerical value used to represent bad values for this type.
See L<badvalue routine in PDL::Bad|PDL::Bad/badvalue> for more details.

=item isnan

Given a string representing a C value, will return a C expression for
this type that indicates whether that value is NaN.

=item floatsuffix

The string appended to floating-point functions for this floating-point
type. Dies if called on non-floating-point type.

=cut

=item orig_badvalue

The default special numerical value used to represent bad values for this
type. (You can change the value that represents bad values for each type
during runtime.) See the
L<orig_badvalue routine in PDL::Bad|PDL::Bad/orig_badvalue> for more details.

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
            join(",",map $PDL::Types::typehash{$_}->{ioname}, @names).
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

    sub floatsuffix { $_[0][1]{floatsuffix} // 'floatsuffix called on non-float type' }

!NO!SUBS!

foreach my $name ( qw( ctype ppsym convertfunc shortctype
		       sym numval ioname defbval
                       ), @TYPE_VERBATIM ) {
  print OUT << "EOS";
    sub $name { \$_[0][1]{$name}; }
EOS
}

print OUT <<'!NO!SUBS!';
sub badvalue {
  my $name = "PDL::_badvalue_int$_[0][0]";
  no strict 'refs';
  $name->( $_[1] );
}
sub orig_badvalue {
  my $name = "PDL::_default_badvalue_int$_[0][0]";
  no strict 'refs';
  $name->();
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

=head1 Adding/removing types

You can change the types that PDL knows about by editing entries in
the definition of the variable C<@types> that appears close to the
top of the file F<Types.pm.PL> (i.e. the file from which this module
was generated).

=head2 Format of a type entry

Each entry in the C<@types> array is a hash reference. Here is an example
taken from the actual code that defines the C<ushort> type:

	     {
	      identifier => 'US',
	      onecharident => 'U',   # only needed if different from identifier
	      pdlctype => 'PDL_Ushort',
	      realctype => 'unsigned short',
	      ppforcetype => 'ushort',
	      usenan => 0,
	      packtype => 'S*',
	      defaultbadval => 'USHRT_MAX',
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

I<Required>. A short sequence of upercase letters that identifies this
type uniquely. More than three characters is probably overkill.


=item *

onecharident

I<Optional>. Only required if the C<identifier> has more than one character.
This should be a unique uppercase character that will be used to reference
this type in PP macro expressions of the C<TBSULFD> type. If you don't
know what I am talking about read the PP manpage or ask on the mailing list.

=item *

pdlctype

I<Required>. The C<typedefed> name that will be used to access this type
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

!NO!SUBS!
