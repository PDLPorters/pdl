#
# this script is executed directly from the top-level Makefile.PL
# (ie before the standard "loop through the directories" behaviour
#  of the WriteMakefile() call in that file)
#

use strict;

use Config;
use File::Basename qw(&basename &dirname);

# Figure out the 4 byte integer type on this machine

sub typeofPDLLong {
   return 'int'  if $Config{'intsize'}==4;
   return 'long' if $Config{'longsize'}==4;
   die "Can not find an integer datatype of size 4 bytes!!!\n";
}

sub typeofPDLi64 {
  return $Config{i64type} or
    die "Can not find an integer 64 bit type";
}

my $bvalflag = 0;
for (@ARGV) {
  if(/^BADVALS=(.*)$/)
    {
      $bvalflag = $1;
    }
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
	     },
	     {
	      identifier => 'S',
	      pdlctype => 'PDL_Short',
	      realctype => 'short',
	      ppforcetype => 'short',
	      usenan => 0,
	      packtype => 's*',
	      defaultbadval => 'SHRT_MIN',
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
	     },
	     {
	      identifier => 'L',
	      pdlctype => 'PDL_Long',
	      realctype => &typeofPDLLong,
	      ppforcetype => 'int',
	      usenan => 0,
	      packtype => 'l*',
	      defaultbadval => 'INT_MIN',
	     },

# note that the I/O routines have *not* been updated to be aware of
# such a type yet
#
       {
	identifier => 'LL',
	onecharident => 'Q',   # only needed if different from identifier
	pdlctype => 'PDL_LongLong',
	realctype => &typeofPDLi64,
	ppforcetype => 'longlong',
	usenan => 0,
	packtype => 'q*',
	defaultbadval => 'LONG_MIN', # this is far from optimal
		                     # but LLONG_MIN/LLONG_MAX are probably
		                     # nonportable
	                             # on the other hand 2^63 should be the
                                     # value of of llong_max which we should be
                                     # able to compute at runtime ?!
      },

	      {
		  identifier => 'F',
		  pdlctype => 'PDL_Float',
		  realctype => 'float',
		  ppforcetype => 'float',
		  usenan => 1,
		  packtype => 'f*',
	          defaultbadval => '-FLT_MAX',
	      },
	      {
		  identifier => 'D',
		  pdlctype => 'PDL_Double',
		  realctype => 'double',
		  ppforcetype => 'double',
		  usenan => 1,
		  packtype => 'd*',
	          defaultbadval => '-DBL_MAX',
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
  checktypehas 'pdlctype', $type;
  my $cfunc = $type->{pdlctype};
  $cfunc =~ s/PDL_//;
  return lc $cfunc;
}

sub gentypehashentry ($$) {
  my ($type,$num) = @_;
  for my $field (qw/identifier pdlctype realctype ppforcetype usenan
		 defaultbadval/)
    {checktypehas $field, $type}
  my $newhash = {
		 ctype => $type->{pdlctype},
		 realctype => $type->{realctype},
		 ppsym => $type->{onecharident} || $type->{identifier},
		 ppforcetype => $type->{ppforcetype},
		 convertfunc => &convertfunc($type),
		 sym => &gentypenames($type),
		 numval => $num,
		 usenan => $type->{usenan},
		 ioname => &convertfunc($type), # same as the name of the
		                                # convertfunc
		 defbval => $type->{defaultbadval},
		};
  return $newhash;
}

sub gentypehashcode {
  my @types = @_;
  use Data::Dumper;
  local $Data::Dumper::Terse = 1;
  local $Data::Dumper::Indent = 1;
  local $Data::Dumper::Pad = "\t\t";
  my $i = 0;
  my $perlcode = '';
  $perlcode .= "%PDL::Types::typehash = (\n";
  for my $type (@types) {
    print STDERR "making ".gentypenames($type)."...\n";
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
require Exporter;
use Carp;

!NO!SUBS!

print OUT qq{
\@EXPORT = qw( $typeexports
	       \@pack \%typehash );
};

print OUT <<'!NO!SUBS!';

@EXPORT_OK = (@EXPORT, qw/types ppdefs typesrtkeys mapfld typefld/);
%EXPORT_TAGS = (
	All=>[@EXPORT,qw/types ppdefs typesrtkeys mapfld typefld/],
);

@ISA    = qw( Exporter );

!NO!SUBS!

print OUT qq{

# Data types/sizes (bytes) [must be in order of complexity]
# Enum
( $typevars ) = (0..$ntypesm1);
# Corresponding pack types
\@pack= qw/$packtypes/;
\@names= qw/$typenames/;

};

# generate the typehash output
print OUT gentypehashcode @types;

print OUT <<'!NO!SUBS!';

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

=head1 Support functions

A number of functions are available for module writers
to get/process type information. These are used in various
places (e.g. C<PDL::PP>, C<PDL::Core>) to generate the
appropriate type loops, etc.

=head2 typesrtkeys

return array of keys of typehash sorted in order of type complexity

=cut

sub typesrtkeys {
  return sort {$typehash{$a}->{numval} <=> $typehash{$b}->{numval}}
	keys %typehash;
}

=head2 ppdefs

return array of pp symbols for all known types

=cut

sub ppdefs {
	return map {$typehash{$_}->{ppsym}} typesrtkeys;
}

=head2 typefld

return specified field (C<$fld>) for specified type (C<$type>)
by querying type hash

=cut

sub typefld {
  my ($type,$fld) = @_;
  croak "unknown type $type" unless exists $typehash{$type};
  croak "unknown field $fld in type $type"
     unless exists $typehash{$type}->{$fld};
  return $typehash{$type}->{$fld};
}

=head2 mapfld

map a given source field to the corresponding target field by
querying the type hash

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

=head1 PDL::Type OBJECTS

This module declares one class - C<PDL::Type> - objects of this class
are returned by the L<type|PDL::Core/type> method of a piddle.  It has
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

Returns one of 'PDL_B', 'PDL_S', 'PDL_US', 'PDL_L', 'PDL_LL', 'PDL_F' 
or 'PDL_D'.

=item ctype

Returns the macro used to represent this type in C code (eg 'PDL_Long').

=item ppsym

The letter used to represent this type in PP code code (eg 'U' for L<ushort|PDL::Core/ushort>).

=item realctype

The actual C type used to store this type.

=item shortctype

The value returned by C<ctype> without the 'PDL_' prefix.

!NO!SUBS!

my $d1 = $bvalflag ?
  "Returns the value used to indicate a missing (or bad) element for the given data type. " .
  "See the L<badvalue routine in PDL::Bad|PDL::Bad/badvalue> for more details."
:
  "Since PDL was compiled without support for 'bad' values, this routine returns undef.";

my $d2 = $bvalflag ?
  "Returns the original value used to represent bad values for a given type. " .
  "See the L<orig_badvalue routine in PDL::Bad|PDL::Bad/orig_badvalue> for more details."
:
  "Since PDL was compiled without support for 'bad' values, this routine returns undef.";

print OUT qq{
=item badvalue

$d1

=item orig_badvalue

$d2

=back

=cut

};

print OUT <<'!NO!SUBS!';
{
    package PDL::Type;
    sub new {
        my($type,$val) = @_;
        if("PDL::Type" eq ref $val) { return bless [@$val],$type; }
        if(ref $val and $val->isa(PDL)) {
            if($val->getndims != 0) {
              PDL::Core::barf(
                "Can't make a type out of non-scalar piddle $val!");
            }
            $val = $val->at;
        }
      PDL::Core::barf("Can't make a type out of non-scalar $val!".
          (ref $val)."!") if ref $val;
        return bless [$val],$type;
    }

sub enum   { return $_[0]->[0]; }
sub symbol { return $PDL::Types::names[ $_[0]->enum ]; }
sub PDL::Types::types { # return all known types as type objects
  map { new PDL::Type PDL::Types::typefld($_,'numval') } 
      PDL::Types::typesrtkeys();
}

!NO!SUBS!

foreach my $name ( qw( ctype ppsym realctype ppforcetype convertfunc
		       sym numval usenan ioname defbval) ) {
  print OUT << "EOS";
sub $name {
  return \$PDL::Types::typehash{\$_[0]->symbol}->{$name};
}
EOS
}

## add the code for returning the bad value for a particular
## type. Up to (and including) 2.3.4, this code was actually in
## Basic/Bad/bad.pd.
##

if ( $bvalflag ) {
    print OUT <<'!NO!SUBS!';

no strict 'refs';
sub badvalue {
  my ( $self, $val ) = @_;
  my $name = "PDL::_badvalue_int" . $self->enum();
  if ( defined $val ) { return &{$name}( $val )->sclr; }
  else                { return &{$name}()->sclr; }
}

sub orig_badvalue {
  my $self = shift;
  my $name = "PDL::_default_badvalue_int" . $self->enum();
  return &{$name}()->sclr;
}
use strict 'refs';

!NO!SUBS!

} else {
    print OUT qq{
sub badvalue { return undef; }
sub orig_badvalue { return undef; }
};

} # if: $bvalflag

print OUT <<'!NO!SUBS!';

sub shortctype { my $txt = $_[0]->ctype; $txt =~ s/PDL_//; return $txt; }

# make life a bit easier
use overload (
	      "\"\""  => sub { lc $_[0]->shortctype },
	      "<=>"   => sub { $_[2] ? $_[1]->enum <=> $_[0]->enum :
	                               $_[0]->enum <=> $_[1]->enum },
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
	     },

Before we start to explain the fields please take this important
message on board:
I<entries must be listed in order of increasing complexity>. This
is critical to ensure that PDL"s type conversion works correctly.
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
