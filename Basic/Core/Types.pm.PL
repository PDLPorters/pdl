
use strict;

use Config;
use File::Basename qw(&basename &dirname);

# Figure out the 4 byte integer type on this machine

sub typeofPDLLong {
   return 'int'  if $Config{'intsize'}==4;
   return 'long' if $Config{'longsize'}==4;
   die "Can not find an integer datatype of size 4 bytes!!!\n";
}

my $itype = &typeofPDLLong;

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

# In this section, perl variables will be expanded during extraction.
# You can use $Config{...} to use Configure variables.

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

Internal module - holds all the PDL Type info.  This can be
accessed using the C<PDL::Type> object returned by
the L<type|PDL::Core/type> method.

=cut

### Generated from Types.pm.PL automatically - do not modify! ###

=head2 Adding/Removing types

It is now relatively easy to change the types known to PDL.
Most changes required are limited to this file. You need to
update the following variables/statements in this file (F<Types.pm.PL>):

=over 5

=item C<@EXPORT>

Add the numerical constant for the new type to C<@EXPORT>, e.g. C<$PDL_LL>
for a longlong type:

   @EXPORT = qw( $PDL_B $PDL_S $PDL_US $PDL_L $PDL_LL $PDL_F $PDL_D @pack %typehash );

=item Enumeration of type constants

Add your type to the enum statement, e.g. here C<$PDL_LL> for a longlong type:

  ( $PDL_B, $PDL_S, $PDL_US, $PDL_L, $PDL_LL, $PDL_F, $PDL_D ) = (0..6);

Make sure these are arranged in order of increasing complexity for correct
type conversions!

=item corresponding pack types

This example adds C<"q*"> for a 64 bit long type:

  @PDL::Types::pack=("C*", "s*", "S*", "l*", "q*", "f*", "d*");

=item C type names

add an identifier for the new type to the corresponding perl array, e.g.

  @PDL::Types::names= qw/PDL_B PDL_S PDL_US PDL_L PDL_LL PDL_F PDL_D/;

=item typehash

add a line to the type hash initialization for your new type, e.g.

  ["\$PDL_LL", "'PDL_LL'", "'PDL_LongLong'", "'Q'", "'long long'",    0, "'longlong'"],

for our longlong type (see initialization code for meaning of the various fields).

=item typesynonyms

statement that need to be added to pdl.h . In our example, we need

  #define PDL_Q PDL_LL

in F<pdl.h>. This should really be automated.

=back

Finally, you need to check modules that do I/O (generally in the F<IO>
part of the directory tree) and check that any type macros in PP files
(i.e. C<$TBSULFD...>) are updated to reflect the new type. PDL::PP::Dump
has a mode to check for type macros requiring updating. Do something like

    find . -name \*.pd -exec perl -Mblib=. -M'PDL::PP::Dump=typecheck' {} \;

from the PDL root directory I<after> updating F<Types.pm> to check
for such places.

=cut

package PDL::Types;
require Exporter;
use Carp;

@EXPORT = qw( $PDL_B $PDL_S $PDL_US $PDL_L $PDL_F $PDL_D @pack %typehash );
@EXPORT_OK = (@EXPORT, qw/ppdefs typesrtkeys mapfld typefld/);
%EXPORT_TAGS = (
	All=>[@EXPORT,qw/ppdefs typesrtkeys mapfld typefld/],
);

@ISA    = qw( Exporter );

# Data types/sizes (bytes) [must be in order of complexity]

# Enum
( $PDL_B, $PDL_S, $PDL_US, $PDL_L, $PDL_F, $PDL_D ) = (0..5);

# Corresponding pack types
@PDL::Types::pack=("C*", "s*", "S*", "l*", "f*", "d*");
@PDL::Types::names= qw/PDL_B PDL_S PDL_US PDL_L PDL_F PDL_D/;

# should be usable in a couple of places, e.g. Dev.pm (how to locate
# during compilation?) and PDL::PP::PDLCode, also used in PDL::Dbg
#
%PDL::Types::typehash = (
!NO!SUBS!

print OUT 
    map {
	my $key = $_;
	my $convertfunc = $key->[2];
	$convertfunc =~ s/PDL_//;
	$convertfunc = lc $convertfunc;
	"   $key->[1] => {\n" .
	"     numval => $key->[0], sym => $key->[1], ctype => $key->[2],\n" .
	"     ppsym => $key->[3], realctype => $key->[4],\n" .
	"     usenan => $key->[5], ppforcetype => $key->[6],\n" .
        "     convertfunc => $convertfunc,\n".
	"             },\n";
    }
(
 ["\$PDL_B",  "'PDL_B'",  "'PDL_Byte'",   "'B'", "'unsigned char'" , 0, "'byte'"],
 ["\$PDL_S",  "'PDL_S'",  "'PDL_Short'",  "'S'", "'short'",          0, "'short'"],
 ["\$PDL_US", "'PDL_US'", "'PDL_Ushort'", "'U'", "'unsigned short'", 0, "'ushort'"],
 ["\$PDL_L",  "'PDL_L'",  "'PDL_Long'",   "'L'", "'".$itype."'",     0, "'int'"],
 ["\$PDL_F",  "'PDL_F'",  "'PDL_Float'",  "'F'", "'float'",          1, "'float'"],
 ["\$PDL_D",  "'PDL_D'",  "'PDL_Double'", "'D'", "'double'",         1, "'double'"],
 ); 

print OUT ");\n\n";

# create the PDL::type package

print OUT <<'!NO!SUBS!';

=head1 Support functions

A number of functions are available for module writers
to get/process type information. These are used in various
places (e.g. C<PDL::PP>, C<PDL::Core>) to generate the
appropriate type loops, etc.

# sorted keys into typehash
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
  croak "unknown field $fld" unless exists $typehash{$type}->{$fld};
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
  my $add = join "\n", map {"#define PDL_".typefld($_,'ppsym')." ".typefld($_,'sym')}
    grep {"PDL_".typefld($_,'ppsym') ne typefld($_,'sym')} typesrtkeys;
  print "adding...\n$add\n";
  return "$add\n";
}

=head1 PDL::Type OBJECTS

This module contains one object - C<PDL::Type> - that is returned by the
L<type|PDL::Core/type> method of a piddle.  It has several methods, listed below,
which provide an easy way to access type infotmation:

=over 4

=item enum

Returns the number representing this datatype (see L<get_datatype|PDL::Core/PDL::get_datatype>).

=item symbol

Returns one of 'PDL_B', 'PDL_S', 'PDL_US', 'PDL_L', 'PDL_LL', 'PDL_F' or 'PDL_D'. 

=item ctype

Returns the macro used to represent this type in C code (eg 'PDL_Long').

=item ppsym

The letter used to represent this type in PP code code (eg 'U' for L<ushort|PDL::Core/ushort>).

=item realctype

The actual C type used to store this type.

=item shortctype

The value returned by C<ctype> without the 'PDL_'.

=cut

{
    package PDL::Type;
    sub new {
        my($type,$val) = @_;
        if("PDL::Type" eq ref $val) { return bless [@$val],$type; }
        if(ref $val and $val->isa(PDL)) {
            if($val->getndims != 0) {
              PDL::Core::barf("Can't make a type out of non-scalar piddle $val!");
            }
            $val = $val->at;
        }
      PDL::Core::barf("Can't make a type out of non-scalar $val!".(ref $val)."!") if ref $val;
        return bless [$val],$type;
    }
 
sub enum   { return $_[0]->[0]; }
sub symbol { return $PDL::Types::names[ $_[0]->enum ]; }
!NO!SUBS!

    foreach my $name ( qw( ctype ppsym realctype ) ) {
	print OUT "sub $name { return \$PDL::Types::typehash{\$_[0]->symbol}->{$name}; }\n";
    }

print OUT <<'!NO!SUBS!';
sub shortctype { my $txt = $_[0]->ctype; $txt =~ s/PDL_//; return $txt; }                      


} # package: PDL::Type                                                          

# Return
1;

!NO!SUBS!
