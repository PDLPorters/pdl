
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

package PDL::Types;
require Exporter;

@EXPORT = qw( $PDL_B $PDL_S $PDL_US $PDL_L $PDL_F $PDL_D @pack %typehash );
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
	"   $key->[1] => {\n" .
	"     numval => $key->[0], sym => $key->[1], ctype => $key->[2],\n" .
	"     ppsym => $key->[3], realctype => $key->[4]\n" .
	"             },\n";
    }
(
 ["\$PDL_B",  "'PDL_B'",  "'PDL_Byte'",   "'B'", "'unsigned char'"  ],
 ["\$PDL_S",  "'PDL_S'",  "'PDL_Short'",  "'S'", "'short'"          ],
 ["\$PDL_US", "'PDL_US'", "'PDL_Ushort'", "'U'", "'unsigned short'" ],
 ["\$PDL_L",  "'PDL_L'",  "'PDL_Long'",   "'L'", "'".$itype."'"     ],
 ["\$PDL_F",  "'PDL_F'",  "'PDL_Float'",  "'F'", "'float'"          ],
 ["\$PDL_D",  "'PDL_D'",  "'PDL_Double'", "'D'", "'double'"         ]
 ); 

print OUT ");\n\n";

# create the PDL::type package

print OUT <<'!NO!SUBS!';

=head1 OBJECTS

This module contains one object - C<PDL::Type> - that is returned by the
L<type|PDL::Core/type> method of a piddle.  It has several methods, listed below,
which provide an easy way to access type infotmation:

=over 4

=item enum

Returns the number representing this datatype (see L<get_datatype|PDL::Core/PDL::get_datatype>).

=item symbol

Returns one of 'PDL_B', 'PDL_S', 'PDL_US', 'PDL_L', 'PDL_F' or 'PDL_D'. 

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

