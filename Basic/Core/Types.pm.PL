# TODO
#
# Need to be able to change bad value for a given type,
# and ensure that it is propogated through all the
# necessary structures (only if bad alue support is
# required though!)

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

# check for bad value support
use File::Spec;
require File::Spec->catfile( File::Spec->updir, File::Spec->updir, "perldl.conf" );
use vars qw( %PDL_CONFIG );
my $bvalflag = $PDL_CONFIG{WITH_BADVAL} || 0;

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

if ( $bvalflag ) {
    print "Extracting $file (WITH bad value support)\n";
} else {		     
    print "Extracting $file (NO bad value support)\n";
}
chmod 0644, $file;

# In this section, perl variables will be expanded during extraction.
# You can use $Config{...} to use Configure variables.

print OUT <<'!NO!SUBS!';

=head1 NAME

PDL::Types - define fundamental PDL Datatypes

=head1 SYNOPSIS

 use PDL::Types;

=head1 DESCRIPTION

Internal module - holds all the PDL Type info

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

!NO!SUBS!

# From Starlink
#
#include <float.h>
#include <limits.h>
# 
#define VAL__BADF    -FLT_MAX
#define VAL__BADD    -DBL_MAX
#define VAL__BADI    INT_MIN
#define VAL__BADS    SHRT_MIN
#define VAL__BADUS   USHRT_MAX
#define VAL__BADB    CHAR_MIN
#define VAL__BADUB   UCHAR_MAX                                                  

    if ( $bvalflag ) { 
	print OUT <<'!NO!SUBS!';
# Default magic values - can be changed by user
# useful to have stored in an array since it makes access
# from C code a lot easier (than if stored in the typehash
# below, for instance)
#
@PDL::Types::badvals = ( 255, 0, 0, 0, -3e34, -3e34 );
!NO!SUBS!
}
print OUT <<'!NO!SUBS!';

# should be usable in a couple of places, e.g. Dev.pm (how to locate
# during compilation?) and PDL::PP::PDLCode, also used in PDL::Dbg
#
# we do not store the bad values here as well, since any changes 
# would have to be made here as well as @PDL::Types::badvals() 
# and I don't think it's worthwhile
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

print OUT ");\n\n1;\n";


