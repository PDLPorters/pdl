#!/usr/local/bin/perl

use Config;
use File::Basename qw(&basename &dirname);

# Figure out the 4 byte integer type on this machine

sub typeofPDLLong {
   return 'int'  if $Config{'intsize'}==4;
   return 'long' if $Config{'longsize'}==4;
   die "Can not find an integer datatype of size 4 bytes!!!\n";
}

$itype = &typeofPDLLong;

# List explicitly here the variables you want Configure to
# generate.  Metaconfig only looks for shell variables, so you
# have to mention them as if they were shell variables, not
# %Config entries.  Thus you write
#  $startperl
# to ensure Configure will look for $Config{startperl}.

# This forces PL files to create target in same directory as PL file.
# This is so that make depend always knows where to find PL derivatives.
chdir(dirname($0));
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//
	if ($Config{'osname'} eq 'VMS' or
	    $Config{'osname'} eq 'OS2');  # "case-forgiving"
open OUT,">$file" or die "Can't create $file: $!";

print "Extracting $file (with variable substitutions)\n";
chmod 0775, $file;

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


# should be usable in a couple of places, e.g. Dev.pm (how to locate
# during compilation?) and PDL::PP::PDLCode, also used in PDL::Dbg
%PDL::Types::typehash = ();
map {$key = $_; $PDL::Types::typehash{$key->[1]} = {'numval' => $key->[0],
                                                    'sym' => $key->[1],
						    'ctype' => $key->[2],
						    'ppsym' => $key->[3],
						    'realctype' => $key->[4]} }
    ([$PDL_B,'PDL_B','PDL_Byte','B','unsigned char'],
     [$PDL_S,'PDL_S','PDL_Short','S','short'],
     [$PDL_US,'PDL_US','PDL_Ushort','U','unsigned short'],
!NO!SUBS!
print OUT <<"!GROK!THIS!";
     [\$PDL_L,'PDL_L','PDL_Long','L', '$itype'],
!GROK!THIS!
print OUT <<'!NO!SUBS!';
     [$PDL_F,'PDL_F','PDL_Float','F','float'],
     [$PDL_D,'PDL_D','PDL_Double','D','double']);
1;
!NO!SUBS!

