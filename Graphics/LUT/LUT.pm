
=head1 NAME

PDL::Graphics::LUT - provides access to a number of look-up tables

=head1 SYNOPSIS

 use PDL::Graphics::PGPLOT;
 use PDL::Graphics::LUT;

 # what tables are available
 my @tables = lut_names();

 # get the reversed colour table 'smooth',
 # with the gamma intensity ramp
 my ( $l, $r, $g, $b ) = lut_data( 'smooth', 1, 'gamma' );

 # use the table idl5 in ctab
 ctab( lut_data('idl5') );

=head1 DESCRIPTION

PDL::Graphics::LUT contains a number of colour look-up tables
(in rgb format) and intensity ramps, and provides routines to 
access this data.
The format of the data is suitable for use by
L<PDL::Graphics::PGPLOT/ctab>.

Unlike the initial release of the package, the data tables are
now stored within the PDL distribution as FITS files
(see L<$tabledir|/$tabledir> and L<$rampdir|/$rampdir>),
rather than in the module itself.
Changes to these directories will be picked up on the next call
to one of the package functions.

=head1 FUNCTIONS

=head2 lut_names()

=for ref

Return, as a list, the names of the available colour tables.

=for usage

 @tables = lut_names();

=head2 lut_ramps()

=for ref

Return, as a list, the names of the available intensity ramps.

=for usage

 @ramps = lut_ramps();

=head2 lut_data()

=for ref

Load in the requested colour table and intensity ramp.

=for usage

 ( $l, $r, $g, $b ) = lut_data( $table, [ $reverse, [ $ramp ] ] );

Returns the levels and r, g, b components of the colour table
C<$table>. If C<$reverse> is 1 (defaults to B<0> 
if not supplied),
then the r, g, and b components are reversed before being 
returned.
If not supplied, C<$ramp> defaults to B<"ramp"> 
(this is a linear intensity ramp).

The returned values are piddles containing values in the range
0 to 1 inclusive, and are floats.

=head1 VARIABLES

=head2 $tabledir

=for ref

The directory in which the colour tables (in rgb format) 
are stored. 

=head2 $rampdir

=for ref

The directory in which the intensity ramps are stored.

=head2 $suffix

=for ref

The suffix for the data files in C<$tabledir> and
C<$rampdir>.

=head1 FURTHER INFORMATION

The colour tables were taken from the STARLINK GAIA package,
and are provided under the GNU copyleft.
See http://star-www.rl.ac.uk/ and 
http://star-www.dur.ac.uk/~pdraper/ for more details.

=head1 AUTHOR

Doug Burke (djburke@cpan.org), with thanks to 
Peter Draper/STARLINK for providing the colour-table data,
and Christian Soeller and Karl Glazebrook for their help.

All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

=cut

package PDL::Graphics::LUT;

# Just a plain function exporting package
use Exporter;

# attempt to avoid Unix-specific file/directory names
use File::Spec;
use File::Basename;

use PDL::Core qw/:Func :Internal/;    # Grab the Core names
use PDL::Basic;
use PDL::Types;
use PDL::Slices;
use PDL::IO::Misc;

# should be careful that $suffix is a valid length on non-Unix systems
$suffix = ".fits";
use vars qw( $tabledir $rampdir $suffix );

# should really use EXPORT_OK
@EXPORT    = qw( lut_names lut_ramps lut_data );
@EXPORT_OK = qw( $tabledir $rampdir $suffix );
@ISA       = qw( Exporter );

use strict;

############################################################################

# can we find the data?
BEGIN {
    my $d = File::Spec->catdir( "PDL", "Graphics", "LUT" );
    my $lutdir = undef;
    foreach my $path ( @INC ) {
	my $check = File::Spec->catdir( $path, $d );
	if ( -d $check ) { $lutdir = $check; last; }
    }
    barf "Unable to find directory ${d} within the perl libraries.\n"
	unless defined $lutdir;
    $tabledir = File::Spec->catdir( $lutdir, "tables" );
    $rampdir  = File::Spec->catdir( $lutdir, "ramps" );
    barf "Unable to find directory ${tabledir} within the perl libraries.\n"
	unless -d $tabledir;
    barf "Unable to find directory ${rampdir} within the perl libraries.\n"
	unless -d $rampdir;
}

############################################################################

# exported functions

# Return the list of available tables
#
sub lut_names () { 
    my $glob = File::Spec->catfile( $tabledir, "*${suffix}" );
    # note: really should protect any "."'s in $suffix, but too lazy
    return map { basename($_,$suffix); } glob( $glob );
}

# Return the list of available ramps
#
sub lut_ramps () { 
    my $glob = File::Spec->catfile( $rampdir, "*${suffix}" );
    # note: really should protect any "."'s in $suffix, but too lazy
    return map { basename($_,$suffix); } glob( $glob );
}

# Return the requested colour table 
#
sub lut_data ($;$$) {
    my $table   = shift;
    my $reverse = $#_ != -1 ? shift : 0;
    my $ramp    = $#_ != -1 ? shift : "ramp";

    my $lfile = File::Spec->catfile( $tabledir, "${table}${suffix}" );
    my $rfile = File::Spec->catfile( $rampdir, "${ramp}${suffix}" );
    print "Reading colour table and intensity ramp from:\n $lfile\n $rfile\n"
	if $PDL::verbose;

    # unknown table?
    unless ( -e $lfile ) {
	my @names = lut_names();
	barf <<"EOD";
Unknown colour table $table
Available tables:
 @names
EOD
    }

    # unknown ramp?
    unless ( -e $rfile ) {
	my @names = lut_ramps();
	barf <<"EOD";
Unknown intensity ramp $ramp
Available ramps:
 @names
EOD
    }

    # read in rgb data
    my $rgb = rfits $lfile;
    $rgb = float($rgb) if $rgb->get_datatype != $PDL_F;
    my ( @ldims ) = $rgb->dims;
    barf "LUT file $lfile is not the correct format (ie n by 3)\n"
	unless $#ldims == 1 and $ldims[1] == 3;

    # read in intensity data
    my $l = rfits $rfile;
    $l = float($l) if $l->get_datatype != $PDL_F;
    barf "Ramp file $rfile does not match the colour table size.\n"
	unless $l->nelem == $ldims[0];

    my $s = $reverse ? "-1:0" : "";
    return ( $l, $rgb->slice("${s},(0)"), $rgb->slice("${s},(1)"), $rgb->slice("${s},(2)") );

} # sub: lut_data()

# Exit with OK status
1;

