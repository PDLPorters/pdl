# -*- perl -*- 

# creates LUT.pm file during initial 'perl Makefile.PL'
# - this way, can add/delete/change LUTs by changing the
#   contents of the tables/ directory, rather than having to
#   faff around with this file
#
# based on Basic/Core/Types.pm.PL
#
$| = 1;

use Config;
use File::Basename qw(&basename &dirname);

use IO::File;

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
my $fh = new IO::File ">$file" or die "Can't create $file: $!";
 
print "Adding LUT data to $file\n";
chmod 0775, $file;                                                              

print $fh <<'END_OF_HEAD';                                                        

### Generated from LUT.pm.PL automatically - do not modify! ###

=head1 NAME

PDL::Graphics::LUT - provides access to a number of look-up tables

=head1 SYNOPSIS

   use PDL::Graphics::PGPLOT;
   use PDL::Graphics::LUT;

   # what tables are available
   my @tables = lut_names();

   # get the reversed colour table 'smooth'
   my ( $l, $r, $g, $b ) = lut_data( 'smooth', 1 );

   # use the table idl5 in ctab
   ctab( lut_data('idl5') );

=head1 DESCRIPTION

PDL::Graphics::LUT contains a number of colour look-up tables
(in rgb format) and provides routines to access this data.
The format of the data is suitable for use by 
L<PDL::Graphics::PGPLOT/ctab()>.

A small delay occurs the first time a function from this package is
executed, as the colour tables are loaded into memory at this time.

=head1 FUNCTIONS

=head2 lut_names()

=for ref

Return, as a list, the names of the available colour tables.

=for usage

  @names = lut_names();

=head2 lut_data()

=for ref

Load in the requested colour table. 

=for usage

  ( $l, $r, $g, $b ) = lut_data( $name, [ $reverse ] );

Returns the levels and r, g, b components of the colour table
$name. If $reverse is true/1 (defaults to B<false/0> if not supplied),
then the r, g, and b components are reversed before being 
returned.

The returned values are piddles, containing values in the range
0 to 1 inclusive, and are in float format.

=head1 FURTHER INFORMATION

The colour tables were taken from the STARLINK GAIA package,
and are provided under the GNU copyleft.
See http://star-www.rl.ac.uk/ and 
http://star-www.dur.ac.uk/~pdraper/ for more details.

=head1 AUTHOR

Doug Burke (burke@ifa.hawaii.edu), with thanks to 
Christian Soeller (c.soeller@auckland.ac.nz) for the idea, and
Peter Draper/STARLINK for providing the colour-table data.

All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

=cut

package PDL::Graphics::LUT;

# Just a plain function exporting package
use Exporter;

# should really use EXPORT_OK, but as there's only two functions here...
@EXPORT = qw( lut_names lut_data );
@ISA    = qw( Exporter );

use PDL::Core qw/:Func :Internal/;    # Grab the Core names
use PDL::Basic;
use PDL::Slices;

use strict;

############################################################################

# store data here
#
my @_names;
my %_data;

sub _read_data {
    # read from __DATA__
    while (1) {
	# look for the start of a new table
	while ( <DATA> ) { last if /^LUTSTART/; }
	last unless defined;           # falls over if we reach the end of the file
	my ( $dummy, $name ) = split;
	    
	barf 'multiple copies of LUT $name in PDL::Graphics::LUT.'
	    if defined $_data{$name};
	
	# read in the table
	my ( @r, @g, @b );
	my $found_end = 0;
	while ( <DATA> ) {
	    $found_end++, last if /^LUTEND/;
	    chomp;
	    my ( $r, $g, $b ) = split;
	    push @r, $r;
	    push @g, $g;
	    push @b, $b;
	}

	# check we found a LUTEND
	barf 'missing LUTEND in __DATA__ section of PDL::Graphics::LUT.'
	    unless $found_end;

	# store the data AS floats
	push @_names, $name;
	$_data{$name}->{r} = float( @r );
	$_data{$name}->{g} = float( @g );
	$_data{$name}->{b} = float( @b );
	my $n = $#r;
	$_data{$name}->{l} = float( sequence($n+1) / $n );

    } # while: ()

} # sub: _read_data()

############################################################################

############################################################################

# exported functions

# Return the list of available tables
#
sub lut_names () { 
    _read_data() if $#_names == -1;
    return @_names; 
}

# Return the requested colour table 
#
sub lut_data ($;$) {
    my $table   = shift;
    my $reverse = $#_ != -1 ? shift : 0;

    _read_data() if $#_names == -1;

    # unknown table?
    if ( not defined( $_data{$table} ) ) {
	barf << "EOD";
Unknown colour table $table
Available tables:
 @_names
EOD
    }

    # return the LUT, reversing if necessary
    my $r = $_data{$table}->{r};
    my $g = $_data{$table}->{g};
    my $b = $_data{$table}->{b};
    my $l = $_data{$table}->{l};

    if ( $reverse ) {
	$r = $r->slice("-1:0");
	$g = $g->slice("-1:0");
	$b = $b->slice("-1:0");
    }

    return ( $l, $r, $g, $b );

} # sub: lut_data()

1;# Exit with OK status

END_OF_HEAD

# now append the table data
# Perhaps this should be in a more compressed form?
# one easy option would be to strip of trailing zeroes
# but could it be stored in a binary format, say?
# 
print $fh "__DATA__\n";

$dname  = "tables";
$suffix = ".lasc";
my $len1 = 1 + length( $dname );
my $len2 = length( $suffix );

foreach ( glob "${dname}/*${suffix}" ) { 
    my $table = substr($_,$len1,length($_)-$len1-$len2);
    print $fh "LUTSTART $table\n";
    $ifh = new IO::File $_, "r";
    while ( <$ifh> ) { 
	# remove multiple white space (includes newline)
	s/\s+/ /g;
	print $fh "$_\n"; 
    }
    $ifh->close;
    print $fh "LUTEND\n";
}
$fh->close;

## End
