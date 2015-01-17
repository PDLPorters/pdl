#package;

use strict;

# This file contains functions to build .pd from the HDF prototypes

# Define a low-level perl interface to HDF from these definitions.
sub create_low_level 
{
    # This file must be modified to only include 
    # netCDF 3 function definitions.
    # Also, all C function declarations must be on one line.
    my $defn = shift;
    my $sub = "create_low_level()";
    
    my @lines = split (/\n/, $defn);

    foreach my $line (@lines) 
    {

        next if ( $line =~ /^\#/ );  # Skip commented out lines
        next if ( $line =~ /^\s*$/ ); # Skip blank lines

        unless ($line =~ /^(\w+\**)\s+(\w+)\((.+)\)(\+*\d*)\;/)
        {
            die "$sub: Can't parse this line!\n";
        }
        my ($return_type, $func_name, $params, $add) = ($1, $2, $3, $4);

        my @vars;
        my @types;
        my $output = {};
        foreach my $param ( split (/,/, $params) ) 
        {
            my ($varname) = ($param =~ /(\w+)$/);
            $param =~ s/$varname//; # parm now contains the full C type
            $output->{$varname} = 1 
                if (($param =~ /\*/) && ($param !~ /const/));
            $param =~ s/const //;  # get rid of 'const' in C type
            $param =~ s/^\s+//;
            $param =~ s/\s+$//;    # pare off the variable type from 'parm'
      
            push (@vars, $varname);
            push (@types, $param);
        }

        # Create the XS header:
        my $xsout = '';
        $xsout .= "$return_type\n";
        $xsout .= "_$func_name (" . join (", ", @vars) . ")\n";
        
        # Add in the variable declarations:
        foreach my $i ( 0 .. $#vars )
        {
            $xsout .= "\t$types[$i]\t$vars[$i]\n";
        }
    
        # Add the CODE section:
        $xsout .= "CODE:\n";
        $xsout .= "\tRETVAL = ";
        $xsout .= "$add + "
            if defined($add);
        $xsout .= "$func_name (";
    
        # Add more variable stuff:
        foreach my $i ( 0 .. $#vars )
        {
            my $type = $types[$i];
            if ($type =~ /PDL/) 
            {
                $type =~ s/PDL//; # Get rid of PDL type when writing xs CODE section
                $xsout .= "($type)$vars[$i]"."->data,";
            }
            else 
            {
                $xsout .= "$vars[$i],";
            }
        }
        chop ($xsout);  # remove last comma
        $xsout .= ");\n";
        
        # Add the OUTPUT section:
        $xsout .= "OUTPUT:\n";
        $xsout .= "\tRETVAL\n";
        foreach my $var ( keys %$output ) 
        {
            $xsout .= "\t$var\n";
        }
        $xsout .= "\n\n";
        
        # Add it to the PDL::PP file:
        pp_addxs ('', $xsout);
    }
} # End of create_low_level()...

sub create_generic
{
    my $defn = shift;
    my @alltype = ('char', 'unsigned char', 'short int', 'unsigned short int',
                   'long int', 'unsigned long int', 'float', 'double');
    my @nametype = ('char', 'uchar', 'short', 'ushort',
                    'long', 'ulong', 'float', 'double');

    foreach my $i ( 0 .. $#alltype )
    {
        my $xsout = $defn;
        $xsout =~ s/GENERIC/$alltype[$i]/eg;     
        $xsout =~ s/NAME/$nametype[$i]/eg;     
        pp_addxs ('', $xsout);
    }
} # End of create_generic()...


1;
