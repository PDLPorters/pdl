

# Stuff used in development/install environment of PDL Makefile.PL's
# - not part of PDL itself.

package PDL::Core::Dev;

use English; use Exporter; use DynaLoader; 
@ISA    = qw( Exporter DynaLoader ); 

@EXPORT = qw(genpp %PDL_DATATYPES PDL_INCLUDE PDL_TYPEMAP);

# Installation locations

sub PDL_INCLUDE { '-I'.whereami().'/PDL/Core' };
sub PDL_TYPEMAP { whereami().'/PDL/Core/typemap.pdl' };

# Data types to C types mapping

%PDL_DATATYPES = ( 'PDL_B'  => 'unsigned char',  'PDL_S' => 'short',  
                   'PDL_US' => 'unsigned short', 'PDL_L' => 'PDL_Long', 
                   'PDL_F'  => 'float', 'PDL_D' => 'double'); 

# non-blocking IO configuration

$O_NONBLOCK = defined $Config{'o_nonblock'} ? $Config{'o_nonblock'}
                : 'O_NONBLOCK';


#################### PDL Generic PreProcessor ####################
#
# Preprocesses *.g files to *.c files allowing 'generic'
# type code which is converted to code for each type.
#
# e.g. the code:
#
#    pdl x;
#    GENERICLOOP(x.datatype)
#       generic *xx = x.data;
#       for(i=0; i<nvals; i++) 
#          xx[i] = i/nvals;
#    ENDGENERICLOOP
#
# is converted into a giant switch statement:
#
#     pdl x;
#     switch (x.datatype) {
# 
#     case PDL_L:
#        {   
#           long *xx = x.data;
#           for(i=0; i<nvals; i++) 
#              xx[i] = i/nvals;
#        }break;
# 
#     case PDL_F:
#        {   
#           float *xx = x.data;
#
#       .... etc. .....
#
# 'generic' is globally substituted for each relevant data type.
#
# This is used in PDL to write generic functions (taking pdl or void
# objects) which is still efficient with perl writing the actual C
# code for each type.
# 
#     1st version - Karl Glazebrook 4/Aug/1996.
#
# Also makes the followings substitutions:
#
# (i) O_NONBLOCK - open flag for non-blocking I/O (5/Aug/96)
#


sub genpp {

   $gotstart = 0; @gencode = ();
   
   while (<>) { # Process files in @ARGV list - result to STDOUT
   
      # Do the miscellaneous substitutions first
   
      s/O_NONBLOCK/$O_NONBLOCK/go;   # I/O
   
   
      if ( /(\s*)?\bGENERICLOOP\s*\(([^\)]*)\)(\s*;)?/ ){  # Start of generic code
         #print $MATCH, "=$1=\n";
      
         die "Found GENERICLOOP while searching for ENDGENERICLOOP\n" if $gotstart;
         $loopvar = $2;
         $indent = $1;
         print $PREMATCH;
         
         @gencode = ();  # Start saving code
         push @gencode, $POSTMATCH;
         $gotstart = 1;
         next;
      }
      
      if ( /\bENDGENERICLOOP(\s*;)?/ ) {
      
         die "Found ENDGENERICLOOP while searching for GENERICLOOP\n" unless $gotstart;
      
         push @gencode, $PREMATCH;
      
         flushgeneric();  # Output the generic code
      
         print $POSTMATCH;  # End of genric code
         $gotstart = 0;
         next;
      }
   
      if ($gotstart) {
         push @gencode, $_;
      }
      else {
         print;
      }
   
   } # End while
   
}

sub flushgeneric {  # Construct the generic code switch
   
   print $indent,"switch ($loopvar) {\n\n";

   for $case (keys %PDL_DATATYPES) {

     $type = $PDL_DATATYPES{$case}; 

     print $indent,"case $case:\n"; # Start of this case
     print $indent,"   {";

     # Now output actual code with substutions

     for  (@gencode) { 
        $line = $_;

        $line =~ s/\bgeneric\b/$type/g;

        print "   ",$line;
     }

     print "}break;\n\n";  # End of this case
   }
   print $indent,"default:\n";
   print $indent,'   croak ("Not a known data type code=%d",'.$loopvar.");\n";
   print $indent,"}";   

}


# Standard PDL postamble

sub postamble {'

# Rules for the generic preprocessor

.SUFFIXES: .g
.g.c:
	$(PERL) -I'.whereami().' -MPDL::Core::Dev -e genpp $<  > $@

.g$(OBJ_EXT):
	$(PERL) -I'.whereami().' -MPDL::Core::Dev -e genpp $<  > $*.c
	$(CCCMD) $(CCCDLFLAGS) -I$(PERL_INC) $(DEFINE) $*.c

';}

# Return library locations

sub whereami {
   for $dir (@INC) {
      return $dir if -e "$dir/PDL/Core/Dev.pm";
   }
   die "Unable to determine directory path to PDL::Core::Dev module\n";
}


1; # Return OK

