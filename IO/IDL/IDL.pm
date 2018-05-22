=head1 NAME

PDL::IO::IDL -- I/O of IDL Save Files

=head1 DESCRIPTION

PDL::IO::IDL allows you to read and write IDL(tm) data files.

Currently, only reading is implemented.  Scalars, arrays,
and structures are all supported.  Heap pointers, compiled code, and
objects are not supported.  Of those three, only heap pointers are
likely to be supported in the future.

This code was not developed by RSI, makers of IDL.  

=head1 NOTES

These things seem to work:

=over 3

=item BYTE, SHORT, LONG, FLOAT, and DOUBLE numeric types and arrays

All of these types seem to work fine.  The corresponding variable is
stored as a PDL in the hash element with the same name as the original
variable in the file.  Arrays are byteswapped as needed and are read in so 
that the dim list has the same indexing order within PDL as it did within IDL.

=item STRINGs and arrays of STRINGs

String types are stored as Perl list refs, in the hash element with
the same name as the original variable in the file.

=item Structures

Structures are stored as hash refs.  The elements of the hash may be
accessed as values within the hash.

=item Common blocks

Variables that are notated as being in a common block are read as
normal.  Common-block names are collected in the special hash value
'+common', which contains a hash each keyword of which is the name of
a common block and each value of which is an array of variable names.

=back

These things are known to be not working and may one day be fixed:

=over 3

=item COMPLEX numbers

These could be implemented as 2-arrays or as PDL::Complex values, but aren't yet.

=item PTR types

These could be implemented as perl refs but currently aren't.

=item writing 

Maybe one day -- but why bother writing a broken file format?  NetCDF is better.

=back

These things are known to be not working and will probably never be fixed

=over 3

=item Compiled code

Decompiling IDL code is a violation of the IDL end-user license.  To
implement this, someone who does not hold an IDL license would have to
reverse-engineer a set of .SAV files sent to that person by someone
else with an IDL license.

=item Objects

IDL objects contain compiled code.

=back

=head1 FUNCTIONS

=cut

  package PDL::IO::IDL;
  
  BEGIN {
    
    use Exporter ();
    package PDL::IO::IDL;
    @ISA = ( Exporter );
    @EXPORT_OK = qw( ridl );
    @EXPORT = @EXPORT_OK;
    @EXPORT_TAGS = ( Func=>[@EXPORT_OK] );

    our $VERSION = "0.5";
    $VERSION = eval $VERSION;
    
    use PDL;
    use PDL::Exporter;
    use Carp;
    
  }
use strict;
  
=head2 ridl

=for usage 

$x = ridl("foo.sav");

=for ref 

Read an IDL save file from a file. 

Upon successful completion, $x is a hash ref containing all of the
variables that are present in the save file, indexed by original
variable name.  

IDL identifiers are case insensitive; they're all converted to
upper-case in the hash that gets returned.  This may be adjustable at
a future date.  Furthermore, because IDL identifiers can't contain
special characters, some fields that start with '+' are used to store
metadata about the file itself.

Numeric arrays are stored as PDLs, structures are stored as hashes,
and string and structure arrays are stored as perl lists.  Named
structure types don't exist in perl in the same way that they do in
IDL, so named structures are described in the 'structs' field of the
global metadata.  Anonymous structures are treated as simple hashes.
Named structures are also simple hashes, but they also contain a field
'+name' that refers to the name of the structure type.  

=cut


sub ridl {
  my( $name ) = shift;
  
  STDERR->autoflush(1);

  open(IDLSAV,"<$name") || barf("ridl: Can't open `$name' for reading\n");
  
  my $hash = read_preamble();

  read_records($hash);

  my @snames = sort keys %{$PDL::IO::IDL::struct_table};
  @snames = grep(!m/^\+/,@snames);
  if(@snames) {
    $hash->{'+structs'}={};
    local $_;
    for(@snames) {
      $hash->{'+structs'}->{$_} = 
	$PDL::IO::IDL::struct_table->{$_}->{'names'};
    }
  }

  return $hash;
}


############################################################
##
## Data structure definitions...
##
## This is a list, each element of which contains a description and
## subroutine to read that particular record type.
##

our $types = [ ['START_MARKER',undef]     # 0      (start of SAVE file)
	      ,['COMMON_BLOCK',\&r_com]   # 1      (COMMON block definition)
	      ,['VARIABLE',\&r_var]       # 2      (Variable data)
	      ,['SYSTEM_VARIABLE',undef]  # 3      (System variable data)
	      ,undef                      # 4        (??)
	      ,undef                      # 5        (??)
	      ,['END_MARKER',\&r_end]     # 6      (End of SAVE file)
	      ,undef                      # 7        (??)
	      ,undef                      # 8        (??)
	      ,undef                      # 9        (??)
	      ,['TIMESTAMP',\&r_ts]       # 10     (Timestamp of the save file)
	      ,undef                      # 11       (??)
              ,['COMPILED',undef]         # 12     (Compiled procedure or func)
              ,['IDENTIFICATION',undef]   # 13     (Author identification)
	      ,['VERSION',\&r_v]          # 14     (IDL Version information)
	      ,['HEAP_HEADER',undef]      # 15     (Heap index information)
	      ,['HEAP_DATA',undef]        # 16     (Heap data)
	      ,['PROMOTE64',\&r_p64]      # 17     (Starts 64-bit file offsets)
	      ];


############################################################
##
## Vtypes -- Representations of IDL scalar variable types.
## The first element is the name, the second element is either a 
## perl string (that should be fed to unpack) or a code ref to a 
## sub that decodes the type.
##

our $vtypes = [
	   undef                                       #  0 
	  ,["Byte",      \&r_byte_pdl, []            ] #  1 
	  ,["Short",     \&r_n_cast,   [long,short]  ] #  2 
	  ,["Long",      \&r_n_pdl,    [long]        ] #  3
	  ,["Float",     \&r_n_pdl,    [float]       ] #  4
	  ,["Double",    \&r_n_pdl,    [double]      ] #  5
	  ,["Complex",   undef                       ] #  6
	  ,["String",    \&r_strvar,   []            ] #  7
	  ,["Structure", sub {},       []            ] #  8
	  ,["ComplexDbl",undef                       ] #  9
	  ,["HeapPtr",   undef                       ] # 10 
	  ,["Object",    undef                       ] # 11
	  ,["UShort",    \&r_n_cast,   [long,ushort] ] # 12
	  ,["ULong",     \&r_n_pdl,    [long]        ] # 13
	  ,["LongLong",  undef                       ] # 14
	  ,["ULongLong", undef                       ] # 15
];
     

###
# Cheesy way to check if 64-bit is OK
our $quad_ok = eval { my @a = unpack "q","00000001"; $a[0]; };

### Initialized in read_preamble.
our $little_endian;
our $swab;
our $p64;


##############################
#
# read_preamble
#
# Reads the preamble of a file and returns the preamble as a hash
# ref.  In case of failure, it barfs.  Also initializes the structure table.
#

sub read_preamble {
  my $buf;
  my $out;

  sysread(IDLSAV,$buf,4) || barf ("PDL::IO::IDL: Couldn't read preamble\n");
  my @sig = unpack("a2S",$buf);

  barf("PDL::IO::IDL: This isn't an IDL save file (wrong magic)\n")
    if($sig[0] ne 'SR');

  if($sig[1] == 1024 || $sig[1] == 4) {
    $little_endian = ($sig[1] == 1024);
  } else {
    barf "Unrecognized IDL save file type\n";
  }

  $swab = $little_endian;

  $p64 = 0;

  $PDL::IO::IDL::struct_table = {};

  return {"+meta"=>{}};
}

##############################
#
# read_records
#
# Reads all the records of the file.  Splits out into several other
# types of record reader...
#
# 

sub read_records {
  my $hash = shift;
  my ($buf, $tbuf);

  my $retval;

  my %nexts;
  my $tag_count = 0;
  do { 

    ### Read header of the record
    
    sysread(IDLSAV, $tbuf, 4) || barf("PDL::IO::IDL: unexpected EOF\n");
    my $type = unpack "N",$tbuf;
    
    ### Record the next seek location
    ### (and discard 8 more bytes)
      
    my $next;
    if($p64) {
      print "Reading 64-bit location..."  if($PDL::debug);
      sysread(IDLSAV,$buf,8 + 8);
      my @next = unpack "NN",$buf;
      $next = $next[1] + 2**32 * $next[0];
    } else {      
      print "Reading 32-bit location..." if($PDL::debug);
      sysread(IDLSAV,$buf,4 + 8);
      $next = unpack "N",$buf;
    }
    print "$next\n" if($PDL::debug);
    
    ###
    ### Infinite-loop detector
    ###

    barf("Repeat index finder was activated! This is a bug or a problem with your file.\n")
      if($nexts{$next}) ;
    $nexts{$next} = 1;

    ###
    ### Call the appropriate handling routine
    ###

    $retval = 1;

    if(defined $types->[$type]) {
      if(defined ($types->[$type]->[1])) {
	print "Found record of type $types->[$type]->[0]...\n" if($PDL::debug || $PDL::IO::IDL::test);
	$retval = &{$types->[$type]->[1]}($hash);
	print "OK.\n" if($PDL::debug);
      } else {
	print STDERR "Ignoring record of type ".$types->[$type]->[0]." - not implemented.\n";
      }
    } else {
      print STDERR "\nIgnoring record of unknown type $type - not implemented.\n";
    }
    print "Seeking $next ($tag_count tags read so far...)\n" if($PDL::debug || $PDL::IO::IDL::test);
    $tag_count++;
    sysseek(IDLSAV, $next, 0);
  $FOO::hash = $hash;    
  } while($retval);

}




##############################
# r_com
#
# Jumptable entry for the COMMONBLOCK keyword -- this loads 
# the variable names that belong in the COMMON block into a
# metavariable.

sub r_com { 
  my $hash = shift;
  my $buf;

  sysread(IDLSAV,$buf,4);
  my $nvars = unpack "N",$buf;

  my $name = r_string();
  $hash->{"+common"}->{$name} = [];
  
  for my $i(1..$nvars) {
    push(@{$hash->{"+common"}->{$name}},r_string());
  }

  return 1;
}

    
  

##############################
# r_end
#
# Jumptable entry for the END TABLE keyword -- just return 0.

sub r_end { 0; }


##############################
# r_ts
#
# TIMESTAMP record handler
#
sub r_ts {
  my $hash = shift;
  my $buf;

  ### Read and discard a LONARR(258) -- why? I don't know.
  sysread(IDLSAV,$buf,1024); 
  $hash->{"+meta"}->{t_date} = r_string();
  $hash->{"+meta"}->{t_user} = r_string();
  $hash->{"+meta"}->{t_host} = r_string();

  return 1;
}



##############################
# r_version
#
# VERSION record handler
#
sub r_v {
  my $hash = shift;
  my $buf;
  my $version;

  sysread(IDLSAV,$buf,4);
  $version = $hash->{"+meta"}->{v_fmt} = unpack "N",$buf;

#  barf("Unknown IDL save file version ".$version)
  print STDERR "Warning: IDL file is v$version (neither 5 nor 6); winging it. Check results!\n"
    if($version != 5 && $version != 6);

  $hash->{"+meta"}->{v_arch} = r_string();
  $hash->{"+meta"}->{v_os} = r_string();
  $hash->{"+meta"}->{v_release} = r_string();
  return 1;
}

##############################
# r_p64
sub r_p64 {
  my $hash = shift;
  $p64 = 1;
}

##############################
# r_var
#
# VARIABLE reader - parse a single variable out of a VARIABLE record.
#

sub r_var {
  my $hash = shift;

  ### Read in the variable name 
  my $name = r_string();


  ### Read in and parse the type 

  my $buf;
  sysread(IDLSAV,$buf,8);
  my ($type,$flags) = unpack "NN",$buf;

  unless(defined $vtypes->[$type]) {
      barf("PDL::IO::IDL: Unknown variable type $type");
  }
  
  unless(defined $vtypes->[$type]->[1]) {
      print STDERR "Ignoring variable $name: unsupported type ".$vtypes->[$type]->[0]."\n";
      return 1;
  }

  print "Variable $name found (flags is $flags)...\n" if($PDL::debug);
  
  if((($flags & 4) == 0)  and  (($flags & 32) == 0)) {
      print "it's a scalar\n" if($PDL::debug);


      sysread(IDLSAV,$buf,4);
      my($seven) = unpack "N",$buf;
      if($seven != 7) {
	print STDERR "Warning: expected data-start key (7) but got $seven, for variable $name\n";
      }
      
      ## Scalar case
      $hash->{$name} = 
	  &{$vtypes->[$type]->[1]}
              ($flags, [],   @{$vtypes->[$type]->[2]})
  } else {
      ## Array case

      my($arrdesc) = r_arraydesc();

      if(($flags & 32) == 0) {
	
	  ## Simple array case
	sysread(IDLSAV,$buf,4);
	my($indicator) = unpack "N",$buf;

	print STDERR "Warning: Reading data from an array but got code $indicator (expected 7)\n"
	  if($indicator != 7);
	  
	  print "simple array...type=$type\n" if($PDL::debug);

	my @args= ($flags,[ @{$arrdesc->{dims}}[0..$arrdesc->{ndims}-1]], 
		   @{$vtypes->[$type]->[2]});
	my $pdl =  &{$vtypes->[$type]->[1]}(@args);
	$hash->{$name} = $pdl; 
	
      } else {

	  ## Structure case
	  print "structure...\n" if($PDL::debug);
	  my($sname) = r_structdesc();

	  my @structs;
	  print "Reading $arrdesc->{nelem} structures....\n" if($PDL::debug || $PDL::IO::IDL::test);
	  my $i;

	  {my $buf; sysread(IDLSAV,$buf,4);}

	  for ($i=0;$i<$arrdesc->{nelem};$i++) {
	    if($PDL::IO::IDL::test && !($i%100)){
	      print "$i of $arrdesc->{nelem}...\n";
	    }
	      
  	    push(@structs,r_struct($sname));
	  }

	  # Make a multi-dimensional list that contains the structs
	  $hash->{$name} = multi_dimify($arrdesc,\@structs);

      }
  }


  return 1;
}
  

##############################
# multi_dimify 
#
# Take a linear list of items and an array descriptor, and 
# hand back a multi-dimensional perl list with the correct dimension
# according to the descriptor.  (This isn't necessary for PDL types,
# only for structures and strings).
#

sub multi_dimify {

    my($arrdesc,$structs,$n) = @_;

    return shift @{$structs} 
	if($arrdesc->{ndims} <= $n  or
	   $arrdesc->{ndims} == 0 or
	   $arrdesc->{ndims}-$n == 1 && $arrdesc->{dims}->[$n]==1);


    if($arrdesc->{ndims} - $n == 1){
      my @ret = splice @{$structs},0,$arrdesc->{dims}->[$n];
      return \@ret;
    }
    

    my $out = [];
    my $i;
    for ($i=0;$i<$arrdesc->{dims}->[$n];$i++) {
	push(@{$out},multi_dimify($arrdesc,$structs,$n+1));
    }

    return $out;
}



######################################################################
######################################################################


#
# r_arraydesc - read an array descriptor from the file 
# 

our $r_arraydesc_table = ['a','b','nbytes','nelem','ndims','c','d','nmax'];

sub r_arraydesc {
    my $out = {};
    my $buf;

    sysread(IDLSAV,$buf,4*8);
    
    my(@vals) = unpack("N"x8,$buf);
    print STDERR "r_arraydesc_table: vals[0]=".$vals[0]." (should be 8)\n"
      if($vals[0] != 8);
    for my $i(0..7) {
	$out->{$r_arraydesc_table->[$i]} = $vals[$i];
    }
    my $nmax = $vals[7];
    my $nelem = $vals[3];

    sysread(IDLSAV,$buf,$nmax*4);
    $out->{dims} = [unpack("N"x$nmax,$buf)];
    my $dims = pdl(@{$out->{dims}});

    $out->{pdldims} = $dims;

    print STDERR "PDL::IO::IDL: Inconsistent array dimensions in variable (nelem=$nelem, dims=".join("x",@{$out->{dims}}).")"
	if($nelem != $dims->prod);
    
    $out;
}


##############################
#
# r_structdesc reads a structure description and stores it in the struct_table.
# You get back the name of the structure.
# 

sub r_structdesc {
    my $buf;

    print "Reading a structure description...\n" if($PDL::IO::IDL::test);

    sysread(IDLSAV,$buf,4);   # Discard initial long (value=9) from descriptor
    my($name) = r_string(); # Have to store structures in the structure table.
    $name =~ s/\s//g;
    
    $name = "+anon".scalar(keys %{$PDL::IO::IDL::struct_table})
	if($name eq '');

    sysread(IDLSAV,$buf,4*3);
    my($predef,$ntags,$nbytes) = unpack("N"x3,$buf);
    print "predef=$predef,ntags=$ntags,nbytes=$nbytes\n" if($PDL::debug);
    if(!($predef & 1)) {
	my $i;
	print "not predefined. ntags=$ntags..\n" if($PDL::debug || $PDL::IO::IDL::test);

	my $st = $PDL::IO::IDL::struct_table->{$name} = {
	    "ntags" => $ntags 
		,"nbytes"=> $nbytes
		,"names" => []
		,"arrays" => []
		,"structs" => []
	    };
	
	### Read tag descriptors.
	sysread(IDLSAV,$buf,3*4*$ntags);
	$st->{descrip} = [(unpack "N"x(3*$ntags), $buf)];

	
	print "ntags is $ntags\n" if($PDL::debug || $PDL::IO::IDL::test);
	### Read tag names.
	for $i(0..$ntags-1) {
	    push(@{$st->{names}},r_string());
	}
	
	### Search for nested arrays & structures
	my ($nstructs,$narrays);

	for $i(0..$ntags-1) {
	    my $x = $st->{descrip}->[$i*3+2];

	    $nstructs++ if($x & 32);
	    $narrays++  if($x & 38);
	}

	print "narrays=$narrays\n" if($PDL::debug || $PDL::IO::IDL::test);
	for $i(0..($narrays-1)) {
	    push( @{$st->{arrays}}, r_arraydesc() );
	}

	print "nstructs=$nstructs\n" if($PDL::debug || $PDL::IO::IDL::test);
	for $i(0..($nstructs-1)) {
	    push( @{$st->{structs}}, r_structdesc() );
	}

    }
    print "finished with structure desc...\n" if($PDL::IO::IDL::test);
    return $name;
}

##############################
#
# r_struct
#
# Given the name of a structure type, read in exactly one of them.
# If I were smarter, this would be the same code as the variable
# reader, but I'm not so it's only similar.
#
our $r_struct_recursion = 0;

sub r_struct {
    my($sname) = shift;
 

    print ("_ "x$r_struct_recursion) . "Reading a structure...\n" if($PDL::IO::IDL::test);
    my $zz=$r_struct_recursion;
    local($r_struct_recursion) = $zz++;

    # Get the structure descriptor from the table.
    my($sd) = $PDL::IO::IDL::struct_table->{$sname};
    barf "Unknown structure type $sname" unless defined($sd);

    # Initialize the structure itself and the array and structure indices.
    my($struct) = {};
    $struct->{'+name'} = $sname unless($sname =~ m/^\+/);

    my($array_no, $struct_no);

    # Loop over tags and snarf each one 
    my($i);
    for($i=0;$i<$sd->{ntags};$i++) {
	my($name) = $sd->{names}->[$i];
	
	my($type) = $sd->{descrip}->[$i*3+1];
	my($flags) = $sd->{descrip}->[$i*3+2];

	print "reading tag #$i ($sd->{names}->[$i])\n" if($PDL::debug);

	barf("PDL::IO::IDL: Unknown variable type $type in structure")
	    unless defined($vtypes->[$type]);
	
	unless(defined($vtypes->[$type]->[1])) {
	    print "Skipping tag $name in structure - unsupported type ".$vtypes->[$type]->[0]."\n";
	    $array_no++ if($flags & 38);
	    $struct_no++ if($flags & 32);

	} else {

	    if( (($flags & 4)==0) and (($flags & 32)==0) ) {
		## Scalar tag case
		$struct->{$name} = &{$vtypes->[$type]->[1]}
		           ($flags, [],   @{$vtypes->[$type]->[2]});
	    } else {

		### Array and/or structure case ###

		my($arrdesc) = $sd->{arrays}->[$array_no++];
#		sysread(IDLSAV,my $buf,4); # skip indicator
		
		if(($flags & 32) == 0) {

		    ### Tag is a simple array ###

		    my @args = ($flags,[ @{$arrdesc->{dims}}[0..$arrdesc->{ndims}-1]],
				@{$vtypes->[$type]->[2]});
		    my $pdl = &{$vtypes->[$type]->[1]}(@args);
		    print "  pdl is $pdl\n" if($PDL::debug);
		    $struct->{$name} = $pdl;

		} else {

		    ### Tag is a structure ###
		    
		    my $tsname = $sd->{structs}->[$struct_no++];
		    my @structs = ();
		    for $i(1..$arrdesc->{nelem}) {
			push(@structs,r_struct($tsname));
		    }

		    $struct->{$name} = multi_dimify($arrdesc,\@structs);

		}
	    }
	}
    } # end of ntags loop

    return $struct;
}
		
    
    

##############################
#
# r_string
#
# Reads a string value, leaving the file pointer correctly aligned
# on a 32-bit boundary (if it started that way).  Returns the string as
# a perl scalar.
#
sub r_string{
  my ($buf,$foo);
  sysread(IDLSAV, $buf, 4);  # Read the length...

  my ($len) = unpack "N",$buf;
  # Pad the length out to the next 32-bit boundary

  my $plen =  $len - ($len % -4) ;

  sysread(IDLSAV,$buf,$plen);
  return unpack "A$len",$buf;
}


##############################
#
# r_strvar 
#
# Reads a string variable (different than r_string because 
# of the extra length duplication in the IDL file...)
#
sub r_strvar {
    my $buf;
    my $flags = shift;
    sysread(IDLSAV,$buf,4);
    return r_string();
}

##############################
#
# r_byte_pdl
#
# Reads a byte PDL (stored as a strvar)
#
sub r_byte_pdl {
  my($flags,$dims) = @_;

  sysread(IDLSAV,my $buf,4)
    if($#$dims > 1);

  my $x = r_string();

  my $pdl = new PDL;
  $pdl->set_datatype(byte->enum);
  $pdl->setdims($dims);
  ${ $pdl->get_dataref() } = $x;
  $pdl->upd_data;

  $pdl;
}

##############################
#
# r_n_pdl
#
# Reads <n> normal integer-type numerical values as a pdl.
# You feed in the dimlist and type, you get back the 
# final pdl.  The read is padded to the nearest word boundary.
#

sub r_n_pdl {
    my($flags,$dims,$type) = @_;
    
    my $nelem = pdl($dims)->prod;
    my $dsize = PDL::Core::howbig($type);
    my $hunksize = $dsize * $nelem;

    my $pdl = PDL->new_from_specification($type,@$dims);
    my $dref = $pdl->get_dataref();
    
    my $len = sysread(IDLSAV, $$dref, $hunksize - ($hunksize % -4) );
    $pdl->upd_data;
    
    print "bytes were ",join(",",unpack "C"x($hunksize-($hunksize%-4)),$$dref),"\n" if($PDL::debug);
    
    
    if($swab) {
	bswap2($pdl) if($dsize==2);
	bswap4($pdl) if($dsize==4);
	bswap8($pdl) if($dsize==8);
    }

    $pdl;
}

sub r_n_cast {
    my($flags,$dims,$type1,$type2) = @_;

    (r_n_pdl($flags,$dims,$type1))->convert($type2);
}


=head1 AUTHOR, LICENSE, NO WARRANTY

THIS CODE IS PROVIDED WITH NO WARRANTY and may be distributed and/or
modified under the same terms as PDL itself.

This code is based on the analysis of the IDL save file format
published by Craig Markwardt in 2002. 

IDL is a trademark of Research Systems Incorporated (RSI).  The PDL
development team, and authors of this code, are not affiliated with RSI.

=cut

1;
