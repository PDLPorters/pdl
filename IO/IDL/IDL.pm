=head1 NAME

PDL::IO::IDL -- I/O of IDL Save Files

=head1 DESCRIPTION

PDL::IO::IDL allows you to read and write IDL(tm) data files.

Currently, only reading is supported, and even that is to be 
considered EXPERIMENTAL.  Currently, scalars, arrays, and structures
are supported.  Heap pointers, compiled code, and objects are not 
supported.  Of those three, only heap pointers are likely to be
supported in the near future.

=head1 NOTES

These things are known to be not working but will be fixed RSN:

=over 3

=item COMMON blocks

=item COMPLEX numbers

=item PTR types

=item writing (not written yet)

=back

These things are known to be not working and will probably never be fixed

=over 3

=item Compiled code

=item Objects

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

    $VERSION = 0.1;
    
    use PDL;
    use PDL::Exporter;
    use Carp;
    
  }
use strict;
  
=head2 ridl

=for usage 

$a = ridl("foo.sav");

=for ref 

Read an IDL save file from a file. 

Upon successful completion, $a is a hash ref containing all of the
variables that are present in the save file, indexed by original
variable name.  

Numeric arrays are stored as PDLs, structures are stored as hashes,
and string and structure arrays are stored as perl lists.

Because IDL identifiers can't contain special characters, some fields that
start with '+' are used to store metadata about the file itself.

=cut


sub ridl {
  my( $name ) = shift;
  
  STDERR->autoflush(1);

  open(IDLSAV,"<$name") || barf("ridl: Can't open `$name' for reading\n");
  
  my $hash = read_preamble();

  read_records($hash);

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
	      ,['COMMON_BLOCK',undef]     # 1      (COMMON block definition)
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
	  ,["Byte",      \&r_n_pdl,    [byte]        ] #  1 
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

  do { 

    ### Read header of the record
    
    sysread(IDLSAV, $tbuf, 4) || barf("PDL::IO::IDL: unexpected EOF\n");
    my $type = unpack "N",$tbuf;
    
    ### Hack to set 64-bit file offsets BEFORE recording the next seek entry
    
    if($type==17) {
      r_p64($hash);
    }
    
    ### Record the next seek location
    ### (and discard 8 more bytes)
    ###
    ### (Fix this with a 64-bit PDL snarf like in r_n_pdl, below)
    
    my $next;
    if($hash->{'+64'}) {
      sysread(IDLSAV,$buf,8 + 8);
      my @next = unpack "NN",$buf;
      $next = ($little_endian) ? 
	($next[0] + 2**32 * $next[1]) : 
	($next[1] + 2**32 * $next[0]);
    } else {
      sysread(IDLSAV,$buf,4 + 8);
      $next = unpack "N",$buf;
    }
    
    ### Infinite-loop detector

    barf("Repeat index finder was activated!\n")
      if($nexts{$next}) ;
    $nexts{$next} = 1;
    
    ### Jump into the appropriate handling routine

    $retval = 1;

    if(defined $types->[$type]) {
      if(defined ($types->[$type]->[1])) {
	print "Found record of type $types->[$type]->[0]...\n" if($PDL::debug);
	$retval = &{$types->[$type]->[1]}($hash);
	print "OK.\n" if($PDL::debug);
      } else {
	print STDERR "Ignoring record of type ".$types->[$type]->[0]." - not implemented.\n";
      }
    } else {
      print STDERR "\nIgnoring record of unknown type $type - not implemented.\n";
      print STDERR "\t(chars were ",join(",",unpack "cccc",$tbuf),")\n";
    }
    
    sysseek(IDLSAV, $next, 0);
    
  } while($retval);
}





##############################
# r_end
#
# Jumptable entry for the END TABLE keyword -- just return 0.

sub r_end { 0; }



##############################
# r_64
#
# Jumptable entry for the PROMOTE64 keyword -- just check 
# that 64-bit numbers are OK, and set the flag if they are.
sub r_64 {
  my $out = shift;
  barf "File requires 64-bit handling (not compiled into your perl)\n"
    unless($quad_ok);

  $out->{'+64'} = 1;
}


  
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
  print STDERR "Warning: unknown IDL save file version $version; winging it!"
    if($version != 5);

  $hash->{"+meta"}->{v_arch} = r_string();
  $hash->{"+meta"}->{v_os} = r_string();
  $hash->{"+meta"}->{v_release} = r_string();
  return 1;
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
      ## Scalar case
      $hash->{$name} = 
	  &{$vtypes->[$type]->[1]}
              ([],   @{$vtypes->[$type]->[2]})
  } else {
      ## Array case

      my($arrdesc) = r_arraydesc();

      if(($flags & 32) == 0) {
	  ## Simple array case
	  print "simple array...type=$type\n" if($PDL::debug);
	  my @args= ([ @{$arrdesc->{dims}}[0..$arrdesc->{ndims}-1]], 
		     @{$vtypes->[$type]->[2]});
	  my $pdl =  &{$vtypes->[$type]->[1]}(@args);
	  $hash->{$name} = $pdl; 

      } else {
	  ## Structure case
	  print "structure...\n" if($PDL::debug);
	  my($sname) = r_structdesc();

	  my @structs;
	  print "Reading $arrdesc->{nelem} structures....\n" if($PDL::debug);
	  my $i;

	  {my $buf; sysread(IDLSAV,$buf,4);}

	  for ($i=0;$i<$arrdesc->{nelem};$i++) {
  	    push(@structs,r_struct($sname));
	  }

	  # Make a multi-dimensional list that contains the structs
	  $hash->{$name} = multi_dimify($arrdesc,\@structs);
#	  $hash->{$name} = \@structs;
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

    print " "x$n,"m_d: ndims=$arrdesc->{ndims}, n=$n\n";
    return shift @{$structs} 
	if($arrdesc->{ndims} <= $n);

    print "dims is ",join(",",@{$arrdesc->{dims}}),"\n";

    return [splice (@{$structs}, 0, $arrdesc->{dims}->[0])]
	if($arrdesc->{ndims} - $n == 1);
    
    my $out = [];
    my $i;
    for ($i=0;$i<$arrdesc->{dims}->[$n];$i++) {
	push(@{$out},multi_dimify($arrdesc,$structs,$n+1));
    }
    print " "x$n,"ok\n";
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
    sysread(IDLSAV,$buf,4);   # Discard initial long (value=9) from descriptor
    my($name) = r_string(); # Have to store structures in the structure table.
    $name =~ s/\s//g;

    $name = "+anon".scalar(keys %{$PDL::IO::IDL::struct_table})
	if($name eq '');

    sysread(IDLSAV,$buf,4*3);
    my($predef,$ntags,$nbytes) = unpack("N"x3,$buf);
	
    if(!$predef) {
	my $i;
	print "not predefined. ntags=$ntags..\n";

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

	
	print "ntags is $ntags\n";
	### Read tag names.
	for $i(0..$ntags-1) {
	    push(@{$st->{names}},r_string());
    	    print "\t$i: '".$st->{names}->[-1]."'...\n";

	}
	
	### Search for nested arrays & structures
	my ($nstructs,$narrays);

	for $i(0..$ntags-1) {
	    my $a = $st->{descrip}->[$i*3+2];
	    print "\t",$st->{names}->[$i],": type is ".$st->{descrip}->[$i*3+1]." (".$vtypes->[$st->{descrip}->[$i*3+1]]->[0].")\n";
	    $nstructs++ if($a & 32);
	    $narrays++  if($a & 38);
	}

	print "narrays=$narrays\n";
	for $i(0..($narrays-1)) {
	    push( @{$st->{arrays}}, r_arraydesc() );
	}

	print "nstructs=$nstructs\n";
	for $i(0..($nstructs-1)) {
	    push( @{$st->{structs}}, r_structdesc() );
	}

    }

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
sub r_struct {
    my($sname) = shift;
 
    # Get the structure descriptor from the table.
    my($sd) = $PDL::IO::IDL::struct_table->{$sname};
    barf "Unknown structure type $sname" unless defined($sd);

    # Initialize the structure itself and the array and structure indices.
    my($struct) = {};
    my($array_no, $struct_no);

    # Loop over tags and snarf each one 
    my($i);
    for($i=0;$i<$sd->{ntags};$i++) {
	my($name) = $sd->{names}->[$i];
	
	my($type) = $sd->{descrip}->[$i*3+1];
	my($flags) = $sd->{descrip}->[$i*3+2];

	barf("PDL::IO::IDL: Unknown variable type $type in structure")
	    unless defined($vtypes->[$type]);
	
	unless(defined($vtypes->[$type]->[1])) {
	    print "Skipping tag $name in structure - unsupported type ".$vtypes->[$type]->[0]."\n";
	    $array_no++ if($flags & 38);
	    $struct_no++ if($flags & 32);

	} else {
	    print "Reading tag $i ($name)...\n";
	    if( (($flags & 4)==0) and (($flags & 32)==0) ) {
		## Scalar tag case
		$struct->{$name} = &{$vtypes->[$type]->[1]}
		           ([],   @{$vtypes->[$type]->[2]});
	    } else {

		### Array and/or structure case ###

		my($arrdesc) = $sd->{arrays}->[$array_no++];
		
		if(($flags & 32) == 0) {

		    ### Tag is a simple array ###

		    my @args = ([ @{$arrdesc->{dims}}[0..$arrdesc->{ndims}-1]],
				@{$vtypes->[$type]->[2]});
		    my $pdl = &{$vtypes->[$type]->[1]}(@args);

		    $struct->{$name} = $pdl;

		} else {

		    ### Tag is a structure ###
		    
		    my $tsname = $sd->{structs}->[$struct_no++];
		    my @structs = [];
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
    sysread(IDLSAV,$buf,4);
    return r_string();
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
    my($dims,$type) = @_;

    my $pdl = new PDL;

    $pdl->set_datatype($type->enum);
    $pdl->setdims($dims);

    my $dref = $pdl->get_dataref();
    my $dsize = PDL::Core::howbig($type) * (pdl($dims)->prod);

    sysread(IDLSAV, $$dref, $dsize - ($dsize % -4) );
    
    if($little_endian) {
	bswap2($pdl) if($dsize==2);
	bswap4($pdl) if($dsize==4);
	bswap8($pdl) if($dsize==8);
    }

    $pdl;
}

sub r_n_cast {
    my($dims,$type1,$type2) = @_;
    
    (r_n_pdl($dims,$type1))->convert($type2);
}


=head1 AUTHOR, LICENSE, NO WARRANTY

Copyright (c) 2003 Craig DeForest.  THIS CODE IS PROVIDED WITH NO
WARRANTY and may be distributed and/or modified under the same terms
as PDL itself.

Some disclaimers: This code is based on the analysis of the IDL save
file format published by Craig Markwardt in 2002.  I do not myself
have IDL installed on my computer, so the only testing I've been able
to do is with third-party save files sent to me by other scientists.
Your mileage may vary.  Please send bug reports to
"perldl-porters@jach.hawaii.edu".

IDL is a trademark of Research Systems Incorporated.  
RSI is not affiliated with the PDL development team.

=cut

1;
