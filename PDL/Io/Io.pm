

# Standard I/O functions (plain perl) for the PDL module

package PDL::Io;

@EXPORT_OK = qw( rfits rdsa rcols rgrep wfits );

@EXPORT_STATIC = qw( rfits rdsa rcols rgrep );

use PDL::Core;     # Grab the Core names
use DynaLoader; use Carp; use SelfLoader;
@ISA = qw( PDL::Exporter DynaLoader SelfLoader ); 


# Load compiled code only on demand

BEGIN {$io_loaded = 0}

local $^W=0;  # Do it this way to suppress spurious warnings
eval << 'EOD';
sub AUTOLOAD {
   unless($io_loaded) {
      bootstrap PDL::Io;
      print "IO loaded\n" if $PDL::verbose;
      $io_loaded = 1;
   }
   $SelfLoader::AUTOLOAD = $AUTOLOAD;
   goto &SelfLoader::AUTOLOAD;
}
EOD

1; # Return OK status

__DATA__

# SelfLoaded code

sub ext1D { # Internal routine to extend 1D PDL array by size $n
   my ($a,$n) = @_;
   $$a{Data} .= "\0"x($n*howbig($$a{Datatype}));
   $$a{Dims} = [$$a{Dims}[0]+$n];
   $a->flush;
1;}

# Read ASCII cols from file into PDL variables efficiently
# If no columns are specified all are assumed
# Will optionally only process lines matching a pattern.

$colsep = " "; # Default column seperator

sub rcols { 
   croak 'Usage ($x,$y,...) = rcols ($file, [[$pattern],$col1, $col2,] ...) or PDL->rcols(...)' if $#_<1;
   my $class = shift;
   my $file = shift;
   my $pattern;
   if ($_[0] =~ m|^/.*/$|) { # Is a pattern
      $pattern = shift;
      substr($pattern,0,1)=""; substr($pattern,-1,1)="";  # Removes //s
   }

   my @cols = @_;  
   my (@ret,@v,$k); my ($m,$n)=(-1,0); # Count/PDL size

   open(FILE,$file) || die "File $file not found\n"; 
   while(<FILE>) { 

      if ($pattern eq "") {
           next if /^#/;    # Only skip comments
      }
      else{
           next unless /$pattern/; 
      }

      @v = $colsep == ' ' ? split(' ') : split($colsep) ; $m++;  # Count got
      if ($m==0) { 
          @cols = (0..$#v) if $#cols<0; # Use number of cols in first line
          for (0..$#cols) {
              $ret[$_] = double($class->new([0])); # Create PDLs
          }
      }
      if ($n<$m) {
          for (0..$#cols) {
              ext1D( $ret[$_], 10000 ) # Extend PDL in buffered manner
          }
          $n += 10000;
      }
      $k=0; for(@cols) { set $ret[$k++], $m, 1*$v[$_] } # Set values - '1*' is 
   }                                                    # split() bug workaround

   close(FILE);
   for (@ret) { $_->flush; $_ = sec($_,0,$m); }; # Truncate
   wantarray ? return(@ret) : return $ret[0];
}

# Like rcols but with pattern matching
#
# e.g.:
#
# ($a,$b) = rgrep $file, '/Foo (.*) Bar (.*) Mumble/';
#
# i.e. - The vectors $a and $b get the progressive values of $1, $2 etc.

sub rgrep  { 
   croak 'Usage ($x,$y,...) = rgrep ($file, $pattern) or PDL->rgrep(...)' if $#_!=2;
   my $class   = shift;
   my $file    = shift;
   my $pattern = shift; 
   die "Not a pattern\n" unless $pattern =~ m|^/.*/$|; # Looks like /stuff/
   substr($pattern,0,1)=""; substr($pattern,-1,1)="";  # Removes //s

   my (@ret,@v,$k,$nret); my ($m,$n)=(-1,0); # Count/PDL size

   open(FILE,$file) || die "File $file not found\n"; 
   while(<FILE>) { 
      next unless /$pattern/; 
      $k=1; @v=();
      while(defined $$k) { # Loop over $1, $2, ...
         push @v, $$k; $k++;
      }
         
      $m++;  # Count got
      if ($m==0) { 
          $nret = $k-2;   # Last index of values to return
          for (0..$nret) {
              $ret[$_] = double($class->new([0])); # Create PDLs
          }
      }
      if ($n<$m) {
          for (0..$nret) {
              ext1D( $ret[$_], 10000 ); # Extend PDL in buffered manner
          }
          $n += 10000;
      }
      $k=0; for(0..$nret) { set $ret[$k++], $m, 1*$v[$_] } # Set values - '1*' is 
   }                                                      # ensures numeric
   close(FILE);
   for (@ret) { $_->flush; $_ = sec($_,0,$m) }; # Truncate
   wantarray ? return(@ret) : return $ret[0];
}

sub wfits { # Write a PDL to a FITS format file 

   croak 'Usage: wfits($pdl,$file)' if $#_!=1;

   my ($pdl,$file) = @_; 
   my ($k, $buff, $off, $ndims);
   local($nbytes, %hdr);

   open(FITS, ">$file") || croak "Unable to create FITS file $file\n";
   printf FITS "%-80s", "SIMPLE  =                    T ";

   $nbytes = 80; # Number of bytes written so far

   # Write FITS header

   %hdr = %{$$pdl{Hdr}};  # Header copy

   delete $hdr{SIMPLE}; delete $hdr{END}; 

   $hdr{BITPIX} =   8 if $$pdl{Datatype} == $PDL_B;
   $hdr{BITPIX} =  16 if $$pdl{Datatype} == $PDL_S || $$pdl{Datatype} == $PDL_US;
   $hdr{BITPIX} =  32 if $$pdl{Datatype} == $PDL_L;
   $hdr{BITPIX} = -32 if $$pdl{Datatype} == $PDL_F;
   $hdr{BITPIX} = -64 if $$pdl{Datatype} == $PDL_D;
   wheader('BITPIX');
   
   $ndims = scalar(@{$$pdl{Dims}}); # Dimensions of data array
   $hdr{NAXIS}  = $ndims;
   wheader('NAXIS');
   for $k (1..$ndims) { $hdr{"NAXIS$k"} = $$pdl{Dims}[$k-1] }
   for $k (1..$ndims) { wheader("NAXIS$k") }
   for $k (sort keys %hdr) { wheader($k) }

   printf FITS "%-80s", "END"; $nbytes += 80;
   $nbytes %= 2880;
   print FITS " "x(2880-$nbytes) if $nbytes != 0; # Fill up HDU

   # Decide how to byte swap - note does not quite work yet. Needs hack
   # to IO.xs 

   my $bswap = sub {};     # Null routine
   if ( !isbigendian() ) { # Need to set a byte swap routine
      $bswap = \&bswap2 if $$pdl{Datatype} == $PDL_S || $$pdl{Datatype} == $PDL_US;
      $bswap = \&bswap4 if $$pdl{Datatype} == $PDL_L || $$pdl{Datatype} == $PDL_F;
      $bswap = \&bswap8 if $$pdl{Datatype} == $PDL_D;
   }

   # Write FITS data 

   $off = 0; $nbytes = length $$pdl{Data};
   while ($nbytes - $off > 2880) {
      $buff = substr($$pdl{Data}, $off, 2880);
      &$bswap($buff); print  FITS $buff;
      $off += 2880;
   }
   $buff = substr($$pdl{Data}, $off ); 
   $buff .= " "x(2880-length($buff));  # Fill HDU
   &$bswap($buff); print  FITS $buff;

   close(FITS);

1;}

sub wheader {     # Local utility routine of wfits()
   my $k = shift;
   if ($hdr{$k} eq "") {
      printf FITS "%-80s", substr($k,0,8);
   }
   else{
      printf FITS "%-8s= ", substr($k,0,8);
      if ($hdr{$k} =~ /^ *([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))? *$/) { # Number?
         printf FITS "%-70s", substr($hdr{$k},0,70);
      }
     else {
         printf FITS "%-70s", "'".substr($hdr{$k},0,68)."'";
      }
   }
   $nbytes += 80; delete $hdr{$k};
1;}

sub rdsa {
    croak 'Usage: ([$xaxis],$data) = rdsa($file) or PDL->rdsa(...) ' if $#_!=1;
    my $class = shift;
    my $file = shift; my $pdl = $class->new(); my $xpdl;
    eval 'use DSA' unless $dsa_loaded++;
    croak 'Cannot use DSA library' if $@ ne "";

    $status = 0;

    # Most of this stuff stolen from Frossie:

    dsa_open($status);
    dsa_named_input('IMAGE',$file,$status);
    goto skip if $status != 0;

    dsa_get_range('IMAGE',$vmin,$vmax,$status);
    dsa_data_size('IMAGE',5, $data_ndims, \@data_dims, $data_elements, $status);
    dsa_map_data('IMAGE','READ','FLOAT',$data_address,$data_slot,$status);
   
    @data_dims = @data_dims[0..$data_ndims-1];
    print "Dims of $file = @data_dims\n" if $PDL::verbose;
    $$pdl{Dims} = [@data_dims]; $$pdl{Datatype}=$PDL_F;
    mem2string($data_address,4*$data_elements,$$pdl{Data});

    if (wantarray) { # Map X axis values
      dsa_axis_size('IMAGE',1,5, $axis_ndims, \@axis_dims, 
                    $axis_elements, $status);
      dsa_map_axis_data('IMAGE',1,'READ','FLOAT',$axis_address,
                    $axis_slot,$status);
      @axis_dims = @axis_dims[0..$axis_ndims-1];
      $xpdl = $class->new();
      $$xpdl{Dims} = [@axis_dims]; $$xpdl{Datatype}=$PDL_F;
      mem2string($axis_address,4*$axis_elements,$$xpdl{Data});
    }

    skip: dsa_close($status);

    croak("rdsa: obtained DSA error") if $status != 0;
  
    return ($xpdl,$pdl);
}

sub rfits { # Read a FITS format file and return a PDL

   croak 'Usage: $a = rfits($file) or PDL->rfits(...)' if $#_!=1;
   my $class = shift;
   my $file = shift; my $pdl  = $class->new();
   my($nbytes, $line, $name, $rest, $size, $i, $bscale, $bzero);

   open(FITS, $file) || croak "FITS file $file not found";
   $nbytes = 0; # Number of bytes read so far
   $line = "";

   $$pdl{Hdr} = {};  # Read the FITS ASCII header into $pdl{Hdr}
   while( !eof(FITS)) {
      read(FITS,$line,80);
      croak "file $file is not in FITS-format:\n$line\n"
                  if( $nbytes==0 && ($line !~ /^SIMPLE  = +T/));
      $nbytes += 80;

      $name = (split(' ',substr($line,0,8)))[0]; $rest=substr($line,8);
      $$pdl{Hdr}{$name} = "";
      $$pdl{Hdr}{$name}=$1 if $rest =~ m|^= +(.*\S) *$| ;
      $$pdl{Hdr}{$name}=$1 if $rest =~ m|^= +(.*\S) +/.*$| ;
      $$pdl{Hdr}{$name}=$1 if $rest =~ m|^= '(.*)' *$| ;
      $$pdl{Hdr}{$name}=$1 if $rest =~ m|^= '(.*)' +/.*$| ;
      last if $name eq "END";
   }

   $$pdl{Dims} = []; # Store the dimenions 1..N, compute total number of pixels
   $size = 1;  $i=1;
   while(defined( $$pdl{Hdr}{"NAXIS$i"} )) {
     $size = $size*$$pdl{Hdr}{"NAXIS$i"}; 
     push @{$$pdl{Dims}}, $$pdl{Hdr}{"NAXIS$i"} ; $i++;
   }

   $nbytes %= 2880;
   read(FITS,$$pdl{Data},2880-$nbytes) if $nbytes!=0;  # Skip to end of card

   $$pdl{Datatype} = $PDL_B    if $$pdl{Hdr}{"BITPIX"} ==   8;
   $$pdl{Datatype} = $PDL_S    if $$pdl{Hdr}{"BITPIX"} ==  16;
   $$pdl{Datatype} = $PDL_L    if $$pdl{Hdr}{"BITPIX"} ==  32;
   $$pdl{Datatype} = $PDL_F    if $$pdl{Hdr}{"BITPIX"} == -32;
   $$pdl{Datatype} = $PDL_D    if $$pdl{Hdr}{"BITPIX"} == -64;

   print "BITPIX = ",$$pdl{Hdr}{"BITPIX"}," size = $size pixels \n" 
         if $PDL::verbose;

   # Slurp the FITS binary data

   print "Reading ",$size*howbig($$pdl{Datatype}) , "bytes\n" if $PDL::verbose;

   read( FITS, $$pdl{Data}, $size*howbig($$pdl{Datatype}) );

   close(FITS);

   if (!isbigendian() ) { # Need to byte swap on little endian machines
      bswap2($pdl) if $$pdl{Datatype} == $PDL_S;
      bswap4($pdl) if $$pdl{Datatype} == $PDL_L || $$pdl{Datatype} == $PDL_F;
      bswap8($pdl) if $$pdl{Datatype} == $PDL_D;
   }

   $bscale = $$pdl{Hdr}{"BSCALE"}; $bzero = $$pdl{Hdr}{"BZERO"};
   print "BSCALE = $bscale &&  BZERO = $bzero\n" if $PDL::verbose;
   $pdl = $pdl*$bscale + $bzero if $bzero ne "" && $bscale ne "" 
                                && $bzero != 0 && $bscale != 1;
   return $pdl;
}

1; # OK status

