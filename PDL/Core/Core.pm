
# Core routines for PDL module

package PDL::Core;

$PDL::VERSION = "1.03p1";

# Functions exportable in this part of the module

@EXPORT_OK = qw( $PDL_B $PDL_S $PDL_US $PDL_L $PDL_F $PDL_D howbig nelem dims
	      pdl topdl null byte short ushort long float double
	      convert log10 inplace zeroes ones sequence min max sum list
	      listindices sec ins set at axisvals xvals yvals zvals rvals
	      callext convolve hist stats reshape transpose);

@EXPORT_STATIC = qw( pdl topdl null zeroes ones sequence );

use DynaLoader; use Carp;  
@ISA    = qw( PDL::Exporter DynaLoader ); 

bootstrap PDL::Core;

# Important variables (place in PDL namespace)

$PDL::verbose      = 0;        # Whether or not functions waffle
$PDL::name         = "PDL";    # what to call PDL objects
$PDL::use_commas   = 0;        # Whether to insert commas when printing arrays
$PDL::floatformat  = "%7g";    # Default print format for long numbers 
$PDL::doubleformat = "%10.8g"; 

####################### Overloaded operators #######################

{ package PDL;

   use overload
     '+'      =>  sub {PDL::Core::biop(@_,"+")},        
     '-'      =>  sub {PDL::Core::biop(@_,"-")}, 
     '*'      =>  sub {PDL::Core::biop(@_,"*")},        
     '/'      =>  sub {PDL::Core::biop(@_,"/")}, 
     '>'      =>  sub {PDL::Core::biop(@_,">")},        
     '<'      =>  sub {PDL::Core::biop(@_,"<")}, 
     '>='     =>  sub {PDL::Core::biop(@_,">=")},       
     '<='     =>  sub {PDL::Core::biop(@_,"<=")}, 
     '<<'     =>  sub {PDL::Core::biop(@_,"<<")},       
     '>>'     =>  sub {PDL::Core::biop(@_,">>")}, 
     '&'      =>  sub {PDL::Core::biop(@_,"&")},        
     '|'      =>  sub {PDL::Core::biop(@_,"|")}, 
     '^'      =>  sub {PDL::Core::biop(@_,"^")},        
     '=='     =>  sub {PDL::Core::biop(@_,"==")}, 
     '!='     =>  sub {PDL::Core::biop(@_,"!=")},       
     '+='     =>  sub {PDL::Core::biop2(@_,"+")},       
     '-='     =>  sub {PDL::Core::biop2(@_,"-")},
     '*='     =>  sub {PDL::Core::biop2(@_,"*")},       
     '/='     =>  sub {PDL::Core::biop2(@_,"/")},
     '%='     =>  sub {PDL::Core::biop2(@_,"%")},       
     '**='    =>  sub {PDL::Core::biop2(@_,"**")},
     '<<='    =>  sub {PDL::Core::biop2(@_,"<<")},      
     '>>='    =>  sub {PDL::Core::biop2(@_,">>")},
     '&='     =>  sub {PDL::Core::biop2(@_,"&")},       
     '|='     =>  sub {PDL::Core::biop2(@_,"|")},
     '^='     =>  sub {PDL::Core::biop2(@_,"^")},
     '<=>'    =>  sub {PDL::Core::bifunc(@_,"SPACESHIP")}, 
     '**'     =>  sub {PDL::Core::bifunc(@_,"pow")},    
     'atan2'  =>  sub {PDL::Core::bifunc(@_,"atan2")}, 
     '%'      =>  sub {PDL::Core::bifunc(@_,"MODULO")}, 
     '!'      =>  sub {PDL::Core::ufunc($_[0],"!")}, 
     'sqrt'   =>  sub {PDL::Core::ufunc($_[0],"sqrt")}, 
     'sin'    =>  sub {PDL::Core::ufunc($_[0],"sin")},  
     'cos'    =>  sub {PDL::Core::ufunc($_[0],"cos")}, 
     'log'    =>  sub {PDL::Core::ufunc($_[0],"log")},  
     'exp'    =>  sub {PDL::Core::ufunc($_[0],"exp")}, 
     'abs'    =>  sub {PDL::Core::ufunc($_[0],"abs")},  
     "="      =>  sub {shift}, # Don't copy
     'x'      =>  \&PDL::Core::matrix_mult,
     '~'      =>  \&PDL::Core::transpose,               
     "\"\""   =>  \&PDL::Core::string
   ;
}

sub log10{ my $x = shift; my $y = log $x; $y /= log(10); return $y };

##################### Data type/conversion stuff ########################

# Data types/sizes (bytes) [must be in order of complexity] 

( $PDL_B, $PDL_S, $PDL_US, $PDL_L, $PDL_F, $PDL_D ) = (0..5); # Enum

@pack=("C*", "s*", "S*", "l*", "f*", "d*"); # Corresponding pack types

sub nelem {        # Return number of data elements in a PDL
    my $pdl = topdl($PDL::name,shift);
    return length($$pdl{Data})/howbig($$pdl{Datatype});
}

sub dims {  # Return dimensions as @list
   my $pdl = topdl ($PDL::name, shift);
   return @{$$pdl{Dims}};
}

################# Creation/copying functions #######################


# Create a new pdl variable, e.g.:
#
# $a = pdl 42;            # From scalar
# $a = pdl [1,2,3,4];     # From list
# $a = pdl ([1,2],[3,4]); # Ditto
# $a = pdl @x;            # Ditto
# $a = pdl $b;            # From another pdl (copy)

sub pdl { my $x = shift; return $x->new(@_) }

# Inheritable 'new' method for PDL objects

sub null {  # Special token for PDL::PP
   my $class = shift;
   my $new = bless {}, $class;
   $$new{Data}="";
   $$new{Dims}=[0];
   return $new;
}
  
sub PDL::new {         
   my $this = shift;
   return $this->copy if ref($this);
   my $new = bless {}, $this;
   $$new{Datatype} = $PDL_D;
   my $value = (scalar(@_)>1 ? [@_] : shift);  # ref thyself
   $value = 0 if !defined($value);
   if (ref(\$value) eq "SCALAR") { 
       $$new{Data}     = pack( $pack[$$new{Datatype}], $value ); 
       $$new{Dims}     = [];
       return $new;
   }
   elsif (ref($value) eq "ARRAY") { 
       $level = 0; @dims = (); # package vars
       $$new{Data}     = rpack($value); 
       $$new{Dims}     = [reverse @dims];
   }
   elsif (blessed($value)) { # Object 
       $new = $value->copy;
   }
   else {
       croak("Can not interpret argument $value of type ".ref($value) );
   }
   return $new;
}

# Inheritable copy method

sub PDL::copy { 
    my $value = shift;
    croak("Argument is an ".ref($value)." not an object") unless blessed($value);
    my $option  = shift;
    $option = "" if !defined $option;
    if ($$value{Inplace}) {   # Copy protection
       delete $$value{Inplace};
       return $value;
    }
    my $new = bless {}, ref($value);
    for ( grep($_ ne "Data", keys %$value) ) {   # Efficient to ignore Data here
          next if $_ eq "PDL";
          $$new{$_} = rcopyitem( $$value{$_} );  # Deep copy
    }
    $$new{Data} = $option eq "NoData" ? "" : $$value{Data}; # Special
    $new->flush;
    return $new;
}

# Utility to determine if argument is blessed object

sub blessed { 
    my $ref = ref(shift);
    return $ref =~ /^(REF|SCALAR|ARRAY|HASH|CODE|GLOB||)$/ ? 0 : 1;
} 
       
# Convert numbers to PDL if not already

sub topdl {      
    return $_[1] if blessed($_[1]); # Fall through
    return $_[0]->new($_[1]) if ref(\$_[1]) eq "SCALAR";
    croak("Can not convert a ".ref($_[1])." to a ".$_[0]);
0;}

# Convert everything to PDL if not blessed

sub alltopdl {    
    return $_[1] if blessed($_[1]); # Fall through
    return $_[0]->new($_[1]);
0;}

# Flag pdl for in-place operations

sub inplace {
    my $pdl = topdl($PDL::name,shift); $$pdl{Inplace}=1; return $pdl;
}

# Create zero filled array (function/inheritable constructor)

sub zeroes {
    my $class = shift; 
    my $nelems = 1; my @dims;
    for (@_) { 
       croak "Dimensions must be positive" if $_<=0;
       $nelems *= $_; push @dims, $_ 
    }
    my $pdl = bless {}, $class;
    $$pdl{Data}     = "\0"x($nelems*howbig($PDL_F));
    $$pdl{Datatype} = $PDL_F;
    $$pdl{Dims}     = [@dims];
    return $pdl;
} 

# Create one-filled array

sub ones { 
  croak 'Usage: $a = ones($nx, $ny, $nz ...) or PDL->ones(...)' if $#_<1;
  my $x = zeroes(@_); return ++$x 
}

# Create array filled with a sequence

sub sequence {
    croak 'Usage: $a = sequence($nx, $ny, $nz ...) or PDL->sequence(...)' if $#_<1;
    my $class = shift;  my @n = @_; my $n;
    my $nelem = 1; for $n (@n) { croak "Dims must be > 0\n" unless $n>0; $nelem *= $n}
    $pdl = zeroes($class,$nelem); 
    xvals(inplace($pdl)); $$pdl{Dims} = [@_];
    return $pdl;
} 

# Reshape PDL array

sub reshape {
   croak 'Usage reshape($a, $nx, $ny, $nz...)' if $#_<1;
   my $a  = topdl($PDL::name,shift); my @n = @_; my $n;
   my $nelem = 1; for $n (@n) { croak "Dims must be > 0\n" unless $n>0; $nelem *= $n}
   $nelem = ($nelem-nelem($a)) * howbig($$a{Datatype});
   $$a{Dims} = [@_];
   if ($nelem>=0) {
      $$a{Data}.="\0"x$nelem;       # Zero extend
   }else{
      $$a{Data} = substr($$a{Data},0,length($$a{Data})+$nelem); # Chop
   }
   $a->flush;
1;}

# type to type conversion functions (with automatic conversion to pdl vars)

sub byte   { convert(alltopdl($PDL::name,shift),$PDL_B) };
sub short  { convert(alltopdl($PDL::name,shift),$PDL_S) };
sub ushort { convert(alltopdl($PDL::name,shift),$PDL_US) };
sub long   { convert(alltopdl($PDL::name,shift),$PDL_L) };
sub float  { convert(alltopdl($PDL::name,shift),$PDL_F) };
sub double { convert(alltopdl($PDL::name,shift),$PDL_D) };


##################### Misc perl functions #################

# Return histogram - default is 100 bins if $step not specified
# Extremum values are in start/end bins

sub hist {
    croak('Usage: ([$xvals],$hist) = hist($data,[$min,$max,$step])') if $#_<0;
    my($data,$min,$max,$step)=@_;
    $min = min($data) unless defined $min;
    $max = max($data) unless defined $max;
    $step = ($max-$min)/100 unless defined $step;
    my $hist = hist_c($data,$min,$max,$step);
    my $xvals; $xvals = $min + sequence($PDL::name,nelem($hist))*$step + 
                        $step/2  if wantarray();
    return ($xvals,$hist);
}

# Simple statistics

sub stats {
    croak('Usage: ($mean,[$rms]) = stats($data,[$weights])') if $#_>1;
    my ($data,$weights) = @_;
    my ($mean,$rms);
    if ($#_==0) {
       $mean = sum($data)/nelem($data);
       return $mean unless wantarray;
       $rms = sqrt( sum( ($data-$mean)**2 )/nelem($data) );
    }
    else {
       $mean = sum($weights*$data)/sum($weights);
       return $mean unless wantarray;
       $rms = sqrt( sum( $weights*(($data-$mean)**2) )/sum($weights) );
    }
    return ($mean,$rms);
}

##################### Printing ####################


# New string routine

sub string { 
    my($self,$format)=@_;
    my $ndims = scalar(dims($self));
    return "Null" if $ndims==1 && $$self{Dims}[0]==0; # Null token
    if ($ndims==0) {
       my @x = unpack($pack[$$self{Datatype}], $$self{Data});
       return ($format ? sprintf($format, $x[0]) : "$x[0]");
    }
    local $sep  = $PDL::use_commas ? "," : " ";
    local $sep2 = $PDL::use_commas ? "," : "";
    if ($ndims==1) {
       return str1D($self,$format);
    }
    else{
       return strND($self,$format,0);
    }
}

############## Section/subsection functions ###################

sub list{ # pdl -> @list
     croak 'Usage: list($pdl)' if $#_!=0;
     my $pdl = topdl($PDL::name,shift);
     return unpack($pack[$$pdl{Datatype}], $$pdl{Data});
}

sub listindices{ # Return list of index values for 1D pdl
     croak 'Usage: list($pdl)' if $#_!=0;
     my $pdl = shift; 
     croak 'Not 1D' if scalar(dims($pdl)) != 1;
     return (0..nelem($pdl)-1);
}

sub sec{    # Subsection of object
    croak 'Usage: sec($pdl, $x1, $x2, $y1, $y2, ...)' if $#_<2;
    my $self = shift;
    sec_c ($self, [@_]);
}

sub ins{     # Insert - return new copy (can use inplace() though)
    croak 'Usage: $new = ins($old, $insertion, $x, $y, ...)' if $#_<2;
    my $self   = pdl($PDL::name,shift); my $insert = shift;
    insertin_c ($self, $insert, [@_]); # Insert in place
    return $self;
}

sub set{    # Sets a particular single value 
    croak 'Usage: set($pdl, $x, $y,.., $value)' if $#_<2;
    my $self  = shift; my $value = pop @_;
    set_c ($self, [@_], $value);
    return $self;
}

sub at{     # Return value at ($x,$y,$z...)
    croak 'Usage: at($pdl, $x, $y, ...)' if $#_<1;
    my $self = shift;
    at_c ($self, [@_]);
}

# Conveniently named interfaces to axisvals()

sub xvals{ axisvals(shift,0) };
sub yvals{ axisvals(shift,1) };
sub zvals{ axisvals(shift,2) };

sub rvals { # Return radial distance from center in N-dims
    my $x = topdl($PDL::name,shift);
    my $y = $x*0.0;
    my $i;
    for ($i=0; $i<scalar(@{$$x{Dims}}); $i++) {
        $y += (axisvals($x,$i)-int($$x{Dims}[$i]/2))**2;
    }
    return sqrt($y);
}


####################### Call external #########################

# Load a shareable image and call a symbol and pass PDL parameters
# to it

sub callext{
    die "Usage: callext(\$file,\$symbol, \@pdl_args)" if scalar(@_)<2;
    my($file,$symbol, @pdl_args) = @_;

    $libref = DynaLoader::dl_load_file($file);
    $err    = DynaLoader::dl_error(); croak $err unless $err eq "";
    $symref = DynaLoader::dl_find_symbol($libref, $symbol);
    $err    = DynaLoader::dl_error(); croak $err unless $err eq "";

    callext_c($symref, @pdl_args);
1;}


###################### Misc internal routines ####################


# Recursively pack an N-D array ref in format [[1,1,2],[2,2,3],[2,2,2]] etc
# package vars $level and @dims must be initialised first.

sub rpack {             
            
    my $a = shift;  my ($ret,$type);

    $ret = "";
    if (ref($a) eq "ARRAY") {

       if (defined($dims[$level])) {
           croak 'Array is not rectangular' unless $dims[$level] == scalar(@$a);
       }else{
          $dims[$level] = scalar(@$a);
       }
       $level++;

       $type = ref($$a[0]);
       for(@$a) { 
          croak 'Array is not rectangular' unless $type eq ref($_); # Equal types
          $ret .= rpack($_); $i++;
       }
       $level--;

    }elsif (ref(\$a) eq "SCALAR") { # Note $PDL_D assumed

      $ret = pack("d*",$_);
 
    }else{
        croak "Don't know how to make a PDL object from passed argument";
    }
    return $ret;
}

sub rcopyitem{        # Return a deep copy of an item - recursively
    my $x = shift; 
    my ($y, $key, $value);
    if (ref(\$x) eq "SCALAR") {
       return $x;
    }elsif (ref($x) eq "SCALAR") {
       $y = $$x; return \$y;
    }elsif (ref($x) eq "ARRAY") {
       $y = [];
       for (@$x) {
           push @$y, rcopyitem($_);
       }
       return $y;
    }elsif (ref($x) eq "HASH") {
       $y={};
       while (($key,$value) = each %$x) {
          $$y{$key} = rcopyitem($value);
       }
       return $y;
    }elsif (blessed($x)) { 
       return $x->copy;
    }else{
       croak ('Deep copy of object failed - unknown component with type '.ref($x));
    }
0;}

# N-D array stringifier

sub strND {
    my($self,$format,$level)=@_;
    my @dims = @{$$self{Dims}};
    if ($#dims==1) { # Return 2D string
       return str2D($self,$format,$level);
    }
    else { # Return list of (N-1)D strings
       my @sec = map {0,$_-1} @dims;
       my $ret="\n"." "x$level ."["; my $j;       
       for ($j=0; $j<$dims[$#dims]; $j++) {
           @sec[$#sec-1..$#sec]=($j,$j);

           my $x = sec($self,@sec);  # Subsection

           $$x{Dims} = [@dims];      # Correct for unit-chopping
           pop @{$$x{Dims}}; $self->flush;

           $ret .= strND($x,$format, $level+1); 
	   chop $ret; $ret .= $sep2;
       }
       chop $ret if $PDL::use_commas;
       $ret .= "\n" ." "x$level ."]\n";
       return $ret;
    }
}
	  

# String 1D array in nice format

sub str1D {
    my($self,$format)=@_;
    croak "Not 1D" if scalar(@{$$self{Dims}})!=1;
    my @x = unpack($pack[$$self{Datatype}], $$self{Data} );
    my ($ret,$dformat,$t);
    $ret = "[";
    $dformat = $PDL::floatformat  if $$self{Datatype} == $PDL_F;
    $dformat = $PDL::doubleformat if $$self{Datatype} == $PDL_D;
    for $t (@x) {
        if ($format) {
	  $t = sprintf $format,$t;
	}
	else{ # Default 
           if ($dformat && length($t)>7) { # Try smaller
             $t = sprintf $dformat,$t;
	   }
	}
       $ret .= $t.$sep;
    }
    chop $ret; $ret.="]";
    return $ret;
}

# String 2D array in nice uniform format

sub str2D{ 
    my($self,$format,$level)=@_;
    my @dims = @{$$self{Dims}};
    croak "Not 2D" if scalar(@dims)!=2;
    my @x = unpack($pack[$$self{Datatype}], $$self{Data} );
    my ($i, $f, $t, $len, $ret);

    if (!defined $format || $format eq "") { # Format not given? - 
                                             # find max length of default
       $len=0;
       for (@x) {$i = length($_); $len = $i>$len ? $i : $len };
       $format = "%".$len."s"; 
       
       if ($len>7) { # Too long? - perhaps try smaller format
          if ($$self{Datatype}==$PDL_F) {
	     $format = $PDL::floatformat;
	  }
	  elsif ($$self{Datatype}==$PDL_D) {
	     $format = $PDL::doubleformat;
	  }
	  else {
	     goto output; # Stick with default
	  }
       }
       else {
          goto output; # Default ok
       }
    } 

    # Find max length of strings in final format
    $len=0;
    for (@x) { 
       $i = length(sprintf $format,$_); $len = $i>$len ? $i : $len;
    }
       
    output:     # Generate output

    $ret = "\n" . " "x$level . "[\n";
    { my $level = $level+1;
      $ret .= " "x$level ."[";
      for ($i=0; $i<=$#x; $i++) { 
          $f = sprintf $format,$x[$i];
          $t = $len-length($f); $f = " "x$t .$f if $t>0;
          $ret .= $f;
	  if (($i+1)%$dims[0]) { 
	     $ret.=$sep;
          }
	  else{ # End of output line
	     $ret.="]";
	     if ($i==$#x) { # very last number
	        $ret.="\n";
	     }
	     else{
	        $ret.= $sep2."\n" . " "x$level ."[";
	     }
	  }
       }
    }
    $ret .= " "x$level."]\n";
    return $ret;
}

# Export routines with support for the 'OO' modifier and
# @EXPORT_STATIC list. Also exports all of @EXPORT_OK if no list
# specified (i.e. opposite default behaviour from builtin).

package PDL::Exporter;

use Exporter;


sub import {

   my $pkg = shift;
   my @exports = @_;
   my @revised_exports = ();
   local $^W=0;  # Supress redefining subroutines warning
   my ($e,$OO,$toeval);
   for $e (@exports) {
      $e eq "OO" ? $OO++ : push @revised_exports, $e;
   }
   @revised_exports = @{"${pkg}::EXPORT_OK"} if scalar(@revised_exports)==0;
   
   my $call = caller;

   if ($OO) {
      Exporter::export( $pkg, $PDL::name, @revised_exports );
   }
   else{
      @static = (); $toeval="";
      @{"${pkg}::EXPORT_FAIL"} = @{"${pkg}::EXPORT_STATIC"}; # Call back handle
      Exporter::export( $pkg, $call, @revised_exports );

      # Redefine the @EXPORT_STATIC list

      for $e (@static) { 
         $toeval .= "sub ${call}::$e { ${pkg}::$e ( \$PDL::name, \@_ ) }; ";
      }
      eval $toeval;
   }
}

sub export_fail {
   my $pkg = shift;
   @static = @_;    # Save static symbols list
   return ();       # Tell exporter still OK to export
}


;# Exit with OK status

1;
