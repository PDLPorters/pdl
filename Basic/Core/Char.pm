package PDL::Char;

@ISA = qw (PDL);
use overload ("\"\""   =>  \&PDL::Char::string);
use strict;
use vars ('$level', '@dims'); # Global Vars used


=head1 NAME

PDL::Char -- PDL subclass which allows reading and writing of fixed-length character strings as byte PDLs

=head1 SYNOPSIS

 use PDL;
 use PDL::Char;

 my $pchar = PDL::Char->new( [['abc', 'def', 'ghi'],['jkl', 'mno', 'pqr']] );
 
 $pchar->setstr(1,0,'foo');
 
 print $pchar; # 'string' bound to "", perl stringify function
 # Prints:
 # [
 #  ['abc' 'foo' 'ghi']
 #  ['jkl' 'mno' 'pqr']
 # ]

 print $pchar->atstr(2,0);
 # Prints:
 # ghi

=head1 DESCRIPTION

This subclass of PDL allows one to manipulate PDLs of 'byte' type as if they were made of fixed
length strings, not just numbers.

This type of behavior is useful when you want to work with charactar grids.  The indexing is done
on a string level and not a character level for the 'setstr' and 'atstr' commands.  

This module is in particular useful for writing NetCDF files that include character data using the
PDL::NetCDF module.

=head1 FUNCTIONS

=head2 new

=for ref

Function to create a byte PDL from a string, list of strings, list of list of strings, etc.

=for usage

 # create a new PDL::Char from a perl array of strings
 $strpdl = PDL::Char->new( ['abc', 'def', 'ghij'] );  

 # Convert a PDL of type 'byte' to a PDL::Char
 $strpdl1 = PDL::Char->new (sequence (byte, 4, 5)+99);

=for example

 $pdlchar3d = PDL::Char->new([['abc','def','ghi'],['jkl', 'mno', 'pqr']]); 

=cut


sub new {		
  my $type = shift;
  my $value = (scalar(@_)>1 ? [@_] : shift);  # ref thyself

  # re-bless byte PDLs as PDL::Char
  if (ref($value) =~ /PDL/) {
    PDL::Core::barf('Cannot convert a non-byte PDL to PDL::Char')
      if ($value->get_datatype != $PDL::Types::PDL_B);
    return bless $value, $type;
  }

  my $ptype = $PDL::Types::PDL_B;
  my $self  = PDL->initialize();
  $self->set_datatype($ptype);
  $value = 0 if !defined($value);
  $level = 0; @dims = (); # package vars
  my $maxlength;      # max length seen for all character strings
  my $samelen	= 1;  # Flag = 1 if all character strings are the same length

  # 1st Pass thru the perl array structure, assume all strings the same length
  my $str = _rcharpack($value,\$maxlength,\$samelen);
  unless( $samelen){  # Strings weren't the same length, go thru again and null pad to
	      	     # the max length.
	$str = _rcharpack2($value,$maxlength);
  }
  $self->setdims([reverse @dims]);
  ${$self->get_dataref} = $str;
  $self->upd_data();
  return bless $self, $type;
}
				
# Take an N-D perl array of strings and pack it into a single string, 
# updating the $level and @dims package vars on the way.  
# Used by the 'char' constructor
#
#  References supplied so $maxlength and $samelen are updated along the way as well.
#    
#    
#   This version (_rcharpack) is for the 1st pass thru the N-d string array.
#    It assumes that all strings are the same length, but also checks to see if they aren't
sub _rcharpack {

  my $a = shift;		     # Input string
  my ($maxlenref, $samelenref) = @_; # reference to $maxlength, $samelen

  my ($ret,$type);
  
  $ret = "";
  if (ref($a) eq "ARRAY") {

    PDL::Core::barf('Array is not rectangular') if (defined($dims[$level]) and 
					$dims[$level] != scalar(@$a));
    $dims[$level] = scalar (@$a);
    $level++;
    
    $type = ref($$a[0]);
    for(@$a) {
      PDL::Core::barf('Array is not rectangular') unless $type eq ref($_); # Equal types
      $ret .= _rcharpack($_,$maxlenref, $samelenref);
    }
    
    $level--;
    
  }elsif (ref(\$a) eq "SCALAR") { 
    my $len = length($a);

    # Check for this length being different then the others:
    $$samelenref = 0 if( defined($$maxlenref) && ($len != $$maxlenref) );
    # Save the max length:
    $$maxlenref = $len if( !defined($$maxlenref) || $len > $$maxlenref); # see if this is the max length seen so far

    $dims[$level] = $len;
    $ret = $a;
    
  }else{
    PDL::Core::barf("Don't know how to make a PDL object from passed argument");
  }
  return $ret;
}				
#
#    
#   This version (_rcharpack2) is for the 2nd pass (if required) thru the N-d string array.
#   If the 1st pass thru (_rcharpack) finds that all strings were not the same length, 
#   this routine will go thru and null-pad all strings to the max length seen.
#     Note: For efficiency, the error checking is not repeated here, because any errors will
#       already be detected in the 1st pass.
#
sub _rcharpack2 {

  my $a = shift;		  # Input string
  my ($maxlen) = @_; 		  # Length to pad strings to

  my ($ret,$type);
  
  $ret = "";
  if (ref($a) eq "ARRAY") {

    #  Checks not needed the second time thru (removed)

    $dims[$level] = scalar (@$a);
    $level++;
    
    $type = ref($$a[0]);
    for(@$a) {
      $ret .= _rcharpack2($_,$maxlen);
    }
    
    $level--;
    
  }elsif (ref(\$a) eq "SCALAR") { 
    my $len = length($a);

    $dims[$level] = $maxlen;
    $ret = $a.("\00" x ($maxlen - $len));
  }
  return $ret;
}


#
#

=head2 string

=for ref

Function to print a character PDL (created by 'char') in a pretty format.

=for usage

 $char = PDL::Char->new( [['abc', 'def', 'ghi'], ['jkl', 'mno', 'pqr']] );
 print $char; # 'string' bound to "", perl stringify function
 # Prints:
 # [
 #  ['abc' 'def' 'ghi']
 #  ['jkl' 'mno' 'pqr']
 # ]

 # 'string' is overloaded to the "" operator, so:
 # print $char;
 # should have the same effect.

=cut

sub string {		
  my $self   = shift;
  my $level  = shift || 0;

  my $sep = $PDL::use_commas ? "," : " ";

  if ($self->dims == 1) {
    my $str = ${$self->get_dataref}; # get copy of string
    $str =~ s/\00+$//g; # get rid of any null padding
    return "\'". $str. "\'". $sep;
  } else {
    my @dims = reverse $self->dims;
    my $ret = '';
    $ret .= (" " x $level) . '[' . ((@dims == 2) ? ' ' : "\n");
    for (my $i=0;$i<$dims[0];$i++) {
      my $slicestr = ":," x (scalar(@dims)-1) . "($i)";
      my $substr = $self->slice($slicestr);
      $ret .= $substr->string($level+1);
    }
    $ret .= (" " x $level) . ']' . $sep . "\n";
    return $ret;
  }
				
}


=head2 setstr

=for ref

Function to set one string value in a character PDL.  The input position is 
the position of the string, not a character in the string.  The first dimension
is assumed to be the length of the string.  

The input string will be null-padded if the string is shorter than the first
dimension of the PDL.  It will be truncated if it is longer.

=for usage

 $char = PDL::Char->new( [['abc', 'def', 'ghi'], ['jkl', 'mno', 'pqr']] );
 $char->setstr(0,1, 'foobar');
 print $char; # 'string' bound to "", perl stringify function
 # Prints:
 # [
 #  ['abc' 'def' 'ghi']
 #  ['foo' 'mno' 'pqr']
 # ]
 $char->setstr(2,1, 'f');
 print $char; # 'string' bound to "", perl stringify function
 # Prints:
 # [
 #  ['abc' 'def' 'ghi']
 #  ['foo' 'mno' 'f']      -> note that this 'f' is stored "f\0\0"
 # ]

=cut

sub setstr {    # Sets a particular single value to a string.
  PDL::Core::barf('Usage: setstr($pdl, $x, $y,.., $value)') if $#_<2;
  my $self = shift;
  my $val  = pop;

  my @dims = $self->dims;
  my $n    = $dims[0];

  for (my $i=0;$i<$n;$i++) {
    my $chr = ($i >= length($val)) ? 0 : unpack ("C", substr ($val, $i, 1));
    PDL::Core::set_c ($self, [$i, @_], $chr);
  }
  
}

=head2 atstr

=for ref

Function to fetch one string value from a PDL::Char type PDL, given a position within the PDL.
The input position of the string, not a character in the string.  The length of the input
string is the implied first dimension.

=for usage

 $char = PDL::Char->new( [['abc', 'def', 'ghi'], ['jkl', 'mno', 'pqr']] );
 print $char->atstr(0,1);
 # Prints:
 # jkl

=cut

sub atstr {    # Fetchs a string value from a PDL::Char
  PDL::Core::barf('Usage: atstr($pdl, $x, $y,..,)') if (@_ < 2);
  my $self = shift;
  
  my $str = ':,' . join (',', map {"($_)"} @_);
  my $a = $self->slice($str);
  
  my $val = ${$a->get_dataref}; # get the data
  $val =~ s/\00+$//g; # get rid of any null padding
  return $val;
}

# yuck ;) this is a cool little accessor method
# rebless a slice into PDL; originally
# Marc's idea used in PDL::Complex
sub numeric {
  my ($seq) = @_;
  return bless $seq->slice(''), 'PDL';
}

1;
