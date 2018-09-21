=head1 NAME

PDL::Matrix -- a convenience matrix class for column-major access

=head1 VERSION

This document refers to version PDL::Matrix 0.5 of PDL::Matrix

=head1 SYNOPSIS

  use PDL::Matrix;

  $m = mpdl [[1,2,3],[4,5,6]];
  $m = PDL::Matrix->pdl([[1,2,3],[4,5,6]]);
  $m = msequence(4,3);
  @dimsa = $x->mdims; # 'dims' is not overloaded

  $v = vpdl [0,1,2,3]
  $v = vzeroes(4);

=head1 DESCRIPTION

=head2 Overview

This package tries to help people who want to use PDL for 2D matrix
computation with lots of indexing involved. It provides a PDL
subclass so one- and two-dimensional piddles that are used as
vectors resp and matrices can be typed in using traditional matrix
convention.

If you want to know more about matrix operation support in PDL, you 
want to read L<PDL::MatrixOps> or L<PDL::Slatec>.

The original pdl class refers to the first index as the first row,
the second index as the first column of a matrix. Consider

  print $B = sequence(3,2)
  [
   [0 1 2]
   [3 4 5]
  ]

which gives a 2x3 matrix in terms of the matrix convention, but the
constructor used (3,2). This might get more confusing when using
slices like sequence(3,2)->slice("1:2,(0)") : with traditional
matrix convention one would expect [2 4] instead of [1 2].

This subclass PDL::Matrix overloads the constructors and indexing
functions of pdls so that they are compatible with the usual matrix
convention, where the first dimension refers to the row of a
matrix. So now, the above example would be written as

  print $B = PDL::Matrix->sequence(3,2) # or $B = msequence(3,2)
  [
   [0 1]
   [2 3]
   [4 5]
  ]

Routines like L<eigens|PDL::MatrixOps/eigens> or
L<inv|PDL::MatrixOps/inv> can be used without any changes.

Furthermore one can construct and use vectors as n x 1 matrices
without mentioning the second index '1'.

=head2 Implementation

C<PDL::Matrix> works by overloading a number of PDL constructors
and methods such that first and second args (corresponding to
first and second dims of corresponding matrices) are effectively swapped.
It is not yet clear if PDL::Matrix achieves a consistent column-major 
look-and-feel in this way.

=head1 NOTES

As of version 0.5 (rewrite by CED) the matrices are stored in the usual
way, just constructed and stringified differently.  That way indexing 
and everything else works the way you think it should.

=head1 FUNCTIONS

=cut

package PDL::Matrix;

@EXPORT_OK = ();


#use PDL::Core;
#use PDL::Slatec;
use PDL::Exporter;
use Carp;

@ISA = qw/PDL::Exporter PDL/;

our $VERSION = "0.5";
$VERSION = eval $VERSION;

#######################################################################=
#########
#
# overloads

use overload( '""' => \&string,
              'x'  => sub {my $foo = $_[0]->null();
                           &PDL::Primitive::matmult(@_[1,0],$foo); 
                           $foo;}
            );

sub string {
    my ($me,@a) = shift;
    return $me->SUPER::string(@a) unless($me->ndims > 0);
    $me = $me->dummy(1,1) unless($me->ndims > 1);
    $me->xchg(0,1)->SUPER::string(@a);
}


# --------> constructors

=head2 mpdl, PDL::Matrix::pdl

=for ref

constructs an object of class PDL::Matrix which is a piddle child class.

=for example

    $m = mpdl [[1,2,3],[4,5,6]];
    $m = PDL::Matrix->pdl([[1,2,3],[4,5,6]]);

=cut

sub pdl {
  my $class = shift;
  my $pdl = $class->SUPER::pdl(@_);
  if($pdl->ndims > 0) {
      $pdl = $pdl->dummy(1,1) unless $pdl->ndims > 1;
      $pdl = $pdl->xchg(0,1);
  }
  bless $pdl, ref $class || $class;
}

=head2 mzeroes, mones, msequence

=for ref

constructs a PDL::Matrix object similar to the piddle constructors
zeroes, ones, sequence.

=cut

for my $func (qw /pdl zeroes ones sequence dims/) {
  push @EXPORT_OK, "m$func";
  eval " sub m$func { PDL::Matrix->$func(\@_) }; ";
}

=head2 vpdl 

=for ref

constructs an object of class PDL::Matrix which is of matrix
dimensions (n x 1)

=for example

    print $v = vpdl [0,1];
    [
     [0]
     [1]
    ]

=cut 

sub vpdl {
  my $pdl = PDL->pdl(@_);
  bless $pdl, PDL::Matrix;
}
push @EXPORT_OK, "vpdl";

=head2 vzeroes, vones, vsequence

=for ref

constructs a PDL::Matrix object with matrix dimensions (n x 1),
therefore only the first scalar argument is used.

=for example

    print $v = vsequence(2);
    [
     [0]
     [1]
    ]

=cut

for my $func (qw /zeroes ones sequence/) {
  push @EXPORT_OK, "v$func";
  my $code = << "EOE";

sub v$func {
  my \@arg = \@_;
  ref(\$arg[0]) ne 'PDL::Type' ? (\@arg = (\$arg[0],1)) :
                                 (\@arg = (\$arg[0],\$arg[1],1));
  PDL::Matrix->$func(\@arg);
}

EOE
# print "evaluating $code\n";
  eval $code;
}



eval "use PDL::Slatec";

my $has_slatec = ($@ ? 0 : 1);
sub inv {
  my $self = shift;
  croak "inv: PDL::Slatec not available" unless $has_slatec;
  return $self->matinv;
}

=head2 kroneckerproduct

=for ref

returns kroneckerproduct of two matrices. This is not efficiently
implemented.

=for example
print kroneckerproduct(msequence(2,2),mones(2,2))
[
 [0 0 1 1]
 [0 0 1 1]
 [2 2 3 3]
 [2 2 3 3]
]

=cut

# returns kroneckerproduct of two matrices
sub kroneckerproduct {
  my @arg = @_;
  
  my ($r0,$c0) = $arg[0]->mdims;
  my ($r1,$c1) = $arg[1]->mdims;
  
  my $out = mzeroes($r0*$r1,$c0*$c1);
  
  for (my $i=0;$i<$r0;$i++) {
    for (my $j=0;$j<$c0;$j++) {
      ($_ = $out->slice(($i*$r1).":".(($i+1)*$r1-1).",".
			($j*$c1).":".(($j+1)*$c1-1)) ) .= $arg[0]->at($i,$j) * $arg[1];
    }
  }
  
  return $out;
}
push @EXPORT_OK, "kroneckerproduct";

sub rotate {
  my ($self,@args) = @_;
  return $self->transpose->SUPER::rotate(@args)->transpose;
}


sub msumover {
  my ($mpdl) = @_;
  return PDL::sumover(transpose($mpdl)->xchg(0,2));
}
push @EXPORT_OK, "msumover";


=head2 det_general

=for ref

returns a generalized determinant of a matrix. If the matrix is not
regular, one can specify the rank of the matrix and the corresponding
subdeterminant is returned. This is implemented using the C<eigens>
function.

=for example
print msequence(3,3)->determinant(2) # determinant of 
                                     # regular 2x2 submatrix
-24

=cut

# 
sub det_general {
  my ($mpdl,$rank) = @_;
  my $eigenvalues = (PDL::Math::eigens($mpdl))[1];
  my @sort = list(PDL::Ufunc::qsorti(abs($eigenvalues)));
  $eigenvalues = $eigenvalues->dice([@sort[-$rank..-1]]);
  PDL::Ufunc::dprod($eigenvalues);
}

=head2 trace

=for ref

returns the trace of a matrix (sum of diagonals)

=cut

sub trace {
  my ($mpdl) = @_;
  $mpdl->diagonal(0,1)->sum;
}

# this has to be overloaded so that the PDL::slice
# is called and not PDL::Matrix::slice :-(
sub dummy($$;$) {
   my ($pdl,$dim) = @_;
   $dim = $pdl->getndims+1+$dim if $dim < 0;
   barf ("too high/low dimension in call to dummy, allowed min/max=0/"
  . $_[0]->getndims)
     if $dim>$pdl->getndims || $dim < 0;
   $_[2] = 1 if ($#_ < 2);
   $pdl->PDL::slice((','x$dim)."*$_[2]");
}


# now some of my very own helper functions...
# stupid function to print a PDL::Matrix object in Maple code
sub stringifymaple {
  my ($self,@args) = @_;

  my ($dimR,$dimC) = mdims($self);
  my $s;

  $s .= $args[0].":=" unless $args[0] eq "";
  if (defined($dimR)) {
    $s .= "matrix($dimR,$dimC,[";
    for(my $i=0;$i<$dimR;++$i) {
      $s .= "[";
      for(my $j=0;$j<$dimC;++$j) {
	$s .= $self->at($i,$j);
	$s .= "," if $j+1<$dimC;
      }
      $s .= "]";
      $s .= "," if $i+1<$dimR;
    }
    $s .= "])";
  }
  else {
    $s = "vector($dimC,[";
    for(my $i=0;$i<$dimC;++$i) {
      $s .= $self->at($i);
      $s .= "," if $i+1<$dimC;
    }
    $s .= "])";
  }
  return $s;
}
sub printmaple {
  print stringifymaple(@_).";\n";
}

# stupid function to print a PDL::Matrix object in (La)TeX code
sub stringifyTeX {
  my ($self,@args) = @_;

  my ($dimR,$dimC) = mdims($self);
  my $s;

  $s .= $args[0]."=" unless $args[0] eq "";
  $s .= "\\begin{pmatrix}\n";
  for(my $i=0;$i<$dimR;++$i) {
    for(my $j=0;$j<$dimC;++$j) {
      $s .= $self->at($i,$j);
      $s .= " & " if $j+1<$dimC;
    }
    $s .= " \\\\ \n" if $i+1<$dimR;
  }
  $s .= "\n \\end{pmatrix}\n";

  return $s;
}

sub printTeX {
  print stringifyTeX(@_)."\n";
}

=pod 

=begin comment

DAL commented this out 17-June-2008. It didn't work, it used the
outmoded (and incorrect) ~-is-transpose convention, and it wasn't
necessary since the regular cross product worked fine.

=head2  vcrossp, PDL::Matrix::crossp

=for ref

similar to PDL::crossp, however reflecting PDL::Matrix notations

#=cut

# crossp for my special vectors
sub crossp {
  my ($pdl1,$pdl2) = @_;
  return PDL::transpose(PDL::crossp(~$pdl1,~$pdl2));
}
sub vcrossp { PDL::Matrix->crossp(\@_) }
push @EXPORT_OK, "vcrossp";

=end comment

=cut

%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

1;

=head1 BUGS AND PROBLEMS

Because we change the way piddles are constructed, not all pdl
operators may be applied to piddle-matrices. The inner product is not
redefined. We might have missed some functions/methods. Internal
consistency of our approach needs yet to be established.

Because PDL::Matrix changes the way slicing behaves, it breaks many
operators, notably those in MatrixOps.  

=head1 TODO

check all PDL functions, benchmarks, optimization, lots of other things ...

=head1 AUTHOR(S)

Stephan Heuel (stephan@heuel.org), Christian Soeller
(c.soeller@auckland.ac.nz).

=head1 COPYRIGHT

All rights reserved. There is no warranty. You are allowed to
redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution, the
copyright notice should be included in the file.

=cut
