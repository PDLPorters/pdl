=head1 Name

MatrixOps -- Matrix operations for PDLs

=head1 SYNOPSIS

   $inv = $a->inv;

   $det = $a->det;

   ($lu,$perm,$par) = $a->lu_decomp;
   $x = lu_backsub($lu,$perm,$b); # solve $a x $x = $b

=head1 DESCRIPTION

PDL::MatrixOps contains a bunch of operations for handling matrices --
LU decomposition, inversion, determinant, etc.  Except as noted, the
matrices are treated as indexed in the normal (column,row) order --
that is to say (x,y) coordinates; this agrees with normal PDL indexing
but is the opposite of mathematical matrix convention.  The matrices
appear correctly when printed.  It should work OK with
L<PDL::Matrix|PDL::Matrix> objects -- which just act like normal PDLs.

There's some duplication with PDL::Slatec (TIMTOWTDI), but with the
benefit that FORTRAN isn't required.

=head1 NOTES

This is intended as a general-purpose linear algebra package to replace
PDL::Slatec and bits of PDL::Math.  If there's something you want that
isn't here, please add and document it!

=head1 TO DO

The following things need to be added:

=over 3

=item * Gaussian elimination

=item * Eigenvalues (from NR or from PDL::Math?)

=item * SVD (from NR or PDL::Math or PDL::Slatec?)

=back

The following other things need doing:

=over 3

=item * simplified highlevel interfaces:

C<solve> with default and options for other backend methods (e.g. LU,
SVD, etc).

others?

=item * Make a MatrixOps::Slatec subclass

=item * Link into GSL?

=back

=head1 METHODS

=cut

BEGIN{
use Exporter ();

package PDL::MatrixOps;
$VERSION = "0.5 (2-Nov-2002)";

@ISA = ('PDL','Exporter');
@EXPORT_OK = qw( lu_decomp lu_backsub inv det determinant );
@EXPORTS =   @EXPORT_OK;
%EXPORT_TAGS = (Func=>\@EXPORT_OK);

use PDL;
use PDL::Exporter;
use Carp;
}

use strict;

######################################################################

=head2 lu_decomp

=for sig

  Signature: (a(m,m); [o]b(n); [o]c; [o]lu)

=for ref

LU decompose a matrix, with row permutation

=for usage

  ($lu, $perm, $parity) = lu_decomp($a);

  $lu = lu_decomp($a, $perm, $par);  # $perm and $par are outputs!

  lu_decomp($a->inplace,$perm,$par); # Everything in place.

=for description

lu_decomp returns an LU decomposition of a square matrix, using
Crout's method with partial pivoting.  It's ported from I<Numerical
Recipes>.  The partial pivoting keeps it numerically stable but
defeats efficient threading, so if you have a few matrices to
decompose accurately, you should use lu_decomp, but if you have a
million matrices to decompose and don't mind a higher error budget you
probably want to use L<lu_decomp2|lu_decomp2>, which doesn't do the
pivoting (and hence gives wrong answers for near-singular or large
matrices), but does do threading.

lu_decomp decomposes the input matrix into matrices L and U such that
LU = A, L is a subdiagonal matrix, and U is a superdiagonal matrix.
By convention, the diagonal of L is all 1's.  

The single output matrix contains all the variable elements of both
the L and U matrices, stacked together.  Because the method uses
pivoting (rearranging the lower part of the matrix for better
numerical stability), you have to permute input vectors before applying
the L and U matrices.  The permutation is returned either in the
second argument or, in list context, as the second element of the
list.  You need the permutation for the output to make any sense, so 
be sure to get it one way or the other.

LU decomposition is the answer to a lot of matrix questions, including
inversion and determinant-finding, and lu_decomp is used by
L<inverse|inverse>.

If you pass in $perm and $parity, they either must be predeclared PDLs
of the correct size ($perm is an n-vector, $parity is a scalar) or
scalars.

If the matrix is singular, then the LU decomposition might not be
defined; in those cases, lu_decomp silently returns undef.  Some 
singular matrices LU-decompose just fine, and those are handled OK but
give a zero determinant (and hence can't be inverted).

lu_decomp uses pivoting, which rearranges the values in the matrix for
more numerical stability.  This makes it really good for large and
even near-singular matrices, but makes it unable to properly vectorize
threaded operation.  If you have a LOT of small matrices to
invert (like, say, a 3x3x1000000 PDL) you should use L<lu_decomp2>,
which doesn't pivot and is therefore threadable (and, of course, works
in-place).

If you ask lu_decomp to thread (by having a nontrivial third dimension
in the matrix) then it will call lu_decomp2 instead.  That is a
numerically unstable (non-pivoting) method that is mainly useful for
smallish, not-so-singular matrices but is threadable.

=cut

*PDL::lu_decomp = \&lu_decomp;

sub lu_decomp {
  my($in) = shift;
  my($permute) = shift;
  my($parity) = shift;
  my($sing_ok) = shift;

  my $TINY = 1e-30;
  
  barf('lu_decomp requires a square (2D) PDL\n')
    if(!UNIVERSAL::isa($in,'PDL') || 
       $in->ndims < 2 || 
       $in->dim(0) != $in->dim(1));

  if(($in->ndims > 2) && !( (pdl($in->dims)->slice("2:-1") < 2)->all )  ) {
    warn('lu_decomp: calling lu_decomp2 for threadability') 
      if($PDL::debug);
    return lu_decomp2($in,$permute,$parity,$sing_ok);
  }
  
  my($n) = $in->dim(0);
  my($n1) = $n; $n1--;

  my($inplace) = $in->is_inplace;
  my($out) = ($inplace) ? $in : $in->copy;


  if(defined $permute) {
    barf('lu_decomp: permutation vector must match the matrix')
      if(!UNIVERSAL::isa($permute,'PDL') || 
	 $permute->ndims != 1 || 
	 $permute->dim(0) != $out->dim(0));
    $permute .= xvals($in->dim(0));
  } else {
    $permute = xvals($in->dim(0));
  }

  if(defined $parity) {
    barf('lu_decomp: parity must be a scalar PDL') 
      if(!UNIVERSAL::isa($parity,'PDL') ||
	 $parity->nelem != 1);
    $parity .= 1.0;
  } else {
    $parity = pdl(1.0);
  }
  
  my($scales) = $in->abs->maximum; # elementwise by rows
  
  if(($scales==0)->sum) {
    return undef;
  }

  # Some holding tanks
  my($tmprow) = zeroes(double(),$out->dim(0)); 
  my($tmpval) = pdl(0.0);
  
  my($out_diag) = $out->diagonal(0,1);
  
  my($col,$row);
  for $col(0..$n1) {       
    for $row(1..$n1) {   
      my($klim) = $row<$col ? $row : $col;
      if($klim > 0) {
	$klim--;
	my($el) = $out->index2d($col,$row);
	$el -= ( $out->slice("($col),0:$klim") *
		 $out->slice("0:$klim,($row)") )->sum;
      }

    }
    
    # Figure a_ij, with pivoting
    
    if($col < $n1) {
      # Find the maximum value in the rest of the row
      my $sl = $out->slice("($col),$col:$n1");
      my $wh = $sl->maximum_ind;
      my $big = $sl->index($wh)->sever;
      
      # Permute if necessary to make the diagonal the maximum
      if($wh != 0) {  # Permute rows to place maximum element on diagonal.
	my $whc = $wh+$col;
	
	my $sl1 = $out->slice(":,($whc)");
	my $sl2 = $out->slice(":,($col)");
	$tmprow .= $sl1;  $sl1 .= $sl2;  $sl2 .= $tmprow;
	
	$sl1 = $permute->index($whc);
	$sl2 = $permute->index($col);
	$tmpval .= $sl1; $sl1 .= $sl2; $sl2 .= $tmpval;
	
	$parity *= -1.0;
      }

      # Sidestep near-singularity (NR does this; not sure if it is helpful)
      $big = $TINY * (1.0 - 2.0*($big < 0))
	if(abs($big) < $TINY);
      
      # Divide by the diagonal element (which is now the largest element)
      $out->slice("($col),".($col+1).":$n1") /= $big;
    } # end of pivoting part
  } # end of column loop

  if(wantarray) {
    return ($out,$permute,$parity);
  }
  $out;
}

######################################################################

=head2 lu_decomp2

=for sig

 Signature: (a(m,m); [0]lu(n)

=for ref

LU decompose a matrix, with no row permutation (threadable!)

=for usage

  ($lu, $perm, $parity) = lu_decomp2($a);

  $lu = lu_decomp2($a,[$perm,$par]); 

  lu_decomp($a->inplace,[$perm,$par]);

=for description

C<lu_decomp2> works just like L<lu_decomp|lu_decomp>, but it does no
pivoting at all and hence can be usefully threaded.  For compatibility
with L<lu_decomp|lu_decomp>, it will give you a permutation list and a parity
scalar if you ask for them -- but they are always trivial.

Because C<lu_decomp2> does not pivot, it is numerically unstable --
that means it is less precise than L<lu_decomp>, particularly for
large or near-singular matrices.  On the other hand, if you want 
to invert rapidly a few x 10^5 2x2 or 3x3 matrices, it's just the ticket.

The output is a single matrix that contains the LU decomposition of $a;
you can even do it in-place, thereby destroying $a, if you want.  See
L<lu_decomp> for more information about LU decomposition. 

=cut

*PDL::lu_decomp2 = \&lu_decomp2;

sub lu_decomp2 {
  my($in) = shift;
  my($perm) = shift;
  my($par) = shift;

  my($sing_ok) = shift;

  my $TINY = 1e-30;
  
  barf('lu_decomp2 requires a square (2D) PDL\n')
    if(!UNIVERSAL::isa($in,'PDL') || 
       $in->ndims < 2 || 
       $in->dim(0) != $in->dim(1));
  
  my($n) = $in->dim(0);
  my($n1) = $n; $n1--;

  my($inplace) = $in->is_inplace;
  my($out) = ($inplace) ? $in : $in->copy;


  if(defined $perm) {
    barf('lu_decomp2: permutation vector must match the matrix')
      if(!UNIVERSAL::isa($perm,'PDL') || 
	 $perm->ndims != 1 || 
	 $perm->dim(0) != $out->dim(0));
    $perm .= xvals($in->dim(0));
  } else {
    $perm = xvals($in->dim(0));
  }

  if(defined $par) {
    barf('lu_decomp: parity must be a scalar PDL') 
      if(!UNIVERSAL::isa($par,'PDL') ||
	 $par->nelem != 1);
    $par .= 1.0;
  } else {
    $par = pdl(1.0);
  }

  my $diagonal = $out->diagonal(0,1);

  my($col,$row);
  for $col(0..$n1) {       
    for $row(1..$n1) {   
      my($klim) = $row<$col ? $row : $col;
      if($klim > 0) {
	$klim--;
	my($el) = $out->index2d($col,$row);

	$el -= ( $out->slice("($col),0:$klim") *
		 $out->slice("0:$klim,($row)") )->sumover;
      }

    }
    
    # Figure a_ij, with no pivoting
    if($col < $n1) {
      # Divide the rest of the column by the diagonal element 
      $out->slice("($col),".($col+1).":$n1") /= $diagonal->index($col)->dummy(0,$n1-$col);
    }

  } # end of column loop

  if(wantarray) {
    return ($out,$perm,$par);
  }
  $out;
}

######################################################################

=head2 lu_backsub

=for sig

 Signature: (lu(m,m); perm(m); b(m))

=for ref

Solve A X = B for matrix A, by back substitution into A's LU decomposition.

=for usage

  ($lu,$perm) = lu_decomp($a);
  $x = lu_backsub($lu,$perm,$par,$b);
  
  lu_backsub($lu,$perm,$b->inplace); # modify $b in-place

  $x = lu_backsub(lu_decomp($a),$b); # (ignores parity value from lu_decomp)

=for description

Given the LU decomposition of a square matrix (from L<lu_decomp|lu_decomp>),
lu_backsub does back substitution into the matrix to solve C<A X = B> for 
given vector C<B>.  It's separated from the lu_decomp method so that you can
call the cheap lu_backsub multiple times and not have to do the expensive
LU decomposition more than once.  

lu_backsub acts on single vectors and threads in the usual way, which
means that it treats C<$b> as the I<transpose> of the input.  If you
want to process a matrix, you must hand in the I<transpose> of the
matrix, and then transpose the output when you get it back.  That's
because PDLs are indexed by (col,row), and matrices are (row,column)
by convention, so a 1-D PDL corresponds to a row vector, not a column
vector.

If C<$lu> is dense and you have more than a few points to solve for,
it's probably cheaper to find C<A^-1> with L<inverse|inverse>, and
just multiply C<X = A^-1 B>.)  In fact, L<inverse|inverse> works by
calling lu_backsub with the identity matrix.

lu_backsub is ported from Section 2.3 of I<Numerical Recipes>.


=cut

*PDL::lu_backsub = \&lu_backsub;
sub lu_backsub {
  my ($lu, $perm, $b, $par);
  if(@_==3) {
    ($lu, $perm, $b) = @_;
  } elsif(@_==4) {
    ($lu, $perm, $par, $b) = @_;
  } 
  
  barf("lu_backsub: LU decomposition is undef -- probably from a singular matrix.\n")
    unless defined($lu);

  barf("Usage: \$x = lu_backsub(\$lu,\$perm,\$b); all must be PDLs\n") 
    unless(UNIVERSAL::isa($lu,'PDL') &&
	 UNIVERSAL::isa($perm,'PDL') &&
	   UNIVERSAL::isa($b,'PDL'));

  my $n = $b->dim(0);
  my $n1 = $n; $n1--;


  # Permute the vector and make a copy if necessary.
  my $out;
  my $nontrivial = !(($perm==xvals($perm))->all);
  if($nontrivial) {
    if($b->is_inplace) {
      $b .= $b->dummy(1,$b->dim(0))->index($perm)->sever;
      $out = $b;
    } else {
      $out = $b->dummy(1,$b->dim(0))->index($perm)->sever;
    }
  } else {
    $out = ($b->is_inplace ? $b : $b->copy);
  }

  # Make sure threading over lu happens OK...

  while($out->ndims < $lu->ndims) {
    $out = $out->dummy(-1,$lu->dim($out->ndims));
  }

  ## Do forward substitution into L
  my $row; my $r1;

  for $row(1..$n1) {
    $r1 = $row-1;
    $out->index($row) -= ($lu->slice("0:$r1,$row") * 
		       $out->slice("0:$r1")
		       )->sumover;
  }

  ## Do backward substitution into U, and normalize by the diagonal
  my $ludiag = $lu->diagonal(0,1)->dummy(1,$n);
  $out->index($n1) /= $ludiag->index($n1);

  for ($row=$n1; $row>0; $row--) {
    $r1 = $row-1;
    $out->index($r1) -= ($lu->slice("$row:$n1,$r1") * 
			$out->slice("$row:$n1")
			)->sumover;
    $out->index($r1) /= $ludiag->index($r1);
  }

  $out;
}

######################################################################

=head2 inv

=for sig

 Signature: (a(m,m); sv opt )

=for usage

  $a1 = inv($a, {$opt});                 #default lu_decomp (not threadable)

=for ref

Invert a square matrix.

You feed in an NxN matrix in $a, and get back its inverse (if it
exists).  The code is inplace-aware, so you can get back the inverse
in $a itself if you want -- though temporary storage is used either
way.  You can cache the LU decomposition in an output option variable.

C<inv> uses lu_decomp by default; that's a numerically stable (pivoting)
LU decomposition method.  If you ask it to thread then a numerically
unstable (non-pivoting) method is used instead, so don't thread 
over largish or near-singular matrices unless you don't care about the anwer.


OPTIONS:

=over 3

=item * s

Boolean value indicating whether to complain if the matrix is singular.  If
this is false, singular matrices cause inverse to barf.  If it's true, then 
singular matrices cause inverse to return undef.

=item * lu (I/O)

This value contains a list ref with the LU decomposition, permutation,
and parity values for $a.  If you do not mention the key, or if the
value is undef, then inverse calls lu_decomp.  If the key exists with
an undef value, then the output of lu_decomp is stashed here (unless
the matrix is singular).  If the value exists, then it's assumed to
hold the lu decomposition.

=item * det (Output)

If this key exists, then the determinant of C<$a> get stored here,
whether or not the matrix is singular.

=back

=cut
*PDL::inv = \&inv;
sub inv {
  my $a = shift;
  my $opt = shift;
  $opt = {} unless defined($opt);

  barf "inverse needs a square PDL as a matrix\n" 
    unless(UNIVERSAL::isa($a,'PDL') &&
	   $a->dims >= 2 &&
	   $a->dim(0) == $a->dim(1)
	   );

  my ($lu,$perm,$par);
  if(exists($opt->{lu}) &&
     ref $opt->{lu} eq 'ARRAY') {
    ($lu,$perm,$par) = @{$opt->{lu}};
  } else {
    ($lu,$perm,$par) = lu_decomp($a);
    $opt->{lu} = [$lu,$perm,$par]
      if exists($opt->{lu});
  }

  my $det = (defined $lu) ? $lu->diagonal(0,1)->prodover * $par : 0;
  $opt->{det} = $det
    if exists($opt->{det});

  unless($det->nelem > 1 || $det) {
    return undef 
      if $opt->{s};
    barf("inverse: got a singular matrix or LU decomposition\n");
  }
  
  my $identity = zeroes($a); 
  $identity->diagonal(0,1)++;

  my $out = lu_backsub($lu,$perm,$identity)->xchg(0,1)->sever;

  return $out
    unless($a->is_inplace);

  $a .= $out;
  $a;
}

######################################################################

=head2 det

=for sig

 Signature: (a(m,m); sv opt)

=for usage

  $det = det($a,{opt});

=for ref

Determinant of a square matrix using LU decomposition (for large matrices)

You feed in a square matrix, you get back the determinant.  Some
options exist that allow you to cache the LU decomposition of the
matrix (note that the LU decomposition is invalid if the determinant
is zero!).  The LU decomposition is cacheable, in case you want to
re-use it.  This method of determinant finding is more rapid than
recursive-descent on large matrices, and if you reuse the LU
decomposition it's essentially free.

If you wask det to thread then L<lu_decomp|lu_decomp> drops you through
to L<lu_decomp2|lu_decomp2>, which is numerically unstable (and hence
not useful for very large matrices) but quite fast.

If you want to use threading on a matrix that's less than, say, 10x10,
and might be near singular, then you might want to use
L<determinant|determinant>, which is a more robust, threadable
determinant finder, instead.

OPTIONS:

=over 3

=item * lu (I/O)

Provides a cache for the LU decomposition of the matrix.  If you 
provide the key but leave the value undefined, then the LU decomposition
goes in here; if you put an LU decomposition here, it will be used and
the matrix won't be decomposed again.

=back

=cut
*PDL::det = \&det;
sub det {
  my($a) = shift;
  my($opt) = shift;
  $opt = {} unless defined($opt);

  my($lu,$perm,$par);
  if(exists ($opt->{u}) and (ref $opt->{lu} eq 'ARRAY')) {
    ($lu,$perm,$par) =  @{$opt->{lu}};
  } else {
    ($lu,$perm,$par) = lu_decomp($a);
    $opt->{lu} = [$lu,$perm,$par]
      if(exists($opt->{lu}));
  }
   
  ( (defined $lu) ? $lu->diagonal(0,1)->prodover * $par : 0 );
}

######################################################################

=head2 determinant

=for sig
 
 Signature: (a(m,m))

=for usage

  $det = determinant($a);

=for ref

Determinant of a square matrix, using recursive descent (threadable).

This is the traditional, robust recursive determinant method taught in
most linear algebra courses.  It scales like C<O(n!)> (and hence is
pitifully slow for large matrices) but is very robust because no 
division is involved (hence no division-by-zero errors for singular
matrices).  It's also threadable, so you can find the determinants of 
a large collection of matrices all at once if you want.

Matrices up to 3x3 are handled by direct multiplication; larger matrices
are handled by recursive descent to the 3x3 case.

The LU-decomposition method L<det|det> is faster in isolation for
single matrices larger than about 4x4, and is much faster if you end up
reusing the LU decomposition of $a, but does not thread well.

=cut

*PDL::determinant = \&determinant;
sub determinant {
  my($a) = shift;
  my($n);
  return undef unless(
		      UNIVERSAL::isa($a,'PDL') &&
		      $a->getndims >= 2 &&
		      ($n = $a->dim(0)) == $a->dim(1)
		      );
  
  return $a->clump(2) if($n==1);
  if($n==2) {
    my($b) = $a->clump(2);
    return $b->index(0)*$b->index(3) - $b->index(1)*$b->index(2);
  }
  if($n==3) {
    my($b) = $a->clump(2);
    
    my $b3 = $b->index(3);
    my $b4 = $b->index(4);
    my $b5 = $b->index(5);
    my $b6 = $b->index(6);
    my $b7 = $b->index(7);
    my $b8 = $b->index(8);

    return ( 
	 $b->index(0) * ( $b4 * $b8 - $b5 * $b7 )
      +  $b->index(1) * ( $b5 * $b6 - $b3 * $b8 )
      +  $b->index(2) * ( $b3 * $b7 - $b4 * $b6 )
	     );
  }
  
  my($i);
  my($sum) = zeroes($a->slice("(0),(0)")); 

  # Do middle submatrices
  for $i(1..$n-2) {
    my $el = $a->slice("($i),(0)");
    next if( all ($el==0) );  # Optimize away unnecessary recursion

    $sum += $el * (1-2*($i%2)) * 
      determinant(        $a->slice("0:".($i-1).",1:-1") ->
		   append($a->slice(($i+1).":-1,1:-1")));
  }

  # Do beginning and end submatrices
  $sum += $a->slice("(0),(0)")  * determinant($a->slice("1:-1,1:-1"));
  $sum -= $a->slice("(-1),(0)") * determinant($a->slice("0:-2,1:-1")) * (1 - 2*($n % 2));
  
  return $sum;
}




=head1 AUTHOR

Copyright (C) 2002 Craig DeForest.  There is no warranty.  You are 
allowed to redistribute and/or modify this work under the same conditions
as PDL itself.  If this file is separated from the PDL distribution, then
the PDL copyright notice should be included in this file.

=cut

1;
