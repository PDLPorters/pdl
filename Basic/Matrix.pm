package PDL::Matrix;

@EXPORT_OK = ();

use PDL::Core;
use PDL::Exporter;

@ISA = qw/PDL::Exporter PDL/;



################################################################################
#
# overloads

# --------> constructors

# pdl is basically the same
sub pdl {
  my $class = shift;
  my $pdl = $class->SUPER::pdl(@_);
  bless $pdl, ref $class || $class;
}

# for constructors with specified dimensions I
# only have to swap these dimensions
for my $func (qw /zeroes ones sequence/) {
  my $code = << "EOE";

sub $func {
  my \$class = shift;
  my \@arg = \@_;
  ref(\$arg[0]) ne 'PDL::Type' ? (\@arg >  1 ? (\@arg[1,0] = \@arg[0,1]) : 1) :
                                 (\@arg >  2 ? (\@arg[2,1] = \@arg[1,2]) : 1);
  my \$pdl = \$class->SUPER::$func(\@arg);
  bless \$pdl, ref \$class || \$class;
}

EOE
# print "evaluating $code\n";
  eval $code;
}

# functions that construct a matrix pdl and that can be
# exported,
# very trivial, they just call its methods 
# optional type argument is checked in the methods
for my $func (qw /pdl zeroes ones sequence/) {
  push @EXPORT_OK, "m$func";
  my $code = << "EOE";

sub m$func { PDL::Matrix->$func(\@_) }

EOE
# print "evaluating $code\n";
  eval $code;
}

# --------> methods
# the slice arguments have to be changed to reflect the swapping
sub slice {
  my $self = shift;
  my $ind = shift;
  # add another dimension if slices has only one
  # convenient for vectors
  $ind =~ s/^([^,]*)$/$1,/;
  # swap
  $ind =~ s/^([^,]*),([^,]*).*/$2,$1/;
  $self->SUPER::slice($ind);
}

# swap arguments if number of arguments is greater than 1
# if its one, look at the first column, assuming it is a vector
sub at {
  my $self = shift;
  my @arg = @_;
  @arg >=2 ? @arg[1,0] = @arg[0,1] : ( (@arg == 1 && $self->dims == 2)  ?
                                       @arg = (0,$arg[0]) : 1 );
  $self->SUPER::at(@arg);
}

# this is needed because only doing
# > use overload '~' => \&PDL::transpose;
# would not give an object of type PDL::Matrix back
# => possible bug in transpose?!
sub transpose {
  my $self = shift;
  my $pdl = $self->PDL::transpose;
  bless $pdl, ref $self;
}

use overload '~' => \&PDL::Matrix::transpose;

# I cannot overload dims, because it is apparently used many times
# in methods like transpose! :-(
# this is not nice but I don´t know how to avoid this
sub mdims {
  my $self = shift;
  my @res = $self->SUPER::dims;
  @res >=2 ? @res[1,0] = @res[0,1] : @res;
  return @res;
}

# this has to be overloaded so that the PDL::slice
# is called and not PDL::Matrix::slice
# :-(
sub dummy($$;$) {
   my ($pdl,$dim) = @_;
   $dim = $pdl->getndims+1+$dim if $dim < 0;
   barf ("too high/low dimension in call to dummy, allowed min/max=0/"
         . $_[0]->getndims)
     if $dim>$pdl->getndims || $dim < 0;
   $_[2] = 1 if ($#_ < 2);
   $pdl->PDL::slice((','x$dim)."*$_[2]");
}

# stupid function to print a PDL::Matrix object in Maple code
sub printmaple {
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
    $s .= "]);\n";
  }
  else {
    $s = "vector($dimC,[";
    for(my $i=0;$i<$dimC;++$i) {
      $s .= $self->at($i);
      $s .= "," if $i+1<$dimC;
    }
    $s .= "]);\n";
  }
  print $s;
  return $s;
}



# functions that construct a vector pdl and that can be
# exported,
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

# vpdl
sub vpdl {
  my $pdl = transpose(PDL->pdl(@_));
  print $PDL;
  bless $pdl, PDL::Matrix;
}
push @EXPORT_OK, "vpdl";

# crossp for my special vectors
# sub vcrossp {
#   my ($pdl1,$pdl2) = @_;
#   print $pdl1->SUPER::slice("(0),"),$pdl2->SUPER::slice("(0),");
#   print PDL->crossp($pdl1->SUPER::slice("(0),"),$pdl2->SUPER::slice("(0),"));
#   return PDL->crossp($pdl1->SUPER::slice("(0),"),$pdl2->SUPER::slice("(0),"));
# }
# push @EXPORT_OK, "vcrossp";

%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

1;
