
use PDL::LiteF;

# Test PDL Subclassing via hashes

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

print "1..6\n";

########### First test normal subclassing ###########

package PDL::Derived;

@PDL::Derived::ISA = qw/PDL/;

sub new {
   my $class = shift;
   my $x = bless {}, $class;
   my $value = shift;
   $$x{PDL} = $value;
   $$x{SomethingElse} = 42;
   return $x;
}

package main;

# Create a PDL::Derived instance

$z = PDL::Derived->new( ones(5,5) ) ;

# PDL::Derived should have PDL properties

$z++;

ok(1, sum($z)==50);

# And should also have extra bits

ok(2, $$z{SomethingElse}==42 );

# And survive destruction

undef $z;

ok(3, 1==1);


########### Now test magic subclassing i.e. PDL=code ref ###########

package PDL::Derived2;

# This is a array of ones of dim 'Coeff'
# All that is stored initially is "Coeff", the
# PDL array is only realised when a boring PDL
# function is called on it. One can imagine methods
# in PDL::Derived2 doing manipulation on the Coeffs
# rather than actualizing the data.

@PDL::Derived2::ISA = qw/PDL/;

sub new {
   my $class = shift;
   my $x = bless {}, $class;
   my $value = shift;
   $$x{Coeff} = $value;
   $$x{PDL} = sub { return $x->cache };
   $$x{SomethingElse} = 42;
   return $x;
}

# Actualize the value (demonstrating cacheing)
# One can imagine expiring the cache if say, Coeffs change

sub cache {
  my $self = shift;
  my $v = $self->{Coeff};
  $self->{Cache} = PDL->ones($v,$v)+2 unless exists $self->{Cache};
  return $self->{Cache};
}

package main;

# Create a PDL::Derived2 instance

$z = PDL::Derived2->new(5);

# PDL::Derived2 should have PDL properties

$z++;

ok(4, sum($z)==100);

# And should also have extra bits

ok(5, $$z{SomethingElse}==42 );

# And survive destruction

undef $z;

ok(6, 1==1);


