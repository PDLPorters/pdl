use strict;
use warnings;
use PDL::LiteF;
use Test::More;

# Test PDL Subclassing via hashes

########### First test normal subclassing ###########
{
package PDL::Derived;
our @ISA = qw/PDL/;
sub new {
  my $class = shift;
  bless {PDL=>shift, SomethingElse=>42}, $class;
}
}

# Create a PDL::Derived instance
my $z = PDL::Derived->new( ones(5,5) ) ;
# PDL::Derived should have PDL properties
$z++;
ok(sum($z)==50, "derived object does PDL stuff");

# And should also have extra bits
ok($$z{SomethingElse}==42, "derived has extra bits" );
# And survive destruction
undef $z;

########### Now test magic subclassing i.e. PDL=code ref ###########
{
package PDL::Derived2;
# This is a array of ones of dim 'Coeff'
# All that is stored initially is "Coeff", the
# PDL array is only realised when a boring PDL
# function is called on it. One can imagine methods
# in PDL::Derived2 doing manipulation on the Coeffs
# rather than actualizing the data.
our @ISA = qw/PDL/;
sub new {
  my $class = shift;
  bless {Coeff=>shift, PDL=>\&cache, SomethingElse=>42}, $class;
}
# Actualize the value (demonstrating cacheing)
# One can imagine expiring the cache if say, Coeffs change
sub cache {
  my $self = shift;
  return $self->{Cache} if exists $self->{Cache};
  $self->{Cache} = PDL->ones(@$self{qw(Coeff Coeff)})+2;
}
}

# Create a PDL::Derived2 instance
$z = PDL::Derived2->new(5);
# PDL::Derived2 should have PDL properties
$z++;
ok(sum($z)==100, "derived2 has PDL properties");
# And should also have extra bits
ok($$z{SomethingElse}==42, "derived2 has extra bits" );
# And survive destruction
undef $z;

###  tests for proper output value typing of the major
###   categories of PDL primitive operations.
###       For example:
###           If $pdlderived is a PDL::derived object (subclassed from PDL),
###              then $pdlderived->sumover should return a PDL::derived object.
###

# Test PDL Subclassing via hashes

########### Subclass typing Test ###########
##  First define a PDL-derived object:
{
package PDL::Derived3;
our @ISA = qw/PDL/;
sub new {
  my ($class, $data) = @_;
  return $class->SUPER::new($data) if ref($data) ne 'PDL'; # if not object, inherited constructor
  my $self = $class->initialize;
  $self->{PDL} = $data;
  $self;
}
####### Initialize function. This over-ridden function is called by the PDL constructors
sub initialize {
  my ($class) = @_;
  my $self = bless { PDL => PDL->null }, ref $class || $class;
  $self->{someThingElse} = 42,
  $self;
}
###### Derived3 Object Needs to supply its own copy #####
sub copy {
  my $self = shift;
  my $new = $self->initialize;
  $new->{PDL} = $self->{PDL}->SUPER::copy;
  # copy the other stuff:
  $new->{someThingElse} = $self->{someThingElse};
  $new;
}
}
## Now check to see if the different categories of primitive operations
##   return the PDL::Derived3 type.

# Create a PDL::Derived3 instance
isa_ok $z = PDL::Derived3->new(ones(5,5)), "PDL::Derived3", "create derived instance";

#### Check the type after incrementing:
$z++;
isa_ok $z, "PDL::Derived3", "check type after incrementing";

#### Check the type after performing sumover:
isa_ok $z->sumover, "PDL::Derived3", "check type after sumover";

#### Check the type after adding two PDL::Derived3 objects:
my $x = PDL::Derived3->new( ones(5,5) ) ;
{
  my @w;
  local $SIG{__WARN__} = sub { push @w, @_ };
  my $w = $x + $z;
  isa_ok $w, "PDL::Derived3", "check type after adding";
  is "@w", '', 'no warnings';
}

#### Check the type after calling null:
isa_ok +PDL::Derived3->null, "PDL::Derived3", "check type after calling null";

##### Check the type for a biops2 operation:
isa_ok +($x == $z), "PDL::Derived3", "check type for biops2 operation";

##### Check the type for a biops3 operation:
isa_ok +($x | $z), "PDL::Derived3", "check type for biops3 operation";

##### Check the type for a ufuncs1 operation:
isa_ok sqrt($z), "PDL::Derived3", "check type for ufuncs1 operation";

##### Check the type for a ufuncs1f operation:
isa_ok sin($z), "PDL::Derived3", "check type for ufuncs1f operation";

##### Check the type for a ufuncs2 operation:
isa_ok ! $z, "PDL::Derived3", "check type for ufuncs2 operation";

##### Check the type for a ufuncs2f operation:
isa_ok log $z, "PDL::Derived3", "check type for ufuncs2f operation";

##### Check the type for a bifuncs operation:
isa_ok $z**2, "PDL::Derived3", "check type for bifuncs operation";

##### Check the type for a slicing operation:
my $a1 = PDL::Derived3->new(1+(xvals zeroes 4,5) + 10*(yvals zeroes 4,5));
isa_ok $a1->slice('1:3:2,2:4:2'), "PDL::Derived3", "check type for slicing operation";

##### Check that slicing with a subclass index works (sf.net bug #369)
$a1 = sequence(10,3,2);
my $idx = PDL::Derived3->new(2,5,8);
ok(defined(eval 'my $r = $a1->slice($idx,"x","x");'), "slice works with subclass index");

########### Test of method over-riding in subclassed objects ###########
### Global Variable used to tell if method over-riding worked ###
$main::OVERRIDEWORKED = 0;
##  First define a PDL-derived object:
{
package PDL::Derived4;
our @ISA = qw/PDL/;
sub new {
  my ($class, $data) = @_;
  return $class->SUPER::new($data) if ref($data) ne 'PDL'; # if not object, inherited constructor
  my $self = $class->initialize;
  $self->{PDL} = $data;
  return $self;
}

####### Initialize function. This over-ridden function is called by the PDL constructors
sub initialize {
  $::INIT_CALLED = 1;
  my $class = shift;
  my $self = bless { PDL => PDL->null }, ref $class || $class;
  $self->{someThingElse} = 42,
  $self;
}

###### Derived4 Object Needs to supply its own copy #####
sub copy {
  $::COPY_CALLED = 1;
  my $self = shift;
  my $new = $self->initialize;
  $new->{PDL} = $self->{PDL}->SUPER::copy;
  # copy the other stuff:
  $new->{someThingElse} = $self->{someThingElse};
  return $new;
}

### Check of over-riding sumover
### This sumover should be called from PDL->sum.
###  If the result is different from the normal sumover by $self->{SomethingElse} (42) then
###   we will know that it has been called.
sub sumover {
  my ($self, $out) = @_;
  return $self->SUPER::sumover + $self->{someThingElse} if !defined $out; # no-argument form of calling
  $self->SUPER::sumover($out); # if output arg given
  $out += $self->{someThingElse};
}

# test of overriding methods. Calls inherited method and
# sets the Global variable main::OVERRIDEWORKED if called
for (qw(minmaximum inner which one2nd)) {
  eval <<EOF;
sub $_ {
  \$main::OVERRIDEWORKED = 1; # set global so we know over-ride worked.
  my \$self = shift;
  \$self->SUPER::$_(\@_);
}
EOF
}
}

###### Testing Begins #########

my $im = PDL::Derived4->new([
  [ 1, 2,  3,  3 , 5],
  [ 2,  3,  4,  5,  6],
  [13, 13, 13, 13, 13],
  [ 1,  3,  1,  3,  1],
  [10, 10,  2,  2,  2,]
]);
isa_ok $im, 'PDL::Derived4';
isa_ok $im->flat, 'PDL::Derived4';

{
  my @w;
  local $SIG{__WARN__} = sub { push @w, @_ };
  # Check overridden sumover called by sum: 134 if PDL::sumover called
  is $im->sum, 176, "PDL::sumover is called by sum";
  is "@w", '', 'no warnings';
}

### Test over-ride of minmaximum:
$main::OVERRIDEWORKED = 0;
my @minMax = $im->minmax;
is $main::OVERRIDEWORKED, 1, "over-ride of minmaximum";

### Test over-ride of inner:
## Update to use inner, not matrix mult - CED 8-May-2010
$main::OVERRIDEWORKED = 0;
my $matMultRes = $im->inner($im);
is $main::OVERRIDEWORKED, 1, "over-ride of inner";

### Test over-ride of which, one2nd
$main::OVERRIDEWORKED = 0;
# which ND test
$a1= PDL::Derived4->sequence(10,10,3,4);
($x, my $y, $z, my $w) = whichND($a1 == 203)->mv(0,-1)->dog;
is $main::OVERRIDEWORKED, 1, "whichND worked"; # whitebox test condition, uugh!

# Check to see if the clip functions return a derived object:
isa_ok $im->clip(5,7), "PDL::Derived4", "clip returns derived object";
isa_ok $im->hclip(5), "PDL::Derived4", "hclip returns derived object";
isa_ok $im->lclip(5), "PDL::Derived4", "lclip returns derived object";

$::COPY_CALLED = $::INIT_CALLED = 0;
my $im2 = $im + 1;
ok !$::COPY_CALLED, 'no copy';
ok $::INIT_CALLED, 'yes init';

$::COPY_CALLED = $::INIT_CALLED = 0;
$im++;
ok !$::COPY_CALLED, 'no copy';
ok !$::INIT_CALLED, 'no init';

######## Test of Subclassed-object copying for simple function cases ########
#  Set 'someThingElse' Data Member to 24. (from 42)
$im->{someThingElse} = 24;
# Test to see if simple functions (a functions
#    with signature sqrt a(), [o]b() ) copies subclassed object correctly.
foreach my $op (qw(bitnot sqrt abs sin cos not exp log10)) {
  $w = $im->$op();
  is $w->{someThingElse}, 24, "$op subclassed object correctly";
}

done_testing;
