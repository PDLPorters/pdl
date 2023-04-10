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
   my $x = bless {}, $class;
   my $value = shift;
   $$x{PDL} = $value;
   $$x{SomethingElse} = 42;
   return $x;
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
   my $class = shift;
   my $data = $_[0];
   my $self;
   if(ref($data) eq 'PDL' ){ # if $data is an object (a pdl)
	   $self = $class->initialize;
	   $self->{PDL} = $data;
   }
   else{	# if $data not an object call inherited constructor
	   $self = $class->SUPER::new($data);
   }
   return $self;
}
####### Initialize function. This over-ridden function is called by the PDL constructors
sub initialize {
	my $class = shift;
        my $self = {
                PDL => PDL->null, 	# used to store PDL object
		someThingElse => 42,
        };
	$class = (ref $class ? ref $class : $class );
        bless $self, $class;
}
###### Derived3 Object Needs to supply its own copy #####
sub copy {
	my $self = shift;
	# setup the object
	my $new = $self->initialize;
	# copy the PDL
	$new->{PDL} = $self->{PDL}->SUPER::copy;
	# copy the other stuff:
	$new->{someThingElse} = $self->{someThingElse};
	return $new;
}
}
## Now check to see if the different categories of primitive operations
##   return the PDL::Derived3 type.

# Create a PDL::Derived3 instance
$z = PDL::Derived3->new( ones(5,5) ) ;
ok(ref($z)eq"PDL::Derived3", "create derived instance");

#### Check the type after incrementing:
$z++;
ok(ref($z) eq "PDL::Derived3", "check type after incrementing");

#### Check the type after performing sumover:
my $y = $z->sumover;
ok(ref($y) eq "PDL::Derived3", "check type after sumover");

#### Check the type after adding two PDL::Derived3 objects:
my $x = PDL::Derived3->new( ones(5,5) ) ;
{
  my @w;
  local $SIG{__WARN__} = sub { push @w, @_ };
  my $w = $x + $z;
  ok(ref($w) eq "PDL::Derived3", "check type after adding");
  is "@w", '', 'no warnings';
}

#### Check the type after calling null:
my $a1 = PDL::Derived3->null();
ok(ref($a1) eq "PDL::Derived3", "check type after calling null");

##### Check the type for a biops2 operation:
my $w = ($x == $z);
ok(ref($w) eq "PDL::Derived3", "check type for biops2 operation");

##### Check the type for a biops3 operation:
$w = ($x | $z);
ok(ref($w) eq "PDL::Derived3", "check type for biops3 operation");

##### Check the type for a ufuncs1 operation:
$w = sqrt($z);
ok(ref($w) eq "PDL::Derived3", "check type for ufuncs1 operation");

##### Check the type for a ufuncs1f operation:
$w = sin($z);
ok(ref($w) eq "PDL::Derived3", "check type for ufuncs1f operation");

##### Check the type for a ufuncs2 operation:
$w = ! $z;
ok(ref($w) eq "PDL::Derived3", "check type for ufuncs2 operation");

##### Check the type for a ufuncs2f operation:
$w = log $z;
ok(ref($w) eq "PDL::Derived3", "check type for ufuncs2f operation");

##### Check the type for a bifuncs operation:
$w =  $z**2;
ok(ref($w) eq "PDL::Derived3", "check type for bifuncs operation");

##### Check the type for a slicing operation:
$a1 = PDL::Derived3->new(1+(xvals zeroes 4,5) + 10*(yvals zeroes 4,5));
$w = $a1->slice('1:3:2,2:4:2');
ok(ref($w) eq "PDL::Derived3", "check type for slicing operation");

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
   my $class = shift;
   my $data = $_[0];
   my $self;
   if(ref($data) eq 'PDL' ){ # if $data is an object (a pdl)
	   $self = $class->initialize;
	   $self->{PDL} = $data;
   }
   else{	# if $data not an object call inherited constructor
	   $self = $class->SUPER::new($data);
   }
   return $self;
}

####### Initialize function. This over-ridden function is called by the PDL constructors
sub initialize {
	my $class = shift;
        my $self = {
                PDL => PDL->null, 	# used to store PDL object
		someThingElse => 42,
        };
	$class = (ref $class ? ref $class : $class );
        bless $self, $class;
}

###### Derived4 Object Needs to supply its own copy #####
sub copy {
	my $self = shift;
	# setup the object
	my $new = $self->initialize;
	# copy the PDL
	$new->{PDL} = $self->{PDL}->SUPER::copy;
	# copy the other stuff:
	$new->{someThingElse} = $self->{someThingElse};
	return $new;
}

### Check of over-riding sumover
### This sumover should be called from PDL->sum.
###  If the result is different from the normal sumover by $self->{SomethingElse} (42) then
###   we will know that it has been called.
sub sumover{
	my $self = shift;
	my ($arg) = @_;
	if( ! defined $arg){   # no-argument form of calling
		$arg = $self->SUPER::sumover;
		return $self->{someThingElse} + $arg;
	}
	else{  # one-argument form of calling
		$self->SUPER::sumover($arg);
		$arg +=  $self->{someThingElse};
	}
}

#### test of overriding minmaximum. Calls inherited minmaximum and
####  Sets the Global variable main::OVERRIDEWORKED if called ####
sub minmaximum{
	my $self = shift;
	my ($arg) = @_;
	$main::OVERRIDEWORKED = 1;  # set the global variable so we know over-ride worked.
	# print "In over-ridden minmaximum\n";
	$self->SUPER::minmaximum(@_);
}

#### test of overriding inner. Calls inherited inner and
####  Sets the Global variable main::OVERRIDEWORKED if called ####
sub inner{
	my $self = shift;
	my ($arg) = @_;
	$main::OVERRIDEWORKED = 1;  # set the global variable so we know over-ride worked.
	# print "In over-ridden inner\n";
	$self->SUPER::inner(@_);
}

#### test of overriding which. Calls inherited which and
####  Sets the Global variable main::OVERRIDEWORKED if called ####
sub which{
	my $self = shift;
	my ($arg) = @_;
	$main::OVERRIDEWORKED++;  # set the global variable so we know over-ride worked.
	# print "In over-ridden which\n";
	$self->SUPER::which(@_);
}

#### test of overriding one2nd. Calls inherited one2nd and
####  increments the Global variable main::OVERRIDEWORKED if called ####
sub one2nd{
	my $self = shift;
	my ($arg) = @_;
	$main::OVERRIDEWORKED++;  # set the global variable so we know over-ride worked.
	# print "In over-ridden one2nd\n";
	$self->SUPER::one2nd(@_);
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

{
  my @w;
  local $SIG{__WARN__} = sub { push @w, @_ };
  # Check for PDL::sumover being called by sum
  ok($im->sum == 176, "PDL::sumover is called by sum" ); # result will be = 134 if derived sumover
                                                         # is not called,   176 if it is called.
  is "@w", '', 'no warnings';
}

### Test over-ride of minmaximum:
$main::OVERRIDEWORKED = 0;
my @minMax = $im->minmax;
ok($main::OVERRIDEWORKED == 1, "over-ride of minmaximum");

### Test over-ride of inner:
## Update to use inner, not matrix mult - CED 8-May-2010
$main::OVERRIDEWORKED = 0;
my $matMultRes = $im->inner($im);
ok($main::OVERRIDEWORKED == 1, "over-ride of inner");

### Test over-ride of which, one2nd
$main::OVERRIDEWORKED = 0;
# which ND test
$a1= PDL::Derived4->sequence(10,10,3,4);
($x, $y, $z, $w) = whichND($a1 == 203)->mv(0,-1)->dog;
ok($main::OVERRIDEWORKED == 1, "whichND worked"); # whitebox test condition, uugh!

# Check to see if the clip functions return a derived object:
ok(ref( $im->clip(5,7) ) eq "PDL::Derived4", "clip returns derived object");
ok(ref( $im->hclip(5) ) eq "PDL::Derived4", "hclip returns derived object");
ok(ref( $im->lclip(5) ) eq "PDL::Derived4", "lclip returns derived object");

########### Test of Subclassed-object copying for simple function cases ###########
##  First define a PDL-derived object:
{
package PDL::Derived5;
our @ISA = qw/PDL/;

sub new {
   my $class = shift;
   my $data = $_[0];
   my $self;
   if(ref($data) eq 'PDL' ){ # if $data is an object (a pdl)
	   $self = $class->initialize;
	   $self->{PDL} = $data;
   }
   else{	# if $data not an object call inherited constructor
	   $self = $class->SUPER::new($data);
   }
   return $self;
}

####### Initialize function. This over-ridden function is called by the PDL constructors
sub initialize {
	my $class = shift;
        my $self = {
                PDL => PDL->null, 	# used to store PDL object
		someThingElse => 42,
        };
	$class = (ref $class ? ref $class : $class );
        bless $self, $class;
}

###### Derived5 Object Needs to supply its own copy #####
sub copy {
	my $self = shift;
	# setup the object
	my $new = $self->initialize;
	# copy the PDL
	$new->{PDL} = $self->{PDL}->SUPER::copy;
	# copy the other stuff:
	$new->{someThingElse} = $self->{someThingElse};
	return $new;
}
}
#######################################################

###### Testing Begins #########
# Create New PDL::Derived5 Object
#   (Initialize sets 'someThingElse' data member
#     to 42)
$im = PDL::Derived5->new([
  [ 1, 2,  3,  3 , 5],
  [ 2,  3,  4,  5,  6],
  [13, 13, 13, 13, 13],
  [ 1,  3,  1,  3,  1],
  [10, 10,  2,  2,  2,]
]);

#  Set 'someThingElse' Data Member to 24. (from 42)
$im->{someThingElse} = 24;

# Test to see if simple functions (a functions
#    with signature sqrt a(), [o]b() ) copies subclassed object correctly.
my @simpleFuncs = (qw/bitnot sqrt abs sin cos not exp log10/);

foreach my $op( @simpleFuncs){
	$w = $im->$op();
	ok($w->{someThingElse} == 24, "$op subclassed object correctly");
}

done_testing;
