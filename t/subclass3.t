#!/usr/bin/perl
#
use PDL::LiteF;
use Test::More tests => 7;


########### Test of method over-riding in subclassed objects ###########

### Global Variable used to tell if method over-riding worked ###
$main::OVERRIDEWORKED = 0;


##  First define a PDL-derived object:
package PDL::Derived;

@PDL::Derived::ISA = qw/PDL/;


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

###### Derived Object Needs to supply its own copy #####
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
#######################################################
package main;

###### Testing Begins #########

$im = new PDL::Derived [
  [ 1, 2,  3,  3 , 5],
  [ 2,  3,  4,  5,  6],
  [13, 13, 13, 13, 13],
  [ 1,  3,  1,  3,  1],
  [10, 10,  2,  2,  2,]
 ];


# Check for PDL::sumover being called by sum
ok($im->sum == 176, "PDL::sumover is called by sum" ); # result will be = 134 if derived sumover
                                                       # is not called,   176 if it is called.

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
my $a= PDL::Derived->sequence(10,10,3,4);     
# $PDL::whichND_no_warning = 1;
# my ($x, $y, $z, $w)=whichND($a == 203);
# ok($main::OVERRIDEWORKED == 2, "whichND test");
my ($x, $y, $z, $w) = whichND($a == 203)->mv(0,-1)->dog;  # quiet deprecation warning
ok($main::OVERRIDEWORKED == 1, "whichND worked");         # whitebox test condition, uugh!

# Check to see if the clip functions return a derived object:
ok(ref( $im->clip(5,7) ) eq "PDL::Derived", "clip returns derived object");
ok(ref( $im->hclip(5) ) eq "PDL::Derived", "hclip returns derived object");
ok(ref( $im->lclip(5) ) eq "PDL::Derived", "lclip returns derived object");
