
use PDL::LiteF;



########### Test of Subclassed-object copying for simple function cases ###########


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


#######################################################
package main;

###### Testing Begins #########
print "1..8\n";   

my $testNo = 1;

# Create New PDL::Derived Object
#   (Initialize sets 'someThingElse' data member
#     to 42)
$im = new PDL::Derived [
  [ 1, 2,  3,  3 , 5],
  [ 2,  3,  4,  5,  6],
  [13, 13, 13, 13, 13],
  [ 1,  3,  1,  3,  1],
  [10, 10,  2,  2,  2,]
 ];

#  Set 'someThingElse' Data Member to 24. (from 42)
$im->{someThingElse} = 24;

# Test to see if simple functions (a functions
#    with signature sqrt a(), [o]b() ) copies subclassed object correctly.
my @simpleFuncs = (qw/ 
bitnot sqrt abs sin cos not exp log10 /);

foreach my $op( @simpleFuncs){
	
	$w = $im->$op(); 

	ok($testNo++, $w->{someThingElse} == 24 ); 
}



sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}
