#!/usr/local/bin/perl

# Test adding errors using perlDL as a demonstration
# of OO-techniques. This sets up a class of object with "data" 
# and "errors" both of which are pdl variables. 

package ErrObj; 

use PDL;
$PDL::verbose=1;

# Overload ops for this object

%OVERLOAD = ('+' => \&add, "\"\"" =>  \&stringify);

sub new {
    my ($class, $value) = @_;
    my $self = {};
    bless $self, $class;
    $$self{Data} = pdl($value);
    $$self{Errs} = $$self{Data} * 0 + 1;  # Init Errs = 1
    return $self;    
}

sub copy{ 
   my $self = shift;
   my $newself = bless {}, ref($self);
   $$newself{Data} = pdl($$self{Data}); # copy
   $$newself{Errs} = pdl($$self{Errs});
   return $newself;
}

sub stringify {
   my $self = shift;
   return "$$self{Data} +/- $$self{Errs}";
}

sub add {
    my ($a, $b) = @_; 
    my $c = $a->copy; 

    $$c{Data} = $$a{Data}+$$b{Data};
    $$c{Errs} = sqrt($$a{Errs}**2 + $$b{Errs}**2);

    return $c;
}
 

package main;

$x = new ErrObj [1..10]; 
$y = new ErrObj [21..30];

print '$x = ',$x,"\n";
print '$y = ',$y,"\n"; 

print '$x + $y =',$x+$y,"\n";
print '$x + $y + $y =',$x+$y+$y,"\n";
