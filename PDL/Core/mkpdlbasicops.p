#!/usr/local/bin/perl

# Perl utility to generate pdlbasicops.c automatically
# for many different ops and datatypes

use lib "../..";
use PDL::Core::Dev;

$date = `date`; chop $date;

@biops   = qw( + * - / > < <= >= == != << >> | & ^ );
@ufuncs  = qw( sqrt sin cos log exp abs ! ~ );
@bifuncs = qw( pow atan2 MODULO SPACESHIP );

sub nofloat { # Decide which ops can't be done on floats/doubles
    my $op = shift;
    my (@bitops) = qw( << >> | & ^ ~ );
    for (@bitops) { return 1 if $_ eq $op }
    return 0;
}

############################ pdl_biop #################################

##### HEADER ######

print <<EOD;


/*************************************************************** 

   pdlbasicops.c                                     

****************************************************************/

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#include <math.h>

/* Some inlined functions */

#define MODULO(X,N)     ( (X) - (N)*((int)((X)/(N))) )
#define SPACESHIP(A,B)  ( (2*((A)>(B))-1) * ((A)!=(B))  ) 
#define abs(A)          ( (A)>0 ? (A) : -(A) )

/* Do a vectorised C = A op B  - either n1=n2 or n1=1 or n2=1 */

void pdl_biop ( char* op, void* c, void* a, void* b, int n1, int n2, int datatype) {

 int i,n3;
 
 if (n1 != n2 && !(n1==1 || n2==1) )
    croak("Arrays contain different numbers of elements");

  n3 = n1 == 1 ? n2 : n1; /* Length of c array */
 
  if (0) { /* Dummy */

EOD

#### Simple OPs loop ####

for $op (@biops) {

print <<EOD;

  } else if (!strcmp(op,"$op")) {

     switch (datatype) {
EOD

   ### Loop over data types ###

    for $i (keys %PDL_DATATYPES) {

    $type = $PDL_DATATYPES{$i}; ($cast1,$cast2,$cast3 ) = ("","","") ;

    ($cast1,$cast2,$cast3 ) = ("($type)","(long)","(long)") 
                if nofloat($op) && ($i eq "PDL_F" || $i eq "PDL_D");

     print <<EOT;

     case $i:

        { $type *aa = ($type*)a; /* Casts */
          $type *bb = ($type*)b;
          $type *cc = ($type*)c;
   
         i = n3; aa += n1-1; bb += n2-1; cc += n3-1;  
   
         if (n2==1) 
            while(i--) 
               *cc--  = $cast1 ( $cast2 *aa-- $op $cast3 *bb );
   
         else if (n1==1) 
            while(i--) 
               *cc--  = $cast1 ( $cast2 *aa $op $cast3 *bb-- );
         else
            while(i--) 
               *cc--  = $cast1 ( $cast2 *aa-- $op $cast3 *bb-- );
        }

       break;

EOT

    } # End of perl loop over datatypes

 
     print <<EOD;

     default:
       
       croak ("Not a known data type code=%d",datatype);
    
     }
EOD

} # Simple Ops loop


#### TRAILER #####

print <<EOD;

  }else{
     croak("Operation %s not supported",op);
  }
 
}

EOD

############################ pdl_bifunc #################################

##### HEADER ######

print <<EOD;

/* Do a vectorised C = F(A,B) - either n1=n2 or n1=1 or n2=1 */

void pdl_bifunc ( char* func, void* c, void* a, void* b, int n1, int n2, int datatype) {

 int i,n3;
 
 if (n1 != n2 && !(n1==1 || n2==1) )
    croak("Arrays contain different numbers of elements");

  n3 = n1 == 1 ? n2 : n1; /* Length of c array */
 
  if (0) { /* Dummy */

EOD

#### Simple OPs loop ####

for $func (@bifuncs) {

print <<EOD;

  } else if (!strcmp(func,"$func")) {

     switch (datatype) {
EOD

   ### Loop over data types ###

    for $i (keys %PDL_DATATYPES) {

    $type = $PDL_DATATYPES{$i};
    print <<EOT;

     case $i:

        { $type *aa = ($type*)a; /* Casts */
          $type *bb = ($type*)b;
          $type *cc = ($type*)c;
   
         i = n3; aa += n1-1; bb += n2-1; cc += n3-1;  
   
         if (n2==1) 
            while(i--) {
               *cc--  = ($type) $func(*aa, *bb); aa--;
            }
   
         else if (n1==1) 
            while(i--) {
               *cc--  = ($type) $func(*aa, *bb); bb--;
            }
         else
            while(i--) {
                *cc-- = ($type) $func(*aa, *bb); aa--; bb--;
            }
        }

       break;

EOT

    } # End of perl loop over datatypes

 
     print <<EOD;

     default:
       
       croak ("Not a known data type code=%d",datatype);
    
     }
EOD

} # Simple Ops loop


#### TRAILER #####

print <<EOD;

  }else{
     croak("Function %s not supported",func);
  }
 
}

EOD

############################ pdl_ufunc #################################

##### HEADER ######

print <<EOD;

/* Do a vectorised in place y=f(x) - n is the number of elements */

void pdl_ufunc ( char* func, void* x, int n, int datatype ) {

  if (0) { /* Dummy */

EOD

#### Simple OPs loop ####

for $func (@ufuncs) {

print <<EOD;

  } else if (!strcmp(func,"$func")) {

     switch (datatype) {
EOD

   ### Loop over data types ###

    for $i (keys %PDL_DATATYPES) {

    $type = $PDL_DATATYPES{$i};  ($cast1,$cast2) = ("","") ;
    $cast1 = "($type)";
    $cast2 = "(long)" if nofloat($func) && ($i eq "PDL_F" || $i eq "PDL_D");

    print <<EOT;

     case $i:

        { $type *xx = ($type*)x; /* Casts */
   
          int i = n; xx += n-1;
          while(i--) {
            *xx  = $cast1 $func($cast2 *xx ) ; xx--;
          }
        }

       break;

EOT

    } # End of perl loop over datatypes

 
     print <<EOD;

     default:
       
       croak ("Not a known data type code=%d",datatype);
    
     }
EOD

} # Simple Ops loop


#### TRAILER #####

print <<EOD;

  }else{
     croak("Function %s not supported",func);
  }
 
}




EOD

