/*
 * prints  1 if machine is big endian
 *         0 if little endian
 *        -1 otherwise
 */

#include <stdio.h>

int main( int argc, char *argv[] ) {
  unsigned short i;
  unsigned char *b;  
  int p;
  i = 42; 
  b = (unsigned char *) (void*) &i;  
  if ( *b == 42 )          { p = 0; }
  else if ( *(b+1) == 42 ) { p = 1; }
  else                     { p = -1; }
  printf( "%d\n", p );
  return 0;
}

