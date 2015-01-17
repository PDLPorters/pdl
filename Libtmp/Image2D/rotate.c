/* rotate.c - code modified from pnmrotate.c which included the following
   copyright notice
*/

/* pnmrotate.c - read a portable anymap and rotate it by some angle
**
** Copyright (C) 1989, 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include <stdlib.h>
#include <math.h>

#ifndef M_PI
#define M_PI	3.14159265358979323846
#endif /*M_PI*/

#define SCALE 4096
#define HALFSCALE 2048

typedef unsigned char imT;    /* image type */

static imT* my_allocarray(int cols, int rows)
{
  imT *arr = NULL;
  if ((arr = malloc(sizeof(imT)*cols*rows)) == NULL)
    croak("error getting memory for temporary array");
  return arr;
}

int getnewsize(int cols, int rows, float fangle, int *newcols, int *newrows)
{
    float xshearfac, yshearfac, new0;
    int tempcols, yshearjunk, x2shearjunk;

    if ( fangle < -90.0 || fangle > 90.0 )
      /* error( "angle must be between -90 and 90 degrees" ); */
      return -1;
    fangle = fangle * M_PI / 180.0;	/* convert to radians */

    xshearfac = tan( fangle / 2.0 );
    if ( xshearfac < 0.0 )
	xshearfac = -xshearfac;
    yshearfac = sin( fangle );
    if ( yshearfac < 0.0 )
	yshearfac = -yshearfac;

    tempcols = rows * xshearfac + cols + 0.999999;
    yshearjunk = ( tempcols - cols ) * yshearfac;
    *newrows = tempcols * yshearfac + rows + 0.999999;
    x2shearjunk = ( *newrows - rows - yshearjunk ) * xshearfac;
    *newrows -= 2 * yshearjunk;
    *newcols = *newrows * xshearfac + tempcols + 0.999999 - 2 * x2shearjunk;
    /* printf("oldrows: %d, oldcols: %d\n",rows,cols);
    printf("newrows: %d, newcols: %d\n",*newrows,*newcols);
    */
    return 0; /* OK */
}

int rotate(imT *im, imT *out, int cols, int rows, int nc, int nr,
	   float fangle, imT bgval, int antialias)
{
    float xshearfac, yshearfac, new0;
    int intnew0;
    imT *xelrow, *newxelrow, *temp1xels, *temp2xels, *nxP, *xP, prevxel, x;
    int tempcols, newcols, yshearjunk, x2shearjunk, row, col, new, newrows;
    register long fracnew0, omfracnew0;

    /* other angles should do a simple multiple of 90 degrees rotate before
       calling this one */
    if ( fangle < -90.0 || fangle > 90.0 )
      /* error( "angle must be between -90 and 90 degrees" ); */
      return -1;
    fangle = fangle * M_PI / 180.0;	/* convert to radians */

    xshearfac = tan( fangle / 2.0 );
    if ( xshearfac < 0.0 )
	xshearfac = -xshearfac;
    yshearfac = sin( fangle );
    if ( yshearfac < 0.0 )
	yshearfac = -yshearfac;

    tempcols = rows * xshearfac + cols + 0.999999;
    yshearjunk = ( tempcols - cols ) * yshearfac;
    newrows = tempcols * yshearfac + rows + 0.999999;
    x2shearjunk = ( newrows - rows - yshearjunk ) * xshearfac;
    newrows -= 2 * yshearjunk;
    newcols = newrows * xshearfac + tempcols + 0.999999 - 2 * x2shearjunk;
    
    /* check that the output has the right size */
    if (nc != newcols || nr != newrows)
      return -2;

    /* First shear X into temp1xels. */
    temp1xels = my_allocarray( tempcols, rows );
    for ( row = 0; row < rows; ++row )
	{
	  xelrow = im + row * cols; /* current row to process */
	if ( fangle > 0 )
	    new0 = row * xshearfac;
	else
	    new0 = ( rows - row ) * xshearfac;
	intnew0 = (int) new0;

	if ( antialias )
	    {
	    fracnew0 = ( new0 - intnew0 ) * SCALE;
	    omfracnew0 = SCALE - fracnew0;

	    for ( col = 0, nxP = temp1xels+row*tempcols;
		  col < tempcols; ++col, ++nxP )
		*nxP = bgval;

	    prevxel = bgval;
	    for ( col = 0, nxP = temp1xels+row*tempcols+intnew0, xP = xelrow;
		  col < cols; ++col, ++nxP, ++xP )
		{
		  *nxP = (fracnew0 * prevxel + omfracnew0 * *xP + HALFSCALE )
		    / SCALE;
		  prevxel = *xP;
		}
	    if ( fracnew0 > 0 && intnew0 + cols < tempcols )
		{
		  *nxP = 
		    ( fracnew0 * prevxel + omfracnew0 * bgval + HALFSCALE )
		    / SCALE;
		}
	    }
	else
	  {
	    for ( col = 0, nxP = temp1xels+row*tempcols;
		  col < intnew0; ++col, ++nxP )
	      *nxP = bgval;
	    for ( col = 0, xP = xelrow; col < cols; ++col, ++nxP, ++xP )
	      *nxP = *xP;
	    for ( col = intnew0 + cols; col < tempcols; ++col, ++nxP )
	      *nxP = bgval;
	  }
	}
    /* Now inverse shear Y from temp1 into temp2. */
    temp2xels = my_allocarray( tempcols, newrows );
    for ( col = 0; col < tempcols; ++col )
	{
	if ( fangle > 0 )
	    new0 = ( tempcols - col ) * yshearfac;
	else
	    new0 = col * yshearfac;
	intnew0 = (int) new0;
	fracnew0 = ( new0 - intnew0 ) * SCALE;
	omfracnew0 = SCALE - fracnew0;
	intnew0 -= yshearjunk;

	for ( row = 0; row < newrows; ++row )
	    temp2xels[row*tempcols+col] = bgval;

	if ( antialias )
	    {
	    prevxel = bgval;
	    for ( row = 0; row < rows; ++row )
		{
		new = row + intnew0;
		if ( new >= 0 && new < newrows )
		    {
		    nxP = temp2xels+new*tempcols+col;
		    x = temp1xels[row*tempcols+col];
		    *nxP = 
		      ( fracnew0 * prevxel + omfracnew0 * x + HALFSCALE )
		      / SCALE;
		    prevxel = x;
		    }
		}
	    if ( fracnew0 > 0 && intnew0 + rows < newrows )
		{
		nxP = temp2xels+(intnew0 + rows)*tempcols+col;
		*nxP = ( fracnew0 * prevxel + omfracnew0 * bgval + HALFSCALE )
		  / SCALE ;
		}
	    }
	else
	  {
	    for ( row = 0; row < rows; ++row )
	      {
		new = row + intnew0;
		if ( new >= 0 && new < newrows )
		  temp2xels[new*tempcols+col] = temp1xels[row*tempcols+col];
	      }
	  }
	}
    free(temp1xels);

    for ( row = 0; row < newrows; ++row )
	{
	  newxelrow = out + row*newcols;;
	if ( fangle > 0 )
	    new0 = row * xshearfac;
	else
	    new0 = ( newrows - row ) * xshearfac;
	intnew0 = (int) new0;
	fracnew0 = ( new0 - intnew0 ) * SCALE;
	omfracnew0 = SCALE - fracnew0;
	intnew0 -= x2shearjunk;

	for ( col = 0, nxP = newxelrow; col < newcols; ++col, ++nxP )
	    *nxP = bgval;

	if ( antialias )
	    {
	    prevxel = bgval;
	    for ( col = 0, xP = temp2xels+row*tempcols;
		  col < tempcols; ++col, ++xP )
		{
		new = intnew0 + col;
		if ( new >= 0 && new < newcols )
		    {
		    nxP = &(newxelrow[new]);
		    *nxP =
		      ( fracnew0 * prevxel + omfracnew0 * *xP + HALFSCALE )
		      / SCALE;
		    prevxel = *xP;
		    }
		}
	    if ( fracnew0 > 0 && intnew0 + tempcols < newcols )
		{
		nxP = &(newxelrow[intnew0 + tempcols]);
		    *nxP =
			( fracnew0 * prevxel + omfracnew0 * bgval + HALFSCALE )
		      / SCALE;
		}
	    }
	else
	  {
	    for ( col = 0, xP = temp2xels+row*tempcols;
		  col < tempcols; ++col, ++xP )
	      {
		new = intnew0 + col;
		if ( new >= 0 && new < newcols )
		    newxelrow[new] = *xP;
	      }
	  }

	}
    free(temp2xels);
    return 0; /* OK */
}
