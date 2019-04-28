/* going to turn it into a standalone module (cut out of xv24to8.c)
 * final goal: turn into call_external module for idl
 */

#include <stdio.h>
#include <stdlib.h>    /* qsort */


#ifdef DEBUG_OUT
#define StartCursor() fprintf(stderr,"%%idlppm_quant: choosing colors")
#define WaitCursor() fputc('.',stderr)
#define FinishCursor() fputc('\n',stderr)
#define FatalError(x) { fprintf(stderr,x); return(NULL); }
#else
#define StartCursor()
#define WaitCursor()
#define FinishCursor()
#define FatalError(x) return(0)
#endif

typedef unsigned char byte;


/***************************************************************/
/* The following code based on code from the 'pbmplus' package */
/* written by Jef Poskanzer                                    */
/***************************************************************/


/* ppmquant.c - quantize the colors in a pixmap down to a specified number
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


typedef unsigned char pixval;

#define PPM_MAXMAXVAL 255
typedef struct { pixval r, g, b; } pixel;

#define PPM_GETR(p) ((p).r)
#define PPM_GETG(p) ((p).g)
#define PPM_GETB(p) ((p).b)

#define PPM_ASSIGN(p,red,grn,blu) \
  { (p).r = (red); (p).g = (grn); (p).b = (blu); }

#define PPM_EQUAL(p,q) ( (p).r == (q).r && (p).g == (q).g && (p).b == (q).b )


/* Color scaling macro -- to make writing ppmtowhatever easier. */

#define PPM_DEPTH(newp,p,oldmaxval,newmaxval) \
    PPM_ASSIGN( (newp), \
	        (int) PPM_GETR(p) * (newmaxval) / ((int)oldmaxval), \
	        (int) PPM_GETG(p) * (newmaxval) / ((int)oldmaxval), \
	        (int) PPM_GETB(p) * (newmaxval) / ((int)oldmaxval) )


/* Luminance macro. */

/*
 * #define PPM_LUMIN(p) \
 *   ( 0.299 * PPM_GETR(p) + 0.587 * PPM_GETG(p) + 0.114 * PPM_GETB(p) )
 */

/* Luminance macro, using only integer ops.  Returns an int (*256)  JHB */
#define PPM_LUMIN(p) \
  ( 77 * PPM_GETR(p) + 150 * PPM_GETG(p) + 29 * PPM_GETB(p) )

/* Color histogram stuff. */

typedef struct chist_item* chist_vec;
struct chist_item { pixel color;
			int value;
		      };

typedef struct chist_list_item* chist_list;
struct chist_list_item { struct chist_item ch;
			     chist_list next;
			   };

typedef chist_list* chash_table;

typedef struct box* box_vector;
struct box {
  int index;
  int colors;
  int sum;
};


#define MAXCOLORS 32767
#define CLUSTER_MAXVAL 63

#define LARGE_LUM
#define REP_AVERAGE_PIXELS

#define FS_SCALE 1024

#define HASH_SIZE 6553

#define ppm_hashpixel(p) ((((int) PPM_GETR(p) * 33023 +    \
			    (int) PPM_GETG(p) * 30013 +    \
			    (int) PPM_GETB(p) * 27011) & 0x7fffffff)   \
			  % HASH_SIZE)



/*** function defs ***/
#define PARM(x) x
static chist_vec   mediancut        PARM((chist_vec, int, int, int, int));
static int         redcompare       PARM((const void *, const void *));
static int         greencompare     PARM((const void *, const void *));
static int         bluecompare      PARM((const void *, const void *));
static int         sumcompare       PARM((const void *, const void *));
static chist_vec   ppm_computechist PARM((pixel **, int,int,int,int *));
static chash_table ppm_computechash PARM((pixel **, int,int,int,int *));
static chist_vec   ppm_chashtochist PARM((chash_table, int));
static chash_table ppm_allocchash   PARM((void));
static void        ppm_freechist    PARM((chist_vec));
static void        ppm_freechash    PARM((chash_table));

int ppm_quant(byte *rin, byte *gin, byte *bin, int cols, int rows,
		     byte *pic8, byte *imap, byte *omap,
		     int len, int newcolors, int mode);

static int DEBUG=0;

#define SEPARATE 0
#define PACKED    1
#define PALETTE   2

/* rmap, gmap, bmap   256 byte arrays
** pic24 rows*cols*3 size byte array
** pic8  rows*cols byte array
** newcolors  number of new colors to put into look-up table
*/

/****************************************************************************/
int ppm_quant(byte *rin, byte *gin, byte *bin, int cols, int rows, byte *pic8, byte *imap,
	      byte *omap, int len, int newcolors, int mode)
{
  byte *map;
  pixel**          pixels;
  register pixel*  pP;
  int              row;
  register int     col, limitcol;
  pixval           maxval, newmaxval;
  int              colors;
  register int     index;
  chist_vec chv, colormap;
  chash_table  cht;
  int              i;
  unsigned char    *picptr;
  static char      *fn = "ppmquant()";

  index = 0;
  maxval = 255;

  /*
   *  reformat 24-bit image (3 bytes per pixel) into 2-dimensional
   *  array of pixel structures
   */

  if (DEBUG) fprintf(stderr,"%s: remapping to ppm-style internal fmt\n", fn);
  WaitCursor();

  pixels = (pixel **) malloc(rows * sizeof(pixel *));
  if (!pixels) FatalError("couldn't allocate 'pixels' array");
  for (row=0; row<rows; row++) {
    pixels[row] = (pixel *) malloc(cols * sizeof(pixel));
    if (!pixels[row]) FatalError("couldn't allocate a row of pixels array");
    switch (mode) {
    case SEPARATE:
      for (col=0, pP=pixels[row]; col<cols; col++, pP++) {
	pP->r = *rin++;
	pP->g = *gin++;
	pP->b = *bin++;
      }
      break;
    case PACKED:
      for (col=0, pP=pixels[row]; col<cols; col++, pP++) {
	pP->r = *rin++;
	pP->g = *rin++;
	pP->b = *rin++;
      }
      break;
    case PALETTE:
      for (col=0, pP=pixels[row]; col<cols; col++, pP++) {
	pP->r = imap[*rin*3];
	pP->g = imap[*rin*3+1];
	pP->b = imap[*rin*3+2];
      }
      break;
    default:
      return 0;
      break;
    }

  }
  if (DEBUG) fprintf(stderr,"%s: done format remapping\n", fn);




  /*
   *  attempt to make a histogram of the colors, unclustered.
   *  If at first we don't succeed, lower maxval to increase color
   *  coherence and try again.  This will eventually terminate, with
   *  maxval at worst 15, since 32^3 is approximately MAXCOLORS.
   */

  WaitCursor();
  for ( ; ; ) {
    if (DEBUG) fprintf(stderr, "%s:  making histogram\n", fn);

    chv = ppm_computechist(pixels, cols, rows, MAXCOLORS, &colors);
    if (chv != (chist_vec) 0) break;

    if (DEBUG) fprintf(stderr, "%s: too many colors!\n", fn);
    newmaxval = maxval / 2;
    if (DEBUG) fprintf(stderr, "%s: rescaling colors (maxval=%d) %s\n",
		       fn, newmaxval, "to improve clustering");

    for (row=0; row<rows; ++row)
      for (col=0, pP=pixels[row]; col<cols; ++col, ++pP)
	PPM_DEPTH( *pP, *pP, maxval, newmaxval );
    maxval = newmaxval;
  }

  if (DEBUG) fprintf(stderr,"%s: %d colors found\n", fn, colors);



  /*
   * Step 3: apply median-cut to histogram, making the new colormap.
   */

  WaitCursor();
  if (DEBUG) fprintf(stderr, "%s: choosing %d colors\n", fn, newcolors);
  colormap = mediancut(chv, colors, rows * cols, maxval, newcolors);
  ppm_freechist(chv);



  /*
   *  Step 4: map the colors in the image to their closest match in the
   *  new colormap, and write 'em out.
   */

  if (DEBUG) fprintf(stderr,"%s: mapping image to new colors\n", fn);
  cht = ppm_allocchash();

  picptr = pic8;
  for (row = 0;  row < rows;  ++row) {
    col = 0;  limitcol = cols;  pP = pixels[row];

    if ((row & 0x1f) == 0) WaitCursor();
    do {
      int hash;
      chist_list chl;

      /* Check hash table to see if we have already matched this color. */

      hash = ppm_hashpixel(*pP);
      for (chl = cht[hash];  chl;  chl = chl->next)
	if (PPM_EQUAL(chl->ch.color, *pP)) {index = chl->ch.value; break;}

      if (!chl /*index = -1*/) {/* No; search colormap for closest match. */
	register int i, r1, g1, b1, r2, g2, b2;
	register long dist, newdist;

	r1 = PPM_GETR( *pP );
	g1 = PPM_GETG( *pP );
	b1 = PPM_GETB( *pP );
	dist = 2000000000;

	for (i=0; i<newcolors; i++) {
	  r2 = PPM_GETR( colormap[i].color );
	  g2 = PPM_GETG( colormap[i].color );
	  b2 = PPM_GETB( colormap[i].color );

	  newdist = ( r1 - r2 ) * ( r1 - r2 ) +
	            ( g1 - g2 ) * ( g1 - g2 ) +
	            ( b1 - b2 ) * ( b1 - b2 );

	  if (newdist<dist) { index = i;  dist = newdist; }
	}

	hash = ppm_hashpixel(*pP);
	chl = (chist_list) malloc(sizeof(struct chist_list_item));
	if (!chl) FatalError("ran out of memory adding to hash table");

	chl->ch.color = *pP;
	chl->ch.value = index;
	chl->next = cht[hash];
	cht[hash] = chl;
      }

      *picptr++ = index;

      ++col;
      ++pP;
    }
    while (col != limitcol);
  }

  /* rescale the colormap */
  map = omap;
  for (i=0; i<newcolors; i++) {
    PPM_DEPTH(colormap[i].color, colormap[i].color, maxval, 255);
    *map++ = PPM_GETR( colormap[i].color );
    *map++ = PPM_GETG( colormap[i].color );
    *map++ = PPM_GETB( colormap[i].color );
  }

  /* free the pixels array */
  for (i=0; i<rows; i++) free(pixels[i]);
  free(pixels);

  /* free cht and colormap */
  ppm_freechist(colormap);
  ppm_freechash(cht);

  return 1;
}



/*
** Here is the fun part, the median-cut colormap generator.  This is based
** on Paul Heckbert's paper "Color Image Quantization for Frame Buffer
** Display", SIGGRAPH '82 Proceedings, page 297.
*/



/****************************************************************************/
static chist_vec mediancut( chv, colors, sum, maxval, newcolors )
     chist_vec chv;
     int colors, sum, newcolors;
     int maxval;
{
  chist_vec colormap;
  box_vector bv;
  register int bi, i;
  int boxes;

  bv = (box_vector) malloc(sizeof(struct box) * newcolors);
  colormap = (chist_vec)
             malloc(sizeof(struct chist_item) * newcolors );

  if (!bv || !colormap) FatalError("unable to malloc in mediancut()");

  for (i=0; i<newcolors; i++)
    PPM_ASSIGN(colormap[i].color, 0, 0, 0);

  /*
   *  Set up the initial box.
   */
  bv[0].index = 0;
  bv[0].colors = colors;
  bv[0].sum = sum;
  boxes = 1;


  /*
   ** Main loop: split boxes until we have enough.
   */

  while ( boxes < newcolors ) {
    register int indx, clrs;
    int sm;
    register int minr, maxr, ming, maxg, minb, maxb, v;
    int halfsum, lowersum;

    /*
     ** Find the first splittable box.
     */
    for (bi=0; bv[bi].colors<2 && bi<boxes; bi++) ;
    if (bi == boxes) break;	/* ran out of colors! */

    indx = bv[bi].index;
    clrs = bv[bi].colors;
    sm = bv[bi].sum;

    /*
     ** Go through the box finding the minimum and maximum of each
     ** component - the boundaries of the box.
     */
    minr = maxr = PPM_GETR( chv[indx].color );
    ming = maxg = PPM_GETG( chv[indx].color );
    minb = maxb = PPM_GETB( chv[indx].color );

    for (i=1; i<clrs; i++) {
      v = PPM_GETR( chv[indx + i].color );
      if (v < minr) minr = v;
      if (v > maxr) maxr = v;

      v = PPM_GETG( chv[indx + i].color );
      if (v < ming) ming = v;
      if (v > maxg) maxg = v;

      v = PPM_GETB( chv[indx + i].color );
      if (v < minb) minb = v;
      if (v > maxb) maxb = v;
    }

    /*
     ** Find the largest dimension, and sort by that component.  I have
     ** included two methods for determining the "largest" dimension;
     ** first by simply comparing the range in RGB space, and second
     ** by transforming into luminosities before the comparison.  You
     ** can switch which method is used by switching the commenting on
     ** the LARGE_ defines at the beginning of this source file.
     */
    {
      /* LARGE_LUM version */

      pixel p;
      int rl, gl, bl;

      PPM_ASSIGN(p, maxr - minr, 0, 0);
      rl = PPM_LUMIN(p);

      PPM_ASSIGN(p, 0, maxg - ming, 0);
      gl = PPM_LUMIN(p);

      PPM_ASSIGN(p, 0, 0, maxb - minb);
      bl = PPM_LUMIN(p);

      if (rl >= gl && rl >= bl)
	qsort((char*) &(chv[indx]), (size_t) clrs, sizeof(struct chist_item),
	      redcompare );
      else if (gl >= bl)
	qsort((char*) &(chv[indx]), (size_t) clrs, sizeof(struct chist_item),
	      greencompare );
      else
	qsort((char*) &(chv[indx]), (size_t) clrs, sizeof(struct chist_item),
	      bluecompare );
    }

    /*
     ** Now find the median based on the counts, so that about half the
     ** pixels (not colors, pixels) are in each subdivision.
     */
    lowersum = chv[indx].value;
    halfsum = sm / 2;
    for (i=1; i<clrs-1; i++) {
      if (lowersum >= halfsum) break;
      lowersum += chv[indx + i].value;
    }

    /*
     ** Split the box, and sort to bring the biggest boxes to the top.
     */
    bv[bi].colors = i;
    bv[bi].sum = lowersum;
    bv[boxes].index = indx + i;
    bv[boxes].colors = clrs - i;
    bv[boxes].sum = sm - lowersum;
    ++boxes;
    qsort((char*) bv, (size_t) boxes, sizeof(struct box), sumcompare);
  }  /* while (boxes ... */

  /*
   ** Ok, we've got enough boxes.  Now choose a representative color for
   ** each box.  There are a number of possible ways to make this choice.
   ** One would be to choose the center of the box; this ignores any structure
   ** within the boxes.  Another method would be to average all the colors in
   ** the box - this is the method specified in Heckbert's paper.  A third
   ** method is to average all the pixels in the box.  You can switch which
   ** method is used by switching the commenting on the REP_ defines at
   ** the beginning of this source file.
   */

  for (bi=0; bi<boxes; bi++) {
    /* REP_AVERAGE_PIXELS version */
    register int indx = bv[bi].index;
    register int clrs = bv[bi].colors;
    register long r = 0, g = 0, b = 0, sum = 0;

    for (i=0; i<clrs; i++) {
      r += PPM_GETR( chv[indx + i].color ) * chv[indx + i].value;
      g += PPM_GETG( chv[indx + i].color ) * chv[indx + i].value;
      b += PPM_GETB( chv[indx + i].color ) * chv[indx + i].value;
      sum += chv[indx + i].value;
    }

    r = r / sum;  if (r>maxval) r = maxval;	/* avoid math errors */
    g = g / sum;  if (g>maxval) g = maxval;
    b = b / sum;  if (b>maxval) b = maxval;

    PPM_ASSIGN( colormap[bi].color, r, g, b );
  }

  free(bv);
  return colormap;
}


/**********************************/
static int redcompare(p1, p2)
     const void *p1, *p2;
{
  return (int) PPM_GETR( ((chist_vec)p1)->color ) -
         (int) PPM_GETR( ((chist_vec)p2)->color );
}

/**********************************/
static int greencompare(p1, p2)
     const void *p1, *p2;
{
  return (int) PPM_GETG( ((chist_vec)p1)->color ) -
         (int) PPM_GETG( ((chist_vec)p2)->color );
}

/**********************************/
static int bluecompare(p1, p2)
     const void *p1, *p2;
{
  return (int) PPM_GETB( ((chist_vec)p1)->color ) -
         (int) PPM_GETB( ((chist_vec)p2)->color );
}

/**********************************/
static int sumcompare(p1, p2)
     const void *p1, *p2;
{
  return ((box_vector) p2)->sum - ((box_vector) p1)->sum;
}



/****************************************************************************/
static chist_vec
  ppm_computechist(pixels, cols, rows, maxcolors, colorsP)
     pixel** pixels;
     int cols, rows, maxcolors;
     int* colorsP;
{
  chash_table cht;
  chist_vec chv;

  cht = ppm_computechash(pixels, cols, rows, maxcolors, colorsP);
  if (!cht) return (chist_vec) 0;

  chv = ppm_chashtochist(cht, maxcolors);
  ppm_freechash(cht);
  return chv;
}


/****************************************************************************/
static chash_table ppm_computechash(pixels, cols, rows,
					    maxcolors, colorsP )
     pixel** pixels;
     int cols, rows, maxcolors;
     int* colorsP;
{
  chash_table cht;
  register pixel* pP;
  chist_list chl;
  int col, row, hash;

  cht = ppm_allocchash( );
  *colorsP = 0;

  /* Go through the entire image, building a hash table of colors. */
  for (row=0; row<rows; row++)
    for (col=0, pP=pixels[row];  col<cols;  col++, pP++) {
      hash = ppm_hashpixel(*pP);

      for (chl = cht[hash]; chl != (chist_list) 0; chl = chl->next)
	if (PPM_EQUAL(chl->ch.color, *pP)) break;

      if (chl != (chist_list) 0) ++(chl->ch.value);
      else {
	if ((*colorsP)++ > maxcolors) {
	  ppm_freechash(cht);
	  return (chash_table) 0;
	}

	chl = (chist_list) malloc(sizeof(struct chist_list_item));
	if (!chl) FatalError("ran out of memory computing hash table");

	chl->ch.color = *pP;
	chl->ch.value = 1;
	chl->next = cht[hash];
	cht[hash] = chl;
      }
    }

  return cht;
}


/****************************************************************************/
static chash_table ppm_allocchash()
{
  chash_table cht;
  int i;

  cht = (chash_table) malloc( HASH_SIZE * sizeof(chist_list) );
  if (!cht) FatalError("ran out of memory allocating hash table");

  for (i=0; i<HASH_SIZE; i++ )
    cht[i] = (chist_list) 0;

  return cht;
}


/****************************************************************************/
static chist_vec ppm_chashtochist( cht, maxcolors )
     chash_table cht;
     int maxcolors;
{
  chist_vec chv;
  chist_list chl;
  int i, j;

  /* Now collate the hash table into a simple chist array. */
  chv = (chist_vec) malloc( maxcolors * sizeof(struct chist_item) );

  /* (Leave room for expansion by caller.) */
  if (!chv) FatalError("ran out of memory generating histogram");

  /* Loop through the hash table. */
  j = 0;
  for (i=0; i<HASH_SIZE; i++)
    for (chl = cht[i];  chl != (chist_list) 0;  chl = chl->next) {
      /* Add the new entry. */
      chv[j] = chl->ch;
      ++j;
    }

  return chv;
}


/****************************************************************************/
static void ppm_freechist( chv )
     chist_vec chv;
{
  free( (char*) chv );
}


/****************************************************************************/
static void ppm_freechash( cht )
     chash_table cht;
{
  int i;
  chist_list chl, chlnext;

  for (i=0; i<HASH_SIZE; i++)
    for (chl = cht[i];  chl != (chist_list) 0; chl = chlnext) {
      chlnext = chl->next;
      free( (char*) chl );
    }

  free( (char*) cht );
}
