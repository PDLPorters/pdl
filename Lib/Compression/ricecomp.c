/**********************************************************************
 * A general purpose limited-entropy Rice compressor library
 * 
 * The Rice algorithm is described by Rice, R.F., Yeh, P.-S., and 
 * Miller, W. H. 1993, in Proc. of the 9th AIAA Computing in Aerospace
 * Conference, AIAA-93-45411-CP.  Rice algorithms in general are simplified
 * Golomb codes that are useful for coding data with certain statistical
 * properties (generally, that differences between samples are typically 
 * smaller than the coded dynamic range).  This code compresses blocks
 * of samples (typically 16 or 32 samples at a time) that are stored 
 * in normal 2's complement signed integer form, with a settable number 
 * of 8-bit bytes per sample.
 *
 * Strict Rice coding gives rise
 * (in principle) to extremely large symbols in the worst high-entropy 
 * case, so this library includes a block-level switch 
 * 
 * Assumptions: int is 32 bits ("long int"); short is 16 bits, byte is 8 bits.
 *
 * HISTORICAL NOTE:
 * 
 * This compression library is modified from the CFITSIO library,
 * which is distributed by the U.S. government under the above
 * Free-compatible license.  The code was originally written by
 * Richard White at the STScI and contributed to CFITSIO in July 1999.
 * The code has been further modified (Craig DeForest) to work in a
 * more general-purpose way than just within CFITSIO.
 * 
 *
 * LICENSING & COPYRIGHT: 
 *
 * Portions of this code are copyright (c) U.S. Government; the
 * modifications are copyright (c) Craig DeForest.  The entire library
 * (including modifications) is licensed under the following terms 
 * (inherited from CFITSIO v. 3.24):
 *
 * Permission to freely use, copy, modify, and distribute this software
 * and its documentation without fee is hereby granted, provided that this
 * copyright notice and disclaimer of warranty appears in all copies.
 * 
 * DISCLAIMER:
 *
 * THE SOFTWARE IS PROVIDED 'AS IS' WITHOUT ANY WARRANTY OF ANY KIND,
 * EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED
 * TO, ANY WARRANTY THAT THE SOFTWARE WILL CONFORM TO SPECIFICATIONS,
 * ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, AND FREEDOM FROM INFRINGEMENT, AND ANY WARRANTY THAT THE
 * DOCUMENTATION WILL CONFORM TO THE SOFTWARE, OR ANY WARRANTY THAT
 * THE SOFTWARE WILL BE ERROR FREE.  IN NO EVENT SHALL NASA BE LIABLE
 * FOR ANY DAMAGES, INCLUDING, BUT NOT LIMITED TO, DIRECT, INDIRECT,
 * SPECIAL OR CONSEQUENTIAL DAMAGES, ARISING OUT OF, RESULTING FROM,
 * OR IN ANY WAY CONNECTED WITH THIS SOFTWARE, WHETHER OR NOT BASED
 * UPON WARRANTY, CONTRACT, TORT , OR OTHERWISE, WHETHER OR NOT INJURY
 * WAS SUSTAINED BY PERSONS OR PROPERTY OR OTHERWISE, AND WHETHER OR
 * NOT LOSS WAS SUSTAINED FROM, OR AROSE OUT OF THE RESULTS OF, OR USE
 * OF, THE SOFTWARE OR SERVICES PROVIDED HEREUNDER.
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char Buffer_t;
typedef struct {
	int bitbuffer;		/* bit buffer					*/
	int bits_to_go;		/* bits to go in buffer			*/
	Buffer_t *start;	/* start of buffer				*/
	Buffer_t *current;	/* current position in buffer	*/
	Buffer_t *end;		/* end of buffer				*/
} Buffer;

#define putcbuf(c,mf) 	((*(mf->current)++ = c), 0)

static void start_outputing_bits(Buffer *buffer);
static int done_outputing_bits(Buffer *buffer);
static int output_nbits(Buffer *buffer, int bits, int n);

/**********************************************************************
 * rcomp 
 * 
 * Usage: 
 *   bytes = rcomp( a, sampsiz, nx, buf, buflen, nblock )
 * 
 *   a is a pointer to the input buffer, which contains signed integer
 *   data to be encoded, either as bytes, shorts, or longs.
 * 
 *   sampsiz tells the sample size in bytes (1, 2, or 4)
 * 
 *   nx is the number of input samples to encode.
 * 
 *   buf is a pointer to the output buffer, which must be predeclared.
 * 
 *   clen is the size of the output buffer, in bytes.
 *
 *   nblock is the coding block size to use, in samples (typ. 16 or 32)
 *
 *   
 * The data are encoded (and hopefully compressed) into the output buffer, 
 * and the length of the encoded data is returned.  In case of failure 
 * (e.g. buffer too small) -1 is returned.
 * 
 * The CFITSIO code has this broken out into multiple routines for
 * different data types, but I (CED) have recombined them: the
 * overhead of using a couple of switch() statements to combine them
 * is believed (by me) to be negligible on modern architectures: the
 * process is close to memory-bound, and branch prediction on high end
 * microprocessors makes the type switches take 0 cycles anyway on
 * most iterations.
 * 
 */

int rcomp(void *a_v,		/* input array			*/
	  int bsize,            /* sample size (in bytes)       */
	  int nx,		/* number of input pixels	*/
	  unsigned char *c,	/* output buffer		*/
	  int clen,		/* max length of output		*/
	  int nblock)		/* coding block size		*/
{
Buffer bufmem, *buffer = &bufmem;
int *a = (int *)a_v;
int i, j, thisblock;
int lastpix, nextpix, pdiff;
int v, fs, fsmask, top, fsmax, fsbits, bbits;
int lbitbuffer, lbits_to_go;
unsigned int psum;
double pixelsum, dpsum;
unsigned int *diff;

 // Blocksize is picked so that boundaries lie on 64-bit word edges for all data types
 if(nblock & 0x7 ) { 
   fprintf(stderr,"rcomp: nblock must be divisible by 4 (is %d)\n",nblock);
   fflush(stderr);
   return(-1);
 }

 /* Magic numbers from fits_rcomp in CFITSIO; these have to match the ones in 
 *  rdecomp, below 
 */
 switch(bsize) {
 case 1: // byte
   fsbits = 3;
   fsmax = 6;
   break;
 case 2: // int
   fsbits = 4;
   fsmax = 14;
   break;
 case 4: // long
   fsbits = 5;
   fsmax = 25;
   break;
 default:
   fprintf(stderr,"rcomp: bsize must be 1, 2, or 4 bytes");
   fflush(stderr);
   return(-1);
 }
 
 bbits = 1<<fsbits;
 
 /*
  * Set up buffer pointers
  */
 buffer->start = c;
 buffer->current = c;
 buffer->end = c+clen;
 buffer->bits_to_go = 8;
 
 /*
  * array for differences mapped to non-negative values
  * Treat as an array of longs so it works in all cases
  *
  */
 diff = (unsigned int *) malloc(nblock*sizeof(unsigned int));
 if (diff == (unsigned int *) NULL) {
   fprintf(stderr,"rcomp: insufficient memory (allocating %d ints for internal buffer)",nblock);
   fflush(stderr);
   return(-1);
 }
 /*
  * Code in blocks of nblock pixels
  */
 start_outputing_bits(buffer);

 /* write out first sample to the first bsize bytes of the buffer */
 {
   int a0;
   int z;
   a0 = a[0];
   z = output_nbits(buffer, a0, bsize * 8);
   if (z) {
     // no error message - buffer overruns are silent
     free(diff);
     return(-1);
 }
 }

 /* the first difference will always be zero */
 switch(bsize) {
 case 1: lastpix = *((char *)a); break;
 case 2: lastpix = *((short *)a); break;
 case 4: lastpix = *((int *)a); break;
 default: break; // never happens (would be caught by first switch)
 }

 thisblock = nblock;

 for (i=0; i<nx; i += nblock) {
   /* last block may be shorter */
   if (nx-i < nblock) thisblock = nx-i;
   /*
    * Compute differences of adjacent pixels and map them to unsigned values.
    * Note that this may overflow the integer variables -- that's
    * OK, because we can recover when decompressing.  If we were
    * compressing shorts or bytes, would want to do this arithmetic
    * with short/byte working variables (though diff will still be
    * passed as an int.)
    *
    * compute sum of mapped pixel values at same time
    * use double precision for sum to allow 32-bit integer inputs
    *
    * This is the last time we refer directly to the input data - they
    * are converted from byte/short/long format to long diffs, so 
    * no more type switches are needed.
    * 
    */
	pixelsum = 0.0;
	for (j=0; j<thisblock; j++) {
	  switch(bsize) {
	  case 1: nextpix = ((char *)a)[i+j]; break;
	  case 2: nextpix = ((short *)a)[i+j]; break;
	  case 4: nextpix = ((int *)a)[i+j]; break;
	  default: break; // never happens
	  }

	  pdiff = nextpix - lastpix;
	  diff[j] = (unsigned int) ((pdiff<0) ? ~(pdiff<<1) : (pdiff<<1));
	  pixelsum += diff[j];
	  lastpix = nextpix;

	}

	/*
	 * compute number of bits to split from sum
	 */
	dpsum = (pixelsum - (thisblock/2) - 1)/thisblock;
	if (dpsum < 0) dpsum = 0.0;
	psum = ((unsigned int) dpsum ) >> 1;
	for (fs = 0; psum>0; fs++) psum >>= 1;

	/*
	 * write the codes
	 * fsbits ID bits used to indicate split level
	 */
	if (fs >= fsmax) {
	  /* Special high entropy case when FS >= fsmax
	   * Just write pixel difference values directly, no Rice coding at all.
	   */
	  if (output_nbits(buffer, fsmax+1, fsbits) ) {
	    // no error message - buffer overrun is silent.
	    free(diff);
	    return(-1);
	  }
	  for (j=0; j<thisblock; j++) {
	    if (output_nbits(buffer, diff[j], bbits) ) {
	      free(diff);
	      return(-1);
	    }
	  }
	} else if (fs == 0 && pixelsum == 0) {
	  /*
	   * special low entropy case when FS = 0 and pixelsum=0 (all
	   * pixels in block are zero.)
	   * Output a 0 and return
	   */
	  if (output_nbits(buffer, 0, fsbits) ) {
	    free(diff);
	    return(-1);
	  }
	} else {
	  /* normal case: not either very high or very low entropy */
	  if (output_nbits(buffer, fs+1, fsbits) ) {
	    free(diff);
	    return(-1);
	  }
	  fsmask = (1<<fs) - 1;
	  /*
	   * local copies of bit buffer to improve optimization
	   */
	  lbitbuffer = buffer->bitbuffer;
	  lbits_to_go = buffer->bits_to_go;
	  for (j=0; j<thisblock; j++) {
	    v = diff[j];
	    top = v >> fs;
	    /*
	     * top is coded by top zeros + 1
	     */
	    if (lbits_to_go >= top+1) {
	      lbitbuffer <<= top+1;
	      lbitbuffer |= 1;
	      lbits_to_go -= top+1;
	    } else {
	      lbitbuffer <<= lbits_to_go;
	      putcbuf(lbitbuffer & 0xff,buffer);
	      
	      for (top -= lbits_to_go; top>=8; top -= 8) {
		putcbuf(0, buffer);
	      }
	      lbitbuffer = 1;
	      lbits_to_go = 7-top;
	    }
	    /*
	     * bottom FS bits are written without coding
	     * code is output_nbits, moved into this routine to reduce overheads
	     * This code potentially breaks if FS>24, so I am limiting
	     * FS to 24 by choice of FSMAX above.
	     */
	    if (fs > 0) {
	      lbitbuffer <<= fs;
	      lbitbuffer |= v & fsmask;
	      lbits_to_go -= fs;
	      while (lbits_to_go <= 0) {
		putcbuf((lbitbuffer>>(-lbits_to_go)) & 0xff,buffer);
		lbits_to_go += 8;
	      }
	    }
	  }
	  
	  /* check if overflowed output buffer */
	  if (buffer->current > buffer->end) {
	    free(diff);
	    return(-1);
	  }
	  buffer->bitbuffer = lbitbuffer;
	  buffer->bits_to_go = lbits_to_go;
	}
 }
 done_outputing_bits(buffer);
 free(diff);
 /*
  * return number of bytes used
  */
 return(buffer->current - buffer->start);
}

/*---------------------------------------------------------------------------*/
/* bit_output.c
 *
 * Bit output routines
 * Procedures return zero on success, EOF on end-of-buffer
 *
 * Programmer: R. White     Date: 20 July 1998
 */

/* Initialize for bit output */

static void start_outputing_bits(Buffer *buffer)
{
    /*
     * Buffer is empty to start with
     */
    buffer->bitbuffer = 0;
    buffer->bits_to_go = 8;
}

/*---------------------------------------------------------------------------*/
/* Output N bits (N must be <= 32) */

static int output_nbits(Buffer *buffer, int bits, int n)
{
/* local copies */
int lbitbuffer;
int lbits_to_go;
    /* AND mask for the right-most n bits */
    static unsigned int mask[33] = 
         {0,
	  0x1,       0x3,       0x7,       0xf,       0x1f,       0x3f,       0x7f,       0xff,
	  0x1ff,     0x3ff,     0x7ff,     0xfff,     0x1fff,     0x3fff,     0x7fff,     0xffff,
	  0x1ffff,   0x3ffff,   0x7ffff,   0xfffff,   0x1fffff,   0x3fffff,   0x7fffff,   0xffffff,
	  0x1ffffff, 0x3ffffff, 0x7ffffff, 0xfffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff, 0xffffffff};

    /*
     * insert bits at end of bitbuffer
     */

    lbitbuffer = buffer->bitbuffer;
    lbits_to_go = buffer->bits_to_go;
    if (lbits_to_go+n > 32) {
	/*
	 * special case for large n: put out the top lbits_to_go bits first
	 * note that 0 < lbits_to_go <= 8
	 */
	lbitbuffer <<= lbits_to_go;
/*	lbitbuffer |= (bits>>(n-lbits_to_go)) & ((1<<lbits_to_go)-1); */
	lbitbuffer |= (bits>>(n-lbits_to_go)) & *(mask+lbits_to_go);

	if(buffer->current >= buffer->end - 1)
	  return 1;

	putcbuf(lbitbuffer & 0xff,buffer);
	n -= lbits_to_go;
	lbits_to_go = 8;
    }
    lbitbuffer <<= n;
/*    lbitbuffer |= ( bits & ((1<<n)-1) ); */
    lbitbuffer |= ( bits & *(mask+n) );
    lbits_to_go -= n;
    while (lbits_to_go <= 0) {
	/*
	 * bitbuffer full, put out top 8 bits
	 */
      if(buffer->current >= buffer->end)
	return 1;
      
      putcbuf((lbitbuffer>>(-lbits_to_go)) & 0xff,buffer);
      lbits_to_go += 8;
    }
    buffer->bitbuffer = lbitbuffer;
    buffer->bits_to_go = lbits_to_go;

      if(buffer->bits_to_go < 8 && buffer->current >= buffer->end -2)
	return 1;

    return(0);
}
/*---------------------------------------------------------------------------*/
/* Flush out the last bits */

static int done_outputing_bits(Buffer *buffer)
{
    if(buffer->bits_to_go < 8) {
	putcbuf(buffer->bitbuffer<<buffer->bits_to_go,buffer);
    }
    return(0);
}


/**********************************************************************
 * rdecomp
 * 
 * Usage:
 *   errflag = rdecomp(a, clen, outbuf, sampsiz, nx, nblock)
 * 
 *   a is a pointer to the input buffer, which contains rice-compressed
 *   data (e.g. from rcomp, above).
 * 
 *   clen is the length of the input buffer, in bytes.
 * 
 *   outbuf is a pointer to the output buffer, which should be 
 *   a pre-allocated array of chars, shorts, or longs according to 
 *   sampsiz.
 * 
 *   sampsiz tells the sample size in bytes (1, 2, or 4)
 * 
 *   nx tells the number of samples in the output buffer (which are
 *   all expected to be present in the compressed stream).
 *
 *   nblock is the block size, in samples, for compression.
 * 
 *
 *   The data are decoded into the output buffer.  On normal completion
 *   0 is returned.
 */

int rdecomp (unsigned char *c,		/* input buffer			    */
	     int clen,			/* length of input (bytes)	    */
	     void *array,	        /* output array		 	    */
	     int bsize,                 /* bsize - bytes per pix of output  */
	     int nx,			/* number of output pixels          */
	     int nblock)		/* coding block size (in pixels)    */
{
  int i, k, imax;
  int nbits, nzero, fs;
  unsigned char *cend, bytevalue;
  unsigned int b, diff, lastpix;
  int fsmax, fsbits, bbits;
  static int *nonzero_count = (int *)NULL;
  
  /*
   * From bsize derive:
   * FSBITS = # bits required to store FS
   * FSMAX = maximum value for FS
   * BBITS = bits/pixel for direct coding
   *
   * (These magic numbers have to match the ones in rcomp above.)
   */
  
  
  switch (bsize) {
  case 1:
    fsbits = 3;
    fsmax = 6;
    break;
  case 2:
    fsbits = 4;
    fsmax = 14;
    break;
  case 4:
    fsbits = 5;
    fsmax = 25;
    break;
  default:
    fprintf(stderr,"rdecomp: bsize must be 1, 2, or 4 bytes");
    fflush(stderr);
    return 1;
  }
  
  bbits = 1<<fsbits;

  if (nonzero_count == (int *) NULL) {
    /*
     * nonzero_count is lookup table giving number of bits
     * in 8-bit values not including leading zeros; gets allocated 
     * and calculated the first time through
     */
    
    /*  NOTE!!!  This memory never gets freed (permanent table)  */
    nonzero_count = (int *) malloc(256*sizeof(int));
    if (nonzero_count == (int *) NULL) {
      fprintf(stderr,"rdecomp: insufficient memory!\n");
      fflush(stderr);
      return 1;
    }
    nzero = 8;
    k = 128;
    for (i=255; i>=0; ) {
      for ( ; i>=k; i--) nonzero_count[i] = nzero;
      k = k/2;
      nzero--;
    }
  }

    /*
     * Decode in blocks of nblock pixels
     */

    /* first bytes of input buffer contain the value of the first */
    /* integer value, without any encoding */


    cend = c + clen;
    
    lastpix = 0;
    switch(bsize) {
    case 4:
      bytevalue = c[0];
      lastpix = lastpix | (bytevalue<<24);
      bytevalue = c[1];
      lastpix = lastpix | (bytevalue<<16);
      bytevalue = c[2];
      lastpix = lastpix | (bytevalue<<8);
      bytevalue = c[3];
      lastpix = lastpix | bytevalue;
      c+=4;
      break;
    case 2:
      bytevalue = c[0];
      lastpix = lastpix | (bytevalue<<8);
      bytevalue = c[1];
      lastpix = lastpix | bytevalue;
      c+=2;
      break;
    case 1:
      lastpix = c[0];
      c++;
      break;
    default: // never happens
      break; 
    }

    b = *c++;		    /* bit buffer			*/
    nbits = 8;		    /* number of bits remaining in b	*/
    for (i = 0; i<nx; ) {
	/* get the FS value from first fsbits */
	nbits -= fsbits;
	while (nbits < 0) {
	    b = (b<<8) | (*c++);
	    nbits += 8;
	}
	fs = (b >> nbits) - 1;

	b &= (1<<nbits)-1;
	/* loop over the next block */
	imax = i + nblock;
	if (imax > nx) imax = nx;
	if (fs<0) {
	    /* low-entropy case, all zero differences */
	  for ( ; i<imax; i++) {
	    switch(bsize) {
	    case 1: ((char *)array)[i] = lastpix; break;
	    case 2: ((short *)array)[i] = lastpix; break;
	    case 4: ((int *)array)[i] = lastpix; break;
	    default: break;
	    }
	  }
	} else if (fs==fsmax) {
	    /* high-entropy case, directly coded pixel values */
	    for ( ; i<imax; i++) {
		k = bbits - nbits;
		diff = b<<k;
		for (k -= 8; k >= 0; k -= 8) {
		    b = *c++;
		    diff |= b<<k;
		}
		if (nbits>0) {
		    b = *c++;
		    diff |= b>>(-k);
		    b &= (1<<nbits)-1;
		} else {
		    b = 0;
		}
		/*
		 * undo mapping and differencing
		 * Note that some of these operations will overflow the
		 * unsigned int arithmetic -- that's OK, it all works
		 * out to give the right answers in the output file.
		 */
		if ((diff & 1) == 0) {
		    diff = diff>>1;
		} else {
		    diff = ~(diff>>1);
		}

		switch(bsize) {
		case 1: 
		  ((char *)array)[i] = diff + lastpix; 
		  lastpix = ((char *)array)[i];
		  break;
		case 2: 
		  ((short *)array)[i] = diff + lastpix;
		  lastpix = ((short *)array)[i];
		  break;
		case 4:
		  ((int *)array)[i] = diff + lastpix;
		  lastpix = ((int *)array)[i];
		  break;
		default: // never happens
		  break;
		}
	    }
	} else {
	    /* normal case, Rice coding */
	    for ( ; i<imax; i++) {
		/* count number of leading zeros */
		while (b == 0) {
		    nbits += 8;
		    b = *c++;
		}
		nzero = nbits - nonzero_count[b];
		nbits -= nzero+1;
		/* flip the leading one-bit */
		b ^= 1<<nbits;
		/* get the FS trailing bits */
		nbits -= fs;
		while (nbits < 0) {
		    b = (b<<8) | (*c++);
		    nbits += 8;
		}
		diff = (nzero<<fs) | (b>>nbits);
		b &= (1<<nbits)-1;

		/* undo mapping and differencing */
		if ((diff & 1) == 0) {
		    diff = diff>>1;
		} else {
		    diff = ~(diff>>1);
		}

		switch(bsize) {
		case 1: 
		  ((char *)array)[i] = diff + lastpix; 
		  lastpix = ((char *)array)[i];
		  break;
		case 2: 
		  ((short *)array)[i] = diff + lastpix;
		  lastpix = ((short *)array)[i];
		  break;
		case 4:
		  ((int *)array)[i] = diff + lastpix;
		  lastpix = ((int *)array)[i];
		  break;
		default: // never happens
		  break;
		}
	    }
	}
	if (c > cend) {
	  fprintf(stderr,"rdecomp: decompression error: hit end of compressed byte stream\n");
	  fflush(stderr);
	  return 1;
	}
    }
    return 0;
}

