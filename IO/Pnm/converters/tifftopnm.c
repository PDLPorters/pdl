/*
** tifftopnm.c - converts a Tagged Image File to a portable anymap
**
** Derived by Jef Poskanzer from tif2ras.c, which is:
**
** Copyright (c) 1990 by Sun Microsystems, Inc.
**
** Author: Patrick J. Naughton
** naughton@wind.sun.com
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted,
** provided that the above copyright notice appear in all copies and that
** both that copyright notice and this permission notice appear in
** supporting documentation.
**
** This file is provided AS IS with no warranties of any kind.  The author
** shall have no liability with respect to the infringement of copyrights,
** trade secrets or any patents by this file or any part thereof.  In no
** event will the author be liable for any lost revenue or profits or
** other special, indirect and consequential damages.
*/

#include "pnm.h"
#ifdef VMS
#ifdef SYSV
#undef SYSV
#endif
#include <tiffioP.h>
#endif
#include <tiffio.h>

#define MAXCOLORS 1024
#ifndef PHOTOMETRIC_DEPTH
#define PHOTOMETRIC_DEPTH 32768
#endif

static int
checkcmap(unsigned short *r, unsigned short *g, unsigned short *b,
	  unsigned short bps)
{
    long n = 1L<<bps;

    while (n-- > 0)
	if (*r++ >= 256 || *g++ >= 256 || *b++ >= 256)
	    return (16);
    return (8);
}

int
main( argc, argv )
    int argc;
    char* argv[];
    {
    int argn, cols, rows, grayscale, format;
    uint16 plan;
    int numcolors;
    register TIFF* tif;
    int row, i;
    register int col;
    u_char* buf,*bufr,*bufg,*bufb;
    register u_char* inP;
    int maxval;
    xel* xelrow;
    register xel* xP;
    xel colormap[MAXCOLORS];
    int headerdump, scale8bp, nolut;
    register u_char sample;
    register int bitsleft;
    unsigned short bps, spp, photomet;
    unsigned short* redcolormap;
    unsigned short* greencolormap;
    unsigned short* bluecolormap;
    char* usage = "[-headerdump -8bpalette -nolut] [tifffile]";
    tsize_t sz;

    pnm_init( &argc, argv );

    argn = 1;
    headerdump = 0;
    scale8bp = 0;
    nolut = 0;

    while ( argn < argc && argv[argn][0] == '-' && argv[argn][1] != '\0' )
	{
	if ( pm_keymatch( argv[argn], "-headerdump", 2 ) )
	    headerdump = 1;
	else if ( pm_keymatch( argv[argn], "-8bpalette", 2 ) )
	    scale8bp = 1;
       else if ( pm_keymatch( argv[argn], "-nolut", 2 ) )
           nolut = 1;
	else
	    pm_usage( usage );
	++argn;
	}

    if ( argn != argc )
	{
	tif = TIFFOpen( argv[argn], "r" );
	if ( tif == NULL )
	    pm_error( "error opening TIFF file %s", argv[argn] );
	++argn;
	}
    else
	{
	tif = TIFFFdOpen( 0, "Standard Input", "r" );
	if ( tif == NULL )
	    pm_error( "error opening standard input as TIFF file" );
	}

    if ( argn != argc )
	pm_usage( usage );

    if ( headerdump )
	TIFFPrintDirectory( tif, stderr, TIFFPRINT_NONE );

    if ( ! TIFFGetField( tif, TIFFTAG_BITSPERSAMPLE, &bps ) )
	bps = 1;
    if ( ! TIFFGetField( tif, TIFFTAG_SAMPLESPERPIXEL, &spp ) )
	spp = 1;
    if ( ! TIFFGetField( tif, TIFFTAG_PHOTOMETRIC, &photomet ) )
	pm_error( "error getting photometric" );

    switch ( spp )
	{
	case 1:
	case 3:
	case 4:
	break;

	default:
	pm_error(
	    "can only handle 1-channel gray scale or 1- or 3-channel color" );
	}

    (void) TIFFGetField( tif, TIFFTAG_IMAGEWIDTH, &cols );
    (void) TIFFGetField( tif, TIFFTAG_IMAGELENGTH, &rows );
    TIFFGetField( tif, TIFFTAG_PLANARCONFIG, &plan);

    if ( headerdump )
	{
	pm_message( "%dx%dx%d image", cols, rows, bps * spp );
	pm_message( "%d bits/sample, %d samples/pixel", bps, spp );
	}

    maxval = ( 1 << bps ) - 1;
    if ( maxval == 1 && spp == 1 )
	{
	if ( headerdump )
	    pm_message("monochrome" );
	grayscale = 1;
	}
    else
	{
	switch ( photomet )
	    {
	    case PHOTOMETRIC_MINISBLACK:
	    if ( headerdump )
		pm_message( "%d graylevels (min=black)", maxval + 1 );
	    grayscale = 1;
	    break;

	    case PHOTOMETRIC_MINISWHITE:
	    if ( headerdump )
		pm_message( "%d graylevels (min=white)", maxval + 1 );
	    grayscale = 1;
	    break;

	    case PHOTOMETRIC_PALETTE:
	    if ( headerdump )
             pm_message( "colormapped" );
           if (!nolut) {
             if ( ! TIFFGetField( tif, TIFFTAG_COLORMAP, &redcolormap, &greencolormap, &bluecolormap ) )
		pm_error( "error getting colormaps" );
             numcolors = maxval + 1;
             if ( numcolors > MAXCOLORS )
		pm_error( "too many colors" );
             maxval = PNM_MAXMAXVAL;
             grayscale = 0;
             /* do the conversion only if necessary due to PNM_MAXMAXVAL
                being smaller than the largest possible colormap entry,
                saves you trouble with rounding errors, etc. (CS) */
             if (PNM_MAXMAXVAL < 65535L)
		for ( i = 0; i < numcolors; ++i )
		  {
		    register xelval r, g, b;
                   r = (long) redcolormap[i] * PNM_MAXMAXVAL / 65535L;
                   g = (long) greencolormap[i] * PNM_MAXMAXVAL / 65535L;
                   b = (long) bluecolormap[i] * PNM_MAXMAXVAL / 65535L;
		    PPM_ASSIGN( colormap[i], r, g, b );
                   fprintf(stderr,"in: %hu, out: %d\n",greencolormap[i],g);
		  }
	      else
               if (checkcmap(redcolormap,greencolormap,bluecolormap,bps) == 16
                   && scale8bp) 
                 for ( i = 0; i < numcolors; ++i )
                   {
                     register xelval r, g, b;
                     r = redcolormap[i] >> 8;
                     g = greencolormap[i] >> 8;
                     b = bluecolormap[i] >> 8;
                     PPM_ASSIGN( colormap[i], r, g, b );
                     fprintf(stderr,"case2: in: %hu, out: %d\n",
                             greencolormap[i],g);
                   }
               else
                 for ( i = 0; i < numcolors; ++i )
                   {
                     register xelval r, g, b;
                     r = redcolormap[i];
                     g = greencolormap[i];
                     b = bluecolormap[i];
                     PPM_ASSIGN( colormap[i], r, g, b );
                     fprintf(stderr,"case2: in: %hu, out: %d\n",
                             greencolormap[i],g);
                   }
           } else
             grayscale = 1;
	    break;

	    case PHOTOMETRIC_RGB:
	    if ( headerdump )
		pm_message( "truecolor" );
	    grayscale = 0;
	    break;

	    case PHOTOMETRIC_MASK:
	    pm_error( "don't know how to handle PHOTOMETRIC_MASK" );

	    case PHOTOMETRIC_DEPTH:
	    pm_error( "don't know how to handle PHOTOMETRIC_DEPTH" );

	    default:
	    pm_error( "unknown photometric: %d", photomet );
	    }
	}
    if ( maxval > PNM_MAXMAXVAL )
	pm_error(
"bits/sample is too large - try reconfiguring with PGM_BIGGRAYS\n    or without PPM_PACKCOLORS" );


    if ( grayscale )
	{
	if ( maxval == 1 )
	    {
	    format = PBM_TYPE;
	    pm_message( "writing PBM file" );
	    }
	else
	    {
	    format = PGM_TYPE;
	    pm_message( "writing PGM file" );
	    }
	}
    else
	{
	format = PPM_TYPE;
	pm_message( "writing PPM file" );
	}

    pm_message( "The scanlinesize is %d\n",TIFFScanlineSize(tif));
    pm_message( "With planar configuration %u\n",plan);

    sz = TIFFScanlineSize(tif);
    if (plan == 2)
      sz *= spp;
    buf = (u_char*) malloc(sz);
    if (plan == 2)
      {
	tsize_t szl = TIFFScanlineSize(tif);
	bufr = buf;
	bufg = buf + szl;
	bufb = bufg + szl;
      }

    if ( buf == NULL )
	pm_error( "can't allocate memory for scanline buffer" );
    pnm_writepnminit( stdout, cols, rows, (xelval) maxval, format, 0 );
    xelrow = pnm_allocrow( cols );

#define NEXTSAMPLE \
    { \
    if ( bitsleft == 0 ) \
	{ \
	++inP; \
	bitsleft = 8; \
	} \
    bitsleft -= bps; \
    sample = ( *inP >> bitsleft ) & maxval; \
    }

    for ( row = 0; row < rows; ++row )
	{
	  if (plan ==2 && photomet == PHOTOMETRIC_RGB) {
	    if ( TIFFReadScanline( tif, bufr, row, 0 ) < 0 )
	      pm_error( "bad data read on line %d", row );
	    if ( TIFFReadScanline( tif, bufg, row, 1 ) < 0 )
	      pm_error( "bad data read on line %d", row );
	    if ( TIFFReadScanline( tif, bufb, row, 2 ) < 0 )
	      pm_error( "bad data read on line %d", row );
	  }
	  else
	    if ( TIFFReadScanline( tif, buf, row, 0 ) < 0 )
	      pm_error( "bad data read on line %d", row );

	inP = buf;
	bitsleft = 8;
	xP = xelrow;

	switch ( photomet )
	    {
	    case PHOTOMETRIC_MINISBLACK:
	    for ( col = 0; col < cols; ++col, ++xP )
		{
		NEXTSAMPLE
		PNM_ASSIGN1( *xP, sample );
		}
	    break;

	    case PHOTOMETRIC_MINISWHITE:
	    for ( col = 0; col < cols; ++col, ++xP )
		{
		NEXTSAMPLE
		sample = maxval - sample;
		PNM_ASSIGN1( *xP, sample );
		}
	    break;

	    case PHOTOMETRIC_PALETTE:
             if (!nolut)
               for ( col = 0; col < cols; ++col, ++xP )
                 {
                   NEXTSAMPLE
                     *xP = colormap[sample];
                 }
             else 
               for ( col = 0; col < cols; ++col, ++xP )
                 {
                   NEXTSAMPLE
                     PNM_ASSIGN1( *xP, sample );
                 }
	    break;

	    case PHOTOMETRIC_RGB:
	    for ( col = 0; col < cols; ++col, ++xP )
		{
		register xelval r, g, b;

		if (plan != 2) {
		  NEXTSAMPLE
		    r = sample;
		  NEXTSAMPLE
		    g = sample;
		  NEXTSAMPLE
		    b = sample;
		  if ( spp == 4 )
		    NEXTSAMPLE		/* skip alpha channel */
		      }
		else {
		  r = bufr[col];
		  g = bufg[col];
		  b = bufb[col];
		}
		PPM_ASSIGN( *xP, r, g, b );
		}
	    break;

	    default:
	    pm_error( "unknown photometric: %d", photomet );
	    }
	pnm_writepnmrow( stdout, xelrow, cols, (xelval) maxval, format, 0 );
	}

    pm_close( stdout );
    exit( 0 );
    }
