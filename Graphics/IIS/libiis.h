
/*
   Note: libiis.h in PDL distribution has iis_display
   redefined
*/

/*

  libiis.h - IIS constants for IIS C library v1.0

  Loosely based on code from various sources

  Karl Glazebrook, Anglo-Australian Observatory 30/May/1996

  email: kgb@aaoepp.aao.gov.au

*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#define TRANSFER_ID	0
#define THING_COUNT	1
#define	SUB_UNIT	2
#define	CHECK_SUM	3
#define	X_REGISTER	4
#define Y_REGISTER	5
#define Z_REGISTER	6
#define T_REGISTER	7

/* Transfer ID definitions */

#define  IREAD             0100000
#define  IWRITE                 00
#define  PACKED             040000
#define  BYPASSIFM          020000
#define  BYTE               010000
#define  ADDWRITE            04000
#define  ACCUM               02000
#define  BLOCKXFER           01000
#define  VRETRACE             0400
#define  MUX32                0200
#define  IMT800               0100

/* Subunits */

#define  REFRESH                 01
#define  LUT                     02
#define  OFM                     03
#define  IFM                     04
#define  FEEDBACK                05
#define  SCROLL                  06
#define  VIDEOM                  07
#define  SUMPROC                 010
#define  GRAPHICS                011
#define  CURSOR                  012
#define  ALU                     013
#define  ZOOM                    014
#define  IPB                     017

/* following from iraf "iis.h" */
#define  IMCURSOR                020
#define  WCS	                 021

/* checksum */
#define	CHECKSUMVAL		0177777

/* Command definitions from iraf iis.h */

#define  COMMAND           100000B


/* X-register */
#define  ADVXONTC          0100000
#define  ADVXONYOV          040000

/* Y-register */

#define  ADVYONXOV         0100000
#define  ADVYONTC           040000

/* Z-register */

#define  CHAN1                  01
#define  CHAN2                  02
#define  CHAN3                  04
#define  CHAN4                 010
#define  GRCHAN            0100000

/* T-register */

#define  BITPL0                 01
#define  BITPL1                 02
#define  BITPL2                 04
#define  BITPL3                010
#define  BITPL4                020
#define  BITPL5                040
#define  BITPL6               0100
#define  BITPL7               0200
#define  ALLBITPL             0377

/* IIS WCS buffer */


/* Imtool colour numbers */

#define FMIN                    1
#define	FMAX			200
#define	IIS_GREEN		201
#define IIS_BLACK		202
#define	IIS_WHITE		203
#define IIS_RED			204
#define IIS_BLUE		206
#define IIS_YELLOW		207

/* Buffer sizes */

#define BUFFSZ    16384  /* Approx number of bytes to transfer per record */
#define RBUFFSZ    2048  /* Approx number of bytes to transfer when reading */
#define STRSIZE    1024  /* Useful string size */
#define SZ_WCSTEXT  320  /* WCS text */

/* Global variables */

static int iispipe_i;   /* FIFO pipe handles */
static int iispipe_o;
static int fbconfig;    /* Frame buffer configaration */
static int frameX;
static int frameY;

/* Public functions prototypes */

int  iis_display(void *f, int datatype,
         int N1, int N2, float fmin, float fmax, int frame);
void iis_cur(float*x, float*y, char* ch);
void iis_drawcirc(float xcen, float ycen, float radius, int colour, int frame);
void iis_open(char* inpipe, char* outpipe, int fb, int fbx, int fby);
void iis_close();

/* Private functions prototypes */

void iis_write(char* buf, int size);
void iis_read (char* buf, int size);
void iis_checksum(unsigned short *hdr);
void iis_error(char* error1, char* error2);
int iis_chan(int frame);
int iis_round ( float i );
float iis_abs (float x);


