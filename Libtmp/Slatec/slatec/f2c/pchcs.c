/* pchcs.f -- translated by f2c (version 20060506).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* DECK PCHCS */
/* Subroutine */ int pchcs_(real *switch__, integer *n, real *h__, real *
	slope, real *d__, integer *incfd, integer *ierr)
{
    /* Initialized data */

    static real zero = 0.f;
    static real one = 1.f;
    static real fudge = 4.f;

    /* System generated locals */
    integer d_dim1, d_offset, i__1;
    real r__1, r__2, r__3;

    /* Local variables */
    static integer i__, k;
    static real del[3], fact, dfmx;
    static integer indx;
    static real dext, dfloc;
    extern doublereal pchst_(real *, real *);
    static real slmax;
    extern /* Subroutine */ int pchsw_(real *, integer *, real *, real *, 
	    real *, real *, integer *);
    static real wtave[2];
    static integer nless1;

/* ***BEGIN PROLOGUE  PCHCS */
/* ***SUBSIDIARY */
/* ***PURPOSE  Adjusts derivative values for PCHIC */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***TYPE      SINGLE PRECISION (PCHCS-S, DPCHCS-D) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/* ***DESCRIPTION */

/*         PCHCS:  PCHIC Monotonicity Switch Derivative Setter. */

/*     Called by  PCHIC  to adjust the values of D in the vicinity of a */
/*     switch in direction of monotonicity, to produce a more "visually */
/*     pleasing" curve than that given by  PCHIM . */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, IERR */
/*        REAL  SWITCH, H(N), SLOPE(N), D(INCFD,N) */

/*        CALL  PCHCS (SWITCH, N, H, SLOPE, D, INCFD, IERR) */

/*   Parameters: */

/*     SWITCH -- (input) indicates the amount of control desired over */
/*           local excursions from data. */

/*     N -- (input) number of data points.  (assumes N.GT.2 .) */

/*     H -- (input) real array of interval lengths. */
/*     SLOPE -- (input) real array of data slopes. */
/*           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are: */
/*                  H(I) =  X(I+1)-X(I), */
/*              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1. */

/*     D -- (input) real array of derivative values at the data points, */
/*           as determined by PCHCI. */
/*          (output) derivatives in the vicinity of switches in direction */
/*           of monotonicity may be adjusted to produce a more "visually */
/*           pleasing" curve. */
/*           The value corresponding to X(I) is stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*           No other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in D. */
/*           This argument is provided primarily for 2-D applications. */

/*     IERR -- (output) error flag.  should be zero. */
/*           If negative, trouble in PCHSW.  (should never happen.) */

/*    ------- */
/*    WARNING:  This routine does no validity-checking of arguments. */
/*    ------- */

/*  Fortran intrinsics used:  ABS, MAX, MIN. */

/* ***SEE ALSO  PCHIC */
/* ***ROUTINES CALLED  PCHST, PCHSW */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820617  Redesigned to (1) fix  problem with lack of continuity */
/*           approaching a flat-topped peak (2) be cleaner and */
/*           easier to verify. */
/*           Eliminated subroutines PCHSA and PCHSX in the process. */
/*   820622  1. Limited fact to not exceed one, so computed D is a */
/*             convex combination of PCHCI value and PCHSD value. */
/*           2. Changed fudge from 1 to 4 (based on experiments). */
/*   820623  Moved PCHSD to an inline function (eliminating MSWTYP). */
/*   820805  Converted to SLATEC library version. */
/*   870813  Minor cosmetic changes. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR section in prologue.  (WRB) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  PCHCS */

/*  Programming notes: */
/*     1. The function  PCHST(ARG1,ARG2)  is assumed to return zero if */
/*        either argument is zero, +1 if they are of the same sign, and */
/*        -1 if they are of opposite sign. */
/* **End */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  DEFINE INLINE FUNCTION FOR WEIGHTED AVERAGE OF SLOPES. */


/*  INITIALIZE. */

    /* Parameter adjustments */
    --h__;
    --slope;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;

    /* Function Body */
/* ***FIRST EXECUTABLE STATEMENT  PCHCS */
    *ierr = 0;
    nless1 = *n - 1;

/*  LOOP OVER SEGMENTS. */

    i__1 = nless1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if ((r__1 = pchst_(&slope[i__ - 1], &slope[i__])) < 0.f) {
	    goto L100;
	} else if (r__1 == 0) {
	    goto L300;
	} else {
	    goto L900;
	}
/*             -------------------------- */

L100:

/* ....... SLOPE SWITCHES MONOTONICITY AT I-TH POINT ..................... */

/*           DO NOT CHANGE D IF 'UP-DOWN-UP'. */
	if (i__ > 2) {
	    if (pchst_(&slope[i__ - 2], &slope[i__]) > zero) {
		goto L900;
	    }
/*                   -------------------------- */
	}
	if (i__ < nless1) {
	    if (pchst_(&slope[i__ + 1], &slope[i__ - 1]) > zero) {
		goto L900;
	    }
/*                   ---------------------------- */
	}

/*   ....... COMPUTE PROVISIONAL VALUE FOR D(1,I). */

	dext = h__[i__] / (h__[i__ - 1] + h__[i__]) * slope[i__ - 1] + h__[
		i__ - 1] / (h__[i__ - 1] + h__[i__]) * slope[i__];

/*   ....... DETERMINE WHICH INTERVAL CONTAINS THE EXTREMUM. */

	if ((r__1 = pchst_(&dext, &slope[i__ - 1])) < 0.f) {
	    goto L200;
	} else if (r__1 == 0) {
	    goto L900;
	} else {
	    goto L250;
	}
/*                ----------------------- */

L200:
/*              DEXT AND SLOPE(I-1) HAVE OPPOSITE SIGNS -- */
/*                        EXTREMUM IS IN (X(I-1),X(I)). */
	k = i__ - 1;
/*              SET UP TO COMPUTE NEW VALUES FOR D(1,I-1) AND D(1,I). */
	wtave[1] = dext;
	if (k > 1) {
	    wtave[0] = h__[k] / (h__[k - 1] + h__[k]) * slope[k - 1] + h__[k 
		    - 1] / (h__[k - 1] + h__[k]) * slope[k];
	}
	goto L400;

L250:
/*              DEXT AND SLOPE(I) HAVE OPPOSITE SIGNS -- */
/*                        EXTREMUM IS IN (X(I),X(I+1)). */
	k = i__;
/*              SET UP TO COMPUTE NEW VALUES FOR D(1,I) AND D(1,I+1). */
	wtave[0] = dext;
	if (k < nless1) {
	    wtave[1] = h__[k + 1] / (h__[k] + h__[k + 1]) * slope[k] + h__[k] 
		    / (h__[k] + h__[k + 1]) * slope[k + 1];
	}
	goto L400;

L300:

/* ....... AT LEAST ONE OF SLOPE(I-1) AND SLOPE(I) IS ZERO -- */
/*                     CHECK FOR FLAT-TOPPED PEAK ....................... */

	if (i__ == nless1) {
	    goto L900;
	}
	if (pchst_(&slope[i__ - 1], &slope[i__ + 1]) >= zero) {
	    goto L900;
	}
/*                ----------------------------- */

/*           WE HAVE FLAT-TOPPED PEAK ON (X(I),X(I+1)). */
	k = i__;
/*           SET UP TO COMPUTE NEW VALUES FOR D(1,I) AND D(1,I+1). */
	wtave[0] = h__[k] / (h__[k - 1] + h__[k]) * slope[k - 1] + h__[k - 1] 
		/ (h__[k - 1] + h__[k]) * slope[k];
	wtave[1] = h__[k + 1] / (h__[k] + h__[k + 1]) * slope[k] + h__[k] / (
		h__[k] + h__[k + 1]) * slope[k + 1];

L400:

/* ....... AT THIS POINT WE HAVE DETERMINED THAT THERE WILL BE AN EXTREMUM */
/*        ON (X(K),X(K+1)), WHERE K=I OR I-1, AND HAVE SET ARRAY WTAVE-- */
/*           WTAVE(1) IS A WEIGHTED AVERAGE OF SLOPE(K-1) AND SLOPE(K), */
/*                    IF K.GT.1 */
/*           WTAVE(2) IS A WEIGHTED AVERAGE OF SLOPE(K) AND SLOPE(K+1), */
/*                    IF K.LT.N-1 */

	slmax = (r__1 = slope[k], dabs(r__1));
	if (k > 1) {
/* Computing MAX */
	    r__2 = slmax, r__3 = (r__1 = slope[k - 1], dabs(r__1));
	    slmax = dmax(r__2,r__3);
	}
	if (k < nless1) {
/* Computing MAX */
	    r__2 = slmax, r__3 = (r__1 = slope[k + 1], dabs(r__1));
	    slmax = dmax(r__2,r__3);
	}

	if (k > 1) {
	    del[0] = slope[k - 1] / slmax;
	}
	del[1] = slope[k] / slmax;
	if (k < nless1) {
	    del[2] = slope[k + 1] / slmax;
	}

	if (k > 1 && k < nless1) {
/*           NORMAL CASE -- EXTREMUM IS NOT IN A BOUNDARY INTERVAL. */
	    fact = fudge * (r__1 = del[2] * (del[0] - del[1]) * (wtave[1] / 
		    slmax), dabs(r__1));
	    d__[k * d_dim1 + 1] += dmin(fact,one) * (wtave[0] - d__[k * 
		    d_dim1 + 1]);
	    fact = fudge * (r__1 = del[0] * (del[2] - del[1]) * (wtave[0] / 
		    slmax), dabs(r__1));
	    d__[(k + 1) * d_dim1 + 1] += dmin(fact,one) * (wtave[1] - d__[(k 
		    + 1) * d_dim1 + 1]);
	} else {
/*           SPECIAL CASE K=1 (WHICH CAN OCCUR ONLY IF I=2) OR */
/*                        K=NLESS1 (WHICH CAN OCCUR ONLY IF I=NLESS1). */
	    fact = fudge * dabs(del[1]);
	    d__[i__ * d_dim1 + 1] = dmin(fact,one) * wtave[i__ - k];
/*              NOTE THAT I-K+1 = 1 IF K=I  (=NLESS1), */
/*                        I-K+1 = 2 IF K=I-1(=1). */
	}


/* ....... ADJUST IF NECESSARY TO LIMIT EXCURSIONS FROM DATA. */

	if (*switch__ <= zero) {
	    goto L900;
	}

	dfloc = h__[k] * (r__1 = slope[k], dabs(r__1));
	if (k > 1) {
/* Computing MAX */
	    r__2 = dfloc, r__3 = h__[k - 1] * (r__1 = slope[k - 1], dabs(r__1)
		    );
	    dfloc = dmax(r__2,r__3);
	}
	if (k < nless1) {
/* Computing MAX */
	    r__2 = dfloc, r__3 = h__[k + 1] * (r__1 = slope[k + 1], dabs(r__1)
		    );
	    dfloc = dmax(r__2,r__3);
	}
	dfmx = *switch__ * dfloc;
	indx = i__ - k + 1;
/*        INDX = 1 IF K=I, 2 IF K=I-1. */
/*        --------------------------------------------------------------- */
	pchsw_(&dfmx, &indx, &d__[k * d_dim1 + 1], &d__[(k + 1) * d_dim1 + 1],
		 &h__[k], &slope[k], ierr);
/*        --------------------------------------------------------------- */
	if (*ierr != 0) {
	    return 0;
	}

/* ....... END OF SEGMENT LOOP. */

L900:
	;
    }

    return 0;
/* ------------- LAST LINE OF PCHCS FOLLOWS ------------------------------ */
} /* pchcs_ */

