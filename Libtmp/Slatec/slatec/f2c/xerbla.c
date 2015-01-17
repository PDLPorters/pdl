/* xerbla.f -- translated by f2c (version 20060506).
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

/* Table of constant values */

static integer c__1 = 1;
static integer c__5 = 5;

/* DECK XERBLA */
/* Subroutine */ int xerbla_(char *srname, integer *info, ftnlen srname_len)
{
    /* System generated locals */
    address a__1[5];
    integer i__1[5];
    char ch__1[59];

    /* Builtin functions */
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    ;
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static char xern1[2];
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

    /* Fortran I/O blocks */
    static icilist io___2 = { 0, xern1, 0, "(I2)", 2, 1 };


/* ***BEGIN PROLOGUE  XERBLA */
/* ***SUBSIDIARY */
/* ***PURPOSE  Error handler for the Level 2 and Level 3 BLAS Routines. */
/* ***LIBRARY   SLATEC */
/* ***CATEGORY  R3 */
/* ***TYPE      ALL (XERBLA-A) */
/* ***KEYWORDS  ERROR MESSAGE */
/* ***AUTHOR  Dongarra, J. J., (ANL) */
/* ***DESCRIPTION */

/*  Purpose */
/*  ======= */

/*  It is called by Level 2 and 3 BLAS routines if an input parameter */
/*  is invalid. */

/*  Parameters */
/*  ========== */

/*  SRNAME - CHARACTER*6. */
/*           On entry, SRNAME specifies the name of the routine which */
/*           called XERBLA. */

/*  INFO   - INTEGER. */
/*           On entry, INFO specifies the position of the invalid */
/*           parameter in the parameter-list of the calling routine. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   860720  DATE WRITTEN */
/*   910610  Routine rewritten to serve as an interface between the */
/*           Level 2 and Level 3 BLAS routines and the SLATEC error */
/*           handler XERMSG.  (BKS) */
/* ***END PROLOGUE  XERBLA */

/*     ..    Scalar Arguments .. */

/* ***FIRST EXECUTABLE STATEMENT  XERBLA */

    s_wsfi(&io___2);
    do_fio(&c__1, (char *)&(*info), (ftnlen)sizeof(integer));
    e_wsfi();
/* Writing concatenation */
    i__1[0] = 12, a__1[0] = "On entry to ";
    i__1[1] = 6, a__1[1] = srname;
    i__1[2] = 18, a__1[2] = " parameter number ";
    i__1[3] = 2, a__1[3] = xern1;
    i__1[4] = 21, a__1[4] = " had an illegal value";
    s_cat(ch__1, a__1, i__1, &c__5, (ftnlen)59);
    xermsg_("SLATEC", srname, ch__1, info, &c__1, (ftnlen)6, (ftnlen)6, (
	    ftnlen)59);

    return 0;

/*     End of XERBLA. */

} /* xerbla_ */

