#ifndef __PDLTHREAD_H
#define __PDLTHREAD_H

#define PDL_BROADCAST_MAGICKED 0x0001
#define PDL_BROADCAST_MAGICK_BUSY 0x0002
#define PDL_BROADCAST_INITIALIZED 0x0004

#define PDL_LIST_FLAGS_PDLBROADCAST(X) \
 X(PDL_BROADCAST_MAGICKED) \
 X(PDL_BROADCAST_MAGICK_BUSY) \
 X(PDL_BROADCAST_INITIALIZED)

#define PDL_BRC_MAGICNO 0x92314764
#define PDL_BRC_CHKMAGIC(it) PDL_CHKMAGIC_GENERAL(it, PDL_BRC_MAGICNO, "BROADCAST")
#define PDL_BRC_SETMAGIC(it) (it)->magicno = PDL_BRC_MAGICNO

/* XXX To avoid mallocs, these should also have "default" values */
typedef struct pdl_broadcast {
	struct pdl_transvtable *transvtable;
        unsigned int magicno;
	int gflags;	/* Flags about this struct */
	PDL_Indx ndims;	/* Number of dimensions broadcasted over */
	PDL_Indx nimpl;	/* Number of these that are implicit */
	PDL_Indx npdls;	/* Number of pdls involved */
	PDL_Indx nextra;
	PDL_Indx *inds;	/* Indices for each of the dimensions */
	PDL_Indx *dims;	/* Dimensions of each dimension */
	PDL_Indx *offs;	/* Offsets for each of the pdls */
	PDL_Indx *incs;	/* npdls * ndims array of increments. Fast because
	 		               of constant indices for first loops */
	PDL_Indx *realdims;  /* realdims for each pdl (e.g., specified by PP signature) */
	pdl **pdls;
        char *flags;    /* per pdl flags */
        PDL_Indx mag_nth;    /* magicked thread dim */
        PDL_Indx mag_nthpdl; /* magicked ndarray */
        PDL_Indx mag_nthr;   /* number of threads */
        PDL_Indx mag_skip;   /* first pthread to skip if remainder, 0=none */
        PDL_Indx mag_stride; /* the base size to stride, without adding 1 if before drop */
        /*
           **
          t****
           ****
           ****
           --k--->thr (zero-based)

          t=3 (mag_stride)
          k=2 (mag_skip)
          offsets=[0,4,8,11,14]

          t****
           ****
           ****
           k----->thr (zero-based)

          t=3 (mag_stride)
          k=0 (mag_skip)
          offsets=[0,3,6,9,12]

          offset=thr*t + MIN(thr,k) // see macro PDL_BRC_OFFSET
        */
} pdl_broadcast;

#define PDL_BRC_OFFSET(thr, broadcast) ((thr)*((broadcast)->mag_stride) + PDLMIN((thr),(broadcast)->mag_skip))
#define PDL_BRC_INC(incs, npdls, p, d) ((incs)[(d)*(npdls) + (p)])
#define PDL_BRC_THR_OFFSET(broadcast, thr, j) \
  (PDL_REPROFFS(broadcast->pdls[j]) + ( \
    !thr ? 0 : \
    PDL_BISTEMP(broadcast->flags[j]) ? thr * broadcast->pdls[j]->dimincs[broadcast->pdls[j]->ndims-1] : \
    PDL_BRC_OFFSET(thr, broadcast) * PDL_BRC_INC(broadcast->incs, broadcast->npdls, j, broadcast->mag_nth) \
  ))

static inline int pdl_broadcast_nd_step(
  PDL_Indx npdls, PDL_Indx *offsp,
  PDL_Indx nth, PDL_Indx ndims, PDL_Indx *incs, PDL_Indx *dims, PDL_Indx *inds
) {
  PDL_Indx i,j;
  for (i=nth; i < ndims; i++) {
    for (j=0; j < npdls; j++) offsp[j] += incs[i*npdls + j];
    if (++inds[i] < dims[i]) return 1; /* Actual carry test */
    inds[i] = 0;
    for (j=0; j < npdls; j++) offsp[j] -= incs[i*npdls + j] * dims[i];
  }
  return 0;
}

/* Broadcast per pdl flags */
#define		PDL_BROADCAST_TEMP 	0x02

#define PDL_BISTEMP(flag) (flag & PDL_BROADCAST_TEMP)

/* __PDLTHREAD_H */
#endif
