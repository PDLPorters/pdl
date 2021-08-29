#ifndef __PDLTHREAD_H
#define __PDLTHREAD_H

typedef struct pdl_errorinfo {
	char *funcname;
	char **paramnames;
	int nparamnames;
} pdl_errorinfo;

#define PDL_THREAD_MAGICKED 0x0001
#define PDL_THREAD_MAGICK_BUSY 0x0002
#define PDL_THREAD_INITIALIZED 0x0004

#define PDL_THR_MAGICNO 0x92314764
#define PDL_THR_SETMAGIC(it) it->magicno = PDL_THR_MAGICNO
#define PDL_THR_CLRMAGIC(it) (it)->magicno = 0x99876134

/* XXX To avoid mallocs, these should also have "default" values */
typedef struct pdl_thread {
	pdl_errorinfo *einfo;
        unsigned int magicno;
	int gflags;	/* Flags about this struct */
	PDL_Indx ndims;	/* Number of dimensions threaded over */
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
} pdl_thread;

/* Thread per pdl flags */
#define		PDL_THREAD_VAFFINE_OK	0x01

#define PDL_TVAFFOK(flag) (flag & PDL_THREAD_VAFFINE_OK)
#define PDL_TREPRINC(pdl,flag,which) (PDL_TVAFFOK(flag) ? \
		pdl->vafftrans->incs[which] : pdl->dimincs[which])
#define PDL_TREPROFFS(pdl,flag) (PDL_TVAFFOK(flag) ? pdl->vafftrans->offs : 0)

/* __PDLTHREAD_H */
#endif
