

typedef struct pdl_errorinfo {
	char *funcname;
	char **paramnames;
	int nparamnames;
} pdl_errorinfo;

#define PDL_THREAD_MAGICKED 0x0001
#define PDL_THREAD_MAGICK_BUSY 0x0002

/* XXX To avoid mallocs, these should also have "default" values */
typedef struct pdl_thread {
	pdl_errorinfo *einfo;
	int gflags;	/* Flags about this struct */
	int ndims;	/* Number of dimensions threaded over */
	int nimpl;	/* Number of these that are implicit */
	int npdls;	/* Number of pdls involved */
	int nextra;
	int *inds;	/* Indices for each of the dimensions */
	int *dims;	/* Dimensions of each dimension */
	int *offs;	/* Offsets for each of the pdls */
	int *incs;	/* npdls * ndims array of increments. Fast because
	 		   of constant indices for first loops */
	int *realdims;  /* realdims for each pdl (e.g., specified by PP signature) */
	pdl **pdls;
        char *flags;    /* per pdl flags */
	int mag_nth;
	int mag_nthpdl;
} pdl_thread;


/* Thread per pdl flags */
#define		PDL_THREAD_VAFFINE_OK	0x01

#define PDL_TVAFFOK(flag) (flag & PDL_THREAD_VAFFINE_OK)
#define PDL_TREPRINC(pdl,flag,which) (PDL_TVAFFOK(flag) ? \
		pdl->vafftrans->incs[which] : pdl->dimincs[which])

#define PDL_TREPROFFS(pdl,flag) (PDL_TVAFFOK(flag) ? pdl->vafftrans->offs : 0)


/* No extra vars; not sure about the NULL arg, means no per pdl args */
#define PDL_THREADINIT(thread,pdls,realdims,creating,npdls,info) \
	  PDL->initthreadstruct(0,pdls,realdims,creating,npdls,info,&thread;\
				NULL)

#define PDL_THREAD_DECLS(thread)

#define PDL_THREADCREATEPAR(thread,ind,dims,temp) \
	  PDL->thread_create_parameter(&thread,ind,dims,temp)
#define PDL_THREADSTART(thread) PDL->startthreadloop(&thread)

#define PDL_THREADITER(thread,ptrs) PDL->iterthreadloop(&thread,0,NULL)

#define PDL_THREAD_INITP(thread,which,ptr) /* Nothing */
#define PDL_THREAD_P(thread,which,ptr) ((ptr)+(thread).offs[ind])
#define PDL_THREAD_UPDP(thread,which,ptr) /* Nothing */



