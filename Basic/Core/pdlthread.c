/* XXX NOTE THAT IT IS NOT SAFE TO USE ->pdls MEMBER OUTSIDE
   INITTHREADSTRUCT! */

#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */


#define MAX2(a,b) if((b)>(a)) a=b;

/*
 * We used to use our own copy of strndup but there were
 * compiler warnings about redefining the symbol, so I have
 * changed to using the Perl C API - see 'perldoc perlapi'
 * and 'perldoc perlcapi' - and created the copy_int/pdl_array
 * routines . The returned arrays should be released using
 * Perl's Safefree call.
 *
 * DJB 2007 March 18. Previous version is v1.6 of pdlthread.c
 */
static int *copy_int_array (int *from, int size) {
  int *to;
  Newx (to, size, int);
  return (int *) CopyD (from, to, size, int);
}

static pdl **copy_pdl_array (pdl **from, int size) {
  pdl **to;
  Newx (to, size, pdl*);
  return (pdl **) CopyD (from, to, size, pdl*);
}

static void print_iarr(int *iarr, int n) {
  int i;
  printf("(");
  for (i=0;i<n;i++)
    printf("%s%d",(i?" ":""),iarr[i]);
  printf(")");
}

#define psp printf("%s",spaces)
void dump_thread(pdl_thread *thread) {
  int i;
  char spaces[] = "    ";
  printf("DUMPTHREAD %p \n",(void*)thread);
  if (0&& thread->einfo) {
    psp; printf("Funcname: %s\n",thread->einfo->funcname);
    psp; printf("Paramaters: ");
    for (i=0;i<thread->einfo->nparamnames;i++)
      printf("%s ",thread->einfo->paramnames[i]);
    printf("\n");
  }
  psp; printf("Flags: %d, Ndims: %d, Nimplicit: %d, Npdls: %d, Nextra: %d\n",
	 thread->gflags,thread->ndims,thread->nimpl,thread->npdls,thread->nextra);

  psp; printf("Dims: "); print_iarr(thread->dims,thread->ndims); printf("\n");
  psp; printf("Inds: "); print_iarr(thread->inds,thread->ndims); printf("\n");
  psp; printf("Offs: "); print_iarr(thread->offs,thread->npdls); printf("\n");
  psp; printf("Incs: "); print_iarr(thread->incs,thread->ndims); printf("\n");
  psp; printf("Realdims: "); print_iarr(thread->realdims,thread->npdls); printf("\n");
  psp; printf("Pdls: (");
  for (i=0;i<thread->npdls;i++)
    printf("%s%p",(i?" ":""),(void*)(thread->pdls[i]));
  printf(")\n");
  psp; printf("Per pdl flags: (");
  for (i=0;i<thread->npdls;i++)
    printf("%s%d",(i?" ":""),thread->flags[i]);
  printf(")\n");
}


/* Function to get the pthread-specific offset
   Input: thread structure
   Outputs: Pointer to pthread-specific offset array (returned by function)
*/
int *pdl_get_threadoffsp(pdl_thread *thread )
{
  if(thread->gflags & PDL_THREAD_MAGICKED) {
  	int thr = pdl_magic_get_thread(thread->pdls[thread->mag_nthpdl]);
	return thread->offs + thr * thread->npdls;
  }
/* The non-multithreaded case: return just the usual offsets */
  return thread->offs;
}



/* Function to get the pthread-specific offset, indexes and pthread number for the supplied thread structure
   Input: thread structure
   Outputs: Pointer to pthread-specific offset array (returned by function)
            Pointer to pthread-specific index array (ind Pointer supplied and modified by function)
	    Pthread index for the current pthread   ( nthr supplied and modified by function)

*/
int *pdl_get_threadoffsp_int(pdl_thread *thread, int *nthr, int **inds)
{
  if(thread->gflags & PDL_THREAD_MAGICKED) {
  	int thr = pdl_magic_get_thread(thread->pdls[thread->mag_nthpdl]);
	*nthr = thr;
	*inds = thread->inds  + thr * thread->ndims;
	return  thread->offs  + thr * thread->npdls;
  }
  *nthr = 0;
/* The non-multithreaded case: return just the usual offsets */
  *inds = thread->inds;
  return thread->offs;
}

void pdl_thread_copy(pdl_thread *from,pdl_thread *to) {
#ifdef PDL_THREAD_DEBUG
	to->magicno = from->magicno;
#endif
	to->gflags = from->gflags;
	to->einfo = from->einfo;
	to->ndims = from->ndims;
	to->nimpl = from->nimpl;
	to->npdls = from->npdls;

	to->inds = copy_int_array(from->inds,to->ndims);
	to->dims = copy_int_array(from->dims,to->ndims);
	to->offs = copy_int_array(from->offs,to->npdls);
	to->incs = copy_int_array(from->incs,to->npdls*to->ndims);
	to->realdims = from->realdims;
	to->flags = savepvn(from->flags,to->npdls);
	to->pdls = copy_pdl_array(from->pdls,to->npdls); /* XX MEMLEAK */

	to->mag_nthpdl = from->mag_nth;
	to->mag_nthpdl = from->mag_nthpdl;
}

void pdl_freethreadloop(pdl_thread *thread) {
	PDLDEBUG_f(printf("Freethreadloop(%p, %p %p %p %p %p %p)\n",
		(void*)thread,
		(void*)(thread->inds), (void*)(thread->dims), (void*)(thread->offs),
		(void*)(thread->incs), (void*)(thread->flags), (void*)(thread->pdls));)
	if(!thread->inds) {return;}
	Safefree(thread->inds);
	Safefree(thread->dims);
	Safefree(thread->offs);
	Safefree(thread->incs);
	Safefree(thread->flags);
	Safefree(thread->pdls);
	pdl_clearthreadstruct(thread);
}

void pdl_clearthreadstruct(pdl_thread *it) {
	PDLDEBUG_f(printf("Clearthreadloop(%p)\n", (void*)it);)
	it->einfo = 0;it->inds = 0;it->dims = 0;
	it->ndims = it->nimpl = it->npdls = 0; it->offs = 0;
	it->pdls = 0;it->incs = 0; it->realdims=0; it->flags=0;
	it->gflags=0; /* unsets PDL_THREAD_INITIALIZED among others */
#ifdef PDL_THREAD_DEBUG
	PDL_THR_CLRMAGIC(it);
#endif
}


/* Function to auto-add pthreading magic (i.e. hints for multiple processor threads )
   to the pdls, based on the target number of pthreads and the pdl-threaded dimensions
   .
   Number of pthreads is limited to a even division of the size of the threaded dimension.
    (e.g. if threaded dim of size 10 and the target number of pthreads is 2, 10/2 = 5 even,
      so the two pthreads will be created to process.
      However if thread dim is size 9 and target number of pthreads is 2, 9 can't be divided
      by 2, so no extra pthreads will be created.
    )
   noPthreadFlag is a flag indicating that the pdl thread that called this function is not multiple
     processor threading safe, so no pthreading magic will be added
*/
void 	pdl_autopthreadmagic( pdl **pdls, int npdls, int * realdims, int* creating, int noPthreadFlag ){

	int j, nthrd, totalDims, *nthreadedDims, **threadedDims, **threadedDimSizes;
	int largest_nvals = 0;  /* The largest PDL size for all the pdls involvled */

	int t;             /* Thread index for each pdl */
	int tdimStart;    /* Start of the threaded dims for each pdl */
	int k;            /* threadedDims array index for each pdl */
	int nthreadDim;   /* Number of thread dims for the current pdl */

	int maxPthreadPDL; /* PDL that has the max (or right at the target) num pthreads */
	int maxPthreadDim; /* Threaded dim number that has the max num pthreads */
	int maxPthread = 0;    /* Maximum achievable pthread */

	int target_pthread = pdl_autopthread_targ;
	pdl_autopthread_actual = 0; /* Initialize the global variable indicating actual number of pthreads */

	/* Don't do anything if auto_pthreading is turned off (i.e. equal zero) */
	if( !target_pthread ) return;

	/* Remove any existing threading magic */
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		nthrd;

		/* Remove thread magic, if there is some set for this pdl */
		if( pdls[j]->magic &&
		  (pdl_magic_thread_nthreads(pdls[j],&nthrd))) {
			pdl_add_threading_magic(pdls[j], -1, -1);
		}
	}

	if( noPthreadFlag ) return;  /* Don't go further if the current pdl function isn't thread-safe */


	/* Find the largest nvals */
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		if( pdls[j]->nvals > largest_nvals ){
			largest_nvals = pdls[j]->nvals;
		}
	}

	/* See if the largest nvals is above the auto_pthread threadshold */
	largest_nvals = largest_nvals>>20; /* Convert to MBytes */

	/* Don't do anything if we are lower than the threshold */
	if( largest_nvals < pdl_autopthread_size )
		return;


	/* Build int arrays of threaded dim numbers and sizes for each pdl */
	nthreadedDims     = (int*)  malloc(sizeof(int)   * (npdls));
	threadedDims      = (int**) malloc(sizeof(int *) * (npdls));
	threadedDimSizes  = (int**) malloc(sizeof(int *) * (npdls));


	/* Find total number of dims and allocate */
	totalDims = 0;
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		threadedDims[j]     = (int*) malloc(sizeof(int) * pdls[j]->ndims);
		threadedDimSizes[j] = (int*) malloc(sizeof(int) * pdls[j]->ndims);
	}



	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		tdimStart = realdims[j];
		nthreadDim = 0;
		for( k=0, t = tdimStart; t < pdls[j]->ndims; t++, k++ ){
			threadedDimSizes[j][k] = pdls[j]->dims[t];
			threadedDims[j][k]      = t;
			nthreadDim++;
		}
		nthreadedDims[j] = nthreadDim;


	}

	/* Go thru each theaded dim and see how many pthreads we can create closest
	   to the maximum target pthreads */
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		for( k=0; k < nthreadedDims[j]; k++){
			int pthreadActual = target_pthread +1;
			int remainder = 1;
			/* Go from the target pthread to 1, untill we get an even division of the dimension size */
			while( (pthreadActual > 0) && (remainder > 0) ){
				pthreadActual--;
				remainder = threadedDimSizes[j][k] % pthreadActual;
			}

			if( pthreadActual > maxPthread ){ /* Record this dim if it is the max */
				maxPthread = pthreadActual;
				maxPthreadPDL = j;
				maxPthreadDim = threadedDims[j][k];
			}

			/* Don't go any further if target pthread achieved */
			if( pthreadActual == target_pthread ) break;
		}
		/* Don't go any further if target pthread achieved */
		if( maxPthread == target_pthread ) break;
	}





	/*
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		printf("PDL %d:\n", j);
		for( k=0; k < nthreadedDims[j]; k++){
			printf("Thread dim %d, Dim No %d, Size %d\n", k, threadedDims[j][k],
				threadedDimSizes[j][k]);
		}
	}
	printf("\n");

	printf("Target Pthread = %d\n", target_pthread);
	printf("maxPthread = %d, maxPthreadPDL = %d, maxPthreadDim = %d\n", maxPthread, maxPthreadPDL, maxPthreadDim);
	*/


	/* Add threading magic */
	if( maxPthread > 1 ){
		pdl_add_threading_magic(pdls[maxPthreadPDL], maxPthreadDim, maxPthread);
		pdl_autopthread_actual = maxPthread; /* Set the global variable indicating actual number of pthreads */

	}

	/* Free the stuff we allocated */
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		free(threadedDims[j]);
		free(threadedDimSizes[j]);
	}
	free(nthreadedDims);
	free(threadedDims);
	free(threadedDimSizes);

}



/* The assumptions this function makes:
 *  pdls is dynamic and may go away -> copied
 *  realdims is static and is NOT copied and NOT freed!!!
 *  creating is only used inside this routine.
 *  errorinfo is assumed static.
 *  usevaffine is assumed static. (uses if exists)
 *
 * Only the first thread-magicked pdl is taken into account.
 *
 *  noPthreadFlag is a flag to indicate the pdl thread is not pthreading safe
 *   (i.e. don't attempt to create multiple posix threads to execute)
 */
void pdl_initthreadstruct(int nobl,
	pdl **pdls,int *realdims,int *creating,int npdls,
	pdl_errorinfo *info,pdl_thread *thread, char *flags, int noPthreadFlag ) {
	int i; int j;
	int ndims=0; int nth;
	int mx;
	int nids;
	int nimpl;
	int nthid;

	int mydim;

	int *nthreadids;
	int nthr = 0; int nthrd;

	PDLDEBUG_f(printf("Initthreadloop(%p)\n", (void*)thread);)
#ifdef PDL_THREAD_DEBUG
	  /* the following is a fix for a problem in the current core logic
           * see comments in pdl_make_physical in pdlapi.c
           * the if clause detects if this thread has previously been initialized
           * if yes free the stuff that was allocated in the last run
           * just returning is not! good enough (I tried it)
           * CS
           */
	if (thread->magicno == PDL_THR_MAGICNO &&
	    thread->gflags & PDL_THREAD_INITIALIZED) {
	  PDLDEBUG_f(printf("REINITIALIZING already initialized thread\n");)
	  PDLDEBUG_f(dump_thread(thread);)
	  /* return; */ /* try again, should (!?) work */

	  if (thread->inds) Safefree(thread->inds);
	  if (thread->dims) Safefree(thread->dims);
	  if (thread->offs) Safefree(thread->offs);
	  if (thread->incs) Safefree(thread->incs);
	  if (thread->flags) Safefree(thread->flags);
	  if (thread->pdls) Safefree(thread->pdls);

	  PDLDEBUG_f(pdl_warn("trying to reinitialize already initialized "
	     "thread (mem-leak!); freeing...");)
	}
	PDL_THR_SETMAGIC(thread);
#endif
	thread->gflags = 0;

	thread->npdls = npdls;
	thread->pdls = copy_pdl_array(pdls,npdls);
	thread->realdims = realdims;
	thread->ndims = 0;

	thread->mag_nth = -1;
	thread->mag_nthpdl = -1;
        thread->mag_nthr = -1;

	nids=0;
	mx=0;
/* Find the max. number of threadids */
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		MAX2(nids,pdls[j]->nthreadids);
		MAX2(mx,pdls[j]->threadids[0] - realdims[j]);
	}
	nthreadids = pdl_malloc(sizeof(int)*nids);
	ndims += mx;  nimpl = mx; thread->nimpl = nimpl;


	//printf("In pdl_initthreadstruct for func %s\n", info->funcname);
	pdl_autopthreadmagic(pdls, npdls, realdims, creating, noPthreadFlag);


	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		/* Check for magical piddles (parallelized) */
		if((!nthr) &&
		  pdls[j]->magic &&
		  (nthr = pdl_magic_thread_nthreads(pdls[j],&nthrd))) {
			thread->mag_nthpdl = j;
			thread->mag_nth = nthrd - realdims[j];
                       thread->mag_nthr = nthr;
			if(thread->mag_nth < 0) {
				pdl_croak_param(info,j,"Cannot magick non-threaded dims \n\t");
			}
		}

		for(i=0; i<nids; i++) {
			mx=0; if(pdls[j]->nthreadids <= nids) {
				MAX2(mx,
				     pdls[j]->threadids[i+1]
				     - pdls[j]->threadids[i]);
			}
			ndims += mx;
			nthreadids[i] = mx;
		}
	}

	if(nthr) {
		thread->gflags |= PDL_THREAD_MAGICKED;
	}

	if(ndims < nobl) { /* If too few, add enough implicit dims */
		thread->nextra = nobl - ndims;
		ndims += thread->nextra;
	} else {
		thread->nextra = 0;
	}

	thread->ndims = ndims;
	thread->nimpl = nimpl;

      Newx(thread->inds, thread->ndims * (nthr>0 ? nthr : 1), int); /* Create space for pthread-specific inds (i.e. copy for each pthread)*/
      if(thread->inds == NULL) croak("Failed to allocate memory for thread->inds in pdlthread.c");

      Newx(thread->dims, thread->ndims, int);
      if(thread->dims == NULL) croak("Failed to allocate memory for thread->dims in pdlthread.c");

      Newx(thread->offs, thread->npdls * (nthr>0 ? nthr : 1), int); /* Create space for pthread-specific offs */
      if(thread->offs == NULL) croak("Failed to allocate memory for thread->offs in pdlthread.c");

      Newx(thread->incs, thread->ndims * npdls, int);
      if(thread->incs == NULL) croak("Failed to allocate memory for thread->incs in pdlthread.c");

      Newx(thread->flags, npdls, char);
      if(thread->flags == NULL) croak("Failed to allocate memory for thread->flags in pdlthread.c");

	nth=0; /* Index to dimensions */

	/* populate the per_pdl_flags */

	for (i=0;i<npdls; i++) {
	  thread->offs[i] = 0; /* initialize offsets */
	  thread->flags[i] = 0;
	  if (PDL_VAFFOK(pdls[i]) && VAFFINE_FLAG_OK(flags,i))
	    thread->flags[i] |= PDL_THREAD_VAFFINE_OK;
	}
	flags = thread->flags; /* shortcut for the remainder */

/* Make implicit inds */

	for(i=0; i<nimpl; i++) {
		thread->dims[nth] = 1;
		for(j=0; j<thread->npdls; j++) {
			thread->incs[nth*npdls+j] = 0;
			if(creating[j]) continue;
			if(thread->pdls[j]->threadids[0]-
					thread->realdims[j] <= i)
				continue;
			if(pdls[j]->dims[i+realdims[j]] != 1) {
				if(thread->dims[nth] != 1) {
					if(thread->dims[nth] !=
						pdls[j]->dims[i+realdims[j]]) {
                                              pdl_croak_param(info,j,"Mismatched implicit thread dimension %d: should be %d, is %d\n\t",
							i,
							thread->dims[nth],
							pdls[j]->dims[i+thread->realdims[j]]);
					}
				} else {
					thread->dims[nth] =
						pdls[j]->dims[i+realdims[j]];
				}
				thread->incs[nth*npdls+j] =
					PDL_TREPRINC(pdls[j],flags[j],i+realdims[j]);
			}
		}
		nth++;
	}

/* Go through everything again and make the real things */

	for(nthid=0; nthid<nids; nthid++) {
	for(i=0; i<nthreadids[nthid]; i++) {
		thread->dims[nth] = 1;
		for(j=0; j<thread->npdls; j++) {
			thread->incs[nth*npdls+j] = 0;
			if(creating[j]) continue;
			if(thread->pdls[j]->nthreadids < nthid)
				continue;
			if(thread->pdls[j]->threadids[nthid+1]-
			   thread->pdls[j]->threadids[nthid]
					<= i) continue;
			mydim = i+thread->pdls[j]->threadids[nthid];
			if(pdls[j]->dims[mydim]
					!= 1) {
				if(thread->dims[nth] != 1) {
					if(thread->dims[nth] !=
						pdls[j]->dims[mydim]) {
						pdl_croak_param(info,j,"Mismatched Implicit thread dimension %d: should be %d, is %d",
							i,
							thread->dims[nth],
							pdls[j]->dims[i+thread->realdims[j]]);
					}
				} else {
					thread->dims[nth] =
						pdls[j]->dims[mydim];
				}
				thread->incs[nth*npdls+j] =
					PDL_TREPRINC(pdls[j],flags[j],mydim);
			}
		}
		nth++;
	}
	}


/* Make sure that we have the obligatory number of threaddims */

	for(; nth<ndims; nth++) {
		thread->dims[nth]=1;
		for(j=0; j<npdls; j++)
			thread->incs[nth*npdls+j] = 0;
	}
/* If threading, make the true offsets and dims.. */

	if(nthr > 0) {
		int n1 = thread->dims[thread->mag_nth] / nthr;
		int n2 = thread->dims[thread->mag_nth] % nthr;
		if(n2) {
			die("Cannot magick-thread with non-divisible n!");
		}
		thread->dims[thread->mag_nth] = n1;
	}
	thread->gflags |= PDL_THREAD_INITIALIZED;
	PDLDEBUG_f(dump_thread(thread);)
}

void pdl_thread_create_parameter(pdl_thread *thread,int j,int *dims,
				 int temp)
{
	int i;
	int td = temp ? 0 : thread->nimpl;

	if(!temp && thread->nimpl != thread->ndims - thread->nextra) {
		pdl_croak_param(thread->einfo,j,
			"Trying to create parameter while explicitly threading.\
See the manual for why this is impossible");
	}
	pdl_reallocdims(thread->pdls[j], thread->realdims[j] + td);
	for(i=0; i<thread->realdims[j]; i++)
		thread->pdls[j]->dims[i] = dims[i];
	if (!temp)
	  for(i=0; i<thread->nimpl; i++)
		thread->pdls[j]->dims[i+thread->realdims[j]] =
                       thread->dims[i] *
                     ((i == thread->mag_nth && thread->mag_nthr > 0) ?
                           thread->mag_nthr : 1);
	thread->pdls[j]->threadids[0] = td + thread->realdims[j];
	pdl_resize_defaultincs(thread->pdls[j]);
	for(i=0; i<thread->nimpl; i++) {
		thread->incs[thread->npdls*i + j] =
		  temp ? 0 :
		  PDL_REPRINC(thread->pdls[j],i+thread->realdims[j]);
	}
}

int pdl_startthreadloop(pdl_thread *thread,void (*func)(pdl_trans *),
			pdl_trans *t) {
	int i,j;
	int *offsp; int nthr;
	int *inds;
	if(  (thread->gflags & (PDL_THREAD_MAGICKED | PDL_THREAD_MAGICK_BUSY))
	     == PDL_THREAD_MAGICKED ) {

		/* If no function supplied (i.e. being called from PDL::thread_over), don't run in parallel */
		if(!func) {
			thread->gflags &= ~PDL_THREAD_MAGICKED; /* Cancel thread_magicked */
		}
		else{


			thread->gflags |= PDL_THREAD_MAGICK_BUSY;
			/* Do the threadloop magically (i.e. in parallel) */
			pdl_magic_thread_cast(thread->pdls[thread->mag_nthpdl],
				func,t, thread);
			thread->gflags &= ~PDL_THREAD_MAGICK_BUSY;
			return 1; /* DON'T DO THREADLOOP AGAIN */
		}
	}
	offsp = pdl_get_threadoffsp_int(thread,&nthr, &inds);
	for(i=0; i<thread->ndims; i++)
		inds[i] = 0;
	for(j=0; j<thread->npdls; j++)
		offsp[j] = PDL_TREPROFFS(thread->pdls[j],thread->flags[j]) +
			(!nthr?0:
				nthr * thread->dims[thread->mag_nth] *
				    thread->incs[thread->mag_nth*thread->npdls + j]);
	return 0;
}

/* This will have to be macroized */
int pdl_iterthreadloop(pdl_thread *thread,int nth) {
	int i,j;
	int stop = 0;
	int stopdim;
	int *offsp; int nthr;
	int *inds;
	offsp = pdl_get_threadoffsp_int(thread,&nthr, &inds);
	for(j=0; j<thread->npdls; j++)
		offsp[j] = PDL_TREPROFFS(thread->pdls[j],thread->flags[j]);
	for(i=nth; i<thread->ndims; i++) {
		inds[i] ++;
		if( inds[i] >= thread->dims[i])
			inds[i] = 0;
		else
		{	stopdim = i; stop = 1; break; }
	}
	if(stop) goto calc_offs;
	return 0;
calc_offs:

	for(j=0; j<thread->npdls; j++) {
		offsp[j] = PDL_TREPROFFS(thread->pdls[j],thread->flags[j]) +
		(!nthr?0:
			nthr * thread->dims[thread->mag_nth] *
			    thread->incs[thread->mag_nth*thread->npdls + j]);
			;
		for(i=nth; i<thread->ndims; i++) {
			offsp[j] += thread->incs[i*thread->npdls+j] *
					 inds[i];
		}
	}
	return stopdim+1;
}

void pdl_croak_param(pdl_errorinfo *info,int paramIndex, char *pat, ...)
{
  // I barf a string such as "PDL: function(a,b,c): Parameter 'b' errormessage"

  char message  [4096] = {'\0'};
  int i;
  va_list args;

#define msgptr_advance()                        \
do {                                            \
  int N      = strlen(msgptr);                  \
  msgptr    += N;                               \
  remaining -= N;                               \
} while(0)


  char* msgptr    = message;
  int   remaining = sizeof(message);

  if(info)
  {
    if(paramIndex < 0 || paramIndex >= info->nparamnames)
    {
      strcat(msgptr, "ERROR: UNKNOWN PARAMETER");
      msgptr_advance();
    }
    else
    {
      snprintf(msgptr, remaining, "PDL: %s(", info->funcname);
      msgptr_advance();

      for(i=0; i<info->nparamnames; i++)
      {
        snprintf(msgptr, remaining, "%s", info->paramnames[i]);
        msgptr_advance();

        if(i < info->nparamnames-1)
        {
          snprintf(msgptr, remaining, ",");
          msgptr_advance();
        }
      }

      snprintf(msgptr, remaining, "): Parameter '%s':\n",
               info->paramnames[paramIndex]);
      msgptr_advance();
    }
  }

  va_start(args,pat);

  vsnprintf(msgptr, remaining, pat, args);

  va_end(args);

  pdl_barf(message);
}
