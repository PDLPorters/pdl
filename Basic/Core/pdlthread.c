/* XXX NOTE THAT IT IS NOT SAFE TO USE ->pdls MEMBER OUTSIDE
   INITTHREADSTRUCT! */

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#define MAX2(a,b) if((b)>(a)) a=b;

/**** Convenience routines for moving around collections
 **** of indices and PDL pointers.
 **** (Note that copy_int_array is confusingly named sicne it
 **** doesn't copy ints, it copies PDL_Indx's.)
 ****/
static PDL_Indx *copy_int_array (PDL_Indx *from, int size) {
  int *to;
  Newx (to, size, int);
  return (PDL_Indx *) CopyD (from, to, size, int);
}

static pdl **copy_pdl_array (pdl **from, int size) {
  pdl **to;
  Newx (to, size, pdl*);
  return (pdl **) CopyD (from, to, size, pdl*);
}


/******************************
 * dump_thread and helpers -- debugging routine used for 
 * describing internal state 
 */
static void print_iarr(PDL_Indx *iarr, int n) {
  int i;
  printf("(");
  for (i=0;i<n;i++)
    printf("%s%"IND_FLAG,(i?" ":""),iarr[i]);  // IND_FLAG in pdl.h, at build time from Types.pm.PL
  printf(")");
}

#define psp printf("%s",spaces)
void dump_thread(pdl_thread *thread) {
  int i, found=0, sz=0;
  char spaces[] = "    ";
  int flagval[] = {
#define X(f) f,
PDL_LIST_FLAGS_PDLTHREAD(X)
#undef X
    0
  };
  char *flagchar[] = {
#define X(f) #f,
PDL_LIST_FLAGS_PDLTHREAD(X)
#undef X
    NULL
  };
  fflush(stdout);
  printf("DUMPTHREAD %p\n",(void*)thread);
  if (thread->transvtable) {
    psp; printf("Funcname: %s\n",thread->transvtable->name);
    psp; printf("Parameters: ");
    for (i=0;i<thread->transvtable->npdls;i++)
      printf("%s ",thread->transvtable->par_names[i]);
    printf("\n");
  }
  psp; printf("Flags: ");
  for (i=0;flagval[i]!=0; i++)
    if (thread->gflags & flagval[i]) {
      printf("%s%s",found ? "|":"",flagchar[i]);
      found = 1;
      sz += strlen(flagchar[i]);
      if (sz>PDL_MAXLIN) {sz=0; printf("\n");psp;}
    }
  printf("\n");
  psp; printf("Ndims: %"IND_FLAG", Nimplicit: %"IND_FLAG", Npdls: %"IND_FLAG", Nextra: %"IND_FLAG"\n",
	 thread->ndims,thread->nimpl,thread->npdls,thread->nextra);
  psp; printf("Mag_nth: %"IND_FLAG", Mag_nthpdl: %"IND_FLAG", Mag_nthr: %"IND_FLAG", Mag_skip: %"IND_FLAG", Mag_stride: %"IND_FLAG"\n",
	 thread->mag_nth,thread->mag_nthpdl,thread->mag_nthr,thread->mag_skip,thread->mag_stride);
  if (thread->mag_nthr <= 0) {
    psp; printf("Dims: "); print_iarr(thread->dims,thread->ndims); printf("\n");
    psp; printf("Inds: "); print_iarr(thread->inds,thread->ndims); printf("\n");
    psp; printf("Offs: "); print_iarr(thread->offs,thread->npdls); printf("\n");
  } else {
    psp; printf("Dims (per thread):\n");
    for (i=0;i<thread->mag_nthr;i++) {
      psp; psp; print_iarr(thread->dims + i*thread->ndims,thread->ndims);
      printf("\n");
    }
    psp; printf("Inds (per thread):\n");
    for (i=0;i<thread->mag_nthr;i++) {
      psp; psp; print_iarr(thread->inds + i*thread->ndims,thread->ndims);
      printf("\n");
    }
    psp; printf("Offs (per thread):\n");
    for (i=0;i<thread->mag_nthr;i++) {
      psp; psp; print_iarr(thread->offs + i*thread->npdls,thread->npdls);
      printf("\n");
    }
  }
  psp; printf("Incs (per pdl):\n");
  for (i=0;i<thread->npdls;i++) {
    psp; psp; print_iarr(thread->incs + i*thread->ndims,thread->ndims);
    printf("\n");
  }
  psp; printf("Realdims: "); print_iarr(thread->realdims,thread->npdls); printf("\n");
  psp; printf("Pdls: (");
  for (i=0;i<thread->npdls;i++)
    printf("%s%p",(i?" ":""),(void*)(thread->pdls[i]));
  printf(")\n");
  psp; printf("Per pdl flags: (");
  for (i=0;i<thread->npdls;i++)
    printf("%s%d",(i?" ":""),thread->flags[i]);
  printf(")\n");
  fflush(stdout);
}


/*******
 * pdl_get_threaddims - get the pthread-specific threading dims from a PDL
 *  Input: thread structure
 *  Outputs: see above (returned by function)
 */
PDL_Indx *pdl_get_threaddims(pdl_thread *thread)
{
  /* The non-multithreaded case: return just the usual value */
  if (!(thread->gflags & PDL_THREAD_MAGICKED)) return thread->dims;
  int thr = pdl_magic_get_thread(thread->pdls[thread->mag_nthpdl]);
  return thread->dims + thr * thread->ndims;
}


/*******
 * pdl_get_threadoffsp - get the pthread-specific offset arrays from a PDL
 *  Input: thread structure
 *  Outputs: Pointer to pthread-specific offset array (returned by function)
 */
PDL_Indx *pdl_get_threadoffsp(pdl_thread *thread)
{
  /* The non-multithreaded case: return just the usual offsets */
  if (!(thread->gflags & PDL_THREAD_MAGICKED)) return thread->offs;
  int thr = pdl_magic_get_thread(thread->pdls[thread->mag_nthpdl]);
  return thread->offs + thr * thread->npdls;
}


/* Function to get the pthread-specific offset, indexes and pthread number for the supplied thread structure
   Input: thread structure
   Outputs: Pointer to pthread-specific offset array (returned by function)
            Pointer to pthread-specific index array (ind Pointer supplied and modified by function)
            Pointer to pthread-specific dims array (dims Pointer supplied and modified by function)
	    Pthread index for the current pthread   ( thr supplied and modified by function)
*/
PDL_Indx* pdl_get_threadoffsp_int(pdl_thread *thread, int *pthr, PDL_Indx **inds, PDL_Indx **dims)
{
  if(thread->gflags & PDL_THREAD_MAGICKED) {
  	int thr = pdl_magic_get_thread(thread->pdls[thread->mag_nthpdl]);
	*pthr = thr;
	*inds = thread->inds  + thr * thread->ndims;
	*dims = thread->dims  + thr * thread->ndims;
	return  thread->offs  + thr * thread->npdls;
  }
  *pthr = 0;
/* The non-multithreaded case: return just the usual offsets */
  *dims = thread->dims;
  *inds = thread->inds;
  return thread->offs;
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
	it->transvtable = 0;it->inds = 0;it->dims = 0;
	it->ndims = it->nimpl = it->npdls = 0; it->offs = 0;
	it->pdls = 0;it->incs = 0; it->realdims=0; it->flags=0;
	it->gflags=0; /* unsets PDL_THREAD_INITIALIZED among others */
	PDL_THR_CLRMAGIC(it);
}

void pdl_dump_threading_info(
  int npdls, PDL_Indx* creating, int target_pthread,
  PDL_Indx *nthreadedDims, PDL_Indx **threadedDims, PDL_Indx **threadedDimSizes,
  int maxPthreadPDL, int maxPthreadDim, int maxPthread
) {
  PDL_Indx j, k;
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    printf("PDL %"IND_FLAG":\n", j);
    for( k=0; k < nthreadedDims[j]; k++){
      printf("Thread dim %"IND_FLAG", Dim No %"IND_FLAG", Size %"IND_FLAG"\n",
        k, threadedDims[j][k], threadedDimSizes[j][k]);
    }
  }
  printf("\nTarget Pthread = %d\n"
    "maxPthread = %d, maxPthreadPDL = %d, maxPthreadDim = %d\n",
    target_pthread, maxPthread, maxPthreadPDL, maxPthreadDim);
}

void pdl_find_max_pthread(
  pdl **pdls, int npdls, PDL_Indx* realdims, PDL_Indx* creating,
  int target_pthread,
  int *p_maxPthread, /* Maximum achievable pthread */
  int *p_maxPthreadDim, /* Threaded dim number that has the max num pthreads */
  int *p_maxPthreadPDL /* PDL that has the max (or right at the target) num pthreads */
) {
  PDL_Indx j, k, t;
  /* Build int arrays of threaded dim numbers and sizes for each pdl */
  PDL_Indx max_remainder = 0;
  PDL_Indx nthreadedDims[npdls];
  PDL_Indx *threadedDims[npdls];
  PDL_Indx *threadedDimSizes[npdls];
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    threadedDims[j]     = (PDL_Indx*) malloc(sizeof(PDL_Indx) * pdls[j]->ndims);
    threadedDimSizes[j] = (PDL_Indx*) malloc(sizeof(PDL_Indx) * pdls[j]->ndims);
  }
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    for( k=0, t = realdims[j]; t < pdls[j]->ndims; t++, k++ ){
      threadedDimSizes[j][k] = pdls[j]->dims[t];
      threadedDims[j][k]      = t;
    }
    nthreadedDims[j] = pdls[j]->ndims - realdims[j];
  }
  /* Go through each threaded dim and find best match */
  *p_maxPthread = 0;
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    for( k=0; k < nthreadedDims[j]; k++){
      PDL_Indx this_dim = threadedDimSizes[j][k];
      PDL_Indx this_remainder = this_dim % target_pthread;
      if( this_remainder == 0 ){
        *p_maxPthread = target_pthread;
        *p_maxPthreadPDL = j;
        *p_maxPthreadDim = threadedDims[j][k];
        break;
      }
      if( this_dim > *p_maxPthread && this_remainder > max_remainder ){
        max_remainder = this_remainder;
        *p_maxPthread = PDLMIN(target_pthread, this_dim);
        *p_maxPthreadPDL = j;
        *p_maxPthreadDim = threadedDims[j][k];
      }
    }
    /* Don't go any further if target pthread achieved */
    if( *p_maxPthread == target_pthread ) break;
  }
  PDLDEBUG_f(pdl_dump_threading_info(
    npdls, creating, target_pthread,
    nthreadedDims, threadedDims, threadedDimSizes,
    *p_maxPthreadPDL, *p_maxPthreadDim, *p_maxPthread
  ));
  /* Free the stuff we allocated */
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    free(threadedDims[j]);
    free(threadedDimSizes[j]);
  }
}

/* Function to auto-add pthreading magic (i.e. hints for multiple
   processor threads) to the pdls, based on the target number of
   pthreads and the pdl-threaded dimensions.
   noPthreadFlag is a flag indicating that the pdl thread that
   called this function is not multiple processor threading safe,
   so no pthreading magic will be added.
*/
void 	pdl_autopthreadmagic( pdl **pdls, int npdls, PDL_Indx* realdims, PDL_Indx* creating, int noPthreadFlag ){
	PDL_Indx j, nthrd;
	PDL_Indx largest_nvals = 0;  /* The largest PDL size for all the pdls involved */
	int maxPthreadPDL; /* PDL that has the max (or right at the target) num pthreads */
	int maxPthreadDim; /* Threaded dim number that has the max num pthreads */
	int maxPthread = 0;    /* Maximum achievable pthread */
	int target_pthread = pdl_autopthread_targ;
	pdl_autopthread_actual = 0; /* Initialize the global variable indicating actual number of pthreads */
	pdl_autopthread_dim = -1; /* Initialize the global variable indicating actual dim pthreaded on */

	/* Don't do anything if auto_pthreading is turned off (i.e. equal zero) */
	if( !target_pthread ) return;

	/* Remove any existing threading magic */
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;

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

	pdl_find_max_pthread(
	  pdls, npdls, realdims, creating, target_pthread,
	  &maxPthread, &maxPthreadDim, &maxPthreadPDL
	);

	/* Add threading magic */
	if( maxPthread > 1 ){
		pdl_add_threading_magic(pdls[maxPthreadPDL], maxPthreadDim, maxPthread);
		pdl_autopthread_actual = maxPthread; /* Set the global variable indicating actual number of pthreads */
		pdl_autopthread_dim = maxPthreadDim;
	}
}

void pdl_thread_mismatch_msg(
  char *s,
  pdl **pdls, pdl_thread *thread,
  PDL_Indx i, PDL_Indx j, PDL_Indx nimpl,
  PDL_Indx *realdims,PDL_Indx *creating
) {
  /* This probably uses a lot more lines than necessary */
  int ii,jj,maxrealdims;
  sprintf(s,
    "  Mismatched implicit thread dimension %"IND_FLAG": size %"IND_FLAG" vs. %"IND_FLAG"\nThere are %"IND_FLAG" PDLs in the expression; %"IND_FLAG" thread dim%s.\n",
    i,thread->dims[i],pdls[j]->dims[i+realdims[j]],
    thread->npdls,nimpl,(nimpl==1)?"":"s"
  );
  s += strlen(s);
  for(ii=maxrealdims=0; ii<thread->npdls; ii++)
    if(thread->realdims[ii]>maxrealdims)
      maxrealdims=thread->realdims[ii];
  sprintf(s,  "   PDL IN EXPR.    "); s += strlen(s);
  if(maxrealdims > 0) {
    char format[80];
    sprintf(format,"%%%ds",8 * maxrealdims + 3);
    sprintf(s,format,"ACTIVE DIMS | ");
    s += strlen(s);
  }
  sprintf(s,"THREAD DIMS\n");
  s += strlen(s);
  for(ii=0; ii<thread->npdls; ii++) {
    sprintf(s,"   #%3d (%s",ii,creating[ii]?"null)\n":"normal): ");
    s += strlen(s);
    if(creating[ii])
      continue;
    if(maxrealdims == 1) {
      sprintf(s,"    ");
      s += strlen(s);
    }
    for(jj=0; jj< maxrealdims - thread->realdims[ii]; jj++) {
      sprintf(s,"%8s"," ");
      s += strlen(s);
    }
    for(jj=0; jj< thread->realdims[ii]; jj++) {
      sprintf(s,"%8"IND_FLAG,pdls[ii]->dims[jj]);
      s += strlen(s);
    }
    if(maxrealdims) {
      sprintf(s," | ");
      s += strlen(s);
    }
    for(jj=0; jj<nimpl && jj + thread->realdims[ii] < pdls[ii]->ndims; jj++) {
      sprintf(s,"%8"IND_FLAG,pdls[ii]->dims[jj+thread->realdims[ii]]);
      s += strlen(s);
    }
    sprintf(s,"\n");
    s += strlen(s);
  }
}

/* The assumptions this function makes:
 *  pdls is dynamic and may go away -> copied
 *  realdims is static and is NOT copied and NOT freed!!!
 *  creating is only used inside this routine.
 *  vtable is assumed static.
 *  usevaffine is assumed static. (uses if exists)
 *
 * Only the first thread-magicked pdl is taken into account.
 *
 *  noPthreadFlag is a flag to indicate the pdl thread is not pthreading safe
 *   (i.e. don't attempt to create multiple posix threads to execute)
 */
void pdl_initthreadstruct(int nobl,
	pdl **pdls,PDL_Indx *realdims,PDL_Indx *creating,PDL_Indx npdls,
	pdl_transvtable *vtable,pdl_thread *thread, char *flags, int noPthreadFlag ) {
	PDL_Indx i, j;
	PDL_Indx ndims=0;
	PDL_Indx nth; /* Index to dimensions */
	PDL_Indx mx, nids, nimpl, nthid;
	PDL_Indx mydim;
	PDL_Indx nthr = 0; PDL_Indx nthrd;

	PDLDEBUG_f(printf("Initthreadloop(%p)\n", (void*)thread);)
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

	  PDLDEBUG_f(pdl_pdl_warn("trying to reinitialize already initialized "
	     "thread (mem-leak!); freeing...");)
	}
	PDL_THR_SETMAGIC(thread);
	thread->gflags = 0;

	thread->npdls = npdls;
	thread->pdls = copy_pdl_array(pdls,npdls);
	thread->realdims = realdims;

	/* Accumulate the maximum number of thread dims across the collection of PDLs */
	nids=mx=0;
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		MAX2(nids,pdls[j]->nthreadids);
		MAX2(mx,pdls[j]->threadids[0] - realdims[j]);
	}
	ndims += thread->nimpl = nimpl = mx;

	pdl_autopthreadmagic(pdls, npdls, realdims, creating, noPthreadFlag);

	thread->mag_nth = -1;
	thread->mag_nthpdl = -1;
	thread->mag_nthr = -1;
	PDL_Indx nthreadids[nids];
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		/* Check for magical ndarrays (parallelized) */
		if((!nthr) &&
		  pdls[j]->magic &&
		  (nthr = pdl_magic_thread_nthreads(pdls[j],&nthrd))) {
			thread->mag_nthpdl = j;
			thread->mag_nth = nthrd - realdims[j];
			thread->mag_nthr = nthr;
			if(thread->mag_nth < 0) {
				pdl_croak_param(vtable,j,"Cannot magick non-threaded dims \n\t");
			}
		}
		for(i=0; i<nids; i++) {
			ndims += nthreadids[i] =
				PDLMAX(0, pdls[j]->nthreadids <= nids ?
				     pdls[j]->threadids[i+1]
				     - pdls[j]->threadids[i] : 0);
		}
	}
	if(nthr) {
		thread->gflags |= PDL_THREAD_MAGICKED;
	}
	ndims += thread->nextra = PDLMAX(0, nobl - ndims); /* If too few, add enough implicit dims */

	thread->ndims = ndims;
	thread->nimpl = nimpl;

      Newxz(thread->inds, ndims * (nthr>0 ? nthr : 1), PDL_Indx); /* Create space for pthread-specific inds (i.e. copy for each pthread)*/
      if(thread->inds == NULL) croak("Failed to allocate memory for thread->inds in pdlthread.c");

      Newxz(thread->dims, ndims * (nthr>0 ? nthr : 1), PDL_Indx);
      if(thread->dims == NULL) croak("Failed to allocate memory for thread->dims in pdlthread.c");
      for(nth=0; nth<ndims; nth++) thread->dims[nth]=1; // all start size 1

      Newxz(thread->offs, npdls * (nthr>0 ? nthr : 1), PDL_Indx); /* Create space for pthread-specific offs */
      if(thread->offs == NULL) croak("Failed to allocate memory for thread->offs in pdlthread.c");

      Newxz(thread->incs, ndims * npdls, PDL_Indx);
      if(thread->incs == NULL) croak("Failed to allocate memory for thread->incs in pdlthread.c");

      Newxz(thread->flags, npdls, char);
      if(thread->flags == NULL) croak("Failed to allocate memory for thread->flags in pdlthread.c");

	/* populate the per_pdl_flags */

	for (i=0;i<npdls; i++) {
	  if (PDL_VAFFOK(pdls[i]) && VAFFINE_FLAG_OK(flags,i))
	    thread->flags[i] |= PDL_THREAD_VAFFINE_OK;
	}
	flags = thread->flags; /* shortcut for the remainder */

/* Make implicit inds */

	for(nth=0; nth<nimpl; nth++) {                // Loop over number of implicit threads
	  for(j=0; j<npdls; j++) {                    // Now loop over the PDLs to be merged
	    if(creating[j]) continue;                 // If jth PDL is null, don't bother trying to match
	    if(thread->pdls[j]->threadids[0]-         // If we're off the end of the current PDLs dimlist,
	       realdims[j] <= nth)                    //    then just skip it.
	      continue;
	    if(pdls[j]->dims[nth+realdims[j]] != 1) { // If the current dim in the current PDL is not 1,
	      if(thread->dims[nth] != 1) {            //   ... and the current planned size isn't 1,
		if(thread->dims[nth] !=
		   pdls[j]->dims[nth+realdims[j]]) {  //   ... then check to make sure they're the same.
		  char buf0[BUFSIZ];
		  buf0[0] = '\0';
		  pdl_thread_mismatch_msg(
		    buf0, pdls, thread, nth, j, nimpl, realdims, creating
		  );
		  pdl_croak_param(vtable,j,"%s\n..",buf0);
		}

		/* If we're still here, they're the same -- OK! */

	      } else {                                // current planned size is 1 -- mod it to match this PDL
		thread->dims[nth] =
		  pdls[j]->dims[nth+realdims[j]];
	      }

	      thread->incs[nth*npdls+j] =                      // Update the corresponding data stride
		PDL_TREPRINC(pdls[j],flags[j],nth+realdims[j]);//   from the PDL or from its vafftrans if relevant.
	    }
	  }
	}

/* Go through everything again and make the real things */

	for(nthid=0; nthid<nids; nthid++) {
	for(i=0; i<nthreadids[nthid]; i++) {
		for(j=0; j<npdls; j++) {
			if(creating[j]) continue;
			if(thread->pdls[j]->nthreadids < nthid) continue;
			if(thread->pdls[j]->threadids[nthid+1]-
			   thread->pdls[j]->threadids[nthid]
					<= i) continue;
			mydim = i+thread->pdls[j]->threadids[nthid];
			if(pdls[j]->dims[mydim]
					!= 1) {
				if(thread->dims[nth] != 1) {
					if(thread->dims[nth] !=
						pdls[j]->dims[mydim]) {
						pdl_croak_param(vtable,j,"Mismatched Implicit thread dimension %d: should be %d, is %d",
							i,
							thread->dims[nth],
							pdls[j]->dims[i+realdims[j]]);
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

/* If threading, make the true offsets and dims.. */

	thread->mag_skip = 0;
	thread->mag_stride = 0;
	if(nthr > 0) {
		int n1 = thread->dims[thread->mag_nth] / nthr;
		int n2 = thread->dims[thread->mag_nth] % nthr;
		thread->mag_stride = n1;
		if(n2) {
		    n1++;
		    thread->mag_skip = n2;
		}
		thread->dims[thread->mag_nth] = n1;
		for(i=1; i<nthr; i++)
		    for(j=0; j<ndims; j++)
			thread->dims[j + i*ndims] = thread->dims[j];
		if (n2)
		    for(i=n2; i<nthr; i++)
			thread->dims[thread->mag_nth + i*ndims]--;
	}
	thread->gflags |= PDL_THREAD_INITIALIZED;
	PDLDEBUG_f(dump_thread(thread);)
}

void pdl_thread_create_parameter(pdl_thread *thread, PDL_Indx j,PDL_Indx *dims,
				 int temp)
{
	PDL_Indx i;
	PDL_Indx td = temp ? 0 : thread->nimpl;
	if(!temp && thread->nimpl != thread->ndims - thread->nextra) {
		pdl_croak_param(thread->transvtable,j,
			"Trying to create parameter while explicitly threading.\
See the manual for why this is impossible");
	}
	pdl_reallocdims(thread->pdls[j], thread->realdims[j] + td);
	for(i=0; i<thread->realdims[j]; i++)
		thread->pdls[j]->dims[i] = dims[i];
	if (!temp)
	  for(i=0; i<thread->nimpl; i++)
		thread->pdls[j]->dims[i+thread->realdims[j]] =
		    (i == thread->mag_nth && thread->mag_nthr > 0)
		     ? PDL_THR_OFFSET(thread->mag_nthr, thread)
		     : thread->dims[i];
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
	PDL_Indx i,j, npdls = thread->npdls;
	PDL_Indx *offsp; int thr;
	PDL_Indx *inds, *dims;
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
	offsp = pdl_get_threadoffsp_int(thread,&thr, &inds, &dims);
	for(j=0; j<npdls; j++)
	    offsp[j] = PDL_TREPROFFS(thread->pdls[j],thread->flags[j]);
	if (thr)
	    for(j=0; j<npdls; j++) offsp[j] += PDL_THR_OFFSET(thr, thread) *
		thread->incs[thread->mag_nth*npdls + j];
	return 0;
}

/* nth is how many dims are done inside the threadloop itself */
/* inds is how far along each non-threadloop dim we are */
int pdl_iterthreadloop(pdl_thread *thread,PDL_Indx nth) {
	PDL_Indx i,j;
	int another_threadloop = 0;
	PDL_Indx *offsp; int thr;
	PDL_Indx *inds, *dims;
	offsp = pdl_get_threadoffsp_int(thread,&thr, &inds, &dims);
	for(i=nth; i<thread->ndims; i++) {
		inds[i] ++;
		if( inds[i] >= dims[i])
			inds[i] = 0;
		else
		{	another_threadloop = 1; break; }
	}
	if (another_threadloop)
	    for(j=0; j<thread->npdls; j++) {
		offsp[j] = PDL_TREPROFFS(thread->pdls[j],thread->flags[j]);
		if (thr)
		    offsp[j] += PDL_THR_OFFSET(thr, thread) *
			thread->incs[thread->mag_nth*thread->npdls + j];
		for(i=nth; i<thread->ndims; i++) {
		    offsp[j] += thread->incs[i*thread->npdls+j] * inds[i];
		}
	    }
	return another_threadloop;
}

void pdl_croak_param(pdl_transvtable *vtable,int paramIndex, char *pat, ...)
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

  if(vtable)
  {
    if(paramIndex < 0 || paramIndex >= vtable->npdls)
    {
      strcat(msgptr, "ERROR: UNKNOWN PARAMETER");
      msgptr_advance();
    }
    else
    {
      snprintf(msgptr, remaining, "PDL: %s(", vtable->name);
      msgptr_advance();

      for(i=0; i<vtable->npdls; i++)
      {
        snprintf(msgptr, remaining, "%s", vtable->par_names[i]);
        msgptr_advance();

        if(i < vtable->npdls-1)
        {
          snprintf(msgptr, remaining, ",");
          msgptr_advance();
        }
      }

      snprintf(msgptr, remaining, "): Parameter '%s':\n",
               vtable->par_names[paramIndex]);
      msgptr_advance();
    }
  }

  va_start(args,pat);

  vsnprintf(msgptr, remaining, pat, args);

  va_end(args);

  pdl_pdl_barf(message);
}
