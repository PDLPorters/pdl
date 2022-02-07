/* XXX NOTE THAT IT IS NOT SAFE TO USE ->pdls MEMBER OUTSIDE
   INITTHREADSTRUCT! */

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#define MAX2(a,b) if((b)>(a)) a=b;

/**** Convenience routines for moving around collections
 **** of indices and PDL pointers.
 ****/
static pdl **copy_pdl_array (pdl **from, int size) {
  pdl **to;
  Newx (to, size, pdl*);
  return (pdl **) CopyD (from, to, size, pdl*);
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
  if (thr < 0) return NULL;
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
  if (thr < 0) return NULL;
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
	if (thr < 0) return NULL;
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

void pdl_freethreadstruct(pdl_thread *thread) {
	PDLDEBUG_f(printf("freethreadstruct(%p, %p %p %p %p %p %p)\n",
		(void*)thread,
		(void*)(thread->inds), (void*)(thread->dims), (void*)(thread->offs),
		(void*)(thread->incs), (void*)(thread->flags), (void*)(thread->pdls)));
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
	PDLDEBUG_f(printf("clearthreadstruct(%p)\n", (void*)it));
	it->transvtable = 0;it->inds = 0;it->dims = 0;
	it->ndims = it->nimpl = it->npdls = 0; it->offs = 0;
	it->pdls = 0;it->incs = 0; it->realdims=0; it->flags=0;
	it->gflags=0; /* unsets PDL_THREAD_INITIALIZED among others */
	PDL_THR_CLRMAGIC(it);
}

pdl_error pdl_find_max_pthread(
  pdl **pdls, int npdls, PDL_Indx* realdims, PDL_Indx* creating,
  int target_pthread,
  int *p_maxPthread, /* Maximum achievable pthread */
  int *p_maxPthreadDim, /* Threaded dim number that has the max num pthreads */
  int *p_maxPthreadPDL /* PDL that has the max (or right at the target) num pthreads */
) {
  pdl_error PDL_err = {0, NULL, 0};
  PDL_Indx j, k, t;
  /* Build int arrays of threaded dim numbers and sizes for each pdl */
  PDL_Indx max_remainder = 0;
  PDL_Indx nthreadedDims[npdls];
  PDL_Indx *threadedDims[npdls];
  PDL_Indx *threadedDimSizes[npdls];
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    threadedDims[j]     = (PDL_Indx*) malloc(sizeof(PDL_Indx) * pdls[j]->ndims);
    if (!threadedDims[j]) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
    threadedDimSizes[j] = (PDL_Indx*) malloc(sizeof(PDL_Indx) * pdls[j]->ndims);
    if (!threadedDimSizes[j]) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
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
  return PDL_err;
}

/* Function to auto-add pthreading magic (i.e. hints for multiple
   processor threads) to the pdls, based on the target number of
   pthreads and the pdl-threaded dimensions.
   noPthreadFlag is a flag indicating that the pdl thread that
   called this function is not multiple processor threading safe,
   so no pthreading magic will be added.
*/
pdl_error pdl_autopthreadmagic( pdl **pdls, int npdls, PDL_Indx* realdims, PDL_Indx* creating, int noPthreadFlag ){
	pdl_error PDL_err = {0, NULL, 0};
	PDL_Indx j, nthrd;
	PDL_Indx largest_nvals = 0;  /* The largest PDL size for all the pdls involved */
	int maxPthreadPDL = -1; /* PDL that has the max (or right at the target) num pthreads */
	int maxPthreadDim = -1; /* Threaded dim number that has the max num pthreads */
	int maxPthread = 0;    /* Maximum achievable pthread */
	int target_pthread = pdl_autopthread_targ;
	pdl_autopthread_actual = 0; /* Initialize the global variable indicating actual number of pthreads */
	pdl_autopthread_dim = -1; /* Initialize the global variable indicating actual dim pthreaded on */

	/* Don't do anything if auto_pthreading is turned off (i.e. equal zero) */
	if( !target_pthread ) return PDL_err;

	/* Remove any existing threading magic */
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;

		/* Remove thread magic, if there is some set for this pdl */
		if( pdls[j]->magic &&
		  (pdl_magic_thread_nthreads(pdls[j],&nthrd))) {
			PDL_RETERROR(PDL_err, pdl_add_threading_magic(pdls[j], -1, -1));
		}
	}

	if( noPthreadFlag ) return PDL_err;  /* Don't go further if the current pdl function isn't thread-safe */

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
		return PDL_err;

	PDL_RETERROR(PDL_err, pdl_find_max_pthread(
	  pdls, npdls, realdims, creating, target_pthread,
	  &maxPthread, &maxPthreadDim, &maxPthreadPDL
	));

	/* Add threading magic */
	if( maxPthread > 1 ){
		PDL_RETERROR(PDL_err, pdl_add_threading_magic(pdls[maxPthreadPDL], maxPthreadDim, maxPthread));
		pdl_autopthread_actual = maxPthread; /* Set the global variable indicating actual number of pthreads */
		pdl_autopthread_dim = maxPthreadDim;
	}
	return PDL_err;
}

pdl_error pdl_dim_checks(
  pdl_transvtable *vtable, pdl **pdls,
  pdl_thread *pdlthread, PDL_Indx *creating,
  PDL_Indx *ind_sizes
) {
  pdl_error PDL_err = {0, NULL, 0};
  PDL_Indx i, j, ind_id;
  PDLDEBUG_f(printf("pdl_dim_checks %p:\n", ind_sizes);
    printf("  ind_sizes: "); pdl_print_iarr(ind_sizes, vtable->ninds);printf("\n"));
  for (i=0; i<vtable->npdls; i++) {
    PDL_Indx ninds = vtable->par_realdims[i];
    PDLDEBUG_f(printf("pdl_dim_checks pdl %"IND_FLAG" (creating=%"IND_FLAG" ninds=%"IND_FLAG"): ", i, creating[i], ninds));
    pdl *pdl = pdls[i];
    PDL_Indx ndims = pdl->ndims;
    PDLDEBUG_f(pdl_dump(pdl));
    if (creating[i]) {
      PDL_Indx dims[PDLMAX(ninds+1, 1)];
      for (j=0; j<ninds; j++)
	dims[j] = ind_sizes[PDL_IND_ID(vtable, i, j)];
      if (vtable->par_flags[i] & PDL_PARAM_ISTEMP)
	dims[ninds] = 1;
      PDL_RETERROR(PDL_err, pdl_thread_create_parameter(
	pdlthread,i,dims,
	vtable->par_flags[i] & PDL_PARAM_ISTEMP
      ));
    } else {
      PDL_Indx *dims = pdl->dims;
      if (pdl->state & PDL_NOMYDIMS)
        return pdl_make_error(PDL_EUSERERROR,
          "Error in %s: input parameter '%s' is null\n",
          vtable->name, vtable->par_names[i]
        );
      if (ninds > 0 && ndims < ninds) {
	/* Dimensional promotion when number of dims is less than required: */
	for (j=0; j<ninds; j++) {
	  ind_id = PDL_IND_ID(vtable, i, j);
	  if (ndims < j+1 && ind_sizes[ind_id] <= 1) ind_sizes[ind_id] = 1;
	}
      }
      /* Now, the real check. */
      for (j=0; j<ninds; j++) {
	ind_id = PDL_IND_ID(vtable, i, j);
	if (ind_sizes[ind_id] == -1 || (ndims > j && ind_sizes[ind_id] == 1))
	  ind_sizes[ind_id] = dims[j];
	else if (ndims > j && ind_sizes[ind_id] != dims[j] && dims[j] != 1)
	  return pdl_make_error(PDL_EUSERERROR,
	    "Error in %s: parameter '%s' index %s size %"IND_FLAG", but ndarray dim has size %"IND_FLAG"\n",
	    vtable->name, vtable->par_names[i], vtable->ind_names[ind_id],
	    ind_sizes[ind_id], dims[j]
	  );
      }
      if (vtable->par_flags[i] & PDL_PARAM_ISPHYS)
	PDL_RETERROR(PDL_err, pdl_make_physical(pdl));
    }
  }
  for (i=0; i<vtable->npdls; i++) {
    PDL_Indx ninds = vtable->par_realdims[i];
    short flags = vtable->par_flags[i];
    if (!ninds || !(flags & PDL_PARAM_ISPHYS)) continue;
    pdl *pdl = pdls[i];
    PDL_Indx *dims = pdl->dims;
    for (j=0; j<ninds; j++) {
      ind_id = PDL_IND_ID(vtable, i, j);
      if (ind_sizes[ind_id] > 1 && ind_sizes[ind_id] != dims[j])
	return pdl_make_error(PDL_EUSERERROR,
	  "Error in %s: [phys] parameter '%s' index '%s' size %"IND_FLAG", but ndarray dim has size %"IND_FLAG"\n",
	  vtable->name, vtable->par_names[i], vtable->ind_names[ind_id],
	  ind_sizes[ind_id], dims[j]
	);
    }
  }
  PDLDEBUG_f(printf("pdl_dim_checks after:\n");
    printf("  ind_sizes: "); pdl_print_iarr(ind_sizes, vtable->ninds);
    printf("\n"));
  return PDL_err;
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
pdl_error pdl_initthreadstruct(int nobl,
	pdl **pdls,PDL_Indx *realdims,PDL_Indx *creating,PDL_Indx npdls,
	pdl_transvtable *vtable,pdl_thread *thread,
	PDL_Indx *ind_sizes, PDL_Indx *inc_sizes,
	char *flags, int noPthreadFlag
) {
	pdl_error PDL_err = {0, NULL, 0};
	PDL_Indx i, j;
	PDL_Indx ndims=0;
	PDL_Indx nth; /* Index to dimensions */
	PDL_Indx mx, nids, nimpl, nthid;
	PDL_Indx mydim;
	PDL_Indx nthr = 0; PDL_Indx nthrd;

	PDLDEBUG_f(printf("initthreadstruct(%p)\n", (void*)thread));
	char already_alloced = (thread->magicno == PDL_THR_MAGICNO &&
	    thread->gflags & PDL_THREAD_INITIALIZED);
	PDL_Indx already_nthr = already_alloced ? thread->mag_nthr : -1;
	PDL_Indx already_ndims = already_alloced ? thread->ndims : -1;
	PDL_THR_SETMAGIC(thread);
	thread->gflags = 0;

	thread->npdls = npdls;
	thread->realdims = realdims;
	thread->transvtable = vtable;

	/* Accumulate the maximum number of thread dims across the collection of PDLs */
	nids=mx=0;
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		MAX2(nids,pdls[j]->nthreadids);
		MAX2(mx,pdls[j]->threadids[0] - realdims[j]);
	}
	ndims += thread->nimpl = nimpl = mx;

	PDL_RETERROR(PDL_err, pdl_autopthreadmagic(pdls, npdls, realdims, creating, noPthreadFlag));

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
				return pdl_croak_param(vtable,j,"Cannot magick non-threaded dims \n\t");
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

	PDL_Indx nthr1 = PDLMAX(nthr, 1);
	if (!already_alloced || already_nthr != nthr1 || ndims != already_ndims) {
	  if (already_alloced) {
	    Safefree(thread->inds);
	    Safefree(thread->dims);
	    Safefree(thread->offs);
	  }
	  Newxz(thread->inds, ndims * nthr1, PDL_Indx); /* Create space for pthread-specific inds (i.e. copy for each pthread)*/
	  if(thread->inds == NULL) return pdl_make_error_simple(PDL_EFATAL, "Failed to allocate memory for thread->inds in pdlthread.c");
	  Newxz(thread->dims, ndims * nthr1, PDL_Indx);
	  if(thread->dims == NULL) return pdl_make_error_simple(PDL_EFATAL, "Failed to allocate memory for thread->dims in pdlthread.c");
	  Newxz(thread->offs, npdls * nthr1, PDL_Indx); /* Create space for pthread-specific offs */
	  if(thread->offs == NULL) return pdl_make_error_simple(PDL_EFATAL, "Failed to allocate memory for thread->offs in pdlthread.c");
	}
	for(nth=0; nth<ndims; nth++) thread->dims[nth]=1; // all start size 1

	if (!already_alloced) {
	  thread->pdls = copy_pdl_array(pdls,npdls);
	  Newxz(thread->incs, ndims * npdls, PDL_Indx);
	  if(thread->incs == NULL) return pdl_make_error_simple(PDL_EFATAL, "Failed to allocate memory for thread->incs in pdlthread.c");
	  Newxz(thread->flags, npdls, char);
	  if(thread->flags == NULL) return pdl_make_error_simple(PDL_EFATAL, "Failed to allocate memory for thread->flags in pdlthread.c");
	}

	/* populate the per_pdl_flags */
	for (i=0;i<npdls; i++) {
	  if (pdls[i] && PDL_VAFFOK(pdls[i]) && VAFFINE_FLAG_OK(flags,i))
	    thread->flags[i] |= PDL_THREAD_VAFFINE_OK;
	  if (vtable && vtable->par_flags[i] & PDL_PARAM_ISTEMP)
	    thread->flags[i] |= PDL_THREAD_TEMP;
	}
	flags = thread->flags; /* shortcut for the remainder */

/* Make implicit inds */

	for(nth=0; nth<nimpl; nth++) {                // Loop over number of implicit threads
	  for(j=0; j<npdls; j++) {                    // Now loop over the PDLs to be merged
	    if(creating[j]) continue;                 // If jth PDL is null, don't bother trying to match
	    if(pdls[j]->threadids[0]-         // If we're off the end of the current PDLs dimlist,
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
		  return pdl_croak_param(vtable,j,"%s\n..",buf0);
		}

		/* If we're still here, they're the same -- OK! */

	      } else {                                // current planned size is 1 -- mod it to match this PDL
		thread->dims[nth] =
		  pdls[j]->dims[nth+realdims[j]];
	      }

	      PDL_THR_INC(thread->incs, npdls, j, nth) =       // Update the corresponding data stride
		PDL_TREPRINC(pdls[j],flags[j],nth+realdims[j]);//   from the PDL or from its vafftrans if relevant.
	    }
	  }
	}

/* Go through everything again and make the real things */

	for(nthid=0; nthid<nids; nthid++) {
	for(i=0; i<nthreadids[nthid]; i++) {
		for(j=0; j<npdls; j++) {
			if (PDL_TISTEMP(flags[j]))
			    PDL_THR_INC(thread->incs, npdls, j, nth) =
				pdls[j]->dimincs[pdls[j]->ndims-1];
			if(creating[j]) continue;
			if(pdls[j]->nthreadids < nthid) continue;
			if(pdls[j]->threadids[nthid+1]-
			   pdls[j]->threadids[nthid]
					<= i) continue;
			mydim = i+pdls[j]->threadids[nthid];
			if(pdls[j]->dims[mydim]
					!= 1) {
				if(thread->dims[nth] != 1) {
					if(thread->dims[nth] !=
						pdls[j]->dims[mydim]) {
						return pdl_croak_param(vtable,j,"Mismatched Implicit thread dimension %d: should be %d, is %d",
							i,
							thread->dims[nth],
							pdls[j]->dims[i+realdims[j]]);
					}
				} else {
					thread->dims[nth] =
						pdls[j]->dims[mydim];
				}
				PDL_THR_INC(thread->incs, npdls, j, nth) =
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
	if (ind_sizes)
		PDL_RETERROR(PDL_err, pdl_dim_checks(vtable, pdls, thread, creating, ind_sizes));
	if (inc_sizes)
	  for (i=0; i<vtable->npdls; i++) {
	    pdl *pdl = pdls[i];
	    for (j=0; j<vtable->par_realdims[i]; j++)
	      inc_sizes[PDL_INC_ID(vtable,i,j)] =
		(pdl->ndims <= j || pdl->dims[j] <= 1) ? 0 :
		(vtable->par_flags[i] & PDL_PARAM_ISPHYS) ? pdl->dimincs[j] :
		PDL_REPRINC(pdl,j);
	  }
	thread->gflags |= PDL_THREAD_INITIALIZED;
	PDLDEBUG_f(pdl_dump_thread(thread));
	return PDL_err;
}

pdl_error pdl_thread_create_parameter(pdl_thread *thread, PDL_Indx j,PDL_Indx *dims,
				 int temp)
{
	pdl_error PDL_err = {0, NULL, 0};
	PDL_Indx i;
	PDL_Indx td = temp ? 0 : thread->nimpl;
	if(!temp && thread->nimpl != thread->ndims - thread->nextra) {
		return pdl_croak_param(thread->transvtable,j,
			"Trying to create parameter while explicitly threading.\
See the manual for why this is impossible");
	}
	if (!thread->pdls[j] && !(thread->pdls[j] = pdl_pdlnew()))
	    return pdl_make_error_simple(PDL_EFATAL, "Error in pdlnew");
	PDL_RETERROR(PDL_err, pdl_reallocdims(thread->pdls[j], thread->realdims[j] + td + (temp ? 1 : 0)));
	for(i=0; i<thread->realdims[j] + (temp ? 1 : 0); i++)
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
		PDL_THR_INC(thread->incs, thread->npdls, j, i) =
		  temp ? 0 :
		  PDL_REPRINC(thread->pdls[j],i+thread->realdims[j]);
	}
	return PDL_err;
}

int pdl_startthreadloop(pdl_thread *thread,pdl_error (*func)(pdl_trans *),
			pdl_trans *t, pdl_error *error_ret) {
	PDL_Indx j, npdls = thread->npdls;
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
			for(j=0; j<npdls; j++) {
			    if (!(t->vtable->par_flags[j] & PDL_PARAM_ISTEMP)) continue;
			    pdl *it = thread->pdls[j];
			    it->dims[it->ndims-1] = thread->mag_nthr;
			    pdl_resize_defaultincs(it);
			    pdl_error PDL_err = pdl_make_physical(it);
			    if (PDL_err.error) {
				*error_ret = PDL_err;
				return 1;
			    }
			}
			pdl_error PDL_err = pdl_magic_thread_cast(thread->pdls[thread->mag_nthpdl],
				func,t, thread);
			if (PDL_err.error) {
			    *error_ret = PDL_err;
			    return 1;
			}
			thread->gflags &= ~PDL_THREAD_MAGICK_BUSY;
			return 1; /* DON'T DO THREADLOOP AGAIN */
		}
	}
	offsp = pdl_get_threadoffsp_int(thread,&thr, &inds, &dims);
	if (!offsp) return -1;
	for(j=0; j<npdls; j++) {
	    offsp[j] = PDL_TREPROFFS(thread->pdls[j],thread->flags[j]);
	}
	if (thr)
	    for(j=0; j<npdls; j++)
		offsp[j] += PDL_TISTEMP(thread->flags[j])
		    ? thr * thread->pdls[j]->dimincs[thread->pdls[j]->ndims-1]
		    : PDL_THR_OFFSET(thr, thread) *
			PDL_THR_INC(thread->incs, thread->npdls, j, thread->mag_nth);
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
	if (!offsp) return -1;
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
		    offsp[j] += PDL_TISTEMP(thread->flags[j])
			? thr * thread->pdls[j]->dimincs[thread->pdls[j]->ndims-1]
			: PDL_THR_OFFSET(thr, thread) *
			    PDL_THR_INC(thread->incs, thread->npdls, j, thread->mag_nth);
		for(i=nth; i<thread->ndims; i++) {
		    offsp[j] += PDL_THR_INC(thread->incs, thread->npdls, j, i) * inds[i];
		}
	    }
	return another_threadloop;
}
