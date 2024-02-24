/* XXX NOTE THAT IT IS NOT SAFE TO USE ->pdls MEMBER OUTSIDE
   INITBROADCASTSTRUCT! */

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
 * pdl_get_broadcastdims - get the pthread-specific broadcasting dims from a PDL
 *  Input: broadcast structure
 *  Outputs: see above (returned by function)
 */
PDL_Indx *pdl_get_broadcastdims(pdl_broadcast *broadcast)
{
  /* The non-multithreaded case: return just the usual value */
  if (!(broadcast->gflags & PDL_BROADCAST_MAGICKED)) return broadcast->dims;
  int thr = pdl_magic_get_thread(broadcast->pdls[broadcast->mag_nthpdl]);
  if (thr < 0) return NULL;
  return broadcast->dims + thr * broadcast->ndims;
}


/*******
 * pdl_get_threadoffsp - get the pthread-specific offset arrays from a PDL
 *  Input: broadcast structure
 *  Outputs: Pointer to pthread-specific offset array (returned by function)
 */
PDL_Indx *pdl_get_threadoffsp(pdl_broadcast *broadcast)
{
  /* The non-multithreaded case: return just the usual offsets */
  if (!(broadcast->gflags & PDL_BROADCAST_MAGICKED)) return broadcast->offs;
  int thr = pdl_magic_get_thread(broadcast->pdls[broadcast->mag_nthpdl]);
  if (thr < 0) return NULL;
  return broadcast->offs + thr * broadcast->npdls;
}


/* Function to get the pthread-specific offset, indexes and pthread number for the supplied broadcast structure
   Input: broadcast structure
   Outputs: Pointer to pthread-specific offset array (returned by function)
            Pointer to pthread-specific index array (ind Pointer supplied and modified by function)
            Pointer to pthread-specific dims array (dims Pointer supplied and modified by function)
	    Pthread index for the current pthread   ( thr supplied and modified by function)
*/
PDL_Indx* pdl_get_threadoffsp_int(pdl_broadcast *broadcast, int *pthr, PDL_Indx **inds, PDL_Indx **dims)
{
  if(broadcast->gflags & PDL_BROADCAST_MAGICKED) {
	int thr = pdl_magic_get_thread(broadcast->pdls[broadcast->mag_nthpdl]);
	if (thr < 0) return NULL;
	*pthr = thr;
	*inds = broadcast->inds  + thr * broadcast->ndims;
	*dims = broadcast->dims  + thr * broadcast->ndims;
	return  broadcast->offs  + thr * broadcast->npdls;
  }
  *pthr = 0;
/* The non-multithreaded case: return just the usual offsets */
  *dims = broadcast->dims;
  *inds = broadcast->inds;
  return broadcast->offs;
}

void pdl_freebroadcaststruct(pdl_broadcast *broadcast) {
	PDLDEBUG_f(printf("freebroadcaststruct(%p, %p %p %p %p %p %p)\n",
		(void*)broadcast,
		(void*)(broadcast->inds), (void*)(broadcast->dims), (void*)(broadcast->offs),
		(void*)(broadcast->incs), (void*)(broadcast->flags), (void*)(broadcast->pdls)));
	if(!broadcast->inds) {return;}
	Safefree(broadcast->inds);
	Safefree(broadcast->dims);
	Safefree(broadcast->offs);
	Safefree(broadcast->incs);
	Safefree(broadcast->flags);
	Safefree(broadcast->pdls);
	pdl_clearbroadcaststruct(broadcast);
}

void pdl_clearbroadcaststruct(pdl_broadcast *it) {
	PDLDEBUG_f(printf("clearbroadcaststruct(%p)\n", (void*)it));
	it->transvtable = 0;it->inds = 0;it->dims = 0;
	it->ndims = it->nimpl = it->npdls = 0; it->offs = 0;
	it->pdls = 0;it->incs = 0; it->realdims=0; it->flags=0;
	it->gflags=0; /* unsets PDL_BROADCAST_INITIALIZED among others */
	PDL_CLRMAGIC(it);
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
  /* Build int arrays of broadcasted dim numbers and sizes for each pdl */
  PDL_Indx max_remainder = 0;
  PDL_Indx nbroadcastedDims[npdls];
  PDL_Indx *broadcastedDims[npdls];
  PDL_Indx *broadcastedDimSizes[npdls];
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    broadcastedDims[j]     = (PDL_Indx*) malloc(sizeof(PDL_Indx) * pdls[j]->ndims);
    if (!broadcastedDims[j]) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
    broadcastedDimSizes[j] = (PDL_Indx*) malloc(sizeof(PDL_Indx) * pdls[j]->ndims);
    if (!broadcastedDimSizes[j]) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
  }
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    for( k=0, t = realdims[j]; t < pdls[j]->ndims; t++, k++ ){
      broadcastedDimSizes[j][k] = pdls[j]->dims[t];
      broadcastedDims[j][k]      = t;
    }
    nbroadcastedDims[j] = pdls[j]->ndims - realdims[j];
  }
  /* Go through each broadcasted dim and find best match */
  *p_maxPthread = 0;
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    for( k=0; k < nbroadcastedDims[j]; k++){
      PDL_Indx this_dim = broadcastedDimSizes[j][k];
      PDL_Indx this_remainder = this_dim % target_pthread;
      if( this_remainder == 0 ){
        *p_maxPthread = target_pthread;
        *p_maxPthreadPDL = j;
        *p_maxPthreadDim = broadcastedDims[j][k];
        break;
      }
      if( this_dim > *p_maxPthread && this_remainder > max_remainder ){
        max_remainder = this_remainder;
        *p_maxPthread = PDLMIN(target_pthread, this_dim);
        *p_maxPthreadPDL = j;
        *p_maxPthreadDim = broadcastedDims[j][k];
      }
    }
    /* Don't go any further if target pthread achieved */
    if( *p_maxPthread == target_pthread ) break;
  }
  PDLDEBUG_f(pdl_dump_broadcasting_info(
    npdls, creating, target_pthread,
    nbroadcastedDims, broadcastedDims, broadcastedDimSizes,
    *p_maxPthreadPDL, *p_maxPthreadDim, *p_maxPthread
  ));
  /* Free the stuff we allocated */
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    free(broadcastedDims[j]);
    free(broadcastedDimSizes[j]);
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

	/* See if the largest nvals is above the auto_pthread threshold */
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
  pdl_broadcast *broadcast, PDL_Indx *creating,
  PDL_Indx *ind_sizes, char load_only
) {
  pdl_error PDL_err = {0, NULL, 0};
  PDL_Indx i, j, ind_id;
  PDLDEBUG_f(printf("pdl_dim_checks(load_only=%d) %p:\n", load_only, ind_sizes);
    printf("  ind_sizes: "); pdl_print_iarr(ind_sizes, vtable->ninds);printf("\n"));
  for (i=0; i<vtable->npdls; i++) {
    pdl *pdl = pdls[i];
    PDL_Indx ninds = vtable->par_realdims[i], ndims = pdl->ndims;
    PDLDEBUG_f(printf("pdl_dim_checks pdl %"IND_FLAG" (creating=%"IND_FLAG" ninds=%"IND_FLAG"): ", i, creating[i], ninds));
    PDLDEBUG_f(pdl_dump(pdl));
    short flags = vtable->par_flags[i];
    if (!load_only && creating[i]) continue;
    PDL_Indx *dims = pdl->dims;
    for (j=0; j<ninds; j++) {
      PDL_Indx ind_id = PDL_IND_ID(vtable, i, j), ind_sz = ind_sizes[ind_id];
      if (j >= ndims && ind_sz == -1)
        /* Dimensional promotion when number of dims is less than required: */
        ind_sz = ind_sizes[ind_id] = 1;
      if (load_only && creating[i]) continue;
      if (ind_sz == -1 || (j < ndims && ind_sz == 1)) {
        ind_sizes[ind_id] = dims[j];
        continue;
      }
      if (j < ndims && ind_sz != dims[j] && (
        (i >= vtable->nparents) ||
        dims[j] != 1
      ))
        return pdl_make_error(PDL_EUSERERROR,
          "Error in %s: parameter '%s' index %s size %"IND_FLAG", but ndarray dim has size %"IND_FLAG"\n",
          vtable->name, vtable->par_names[i], vtable->ind_names[ind_id],
          ind_sz, dims[j]
        );
    }
  }
  if (!load_only)
    for (i=0; i<vtable->npdls; i++) {
      PDL_Indx ninds = vtable->par_realdims[i];
      short flags = vtable->par_flags[i];
      if (!ninds || !(flags & PDL_PARAM_ISPHYS) || creating[i]) continue;
      pdl *pdl = pdls[i];
      PDL_Indx *dims = pdl->dims;
      for (j=0; j<ninds; j++) {
        PDL_Indx ind_id = PDL_IND_ID(vtable, i, j), ind_sz = ind_sizes[ind_id];
        if (!(ind_sz > 1 && ind_sz != dims[j])) continue;
        return pdl_make_error(PDL_EUSERERROR,
          "Error in %s: [phys] parameter '%s' index '%s' size %"IND_FLAG", but ndarray dim has size %"IND_FLAG"\n",
          vtable->name, vtable->par_names[i], vtable->ind_names[ind_id],
          ind_sz, dims[j]
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
 *  noPthreadFlag is a flag to indicate the trans is not pthreading safe
 *   (i.e. don't attempt to create multiple posix threads to execute)
 */
pdl_error pdl_initbroadcaststruct(int nobl,
	pdl **pdls,PDL_Indx *realdims,PDL_Indx *creating,PDL_Indx npdls,
	pdl_transvtable *vtable,pdl_broadcast *broadcast,
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

	PDLDEBUG_f(printf("initbroadcaststruct(%p)\n", (void*)broadcast));
	char already_alloced = (broadcast->magicno == PDL_BRC_MAGICNO &&
	    broadcast->gflags & PDL_BROADCAST_INITIALIZED);
	PDL_Indx already_nthr = already_alloced ? broadcast->mag_nthr : -1;
	PDL_Indx already_ndims = already_alloced ? broadcast->ndims : -1;
	PDL_BRC_SETMAGIC(broadcast);
	broadcast->gflags = 0;

	broadcast->npdls = npdls;
	broadcast->realdims = realdims;
	broadcast->transvtable = vtable;

	/* Accumulate the maximum number of broadcast dims across the collection of PDLs */
	nids=mx=0;
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		MAX2(nids,pdls[j]->nbroadcastids);
		MAX2(mx,pdls[j]->broadcastids[0] - realdims[j]);
	}
	ndims += broadcast->nimpl = nimpl = mx;

	PDL_RETERROR(PDL_err, pdl_autopthreadmagic(pdls, npdls, realdims, creating, noPthreadFlag));

	broadcast->mag_nth = -1;
	broadcast->mag_nthpdl = -1;
	broadcast->mag_nthr = -1;
	PDL_Indx nbroadcastids[nids];
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		/* Check for magical ndarrays (parallelized) */
		if((!nthr) &&
		  pdls[j]->magic &&
		  (nthr = pdl_magic_thread_nthreads(pdls[j],&nthrd))) {
			broadcast->mag_nthpdl = j;
			broadcast->mag_nth = nthrd - realdims[j];
			broadcast->mag_nthr = nthr;
			if(broadcast->mag_nth < 0) {
				return pdl_croak_param(vtable,j,"Cannot magick non-broadcasted dims \n\t");
			}
		}
		for(i=0; i<nids; i++) {
			ndims += nbroadcastids[i] =
				PDLMAX(0, pdls[j]->nbroadcastids <= nids ?
				     pdls[j]->broadcastids[i+1]
				     - pdls[j]->broadcastids[i] : 0);
		}
	}
	if(nthr) {
		broadcast->gflags |= PDL_BROADCAST_MAGICKED;
	}
	ndims += broadcast->nextra = PDLMAX(0, nobl - ndims); /* If too few, add enough implicit dims */

	broadcast->ndims = ndims;
	broadcast->nimpl = nimpl;

	PDL_Indx nthr1 = PDLMAX(nthr, 1);
	if (!already_alloced || already_nthr != nthr1 || ndims != already_ndims) {
	  if (already_alloced) {
	    Safefree(broadcast->inds);
	    Safefree(broadcast->dims);
	    Safefree(broadcast->offs);
	  }
	  Newxz(broadcast->inds, ndims * nthr1, PDL_Indx); /* Create space for pthread-specific inds (i.e. copy for each pthread)*/
	  if(broadcast->inds == NULL) return pdl_make_error_simple(PDL_EFATAL, "Failed to allocate memory for broadcast->inds in pdlbroadcast.c");
	  Newxz(broadcast->dims, ndims * nthr1, PDL_Indx);
	  if(broadcast->dims == NULL) return pdl_make_error_simple(PDL_EFATAL, "Failed to allocate memory for broadcast->dims in pdlbroadcast.c");
	  Newxz(broadcast->offs, npdls * nthr1, PDL_Indx); /* Create space for pthread-specific offs */
	  if(broadcast->offs == NULL) return pdl_make_error_simple(PDL_EFATAL, "Failed to allocate memory for broadcast->offs in pdlbroadcast.c");
	}
	for(nth=0; nth<ndims; nth++) broadcast->dims[nth]=1; // all start size 1

	if (!already_alloced) {
	  broadcast->pdls = copy_pdl_array(pdls,npdls);
	  Newxz(broadcast->incs, ndims * npdls, PDL_Indx);
	  if(broadcast->incs == NULL) return pdl_make_error_simple(PDL_EFATAL, "Failed to allocate memory for broadcast->incs in pdlbroadcast.c");
	  Newxz(broadcast->flags, npdls, char);
	  if(broadcast->flags == NULL) return pdl_make_error_simple(PDL_EFATAL, "Failed to allocate memory for broadcast->flags in pdlbroadcast.c");
	}

	/* populate the per_pdl_flags */
	for (i=0;i<npdls; i++) {
	  if (pdls[i] && PDL_VAFFOK(pdls[i]) && VAFFINE_FLAG_OK(flags,i))
	    broadcast->flags[i] |= PDL_BROADCAST_VAFFINE_OK;
	  if (vtable && vtable->par_flags[i] & PDL_PARAM_ISTEMP)
	    broadcast->flags[i] |= PDL_BROADCAST_TEMP;
	}
	flags = broadcast->flags; /* shortcut for the remainder */

/* Make implicit inds */

	for(nth=0; nth<nimpl; nth++) {                // Loop over number of implicit broadcast dims
	  for(j=0; j<npdls; j++) {                    // Now loop over the PDLs to be merged
	    if(creating[j]) continue;                 // If jth PDL is null, don't bother trying to match
	    if(pdls[j]->broadcastids[0]-         // If we're off the end of the current PDLs dimlist,
	       realdims[j] <= nth)                    //    then just skip it.
	      continue;
	    PDL_Indx cur_pdl_dim = pdls[j]->dims[nth+realdims[j]];
	    if (vtable && j >= vtable->nparents && cur_pdl_dim == 1 && cur_pdl_dim != broadcast->dims[nth])
	      return pdl_make_error(PDL_EUSERERROR,
		"Error in %s: output parameter '%s' implicit dim %"IND_FLAG" size %"IND_FLAG", but dim has size %"IND_FLAG"\n",
		vtable->name, vtable->par_names[j], nth, broadcast->dims[nth],
		cur_pdl_dim
	      );
	    if(cur_pdl_dim != 1) { // If the current dim in the current PDL is not 1,
	      if(broadcast->dims[nth] != 1) {            //   ... and the current planned size isn't 1,
		if(broadcast->dims[nth] != cur_pdl_dim) { //   ... then check to make sure they're the same.
		  char buf0[BUFSIZ];
		  buf0[0] = '\0';
		  pdl_broadcast_mismatch_msg(
		    buf0, pdls, broadcast, nth, j, nimpl, realdims, creating
		  );
		  return pdl_croak_param(vtable,j,"%s\n..",buf0);
		}

		/* If we're still here, they're the same -- OK! */

	      } else {                                // current planned size is 1 -- mod it to match this PDL
		broadcast->dims[nth] = cur_pdl_dim;
	      }

	      PDL_BRC_INC(broadcast->incs, npdls, j, nth) =       // Update the corresponding data stride
		PDL_BREPRINC(pdls[j],flags[j],nth+realdims[j]);//   from the PDL or from its vafftrans if relevant.
	    }
	  }
	}

/* Go through everything again and make the real things */

	for(nthid=0; nthid<nids; nthid++) {
	for(i=0; i<nbroadcastids[nthid]; i++) {
		for(j=0; j<npdls; j++) {
			if (PDL_BISTEMP(flags[j]))
			    PDL_BRC_INC(broadcast->incs, npdls, j, nth) =
				pdls[j]->dimincs[pdls[j]->ndims-1];
			if(creating[j]) continue;
			if(pdls[j]->nbroadcastids < nthid) continue;
			if(pdls[j]->broadcastids[nthid+1]-
			   pdls[j]->broadcastids[nthid]
					<= i) continue;
			mydim = i+pdls[j]->broadcastids[nthid];
			if(pdls[j]->dims[mydim]
					!= 1) {
				if(broadcast->dims[nth] != 1) {
					if(broadcast->dims[nth] !=
						pdls[j]->dims[mydim]) {
						return pdl_croak_param(vtable,j,"Mismatched Implicit broadcast dimension %d: should be %d, is %d",
							i,
							broadcast->dims[nth],
							pdls[j]->dims[i+realdims[j]]);
					}
				} else {
					broadcast->dims[nth] =
						pdls[j]->dims[mydim];
				}
				PDL_BRC_INC(broadcast->incs, npdls, j, nth) =
					PDL_BREPRINC(pdls[j],flags[j],mydim);
			}
		}
		nth++;
	}
	}

/* If threading, make the true offsets and dims.. */

	broadcast->mag_skip = 0;
	broadcast->mag_stride = 0;
	if(nthr > 0) {
		int n1 = broadcast->dims[broadcast->mag_nth] / nthr;
		int n2 = broadcast->dims[broadcast->mag_nth] % nthr;
		broadcast->mag_stride = n1;
		if(n2) {
		    n1++;
		    broadcast->mag_skip = n2;
		}
		broadcast->dims[broadcast->mag_nth] = n1;
		for(i=1; i<nthr; i++)
		    for(j=0; j<ndims; j++)
			broadcast->dims[j + i*ndims] = broadcast->dims[j];
		if (n2)
		    for(i=n2; i<nthr; i++)
			broadcast->dims[broadcast->mag_nth + i*ndims]--;
	}
	if (ind_sizes) {
		PDL_RETERROR(PDL_err, pdl_dim_checks(vtable, pdls, broadcast, creating, ind_sizes, 0));
		for (i=0; i<vtable->npdls; i++) {
		  PDL_Indx ninds = vtable->par_realdims[i];
		  short flags = vtable->par_flags[i];
		  if (creating[i]) {
		    PDL_Indx dims[PDLMAX(ninds+1, 1)];
		    for (j=0; j<ninds; j++)
		      dims[j] = ind_sizes[PDL_IND_ID(vtable, i, j)];
		    if (flags & PDL_PARAM_ISTEMP)
		      dims[ninds] = 1;
		    PDL_RETERROR(PDL_err, pdl_broadcast_create_parameter(
		      broadcast,i,dims,
		      flags & PDL_PARAM_ISTEMP
		    ));
		  }
		  if ((flags & PDL_PARAM_ISPHYS))
		    PDL_RETERROR(PDL_err, pdl_make_physical(pdls[i]));
		}
	}
	if (inc_sizes)
	  for (i=0; i<vtable->npdls; i++) {
	    pdl *pdl = pdls[i];
	    for (j=0; j<vtable->par_realdims[i]; j++)
	      inc_sizes[PDL_INC_ID(vtable,i,j)] =
		(pdl->ndims <= j || pdl->dims[j] <= 1) ? 0 :
		(vtable->par_flags[i] & PDL_PARAM_ISPHYS) ? pdl->dimincs[j] :
		PDL_REPRINC(pdl,j);
	  }
	broadcast->gflags |= PDL_BROADCAST_INITIALIZED;
	PDLDEBUG_f(pdl_dump_broadcast(broadcast));
	return PDL_err;
}

pdl_error pdl_broadcast_create_parameter(pdl_broadcast *broadcast, PDL_Indx j,PDL_Indx *dims,
				 int temp)
{
	pdl_error PDL_err = {0, NULL, 0};
	PDL_Indx i;
	PDL_Indx td = temp ? 0 : broadcast->nimpl;
	if(!temp && broadcast->nimpl != broadcast->ndims - broadcast->nextra) {
		return pdl_croak_param(broadcast->transvtable,j,
			"Trying to create parameter while explicitly broadcasting.\
See the manual for why this is impossible");
	}
	if (!broadcast->pdls[j] && !(broadcast->pdls[j] = pdl_pdlnew()))
	    return pdl_make_error_simple(PDL_EFATAL, "Error in pdlnew");
	PDL_RETERROR(PDL_err, pdl_reallocdims(broadcast->pdls[j], broadcast->realdims[j] + td + (temp ? 1 : 0)));
	for(i=0; i<broadcast->realdims[j] + (temp ? 1 : 0); i++)
		broadcast->pdls[j]->dims[i] = dims[i];
	if (!temp)
	  for(i=0; i<broadcast->nimpl; i++)
		broadcast->pdls[j]->dims[i+broadcast->realdims[j]] =
		    (i == broadcast->mag_nth && broadcast->mag_nthr > 0)
		     ? PDL_BRC_OFFSET(broadcast->mag_nthr, broadcast)
		     : broadcast->dims[i];
	broadcast->pdls[j]->broadcastids[0] = td + broadcast->realdims[j];
	pdl_resize_defaultincs(broadcast->pdls[j]);
	for(i=0; i<broadcast->nimpl; i++) {
		PDL_BRC_INC(broadcast->incs, broadcast->npdls, j, i) =
		  temp ? 0 :
		  PDL_REPRINC(broadcast->pdls[j],i+broadcast->realdims[j]);
	}
	return PDL_err;
}

int pdl_startbroadcastloop(pdl_broadcast *broadcast,pdl_error (*func)(pdl_trans *),
			pdl_trans *t, pdl_error *error_ret) {
	PDL_Indx j, npdls = broadcast->npdls;
	PDL_Indx *offsp; int thr;
	PDL_Indx *inds, *dims;
	if(  (broadcast->gflags & (PDL_BROADCAST_MAGICKED | PDL_BROADCAST_MAGICK_BUSY))
	     == PDL_BROADCAST_MAGICKED ) {
		/* If no function supplied (i.e. being called from PDL::broadcast_over), don't run in parallel */
		if(!func) {
			broadcast->gflags &= ~PDL_BROADCAST_MAGICKED; /* Cancel thread_magicked */
		}
		else{
			broadcast->gflags |= PDL_BROADCAST_MAGICK_BUSY;
			/* Do the broadcastloop magically (i.e. in parallel) */
			for(j=0; j<npdls; j++) {
			    if (!(t->vtable->par_flags[j] & PDL_PARAM_ISTEMP)) continue;
			    pdl *it = broadcast->pdls[j];
			    it->dims[it->ndims-1] = broadcast->mag_nthr;
			    pdl_resize_defaultincs(it);
			    pdl_error PDL_err = pdl_make_physical(it);
			    if (PDL_err.error) {
				*error_ret = PDL_err;
				return 1;
			    }
			}
			pdl_error PDL_err = pdl_magic_thread_cast(broadcast->pdls[broadcast->mag_nthpdl],
				func,t, broadcast);
			if (PDL_err.error) {
			    *error_ret = PDL_err;
			    return 1;
			}
			broadcast->gflags &= ~PDL_BROADCAST_MAGICK_BUSY;
			return 1; /* DON'T DO BROADCASTLOOP AGAIN */
		}
	}
	offsp = pdl_get_threadoffsp_int(broadcast,&thr, &inds, &dims);
	if (!offsp) return -1;
	for(j=0; j<broadcast->ndims; j++)
	    if (!dims[j]) return 1; /* do nothing if empty */
	for(j=0; j<npdls; j++) {
	    offsp[j] = PDL_BREPROFFS(broadcast->pdls[j],broadcast->flags[j]);
	}
	if (thr)
	    for(j=0; j<npdls; j++)
		offsp[j] += PDL_BISTEMP(broadcast->flags[j])
		    ? thr * broadcast->pdls[j]->dimincs[broadcast->pdls[j]->ndims-1]
		    : PDL_BRC_OFFSET(thr, broadcast) *
			PDL_BRC_INC(broadcast->incs, broadcast->npdls, j, broadcast->mag_nth);
	return 0;
}

/* nth is how many dims are done inside the broadcastloop itself */
/* inds is how far along each non-broadcastloop dim we are */
int pdl_iterbroadcastloop(pdl_broadcast *broadcast,PDL_Indx nth) {
	PDL_Indx i,j;
	int another_broadcastloop = 0;
	PDL_Indx *offsp; int thr;
	PDL_Indx *inds, *dims;
	offsp = pdl_get_threadoffsp_int(broadcast,&thr, &inds, &dims);
	if (!offsp) return -1;
	for(i=nth; i<broadcast->ndims; i++) {
		inds[i] ++;
		if( inds[i] >= dims[i])
			inds[i] = 0;
		else
		{	another_broadcastloop = 1; break; }
	}
	if (another_broadcastloop)
	    for(j=0; j<broadcast->npdls; j++) {
		offsp[j] = PDL_BREPROFFS(broadcast->pdls[j],broadcast->flags[j]);
		if (thr)
		    offsp[j] += PDL_BISTEMP(broadcast->flags[j])
			? thr * broadcast->pdls[j]->dimincs[broadcast->pdls[j]->ndims-1]
			: PDL_BRC_OFFSET(thr, broadcast) *
			    PDL_BRC_INC(broadcast->incs, broadcast->npdls, j, broadcast->mag_nth);
		for(i=nth; i<broadcast->ndims; i++) {
		    offsp[j] += PDL_BRC_INC(broadcast->incs, broadcast->npdls, j, i) * inds[i];
		}
	    }
	return another_broadcastloop;
}
