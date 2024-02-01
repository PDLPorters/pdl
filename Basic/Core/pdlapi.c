/* pdlapi.c - functions for manipulating pdl structs  */

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#define VTABLE_OR_DEFAULT(what, trans, is_fwd, func, default_func) \
  do { \
    PDLDEBUG_f(printf("VTOD call " #func "(%p=%s)\n", trans, trans->vtable->name)); \
    what(PDL_err, ((trans)->vtable->func \
      ? (trans)->vtable->func \
      : pdl_ ## default_func)(trans)); \
    pdl **pdls = trans->pdls; \
    PDL_Indx i, istart = is_fwd ? trans->vtable->nparents : 0, iend = is_fwd ? trans->vtable->npdls : trans->vtable->nparents; \
    for (i = istart; i < iend; i++) \
      if (pdls[i] && (pdls[i]->state & PDL_BADVAL)) \
        pdl_propagate_badflag(pdls[i], !!(pdls[i]->state & PDL_BADVAL)); \
  } while (0)

#define REDODIMS(what, trans) do { \
    if ((trans)->vtable->redodims) \
      what(PDL_err, pdl_dim_checks( \
	(trans)->vtable, (trans)->pdls, \
	NULL, NULL, \
	(trans)->ind_sizes, 1)); \
    if (trans->dims_redone) { \
	FREETRANS(trans, 0); \
	if (PDL_err.error) return PDL_err; \
	trans->dims_redone = 0; \
    } \
    what(PDL_err, ((trans)->vtable->redodims \
      ? (trans)->vtable->redodims \
      : pdl_redodims_default)(trans)); \
  } while (0)
#define READDATA(trans) VTABLE_OR_DEFAULT(PDL_ACCUMERROR, trans, 1, readdata, readdata_affine)
#define WRITEDATA(trans) VTABLE_OR_DEFAULT(PDL_ACCUMERROR, trans, 0, writebackdata, writebackdata_affine)
#define FREETRANS(trans, destroy) \
    if(trans->vtable->freetrans) { \
	PDLDEBUG_f(printf("call freetrans\n")); \
	PDL_ACCUMERROR(PDL_err, trans->vtable->freetrans(trans, destroy)); \
	    /* ignore error for now as need to still free rest */ \
	if (destroy) PDL_CLRMAGIC(trans); \
    }
#define CHANGED(...) \
    PDL_ACCUMERROR(PDL_err, pdl_changed(__VA_ARGS__))

extern Core PDL;

pdl_error pdl__make_physvaffine_recprotect(pdl *it, int recurse_count);
/* Make sure transformation is done */
pdl_error pdl__ensure_trans(pdl_trans *trans,int what,int *wd, int recurse_count)
{
	pdl_error PDL_err = {0, NULL, 0};
	PDLDEBUG_f(printf("pdl__ensure_trans %p what=", trans); pdl_dump_flags_fixspace(what, 0, PDL_FLAGS_PDL));
	PDL_TR_CHKMAGIC(trans);
	PDL_Indx j, flag=what, par_pvaf=0;
	pdl_transvtable *vtable = trans->vtable;
/* Make parents physical */
	for(j=0; j<vtable->npdls; j++) {
		if(VAFFINE_FLAG_OK(vtable->per_pdl_flags,j))
			par_pvaf++;
		PDL_RETERROR(PDL_err, pdl__make_physvaffine_recprotect(trans->pdls[j], recurse_count+1));
	}
	for(j=vtable->nparents; j<vtable->npdls; j++)
		flag |= trans->pdls[j]->state & PDL_ANYCHANGED;
	PDLDEBUG_f(printf("pdl__ensure_trans after accum, par_pvaf=%"IND_FLAG" flag=", par_pvaf); pdl_dump_flags_fixspace(what, 0, PDL_FLAGS_PDL));
	if (flag & PDL_PARENTDIMSCHANGED) REDODIMS(PDL_RETERROR, trans);
	for(j=vtable->nparents; j<vtable->npdls; j++)
		if(trans->pdls[j]->trans_parent == trans)
			PDL_ENSURE_ALLOCATED(trans->pdls[j]);
	if(flag & (PDL_PARENTDATACHANGED | PDL_PARENTDIMSCHANGED)) {
		if(par_pvaf && (trans->flags & PDL_ITRANS_ISAFFINE)) {
		  /* Attention: this assumes affine = p2child */
		  /* need to signal that redodims has already been called */
		        PDLDEBUG_f(printf("pdl__ensure_trans vaffine output turning off dimschanged, before="); pdl_dump_flags_fixspace(trans->pdls[1]->state, 0, PDL_FLAGS_PDL));
		        trans->pdls[1]->state &= ~PDL_PARENTDIMSCHANGED;
			PDL_RETERROR(PDL_err, pdl__make_physvaffine_recprotect(trans->pdls[1], recurse_count+1));
			PDL_ACCUMERROR(PDL_err, pdl_readdata_vaffine(trans->pdls[1]));
		} else
			READDATA(trans);
	}
	for(j=vtable->nparents; j<vtable->npdls; j++) {
		pdl *child = trans->pdls[j];
		PDLDEBUG_f(printf("pdl__ensure_trans child=%p turning off all changed, before=", child); pdl_dump_flags_fixspace(child->state, 0, PDL_FLAGS_PDL));
		child->state &= ~PDL_ANYCHANGED;
		if (!wd) continue;
		PDLDEBUG_f(printf("   pdl__ensure_trans wd="); pdl_dump_flags_fixspace(wd[j], 0, PDL_FLAGS_PDL));
		char isvaffine = (PDL_VAFFOK(child) &&
		    VAFFINE_FLAG_OK(vtable->per_pdl_flags,j));
		if (!isvaffine || (wd[j] & PDL_PARENTDIMSCHANGED))
		    CHANGED(child,wd[j],0);
		if (isvaffine)
		    CHANGED(child->vafftrans->from,PDL_PARENTDATACHANGED,0);
	}
	return PDL_err;
}

pdl *pdl_null() {
	PDLDEBUG_f(printf("pdl_null\n"));
	return pdl_pdlnew();
}

pdl *pdl_scalar(PDL_Anyval anyval) {
	PDLDEBUG_f(printf("pdl_scalar type=%d val=", anyval.type); pdl_dump_anyval(anyval); printf("\n"););
	pdl *it = pdl_pdlnew();
	if (!it) return it;
	it->datatype = anyval.type;
	it->broadcastids[0] = it->ndims = 0; /* 0 dims in a scalar */
	pdl_resize_defaultincs(it);
	pdl_error PDL_err = pdl_allocdata(it);
	if (PDL_err.error) { pdl_destroy(it); return NULL; }
	it->value = anyval.value;
	it->state &= ~(PDL_NOMYDIMS); /* has dims */
	return it;
}

pdl *pdl_get_convertedpdl(pdl *old,int type) {
	PDLDEBUG_f(printf("pdl_get_convertedpdl\n"));
	if(old->datatype == type) return old;
	pdl *it = pdl_pdlnew();
	if (!it) return it;
	pdl_error PDL_err = pdl_converttypei_new(old,it,type);
	if (PDL_err.error) { pdl_destroy(it); return NULL; }
	return it;
}

pdl_error pdl_allocdata(pdl *it) {
  pdl_error PDL_err = {0, NULL, 0};
  PDLDEBUG_f(printf("pdl_allocdata %p, %"IND_FLAG", %d\n",(void*)it, it->nvals,
	  it->datatype));
  if(it->nvals < 0)
    return pdl_make_error(PDL_EUSERERROR, "Tried to allocdata with %"IND_FLAG" values", it->nvals);
  PDL_Indx nbytes = it->nvals * pdl_howbig(it->datatype);
  PDL_Indx ncurr  = it->nbytes;
  if (ncurr == nbytes)
    return PDL_err;    /* Nothing to be done */
  if(it->state & PDL_DONTTOUCHDATA)
    return pdl_make_error_simple(PDL_EUSERERROR, "Trying to touch data of an untouchable (mmapped?) pdl");
  char was_useheap = (ncurr > sizeof(it->value)),
    will_useheap = (nbytes > sizeof(it->value));
  if (!was_useheap && !will_useheap) {
    it->data = &it->value;
  } else if (!will_useheap) {
    /* was heap, now not */
    void *data_old = it->data;
    memmove(it->data = &it->value, data_old, PDLMIN(ncurr, nbytes));
    SvREFCNT_dec((SV*)it->datasv);
    it->datasv = NULL;
  } else {
    /* now change to be heap */
    if (it->datasv == NULL)
      it->datasv = newSVpvn("", 0);
    (void)SvGROW((SV*)it->datasv, nbytes);
    SvCUR_set((SV*)it->datasv, nbytes);
    if (it->data && !was_useheap)
      memmove(SvPV_nolen((SV*)it->datasv), it->data, PDLMIN(ncurr, nbytes));
    it->data = SvPV_nolen((SV*)it->datasv);
  }
  if (nbytes > ncurr) memset(it->data + ncurr, 0, nbytes - ncurr);
  it->nbytes = nbytes;
  it->state |= PDL_ALLOCATED;
  PDLDEBUG_f(pdl_dump(it));
  return PDL_err;
}

pdl* pdl_pdlnew() {
     pdl* it;
     it = (pdl*) malloc(sizeof(pdl));
     if (!it) return it;
     memset(it, 0, sizeof(pdl));
     it->magicno = PDL_MAGICNO;
     it->datatype = PDL_D;
     it->trans_parent = NULL;
     it->vafftrans = NULL;
     it->sv = NULL;
     it->datasv = 0;
     it->data = 0;
     it->has_badvalue = 0;
     it->state = PDL_NOMYDIMS;
     it->dims = it->def_dims;
     it->nbytes = it->nvals = it->dims[0] = 0;
     it->dimincs = it->def_dimincs;
     it->dimincs[0] = 1;
     it->nbroadcastids = 1;
     it->broadcastids = it->def_broadcastids;
     it->broadcastids[0] = it->ndims = 1;
     PDL_Indx i;
     for(i=0; i<PDL_NCHILDREN; i++) {it->trans_children.trans[i]=NULL;}
     it->trans_children.next = NULL;
     it->magic = 0;
     it->hdrsv = 0;
     PDLDEBUG_f(printf("pdl_pdlnew %p (size=%zu)\n",(void*)it,sizeof(pdl)));
     return it;
}

void pdl_vafftrans_free(pdl *it)
{
	if(it->vafftrans && it->vafftrans->incs)
		free(it->vafftrans->incs);
	if(it->vafftrans)
		free(it->vafftrans);
	it->vafftrans=0;
	it->state &= ~PDL_OPT_VAFFTRANSOK;
}

pdl_error pdl_vafftrans_alloc(pdl *it)
{
	pdl_error PDL_err = {0, NULL, 0};
	if(!it->vafftrans) {
		it->vafftrans = malloc(sizeof(*(it->vafftrans)));
		if (!it->vafftrans) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
		it->vafftrans->incs = 0;
		it->vafftrans->ndims = 0;
	}
	if(!it->vafftrans->incs || it->vafftrans->ndims < it->ndims ) {
		if(it->vafftrans->incs) free(it->vafftrans->incs);
		it->vafftrans->incs = malloc(sizeof(*(it->vafftrans->incs))
					     * (size_t)it->ndims);
		if (!it->vafftrans->incs) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
		it->vafftrans->ndims = it->ndims;
	}
	return PDL_err;
}

/* Recursive! */
void pdl_vafftrans_remove(pdl * it)
{
	PDLDEBUG_f(printf("pdl_vafftrans_remove: %p\n", (void*)it));
	PDL_DECL_CHILDLOOP(it);
	PDL_START_CHILDLOOP(it)
		pdl_trans *t = PDL_CHILDLOOP_THISCHILD(it);
		if(!(t->flags & PDL_ITRANS_ISAFFINE)) continue;
		int i;
		for(i=t->vtable->nparents; i<t->vtable->npdls; i++)
			pdl_vafftrans_remove(t->pdls[i]);
	PDL_END_CHILDLOOP(it)
	pdl_vafftrans_free(it);
}

/* Explicit free. Do not use, use destroy instead, which causes this
   to be called when the time is right */
pdl_error pdl__free(pdl *it) {
    pdl_error PDL_err = {0, NULL, 0};
    PDLDEBUG_f(printf("pdl__free %p\n",(void*)it));
    PDL_CHKMAGIC(it);
    /* now check if magic is still there */
    if (pdl__ismagic(it)) {
      PDLDEBUG_f(printf("%p is still magic\n",(void*)it);pdl__print_magic(it));
    }
    it->magicno = 0x42424245;
    if(it->dims       != it->def_dims)       free((void*)it->dims);
    if(it->dimincs    != it->def_dimincs)    free((void*)it->dimincs);
    if(it->broadcastids  != it->def_broadcastids)  free((void*)it->broadcastids);
    if(it->vafftrans) {
	pdl_vafftrans_free(it);
    }
    pdl_trans_children *p1 = it->trans_children.next;
    while(p1) {
	pdl_trans_children *p2 = p1->next;
	free(p1);
	p1 = p2;
    }
/* Call special freeing magic, if exists */
    if(PDL_ISMAGIC(it)) {
	pdl__call_magic(it, PDL_MAGIC_DELETEDATA);
	pdl__magic_free(it);
    }
    if(it->datasv) {
	    PDLDEBUG_f(printf("SvREFCNT_dec datasv=%p\n",it->datasv);)
	    SvREFCNT_dec(it->datasv);
	    it->data=0;
    } else if(it->data && it->data != &it->value) {
	    pdl_pdl_warn("Warning: special data without datasv is not freed currently!!");
    }
    if(it->hdrsv) {
	PDLDEBUG_f(printf("SvREFCNT_dec hdrsv=%p\n",it->hdrsv);)
	SvREFCNT_dec(it->hdrsv);
	it->hdrsv = 0;
    }
    free(it);
    PDLDEBUG_f(printf("pdl__free end %p\n",(void*)it));
    return PDL_err;
}

/* NULL out the pdl from the trans's inputs, and the trans from the
   pdl's trans_children */
void pdl__removetrans_children(pdl *it,pdl_trans *trans)
{
	PDLDEBUG_f(printf("pdl__removetrans_children(%s=%p): %p\n",
	  trans->vtable->name, trans, it));
	PDL_Indx i; int flag = 0;
	for(i=0; i<trans->vtable->nparents; i++)
		if(trans->pdls[i] == it)
			trans->pdls[i] = NULL;
	PDL_DECL_CHILDLOOP(it);
	PDL_START_CHILDLOOP(it)
		if (PDL_CHILDLOOP_THISCHILD(it) != trans) continue;
		PDL_CHILDLOOP_THISCHILD(it) = NULL;
		flag = 1;
		/* Can't return; might be many times (e.g. $x+$x) */
	PDL_END_CHILDLOOP(it)
	/* this might be due to a croak when performing the trans; so
	   warn only for now, otherwise we leave trans undestructed ! */
	if(!flag)
		pdl_pdl_warn("Child not found for pdl %p, trans %p\n",it, trans);
}

/* NULL out the trans's nth pdl in/output, and this trans as pdl's
   trans_parent */
void pdl__removetrans_parent(pdl *it, pdl_trans *trans, PDL_Indx nth)
{
	PDLDEBUG_f(printf("pdl__removetrans_parent from %p (%s=%p): %"IND_FLAG"\n",
	  it, trans->vtable->name, trans, nth));
	trans->pdls[nth] = 0;
	if (it->trans_parent != trans) return; /* only do rest if trans is parent */
	it->trans_parent = 0;
	it->state &= ~PDL_MYDIMS_TRANS;
}

pdl_error pdl_trans_finaldestroy(pdl_trans *trans)
{
  pdl_error PDL_err = {0, NULL, 0};
  PDLDEBUG_f(printf("pdl_trans_finaldestroy %p\n", trans));
  FREETRANS(trans, 1);
  if(trans->vtable->flags & PDL_TRANS_DO_BROADCAST)
    pdl_freebroadcaststruct(&trans->broadcast);
  trans->vtable = 0; /* Make sure no-one uses this */
  PDLDEBUG_f(printf("call free\n"));
  if (trans->params) free(trans->params);
  free(trans->ind_sizes);
  free(trans->inc_sizes);
  free(trans);
  return PDL_err;
}

pdl_error pdl__destroy_recprotect(pdl *it, int recurse_count);
pdl_error pdl_destroytransform(pdl_trans *trans,int ensure,int *wd, int recurse_count)
{
	pdl_error PDL_err = {0, NULL, 0};
	PDL_TR_CHKMAGIC(trans);
	PDL_Indx j;
	int ismutual = (trans->flags & PDL_ITRANS_DO_DATAFLOW_ANY);
	if (!trans->vtable)
		return pdl_make_error(PDL_EFATAL, "ZERO VTABLE DESTTRAN 0x%p %d\n",trans,ensure);
	if (!ismutual) for(j=0; j<trans->vtable->nparents; j++)
	  if (!trans->pdls[j]) return pdl_make_error(PDL_EFATAL, "NULL pdls[%td] in %s", j, trans->vtable->name); else
	  if (trans->pdls[j]->state & PDL_DATAFLOW_ANY) { ismutual=1; break; }
	PDLDEBUG_f(printf("pdl_destroytransform %s=%p (ensure=%d ismutual=%d)\n",
			  trans->vtable->name,
			  (void*)trans,ensure,ismutual));
	if(ensure)
		PDL_ACCUMERROR(PDL_err, pdl__ensure_trans(trans,ismutual ? 0 : PDL_PARENTDIMSCHANGED,wd, recurse_count+1));
	pdl *destbuffer[trans->vtable->npdls];
	int ndest = 0;
	for(j=0; j<trans->vtable->nparents; j++) {
	  pdl *parent = trans->pdls[j];
	  if(!parent) continue;
	  PDL_CHKMAGIC(parent);
	  pdl__removetrans_children(parent,trans);
	  if (!(parent->state & PDL_DESTROYING) && !parent->sv) {
	    parent->state |= PDL_DESTROYING; /* so no mark twice */
	    destbuffer[ndest++] = parent;
	  }
	}
	for(j=trans->vtable->nparents; j<trans->vtable->npdls; j++) {
	  pdl *child = trans->pdls[j];
	  PDL_CHKMAGIC(child);
	  pdl__removetrans_parent(child,trans,j);
	  if (ismutual && child->vafftrans) pdl_vafftrans_remove(child);
	  if ((!(child->state & PDL_DESTROYING) && !child->sv) ||
	      (trans->vtable->par_flags[j] & PDL_PARAM_ISTEMP)) {
	    child->state |= PDL_DESTROYING; /* so no mark twice */
	    destbuffer[ndest++] = child;
	  }
	}
	PDL_ACCUMERROR(PDL_err, pdl_trans_finaldestroy(trans));
	for(j=0; j<ndest; j++) {
		destbuffer[j]->state &= ~PDL_DESTROYING; /* safe, set by us */
		PDL_ACCUMERROR(PDL_err, pdl__destroy_recprotect(destbuffer[j], recurse_count+1));
	}
	PDLDEBUG_f(printf("pdl_destroytransform leaving %p\n", (void*)trans));
	return PDL_err;
}

/*
  A ndarray may be
   - a parent of something - just ensure & destroy
   - a child of something - just ensure & destroy
   - parent of two pdls which both propagate backwards - mustn't destroy.
   - both parent and child at same time, to something that propagates.
  Therefore, simple rules:
   - allowed to destroy if
      1. a parent with max. 1 backwards propagating transformation
      2. a child with no trans_children
  When an ndarray is destroyed, it must tell its trans_children and/or
  parent.
*/
pdl_error pdl__destroy_recprotect(pdl *it, int recurse_count) {
    pdl_error PDL_err = {0, NULL, 0};
    int nback=0,nback2=0,nforw=0;
    int nafn=0;
    PDL_DECL_CHILDLOOP(it);
    PDL_CHKMAGIC(it);
    PDLDEBUG_f(printf("pdl_destroy: ");pdl_dump(it));
    if(it->state & PDL_DESTROYING) {
        PDLDEBUG_f(printf("  already destroying, returning\n"));
	return PDL_err;
    }
    it->state |= PDL_DESTROYING;
    /* Clear the sv field so that there will be no dangling ptrs */
    if(it->sv) {
	    mg_free((SV *)it->sv);
	    sv_setiv(it->sv,0x4242);
	    it->sv = NULL;
    }
    /* 1. count the trans_children that do flow */
    PDL_START_CHILDLOOP(it)
	pdl_trans *curt = PDL_CHILDLOOP_THISCHILD(it);
	if(curt->flags & PDL_ITRANS_DO_DATAFLOW_F)
		nforw ++;
	if(curt->flags & PDL_ITRANS_DO_DATAFLOW_B)
	{
		nback ++;
		/* Cases where more than two in relationship
		 * must always be soft-destroyed */
		if(curt->vtable->npdls > 2) nback2++;
	}
	if ((curt->flags & PDL_ITRANS_ISAFFINE) && !(curt->pdls[1]->state & PDL_ALLOCATED))
		nafn ++;
    PDL_END_CHILDLOOP(it)
/* First case where we may not destroy */
    if(nback2 > 0) goto soft_destroy;
    if(nback > 1) goto soft_destroy;
/* Also not here */
    if(it->trans_parent && nforw) goto soft_destroy;
/* Also, we do not wish to destroy if the trans_children would be larger
 * than the parent and are currently not allocated (e.g. lags).
 * Because this is too much work to check, we refrain from destroying
 * for now if there is an affine child that is not allocated
 */
    if(nafn) goto soft_destroy;
    if(pdl__magic_isundestroyable(it)) {
        PDLDEBUG_f(printf("pdl_destroy not destroying as magic %p\n",(void*)it));
	goto soft_destroy;
    }
    PDL_START_CHILDLOOP(it)
	PDL_RETERROR(PDL_err, pdl_destroytransform(PDL_CHILDLOOP_THISCHILD(it),1,NULL, recurse_count+1));
    PDL_END_CHILDLOOP(it)
    pdl_trans *trans = it->trans_parent;
    if (trans)
        /* Ensure only if there are other children! */
      PDL_RETERROR(PDL_err, pdl_destroytransform(trans,trans->vtable->npdls
				      - trans->vtable->nparents > 1,NULL, recurse_count+1));
/* Here, this is a child but has no children - fall through to hard_destroy */
   PDL_RETERROR(PDL_err, pdl__free(it));
   PDLDEBUG_f(printf("pdl_destroy end %p\n",(void*)it));
   return PDL_err;
  soft_destroy:
    PDLDEBUG_f(printf("pdl_destroy may have dependencies, not destroy %p, nba(%d, %d), nforw(%d), tra(%p=%s), nafn(%d)\n",
	it, nback, nback2, nforw, it->trans_parent, it->trans_parent?it->trans_parent->vtable->name:"", nafn));
    it->state &= ~PDL_DESTROYING;
    return PDL_err;
}

pdl_error pdl_destroy(pdl *it) {
  return pdl__destroy_recprotect(it, 0);
}

/* Straight copy, no dataflow */
pdl *pdl_hard_copy(pdl *src) {
	PDLDEBUG_f(printf("pdl_hard_copy (src=%p): ", src));
	pdl_error PDL_err = pdl_make_physical(src); /* Wasteful XXX... should be lazier */
	if (PDL_err.error) return NULL;
	int i;
	pdl *it = pdl_pdlnew();
	if (!it) return it;
	it->state = 0;
	PDLDEBUG_f(printf("pdl_hard_copy (src=%p): ", src);pdl_dump(it));
	it->datatype = src->datatype;
	PDL_err = pdl_setdims(it,src->dims,src->ndims);
	if (PDL_err.error) { pdl_destroy(it); return NULL; }
	PDL_err = pdl_allocdata(it);
	if (PDL_err.error) { pdl_destroy(it); return NULL; }
	if(src->state & PDL_NOMYDIMS)
		it->state |= PDL_NOMYDIMS;
	PDL_err = pdl_reallocbroadcastids(it,src->nbroadcastids);
	if (PDL_err.error) { pdl_destroy(it); return NULL; }
	for(i=0; i<src->nbroadcastids; i++) {
		it->broadcastids[i] = src->broadcastids[i];
	}
	memcpy(it->data,src->data, pdl_howbig(it->datatype) * (size_t)it->nvals);
	return it;
}

/* Reallocate this PDL to have ndims dimensions. */
pdl_error pdl_reallocdims(pdl *it, PDL_Indx ndims) {
   pdl_error PDL_err = {0, NULL, 0};
   if (it->ndims < ndims) {  /* Need to realloc for more */
      if(it->dims != it->def_dims) free(it->dims);
      if(it->dimincs != it->def_dimincs) free(it->dimincs);
      if (ndims>PDL_NDIMS) {  /* Need to malloc */
         if (!(it->dims = malloc(ndims*sizeof(*(it->dims)))))
            return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
         if (!(it->dimincs = malloc(ndims*sizeof(*(it->dimincs))))) {
            free(it->dims);
            return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
         }
      }
      else {
         it->dims = it->def_dims;
         it->dimincs = it->def_dimincs;
      }
   }
   it->ndims = ndims;
   return PDL_err;
}

/* Reallocate n broadcastids. Set the new extra ones to the end */
pdl_error pdl_reallocbroadcastids(pdl *it, PDL_Indx n) {
	pdl_error PDL_err = {0, NULL, 0};
	PDL_Indx i;
	PDL_Indx *olds; PDL_Indx nold;
	if(n <= it->nbroadcastids) {
		it->nbroadcastids = n;
		it->broadcastids[n-1] = it->ndims;
		return PDL_err;
	}
	nold = it->nbroadcastids; olds = it->broadcastids;
	if(n > PDL_NBROADCASTIDS) {
		it->broadcastids = malloc(sizeof(*(it->broadcastids))*n);
		if (!it->broadcastids) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
	} else {
		it->broadcastids = it->def_broadcastids;
	}
	it->nbroadcastids = n;
	if(it->broadcastids != olds) {
		for(i=0; i<nold && i<n; i++)
			it->broadcastids[i] = olds[i];
	}
	if(olds != it->def_broadcastids) { free(olds); }
	for(i=nold; i<it->nbroadcastids; i++) {
		it->broadcastids[i] = it->ndims;
	}
	return PDL_err;
}

/* Recalculate default increments */
void pdl_resize_defaultincs(pdl *it) {
    PDL_Indx inc = 1, i = 0;
    for(i=0; i<it->ndims; i++) {
	it->dimincs[i] = inc; inc *= it->dims[i];
    }
    if (it->nvals != inc) /* Need to realloc only if nvals changed */
	it->state &= ~PDL_ALLOCATED;
    it->nvals = inc;
}

/* Init dims & incs - if *incs is NULL ignored (but space is always same for both)  */
pdl_error pdl_setdims(pdl* it, PDL_Indx * dims, PDL_Indx ndims) {
  pdl_error PDL_err = {0, NULL, 0};
  PDLDEBUG_f(printf("pdl_setdims %p: ", it);pdl_print_iarr(dims, ndims);printf("\n"));
  PDL_Indx i, old_nvals = it->nvals, new_nvals = 1;
  for (i=0; i<ndims; i++) new_nvals *= dims[i];
  int what = (old_nvals == new_nvals) ? 0 : PDL_PARENTDATACHANGED;
  if ((it->state & PDL_NOMYDIMS) || ndims != it->ndims)
    what |= PDL_PARENTDIMSCHANGED;
  else
    for (i=0; i<ndims; i++)
      if (dims[i] != it->dims[i]) { what |= PDL_PARENTDIMSCHANGED; break; }
  if (!what) { PDLDEBUG_f(printf("pdl_setdims NOOP\n")); return PDL_err; }
  PDL_RETERROR(PDL_err, pdl_reallocdims(it,ndims));
  for(i=0; i<ndims; i++) it->dims[i] = dims[i];
  pdl_resize_defaultincs(it);
  PDL_RETERROR(PDL_err, pdl_reallocbroadcastids(it,1));
  it->broadcastids[0] = ndims;
  it->state &= ~PDL_NOMYDIMS;
  CHANGED(it,what,0);
  return PDL_err;
}

/* This is *not* careful! */
pdl_error pdl_setdims_careful(pdl *it)
{
	pdl_error PDL_err = {0, NULL, 0};
	pdl_resize_defaultincs(it);
        PDL_err = pdl_reallocbroadcastids(it,1); /* XXX For now */
	return PDL_err;
}

PDL_Anyval pdl_get_offs(pdl *it, PDL_Indx offs) {
	PDL_Indx dummy1=offs+1; PDL_Indx dummy2=1;
	return pdl_at(it->data, it->datatype, &offs, &dummy1, &dummy2, 0, 1);
}

pdl_error pdl__addchildtrans(pdl *it,pdl_trans *trans)
{
	pdl_error PDL_err = {0, NULL, 0};
	PDLDEBUG_f(printf("pdl__addchildtrans add to %p trans=%s\n", it, trans->vtable?trans->vtable->name:""));
	int i; pdl_trans_children *c = &it->trans_children;
	do {
	    if (c->next) { c=c->next; continue; } else {
		for(i=0; i<PDL_NCHILDREN; i++)
		    if(! c->trans[i]) {
			c->trans[i] = trans; return PDL_err;
		    }
		break;
	    }
	} while(1);
	c = c->next = malloc(sizeof(pdl_trans_children));
	if (!c) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
	c->trans[0] = trans;
	for(i=1; i<PDL_NCHILDREN; i++)
		c->trans[i] = 0;
	c->next = 0;
	return PDL_err;
}

pdl_error pdl_make_physdims(pdl *it) {
	pdl_error PDL_err = {0, NULL, 0};
	if (!it) return pdl_make_error_simple(PDL_EFATAL, "make_physdims called with NULL");
	PDL_Indx i;
	int c = (it->state & PDL_PARENTDIMSCHANGED);
	PDLDEBUG_f(printf("make_physdims %p (dimschanged=%X)\n",(void*)it, c));
        PDL_CHKMAGIC(it);
	if(!c) {
	  PDLDEBUG_f(printf("make_physdims exit (NOP) %p\n",(void*)it));
	  return PDL_err;
	}
	PDLDEBUG_f(printf("make_physdims turning off dimschanged, before="); pdl_dump_flags_fixspace(it->state, 0, PDL_FLAGS_PDL));
	it->state &= ~PDL_PARENTDIMSCHANGED;
	pdl_trans *trans = it->trans_parent;
	PDLDEBUG_f(printf("make_physdims %p TRANS:\n",it);
	    pdl_dump_trans_fixspace(trans,3));
	for(i=0; i<trans->vtable->nparents; i++) {
		PDL_RETERROR(PDL_err, pdl_make_physdims(trans->pdls[i]));
	}
	/* doesn't this mean that all children of this trans have
	   now their dims set and accordingly all those flags should
	   be reset? Otherwise redodims will be called for them again? */
	PDLDEBUG_f(printf("make_physdims: calling redodims %p on %p\n",
			  trans,it));
	REDODIMS(PDL_RETERROR, trans);
	/* why this one? will the old allocated data be freed correctly? */
	if((c & PDL_PARENTDIMSCHANGED) && (it->state & PDL_ALLOCATED)) {
		PDLDEBUG_f(printf("make_physdims turning off allocated, before="); pdl_dump_flags_fixspace(it->state, 0, PDL_FLAGS_PDL));
		it->state &= ~PDL_ALLOCATED;
	}
	PDLDEBUG_f(printf("make_physdims exit %p\n",(void*)it));
	return PDL_err;
}

static inline pdl_error pdl_trans_flow_null_checks(pdl_trans *trans, int *ret) {
  pdl_error PDL_err = {0, NULL, 0};
  int pfflag=0;
  PDL_Indx i;
  pdl_transvtable *vtable = trans->vtable;
/* Then, set our children. This is: */
/* First, determine whether any of our children already have
 * a parent, and whether they need to be updated. If this is
 * the case, we need to do some thinking. */
  for(i=0; i<vtable->nparents; i++) {
    int state = trans->pdls[i]->state;
    if (state & PDL_NOMYDIMS)
      return pdl_make_error(PDL_EUSERERROR,
	"Error in %s: input parameter '%s' is null",
	vtable->name, vtable->par_names[i]
      );
    if(state & PDL_DATAFLOW_ANY) pfflag++;
  }
  for(; i<vtable->npdls; i++) {
/* If children are flowing, croak. It's too difficult to handle properly */
    if(trans->pdls[i]->state & PDL_DATAFLOW_ANY)
	return pdl_make_error_simple(PDL_EUSERERROR, "Sorry, cannot flowing families right now\n");
/* Same, if children have trans yet parents are flowing */
    if(trans->pdls[i]->trans_parent && pfflag)
	return pdl_make_error_simple(PDL_EUSERERROR, "Sorry, cannot flowing families right now (2)\n");
  }
  *ret = pfflag;
  return PDL_err;
}

/* Called with a filled pdl_trans struct.
 * Sets the parent and trans fields of the ndarrays correctly,
 * creating families and the like if necessary.
 * Alternatively may just execute transformation
 * that would require families but is not dataflowed.
 */
pdl_error pdl_make_trans_mutual(pdl_trans *trans)
{
  pdl_error PDL_err = {0, NULL, 0};
  PDLDEBUG_f(printf("make_trans_mutual %p\n",(void*)trans);pdl_dump_trans_fixspace(trans,3));
  pdl_transvtable *vtable = trans->vtable;
  pdl **pdls = trans->pdls;
  PDL_Indx i, npdls=vtable->npdls, nparents=vtable->nparents;
  PDL_Indx nchildren = npdls - nparents;
  /* copy the converted outputs from the end-area to use as actual
    outputs - cf type_coerce */
  for (i=vtable->nparents; i<vtable->npdls; i++) pdls[i] = pdls[i+nchildren];
  PDL_TR_CHKMAGIC(trans);
  int pfflag=0;
  PDL_err = pdl_trans_flow_null_checks(trans, &pfflag);
  if (PDL_err.error) {
    PDL_ACCUMERROR(PDL_err, pdl_trans_finaldestroy(trans));
    return PDL_err;
  }
  char dataflow = !!(pfflag || (trans->flags & PDL_ITRANS_DO_DATAFLOW_ANY));
  PDLDEBUG_f(printf("make_trans_mutual dataflow=%d\n", (int)dataflow));
  for(i=0; i<nparents; i++) {
    pdl *parent = pdls[i];
    PDL_RETERROR(PDL_err, pdl__addchildtrans(parent,trans));
    if (parent->state & PDL_DATAFLOW_F) trans->flags |= PDL_ITRANS_DO_DATAFLOW_F;
  }
  int wd[npdls];
  for(i=nparents; i<npdls; i++) {
	pdl *child = pdls[i];
	char isnull = !!(child->state & PDL_NOMYDIMS);
	wd[i]=(isnull ? PDL_PARENTDIMSCHANGED : PDL_PARENTDATACHANGED);
	PDLDEBUG_f(printf("make_trans_mutual wd[%"IND_FLAG"]=", i); pdl_dump_flags_fixspace(wd[i], 0, PDL_FLAGS_PDL));
	if (dataflow) {
		/* This is because for "+=" (a = a + b) we must check for
		   previous parent transformations and mutate if they exist
		   if no dataflow. */
		PDLDEBUG_f(printf("make_trans_mutual turning on allchanged, before="); pdl_dump_flags_fixspace(child->state, 0, PDL_FLAGS_PDL));
		child->state |= PDL_PARENTDIMSCHANGED | PDL_PARENTDATACHANGED;
	}
	if (dataflow || isnull) child->trans_parent = trans;
	if (isnull)
	    child->state = (child->state & ~PDL_NOMYDIMS) | PDL_MYDIMS_TRANS;
  }
  if (!dataflow)
	PDL_ACCUMERROR(PDL_err, pdl_destroytransform(trans,1,wd,0));
  PDLDEBUG_f(printf("make_trans_mutual exit %p\n",(void*)trans));
  return PDL_err;
} /* pdl_make_trans_mutual() */

pdl_error pdl_redodims_default(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  PDLDEBUG_f(printf("pdl_redodims_default ");pdl_dump_trans_fixspace(trans,0));
  pdl_transvtable *vtable = trans->vtable;
  PDL_Indx creating[vtable->npdls];
  pdl **pdls = trans->pdls;
  PDL_Indx i;
  for (i=0; i<vtable->npdls; i++) {
    short flags = vtable->par_flags[i];
    creating[i] = (flags & PDL_PARAM_ISCREAT) &&
      PDL_DIMS_FROM_TRANS(trans,pdls[i]);
  }
  if (vtable->flags & PDL_TRANS_DO_BROADCAST)
    PDL_RETERROR(PDL_err, pdl_initbroadcaststruct(2, pdls,
      vtable->par_realdims, creating, vtable->npdls, vtable,
      &trans->broadcast, trans->ind_sizes, trans->inc_sizes,
      vtable->per_pdl_flags, vtable->flags & PDL_TRANS_NO_PARALLEL));
  pdl_hdr_childcopy(trans);
  trans->dims_redone = 1;
  return PDL_err;
}

pdl_error pdl__make_physical_recprotect(pdl *it, int recurse_count) {
	pdl_error PDL_err = {0, NULL, 0};
	int i, vaffinepar=0;
	if(recurse_count > 1000)
	  return pdl_make_error_simple(PDL_EUSERERROR, "PDL:Internal Error: data structure recursion limit exceeded (max 1000 levels)\n\tThis could mean that you have found an infinite-recursion error in PDL, or\n\tthat you are building data structures with very long dataflow dependency\n\tchains.  You may want to try using sever() to break the dependency.\n");
	PDLDEBUG_f(printf("make_physical %p\n",(void*)it));
        PDL_CHKMAGIC(it);
	if(!(it->state & PDL_ANYCHANGED))  {
		if (!(it->state & PDL_ALLOCATED)) PDL_RETERROR(PDL_err, pdl_allocdata(it));
		goto mkphys_end;
	}
	if(!it->trans_parent) {
		return pdl_make_error_simple(PDL_EFATAL, "PDL Not physical but doesn't have parent");
	}
	if((it->trans_parent->flags & PDL_ITRANS_ISAFFINE) && !PDL_VAFFOK(it))
		PDL_RETERROR(PDL_err, pdl__make_physvaffine_recprotect(it, recurse_count+1));
	if(PDL_VAFFOK(it)) {
		PDLDEBUG_f(printf("make_physical: VAFFOK\n"));
		PDL_RETERROR(PDL_err, pdl_readdata_vaffine(it));
		PDLDEBUG_f(printf("make_physical turning off anychanged, before="); pdl_dump_flags_fixspace(it->state, 0, PDL_FLAGS_PDL));
		it->state &= (~PDL_ANYCHANGED);
		goto mkphys_end;
	}
	PDL_TR_CHKMAGIC(it->trans_parent);
	for(i=0; i<it->trans_parent->vtable->nparents; i++) {
		if(VAFFINE_FLAG_OK(it->trans_parent->vtable->per_pdl_flags,i)) {
			PDL_RETERROR(PDL_err, pdl__make_physvaffine_recprotect(it->trans_parent->pdls[i], recurse_count+1));
                        /* check if any of the parents is a vaffine */
                        vaffinepar = vaffinepar || (it->trans_parent->pdls[i]->data != PDL_REPRP(it->trans_parent->pdls[i]));
                }  else
			PDL_RETERROR(PDL_err, pdl__make_physical_recprotect(it->trans_parent->pdls[i], recurse_count+1));
	}
        /* XXX The real question is: why do we need another call to
         * redodims if !(it->state & PDL_ALLOCATED)??????
         */
	PDLDEBUG_f(printf("make_physical vaffinepar=%d, state=", vaffinepar); pdl_dump_flags_fixspace(it->state, 0, PDL_FLAGS_PDL));
	if((!(it->state & PDL_ALLOCATED) && vaffinepar) ||
	   it->state & PDL_PARENTDIMSCHANGED)
		REDODIMS(PDL_RETERROR, it->trans_parent);
	if(!(it->state & PDL_ALLOCATED)) {
		PDL_RETERROR(PDL_err, pdl_allocdata(it));
	}
	READDATA(it->trans_parent);
	PDLDEBUG_f(printf("make_physical turning off anychanged and OPTs, before="); pdl_dump_flags_fixspace(it->state, 0, PDL_FLAGS_PDL));
	it->state &= ~(PDL_ANYCHANGED | PDL_OPT_ANY_OK);
  mkphys_end:
	PDLDEBUG_f(printf("make_physical exiting: "); pdl_dump(it));
	return PDL_err;
}

pdl_error pdl_make_physical(pdl *it) {
  return pdl__make_physical_recprotect(it, 0);
}

pdl_error pdl_changed(pdl *it, int what, int recursing)
{
    pdl_error PDL_err = {0, NULL, 0};
    int i; int j;
    PDLDEBUG_f(
      printf("pdl_changed: entry for pdl %p recursing: %d, what=",
	     (void*)it,recursing);
      pdl_dump_flags_fixspace(what,0,PDL_FLAGS_PDL);
      if (it->state & PDL_TRACEDEBUG) pdl_dump(it);
    );
    if(recursing) {
	PDLDEBUG_f(printf("pdl_changed: adding what to state, currently="); pdl_dump_flags_fixspace(it->state,0,PDL_FLAGS_PDL));
	it->state |= what;
	if(pdl__ismagic(it)) pdl__call_magic(it,PDL_MAGIC_MARKCHANGED);
    }
    if(it->trans_parent && !recursing && (it->trans_parent->flags & PDL_ITRANS_DO_DATAFLOW_B)) {
	pdl_trans *trans = it->trans_parent;
	if((trans->flags & PDL_ITRANS_ISAFFINE) && (PDL_VAFFOK(it))) {
	    PDLDEBUG_f(printf("pdl_changed: calling writebackdata_vaffine (pdl %p)\n",(void*)it));
	    PDL_ACCUMERROR(PDL_err, pdl_writebackdata_vaffine(it));
	    CHANGED(it->vafftrans->from,what,0);
	} else {
	    PDLDEBUG_f(printf("pdl_changed: calling writebackdata from vtable, triggered by pdl %p, using trans %p\n",(void*)it,(void*)(trans)));
	    WRITEDATA(trans);
	    for(i=0; i<trans->vtable->nparents; i++) {
		pdl *pdl = trans->pdls[i];
		CHANGED(
		    (VAFFINE_FLAG_OK(trans->vtable->per_pdl_flags,i) &&
		       pdl->trans_parent &&
		       (pdl->trans_parent->flags & PDL_ITRANS_ISAFFINE) &&
		       PDL_VAFFOK(pdl))
		    ? pdl->vafftrans->from
		    : pdl,
		    what,0);
	    }
	}
    } else {
	PDL_DECL_CHILDLOOP(it);
	PDL_START_CHILDLOOP(it)
	    pdl_trans *trans = PDL_CHILDLOOP_THISCHILD(it);
	    for(j=trans->vtable->nparents; j<trans->vtable->npdls; j++)
		if (trans->pdls[j] != it && (trans->pdls[j]->state & what) != what)
		    CHANGED(trans->pdls[j],what,1);
	PDL_END_CHILDLOOP(it)
    }
    PDLDEBUG_f(printf("pdl_changed: exiting for pdl %p\n",(void*)it));
    return PDL_err;
}

/* Current assumptions: only
 * "slice" and "diagonal"-type things supported.
 *
 * We need to do careful testing for clump-type things.
 */

/* pdl_make_physvaffine can be called on *any* pdl -- vaffine or not --
   it will call make_physical as needed on those
   this function is the right one to call in any case if you want to
   make only those physical (i.e. allocating their own data, etc) which
   have to be and leave those vaffine with updated dims, etc, that do
   have an appropriate transformation of which they are a child.
   should probably have been called make_physcareful to point out what
   it really does
*/
pdl_error pdl__make_physvaffine_recprotect(pdl *it, int recurse_count)
{
	pdl_error PDL_err = {0, NULL, 0};
	pdl_trans *t;
	pdl *parent;
	pdl *current;
	PDL_Indx i,j;
	PDL_Indx inc;
	PDL_Indx newinc;
	PDL_Indx ninced;
	int flag;
	int incsign;
	PDLDEBUG_f(printf("make_physvaffine %p\n",(void*)it));
	PDL_RETERROR(PDL_err, pdl_make_physdims(it));
	PDL_Indx incsleft[it->ndims];
	if(!it->trans_parent || !(it->trans_parent->flags & PDL_ITRANS_ISAFFINE)) {
		PDL_RETERROR(PDL_err, pdl__make_physical_recprotect(it, recurse_count+1));
		goto mkphys_vaff_end;
	}
	if (!it->vafftrans || it->vafftrans->ndims < it->ndims)
	  PDL_RETERROR(PDL_err, pdl_vafftrans_alloc(it));
        for(i=0; i<it->ndims; i++) {
		it->vafftrans->incs[i] = it->dimincs[i];
	}
	flag=0;
	it->vafftrans->offs = 0;
	t=it->trans_parent;
	current = it;
	while(t && (t->flags & PDL_ITRANS_ISAFFINE)) {
		PDL_Indx cur_offset = 0;
		if (!t->incs)
		  return pdl_make_error_simple(PDL_EUSERERROR, "pdl_make_physvaffine: affine trans has NULL incs\n");
		parent = t->pdls[0];
		/* For all dimensions of the childest ndarray */
		for(i=0; i<it->ndims; i++) {
			PDL_Indx offset_left = it->vafftrans->offs;
			/* inc = the increment at the current stage */
			inc = it->vafftrans->incs[i];
			incsign = (inc >= 0 ? 1:-1);
			inc *= incsign;
			newinc = 0;
			/* For all dimensions of the current ndarray */
			for(j=current->ndims-1; j>=0 && current->dimincs[j] != 0; j--) {
				cur_offset = offset_left / current->dimincs[j];
				offset_left -= cur_offset * current->dimincs[j];
				if(incsign < 0) {
					cur_offset = (current->dims[j]-1) - cur_offset;
				}
				/* If the absolute value > this so */
				/* we have a contribution from this dimension */
				if(inc >= current->dimincs[j]) {
					/* We used this many of this dim */
					ninced = inc / current->dimincs[j];
					if(cur_offset + it->dims[i]*ninced >
						current->dims[j]) {
					  PDL_Indx foo =
					   (cur_offset + it->dims[i]*ninced)*
					   current->dimincs[j];
					  PDL_Indx k;
					  for(k=j+1; k<current->ndims; k++) {
						foo -= current->dimincs[k-1] *
							current->dims[k-1];
						if(foo<=0) break;
						if(t->incs[k] !=
						   t->incs[k-1] *
						   current->dims[k-1]) {
						   /* XXXXX */
							flag=1;
						 /*
	warn("Illegal vaffine; fix loop to break: %d %d %d k=%d s=%d, (%d+%d*%d>%d) %d %d %d %d.\n",t,current,it,
		k,incsign,cur_offset,it->dims[i],ninced,current->dims[j],current->dimincs[j],
		t->incs[k],t->incs[k-1],current->dims[k-1]);
						*/
						}
					  }
					}
					newinc += t->incs[j]*ninced;
					inc %= current->dimincs[j];
				}
			}
			incsleft[i] = incsign*newinc;
		}
		if(flag) break;
		for(i=0; i<it->ndims; i++) {
			it->vafftrans->incs[i] = incsleft[i];
		}
		{
			PDL_Indx offset_left = it->vafftrans->offs;
			inc = it->vafftrans->offs;
			newinc = 0;
			for(j=current->ndims-1; j>=0 && current->dimincs[j] != 0; j--) {
				cur_offset = offset_left / current->dimincs[j];
				offset_left -= cur_offset * current->dimincs[j];
				newinc += t->incs[j]*cur_offset;
			}
			it->vafftrans->offs = newinc;
			it->vafftrans->offs += t->offs;
		}
		t = parent->trans_parent;
		current = parent;
	}
	it->vafftrans->from = current;
	it->state |= PDL_OPT_VAFFTRANSOK;
	PDL_RETERROR(PDL_err, pdl__make_physical_recprotect(current, recurse_count+1));
  mkphys_vaff_end:
	PDLDEBUG_f(printf("make_physvaffine exit %p\n",(void*)it));
	return PDL_err;
}

pdl_error pdl_make_physvaffine(pdl *it)
{
  return pdl__make_physvaffine_recprotect(it, 0);
}

pdl_error pdl_set_datatype(pdl *a, int datatype)
{
    pdl_error PDL_err = {0, NULL, 0};
    PDL_RETERROR(PDL_err, pdl_make_physical(a));
    if(a->trans_parent)
	PDL_RETERROR(PDL_err, pdl_destroytransform(a->trans_parent,1,NULL,0));
    if (a->state & PDL_NOMYDIMS)
	a->datatype = datatype;
    else
	PDL_RETERROR(PDL_err, pdl_converttype( a, datatype ));
    return PDL_err;
}

pdl_error pdl_sever(pdl *src)
{
    pdl_error PDL_err = {0, NULL, 0};
    if (!src->trans_parent) return PDL_err;
    PDL_RETERROR(PDL_err, pdl_make_physvaffine(src));
    PDL_RETERROR(PDL_err, pdl_destroytransform(src->trans_parent,1,NULL,0));
    return PDL_err;
}

#define PDL_MAYBE_PROPAGATE_BADFLAG(t, newval) \
  for( i = 0; i < (t)->vtable->npdls; i++ ) { \
    pdl *tpdl = (t)->pdls[i]; \
    /* make sure we propagate if changed */ \
    if (!!newval != !!(tpdl->state & PDL_BADVAL)) \
      pdl_propagate_badflag( tpdl, newval ); \
  }

/* newval = 1 means set flag, 0 means clear it */
void pdl_propagate_badflag( pdl *it, int newval ) {
    PDLDEBUG_f(printf("pdl_propagate_badflag pdl=%p newval=%d\n", it, newval));
    PDL_Indx i;
    if (newval)
	it->state |=  PDL_BADVAL;
    else
	it->state &= ~PDL_BADVAL;
    if (it->trans_parent)
	PDL_MAYBE_PROPAGATE_BADFLAG(it->trans_parent, newval)
    PDL_DECL_CHILDLOOP(it)
    PDL_START_CHILDLOOP(it)
	pdl_trans *trans = PDL_CHILDLOOP_THISCHILD(it);
	trans->bvalflag = !!newval;
	PDL_MAYBE_PROPAGATE_BADFLAG(trans, newval)
    PDL_END_CHILDLOOP(it)
}

void pdl_propagate_badvalue( pdl *it ) {
    PDL_DECL_CHILDLOOP(it)
    PDL_START_CHILDLOOP(it)
	pdl_trans *trans = PDL_CHILDLOOP_THISCHILD(it);
	PDL_Indx i;
	for( i = trans->vtable->nparents; i < trans->vtable->npdls; i++ ) {
	    pdl *child = trans->pdls[i];
            child->has_badvalue = 1;
            child->badvalue = it->badvalue;
	    /* make sure we propagate to grandchildren, etc */
	    pdl_propagate_badvalue( child );
        } /* for: i */
    PDL_END_CHILDLOOP(it)
} /* pdl_propagate_badvalue */

PDL_Anyval pdl_get_badvalue( int datatype ) {
    PDL_Anyval retval = { PDL_INVALID, {0} };
#define X(datatype, ctype, ppsym, shortctype, ...) \
    retval.type = datatype; retval.value.ppsym = PDL.bvals.shortctype;
    PDL_GENERICSWITCH(PDL_TYPELIST2_ALL, datatype, X, return retval)
#undef X
    return retval;
}

PDL_Anyval pdl_get_pdl_badvalue( pdl *it ) {
    return it->has_badvalue ? it->badvalue : pdl_get_badvalue( it->datatype );
}

pdl_trans *pdl_create_trans(pdl_transvtable *vtable) {
    size_t it_sz = sizeof(pdl_trans)+sizeof(pdl *)*(
      vtable->npdls + (vtable->npdls - vtable->nparents) /* outputs twice */
    );
    pdl_trans *it = malloc(it_sz);
    if (!it) return it;
    memset(it, 0, it_sz);
    PDL_TR_SETMAGIC(it);
    if (vtable->structsize) {
      it->params = malloc(vtable->structsize);
      if (!it->params) return NULL;
      memset(it->params, 0, vtable->structsize);
    }
    it->flags = vtable->iflags;
    it->dims_redone = 0;
    it->bvalflag = 0;
    it->vtable = vtable;
    PDL_CLRMAGIC(&it->broadcast);
    it->broadcast.inds = 0;
    it->broadcast.gflags = 0;
    it->ind_sizes = (PDL_Indx *)malloc(sizeof(PDL_Indx) * vtable->ninds);
    if (!it->ind_sizes) return NULL;
    int i; for (i=0; i<vtable->ninds; i++) it->ind_sizes[i] = -1;
    it->inc_sizes = (PDL_Indx *)malloc(sizeof(PDL_Indx) * vtable->nind_ids);
    if (!it->inc_sizes) return NULL;
    for (i=0; i<vtable->nind_ids; i++) it->inc_sizes[i] = -1;
    it->offs = -1;
    it->incs = NULL;
    it->__datatype = PDL_INVALID;
    return it;
}

pdl_error pdl_type_coerce(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  PDL_Indx i;
  pdl_transvtable *vtable = trans->vtable;
  pdl **pdls = trans->pdls;
  trans->__datatype = -1;
  char p2child_has_badvalue = (vtable->npdls == 2 && pdls[0]->has_badvalue
      && (vtable->par_flags[1] & PDL_PARAM_ISCREATEALWAYS));
  PDL_Anyval parent_badvalue = p2child_has_badvalue ? pdls[0]->badvalue : (PDL_Anyval){PDL_INVALID, {0}};
  PDL_Indx nchildren = vtable->npdls - vtable->nparents;
  /* copy the "real" (passed-in) outputs to the end-area to use as actual
    outputs, possibly after being converted, leaving the passed-in ones
    alone to be picked up for use in CopyBadStatusCode */
  for (i=vtable->nparents; i<vtable->npdls; i++) pdls[i+nchildren] = pdls[i];
  for (i=0; i<vtable->npdls; i++) {
    pdl *pdl = pdls[i];
    short flags = vtable->par_flags[i];
    if (flags & (PDL_PARAM_ISIGNORE|PDL_PARAM_ISTYPED|PDL_PARAM_ISCREATEALWAYS))
      continue;
    if (trans->__datatype < pdl->datatype && (
      !(flags & PDL_PARAM_ISCREAT) ||
      ((flags & PDL_PARAM_ISCREAT) && !((pdl->state & PDL_NOMYDIMS) && pdl->trans_parent == NULL))
    ))
      trans->__datatype = pdl->datatype;
  }
  int type_match = 0, last_dtype = -1;
  for (i=0;vtable->gentypes[i]!=-1; i++) {
    last_dtype = vtable->gentypes[i];
    if (trans->__datatype != last_dtype) continue;
    type_match = 1;
    break;
  }
  if (!type_match) trans->__datatype = last_dtype;
  pdl_datatypes trans_dtype = trans->__datatype;
  for (i=0; i<vtable->npdls; i++) {
    pdl *pdl = pdls[i];
    short flags = vtable->par_flags[i];
    pdl_datatypes new_dtype = trans_dtype;
    if (flags & PDL_PARAM_ISIGNORE) continue;
    if (flags & PDL_PARAM_ISTYPED) {
      new_dtype = vtable->par_types[i];
      if (flags & PDL_PARAM_ISTPLUS) new_dtype = PDLMAX(new_dtype, trans_dtype);
    } else if (flags & PDL_PARAM_ISREAL) {
      if (trans_dtype >= PDL_CF) new_dtype = trans_dtype - (PDL_CF - PDL_F);
    } else if (flags & PDL_PARAM_ISCOMPLEX) {
      if (trans_dtype < PDL_CF) new_dtype = PDLMAX(PDL_CF, trans_dtype + (PDL_CF - PDL_F));
    }
    if ((pdl->state & PDL_NOMYDIMS) && (!pdl->trans_parent || pdl->trans_parent == trans)) {
      pdl->badvalue = parent_badvalue;
      pdl->has_badvalue = p2child_has_badvalue;
      pdl->datatype = new_dtype;
    } else if (new_dtype != pdl->datatype) {
      PDLDEBUG_f(printf("pdl_type_coerce (%s) pdl=%"IND_FLAG" from %d to %d\n", vtable->name, i, pdl->datatype, new_dtype));
      pdl = pdl_get_convertedpdl(pdl, new_dtype);
      if (!pdl)
        return pdl_make_error(PDL_EFATAL, "%s got NULL pointer from get_convertedpdl on param %s", vtable->name, vtable->par_names[i]);
      if (pdl->datatype != new_dtype)
        return pdl_make_error_simple(PDL_EFATAL, "type not expected value after get_convertedpdl\n");
      /* if type-convert output, put in end-area */
      pdls[i + (i >= vtable->nparents ? nchildren : 0)] = pdl;
    }
  }
  return PDL_err;
}

char pdl_trans_badflag_from_inputs(pdl_trans *trans) {
  PDL_Indx i;
  pdl_transvtable *vtable = trans->vtable;
  pdl **pdls = trans->pdls;
  char retval = 0;
  for (i=0; i<vtable->npdls; i++) {
    pdl *pdl = pdls[i];
    if ((vtable->par_flags[i] & (PDL_PARAM_ISOUT|PDL_PARAM_ISTEMP)) ||
        !(pdl->state & PDL_BADVAL)) continue;
    trans->bvalflag = retval = 1;
    break;
  }
  if (retval && (vtable->flags & PDL_TRANS_BADIGNORE)) {
    printf("WARNING: %s does not handle bad values.\n", vtable->name);
    trans->bvalflag = 0; /* but still return true */
  }
  return retval;
}

pdl_error pdl_trans_check_pdls(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  PDL_Indx i;
  pdl_transvtable *vtable = trans->vtable;
  pdl **pdls = trans->pdls;
  for (i=0; i<vtable->npdls; i++) {
    if (vtable->par_flags[i] & PDL_PARAM_ISTEMP)
      if (!(pdls[i] = pdl_pdlnew()))
	return pdl_make_error_simple(PDL_EFATAL, "Error in pdlnew");
    if (!pdls[i])
      return pdl_make_error(PDL_EFATAL, "%s got NULL pointer on param %s", vtable->name, vtable->par_names[i]);
  }
  if (vtable->flags & PDL_TRANS_OUTPUT_OTHERPAR)
    for (i = 0; i < vtable->npdls; i++)
      if (!(trans->pdls[i]->state & PDL_NOMYDIMS) && trans->pdls[i]->ndims > vtable->par_realdims[i])
        return pdl_make_error(PDL_EUSERERROR,
          "Can't broadcast with output OtherPars but par '%s' has %"IND_FLAG" dims, > %"IND_FLAG"!",
          vtable->par_names[i], trans->pdls[i]->ndims, vtable->par_realdims[i]
        );
  return PDL_err;
}
