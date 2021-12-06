/* pdlapi.c - functions for manipulating pdl structs  */

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#define VTABLE_OR_DEFAULT(trans, func, default_func) \
  ((trans)->vtable->func \
    ? (trans)->vtable->func \
    : pdl_ ## default_func)(trans)

#define REDODIMS(trans) VTABLE_OR_DEFAULT(trans, redodims, redodims_default)
#define READDATA(trans) VTABLE_OR_DEFAULT(trans, readdata, readdata_affine)
#define WRITEDATA(trans) VTABLE_OR_DEFAULT(trans, writebackdata, writebackdata_affine)

extern Core PDL;

void pdl__ensure_trans(pdl_trans *trans,int what) ;

static int is_parent_of(pdl *it,pdl_trans *trans) {
	int i;
	for(i=0; i<trans->vtable->nparents; i++) {
		if(trans->pdls[i] == it)  return 1;
	}
	return 0;
}

pdl *pdl_null() {
	PDL_Indx d[1] = {0};
	pdl *it = pdl_pdlnew();
	if (!it) return it;
	PDL_Anyval zero = { PDL_B, {0} };
	pdl_makescratchhash(it, zero);
	pdl_setdims(it,d,1);
	it->state |= PDL_NOMYDIMS;
	return it;
}

pdl *pdl_get_convertedpdl(pdl *old,int type) {
	if(old->datatype == type) return old;
	pdl *it = pdl_null();
	if (!it) return it;
	pdl_converttypei_new(old,it,type);
	return it;
}

void pdl_grow(pdl* a, PDL_Indx newsize) {
   SV* foo;
   STRLEN nbytes;
   STRLEN ncurr;
   STRLEN len;
   nbytes = ((STRLEN) newsize) * pdl_howbig(a->datatype);
   ncurr  = a->datasv ? SvCUR((SV *)a->datasv) : 0;
   if (ncurr == nbytes)
      return;    /* Nothing to be done */
   if(a->state & PDL_DONTTOUCHDATA) {
      die("Trying to touch data of an untouchable (mmapped?) pdl");
   }
   if(a->datasv == NULL)
      a->datasv = newSVpv("",0);
   foo = a->datasv;
   if(nbytes > (1024*1024*1024)) {
     SV *sv = get_sv("PDL::BIGPDL",0);
     if(sv == NULL || !(SvTRUE(sv)))
       die("Probably false alloc of over 1Gb PDL! (set $PDL::BIGPDL = 1 to enable)");
     fflush(stdout);
   }
   (void)SvGROW ( foo, nbytes );
   SvCUR_set( foo, nbytes );
   a->data = (void *) SvPV( foo, len ); a->nvals = newsize;
   if (nbytes > ncurr) memset(a->data + ncurr, 0, nbytes - ncurr);
}

void pdl_allocdata(pdl *it) {
	int i;
	PDL_Indx nvals=1;
	for(i=0; i<it->ndims; i++) {
			nvals *= it->dims[i];
	}
	it->nvals = nvals;
	PDLDEBUG_f(printf("pdl_allocdata %p, %"IND_FLAG", %d\n",(void*)it, it->nvals,
		it->datatype));
	pdl_grow(it,nvals);
	PDLDEBUG_f(pdl_dump(it));
	it->state |= PDL_ALLOCATED;
}

pdl* pdl_pdlnew() {
     int i;
     pdl* it;
     it = (pdl*) malloc(sizeof(pdl));
     if (!it) return it;
     memset(it, 0, sizeof(pdl));
     it->magicno = PDL_MAGICNO;
     it->state = 0;
     it->datatype = 0;
     it->trans_parent = NULL;
     it->vafftrans = NULL;
     it->sv = NULL;
     it->datasv = 0;
     it->data = 0;
     it->has_badvalue = 0;
     it->dims = it->def_dims;
     it->dimincs = it->def_dimincs;
     it->ndims = 0;
     it->nthreadids = 1;
     it->threadids = it->def_threadids;
     it->threadids[0] = 0;
     for(i=0; i<PDL_NCHILDREN; i++) {it->children.trans_parent[i]=NULL;}
     it->children.next = NULL;
     it->magic = 0;
     it->hdrsv = 0;
     PDLDEBUG_f(printf("CREATE %p (size=%zu)\n",(void*)it,sizeof(pdl)));
     return it;
}

/* Explicit free. Do not use, use destroy instead, which causes this
   to be called when the time is right */
void pdl__free(pdl *it) {
    pdl_children *p1,*p2;
    PDL_CHKMAGIC(it);

    /* now check if magic is still there */
    if (pdl__ismagic(it)) {
      PDLDEBUG_f(printf("%p is still magic\n",(void*)it));
      PDLDEBUG_f(pdl__print_magic(it));
    }

    it->magicno = 0x42424245;
    PDLDEBUG_f(printf("FREE %p\n",(void*)it));
    if(it->dims       != it->def_dims)       free((void*)it->dims);
    if(it->dimincs    != it->def_dimincs)    free((void*)it->dimincs);
    if(it->threadids  != it->def_threadids)  free((void*)it->threadids);

    if(it->vafftrans) {
	pdl_vafftrans_free(it);
    }

    p1 = it->children.next;
    while(p1) {
	p2 = p1->next;
	free(p1);
	p1 = p2;
    }

/* Call special freeing magic, if exists */
    if(PDL_ISMAGIC(it)) {
	pdl__call_magic(it, PDL_MAGIC_DELETEDATA);
	pdl__magic_free(it);
    }

    if(it->datasv) {
	    SvREFCNT_dec(it->datasv);
	    it->data=0;
    } else if(it->data) {
	    pdl_pdl_warn("Warning: special data without datasv is not freed currently!!");
    }
    if(it->hdrsv) {
	SvREFCNT_dec(it->hdrsv);
	it->hdrsv = 0;
    }
    free(it);
    PDLDEBUG_f(printf("ENDFREE %p\n",(void*)it));
}

/* Problem with this function: when transformation is destroyed,
 * there may be several different children with the same name.
 * Therefore, we cannot croak :(
 */
void pdl__removechildtrans(pdl *it,pdl_trans *trans, PDL_Indx nth,int all)
{
	PDL_Indx i; pdl_children *c; int flag = 0;
	if(all) {
		for(i=0; i<trans->vtable->nparents; i++)
			if(trans->pdls[i] == it)
				trans->pdls[i] = NULL;
	} else {
		trans->pdls[nth] = 0;
	}
	c = &it->children;
	do {
		for(i=0; i<PDL_NCHILDREN; i++) {
			if(c->trans_parent[i] == trans) {
				c->trans_parent[i] = NULL;
				flag = 1;
				if(!all) return;
				/* Can't return; might be many times
				  (e.g. $x+$x) */
			}
		}
		c=c->next;
	} while(c);
	/* this might be due to a croak when performing the trans; so
	   warn only for now, otherwise we leave trans undestructed ! */
	if(!flag)
		pdl_pdl_warn("Child not found for pdl %d, %d\n",it, trans);
}

void pdl__removeparenttrans(pdl *it, pdl_trans *trans, PDL_Indx nth)
{
	trans->pdls[nth] = 0;
	it->trans_parent = 0;
}

/* XXX Two next routines are memleaks */
/* somehow this transform will call (implicitly) redodims twice on
   an unvaffined pdl; leads to memleak if redodims allocates stuff
   that is only freed in later call to freefunc */
void pdl_destroytransform(pdl_trans *trans,int ensure)
{
	PDL_Indx j;
	pdl *pdl, *destbuffer[100];
	int ndest = 0;
	int ismutual = !(trans->flags & PDL_ITRANS_NONMUTUAL);
	PDLDEBUG_f(printf("entering pdl_destroytransform %p (ensure %d, ismutual %d)\n",
			  (void*)trans,ensure,ismutual));
	if(100 < trans->vtable->npdls) {
		die("Huge trans");
	}
	PDL_TR_CHKMAGIC(trans);
	if(!trans->vtable) {
	  die("ZERO VTABLE DESTTRAN 0x%p %d\n",trans,ensure);
	}
	if(ensure) {
		PDLDEBUG_f(printf("pdl_destroytransform: ensure\n"));
		pdl__ensure_trans(trans,ismutual ? 0 : PDL_PARENTDIMSCHANGED);
	}
	if (ismutual) {
	  for(j=0; j<trans->vtable->nparents; j++) {
	    pdl = trans->pdls[j];
	    if(!pdl) continue;
	    PDL_CHKMAGIC(pdl);
	    PDLDEBUG_f(printf("pdl_removectransform(%p): %p %"IND_FLAG"\n",
	      (void*)trans, (void*)(pdl), j));
	    pdl__removechildtrans(pdl,trans,j,1);
	    if(!(pdl->state & PDL_DESTROYING) && !pdl->sv) {
	      destbuffer[ndest++] = pdl;
	    }
	  }
	  for(; j<trans->vtable->npdls; j++) {
	    pdl = trans->pdls[j];
	    PDL_CHKMAGIC(pdl);
	    PDLDEBUG_f(printf("pdl_removeptransform(%p): %p %"IND_FLAG"\n",
	      (void*)trans, (void*)(pdl), j));
	    pdl__removeparenttrans(pdl,trans,j);
	    if(pdl->vafftrans) {
	      PDLDEBUG_f(printf("pdl_removevafft: %p\n", (void*)pdl));
	      pdl_vafftrans_remove(pdl);
	    }
	    if(!(pdl->state & PDL_DESTROYING) && !pdl->sv) {
	      destbuffer[ndest++] = pdl;
	    }
	  }
	} else {
	  PDL_TR_CHKMAGIC(trans);
	  for(j=trans->vtable->nparents; j<trans->vtable->npdls; j++) {
	    pdl = trans->pdls[j];
	    pdl->state &= ~PDL_NOMYDIMS;
	    if(pdl->trans_parent == trans)
	      pdl->trans_parent = 0;
	  }
	}
	PDL_TR_CHKMAGIC(trans);
	if(trans->vtable->freetrans) {
		PDLDEBUG_f(printf("call freetrans\n"));
		trans->vtable->freetrans(trans); /* Free malloced objects */
	}
	PDL_TR_CLRMAGIC(trans);
	if(trans->vtable->flags & PDL_TRANS_DO_THREAD)
	  pdl_freethreadloop(&trans->pdlthread);
	trans->vtable = 0; /* Make sure no-one uses this */
	PDLDEBUG_f(printf("call free\n"));
	if (trans->params) free(trans->params);
	free(trans->ind_sizes);
	free(trans->inc_sizes);
	free(trans);
	if (ismutual)
	  for(j=0; j<ndest; j++) {
		  pdl_destroy(destbuffer[j]);
	  }
	PDLDEBUG_f(printf("leaving pdl_destroytransform %p\n", (void*)trans));
}

void pdl__destroy_childtranses(pdl *it,int ensure) {
	PDL_DECL_CHILDLOOP(it);
	PDL_START_CHILDLOOP(it)
		pdl_destroytransform(PDL_CHILDLOOP_THISCHILD(it),ensure);
	PDL_END_CHILDLOOP(it)
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
      2. a child with no children 

  When an ndarray is destroyed, it must tell its children and/or
  parent.

*/
void pdl_destroy(pdl *it) {
    int nback=0,nback2=0,nforw=0,nundest=0,nundestp=0;
    int nafn=0;
    pdl_trans *curt;
    PDL_DECL_CHILDLOOP(it);
    PDL_CHKMAGIC(it);
    PDLDEBUG_f(printf("Destr. %p\n",(void*)it);)
    if(it->state & PDL_DESTROYING) {
        PDLDEBUG_f(printf("Already Destr. %p\n",(void*)it);)
	return;
    }
    it->state |= PDL_DESTROYING;
    /* Clear the sv field so that there will be no dangling ptrs */
    if(it->sv) {
	    sv_setiv(it->sv,0x4242);
	    it->sv = NULL;
    }

    /* 1. count the children that do flow */
    PDL_START_CHILDLOOP(it)
	curt = PDL_CHILDLOOP_THISCHILD(it);
	if(PDL_CHILDLOOP_THISCHILD(it)->flags & (PDL_ITRANS_DO_DATAFLOW_F|
						 PDL_ITRANS_DO_DATAFLOW_B))
		nforw ++;
	if(PDL_CHILDLOOP_THISCHILD(it)->flags & PDL_ITRANS_DO_DATAFLOW_B)
	{
		nback ++;
		/* Cases where more than two in relationship
		 * must always be soft-destroyed */
		if(curt->vtable->npdls > 2) nback2++;
	}

	if(PDL_CHILDLOOP_THISCHILD(it)->flags & PDL_ITRANS_ISAFFINE) {
		if(!(curt->pdls[1]->state & PDL_ALLOCATED)) {
			nafn ++;
		}
	}
    PDL_END_CHILDLOOP(it)

/* First case where we may not destroy */
    if(nback2 > 0) goto soft_destroy;
    if(nback > 1) goto soft_destroy;

/* Also not here */
    if(it->trans_parent && nforw) goto soft_destroy;

/* Also, we do not wish to destroy if the children would be larger
 * than the parent and are currently not allocated (e.g. lags).
 * Because this is too much work to check, we refrain from destroying
 * for now if there is an affine child that is not allocated
 */
    if(nafn) goto soft_destroy;
    if(pdl__magic_isundestroyable(it)) {
        PDLDEBUG_f(printf("Magic, not Destr. %p\n",(void*)it);)
	goto soft_destroy;
    }

    pdl__destroy_childtranses(it,1);

    if(it->trans_parent) {
      PDLDEBUG_f(printf("Destr_trans. %p %d\n",(void*)(it->trans_parent), it->trans_parent->flags);)
        /* Ensure only if there are other children! */
      pdl_destroytransform(it->trans_parent,(it->trans_parent->vtable->npdls
				      - it->trans_parent->vtable->nparents > 1));
    }

/* Here, this is a child but has no children */
    goto hard_destroy;


   hard_destroy:

   pdl__free(it);
   PDLDEBUG_f(printf("End destroy %p\n",(void*)it);)

   return;

  soft_destroy:
    PDLDEBUG_f(printf("May have dependencies, not destr. %p, nu(%d, %d), nba(%d, %d), nforw(%d), tra(%p), nafn(%d)\n",
				(void*)it, nundest, nundestp, nback, nback2, nforw, (void*)(it->trans_parent), nafn);)
    it->state &= ~PDL_DESTROYING;
}


/* Straight copy, no dataflow */
pdl *pdl_hard_copy(pdl *src) {
	pdl_make_physical(src); /* Wasteful XXX... should be lazier */
	int i;
	pdl *it = pdl_null();
	if (!it) return it;
	it->state = 0;
	PDLDEBUG_f(printf("pdl_hard_copy (%p): ", src));PDLDEBUG_f(pdl_dump(it);)
	it->datatype = src->datatype;
	pdl_setdims(it,src->dims,src->ndims);
	pdl_allocdata(it);
	if(src->state & PDL_NOMYDIMS)
		it->state |= PDL_NOMYDIMS;
	pdl_reallocthreadids(it,src->nthreadids);
	for(i=0; i<src->nthreadids; i++) {
		it->threadids[i] = src->threadids[i];
	}
	memcpy(it->data,src->data, pdl_howbig(it->datatype) * (size_t)it->nvals);
	return it;
}

/* Reallocate this PDL to have ndims dimensions. */
void pdl_reallocdims(pdl *it, PDL_Indx ndims) {
   if (it->ndims < ndims) {  /* Need to realloc for more */
      if(it->dims != it->def_dims) free(it->dims);
      if(it->dimincs != it->def_dimincs) free(it->dimincs);
      if (ndims>PDL_NDIMS) {  /* Need to malloc */
         it->dims = malloc(ndims*sizeof(*(it->dims)));
         it->dimincs = malloc(ndims*sizeof(*(it->dimincs)));
         if (!it->dims || !it->dimincs)
            croak("Out of Memory\n");
      }
      else {
         it->dims = it->def_dims;
         it->dimincs = it->def_dimincs;
      }
   }
   it->ndims = ndims;
}

/* Reallocate n threadids. Set the new extra ones to the end */
void pdl_reallocthreadids(pdl *it, PDL_Indx n) {
	PDL_Indx i;
	PDL_Indx *olds; PDL_Indx nold;
	if(n <= it->nthreadids) {
		it->nthreadids = n; it->threadids[n-1] = it->ndims; return;
	}
	nold = it->nthreadids; olds = it->threadids;
	if(n > PDL_NTHREADIDS) {
		it->threadids = malloc(sizeof(*(it->threadids))*n);
		if (!it->threadids) croak("Out of Memory\n");
	} else {
		it->threadids = it->def_threadids;
	}
	it->nthreadids = n;
	if(it->threadids != olds) {
		for(i=0; i<nold && i<n; i++)
			it->threadids[i] = olds[i];
	}
	if(olds != it->def_threadids) { free(olds); }
	for(i=nold; i<it->nthreadids; i++) {
		it->threadids[i] = it->ndims;
	}
}

/* Recalculate default increments and grow the PDL data */

void pdl_resize_defaultincs(pdl *it) {
	PDL_Indx inc = 1;
	unsigned int i=0;
	for(i=0; i<it->ndims; i++) {
		it->dimincs[i] = inc; inc *= it->dims[i];
	}
	it->nvals = inc;
        it->state &= ~PDL_ALLOCATED; /* Need to realloc when phys */
}

/* Init dims & incs - if *incs is NULL ignored (but space is always same for both)  */

void pdl_setdims(pdl* it, PDL_Indx * dims, PDL_Indx ndims) {
   PDL_Indx i;
   PDLDEBUG_f(printf("pdl_setdims: "));PDLDEBUG_f(pdl_dump(it);)
   /* This mask avoids all kinds of subtle dereferencing bugs (CED 11/2015) */
   if(it->trans_parent || it->vafftrans || it->children.next ) {
      pdl_pdl_barf("Can't setdims on a PDL that already has children");
   }
   /* not sure if this is still necessary with the mask above... (CED 11/2015)  */
   pdl_children_changesoon(it,PDL_PARENTDIMSCHANGED|PDL_PARENTDATACHANGED);
   pdl_reallocdims(it,ndims);
   for(i=0; i<ndims; i++) it->dims[i] = dims[i];
   pdl_resize_defaultincs(it);
   pdl_reallocthreadids(it,1);
   it->threadids[0] = ndims;
   it->state &= ~PDL_NOMYDIMS;
   pdl_changed(it,PDL_PARENTDIMSCHANGED|PDL_PARENTDATACHANGED,0);
}

/* This is *not* careful! */
void pdl_setdims_careful(pdl *it)
{
	pdl_resize_defaultincs(it);
        pdl_reallocthreadids(it,1); /* XXX For now */
}

PDL_Anyval pdl_get(pdl *it,PDL_Indx *inds) {
        PDL_Indx i;
        PDL_Indx offs=PDL_REPROFFS(it);
        PDL_Indx *incs=PDL_REPRINCS(it);
        for(i=0; i<it->ndims; i++)
                offs += incs[i] * inds[i];
        return pdl_get_offs(PDL_REPRP(it),offs);
}

PDL_Anyval pdl_get_offs(pdl *it, PDL_Indx offs) {
	PDL_Indx dummy1=offs+1; PDL_Indx dummy2=1;
	return pdl_at(it->data, it->datatype, &offs, &dummy1, &dummy2, 0, 1);
}

void pdl_put_offs(pdl *it, PDL_Indx offs, PDL_Anyval value) {
	PDL_Indx dummy1=offs+1; PDL_Indx dummy2=1;
	pdl_set(it->data, it->datatype, &offs, &dummy1, &dummy2, 0, 1, value);
}


void pdl__addchildtrans(pdl *it,pdl_trans *trans, PDL_Indx nth)
{
	int i; pdl_children *c;
	trans->pdls[nth] = it;
	c = &it->children;
	do {
		for(i=0; i<PDL_NCHILDREN; i++) {
			if(! c->trans_parent[i]) {
				c->trans_parent[i] = trans; return;
			}
		}
		if(!c->next) break;
		c=c->next;
	} while(1) ;
	c->next = malloc(sizeof(pdl_children));
	if (!c->next) croak("Out of Memory\n");
	c->next->trans_parent[0] = trans;
	for(i=1; i<PDL_NCHILDREN; i++)
		c->next->trans_parent[i] = 0;
	c->next->next = 0;
}

void pdl_make_physdims(pdl *it) {
	PDL_Indx i;
	int c = (it->state & (PDL_PARENTDIMSCHANGED | PDL_PARENTREPRCHANGED)) ;
	PDLDEBUG_f(printf("Make_physdims %p (%X)\n",(void*)it, c));
        PDL_CHKMAGIC(it);
	if(!c) {
	  PDLDEBUG_f(printf("Make_physdims_exit (NOP) %p\n",(void*)it));
	  return;
	}
	it->state &= ~(PDL_PARENTDIMSCHANGED | PDL_PARENTREPRCHANGED);
	/* the fact that a PARENTXXXCHANGED flag is set seems
	   to imply that this pdl has an associated trans ? */
	for(i=0; i<it->trans_parent->vtable->nparents; i++) {
		pdl_make_physdims(it->trans_parent->pdls[i]);
	}
	/* doesn't this mean that all children of this trans have
	   now their dims set and accordingly all those flags should
	   be reset? Otherwise redodims will be called for them again? */
	PDLDEBUG_f(printf("Make_physdims: calling redodims %p on %p\n",
			  (void*)(it->trans_parent),(void*)it));
	REDODIMS(it->trans_parent);
	/* why this one? will the old allocated data be freed correctly? */
	if((c & PDL_PARENTDIMSCHANGED) && (it->state & PDL_ALLOCATED)) {
		it->state &= ~PDL_ALLOCATED;
	}
	PDLDEBUG_f(printf("Make_physdims_exit %p\n",(void*)it));
}

/* Order is important: do childtrans first, then parentrans. */

void pdl_set_trans_childtrans(pdl *it, pdl_trans *trans, PDL_Indx nth)
{
	pdl__addchildtrans(it,trans,nth);
/* Determine if we want to do dataflow */
	if(it->state & PDL_DATAFLOW_F)
		trans->flags |= PDL_ITRANS_DO_DATAFLOW_F;
	if(it->state & PDL_DATAFLOW_B)
		trans->flags |= PDL_ITRANS_DO_DATAFLOW_B;
}

/* This is because for "+=" (a = a + b) we must check for
   previous parent transformations and mutate if they exist
   if no dataflow. */

void pdl_set_trans_parenttrans(pdl *it, pdl_trans *trans,PDL_Indx nth)
{
	if((it->trans_parent || is_parent_of(it,trans))
	   /* && (it->state & PDL_DATAFLOW_F) */ ) {
		croak("Sorry, families not allowed now (i.e. You cannot modify dataflowing pdl)\n");
	}
	it->trans_parent = trans;
	it->state |= PDL_PARENTDIMSCHANGED | PDL_PARENTDATACHANGED ;
	trans->pdls[nth] = it;
}

/* Called with a filled pdl_trans struct.
 * Sets the parent and trans fields of the ndarrays correctly,
 * creating families and the like if necessary.
 * Alternatively may just execute transformation
 * that would require families but is not dataflowed.
 */
void pdl_make_trans_mutual(pdl_trans *trans)
{
  PDL_Indx i;
  int fflag=0;
  int cfflag=0;
  int pfflag=0;
  PDL_TR_CHKMAGIC(trans);
/* Then, set our children. This is: */
/* First, determine whether any of our children already have
 * a parent, and whether they need to be updated. If this is
 * the case, we need to do some thinking. */
  PDLDEBUG_f(printf("make_trans_mutual %p\n",(void*)trans));
  PDLDEBUG_f(pdl_dump_trans_fixspace(trans,3));
  for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
	if(trans->pdls[i]->trans_parent) fflag ++;
	if(trans->pdls[i]->state & PDL_DATAFLOW_ANY) cfflag++;
  }
  for(i=0; i<trans->vtable->nparents; i++)
	if(trans->pdls[i]->state & PDL_DATAFLOW_ANY)
		pfflag++;
/* If children are flowing, croak. It's too difficult to handle
 * properly */
  if(cfflag)
	croak("Sorry, cannot flowing families right now\n");
/* Same, if children have trans yet parents are flowing */
  if(pfflag && fflag)
	croak("Sorry, cannot flowing families right now (2)\n");
/* Now, if parents are not flowing, just execute the transformation */
  if(!pfflag && !(trans->flags & PDL_ITRANS_DO_DATAFLOW_ANY)) {
	int wd[trans->vtable->npdls];
	/* mark this transform as non mutual in case we croak during
	   ensuring it */
	  trans->flags |= PDL_ITRANS_NONMUTUAL;
	  for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
		pdl *child = trans->pdls[i];
		pdl_children_changesoon(child,
			wd[i]=(child->state & PDL_NOMYDIMS ?
			 PDL_PARENTDIMSCHANGED : PDL_PARENTDATACHANGED));
		/* mark all pdls that have been given as nulls (PDL_NOMYDIMS)
		   as getting their dims from this trans */
		if(child->state & PDL_NOMYDIMS) {
			child->state &= ~PDL_NOMYDIMS;
			child->state |= PDL_MYDIMS_TRANS;
			child->trans_parent = trans;
		}
	  }
	/* now actually perform the transformation, i.e. call
	   transform's redodims and readdata vtable entries
	 */
	pdl__ensure_trans(trans,PDL_PARENTDIMSCHANGED); /* XXX Why? */
	/* Es ist vollbracht */
	for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
		pdl *child = trans->pdls[i];
		if( PDL_VAFFOK(child) &&
		    (trans->vtable->per_pdl_flags[i] & PDL_TPDL_VAFFINE_OK) )  {
			if(wd[i] & PDL_PARENTDIMSCHANGED)
				pdl_changed(child, PDL_PARENTDIMSCHANGED,0);
			pdl_vaffinechanged(child,PDL_PARENTDATACHANGED);
		} else
			pdl_changed(child,wd[i],0);
	}
	pdl_destroytransform(trans,0);
  } else { /* do the full flowing transform */
          PDLDEBUG_f(printf("make_trans_mutual flowing!\n"));
	  for(i=0; i<trans->vtable->nparents; i++)
		pdl_set_trans_childtrans(trans->pdls[i],trans,i);
	  for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
		pdl *child = trans->pdls[i];
		pdl_set_trans_parenttrans(child,trans,i);
		if(child->state & PDL_NOMYDIMS) {
			child->state &= ~PDL_NOMYDIMS;
			child->state |= PDL_MYDIMS_TRANS;
		}
	  }
	  if(!(trans->flags & PDL_ITRANS_TWOWAY))
		trans->flags &= ~PDL_ITRANS_DO_DATAFLOW_B;
  }
  PDLDEBUG_f(printf("make_trans_mutual_exit %p\n",(void*)trans));
} /* pdl_make_trans_mutual() */

void pdl_redodims_default(pdl_trans *trans) {
  PDLDEBUG_f(printf("pdl_redodims_default "));
  PDLDEBUG_f(pdl_dump_trans_fixspace(trans,0));
  PDL_Indx creating[trans->vtable->npdls];
  pdl_transvtable *vtable = trans->vtable;
  pdl **pdls = trans->pdls;
  PDL_Indx i;
  for (i=0; i<vtable->npdls; i++)
    creating[i] = (vtable->par_flags[i] & PDL_PARAM_ISCREAT) &&
      PDL_DIMS_FROM_TRANS(trans,pdls[i]);
  pdl_initthreadstruct(2, pdls,
    vtable->par_realdims, creating, vtable->npdls, vtable,
    &trans->pdlthread, trans->ind_sizes, trans->inc_sizes,
    vtable->per_pdl_flags, vtable->flags & PDL_TRANS_NO_PARALLEL);
  pdl_hdr_childcopy(trans);
  trans->dims_redone = 1;
}

void pdl_make_physical(pdl *it) {
	int i, vaffinepar=0;
	DECL_RECURSE_GUARD;

	PDLDEBUG_f(printf("Make_physical %p\n",(void*)it));
        PDL_CHKMAGIC(it);

	START_RECURSE_GUARD;
	if(it->state & PDL_ALLOCATED && !(it->state & PDL_ANYCHANGED))  {
		goto mkphys_end;
	}
	if(!(it->state & PDL_ANYCHANGED))  {
		pdl_allocdata(it);
		goto mkphys_end;
	}
	if(!it->trans_parent) {
	        ABORT_RECURSE_GUARD;
		die("PDL Not physical but doesn't have parent");
	}
	if(it->trans_parent->flags & PDL_ITRANS_ISAFFINE) {
		if(!PDL_VAFFOK(it))
			pdl_make_physvaffine(it);
	}
	if(PDL_VAFFOK(it)) {
	  PDLDEBUG_f(printf("Make_phys: VAFFOK\n"));
		pdl_readdata_vaffine(it);
		it->state &= (~PDL_ANYCHANGED);
		PDLDEBUG_f(pdl_dump(it));
		goto mkphys_end;
	}
	PDL_TR_CHKMAGIC(it->trans_parent);
	for(i=0; i<it->trans_parent->vtable->nparents; i++) {
		if(it->trans_parent->vtable->per_pdl_flags[i] &
		    PDL_TPDL_VAFFINE_OK) {
			pdl_make_physvaffine(it->trans_parent->pdls[i]);
                        /* check if any of the parents is a vaffine */
                        vaffinepar = vaffinepar || (it->trans_parent->pdls[i]->data != PDL_REPRP(it->trans_parent->pdls[i]));
                }  
		else
			pdl_make_physical(it->trans_parent->pdls[i]);
	}
        /* the next one is really strange:
         *
         * why do we need to call redodims if   !(it->state & PDL_ALLOCATED)   ???
         * this results in a) redodims called twice if make_physdims had already been
         * called for this ndarray and results in associated memory leaks!
         * On the other hand, if I comment out  !(it->state & PDL_ALLOCATED)
         * then we get errors for cases like 
         *                  $in = $lut->transpose->index($im->dummy(0));
         *                  $in .= pdl -5;
         * Currently ugly fix: detect in initthreadstruct that it has been called before
         * and free all pdl_thread related memory before reallocating
         * NOTE: this does not catch leaks when additional memory was allocated from with
         *       redodims!!!!!
         *
         * The real question is: why do we need another call to
         * redodims if !(it->state & PDL_ALLOCATED)??????
         * changed it so that redodims only called if
         *            (!(it->state & PDL_ALLOCATED) && vaffinepar)
         * i.e. at least one of the parent ndarrays is a real vaffine
         * CS
         */
	if((!(it->state & PDL_ALLOCATED) && vaffinepar) ||
	   it->state & PDL_PARENTDIMSCHANGED ||
	   it->state & PDL_PARENTREPRCHANGED)
		REDODIMS(it->trans_parent);
	if(!(it->state & PDL_ALLOCATED)) {
		pdl_allocdata(it);
	}
	READDATA(it->trans_parent);
	it->state &= (~PDL_ANYCHANGED) & (~PDL_OPT_ANY_OK);

  mkphys_end:
	PDLDEBUG_f(printf("Make_physical_exit %p\n",(void*)it));
	END_RECURSE_GUARD;
}

void pdl_children_changesoon_c(pdl *it,int what)
{
	pdl_trans *t;
	int i;
	PDL_DECL_CHILDLOOP(it);
	PDL_START_CHILDLOOP(it)
		t = PDL_CHILDLOOP_THISCHILD(it);
		if(!(t->flags & PDL_ITRANS_DO_DATAFLOW_F)) {
			pdl_destroytransform(t,1);
		} else {
			for(i=t->vtable->nparents; i<t->vtable->npdls; i++) {
				pdl_children_changesoon_c(t->pdls[i],what);
			}
		}
	PDL_END_CHILDLOOP(it)
}

/* Change soon: if this is not writeback, separate from
   parent.
   If the children of this are not writeback, separate them.
 */

void pdl_children_changesoon(pdl *it, int what)
{
	unsigned int i;
	if(it->trans_parent &&
	   !(it->trans_parent->flags & PDL_ITRANS_DO_DATAFLOW_B)) {
		pdl_destroytransform(it->trans_parent,1);
	} else if(it->trans_parent) {
		if(!(it->trans_parent->flags & PDL_ITRANS_TWOWAY)) {
			die("PDL: Internal error: Trying to reverse irreversible trans");
		}
		for(i=0; i<it->trans_parent->vtable->nparents; i++)
			pdl_children_changesoon(it->trans_parent->pdls[i],what);
		return;
	}
	pdl_children_changesoon_c(it,what);
}

/* what should always be PARENTDATA */
void pdl_vaffinechanged(pdl *it, int what)
{
	if(!PDL_VAFFOK(it)) {
		croak("Vaffine not ok!, trying to use vaffinechanged");
	}
	PDLDEBUG_f(printf("pdl_vaffinechanged: writing back data, triggered by pdl %p, using parent %p\n",(void*)it,(void*)(it->vafftrans->from))); 
	pdl_changed(it->vafftrans->from,what,0);
}

void pdl_changed(pdl *it, int what, int recursing)
{
	pdl_children *c; int i; int j;

	PDLDEBUG_f(do {
          printf("pdl_changed: entry for pdl %p recursing: %d, what ",
		 (void*)it,recursing);
          pdl_dump_flags_fixspace(what,0,PDL_FLAGS_PDL);
	  if (it->state & PDL_TRACEDEBUG)
	     pdl_dump(it);
	} while (0));

	if(recursing) {
		it->state |= what;
		if(pdl__ismagic(it))
			pdl__call_magic(it,PDL_MAGIC_MARKCHANGED);
			}
	if(it->trans_parent && !recursing &&		(it->trans_parent->flags & PDL_ITRANS_DO_DATAFLOW_B)) {
		if((it->trans_parent->flags & PDL_ITRANS_ISAFFINE) &&
		   (PDL_VAFFOK(it))) {
		  PDLDEBUG_f(printf("pdl_changed: calling writebackdata_vaffine (pdl %p)\n",(void*)it));
			pdl_writebackdata_vaffine(it);
			pdl_changed(it->vafftrans->from,what,0);
		} else {
			PDLDEBUG_f(printf("pdl_changed: calling writebackdata from vtable, triggered by pdl %p, using trans %p\n",(void*)it,(void*)(it->trans_parent)));
			WRITEDATA(it->trans_parent);
			for(i=0; i<it->trans_parent->vtable->nparents; i++) {
				if((it->trans_parent->vtable->per_pdl_flags[i] &
				    PDL_TPDL_VAFFINE_OK) &&
				   (it->trans_parent->pdls[i]->trans_parent) &&
				   (it->trans_parent->pdls[i]->trans_parent->flags & PDL_ITRANS_ISAFFINE) &&
				   (PDL_VAFFOK(it->trans_parent->pdls[i]))
				   ) {
					pdl_changed(it->trans_parent->pdls[i]->vafftrans->from,what,0);
				} else {
					pdl_changed(it->trans_parent->pdls[i],what,0);
				}
			}
		}
	} else {
		c=&it->children;
		do {
			for(i=0; i<PDL_NCHILDREN; i++) {
				if(c->trans_parent[i]) {
					for(j=c->trans_parent[i]->vtable->nparents;
						j<c->trans_parent[i]->vtable->npdls;
						j++) {
						pdl_changed(c->trans_parent[i]->pdls[j],what,1);
					}
				}
			}
			c=c->next;
		} while(c);
	}
	PDLDEBUG_f(printf("pdl_changed: exiting for pdl %p\n",(void*)it));
}

/* Make sure transformation is done */
void pdl__ensure_trans(pdl_trans *trans,int what)
{
	int j;
/* Make parents physical */
	int flag=0;
	int par_pvaf=0;
	flag |= what;

	PDL_TR_CHKMAGIC(trans);

	for(j=0; j<trans->vtable->nparents; j++) {
		if(trans->vtable->per_pdl_flags[j] &
		    PDL_TPDL_VAFFINE_OK) {
			par_pvaf++;
			if(!trans->pdls[j]) {return;} /* XXX!!! */
			pdl_make_physvaffine(trans->pdls[j]);
		} else {
			if(!trans->pdls[j]) {return;} /* XXX!!! */
			pdl_make_physical(trans->pdls[j]);
		}
	}

	for(; j<trans->vtable->npdls; j++) {
		if(trans->pdls[j]->trans_parent != trans) {
			if(trans->vtable->per_pdl_flags[j] &
			    PDL_TPDL_VAFFINE_OK) {
				par_pvaf++;
				if(!trans->pdls[j]) {return;} /* XXX!!! */
				pdl_make_physvaffine(trans->pdls[j]);
			} else
                       {       if(!trans->pdls[j]) {return;} /* XXX!!! */
                       PDLDEBUG_f(printf("not vaffine ok: %d\n",
                                         trans->vtable->per_pdl_flags[j]));
		       
				pdl_make_physical(trans->pdls[j]);

                       }
		}
		flag |= trans->pdls[j]->state & PDL_ANYCHANGED;
	}
	
	if(flag & PDL_PARENTDIMSCHANGED)
		REDODIMS(trans);
	for(j=0; j<trans->vtable->npdls; j++) {
		if(trans->pdls[j]->trans_parent == trans)
			PDL_ENSURE_ALLOCATED(trans->pdls[j]);
	}

	if(flag & (PDL_PARENTDATACHANGED | PDL_PARENTDIMSCHANGED)) {
		if(par_pvaf && (trans->flags & PDL_ITRANS_ISAFFINE)) {
		  /* Attention: this assumes affine = p2child */
		  /* need to signal that redodims has already been called */
		  /* is it correct to also unset PDL_PARENTREPRCHANGED? */
		        trans->pdls[1]->state &= ~(PDL_PARENTDIMSCHANGED |
						  PDL_PARENTREPRCHANGED);
			pdl_make_physvaffine(trans->pdls[1]);
			pdl_readdata_vaffine(trans->pdls[1]);
		} else {
			READDATA(trans);
		}
	}
	for(j=trans->vtable->nparents; j<trans->vtable->npdls; j++) {
		trans->pdls[j]->state &= ~PDL_ANYCHANGED;
	}
}

/* Recursive! */
void pdl_vafftrans_remove(pdl * it)
{
	pdl_trans *t; int i;
	PDL_DECL_CHILDLOOP(it);
	PDL_START_CHILDLOOP(it)
		t = PDL_CHILDLOOP_THISCHILD(it);
		if(t->flags & PDL_ITRANS_ISAFFINE) {
			for(i=t->vtable->nparents; i<t->vtable->npdls; i++)
				pdl_vafftrans_remove(t->pdls[i]);
		}
	PDL_END_CHILDLOOP(it)
	pdl_vafftrans_free(it);
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
   have an appropriate transformation of which they are a child

   should probably have been called make_physcareful to point out what
   it really does
*/
void pdl_make_physvaffine(pdl *it)
{
	pdl_trans *t;
	pdl *parent;
	pdl *current;
	PDL_Indx i,j;
	PDL_Indx inc;
	PDL_Indx newinc;
	PDL_Indx ninced;
	int flag;
	int incsign;

	PDLDEBUG_f(printf("Make_physvaffine %p\n",(void*)it));

	pdl_make_physdims(it);

	PDL_Indx incsleft[it->ndims];
	if(!it->trans_parent) {
		pdl_make_physical(it);
		goto mkphys_vaff_end;
	}
	if(!(it->trans_parent->flags & PDL_ITRANS_ISAFFINE)) {
		pdl_make_physical(it);
		goto mkphys_vaff_end;
	}

	if (!it->vafftrans || it->vafftrans->ndims < it->ndims)
	  pdl_vafftrans_alloc(it);

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
		  pdl_pdl_barf("pdl_make_physvaffine: affine trans has NULL incs\n");
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
	pdl_make_physical(current);

  mkphys_vaff_end:
	PDLDEBUG_f(printf("Make_physvaffine_exit %p\n",(void*)it));
}

void pdl_vafftrans_alloc(pdl *it)
{
	if(!it->vafftrans) {
		it->vafftrans = malloc(sizeof(*(it->vafftrans)));
		if (!it->vafftrans) croak("Out of Memory\n");
		it->vafftrans->incs = 0;
		it->vafftrans->ndims = 0;
	}
	if(!it->vafftrans->incs || it->vafftrans->ndims < it->ndims ) {
		if(it->vafftrans->incs) free(it->vafftrans->incs);
		it->vafftrans->incs = malloc(sizeof(*(it->vafftrans->incs))
					     * (size_t)it->ndims);
		if (!it->vafftrans->incs) croak("Out of Memory\n");
		it->vafftrans->ndims = it->ndims;
	}
}

void pdl_set_datatype(pdl *a, int datatype)
{
    pdl_make_physical(a);
    if(a->trans_parent)
	    pdl_destroytransform(a->trans_parent,1);
    pdl_converttype( a, datatype );
}

void pdl_sever(pdl *src)
{
    if (!src->trans_parent) return;
    pdl_make_physvaffine(src);
    pdl_destroytransform(src->trans_parent,1);
}

/* newval = 1 means set flag, 0 means clear it */
void pdl_propagate_badflag( pdl *it, int newval ) {
    PDL_DECL_CHILDLOOP(it)
    PDL_START_CHILDLOOP(it)
    {
	pdl_trans *trans = PDL_CHILDLOOP_THISCHILD(it);
	int i;
	for( i = trans->vtable->nparents;
	     i < trans->vtable->npdls; i++ ) {
	    pdl *child = trans->pdls[i];
	    char need_recurse = (!!newval != !!(child->state & PDL_BADVAL));
	    if ( newval ) {
		child->state |=  PDL_BADVAL;
            } else {
		child->state &= ~PDL_BADVAL;
	    }
	    /* make sure we propagate to grandchildren, etc if changed */
	    if (need_recurse)
		pdl_propagate_badflag( child, newval );
        } /* for: i */
    }
    PDL_END_CHILDLOOP(it)
} /* pdl_propagate_badflag */

void pdl_propagate_badvalue( pdl *it ) {
    PDL_DECL_CHILDLOOP(it)
    PDL_START_CHILDLOOP(it)
    {
	pdl_trans *trans = PDL_CHILDLOOP_THISCHILD(it);
	int i;
	for( i = trans->vtable->nparents;
	     i < trans->vtable->npdls; i++ ) {
	    pdl *child = trans->pdls[i];
            child->has_badvalue = 1;
            child->badvalue = it->badvalue;
	    /* make sure we propagate to grandchildren, etc */
	    pdl_propagate_badvalue( child );
        } /* for: i */
    }
    PDL_END_CHILDLOOP(it)
} /* pdl_propagate_badvalue */

PDL_Anyval pdl_get_badvalue( int datatype ) {
    PDL_Anyval retval = { -1, {0} };
#define X(datatype, ctype, ppsym, shortctype, defbval) \
    retval.type = datatype; retval.value.ppsym = PDL.bvals.shortctype;
    PDL_GENERICSWITCH(datatype, X, croak("Not a known data type code=%d", datatype))
#undef X
    return retval;
}

PDL_Anyval pdl_get_pdl_badvalue( pdl *it ) {
    return it->has_badvalue ? it->badvalue : pdl_get_badvalue( it->datatype );
}

pdl_trans *pdl_create_trans(pdl_transvtable *vtable) {
    size_t it_sz = sizeof(pdl_trans)+sizeof(pdl *)*vtable->npdls;
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
    PDL_THR_CLRMAGIC(&it->pdlthread);
    it->pdlthread.inds = 0;
    it->ind_sizes = (PDL_Indx *)malloc(sizeof(PDL_Indx) * vtable->ninds);
    if (!it->ind_sizes) return NULL;
    int i; for (i=0; i<vtable->ninds; i++) it->ind_sizes[i] = -1;
    it->inc_sizes = (PDL_Indx *)malloc(sizeof(PDL_Indx) * vtable->nind_ids);
    if (!it->inc_sizes) return NULL;
    for (i=0; i<vtable->nind_ids; i++) it->inc_sizes[i] = -1;
    it->offs = -1;
    return it;
}

void pdl_dim_checks(
  pdl_transvtable *vtable, pdl **pdls,
  pdl_thread *pdlthread, PDL_Indx *creating,
  PDL_Indx *ind_sizes
) {
  PDL_Indx i, j, ind_id;
  PDLDEBUG_f(printf("pdl_dim_checks %p:\n", ind_sizes));
  PDLDEBUG_f(do {printf("  ind_sizes: "); pdl_print_iarr(ind_sizes, vtable->ninds);printf("\n");}while(0));
  for (i=0; i<vtable->npdls; i++) {
    PDL_Indx ninds = vtable->par_realdims[i];
    pdl *pdl = pdls[i];
    PDL_Indx ndims = pdl->ndims;
    PDLDEBUG_f(printf("pdl_dim_checks pdl %"IND_FLAG": ", i));
    PDLDEBUG_f(pdl_dump(pdl));
    if (creating[i]) {
      PDL_Indx dims[PDLMAX(ninds, 1)]; /* Empty arrays not allowed in C99 */
      for (j=0; j<ninds; j++)
	dims[j] = ind_sizes[PDL_IND_ID(vtable, i, j)];
      pdl_thread_create_parameter(
	pdlthread,i,dims,
	vtable->par_flags[i] & PDL_PARAM_ISTEMP
      );
    } else {
      PDL_Indx *dims = pdl->dims;
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
	  pdl_pdl_barf(
	    "Error in %s: parameter '%s' index %s size %"IND_FLAG", but ndarray dim has size %"IND_FLAG"\n",
	    vtable->name, vtable->par_names[i], vtable->ind_names[ind_id],
	    ind_sizes[ind_id], dims[j]
	  );
      }
      if (vtable->par_flags[i] & PDL_PARAM_ISPHYS)
	pdl_make_physical(pdl);
    }
  }
  for (i=0; i<vtable->npdls; i++) {
    PDL_Indx ninds = vtable->par_realdims[i];
    pdl *pdl = pdls[i];
    PDL_Indx *dims = pdl->dims;
    if (!ninds || !(vtable->par_flags[i] & PDL_PARAM_ISPHYS)) continue;
    for (j=0; j<ninds; j++) {
      ind_id = PDL_IND_ID(vtable, i, j);
      if (ind_sizes[ind_id] > 1 && ind_sizes[ind_id] != dims[j])
	pdl_pdl_barf(
	  "Error in %s: [phys] parameter '%s' index '%s' size %"IND_FLAG", but ndarray dim has size %"IND_FLAG"\n",
	  vtable->name, vtable->par_names[i], vtable->ind_names[ind_id],
	  ind_sizes[ind_id], dims[j]
	);
    }
  }
  PDLDEBUG_f(printf("pdl_dim_checks after:\n"));
  PDLDEBUG_f(do {printf("  ind_sizes: "); pdl_print_iarr(ind_sizes, vtable->ninds);printf("\n");fflush(stdout);}while(0));
}

void pdl_type_coerce(pdl_trans *trans) {
  PDL_Indx i;
  pdl_transvtable *vtable = trans->vtable;
  pdl **pdls = trans->pdls;
  trans->__datatype = -1;
  if (vtable->npdls == 2 && pdls[0]->has_badvalue
      && (vtable->par_flags[1] & PDL_PARAM_ISCREATEALWAYS)) {
    /* P2Child case */
    trans->has_badvalue = 1;
    trans->badvalue = pdls[0]->badvalue;
  }
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
    if ((flags & PDL_PARAM_ISCREATEALWAYS) ||
       ((flags & PDL_PARAM_ISCREAT) && (pdl->state & PDL_NOMYDIMS) && pdl->trans_parent == NULL)) {
      pdl->badvalue = trans->badvalue;
      pdl->has_badvalue = trans->has_badvalue;
      pdl->datatype = new_dtype;
    } else if (new_dtype != pdl->datatype) {
      pdls[i] = pdl_get_convertedpdl(pdl, new_dtype);
      if (!pdls[i]) pdl_pdl_barf("%s got NULL pointer from get_convertedpdl on param %s", vtable->name, vtable->par_names[i]);
      if(pdls[i]->datatype != new_dtype) pdl_pdl_barf("type not expected value after get_convertedpdl\n");
    }
  }
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

void pdl_trans_check_pdls(pdl_trans *trans) {
  PDL_Indx i;
  pdl_transvtable *vtable = trans->vtable;
  pdl **pdls = trans->pdls;
  for (i=0; i<vtable->npdls; i++) {
    if (!pdls[i])
      pdl_pdl_barf("%s got NULL pointer on param %s", vtable->name, vtable->par_names[i]);
  }
}
