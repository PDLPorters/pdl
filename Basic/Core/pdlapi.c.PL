#
# Create pdlapi.c
# - needed since we allow bad pixel handling to be switched off
#
 
use strict;
 
use Config;
use File::Basename qw(&basename &dirname);
 
# check for bad value support
use vars qw( $bvalflag $usenan );
require "badsupport.p";

# This forces PL files to create target in same directory as PL file.
# This is so that make depend always knows where to find PL derivatives.
chdir(dirname($0));
my $file;
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//                                                              
    if ($Config{'osname'} eq 'VMS' or
	$Config{'osname'} eq 'OS2');  # "case-forgiving"
open OUT,">$file" or die "Can't create $file: $!";
 
if ( $bvalflag ) {
    print "Extracting $file (WITH bad value support)\n";
} else {
    print "Extracting $file (NO bad value support)\n";
}
chmod 0644, $file;
 
print OUT <<"!WITH!SUBS!";
 
/* pdlapi.c - generated automatically by pdlapi.c.PL */
/*          - functions for manipulating pdl structs */
/*          - bad value support = $bvalflag */

!WITH!SUBS!
 
print OUT <<'!NO!SUBS!';

#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

/* Uncomment the following if you have core dumps or strange
 * behaviour - it may reveal the cause by croaking because of
 * bad magic number.
 */

/* #define DONT_REALLY_FREE
 */

/* This define causes the affine transformations not to be
 * optimized away so $a->slice(...) will always made physical.
 * Uncommenting this define is not recommended at the moment
 */

/* #define DONT_OPTIMIZE
 * #define DONT_VAFFINE
 */

extern Core PDL;

void pdl__ensure_trans(pdl_trans *trans,int what) ;

static int has_children(pdl *it) {
	PDL_DECL_CHILDLOOP(it)
	PDL_START_CHILDLOOP(it)
		return 1;
	PDL_END_CHILDLOOP(it)
	return 0;
}

static int is_child_of(pdl *it,pdl_trans *trans) {
	int i;
	for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
		if(trans->pdls[i] == it)  return 1;
	}
	return 0;
}

static int is_parent_of(pdl *it,pdl_trans *trans) {
	int i;
	for(i=0; i<trans->vtable->nparents; i++) {
		if(trans->pdls[i] == it)  return 1;
	}
	return 0;
}

pdl *pdl_null() {
	PDL_Long d[1] = {0};
	pdl *it = pdl_new();
	pdl_makescratchhash(it,0.0,PDL_B);
	pdl_setdims(it,d,1);
	it->state |= PDL_NOMYDIMS;
	return it;
}

pdl *pdl_get_convertedpdl(pdl *old,int type) {
	if(old->datatype != type) {
		pdl *it;
		it = pdl_null();
		PDL.converttypei_new(old,it,type);
		if(it->datatype != type) { croak("FOOBAR! HELP!\n"); }
		return it;
	} else {
		return old;
	}
}

void pdl_allocdata(pdl *it) {
	int i;
	int nvals=1;
	SV *bar;
	for(i=0; i<it->ndims; i++) {
			nvals *= it->dims[i];
	}
	it->nvals = nvals;
	PDLDEBUG_f(printf("pdl_allocdata %d, %d, %d\n",it, it->nvals,
		it->datatype));

	pdl_grow(it,nvals);
	PDLDEBUG_f(pdl_dump(it));

	it->state |= PDL_ALLOCATED;
}

/* Wrapper to pdl_create so that the pdl_new and pdl_tmp functions
   can be stored in the Core struct and exported to external
   PDL XS modules */
pdl* pdl_external_new() {
  return  pdl_new();
}
pdl* pdl_external_tmp() {
  return pdl_tmp();
}
/* Return a new pdl - type is PDL_PERM or PDL_TMP - the latter is auto-freed
 * when current perl context is left
 *
 * pdl_new() and pdl_tmp() are macroes defined in pdlcore.h
 * which just call this routine.
 */


pdl* pdl_create(int type) {
     int i;
     pdl* it;

     if(type == PDL_TMP) {croak("PDL internal error. FIX!\n");}

     it = (pdl*) malloc(sizeof(pdl));
     if (it==NULL)
        croak("Out of Memory\n");

     it->magicno = PDL_MAGICNO;
     it->state = 0;
     it->datatype = 0;
     it->trans = NULL;
     it->vafftrans = NULL;
     it->sv = NULL;
     it->datasv = 0;
     it->data = 0;

     it->dims = it->def_dims;
     it->dimincs = it->def_dimincs;
     it->ndims = 0;

     it->nthreadids = 0;
     it->threadids = it->def_threadids;
     it->threadids[0] = 0;

     for(i=0; i<PDL_NCHILDREN; i++) {it->children.trans[i]=NULL;}
     it->children.next = NULL;

     it->living_for = 0;
     it->progenitor = 0;
     it->future_me = 0;

     it->magic = 0;
     it->hdrsv = 0;

     PDLDEBUG_f(printf("CREATE %d\n",it));
     return it;
}

/* Explicit free. Do not use, use destroy instead, which causes this
   to be called when the time is right */
void pdl__free(pdl *it) {
    pdl_children *p1,*p2;
    PDL_CHKMAGIC(it);

    /* now check if magic is still there */
    if (pdl__ismagic(it)) {
      PDLDEBUG_f(printf("%d is still magic\n",it));
      PDLDEBUG_f(pdl__print_magic(it));
    }

    it->magicno = 0x42424245;
    PDLDEBUG_f(printf("FREE %d\n",it));
#ifndef DONT_REALLY_FREE
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
/* Free the phys representation */
/* XXX MEMLEAK */
/*    it->vtable->freetrans(it,it->trans); */

/* Call special freeing magic, if exists */
    if(PDL_ISMAGIC(it)) {
    	pdl__call_magic(it, PDL_MAGIC_DELETEDATA);
    }

    if(it->datasv) {
	    SvREFCNT_dec(it->datasv);
	    it->data=0;
    } else if(it->data) {
    	    warn("Warning: special data without datasv is not freed currently!!");
    }
    if(it->hdrsv) {
    	SvREFCNT_dec(it->hdrsv);
	it->hdrsv = 0;
    }
    free(it);
#endif
    PDLDEBUG_f(printf("ENDFREE %d\n",it));
}

void pdl__destroy_childtranses(pdl *it,int ensure) {
	PDL_DECL_CHILDLOOP(it);
	PDL_START_CHILDLOOP(it)
		pdl_destroytransform(PDL_CHILDLOOP_THISCHILD(it),ensure);
	PDL_END_CHILDLOOP(it)
}

/* Must think about.
  A piddle may be
   - a parent of something - just ensure & destroy
   - a child of something - just ensure & destroy
   - parent of two pdls which both propagate backwards - mustn't destroy.
   - both parent and child at same time, to something that propagates.
  Therefore, simple rules:
   - allowed to destroy if
      1. a parent with max. 1 backwards propagating transformation
      2. a child with no children & this is not the mutator of the family
         (otherwise the mutator might be my child).

  When the mutator or progenitor of a family is destroyed, it must check
  whether the other is also destroyed and if it is, destroy the family.

  Also, when a piddle is destroyed, it must tell its children and/or
  parent so that

  XXX Currently, will not destroy if any back-propagating children.

  Family may be destroyed, if either
   -nothing flowing out, in which case both mutated_to and
    futureprogenitor hash = 0
   -nothing flowing in

*/
void pdl_destroy(pdl *it) {
    int nback=0,nback2=0,nforw=0,nundest=0,nundestp=0;
    int nafn=0;
    pdl_trans *curt;
    PDL_DECL_CHILDLOOP(it);
    PDL_CHKMAGIC(it);
    PDLDEBUG_f(printf("Destr. %d\n",it);)
    if(it->state & PDL_DESTROYING) {
        PDLDEBUG_f(printf("Already Destr. %d\n",it);)
    	return;
    }
    it->state |= PDL_DESTROYING;
    /* Clear the sv field so that there will be no dangling ptrs */
    if(it->sv) {
	    sv_setiv(it->sv,0x4242);
	    it->sv = NULL;
    }

    if(it->progenitor || it->living_for || it->future_me) {
    	/* XXXXXXXXX Shouldn't do this! BAD MEMLEAK */
        PDLDEBUG_f(printf("Family, not Destr. %d\n",it);)
	goto soft_destroy;
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
	if(PDL_CHILDLOOP_THISCHILD(it)->flags & PDL_ITRANS_FORFAMILY)
		nundest ++;
/*	pdl_make_physdims(PDL_CHILDLOOP_THISCHILD(it); */
	if(PDL_CHILDLOOP_THISCHILD(it)->flags & PDL_ITRANS_ISAFFINE) {
		if(!(curt->pdls[1]->state & PDL_ALLOCATED)) {
			nafn ++;
		}
	}
    PDL_END_CHILDLOOP(it)

    if(it->trans && (it->trans->flags & PDL_ITRANS_FORFAMILY))
    	nundestp ++;

/* XXX FIX */
    if(nundest || nundestp) goto soft_destroy;

/* First case where we may not destroy */
    if(nback2 > 0) goto soft_destroy;
    if(nback > 1) goto soft_destroy;

/* Also not here */
    if(it->trans && nforw) goto soft_destroy;

/* Also, we do not wish to destroy if the children would be larger
 * than the parent and are currently not allocated (e.g. lags).
 * Because this is too much work to check, we refrain from destroying
 * for now if there is an affine child that is not allocated
 */
    if(nafn) goto soft_destroy;
    if(pdl__magic_isundestroyable(it)) {
        PDLDEBUG_f(printf("Magic, not Destr. %d\n",it);)
    	goto soft_destroy;
    }

    pdl__destroy_childtranses(it,1);

    if(it->trans) {
      PDLDEBUG_f(printf("Destr_trans. %d %d\n",it->trans, it->trans->flags);)
        /* Ensure only if there are other children! */
	/* XXX Bad: tmp! */
      if (it->trans->flags & PDL_ITRANS_NONMUTUAL)
	pdl_destroytransform_nonmutual(it->trans,(it->trans->vtable->npdls
	  			        - it->trans->vtable->nparents > 1));
      else
    	pdl_destroytransform(it->trans,(it->trans->vtable->npdls
	  			        - it->trans->vtable->nparents > 1));
    }

/* Here, this is a child but has no children */
    goto hard_destroy;


   hard_destroy:
#ifdef OIFSJEFLESJF
/* Now, check for progenitor-stuff. */
    if(it->progenitor) {
      if(!(it->progenitor->sv)) {
	      pdl__family_destroy_if(it->progenitor);
      } else
       	goto soft_destroy;
    } else if(it->living_for) {
    	pdl__family_fut_destroy_if(it);
    }
#endif

/* ... and now we drink */
   pdl__free(it);
   PDLDEBUG_f(printf("End destroy %d\n",it);)

   return;

  soft_destroy:
    PDLDEBUG_f(printf("May have dependencies, not destr. %d, nu(%d, %d), nba(%d, %d), nforw(%d), tra(%d), nafn(%d)\n",it,
    			nundest, nundestp, nback, nback2, nforw, it->trans, nafn);)
    it->state &= ~PDL_DESTROYING;
}


/* Straight copy, no dataflow */
pdl *pdl_hard_copy(pdl *src) {
	int i;
	pdl *it = pdl_null();
	it->state = 0;

	pdl_make_physical(src); /* Wasteful XXX... should be lazier */

	it->datatype = src->datatype;

	pdl_setdims(it,src->dims,src->ndims);
	pdl_allocdata(it);

 /* null != [0] */
#ifdef ELIFSLEFJSEFSE
	if(src->ndims == 1 && src->dims[0] == 0)
#else
	if(src->state & PDL_NOMYDIMS)
#endif
		it->state |= PDL_NOMYDIMS;

	pdl_reallocthreadids(it,src->nthreadids);
	for(i=0; i<src->nthreadids; i++) {
		it->threadids[i] = src->threadids[i];
	}

	memcpy(it->data,src->data, pdl_howbig(it->datatype) * it->nvals);

	return it;

}

/* Dump a tranformation (don't dump the pdls, just pointers to them */
void pdl_dump_trans (pdl_trans *it, int nspac) {
	int i;
	char *spaces = malloc(nspac+1); for(i=0; i<nspac; i++) spaces[i]=' ';
	spaces[i] = '\0';
	printf("%sDUMPTRANS %d (%s)\n",spaces,it,it->vtable->name);
	if(it->flags & PDL_ITRANS_ISAFFINE) {
		pdl_trans_affine *foo = (pdl_trans_affine *)it;
		if(it->pdls[1]->state & PDL_PARENTDIMSCHANGED) {
			printf("%s   AFFINE, BUT DIMSCHANGED\n",spaces);
		} else {
			printf("%s   AFFINE: o:%d, i:(",spaces,foo->offs);
			for(i=0; i<foo->pdls[1]->ndims; i++) {
				printf("%s%d",(i?" ":""),foo->incs[i]);
			}
			printf(") d:(");
			for(i=0; i<foo->pdls[1]->ndims; i++) {
				printf("%s%d",(i?" ":""),foo->pdls[1]->dims[i]);
			}
			printf(")\n");
		}
	}
/*	if(it->vtable->dump) {it->vtable->dump(it);} */
	printf("%s   INPUTS: (",spaces);
	for(i=0; i<it->vtable->nparents; i++)
		printf("%s%d",(i?" ":""),it->pdls[i]);
	printf(")     OUTPUTS: (");
	for(;i<it->vtable->npdls; i++)
		printf("%s%d",(i?" ":""),it->pdls[i]);
	printf(")\n");
	free(spaces);
}

#define MAXLIN 60
void pdl_dump_flags(int flags, int nspac)
{
	int i;
	int len, found, sz;
!NO!SUBS!

    if ( $bvalflag ) { print OUT "	int flagval[15] = {\n"; }
    else             { print OUT "	int flagval[14] = {\n"; }

    print OUT <<'!NO!SUBS!';
	    PDL_ALLOCATED,PDL_PARENTDATACHANGED,
	    PDL_PARENTDIMSCHANGED,PDL_PARENTREPRCHANGED,
	    PDL_DATAFLOW_F,PDL_DATAFLOW_B,PDL_NOMYDIMS,
	    PDL_OPT_VAFFTRANSOK,PDL_INPLACE,PDL_DESTROYING,
	    PDL_DONTTOUCHDATA, PDL_MYDIMS_TRANS, PDL_HDRCPY, 
!NO!SUBS!

    if ( $bvalflag ) { print OUT "	    PDL_BADVAL,\n"; }

    print OUT <<'!NO!SUBS!';
		0};

!NO!SUBS!

    if ( $bvalflag ) { print OUT "	char *flagchar[14] = {\n"; }
    else             { print OUT "	char *flagchar[13] = {\n"; }

    print OUT <<'!NO!SUBS!';
	    "ALLOCATED","PARENTDATACHANGED",
	    "PARENTDIMSCHANGED","PARENTREPRCHANGED",
	    "DATAFLOW_F","DATAFLOW_B","NOMYDIMS",
	    "OPT_VAFFTRANSOK","INPLACE","DESTROYING",
	    "DONTTOUCHDATA","MYDIMS_TRANS", "HDRCPY"
!NO!SUBS!

    if ( $bvalflag ) { print OUT '	    ,"BADVAL"' . "\n"; }

    print OUT <<'!NO!SUBS!';
	};
	char *spaces = malloc(nspac+1); for(i=0; i<nspac; i++) spaces[i]=' ';
	spaces[i] = '\0';

	printf("%sState: (%d) ",spaces,flags);
	len = 0;
	found = 0;
	for (i=0;flagval[i]!=0; i++)
	  if (flags & flagval[i]) {
	    printf("%s%s",found ? "|":"",flagchar[i]);
	    found = 1;
	    sz += strlen(flagchar[i]);
	    if (sz>MAXLIN) {sz=0; printf("\n       %s",spaces);}
	  }
	printf("\n");
	free(spaces);
}

void pdl_dump_spac(pdl *it,int nspac)
{
	PDL_DECL_CHILDLOOP(it)
	int i;
	char *spaces = malloc(nspac+1); for(i=0; i<nspac; i++) spaces[i]=' ';
	spaces[i] = '\0';
	printf("%sDUMPING %d     datatype: %d\n",spaces,it,it->datatype);
	pdl_dump_flags(it->state,nspac+3);
	printf("%s   transv: %d, trans: %d, sv: %d\n",spaces,
		(it->trans?it->trans->vtable:0), it->trans, it->sv);
	if(it->datasv) {
		printf("%s   Data SV: %d, Svlen: %d, data: %d, nvals: %d\n", spaces,
			it->datasv, SvCUR((SV*)it->datasv), it->data, it->nvals);
	}
	printf("%s   Dims: %d (",spaces,it->dims);
	for(i=0; i<it->ndims; i++) {
		printf("%s%d",(i?" ":""),it->dims[i]);
	};
	printf(")\n%s   ThreadIds: %d (",spaces,it->threadids);
	for(i=0; i<it->nthreadids+1; i++) {
		printf("%s%d",(i?" ":""),it->threadids[i]);
	}
	if(PDL_VAFFOK(it)) {
		printf(")\n%s   Vaffine ok: %d, o:%d, i:(",
			spaces,it->vafftrans->from,it->vafftrans->offs);
		for(i=0; i<it->ndims; i++) {
			printf("%s%d",(i?" ":""),it->vafftrans->incs[i]);
		}
	}
	if(it->state & PDL_ALLOCATED) {
		printf(")\n%s   First values: (",spaces);
		for(i=0; i<it->nvals && i<10; i++) {
			printf("%s%f",(i?" ":""),pdl_get_offs(it,i));
		}
	} else {
		printf(")\n%s   (not allocated",spaces);
	}
	printf(")\n");
	if(it->trans) {
		pdl_dump_trans(it->trans,nspac+3);
	}
	printf("%s   CHILDREN:\n",spaces);
	PDL_START_CHILDLOOP(it)
		pdl_dump_trans(PDL_CHILDLOOP_THISCHILD(it),nspac+4);
	PDL_END_CHILDLOOP(it)
	/* XXX phys etc. also */
	free(spaces);
}

void pdl_dump (pdl *it) {
	pdl_dump_spac(it,0);
}


/* Reallocate this PDL to have ndims dimensions. The previous dims
   are copied. */

void pdl_reallocdims(pdl *it,int ndims) {
   int i;
   if (it->ndims < ndims) {  /* Need to realloc for more */
      if(it->dims != it->def_dims) free(it->dims);
      if(it->dimincs != it->def_dimincs) free(it->dimincs);
      if (ndims>PDL_NDIMS) {  /* Need to malloc */
         it->dims = malloc(ndims*sizeof(*(it->dims)));
         it->dimincs = malloc(ndims*sizeof(*(it->dimincs)));
         if (it->dims==NULL || it->dimincs==NULL)
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
/* XXX Check logic */
void pdl_reallocthreadids(pdl *it,int n) {
	int i;
	unsigned char *olds; int nold;
	if(n <= it->nthreadids) {
		it->nthreadids = n; it->threadids[n] = it->ndims; return;
	}
	nold = it->nthreadids; olds = it->threadids;
	if(n >= PDL_NTHREADIDS-1) {
		it->threadids = malloc(sizeof(*(it->threadids))*(n+1));
	} else {
		/* already is default */
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

/* Calculate default increments and grow the PDL data */

void pdl_resize_defaultincs(pdl *it) {
	int inc = 1;
	int i=0;
	for(i=0; i<it->ndims; i++) {
		it->dimincs[i] = inc; inc *= it->dims[i];
	}
	it->nvals = inc;
        it->state &= ~PDL_ALLOCATED; /* Need to realloc when phys */
#ifdef DONT_OPTIMIZE
	pdl_allocdata(it);
#endif
}

/* Init dims & incs - if *incs is NULL ignored (but space is always same for both)  */

void pdl_setdims(pdl* it, PDL_Long * dims, int ndims) {
   int i;

   pdl_reallocdims(it,ndims);

   for(i=0; i<ndims; i++)
      it->dims[i] = dims[i];

   pdl_resize_defaultincs(it);

   pdl_reallocthreadids(it,0);  /* XXX Maybe trouble */
}

/* This is *not* careful! */
void pdl_setdims_careful(pdl *it)
{
	pdl_resize_defaultincs(it);
#ifdef DONT_OPTIMIZE
	pdl_allocdata(it);
#endif
        pdl_reallocthreadids(it,0); /* XXX For now */
}

void pdl_print(pdl *it) {
#ifdef FOO
   int i;
   printf("PDL %d: sv = %d, data = %d, datatype = %d, nvals = %d, ndims = %d\n",
   	(int)it, (int)(it->hash), (int)(it->data), it->datatype, it->nvals, it->ndims);
   printf("Dims: ");
   for(i=0; i<it->ndims; i++) {
   	printf("%d(%d) ",it->dims[i],it->dimincs[i]);
   }
   printf("\n");
#endif
}

double pdl_get(pdl *it,int *inds) {
	int i;
	int offs=0;
	for(i=0; i<it->ndims; i++)
		offs += it->dimincs[i] * inds[i];
	return pdl_get_offs(it,offs);
}

double pdl_get_offs(pdl *it, PDL_Long offs) {
	PDL_Long dummy1=offs+1; PDL_Long dummy2=1;
	return pdl_at(it->data, it->datatype, &offs, &dummy1, &dummy2, 0, 1);
}

void pdl_put_offs(pdl *it, PDL_Long offs, double value) {
	PDL_Long dummy1=offs+1; PDL_Long dummy2=1;
	pdl_set(it->data, it->datatype, &offs, &dummy1, &dummy2, 0, 1, value);
}


void pdl__addchildtrans(pdl *it,pdl_trans *trans,int nth)
{
	int i; pdl_children *c;
	trans->pdls[nth] = it;
	c = &it->children;
	do {
		for(i=0; i<PDL_NCHILDREN; i++) {
			if(! c->trans[i]) {
				c->trans[i] = trans; return;
			}
		}
		if(!c->next) break;
		c=c->next;
	} while(1) ;
	c->next = malloc(sizeof(pdl_children));
	c->next->trans[0] = trans;
	for(i=1; i<PDL_NCHILDREN; i++)
		c->next->trans[i] = 0;
	c->next->next = 0;
}

/* Problem with this function: when transformation is destroyed,
 * there may be several different children with the same name.
 * Therefore, we cannot croak :(
 */
void pdl__removechildtrans(pdl *it,pdl_trans *trans,int nth,int all)
{
	int i; pdl_children *c; int flag = 0;
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
			if(c->trans[i] == trans) {
				c->trans[i] = NULL;
				flag = 1;
				if(!all) return;
				/* return;  Cannot return; might be many times
				  (e.g. $a+$a) */
			}
		}
		c=c->next;
	} while(c);
	/* this might be due to a croak when performing the trans; so
	   warn only for now, otherwise we leave trans undestructed ! */
	if(!flag)
		warn("Child not found for pdl %d, %d\n",it, trans);
}

void pdl__removeparenttrans(pdl *it, pdl_trans *trans, int nth)
{
	trans->pdls[nth] = 0;
	it->trans = 0;
}

void pdl_make_physdims(pdl *it) {
	int i;
	int c = (it->state & (PDL_PARENTDIMSCHANGED | PDL_PARENTREPRCHANGED)) ;
	PDLDEBUG_f(printf("Make_physdims %d\n",it));
        PDL_CHKMAGIC(it);
	if(!(it->state & (PDL_PARENTDIMSCHANGED | PDL_PARENTREPRCHANGED))) {
	  PDLDEBUG_f(printf("Make_physdims_exit (NOP) %d\n",it));
	  return;
	}
	it->state &= ~(PDL_PARENTDIMSCHANGED | PDL_PARENTREPRCHANGED);
	/* the fact that a PARENTXXXCHANGED flag is set seems
	   to imply that this pdl has an associated trans ? */
	for(i=0; i<it->trans->vtable->nparents; i++) {
		pdl_make_physdims(it->trans->pdls[i]);
	}
	/* doesn't this mean that all children of this trans have
	   now their dims set and accordingly all those flags should
	   be reset? Otherwise redodims will be called for them again? */
	PDLDEBUG_f(printf("Make_physdims: calling redodims %d on %d\n",
			  it->trans,it));
	it->trans->vtable->redodims(it->trans);
	/* why this one? will the old allocated data be freed correctly? */
	if((c & PDL_PARENTDIMSCHANGED) && (it->state & PDL_ALLOCATED)) {
		it->state &= ~PDL_ALLOCATED;
	}
	PDLDEBUG_f(printf("Make_physdims_exit %d\n",it));
}

void pdl_writeover(pdl *it) {
	pdl_make_physdims(it);
	pdl_children_changesoon(it,PDL_PARENTDATACHANGED);
	it->state &= ~PDL_PARENTDATACHANGED;
}

/* Order is important: do childtrans first, then parentrans. */

void pdl_set_trans_childtrans(pdl *it, pdl_trans *trans,int nth)
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

void pdl_set_trans_parenttrans(pdl *it, pdl_trans *trans,int nth)
{
	int i; int nthind;
	if((it->trans || is_parent_of(it,trans))
	   /* && (it->state & PDL_DATAFLOW_F) */ ) {
		/* XXX What if in several places */
		nthind=-1;
		for(i=0; i<trans->vtable->nparents; i++)
			if(trans->pdls[i] == it) nthind = i;
		croak("Sorry, families not allowed now (i.e. You cannot modify dataflowing pdl)\n");
		pdl_family_create(it,trans,nthind,nth);
	} else {
		it->trans = trans;
		it->state |= PDL_PARENTDIMSCHANGED | PDL_PARENTDATACHANGED ;
		trans->pdls[nth] = it;
#ifdef FOOBARBAR
		if(trans->flags & PDL_ITRANS_DO_DATAFLOW_F)
			it->state |= PDL_DATAFLOW_F;
		if(trans->flags & PDL_ITRANS_DO_DATAFLOW_B)
			it->state |= PDL_DATAFLOW_B;
#endif
	}
}

/* Called with a filled pdl_trans struct.
 * Sets the parent and trans fields of the piddles correctly,
 * creating families and the like if necessary.
 * Alternatively may just execute transformation
 * that would require families but is not dataflown.
 */
void pdl_make_trans_mutual(pdl_trans *trans)
{
  int i;
  int fflag=0;
  int cfflag=0;
  int pfflag=0;
  PDL_TR_CHKMAGIC(trans);

/* Then, set our children. This is: */
/* First, determine whether any of our children already have
 * a parent, and whether they need to be updated. If this is
 * the case, we need to do some thinking. */

  PDLDEBUG_f(printf("make_trans_mutual %d\n",trans));
  for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
  	if(trans->pdls[i]->trans) fflag ++;
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
  	int *wd = malloc(sizeof(int) * trans->vtable->npdls);
	/* mark this transform as non mutual in case we croak during
	   ensuring it */
	  trans->flags |= PDL_ITRANS_NONMUTUAL;
	  for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
	  	pdl_children_changesoon(trans->pdls[i],
			wd[i]=(trans->pdls[i]->state & PDL_NOMYDIMS ?
			 PDL_PARENTDIMSCHANGED : PDL_PARENTDATACHANGED));
	  }
	  for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
	  	if(trans->pdls[i]->state & PDL_NOMYDIMS) {
			trans->pdls[i]->state &= ~PDL_NOMYDIMS;
			trans->pdls[i]->state |= PDL_MYDIMS_TRANS;
			trans->pdls[i]->trans = trans;
		}
	  }
#ifdef BARBARBAR /* Not done */
	  for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++)
		trans->pdls[i]->state  |=
		   PDL_PARENTDIMSCHANGED | PDL_PARENTDATACHANGED;
#endif
	if(!trans->vtable) {die("INVALID 0V TRANS\n");}
	pdl__ensure_trans(trans,PDL_PARENTDIMSCHANGED); /* XXX Why? */
	/* Es ist vollbracht */
	for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
#ifndef DONT_VAFFINE
		if( PDL_VAFFOK(trans->pdls[i]) &&
		    (trans->vtable->per_pdl_flags[i] & PDL_TPDL_VAFFINE_OK) )  {
		    	if(wd[i] & PDL_PARENTDIMSCHANGED)
				pdl_changed(trans->pdls[i],
					PDL_PARENTDIMSCHANGED,0);
		    	pdl_vaffinechanged(
				trans->pdls[i],PDL_PARENTDATACHANGED);
		} else
#endif
			pdl_changed(trans->pdls[i],wd[i],0);
	}
	pdl_destroytransform_nonmutual(trans,0);
      free(wd);
  } else {
          PDLDEBUG_f(printf("make_trans_mutual flowing!\n"));
	  for(i=0; i<trans->vtable->nparents; i++)
		pdl_set_trans_childtrans(trans->pdls[i],trans,i);
	  for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++)
		pdl_set_trans_parenttrans(trans->pdls[i],trans,i);
	  if(!(trans->flags & PDL_ITRANS_REVERSIBLE))
		trans->flags &= ~PDL_ITRANS_DO_DATAFLOW_B;
	  for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
	  	if(trans->pdls[i]->state & PDL_NOMYDIMS) {
			trans->pdls[i]->state &= ~PDL_NOMYDIMS;
			trans->pdls[i]->state |= PDL_MYDIMS_TRANS;
		}
	  }
  }

#ifdef FOO
/* If we are not flowing, we must disappear */
  if(!(trans->flags & PDL_ITRANS_DO_DATAFLOW_ANY)) {
  	pdl_destroytransform(trans,1);
  }
#endif

  PDLDEBUG_f(printf("make_trans_mutual_exit %d\n",trans));

} /* pdl_make_trans_mutual() */


pdl *pdl_make_now(pdl *it) {
	if(it->future_me) return it->future_me;
	if(!it->progenitor) return it;
	return pdl_family_clone2now(it);
}

void pdl_make_physical(pdl *it) {
	int i, vaffinepar=0;
	DECL_RECURSE_GUARD;

	PDLDEBUG_f(printf("Make_physical %d\n",it));
        PDL_CHKMAGIC(it);

	START_RECURSE_GUARD;
	if(it->state & PDL_ALLOCATED && !(it->state & PDL_ANYCHANGED))  {
		goto mkphys_end;
	}
	if(!(it->state & PDL_ANYCHANGED))  {
		pdl_allocdata(it);
		goto mkphys_end;
	}
	if(!it->trans) {
		die("PDL Not physical but doesn't have parent");
	}
#ifndef DONT_OPTIMIZE
#ifndef DONT_VAFFINE
	if(it->trans->flags & PDL_ITRANS_ISAFFINE) {
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
#endif
#endif
	PDL_TR_CHKMAGIC(it->trans);
	for(i=0; i<it->trans->vtable->nparents; i++) {
#ifndef DONT_OPTIMIZE
#ifndef DONT_VAFFINE
		if(it->trans->vtable->per_pdl_flags[i] &
		    PDL_TPDL_VAFFINE_OK) {
		    	pdl_make_physvaffine(it->trans->pdls[i]);
                        /* check if any of the parents is a vaffine */
                        vaffinepar = vaffinepar || (it->trans->pdls[i]->data != PDL_REPRP(it->trans->pdls[i]));
                }  
		else
#endif
#endif
			pdl_make_physical(it->trans->pdls[i]);
	}
        /* the next one is really strange:
         *
         * why do we need to call redodims if   !(it->state & PDL_ALLOCATED)   ???
         * this results in a) redodims called twice if make_physdims had already been
         * called for this piddle and results in associated memory leaks!
         * On the other hand, if I comment out  !(it->state & PDL_ALLOCATED)
         * then we get errors for cases like 
         *                  $in = $lut->xchg(0,1)->index($im->dummy(0));
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
         * i.e. at least one of the parent piddles is a real vaffine
         * CS
         */
	if((!(it->state & PDL_ALLOCATED) && vaffinepar) ||
	   it->state & PDL_PARENTDIMSCHANGED ||
	   it->state & PDL_PARENTREPRCHANGED) {
		it->trans->vtable->redodims(it->trans);
	}
	if(!(it->state & PDL_ALLOCATED)) {
		pdl_allocdata(it);
	}
	/* Make parents physical first. XXX Needs more reasonable way */
	/* Already done
		for(i=0; i<it->trans->vtable->nparents; i++) {
			pdl_make_physical(it->trans->pdls[i]);
		}
	*/
	/*
	  We think we made them physical or physvaffine already...
	for(i=0; i<it->trans->vtable->npdls; i++) {
		if(!(it->trans->pdls[i]->state & PDL_ALLOCATED)) {
			croak("Trying to readdata without physicality");
		}
	}
	*/
	it->trans->vtable->readdata(it->trans);
	it->state &= (~PDL_ANYCHANGED) & (~PDL_OPT_ANY_OK);

  mkphys_end:
	PDLDEBUG_f(printf("Make_physical_exit %d\n",it));
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
	pdl_children *c; int i;
	if(it->trans &&
	   !(it->trans->flags & PDL_ITRANS_DO_DATAFLOW_B)) {
		pdl_destroytransform(it->trans,1);
	} else if(it->trans) {
		if(!(it->trans->flags & PDL_ITRANS_REVERSIBLE)) {
			die("PDL: Internal error: Trying to reverse irreversible trans");
		}
		for(i=0; i<it->trans->vtable->nparents; i++)
			pdl_children_changesoon(it->trans->pdls[i],what);
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
	pdl_changed(it->vafftrans->from,what,0);
}

/* This is inefficient: _changed writes back, which it really should not,
   before a parent is used (?). */
void pdl_changed(pdl *it, int what, int recursing)
{
	pdl_children *c; int i; int j;
/* XXX This might save time but is actually unsafe:
 * if a -> b -> c, and c made physical and a changed again,
 * the changedness doesn't propagate to c */
/*	if((it->state & what) == what) { return; } */
	if(recursing) {
		it->state |= what;
		it->state &= ~PDL_OPT_VAFFTRANSOK;
		if(pdl__ismagic(it))
			pdl__call_magic(it,PDL_MAGIC_MARKCHANGED);
	}
	if(it->trans && !recursing &&
		(it->trans->flags & PDL_ITRANS_DO_DATAFLOW_B)) {
		if((it->trans->flags & PDL_ITRANS_ISAFFINE) &&
		   (PDL_VAFFOK(it))) {
			pdl_writebackdata_vaffine(it);
			pdl_changed(it->vafftrans->from,what,0);
		} else {
			if(!it->trans->vtable->writebackdata) {
				die("Internal error: got so close to reversing irrev.");
			}
			it->trans->vtable->writebackdata(it->trans);
			for(i=0; i<it->trans->vtable->nparents; i++) {
				if((it->trans->vtable->per_pdl_flags[i] &
				    PDL_TPDL_VAFFINE_OK) &&
				   (it->trans->pdls[i]->trans) &&
				   (it->trans->pdls[i]->trans->flags & PDL_ITRANS_ISAFFINE) &&
				   (PDL_VAFFOK(it->trans->pdls[i]))
				   ) {
					pdl_changed(it->trans->pdls[i]->vafftrans->from,what,0);
				} else {
					pdl_changed(it->trans->pdls[i],what,0);
				}
			}
		}
	} else {
		c=&it->children;
		do {
			for(i=0; i<PDL_NCHILDREN; i++) {
				if(c->trans[i]) {
					for(j=c->trans[i]->vtable->nparents;
						j<c->trans[i]->vtable->npdls;
						j++) {
						pdl_changed(c->trans[i]->pdls[j],what,1);
					}
				}
			}
			c=c->next;
		} while(c);
	}
}

/* This transformation changes soon, so make sure the children
 * who don't flow go away
 * XXX Should be able to specify which children. */
void pdl_trans_changesoon(pdl_trans *trans,int what)
{
	int i;
	for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
		pdl_children_changesoon_c(trans->pdls[i],what);
	}
}

/* Changed, just propagate changes to children
 * XXX should be able to specify which children */
void pdl_trans_changed(pdl_trans *trans,int what)
{
	int i;
	for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
		pdl_changed(trans->pdls[i],what,1);
	}
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
#ifndef DONT_OPTIMIZE
#ifndef DONT_VAFFINE
		if(trans->vtable->per_pdl_flags[j] &
		    PDL_TPDL_VAFFINE_OK) {
			par_pvaf++;
			if(!trans->pdls[j]) {return;} /* XXX!!! */
			pdl_make_physvaffine(trans->pdls[j]);
		} else {
#endif
#endif
			if(!trans->pdls[j]) {return;} /* XXX!!! */
			pdl_make_physical(trans->pdls[j]);
		}
	}
	for(; j<trans->vtable->npdls; j++) {
		if(trans->pdls[j]->trans != trans) {
#ifndef DONT_OPTIMIZE
#ifndef DONT_VAFFINE
			if(trans->vtable->per_pdl_flags[j] &
			    PDL_TPDL_VAFFINE_OK) {
			    	par_pvaf++;
				if(!trans->pdls[j]) {return;} /* XXX!!! */
				pdl_make_physvaffine(trans->pdls[j]);
			} else
#endif
#endif
                       {       if(!trans->pdls[j]) {return;} /* XXX!!! */
                       PDLDEBUG_f(printf("not vaffine ok: %d\n",
                                         trans->vtable->per_pdl_flags[j]));

				pdl_make_physical(trans->pdls[j]);
                       }
		}
		flag |= trans->pdls[j]->state & PDL_ANYCHANGED;
	}
	if(flag & PDL_PARENTDIMSCHANGED) {  /* redodims called here... */
		trans->vtable->redodims(trans);
	}
	for(j=0; j<trans->vtable->npdls; j++) {
		if(trans->pdls[j]->trans == trans)
			PDL_ENSURE_ALLOCATED(trans->pdls[j]);
	}
	if(flag & PDL_PARENTDATACHANGED | flag & PDL_PARENTDIMSCHANGED) {
		int i;
		if(par_pvaf && (trans->flags & PDL_ITRANS_ISAFFINE)) {
		  /* Attention: this assumes affine = p2child */
		  /* need to signal that redodims has already been called */
		  /* is it correct to also unset PDL_PARENTREPRCHANGED? */
		        trans->pdls[1]->state &= ~(PDL_PARENTDIMSCHANGED |
						  PDL_PARENTREPRCHANGED);
			pdl_make_physvaffine(trans->pdls[1]);
			pdl_readdata_vaffine(trans->pdls[1]);
		} else {
#ifdef DONT_VAFFINE
			for(i=0; i<trans->vtable->npdls; i++) {
				if(!(trans->pdls[i]->state & PDL_ALLOCATED)) {
					croak("Trying to readdata without physicality");
				}
			}
#endif
			trans->vtable->readdata(trans);
		}
	}
	for(j=trans->vtable->nparents; j<trans->vtable->npdls; j++) {
		trans->pdls[j]->state &= ~PDL_ANYCHANGED;
	}
}

void pdl__ensure_transdims(pdl_trans *trans)
{
	int j;
	int flag=0;
	PDL_TR_CHKMAGIC(trans);
	for(j=0; j<trans->vtable->nparents; j++) {
		pdl_make_physdims(trans->pdls[j]);
	}
	trans->vtable->redodims(trans);
}

/* There is a potential problem here, calling
   pdl_destroy while the trans structure is not in a defined state.
   We shall ignore this problem for now and hope it goes away ;)
   (XXX FIX ME) */
/* XXX Two next routines are memleaks */
/* somehow this transform will call (implicitly) redodims twice on
   an unvaffined pdl; leads to memleak if redodims allocates stuff
   that is only freed in later call to freefunc */
void pdl_destroytransform(pdl_trans *trans,int ensure)
{
	int j;
	pdl *foo;
	pdl *destbuffer[100];
	int ndest = 0;

	PDLDEBUG_f(printf("entering pdl_destroytransform %d %d\n",trans,ensure));
	if(100 < trans->vtable->npdls) {
		die("Huge trans");
	}

	PDL_TR_CHKMAGIC(trans);
	if(!trans->vtable) {
		die("ZERO VTABLE DESTTRAN %d %d\n",trans,ensure);
	}
	if(ensure) {
		PDLDEBUG_f(printf("pdl_destroytransform: ensure\n"));
		pdl__ensure_trans(trans,0);
	}
	for(j=0; j<trans->vtable->nparents; j++) {
		foo = trans->pdls[j];
		if(!foo) continue;
		PDL_CHKMAGIC(foo);
		PDLDEBUG_f(printf("pdl_removectransform(%d): %d %d\n",
			trans, trans->pdls[j], j));
		pdl__removechildtrans(trans->pdls[j],trans,j,1);
		if(!(foo->state & PDL_DESTROYING) && !foo->sv) {
			destbuffer[ndest++] = foo;
		}
	}
	for(; j<trans->vtable->npdls; j++) {
		foo = trans->pdls[j];
		PDL_CHKMAGIC(foo);
		PDLDEBUG_f(printf("pdl_removeptransform(%d): %d %d\n",
			trans, trans->pdls[j], j));
		pdl__removeparenttrans(trans->pdls[j],trans,j);
		if(foo->vafftrans) {
			PDLDEBUG_f(printf("pdl_removevafft: %d\n", foo));
			pdl_vafftrans_remove(foo);
		}
		if(!(foo->state & PDL_DESTROYING) && !foo->sv) {
			destbuffer[ndest++] = foo;
		}
	}
	PDL_TR_CHKMAGIC(trans);
	if(trans->vtable->freetrans) {
		PDLDEBUG_f(printf("call freetrans\n"));
		trans->vtable->freetrans(trans); /* Free malloced objects */
	}
	PDL_TR_CLRMAGIC(trans);
	trans->vtable = 0; /* Make sure no-one uses this */
	if(trans->freeproc) {
		PDLDEBUG_f(printf("call freeproc\n"));
		trans->freeproc(trans);
	} else {
		PDLDEBUG_f(printf("call free\n"));
		free(trans);
	}

	for(j=0; j<ndest; j++) {
		pdl_destroy(destbuffer[j]);
	}

	PDLDEBUG_f(printf("leaving pdl_destroytransform %d\n", trans));

}

void pdl_destroytransform_nonmutual(pdl_trans *trans,int ensure)
{
	int i;

	PDLDEBUG_f(printf("entering pdl_destroytransform_nonmutual\n"));

	PDL_TR_CHKMAGIC(trans);
	if(ensure) {
		pdl__ensure_trans(trans,PDL_PARENTDIMSCHANGED);
	}
	PDL_TR_CHKMAGIC(trans);
	for(i=trans->vtable->nparents; i<trans->vtable->npdls; i++) {
		trans->pdls[i]->state &= ~PDL_NOMYDIMS;
		if(trans->pdls[i]->trans == trans)
			trans->pdls[i]->trans = 0;
	}
	PDL_TR_CHKMAGIC(trans);
	if(trans->vtable->freetrans) {
		trans->vtable->freetrans(trans);
	}
	PDL_TR_CLRMAGIC(trans);
	trans->vtable = 0; /* Make sure no-one uses this */
	if(trans->freeproc) {
		trans->freeproc(trans);
	} else {
		free(trans);
	}
	PDLDEBUG_f(printf("leaving pdl_destroytransform_nonmutual\n"));
}

void pdl_trans_mallocfreeproc(struct pdl_trans *tr) {
	free(tr);
}

#ifndef DONT_OPTIMIZE

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
	pdl_trans_affine *at;
	pdl *parent;
	pdl *current;
	int *incsleft = 0;
	int i,j;
	int inc;
	int newinc;
	int ninced;
	int flag;
	int incsign;

	PDLDEBUG_f(printf("Make_physvaffine %d\n",it));

	pdl_make_physdims(it);
	if(!it->trans) {
		pdl_make_physical(it);
		goto mkphys_vaff_end;
		/* croak("Trying to make physvaffine without parent!\n"); */
	}
	if(!(it->trans->flags & PDL_ITRANS_ISAFFINE)) {
		pdl_make_physical(it);
		goto mkphys_vaff_end;
	}
	PDL_ENSURE_VAFFTRANS(it);
	incsleft = malloc(sizeof(*incsleft)*it->ndims);
        PDLDEBUG_f(printf("vaff_malloc: got %d\n",incsleft));
        for(i=0; i<it->ndims; i++) {
		it->vafftrans->incs[i] = it->dimincs[i];
	}

	flag=0;
	it->vafftrans->offs = 0;
	t=it->trans;
	current = it;
	while(t && (t->flags & PDL_ITRANS_ISAFFINE)) {
		int cur_offset = 0;
		at = (pdl_trans_affine *)t;
		parent = t->pdls[0];
		/* For all dimensions of the childest piddle */
		for(i=0; i<it->ndims; i++) {
			int offset_left = it->vafftrans->offs;
			/* inc = the increment at the current stage */
			inc = it->vafftrans->incs[i];
			incsign = (inc >= 0 ? 1:-1);
			inc= abs(inc);
			newinc = 0;
			/* For all dimensions of the current piddle */
			for(j=current->ndims-1; j>=0; j--) {
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
					  int foo=
					   (cur_offset + it->dims[i]*ninced)*
					   current->dimincs[j];
					  int k;
					  for(k=j+1; k<current->ndims; k++) {
						foo -= current->dimincs[k-1] *
							current->dims[k-1];
					  	if(foo<=0) break;
						if(at->incs[k] !=
						   at->incs[k-1] *
						   current->dims[k-1]) {
						   /* XXXXX */
						   	flag=1;
						 /*
	warn("Illegal vaffine; fix loop to break: %d %d %d k=%d s=%d, (%d+%d*%d>%d) %d %d %d %d.\n",at,current,it,
		k,incsign,cur_offset,it->dims[i],ninced,current->dims[j],current->dimincs[j],
		at->incs[k],at->incs[k-1],current->dims[k-1]);
						*/
						   	/* croak("Illegal vaffine; fix loop to break.\n"); */
						}
					  }
					}
					newinc += at->incs[j]*ninced;
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
			int offset_left = it->vafftrans->offs;
			inc = it->vafftrans->offs;
			newinc = 0;
			for(j=current->ndims-1; j>=0; j--) {
				cur_offset = offset_left / current->dimincs[j];
				offset_left -= cur_offset * current->dimincs[j];
				newinc += at->incs[j]*cur_offset;
			}
			it->vafftrans->offs = newinc;
			it->vafftrans->offs += at->offs;
		}
		t = parent->trans;
		current = parent;
	}
	it->vafftrans->from = current;
	it->state |= PDL_OPT_VAFFTRANSOK;
	pdl_make_physical(current);

  mkphys_vaff_end:
       PDLDEBUG_f(printf("vaff_malloc: %d\n",incsleft));
       if (incsleft != NULL) free(incsleft);
	PDLDEBUG_f(printf("Make_physvaffine_exit %d\n",it));

}

void pdl_vafftrans_alloc(pdl *it)
{
	if(!it->vafftrans) {
		it->vafftrans = malloc(sizeof(*(it->vafftrans)));
		it->vafftrans->incs = 0;
		it->vafftrans->ndims = 0;
	}
	if(!it->vafftrans->incs || it->vafftrans->ndims < it->ndims ) {
		if(it->vafftrans->incs) free(it->vafftrans->incs);
		it->vafftrans->incs = malloc(sizeof(*(it->vafftrans->incs))
					     * it->ndims);
		it->vafftrans->ndims = it->ndims;
	}
}

#endif

!NO!SUBS!
