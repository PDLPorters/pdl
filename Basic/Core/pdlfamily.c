
/* pdlfamily.c - functions for manipulating pdl families
   in order to get dataflow with mutators (+=, ++..) work. */

/*-----------------------------------------*/
/* Here comes the hard part: we need to
 * be able to do "+=" or other mutators.
 * If the same pdl is given as input and output to some routine,
 * it has to be copied if threading is enabled.
 *
 * Therefore, we define a "family" of PDLs as a set of pdls
 * that are forward and backward propagated to each other.
 * What needs to happen is:
 *  $b = $a->slice(foo);
 *  $b += 3;
 *
 * Orig: hash($a) -> a0,  hash($b) -> b0
 * Now:  hash($a) -> a1,  hash($b) -> b1,
 *       a0 and b0 saved in memory with tmp hashes,
 *       transformation between them duplicated.
 *
 * Because $b was a slice (and not, for example, inversion),
 * it only covers a part of the original $a and therefore we
 * need to first assign a0 to a1 and then add 3 from b0 to b1
 * (or b1 to b1).
 *
 */
#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"

static void pdl_identity_redodims(pdl_trans *__tr);
static void pdl_identity_readdata(pdl_trans *__tr);

#define pdl_identity_trans pdl_trans

/* This is the function that cleverly changes who perl thinks
   we are. This should actually be implemented a little differently
   so that different places in perl could hold on to their ref
   if they wanted. But for now, */
void pdl__xchghashes(pdl *a,pdl *b)
{
    STRLEN n_a;
	SV *t;
	void *d;
	HV *tmp = a->sv;
	a->sv = b->sv;
	b->sv = tmp;
	if(a->sv) sv_setiv(a->sv,(IV) a);
	if(b->sv) sv_setiv(b->sv,(IV) b);
	t = a->datasv;
	a->datasv = b->datasv;
	b->datasv = t;
       a->data = (a->datasv?SvPV((SV*)a->datasv,n_a):NULL);
       b->data = (b->datasv?SvPV((SV*)b->datasv,n_a):NULL);
}

/* Recurse everywhere and set progenitor */

void pdl_family_setprogenitor(pdl *what,pdl *progenitor,pdl_trans *notthis)
{
	PDL_DECL_CHILDLOOP(what)
	pdl_trans *t; int i;
	what->progenitor = progenitor;
	PDL_START_CHILDLOOP(what)
		t = PDL_CHILDLOOP_THISCHILD(what);
		if(t!= notthis && (t->flags & PDL_ITRANS_DO_DATAFLOW_B)) {
			for(i=t->vtable->nparents; i<t->vtable->npdls; i++) {
				pdl_family_setprogenitor(t->pdls[i],progenitor,notthis);
			}
		}
	PDL_END_CHILDLOOP(what)
}

/* Because make_physdims(mutateto) calls our make_physdims again,
   need to set !dimschanged */
static void family_redodims(pdl_trans *tr)
{
	int i;
	pdl_family_trans *tr2 = (pdl_family_trans *)tr;
	pdl_identity_redodims(tr);
	tr2->pdls[1]->state &= ~(PDL_PARENTDIMSCHANGED | PDL_PARENTREPRCHANGED);
	if(tr2->mutateto != tr2->pdls[1]) {
		pdl_make_physdims(tr2->mutatefrom);
		pdl_make_physdims(tr2->mutateto);
	}
	pdl__ensure_transdims(tr2->realtrans);
}

static void family_readdata(pdl_trans *tr)
{
	pdl_family_trans *tr2 = (pdl_family_trans *)tr;
	PDLDEBUG_f(printf("Family_readdata %d -> %d (%d)\n",tr->pdls[0],tr->pdls[1],tr));
	pdl_identity_readdata(tr);
	if(tr2->mutateto != tr2->pdls[1]) {
		tr2->pdls[1]->state &= ~PDL_ANYCHANGED; /* Avoid infinite loops */
		pdl_make_physical(tr2->mutateto);
	}
	pdl__ensure_trans(tr2->realtrans,PDL_PARENTDATACHANGED);
	if(tr2->mutateto != tr2->pdls[1]) {
		pdl_changed(tr2->mutateto, PDL_PARENTDATACHANGED,0);
	}
	PDLDEBUG_f(printf("Family_readdata_exit %d -> %d (%d)\n",tr->pdls[0],tr->pdls[1],tr));
}

static char fam_flags[] = {0,0};
static pdl_transvtable familyvtable = {
	PDL_FUNCTION, 0, 1, 2,
	fam_flags,
	family_redodims,
	family_readdata,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	sizeof(pdl_family_trans),
	"Family virtual trans"
};

pdl *pdl_family_clone2now(pdl *from)
{
	pdl *new_parent;
	pdl_trans *ntrans;
	int i;
	pdl *newpdl;
	if(from->future_me) return from->future_me;

/* Create new pdl */
	newpdl = pdl_hard_copy(from);
	newpdl->state |= PDL_PARENTDATACHANGED
		| PDL_PARENTDIMSCHANGED; /* Mutator -> it's changed */
	newpdl->living_for  |= PDL_LIVINGFOR_FAMILY_NEWMUTATED;
/* Es ist vollbracht */
	from->future_me = newpdl;
	pdl__xchghashes(from,newpdl);
/* Am I progenitor */
	if(from->progenitor == from) {
		return newpdl; /* Do nothing */
	} else {
/* If not, does my parent have a future thing? */
		if(!from->trans->pdls[0]->future_me) {
			pdl_family_clone2now(from->trans->pdls[0]);
		}
		new_parent = from->trans->pdls[0]->future_me;
		if(!from->trans->vtable->copy) {
			die("Cannot copy source transformation!!!\n");
		}
		ntrans = from->trans->vtable->copy(from->trans);

/* Set the new transformation for us */
		for(i=0; i<ntrans->vtable->npdls; i++) {
			if(ntrans->pdls[i] == from->trans->pdls[0]) {
				pdl_set_trans_childtrans(new_parent,
					ntrans, i);
			}
		}
		for(i=0; i<ntrans->vtable->npdls; i++) {
			if(ntrans->pdls[i] == from) {
				pdl_set_trans_parenttrans(newpdl,
					ntrans, i);
			}
		}
	}
	return newpdl;
}

void pdl_family_create(pdl *from,pdl_trans *trans,int ind1,int ind2)
{
	pdl *cur = from;
	pdl *to;
	pdl *progfrom; pdl *progto; /* Progenitors */
	pdl_family_trans *famtrans;
/* 1. find the progenitor of "from". Only 1-parent transformations
 *    allowed for now */
	while(1) {
		if(!cur->trans) break;
		if(!(cur->trans->flags & PDL_ITRANS_DO_DATAFLOW_B)) break;
		if(cur->trans->vtable->nparents != 1) {
			die("Cannot mutate a pdl begotten from more than one progenitors\n");
		}
		if(cur->progenitor || cur->future_me) {
			die("Mutating the mutated! Internal error!\n");
		}
		cur = cur->trans->pdls[0];
	}
 	progfrom = cur;
/* 2. Go to all the reversible children and mark them
 *    and set their progenitor */
	pdl_family_setprogenitor(progfrom,progfrom,trans);

/* 3. Make the clones */
   	progto = pdl_family_clone2now(progfrom);
	progto->living_for  |= PDL_LIVINGFOR_FAMILY_NEWPROGENITOR;
	to = pdl_family_clone2now(from);
	to->living_for  |= PDL_LIVINGFOR_FAMILY_NEWMUTATED;

/* 4. Attach the clone transformation to the correct things */

	famtrans = malloc(sizeof(pdl_family_trans));
	PDL_TR_SETMAGIC(famtrans);
	famtrans->flags = 0;
	famtrans->vtable = &familyvtable;
	famtrans->freeproc = NULL;
	famtrans->realtrans = trans;

	trans->flags |= PDL_ITRANS_FORFAMILY;

	famtrans->mutateto = to;
	famtrans->mutatefrom = from;

	pdl_set_trans_childtrans(progfrom,(pdl_trans *)famtrans,0);
	pdl_set_trans_parenttrans(progto,(pdl_trans *)famtrans,1);
	famtrans->flags &= ~PDL_ITRANS_DO_DATAFLOW_B;

/* 5. Set the child transformation to do horrible things */

	if(ind1>=0)
		trans->pdls[ind1] = from;
	trans->pdls[ind2] = to;

}

/*=============================================================*/

/* XXX Copied from Slices.xs */
static void pdl_identity_redodims(pdl_trans *__tr) {
		int __dim;
		pdl_identity_trans *__priv = (pdl_identity_trans *)
							__tr;
		pdl *__it = __tr->pdls[1];
		pdl *__parent = __tr->pdls[0];
		{

			pdl_reallocdims(__it,__parent->ndims);
			for(__dim=0; __dim<__it->ndims; __dim++) { __it->dims[__dim] = __parent->dims[__dim]; }
			pdl_setdims_careful(__it);
			;}
			__it->threadids[0] = __it->ndims;
		}
static void pdl_identity_readdata(pdl_trans *__tr) {
		int __dim;
		pdl_identity_trans *__priv = (pdl_identity_trans *)
							__tr;
		pdl *__it = __tr->pdls[1];
		pdl *__parent = __tr->pdls[0];
		{
	{int *__myinds = pdl_malloc(sizeof(int)*__it->ndims);
	    		 int *__parentinds = pdl_malloc(sizeof(int)*__parent->ndims);
			 int __parentoffs; int __myoffs=0;
			 int __stop = 0; int __ind;
			 for(__ind = 0; __ind < __it->ndims; __ind++)
			 	__myinds[__ind] = 0;
			 while(!__stop) {
			 for(__ind=0; __ind<__parent->ndims; __ind++)
					__parentinds[__ind] = __myinds[__ind];;__parentoffs=0;
		 for(__ind=0; __ind<__parent->ndims; __ind++) {
			__parentoffs += __parent->dimincs[__ind]*__parentinds[__ind];
		 }

			pdl_put_offs(__it, __myoffs,
				pdl_get_offs(__parent, __parentoffs));
	  		__myoffs++;
	  		__stop=1;
	  		for(__ind=0; __ind < __it->ndims; __ind++) {
				__myinds[__ind]++;
				if(__myinds[__ind] >= __it->dims[__ind]) {
					__myinds[__ind] = 0;
				} else {__stop = 0; break;}
			}
		}
/*	free(__myinds); */
		}
			}

	}

