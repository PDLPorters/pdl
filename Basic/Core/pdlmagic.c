#define PDL_CORE      /* For certain ifdefs */
#ifndef WIN32
#define USE_MMAP
#else
#undef USE_MMAP
#endif

#include "pdlcore.h"

/* Singly linked list */
/* Note that this zeroes ->next!) */

void pdl__magic_add(pdl *it,pdl_magic *mag)
{
	pdl_magic **foo = &(it->magic);
	while(*foo) {
		foo = &((*foo)->next);
	}
	(*foo) = mag;
	mag->next = NULL;
}

void pdl__magic_rm(pdl *it,pdl_magic *mag)
{
	pdl_magic **foo = &(it->magic);
	while(*foo) {
		if(*foo == mag) {
			*foo = (*foo)->next;
		}
		foo = &((*foo)->next);
	}
	die("PDL:Magic not found: Internal error\n");
}

void pdl__magic_free(pdl *it)
{
  if (pdl__ismagic(it) && !pdl__magic_isundestroyable(it)) {
    pdl_magic *foo = it->magic;
    while(foo) {
      pdl_magic *next = foo->next;
      free(foo);
      foo = next;
    }
  }
}

/* Test for undestroyability */

int pdl__magic_isundestroyable(pdl *it)
{
	pdl_magic **foo = &(it->magic);
	while(*foo) {
		if((*foo)->what & PDL_MAGIC_UNDESTROYABLE) {return 1;}
		foo = &((*foo)->next);
	}
	return 0;
}

/* Call magics */

void *pdl__call_magic(pdl *it,int which)
{
	void *ret = NULL;
	pdl_magic **foo = &(it->magic);
	while(*foo) {
		if((*foo)->what & which) {
			if((*foo)->what & PDL_MAGIC_DELAYED)
				pdl_add_delayed_magic(*foo);
			else
				ret = (void *)((*foo)->vtable->cast(*foo));
					/* Cast spell */
		}
		foo = &((*foo)->next);
	}
	return ret;
}

/* XXX FINDS ONLY FIRST */
pdl_magic *pdl__find_magic(pdl *it, int which)
{
	pdl_magic **foo = &(it->magic);
	while(*foo) {
		if((*foo)->what & which) {
			return *foo;
		}
		foo = &((*foo)->next);
	}
	return NULL;
}

pdl_magic *pdl__print_magic(pdl *it)
{
	pdl_magic **foo = &(it->magic);
	while(*foo) {
	  printf("Magic %d\ttype: ",*foo);
		if((*foo)->what & PDL_MAGIC_MARKCHANGED)
		  printf("PDL_MAGIC_MARKCHANGED");
		else if ((*foo)->what & PDL_MAGIC_MUTATEDPARENT)
		  printf("PDL_MAGIC_MUTATEDPARENT");
		else if ((*foo)->what & PDL_MAGIC_THREADING)
		  printf("PDL_MAGIC_THREADING");
		else
		  printf("UNKNOWN");
		if ((*foo)->what & (PDL_MAGIC_DELAYED|PDL_MAGIC_UNDESTROYABLE))
		  {
		    printf(" qualifier(s): ");
		    if ((*foo)->what & PDL_MAGIC_DELAYED)
		      printf(" PDL_MAGIC_DELAYED");
		    if ((*foo)->what & PDL_MAGIC_UNDESTROYABLE)
		      printf(" PDL_MAGIC_UNDESTROYABLE");
		  }
		printf("\n");
		foo = &((*foo)->next);
	}
	return NULL;
}


int pdl__ismagic(pdl *it)
{
	return (it->magic != 0);
}

static pdl_magic **delayed=NULL;
static int ndelayed = 0;
void pdl_add_delayed_magic(pdl_magic *mag) {
	delayed = realloc(delayed,sizeof(*delayed)*++ndelayed);
	delayed[ndelayed-1] = mag;
}
void pdl_run_delayed_magic() {
	int i;
	pdl_magic **oldd = delayed; /* In case someone makes new delayed stuff */
	int nold = ndelayed;
	delayed = NULL;
	ndelayed = 0;
	for(i=0; i<nold; i++) {
		oldd[i]->vtable->cast(oldd[i]);
	}
	free(oldd);
}

/****************
 *
 * ->bind - magic
 */

void *svmagic_cast(pdl_magic *mag)
{
	pdl_magic_perlfunc *magp = (pdl_magic_perlfunc *)mag;
	dSP;
	PUSHMARK(sp);
	perl_call_sv(magp->sv, G_DISCARD | G_NOARGS);
	return NULL;
}

static pdl_magic_vtable svmagic_vtable = {
	svmagic_cast,
	NULL
};

pdl_magic *pdl_add_svmagic(pdl *it,SV *func)
{
	AV *av;
	pdl_magic_perlfunc *ptr = malloc(sizeof(pdl_magic_perlfunc));
	ptr->what = PDL_MAGIC_MARKCHANGED | PDL_MAGIC_DELAYED;
	ptr->vtable = &svmagic_vtable;
	ptr->sv = newSVsv(func);
	ptr->pdl = it;
	ptr->next = NULL;
	pdl__magic_add(it,(pdl_magic *)ptr);
	if(it->state & PDL_ANYCHANGED)
		pdl_add_delayed_magic((pdl_magic *)ptr);
/* In order to have our SV destroyed in time for the interpreter, */
/* XXX Work this out not to memleak */
	av = perl_get_av("PDL::disposable_svmagics",TRUE);
	av_push(av,ptr->sv);
	return (pdl_magic *)ptr;
}


/****************
 *
 * ->bind - magic
 */

pdl_trans *pdl_find_mutatedtrans(pdl *it)
{
	if(!it->magic) return 0;
	return pdl__call_magic(it,PDL_MAGIC_MUTATEDPARENT);
}

static void *fammutmagic_cast(pdl_magic *mag)
{
	pdl_magic_fammut *magp = (pdl_magic_fammut *)mag;
	return magp->ftr;
}

struct pdl_magic_vtable familymutmagic_vtable = {
	fammutmagic_cast,
	NULL
};

pdl_magic *pdl_add_fammutmagic(pdl *it,pdl_trans *ft)
{
	pdl_magic_fammut *ptr = malloc(sizeof(pdl_magic_fammut));
	ptr->what = PDL_MAGIC_MUTATEDPARENT;
	ptr->vtable = &familymutmagic_vtable;
	ptr->ftr = ft;
	ptr->pdl = it;
	ptr->next = NULL;
	pdl__magic_add(it,(pdl_magic *)ptr);
	return (pdl_magic *)ptr;
}

#ifdef PDL_PTHREAD

/**************
 *
 * pthreads
 *
 */

#define TVERB 0

typedef struct ptarg {
	pdl_magic_pthread *mag;
	void (*func)(pdl_trans *);
	pdl_trans *t;
	int no;
} ptarg;

int pdl_pthreads_enabled(void) {return 1;}

static void *pthread_perform(void *vp) {
	struct ptarg *p = (ptarg *)vp;
	if(TVERB) printf("STARTING THREAD %d (%d)\n",p->no, pthread_self());
	pthread_setspecific(p->mag->key,(void *)&(p->no));
	(p->func)(p->t);
	if(TVERB) printf("ENDING THREAD %d (%d)\n",p->no, pthread_self());
	return NULL;
}

int pdl_magic_thread_nthreads(pdl *it,int *nthdim) {
	pdl_magic_pthread *ptr = (pdl_magic_pthread *)pdl__find_magic(it,
					PDL_MAGIC_THREADING);
	if(!ptr) return 0;
	*nthdim = ptr->nthdim;
	return ptr->nthreads;
}

int pdl_magic_get_thread(pdl *it) { /* XXX -> only one thread can handle pdl at once */
	pdl_magic_pthread *ptr;
	int *p;
	ptr = (pdl_magic_pthread *)pdl__find_magic(it,
					PDL_MAGIC_THREADING);
	if(!ptr) {die("Invalid pdl_magic_get_thread!");}
	p = (int*)pthread_getspecific(ptr->key);
	if(!p) {
		die("Invalid pdl_magic_get_thread specific!!!!");
	}
	return *p;
}

void pdl_magic_thread_cast(pdl *it,void (*func)(pdl_trans *),pdl_trans *t) {
	pdl_magic_pthread *ptr; pthread_t *tp; ptarg *tparg;
	int i;
	ptr = (pdl_magic_pthread *)pdl__find_magic(it,
					PDL_MAGIC_THREADING);
	if(!ptr) {die("Invalid pdl_magic_thread_cast!");}
	tp = malloc(sizeof(pthread_t) * ptr->nthreads);
	tparg = malloc(sizeof(*tparg) * ptr->nthreads);
	pthread_key_create(&(ptr->key),NULL);
	if(TVERB) printf("CREATING THREADS, ME: %d, key: %d\n",pthread_self(),
		ptr->key);
	for(i=0; i<ptr->nthreads; i++) {
		tparg[i].mag = ptr;
		tparg[i].func = func;
		tparg[i].t = t;
		tparg[i].no = i;
		pthread_create(tp+i, NULL, pthread_perform, tparg+i);
	}
	if(TVERB) printf("JOINING THREADS, ME: %d, key: %d\n",pthread_self(),
		ptr->key);
	for(i=0; i<ptr->nthreads; i++) {
		pthread_join(tp[i], NULL);
	}
	if(TVERB) printf("FINISHED THREADS, ME: %d, key: %d\n",pthread_self(),
		ptr->key);
	pthread_key_delete((ptr->key));
}

void pdl_add_threading_magic(pdl *it,int nthdim,int nthreads)
{
	pdl_magic_pthread *ptr = malloc(sizeof(pdl_magic_pthread));
	ptr->what = PDL_MAGIC_THREADING;
	ptr->vtable = NULL;
	ptr->next = NULL;
	ptr->nthdim = nthdim;
	ptr->nthreads = nthreads;
	pdl__magic_add(it,(pdl_magic *)ptr);
}

#else
/* Dummy versions */
void pdl_add_threading_magic(pdl *it,int nthdim,int nthreads) {}
int pdl_magic_get_thread(pdl *it) {return 0;}
void pdl_magic_thread_cast(pdl *it,void (*func)(pdl_trans *),pdl_trans *t) {}
int pdl_magic_thread_nthreads(pdl *it,int *nthdim) {return 0;}
int pdl_pthreads_enabled() {return 0;}
#endif

/***************************
 *
 * Delete magic
 *
 */


void pdl_delete_mmapped_data(pdl *p, int param)
{
	if(!p) {return;}
	if(!p->data) {return;}
#ifdef USE_MMAP
	munmap(p->data, param);
#else
        croak("internal error: trying to delete mmaped data on unsupported platform");
#endif
	p->data = 0;
}

static void *delete_mmapped_cast(pdl_magic *mag)
{
	pdl_magic_deletedata *magp = (pdl_magic_deletedata *)mag;
	magp->func(magp->pdl, magp->param);
	return NULL;
}

struct pdl_magic_vtable deletedatamagic_vtable = {
	delete_mmapped_cast,
	NULL
};

void pdl_add_deletedata_magic(pdl *it, void (*func)(pdl *, int param), int param)
{
	pdl_magic_deletedata *ptr = malloc(sizeof(pdl_magic_deletedata));
	ptr->what = PDL_MAGIC_DELETEDATA;
	ptr->vtable = &deletedatamagic_vtable;
	ptr->pdl = it;
	ptr->func = func;
	ptr->param = param;
	pdl__magic_add(it, (pdl_magic *)ptr);
}

