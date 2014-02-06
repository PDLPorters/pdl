#define PDL_CORE      /* For certain ifdefs */
#ifndef WIN32
#define USE_MMAP
#else
#undef USE_MMAP
#endif

#include "pdlcore.h"

#ifdef USE_MMAP
#include <sys/mman.h>
#endif

/* Variable storing our the pthread ID for the main PDL thread.
 *  This is used to tell if we are in the main pthread, or in one of
 *  the pthreads spawned for PDL processing
 * This is only used when compiled with pthreads.
 */
#ifdef PDL_PTHREAD
static pthread_t pdl_main_pthreadID;
static int done_pdl_main_pthreadID_init = 0;

/* deferred error messages are stored here. We can only barf/warn from the main
 *  thread, so worker threads complain here and the complaints are printed out
 *  altogether later
 */
static char* pdl_pthread_barf_msgs     = NULL;
static int   pdl_pthread_barf_msgs_len = 0;
static char* pdl_pthread_warn_msgs     = NULL;
static int   pdl_pthread_warn_msgs_len = 0;

#endif


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
	int found = 0;
	while(*foo) {
		if(*foo == mag) {
			*foo = (*foo)->next;
			found = 1;
		}
		else{
			foo = &((*foo)->next);
		}
	}
	if( !found ){
		die("PDL:Magic not found: Internal error\n");
	}
	return;
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
	  printf("Magic %p\ttype: ",(void*)(*foo));
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
    /* FIXME: Common realloc mistake: 'delayed' nulled but not freed upon failure */
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
	/* if(TVERB) printf("STARTING THREAD %d (%d)\n",p->no, pthread_self()); */
	if(TVERB) printf("STARTING THREAD number %d\n",p->no);
	pthread_setspecific(p->mag->key,(void *)&(p->no));
	(p->func)(p->t);
	/* if(TVERB) printf("ENDING THREAD %d (%d)\n",p->no, pthread_self());   */
	if(TVERB) printf("ENDING THREAD number %d\n",p->no);
	return NULL;
}

int pdl_magic_thread_nthreads(pdl *it,int *nthdim) {
	pdl_magic_pthread *ptr = (pdl_magic_pthread *)pdl__find_magic(it, PDL_MAGIC_THREADING);
	if(!ptr) return 0;
	*nthdim = ptr->nthdim;
	return ptr->nthreads;
}

int pdl_magic_get_thread(pdl *it) { /* XXX -> only one thread can handle pdl at once */
	pdl_magic_pthread *ptr;
	int *p;
	ptr = (pdl_magic_pthread *)pdl__find_magic(it, PDL_MAGIC_THREADING);
	if(!ptr) {die("Invalid pdl_magic_get_thread!");}
	p = (int*)pthread_getspecific(ptr->key);
	if(!p) {
		die("Invalid pdl_magic_get_thread specific!!!!");
	}
	return *p;
}

void pdl_magic_thread_cast(pdl *it,void (*func)(pdl_trans *),pdl_trans *t, pdl_thread *thread) {
	pdl_magic_pthread *ptr; pthread_t *tp; ptarg *tparg;
	int i;
	int clearMagic = 0; /* Flag = 1 if we are temporarily creating pthreading magic in the
						   supplied pdl.  */
	SV * barf_msg;	  /* Deferred barf message. Using a perl SV here so it's memory can be freeed by perl
						 after it is sent to croak */
	SV * warn_msg;	  /* Similar deferred warn message. */

	ptr = (pdl_magic_pthread *)pdl__find_magic(it, PDL_MAGIC_THREADING);
	if(!ptr) {
		/* Magic doesn't exist, create it
			Probably was deleted before the transformation performed, due to
			pdl lazy evaluation.
		*/

		pdl_add_threading_magic(it, thread->mag_nth, thread->mag_nthr);
		clearMagic = 1; /* Set flag to delete magic later */

		/* Try to get magic again */
		ptr = (pdl_magic_pthread *)pdl__find_magic(it, PDL_MAGIC_THREADING);

		if(!ptr) {die("Invalid pdl_magic_thread_cast!");}

	}

	tp = malloc(sizeof(pthread_t) * thread->mag_nthr);
	tparg = malloc(sizeof(*tparg) * thread->mag_nthr);
	pthread_key_create(&(ptr->key),NULL);

	if(TVERB) printf("CREATING THREADS, ME: TBD, key: %ld\n", (unsigned long)(ptr->key));

	/* Get the pthread ID of this main thread we are in.
	 *	Any barf, warn, etc calls in the spawned pthreads can use this
	 *	to tell if its a spawned pthread
	 */
	pdl_main_pthreadID = pthread_self();   /* should do inside pthread_once() */
    done_pdl_main_pthreadID_init = 1;

    for(i=0; i<thread->mag_nthr; i++) {
        tparg[i].mag = ptr;
        tparg[i].func = func;
        tparg[i].t = t;
        tparg[i].no = i;
        if (pthread_create(tp+i, NULL, pthread_perform, tparg+i)) {
            die("Unable to create pthreads!");
        }
    }

    if(TVERB) printf("JOINING THREADS, ME: TBD, key: %ld\n", (unsigned long)(ptr->key));

	for(i=0; i<thread->mag_nthr; i++) {
		pthread_join(tp[i], NULL);
	}

	if(TVERB) printf("FINISHED THREADS, ME: TBD, key: %ld\n", (unsigned long)(ptr->key));

	pthread_key_delete((ptr->key));

	/* Remove pthread magic if we created in this function */
	if( clearMagic ){
		pdl_add_threading_magic(it, -1, -1);
	}

	/* Clean up memory allocated */
	free(tp);
	free(tparg);

	// handle any errors that may have occured in the worker threads I reset the
	// length before actually barfing/warning because barf() may not come back.
	// In that case, I'll have len==0, but an unfreed pointer. This memory will
	// be reclaimed the next time we barf/warn something (since I'm using
	// realloc). If we never barf/warn again, we'll hold onto this memory until
	// the interpreter exits. This is a one-time penalty, though so it's fine
#define handle_deferred_errors(type)							\
	do{															\
		if(pdl_pthread_##type##_msgs_len != 0)					\
		{														\
			pdl_pthread_##type##_msgs_len = 0;					\
			pdl_##type ("%s", pdl_pthread_##type##_msgs);		\
			free(pdl_pthread_##type##_msgs);					\
			pdl_pthread_##type##_msgs	  = NULL;				\
		}														\
	} while(0)

	handle_deferred_errors(warn);
	handle_deferred_errors(barf);
}

/* Function to remove threading magic (added by pdl_add_threading_magic) */
void pdl_rm_threading_magic(pdl *it)
{
	pdl_magic_pthread *ptr = (pdl_magic_pthread *)pdl__find_magic(it, PDL_MAGIC_THREADING);

	/* Don't do anything if threading magic not found */
	if( !ptr) return;

	/* Remove magic */
	pdl__magic_rm(it, (pdl_magic *) ptr);

	/* Free magic */
	free( ptr );
}

/* Function to add threading magic (i.e. identify which PDL dimension should
   be pthreaded and how many pthreads to create
   Note: If nthdim and nthreads = -1 then any pthreading magic is removed */
void pdl_add_threading_magic(pdl *it,int nthdim,int nthreads)
{
      pdl_magic_pthread *ptr;

	/* Remove threading magic if called with parms -1, -1 */
	if( (nthdim == -1) && ( nthreads == -1 ) ){
		 pdl_rm_threading_magic(it);
		 return;
	}

	ptr = malloc(sizeof(pdl_magic_pthread));
	ptr->what = PDL_MAGIC_THREADING;
	ptr->vtable = NULL;
	ptr->next = NULL;
	ptr->nthdim = nthdim;
	ptr->nthreads = nthreads;
	pdl__magic_add(it,(pdl_magic *)ptr);
}

// Barf/warn function for deferred barf message handling during pthreading We
// can't barf/warn during ptheading, because perl-level code isn't
// threadsafe. This routine does nothing if we're in the main thread (allowing
// the caller to barf normally, since there are not threading issues then). If
// we're in a worker thread, this routine stores the message for main-thread
// reporting later
int pdl_pthread_barf_or_warn(const char* pat, int iswarn, va_list *args)
{
	char** msgs;
	int*   len;

	/* Don't do anything if we are in the main pthread */
	if( !done_pdl_main_pthreadID_init || pthread_equal( pdl_main_pthreadID, pthread_self() ) )
		return 0;

	if(iswarn)
	{
		msgs = &pdl_pthread_warn_msgs;
		len	 = &pdl_pthread_warn_msgs_len;
	}
	else
	{
		msgs = &pdl_pthread_barf_msgs;
		len	 = &pdl_pthread_barf_msgs_len;
	}

	// add the new complaint to the list
	{
		static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
		pthread_mutex_lock( &mutex );
		{
			/* In the chunk I'm adding I need to store the actual data and trailing newline. */
			int extralen = vsnprintf(NULL, 0, pat, *args) + 1;

			/* 1 more for the trailing '\0'. (For windows, we first #undef realloc
			   so that the system realloc function is used instead of the PerlMem_realloc
			   macro. This currently works fine, though could conceivably require some
			   tweaking in the future if it's found to cause any problem.) */
#ifdef WIN32
#undef realloc
#endif
            /* FIXME: Common realloc mistake: 'msgs' nulled but not freed upon failure */
			*msgs = realloc(*msgs, *len + extralen + 1);
			vsnprintf( *msgs + *len, extralen + 1, pat, *args);

			/* update the length-so-far. This does NOT include the trailing '\0' */
			*len += extralen;

			/* add the newline to the end */
			(*msgs)[*len-1] = '\n';
			(*msgs)[*len  ] = '\0';
		}
		pthread_mutex_unlock( &mutex );
	}

	if(iswarn)
	{
		/* Return 1, indicating we have handled the warn messages */
		return(1);
	}

	/* Exit the current pthread. Since this was a barf call, and we should be halting execution */
	pthread_exit(NULL);
	return 0;
}


#else
/* Dummy versions */
void pdl_add_threading_magic(pdl *it,int nthdim,int nthreads) {}
int pdl_magic_get_thread(pdl *it) {return 0;}
void pdl_magic_thread_cast(pdl *it,void (*func)(pdl_trans *),pdl_trans *t, pdl_thread *thread) {}
int pdl_magic_thread_nthreads(pdl *it,int *nthdim) {return 0;}
int pdl_pthreads_enabled() {return 0;}
int pdl_pthread_barf_or_warn(const char* pat, int iswarn, va_list *args){ return 0;}
#endif

/***************************
 *
 * Delete magic
 *
 */


void pdl_delete_mmapped_data(pdl *p, Size_t param)
{
	if(!p) {return;}
	if(!p->data) {return;}
#ifdef USE_MMAP
	munmap(p->data, param);
#else
      /*  croak("internal error: trying to delete mmaped data on unsupported platform"); */
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

void pdl_add_deletedata_magic(pdl *it, void (*func)(pdl *, Size_t param), Size_t param)
{
	pdl_magic_deletedata *ptr = malloc(sizeof(pdl_magic_deletedata));
	ptr->what = PDL_MAGIC_DELETEDATA;
	ptr->vtable = &deletedatamagic_vtable;
	ptr->pdl = it;
	ptr->func = func;
	ptr->param = param;
	pdl__magic_add(it, (pdl_magic *)ptr);
}

