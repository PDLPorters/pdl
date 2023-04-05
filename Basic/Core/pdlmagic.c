#include "pdlcore.h"

/* Variable storing the pthread ID for the main PDL thread.
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
static size_t pdl_pthread_barf_msgs_len = 0;
static char* pdl_pthread_warn_msgs     = NULL;
static size_t pdl_pthread_warn_msgs_len = 0;

#endif


/* Singly linked list */
/* Note that this zeroes ->next!) */

void pdl__magic_add(pdl *it,pdl_magic *mag)
{
        pdl_magic **foo = (pdl_magic **)(&(it->magic));
	while(*foo) {
		foo = &((*foo)->next);
	}
	(*foo) = mag;
	mag->next = NULL;
}

pdl_error pdl__magic_rm(pdl *it,pdl_magic *mag)
{
        pdl_error PDL_err = {0, NULL, 0};
        pdl_magic **foo = (pdl_magic **)(&(it->magic));
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
		return pdl_make_error_simple(PDL_EUSERERROR, "PDL:Magic not found: Internal error\n");
	}
	return PDL_err;
}

void pdl__magic_free(pdl *it)
{
  if (pdl__ismagic(it) && !pdl__magic_isundestroyable(it)) {
    pdl_magic *foo = (pdl_magic *)(it->magic);
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
        pdl_magic **foo = (pdl_magic **)(&(it->magic));
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
	pdl_magic **foo = (pdl_magic **)(&(it->magic));
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
        pdl_magic **foo = (pdl_magic **)(&(it->magic));
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
        pdl_magic **foo = (pdl_magic **)(&(it->magic));
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
static PDL_Indx ndelayed = 0;
void pdl_add_delayed_magic(pdl_magic *mag) {
    /* FIXME: Common realloc mistake: 'delayed' nulled but not freed upon failure */
	delayed = realloc(delayed,sizeof(*delayed)*++ndelayed);
	delayed[ndelayed-1] = mag;
}
void pdl_run_delayed_magic() {
	PDL_Indx i;
	pdl_magic **oldd = delayed; /* In case someone makes new delayed stuff */
	PDL_Indx nold = ndelayed;
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
	ENTER; SAVETMPS;
	PUSHMARK(sp);
	perl_call_sv(magp->sv, G_DISCARD | G_NOARGS);
	FREETMPS; LEAVE;
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
	if (!ptr) return NULL;
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
	if (!ptr) return NULL;
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

typedef struct ptarg {
	pdl_magic_pthread *mag;
	pdl_error (*func)(pdl_trans *);
	pdl_trans *t;
	int no;
	pdl_error error_return;
} ptarg;

int pdl_pthreads_enabled(void) {return 1;}


static void *pthread_perform(void *vp) {
	struct ptarg *p = (ptarg *)vp;
	PDLDEBUG_f(printf("STARTING THREAD %d (%lu)\n",p->no, (long unsigned)pthread_self()));
	pthread_setspecific(p->mag->key,(void *)&(p->no));
	p->error_return = (p->func)(p->t);
	PDLDEBUG_f(printf("ENDING THREAD %d (%lu)\n",p->no, (long unsigned)pthread_self()));
	return NULL;
}

int pdl_magic_thread_nthreads(pdl *it,PDL_Indx *nthdim) {
	pdl_magic_pthread *ptr = (pdl_magic_pthread *)pdl__find_magic(it, PDL_MAGIC_THREADING);
	if(!ptr) return 0;
	*nthdim = ptr->nthdim;
	return ptr->nthreads;
}

int pdl_magic_get_thread(pdl *it) {
	pdl_magic_pthread *ptr = (pdl_magic_pthread *)pdl__find_magic(it, PDL_MAGIC_THREADING);
	if(!ptr) return -1;
	int *p = (int*)pthread_getspecific(ptr->key);
	if(!p) return -1;
	return *p;
}

pdl_error pdl_magic_thread_cast(pdl *it,pdl_error (*func)(pdl_trans *),pdl_trans *t, pdl_broadcast *broadcast) {
	pdl_error PDL_err = {0, NULL, 0};
	PDL_Indx i;
	int clearMagic = 0; /* Flag = 1 if we are temporarily creating pthreading magic in the
						   supplied pdl.  */
	pdl_magic_pthread *ptr = (pdl_magic_pthread *)pdl__find_magic(it, PDL_MAGIC_THREADING);
	if(!ptr) {
		/* Magic doesn't exist, create it
			Probably was deleted before the transformation performed, due to
			pdl lazy evaluation.
		*/

		PDL_RETERROR(PDL_err, pdl_add_threading_magic(it, broadcast->mag_nth, broadcast->mag_nthr));
		clearMagic = 1; /* Set flag to delete magic later */

		/* Try to get magic again */
		ptr = (pdl_magic_pthread *)pdl__find_magic(it, PDL_MAGIC_THREADING);

		if(!ptr) {return pdl_make_error_simple(PDL_EFATAL, "Invalid pdl_magic_thread_cast!");}

	}

	pthread_t tp[broadcast->mag_nthr];
	ptarg tparg[broadcast->mag_nthr];
	pthread_key_create(&(ptr->key),NULL);
	/* Get the pthread ID of this main thread we are in.
	 *	Any barf, warn, etc calls in the spawned pthreads can use this
	 *	to tell if its a spawned pthread
	 */
	pdl_main_pthreadID = pthread_self();
	done_pdl_main_pthreadID_init = 1;

	PDLDEBUG_f(printf("CREATING THREADS, ME: TBD, key: %ld\n", (unsigned long)(ptr->key)));
	for(i=0; i<broadcast->mag_nthr; i++) {
	    tparg[i].mag = ptr;
	    tparg[i].func = func;
	    tparg[i].t = t;
	    tparg[i].no = i;
	    tparg[i].error_return = PDL_err;
	    if (pthread_create(tp+i, NULL, pthread_perform, tparg+i)) {
		return pdl_make_error_simple(PDL_EFATAL, "Unable to create pthreads!");
	    }
	}

	PDLDEBUG_f(printf("JOINING THREADS, ME: TBD, key: %ld\n", (unsigned long)(ptr->key)));
	for(i=0; i<broadcast->mag_nthr; i++) {
		pthread_join(tp[i], NULL);
	}
	PDLDEBUG_f(printf("FINISHED THREADS, ME: TBD, key: %ld\n", (unsigned long)(ptr->key)));

	pthread_key_delete((ptr->key));
	done_pdl_main_pthreadID_init = 0;

	/* Remove pthread magic if we created in this function */
	if( clearMagic ){
		PDL_RETERROR(PDL_err, pdl_add_threading_magic(it, -1, -1));
	}

#define handle_deferred_errors(type, action)							\
	do{															\
		if(pdl_pthread_##type##_msgs_len != 0)					\
		{														\
			pdl_pthread_##type##_msgs_len = 0;					\
			action;	\
			free(pdl_pthread_##type##_msgs);					\
			pdl_pthread_##type##_msgs	  = NULL;				\
		}														\
	} while(0)

	handle_deferred_errors(warn, pdl_pdl_warn("%s", pdl_pthread_warn_msgs));
	handle_deferred_errors(barf, PDL_err = pdl_error_accumulate(PDL_err, pdl_make_error(PDL_EFATAL, "%s", pdl_pthread_barf_msgs)));
	for(i=0; i<broadcast->mag_nthr; i++) {
	    PDL_err = pdl_error_accumulate(PDL_err, tparg[i].error_return);
	}
	return PDL_err;
}

/* Function to remove threading magic (added by pdl_add_threading_magic) */
pdl_error pdl_rm_threading_magic(pdl *it)
{
	pdl_error PDL_err = {0, NULL, 0};
	pdl_magic_pthread *ptr = (pdl_magic_pthread *)pdl__find_magic(it, PDL_MAGIC_THREADING);
	/* Don't do anything if threading magic not found */
	if( !ptr) return PDL_err;
	/* Remove magic */
	PDL_RETERROR(PDL_err, pdl__magic_rm(it, (pdl_magic *) ptr));
	/* Free magic */
	free( ptr );
	return PDL_err;
}

/* Function to add threading magic (i.e. identify which PDL dimension should
   be pthreaded and how many pthreads to create
   Note: If nthdim and nthreads = -1 then any pthreading magic is removed */
pdl_error pdl_add_threading_magic(pdl *it,PDL_Indx nthdim,PDL_Indx nthreads)
{
	pdl_error PDL_err = {0, NULL, 0};
	pdl_magic_pthread *ptr;
	/* Remove threading magic if called with parms -1, -1 */
	if( (nthdim == -1) && ( nthreads == -1 ) ){
		 PDL_RETERROR(PDL_err, pdl_rm_threading_magic(it));
		 return PDL_err;
	}
	ptr = malloc(sizeof(pdl_magic_pthread));
	if (!ptr) return pdl_make_error_simple(PDL_EFATAL, "Out of memory");
	ptr->what = PDL_MAGIC_THREADING;
	ptr->vtable = NULL;
	ptr->next = NULL;
	ptr->nthdim = nthdim;
	ptr->nthreads = nthreads;
	pdl__magic_add(it,(pdl_magic *)ptr);
	return PDL_err;
}

char pdl_pthread_main_thread() {
  return !done_pdl_main_pthreadID_init || pthread_equal( pdl_main_pthreadID, pthread_self() );
}

// Barf/warn function for deferred barf message handling during pthreading We
// can't barf/warn during pthreading, because perl-level code isn't
// threadsafe. This routine does nothing if we're in the main thread (allowing
// the caller to barf normally, since there are not threading issues then). If
// we're in a worker thread, this routine stores the message for main-thread
// reporting later
int pdl_pthread_barf_or_warn(const char* pat, int iswarn, va_list *args)
{
	char** msgs;
	size_t* len;

	/* Don't do anything if we are in the main pthread */
	if (pdl_pthread_main_thread()) return 0;

	if(iswarn)
	{
		msgs = &pdl_pthread_warn_msgs;
		len = &pdl_pthread_warn_msgs_len;
	}
	else
	{
		msgs = &pdl_pthread_barf_msgs;
		len = &pdl_pthread_barf_msgs_len;
	}

	size_t extralen = vsnprintf(NULL, 0, pat, *args);
	// add the new complaint to the list
	pdl_pthread_realloc_vsnprintf(msgs, len, extralen, pat, args, 1);

	if(iswarn)
	{
		/* Return 1, indicating we have handled the warn messages */
		return(1);
	}

	/* Exit the current pthread. Since this was a barf call, and we should be halting execution */
	pthread_exit(NULL);
	return 0;
}

void pdl_pthread_realloc_vsnprintf(char **p, size_t *len, size_t extralen, const char *pat, va_list *args, char add_newline) {
  static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
  pthread_mutex_lock( &mutex );
  /* (For windows, we first #undef realloc
     so that the system realloc function is used instead of the PerlMem_realloc
     macro. This currently works fine, though could conceivably require some
     tweaking in the future if it's found to cause any problem.) */
#ifdef WIN32
#undef realloc
#endif
  if (add_newline) extralen += 1;
  extralen += 1; /* +1 for '\0' at end */
  *p = realloc(*p, *len + extralen);
  vsnprintf(*p + *len, extralen, pat, *args);
  *len += extralen; /* update the length-so-far, includes '\0' */
  if (add_newline) (*p)[*len-2] = '\n';
  (*p)[*len-1] = '\0';
  pthread_mutex_unlock( &mutex );
}

void pdl_pthread_free(void *p) {
#ifdef WIN32 /* same reasons as above */
#undef free
#endif
  static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
  pthread_mutex_lock( &mutex );
  free(p);
  pthread_mutex_unlock( &mutex );
}

/* copied from git@github.com:git/git.git 2.34-ish thread-util.c */
/* changed GIT_WINDOWS_NATIVE to WIN32 */
#if defined(hpux) || defined(__hpux) || defined(_hpux)
#  include <sys/pstat.h>
#endif
/*
 * By doing this in two steps we can at least get
 * the function to be somewhat coherent, even
 * with this disgusting nest of #ifdefs.
 */
#ifndef _SC_NPROCESSORS_ONLN
#  ifdef _SC_NPROC_ONLN
#    define _SC_NPROCESSORS_ONLN _SC_NPROC_ONLN
#  elif defined _SC_CRAY_NCPU
#    define _SC_NPROCESSORS_ONLN _SC_CRAY_NCPU
#  endif
#endif
int pdl_online_cpus(void)
{
#ifdef WIN32
	SYSTEM_INFO info;
	GetSystemInfo(&info);
	if ((int)info.dwNumberOfProcessors > 0)
		return (int)info.dwNumberOfProcessors;
#elif defined(hpux) || defined(__hpux) || defined(_hpux)
	struct pst_dynamic psd;
	if (!pstat_getdynamic(&psd, sizeof(psd), (size_t)1, 0))
		return (int)psd.psd_proc_cnt;
#elif defined(HAVE_BSD_SYSCTL) && defined(HW_NCPU)
	int mib[2];
	size_t len;
	int cpucount;
	mib[0] = CTL_HW;
#  ifdef HW_AVAILCPU
	mib[1] = HW_AVAILCPU;
	len = sizeof(cpucount);
	if (!sysctl(mib, 2, &cpucount, &len, NULL, 0))
		return cpucount;
#  endif /* HW_AVAILCPU */
	mib[1] = HW_NCPU;
	len = sizeof(cpucount);
	if (!sysctl(mib, 2, &cpucount, &len, NULL, 0))
		return cpucount;
#endif /* defined(HAVE_BSD_SYSCTL) && defined(HW_NCPU) */
#ifdef _SC_NPROCESSORS_ONLN
	long ncpus;
	if ((ncpus = (long)sysconf(_SC_NPROCESSORS_ONLN)) > 0)
		return (int)ncpus;
#endif
	return 1;
}

#else
/* Dummy versions */
pdl_error pdl_add_threading_magic(pdl *it,PDL_Indx nthdim,PDL_Indx nthreads) {pdl_error PDL_err = {0,NULL,0}; return PDL_err;}
char pdl_pthread_main_thread() { return 1; }
int pdl_magic_get_thread(pdl *it) {return 0;}
pdl_error pdl_magic_thread_cast(pdl *it,pdl_error (*func)(pdl_trans *),pdl_trans *t, pdl_broadcast *broadcast) {pdl_error PDL_err = {0,NULL,0}; return PDL_err;}
int pdl_magic_thread_nthreads(pdl *it,PDL_Indx *nthdim) {return 0;}
int pdl_pthreads_enabled() {return 0;}
int pdl_pthread_barf_or_warn(const char* pat, int iswarn, va_list *args){ return 0;}
int pdl_online_cpus() {return 1;}
#endif

/***************************
 *
 * Delete magic
 *
 */

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

pdl_error pdl_add_deletedata_magic(pdl *it, void (*func)(pdl *, Size_t param), Size_t param)
{
	pdl_error PDL_err = {0, NULL, 0};
	pdl_magic_deletedata *ptr = malloc(sizeof(pdl_magic_deletedata));
	if (!ptr) return pdl_make_error_simple(PDL_EFATAL, "Out of memory");
	ptr->what = PDL_MAGIC_DELETEDATA;
	ptr->vtable = &deletedatamagic_vtable;
	ptr->pdl = it;
	ptr->func = func;
	ptr->param = param;
	pdl__magic_add(it, (pdl_magic *)ptr);
	return PDL_err;
}
