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
static pthread_t pdl_main_pthreadID = 0;

/* Pthread barf variables: 
 *  These are setup so any barf messags that happen when pthreading are saved/deferred
 *  until the pthread joins the main thread.
 */
pthread_mutex_t pthread_barf_mutex = PTHREAD_MUTEX_INITIALIZER; /* ensure safe access */
char ** pdl_pthread_barf_msgs = NULL;  /* Array of deferred barf messages  */
int pdl_pthread_barf_nmsg = 0;         /* Number of deferred barf messages */



pthread_mutex_t pthread_warn_mutex = PTHREAD_MUTEX_INITIALIZER;
char ** pdl_pthread_warn_msgs = NULL;  /* Array of deferred warn messages  */
int pdl_pthread_warn_nmsg = 0;         /* Number of deferred warn messages */


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

void pdl_magic_thread_cast(pdl *it,void (*func)(pdl_trans *),pdl_trans *t, pdl_thread *thread) {
	pdl_magic_pthread *ptr; pthread_t *tp; ptarg *tparg;
	int i;
	int clearMagic = 0; /* Flag = 1 if we are temporarily creating pthreading magic in the
	                       supplied pdl.  */
	SV * barf_msg;    /* Deferred barf message. Using a perl SV here so it's memory can be freeed by perl
	                     after it is sent to croak */
	SV * warn_msg;    /* Similar deferred warn message. */
					  
	ptr = (pdl_magic_pthread *)pdl__find_magic(it,
					PDL_MAGIC_THREADING);
	if(!ptr) {
		/* Magic doesn't exist, create it
		    Probably was deleted before the transformation performed, due to
		    pdl lazy evaluation.
		*/
		
		pdl_add_threading_magic(it, thread->mag_nth, thread->mag_nthr);
		clearMagic = 1; /* Set flag to delete magic later */
		
		/* Try to get magic again */
		ptr = (pdl_magic_pthread *)pdl__find_magic(it,
						PDL_MAGIC_THREADING);
						
		if(!ptr) {die("Invalid pdl_magic_thread_cast!");}
				
	}
	
	tp = malloc(sizeof(pthread_t) * thread->mag_nthr);
	tparg = malloc(sizeof(*tparg) * thread->mag_nthr);
	pthread_key_create(&(ptr->key),NULL);
	if(TVERB) printf("CREATING THREADS, ME: %d, key: %d\n",pthread_self(),
		ptr->key);
		
	/* Get the pthread ID of this main thread we are in. 
	 *  Any barf, warn, etc calls in the spawned pthreads can use this
	 *  to tell if its a spawned pthread
	 */
	pdl_main_pthreadID = pthread_self();
	
	
	for(i=0; i<thread->mag_nthr; i++) {
		tparg[i].mag = ptr;
		tparg[i].func = func;
		tparg[i].t = t;
		tparg[i].no = i;
		pthread_create(tp+i, NULL, pthread_perform, tparg+i);
	}
	if(TVERB) printf("JOINING THREADS, ME: %d, key: %d\n",pthread_self(),
		ptr->key);
	for(i=0; i<thread->mag_nthr; i++) {
		pthread_join(tp[i], NULL);
	}
	if(TVERB) printf("FINISHED THREADS, ME: %d, key: %d\n",pthread_self(),
		ptr->key);
	pthread_key_delete((ptr->key));
	
	/* Remove pthread magic if we created in this function */
	if( clearMagic ){
		pdl_add_threading_magic(it, -1, -1);
	}
	
	/* Clean up memory allocated */
	free(tp);
	free(tparg);
	
	/* Check for deferred barf calls */
	/*   We don't need to use the mutexes to access the global variables, because we
	     are in the main pthread here */
	if( pdl_pthread_barf_nmsg ){
		int msgSize = 0;
		int i;
		
		barf_msg = newSVpv("", 0);
		
		/* Join messages together */
		for( i = 0; i < pdl_pthread_barf_nmsg; i++){
			sv_catpv( barf_msg,  pdl_pthread_barf_msgs[i] );
			
			/* Separate messages by newline, if not one there already and message length > 0 */
			if( ( i < (pdl_pthread_barf_nmsg -1 ) ) &&
			      strlen( pdl_pthread_barf_msgs[i] ) > 0 ){ 
				char * lastChar;
				lastChar = SvEND(barf_msg) - 1;
				if( ! ( *lastChar == '\n') ) {
					sv_catpv( barf_msg,  "\n" );
				}
			}
				
		}
		
		/* Cleanup the pthread variables */
		for( i = 0; i < pdl_pthread_barf_nmsg; i++){
			free( pdl_pthread_barf_msgs[i] );
		}
		free(pdl_pthread_barf_msgs);
		pdl_pthread_barf_msgs = NULL;
		pdl_pthread_barf_nmsg = 0;
		
		/* Now barf with the deferred messages */
		pdl_barf(SvPVX(barf_msg));
	}

	/* Check for deferred warn calls */
	/*   We don't need to use the mutexes to access the global variables, because we
	     are in the main pthread here */
	if( pdl_pthread_warn_nmsg ){
		int msgSize = 0;
		int i;
		
		warn_msg = newSVpv("", 0);
		
		/* Join messages together */
		for( i = 0; i < pdl_pthread_warn_nmsg; i++){
			//printf("Joining message '%s'\n", pdl_pthread_warn_msgs[i]);
			sv_catpv( warn_msg,  pdl_pthread_warn_msgs[i] );

			/* Separate messages by newline, if not one there already and message length > 0 */
			if( ( i < (pdl_pthread_warn_nmsg -1 ) ) &&
			      strlen( pdl_pthread_warn_msgs[i] ) > 0 ){ 
				char * lastChar;
				lastChar = SvEND(warn_msg) - 1;
				if( ! ( *lastChar == '\n') ) {
					sv_catpv( warn_msg,  "\n" );
				}
			}
		}
		
		/* Cleanup the pthread variables */
		for( i = 0; i < pdl_pthread_warn_nmsg; i++){
			free( pdl_pthread_warn_msgs[i] );
		}
		free(pdl_pthread_warn_msgs);
		pdl_pthread_warn_msgs = NULL;
		pdl_pthread_warn_nmsg = 0;
		
		/* Now barf with the deferred messages */
		pdl_warn(SvPVX(warn_msg));
	}
		
}

/* Function to remove threading magic (added by pdl_add_threading_magic) */
void pdl_rm_threading_magic(pdl *it)
{
	pdl_magic_pthread *ptr = (pdl_magic_pthread *)pdl__find_magic(it,
					PDL_MAGIC_THREADING);
					
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

	/* Remove threading magic if called with parms -1, -1 */
	if( (nthdim == -1) && ( nthreads == -1 ) ){
		 pdl_rm_threading_magic(it);
		 return;
	}
		
	pdl_magic_pthread *ptr = malloc(sizeof(pdl_magic_pthread));
	ptr->what = PDL_MAGIC_THREADING;
	ptr->vtable = NULL;
	ptr->next = NULL;
	ptr->nthdim = nthdim;
	ptr->nthreads = nthreads;
	pdl__magic_add(it,(pdl_magic *)ptr);
}

/* Barf function for deferred barf message handling during pthreading */
/*  We can't barf/croak during ptheading, because the normal perl barf isn't
    threadsafe. So barf messages are stored for handling by the main pthread,
    and the pthread is exited */
void pdl_pthread_barf(const char* pat, va_list *args){

	/* Temporary place to put barf messages */
	static char tempBarfMsg[MAX_PDL_PTHREAD_MSG_SIZE + 1]; 
	int msgLen;
	
         //printf("Mainpthread = %x, Current pthread = %x\n", pdl_main_pthreadID, pthread_self());
	/* Don't do anything if we are in the main pthread */
	if( !pdl_main_pthreadID || pthread_equal( pdl_main_pthreadID, pthread_self() ) ){
		//printf("In Main pthread, doing nothing\n");
		return;
	}
	
	//printf("Barf in pthread processing, deferring messages\n");
	
	/* Get message */
	vsnprintf( tempBarfMsg, MAX_PDL_PTHREAD_MSG_SIZE, pat, *args);
	
	msgLen = strnlen( tempBarfMsg, MAX_PDL_PTHREAD_MSG_SIZE);
	
	/* Store the new message on the pthread-global arrays (using mutexes) */
	pthread_mutex_lock( &pthread_barf_mutex );	
	
	pdl_pthread_barf_nmsg++;
	pdl_pthread_barf_msgs = (char **) realloc( pdl_pthread_barf_msgs, pdl_pthread_barf_nmsg * sizeof( char *) );
	pdl_pthread_barf_msgs[pdl_pthread_barf_nmsg - 1] = (char *) malloc( (msgLen + 1) * sizeof( char ) );
	
	strncpy( pdl_pthread_barf_msgs[pdl_pthread_barf_nmsg - 1], tempBarfMsg, msgLen + 1 );
	

	/* Release the mutex */
	pthread_mutex_unlock( &pthread_barf_mutex );
	
	/* Exit the current pthread. Since this was a barf call, and we should be halting execution */
	pthread_exit(NULL);
}	

/* Warn function for deferred warn message handling during pthreading */
/*  We can't call warn during ptheading, because the normal perl warn isn't
    threadsafe. So warn messages are stored for handling by the main pthread,
    
    This function returns 1 if it handles / stores the warning message, 0 otherwise
    
    */
int pdl_pthread_warn(const char* pat, va_list *args){

	/* Temporary place to put warn messages */
	static char tempWarnMsg[MAX_PDL_PTHREAD_MSG_SIZE + 1]; 
	int msgLen;
	
         // printf("Mainpthread = %x, Current pthread = %x\n", pdl_main_pthreadID, pthread_self());
	/* Don't do anything if we are in the main pthread */
	if( !pdl_main_pthreadID || pthread_equal( pdl_main_pthreadID, pthread_self() ) ){
		// printf("In Main pthread, doing nothing\n");
		return(0);
	}
	
	// printf("Warn '%s' in pthread processing, deferring messages\n", pat);
	
	/* Get message */
	vsnprintf( tempWarnMsg, MAX_PDL_PTHREAD_MSG_SIZE, pat, *args);
	
	msgLen = strnlen( tempWarnMsg, MAX_PDL_PTHREAD_MSG_SIZE);
	
	/* Store the new message on the pthread-global arrays (using mutexes) */
	pthread_mutex_lock( &pthread_warn_mutex );	
	
	pdl_pthread_warn_nmsg++;
	pdl_pthread_warn_msgs = (char **) realloc( pdl_pthread_warn_msgs, pdl_pthread_warn_nmsg * sizeof( char *) );
	pdl_pthread_warn_msgs[pdl_pthread_warn_nmsg - 1] = (char *) malloc( (msgLen + 1) * sizeof( char ) );
	
	strncpy( pdl_pthread_warn_msgs[pdl_pthread_warn_nmsg - 1], tempWarnMsg, msgLen+1 );
	

	/* Release the mutex */
	pthread_mutex_unlock( &pthread_warn_mutex );
	
	/* Return 1, indicating we have handled the warn messages */
	return(1);
}	
	

#else
/* Dummy versions */
void pdl_add_threading_magic(pdl *it,int nthdim,int nthreads) {}
int pdl_magic_get_thread(pdl *it) {return 0;}
void pdl_magic_thread_cast(pdl *it,void (*func)(pdl_trans *),pdl_trans *t, pdl_thread *thread) {}
int pdl_magic_thread_nthreads(pdl *it,int *nthdim) {return 0;}
int pdl_pthreads_enabled() {return 0;}
void pdl_pthread_barf(const char* pat, va_list *args){ return 0;};
int pdl_pthread_warn(const char* pat, va_list *args){ return 0;};
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

