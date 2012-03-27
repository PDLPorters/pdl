#ifndef _pdlmagic_H_
#define _pdlmagic_H_

#define PDL_ISMAGIC(it) ((it)->magic != 0)

/* Magic stuff */

struct pdl_magic;

/* If no copy, not copied with the pdl */
typedef struct pdl_magic_vtable {
	void *(*cast)(struct pdl_magic *); /* Cast the spell */
	struct pdl_magic *(*copy)(struct pdl_magic *);
/*	void *(*cast_tr)(struct pdl_magic *,XXX);
 *	int  (*nth_tr)(struct pdl_magic *,XXX);
 */
} pdl_magic_vtable;

#define PDL_MAGIC_MARKCHANGED 0x0001
#define PDL_MAGIC_MUTATEDPARENT 0x0002
#define PDL_MAGIC_THREADING 0x0004
#define PDL_MAGIC_DELETEDATA 0x0008

#define PDL_MAGIC_UNDESTROYABLE     0x4000 /* Someone is referring to this */
				/* when magic removed, call pdl_destroy */
#define PDL_MAGIC_DELAYED     0x8000

#define PDL_MAGICSTART \
		int what; /* when is this magic to be called */ \
		pdl_magic_vtable *vtable; \
		struct pdl_magic *next; \
		pdl *pdl

#define PDL_TRMAGICSTART \
		int what; /* when is this magic to be called */ \
		pdl_magic_vtable *vtable; \
		struct pdl_magic *next; \
		pdl_trans *tr

typedef struct pdl_magic {
	PDL_MAGICSTART;
} pdl_magic;

typedef struct pdl_magic_perlfunc {
	PDL_MAGICSTART;
	SV *sv;         	/* sub{} or subname (perl_call_sv) */
} pdl_magic_perlfunc;

typedef struct pdl_magic_fammut {
	PDL_MAGICSTART;
	pdl_trans *ftr;
} pdl_magic_fammut;

typedef struct pdl_magic_changetrans {
	PDL_MAGICSTART;
	pdl_trans *tr;
} pdl_magic_changetrans;

typedef struct pdl_magic_deletedata {
	PDL_MAGICSTART;
	void (*func)(pdl *p, Size_t param);
	Size_t param;
} pdl_magic_deletedata;

/* #define PDL_PTHREAD */
/* Defined by MakeMaker */
#ifdef PDL_PTHREAD

/* This is a workaround to a perl CORE "feature" where they define a
 * macro PTHREAD_CREATE_JOINABLE with the same name as POSIX threads
 * which works as long as the implementation of POSIX threads also
 * uses macros.  As is, the use of the same name space breaks for
 * win32 pthreads where the identifiers are enums and not #defines
 */
#ifdef PTHREAD_CREATE_JOINABLE
#undef  PTHREAD_CREATE_JOINABLE
#endif

#include <pthread.h>

typedef struct pdl_magic_pthread {
	PDL_MAGICSTART;
	int nthdim;
	int nthreads;
	pthread_key_t key;
} pdl_magic_pthread;
#endif

/* - tr magics */

typedef struct pdl_trmagic {
	PDL_TRMAGICSTART;
} pdl_trmagic;

typedef struct pdl_trmagic_family {
	PDL_TRMAGICSTART;
	pdl *fprog,*tprog;
	pdl *fmut,*tmut;
} pdl_trmagic_family;

/* __ = Don't call from outside pdl if you don't know what you're doing */

void pdl__magic_add(pdl *,pdl_magic *);
void pdl__magic_rm(pdl *,pdl_magic *);
void pdl__magic_free(pdl *);

int pdl__magic_isundestroyable(pdl *);

void *pdl__call_magic(pdl *,int which);
int pdl__ismagic(pdl *);

pdl_magic *pdl__print_magic(pdl *it);

pdl_magic *pdl_add_svmagic(pdl *,SV *);

/* A kind of "dowhenidle" system */

void pdl_add_delayed_magic(pdl_magic *);
void pdl_run_delayed_magic();

pdl_trans *pdl_find_mutatedtrans(pdl *it);

/* Threading magic */

/* Deferred barfing and warning when pthreading  */
int pdl_pthread_barf_or_warn(const char* pat, int iswarn, va_list *args);

void pdl_add_threading_magic(pdl *,int nthdim,int nthreads);

int pdl_magic_thread_nthreads(pdl *,int *nthdim);
int pdl_magic_get_thread(pdl *); /* XXX -> only one thread can handle pdl at once */

void pdl_magic_thread_cast(pdl *,void (*func)(pdl_trans *),pdl_trans *t, pdl_thread *thread);
int pdl_pthreads_enabled(void);

/* Delete data magic */
void pdl_delete_mmapped_data(pdl *p, Size_t param) ;
void pdl_add_deletedata_magic(pdl *it,void (*func)(pdl *, Size_t param), Size_t param);

#endif /* _pdlmagic_H_  */
