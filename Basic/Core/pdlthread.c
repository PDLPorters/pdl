/* XXX NOTE THAT IT IS NOT SAFE TO USE ->pdls MEMBER OUTSIDE
   INITTHREADSTRUCT! */

#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */


#define MAX2(a,b) if((b)>(a)) a=b;

#define strndup strndup_mine

static void *strndup(void *ptr, int size) {
	if(size == 0) return 0; else
	{
	void *newptr = malloc(size);
	int i;
	for(i=0; i<size; i++) ((char *)newptr)[i] = ((char *)ptr)[i];
	return newptr;
	}
}

static void print_iarr(int *iarr, int n) {
  int i;
  printf("(");
  for (i=0;i<n;i++)
    printf("%s%d",(i?" ":""),iarr[i]);
  printf(")");
}

#define psp printf("%s",spaces)
void dump_thread(pdl_thread *thread) {
  int i;
  char spaces[] = "    ";
  printf("DUMPTHREAD 0x%x \n",thread);
  if (0&& thread->einfo) {
    psp; printf("Funcname: %s\n",thread->einfo->funcname);
    psp; printf("Paramaters: ");
    for (i=0;i<thread->einfo->nparamnames;i++)
      printf("%s ",thread->einfo->paramnames[i]);
    printf("\n");
  }
  psp; printf("Flags: %d, Ndims: %d, Nimplicit: %d, Npdls: %d, Nextra: %d\n",
	 thread->gflags,thread->ndims,thread->nimpl,thread->npdls,thread->nextra);

  psp; printf("Dims: "); print_iarr(thread->dims,thread->ndims); printf("\n");
  psp; printf("Inds: "); print_iarr(thread->inds,thread->ndims); printf("\n");
  psp; printf("Offs: "); print_iarr(thread->offs,thread->npdls); printf("\n");
  psp; printf("Incs: "); print_iarr(thread->incs,thread->ndims); printf("\n");
  psp; printf("Realdims: "); print_iarr(thread->realdims,thread->npdls); printf("\n");
  psp; printf("Pdls: (");
  for (i=0;i<thread->npdls;i++)
    printf("%s0x%x",(i?" ":""),thread->pdls[i]);
  printf(")\n");
  psp; printf("Per pdl flags: (");
  for (i=0;i<thread->npdls;i++)
    printf("%s%d",(i?" ":""),thread->flags[i]);
  printf(")\n");
}

int *pdl_get_threadoffsp(pdl_thread *thread)
{
  if(thread->gflags & PDL_THREAD_MAGICKED) {
  	int thr = pdl_magic_get_thread(thread->pdls[thread->mag_nthpdl]);
	return thread->offs + thr * thread->npdls;
  }
/* The non-multithreaded case: return just the usual offsets */
  return thread->offs;
}

int *pdl_get_threadoffsp_int(pdl_thread *thread, int *nthr)
{
  if(thread->gflags & PDL_THREAD_MAGICKED) {
  	int thr = pdl_magic_get_thread(thread->pdls[thread->mag_nthpdl]);
	*nthr = thr;
	return thread->offs + thr * thread->npdls;
  }
  *nthr = 0;
/* The non-multithreaded case: return just the usual offsets */
  return thread->offs;
}

void pdl_thread_copy(pdl_thread *from,pdl_thread *to) {
#ifdef PDL_THREAD_DEBUG
	to->magicno = from->magicno;
#endif
	to->gflags = from->gflags;
	to->einfo = from->einfo;
	to->ndims = from->ndims;
	to->nimpl = from->nimpl;
	to->npdls = from->npdls;
	to->inds = strndup(from->inds,sizeof(*to->inds)*to->ndims);
	to->dims = strndup(from->dims,sizeof(*to->dims)*to->ndims);
	to->offs = strndup(from->offs,sizeof(*to->offs)*to->npdls);
	to->incs = strndup(from->incs,sizeof(*to->offs)*to->npdls*to->ndims);
	to->realdims = from->realdims;
	to->flags = strndup(from->flags,to->npdls);
	to->pdls = strndup(from->pdls,sizeof(*to->pdls)*to->npdls); /* XX MEMLEAK */
	to->mag_nthpdl = from->mag_nth;
	to->mag_nthpdl = from->mag_nthpdl;
}

void pdl_freethreadloop(pdl_thread *thread) {
	PDLDEBUG_f(printf("Freethreadloop(0x%x, 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x)\n",
		thread,
		thread->inds, thread->dims, thread->offs, thread->incs,
		thread->flags, thread->pdls);)
	if(!thread->inds) {return;}
	free(thread->inds);
	free(thread->dims);
	free(thread->offs);
	free(thread->incs);
/*	free(thread->realdims); */
	free(thread->flags);
	free(thread->pdls);
	pdl_clearthreadstruct(thread);
}

void pdl_clearthreadstruct(pdl_thread *it) {
	PDLDEBUG_f(printf("Clearthreadloop(0x%x)\n", it);)
	it->einfo = 0;it->inds = 0;it->dims = 0;
	it->ndims = it->nimpl = it->npdls = 0; it->offs = 0;
	it->pdls = 0;it->incs = 0; it->realdims=0; it->flags=0;
	it->gflags=0; /* unsets PDL_THREAD_INITIALIZED among others */
#ifdef PDL_THREAD_DEBUG
	PDL_THR_CLRMAGIC(it);
#endif
}

/* The assumptions this function makes:
 *  pdls is dynamic and may go away -> copied
 *  realdims is static and is NOT copied and NOT freed!!!
 *  creating is only used inside this routine.
 *  errorinfo is assumed static.
 *  usevaffine is assumed static. (uses if exists)
 *
 * Only the first thread-magicked pdl is taken into account.
 */
void pdl_initthreadstruct(int nobl,
	pdl **pdls,int *realdims,int *creating,int npdls,
	pdl_errorinfo *info,pdl_thread *thread, char *flags) {
	int i; int j;
	int ndims=0; int nth;
	int mx;
	int nids;
	int nimpl;
	int nthid;

	int mydim;

	int *nthreadids;
	int nthr = 0; int nthrd;

	PDLDEBUG_f(printf("Initthreadloop(0x%x)\n", thread);)
#ifdef PDL_THREAD_DEBUG
	  /* the following is a fix for a problem in the current core logic
           * see comments in pdl_make_physical in pdlapi.c
           * the if clause detects if this thread has previously been initialized
           * if yes free the stuff that was allocated in the last run
           * just returning is not! good enough (I tried it)
           * CS
           */
	if (thread->magicno == PDL_THR_MAGICNO && 
	    thread->gflags & PDL_THREAD_INITIALIZED) {
	  PDLDEBUG_f(printf("REINITIALIZING already initialized thread\n");)	
	  PDLDEBUG_f(dump_thread(thread);)	
	  /* return; */ /* try again, should (!?) work */
	  if (thread->inds) free(thread->inds);
	  if (thread->dims) free(thread->dims);
	  if (thread->offs) free(thread->offs);
	  if (thread->incs) free(thread->incs);
	  if (thread->flags) free(thread->flags);
	  if (thread->pdls) free(thread->pdls);
	  PDLDEBUG_f(warn("trying to reinitialize already initialized "
	     "thread (mem-leak!); freeing...");)
	}
	PDL_THR_SETMAGIC(thread);
#endif
	thread->gflags = 0;

	thread->npdls = npdls;
	thread->pdls = strndup(pdls,sizeof(*pdls)*npdls);
	thread->realdims = realdims;
	thread->ndims = 0;

	thread->mag_nth = -1;
	thread->mag_nthpdl = -1;
        thread->mag_nthr = -1;

	nids=0;
	mx=0;
/* Find the max. number of threadids */
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		MAX2(nids,pdls[j]->nthreadids);
		MAX2(mx,pdls[j]->threadids[0] - realdims[j]);
	}
	nthreadids = pdl_malloc(sizeof(int)*nids);
	ndims += mx;  nimpl = mx; thread->nimpl = nimpl;
	for(j=0; j<npdls; j++) {
		if(creating[j]) continue;
		/* Check for magical piddles (parallelized) */
		if((!nthr) &&
		  pdls[j]->magic &&
		  (nthr = pdl_magic_thread_nthreads(pdls[j],&nthrd))) {
			thread->mag_nthpdl = j;
			thread->mag_nth = nthrd - realdims[j];
                       thread->mag_nthr = nthr;
			if(thread->mag_nth < 0) {
				die("Cannot magick non-threaded dims");
			}
		}

		for(i=0; i<nids; i++) {
			mx=0; if(pdls[j]->nthreadids <= nids) {
				MAX2(mx,
				     pdls[j]->threadids[i+1]
				     - pdls[j]->threadids[i]);
			}
			ndims += mx;
			nthreadids[i] = mx;
		}
	}

	if(nthr) {
		thread->gflags |= PDL_THREAD_MAGICKED;
	}

	if(ndims < nobl) { /* If too few, add enough implicit dims */
		thread->nextra = nobl - ndims;
		ndims += thread->nextra;
	} else {
		thread->nextra = 0;
	}

	thread->ndims = ndims;
	thread->nimpl = nimpl;
	thread->inds = malloc(sizeof(int) * thread->ndims);
	thread->dims = malloc(sizeof(int) * thread->ndims);
	thread->offs = malloc(sizeof(int) * thread->npdls
			* (nthr>0 ? nthr : 1));
	thread->incs = malloc(sizeof(int) * thread->ndims * npdls);
	thread->flags = malloc(sizeof(char) * npdls);
	nth=0; /* Index to dimensions */

	/* populate the per_pdl_flags */

	for (i=0;i<npdls; i++) {
	  thread->offs[i] = 0; /* initialize offsets */
	  thread->flags[i] = 0;
	  if (PDL_VAFFOK(pdls[i]) && VAFFINE_FLAG_OK(flags,i))
	    thread->flags[i] |= PDL_THREAD_VAFFINE_OK;
	}
	flags = thread->flags; /* shortcut for the remainder */

/* Make implicit inds */

	for(i=0; i<nimpl; i++) {
		thread->dims[nth] = 1;
		for(j=0; j<thread->npdls; j++) {
			thread->incs[nth*npdls+j] = 0;
			if(creating[j]) continue;
			if(thread->pdls[j]->threadids[0]-
					thread->realdims[j] <= i)
				continue;
			if(pdls[j]->dims[i+realdims[j]] != 1) {
				if(thread->dims[nth] != 1) {
					if(thread->dims[nth] !=
						pdls[j]->dims[i+realdims[j]]) {
                                              pdl_croak_param(info,j,"Mismatched implicit thread dimension %d: should be %d, is %d\n\t",
							i,
							thread->dims[nth],
							pdls[j]->dims[i+thread->realdims[j]]);
					}
				} else {
					thread->dims[nth] =
						pdls[j]->dims[i+realdims[j]];
				}
				thread->incs[nth*npdls+j] =
					PDL_TREPRINC(pdls[j],flags[j],i+realdims[j]);
			}
		}
		nth++;
	}

/* Go through everything again and make the real things */

	for(nthid=0; nthid<nids; nthid++) {
	for(i=0; i<nthreadids[nthid]; i++) {
		thread->dims[nth] = 1;
		for(j=0; j<thread->npdls; j++) {
			thread->incs[nth*npdls+j] = 0;
			if(creating[j]) continue;
			if(thread->pdls[j]->nthreadids < nthid)
				continue;
			if(thread->pdls[j]->threadids[nthid+1]-
			   thread->pdls[j]->threadids[nthid]
					<= i) continue;
			mydim = i+thread->pdls[j]->threadids[nthid];
			if(pdls[j]->dims[mydim]
					!= 1) {
				if(thread->dims[nth] != 1) {
					if(thread->dims[nth] !=
						pdls[j]->dims[mydim]) {
						pdl_croak_param(info,j,"Mismatched Implicit thread dimension %d: should be %d, is %d",
							i,
							thread->dims[nth],
							pdls[j]->dims[i+thread->realdims[j]]);
					}
				} else {
					thread->dims[nth] =
						pdls[j]->dims[mydim];
				}
				thread->incs[nth*npdls+j] =
					PDL_TREPRINC(pdls[j],flags[j],mydim);
			}
		}
		nth++;
	}
	}


/* Make sure that we have the obligatory number of threaddims */

	for(; nth<ndims; nth++) {
		thread->dims[nth]=1;
		for(j=0; j<npdls; j++)
			thread->incs[nth*npdls+j] = 0;
	}
/* If threading, make the true offsets and dims.. */

	if(nthr > 0) {
		int n1 = thread->dims[thread->mag_nth] / nthr;
		int n2 = thread->dims[thread->mag_nth] % nthr;
		if(n2) {
			die("Cannot magick-thread with non-divisible n!");
		}
		thread->dims[thread->mag_nth] = n1;
	}
	thread->gflags |= PDL_THREAD_INITIALIZED;
	PDLDEBUG_f(dump_thread(thread);)	
}

void pdl_thread_create_parameter(pdl_thread *thread,int j,int *dims,
				 int temp)
{
	int i;
	int td = temp ? 0 : thread->nimpl;

	if(!temp && thread->nimpl != thread->ndims - thread->nextra) {
		pdl_croak_param(thread->einfo,j,
			"Trying to create parameter while explicitly threading.\
See the manual for why this is impossible");
	}
	pdl_reallocdims(thread->pdls[j], thread->realdims[j] + td);
	for(i=0; i<thread->realdims[j]; i++)
		thread->pdls[j]->dims[i] = dims[i];
	if (!temp)
	  for(i=0; i<thread->nimpl; i++)
		thread->pdls[j]->dims[i+thread->realdims[j]] =
                       thread->dims[i] *
                     ((i == thread->mag_nth && thread->mag_nthr > 0) ?
                           thread->mag_nthr : 1);
	thread->pdls[j]->threadids[0] = td + thread->realdims[j];
	pdl_resize_defaultincs(thread->pdls[j]);
	for(i=0; i<thread->nimpl; i++) {
		thread->incs[thread->npdls*i + j] =
		  temp ? 0 :
		  PDL_REPRINC(thread->pdls[j],i+thread->realdims[j]);
	}
}

int pdl_startthreadloop(pdl_thread *thread,void (*func)(pdl_trans *),
			pdl_trans *t) {
	int i,j;
	int *offsp; int nthr;
	if((thread->gflags & (PDL_THREAD_MAGICKED | PDL_THREAD_MAGICK_BUSY))
	     == PDL_THREAD_MAGICKED) {
		thread->gflags |= PDL_THREAD_MAGICK_BUSY;
		if(!func) {
			die("NULL FUNCTION WHEN PTHREADING\n");
		}
		/* Do the threadloop magically (i.e. in parallel) */
		pdl_magic_thread_cast(thread->pdls[thread->mag_nthpdl],
			func,t);
		thread->gflags &= ~PDL_THREAD_MAGICK_BUSY;
		return 1; /* DON'T DO THREADLOOP AGAIN */
	}
	for(i=0; i<thread->ndims; i++)
		thread->inds[i] = 0;
	offsp = pdl_get_threadoffsp_int(thread,&nthr);
	for(j=0; j<thread->npdls; j++)
		offsp[j] = PDL_TREPROFFS(thread->pdls[j],thread->flags[j]) +
			(!nthr?0:
				nthr * thread->dims[thread->mag_nth] *
				    thread->incs[thread->mag_nth*thread->npdls + j]);
	return 0;
}

/* This will have to be macroized */
int pdl_iterthreadloop(pdl_thread *thread,int nth) {
	int i,j;
	int stop = 0;
	int stopdim;
	int *offsp; int nthr;
/*	printf("iterthreadloop\n"); */
	for(j=0; j<thread->npdls; j++)
		thread->offs[j] = PDL_TREPROFFS(thread->pdls[j],thread->flags[j]);
	for(i=nth; i<thread->ndims; i++) {
		thread->inds[i] ++;
		if(thread->inds[i] >= thread->dims[i])
			thread->inds[i] = 0;
		else
		{	stopdim = i; stop = 1; break; }
	}
	if(stop) goto calc_offs;
	return 0;
calc_offs:
	offsp = pdl_get_threadoffsp_int(thread,&nthr);
	for(j=0; j<thread->npdls; j++) {
		offsp[j] = PDL_TREPROFFS(thread->pdls[j],thread->flags[j]) +
		(!nthr?0:
			nthr * thread->dims[thread->mag_nth] *
			    thread->incs[thread->mag_nth*thread->npdls + j]);
			;
		for(i=nth; i<thread->ndims; i++) {
			offsp[j] += thread->incs[i*thread->npdls+j] *
					thread->inds[i];
		}
	}
	return stopdim+1;
}

/* prototype, defined in pdlcore.c */
char *pdl_mess(const char *pat, va_list *args);
void pdl_croak_param(pdl_errorinfo *info,int j, char *pat, ...)
{
	va_list args;
	char *message; char *name;
	static char mesgbuf[200];
      static char argsbuf[256], *argb;
      int i, k, l;
      SV *msv;

	va_start(args,pat);
	/* was Perl_mess before; Perl_mess has changed between 5.00X and 5.6 */
	message = pdl_mess(pat,&args);  /* barf dependence !!!! */
	/* Now, croak() overwrites this string. make a copy */
	strcpy(mesgbuf,message); message = mesgbuf;
	va_end(args);
	if(!info) {croak("PDL_CROAK_PARAM: Unknown: parameter %d: %s\n",
		j,message);
	} else {
		if(j >= info->nparamnames)
			name = "ERROR: UNKNOWN PARAMETER";
		else	name = info->paramnames[j];
              for (i=0,argb=argsbuf,l=255;i<info->nparamnames && l;i++) {
                /* Could improve method, but 256 chars should be
                     enough anyway! */
                k = strlen(info->paramnames[i]);
                if (k < l-4) {
                  memcpy(argb,info->paramnames[i],k);
                  argb += k;
                  *argb = ',';
                  argb++;
                  l -= k+1;
                } else {
                  *argb++ = '.';
                  *argb++ = '.';
                  *argb++ = '.';
                  argb++;
                  l = 0;
                }
              }
              *--argb = '\0';
/* this needs to be sorted: barf stuff ?? */
#ifdef croak
#define tcroak croak
#define croak Perl_croak
#ifdef aTHX_
#define _extra aTHX_
#else
#define _extra
#endif
#else
#define _extra
#endif
              croak(_extra "PDL: %s(%s): Parameter '%s'\n%s\n",
                    info->funcname,argsbuf,name,message);
#ifdef tcroak
#undef croak
#define croak tcroak
#undef tcroak
#endif
	}
}


