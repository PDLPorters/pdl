#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#define msgptr_advance()                        \
do {                                            \
  int N      = strlen(msgptr);                  \
  msgptr    += N;                               \
  remaining -= N;                               \
} while(0)

void pdl_croak_param(pdl_transvtable *vtable,int paramIndex, char *pat, ...)
{
  // I barf a string such as "PDL: function(a,b,c): Parameter 'b' errormessage"

  char message  [4096] = {'\0'};
  int i;
  va_list args;

  char* msgptr    = message;
  int   remaining = sizeof(message);

  if(vtable)
  {
    if(paramIndex < 0 || paramIndex >= vtable->npdls)
    {
      strcat(msgptr, "ERROR: UNKNOWN PARAMETER");
      msgptr_advance();
    }
    else
    {
      snprintf(msgptr, remaining, "PDL: %s(", vtable->name);
      msgptr_advance();

      for(i=0; i<vtable->npdls; i++)
      {
        snprintf(msgptr, remaining, "%s", vtable->par_names[i]);
        msgptr_advance();

        if(i < vtable->npdls-1)
        {
          snprintf(msgptr, remaining, ",");
          msgptr_advance();
        }
      }

      snprintf(msgptr, remaining, "): Parameter '%s':\n",
               vtable->par_names[paramIndex]);
      msgptr_advance();
    }
  }

  va_start(args,pat);

  vsnprintf(msgptr, remaining, pat, args);

  va_end(args);

  pdl_pdl_barf(message);
}

void pdl_print_iarr(PDL_Indx *iarr, int n) {
  int i;
  printf("(");
  for (i=0;i<n;i++) printf("%s%"IND_FLAG,(i?" ":""),iarr[i]);
  printf(")");
}

#define psp printf("%s",spaces)
void pdl_dump_thread(pdl_thread *thread) {
  int i, j, found=0, sz=0;
  char spaces[] = "    ";
  int flagval[] = {
#define X(f) f,
PDL_LIST_FLAGS_PDLTHREAD(X)
#undef X
    0
  };
  char *flagchar[] = {
#define X(f) #f,
PDL_LIST_FLAGS_PDLTHREAD(X)
#undef X
    NULL
  };
  int paramflagval[] = {
#define X(f) f,
PDL_LIST_FLAGS_PARAMS(X)
#undef X
    0
  };
  char *paramflagchar[] = {
#define X(f) #f,
PDL_LIST_FLAGS_PARAMS(X)
#undef X
    NULL
  };
  int typeval[] = {
#define X(sym, ctype, ppsym, shortctype, defbval) sym,
PDL_GENERICLIST(X)
#undef X
    -1
  };
  char *typechar[] = {
#define X(sym, ctype, ppsym, shortctype, defbval) #sym,
PDL_GENERICLIST(X)
#undef X
    NULL
  };
  fflush(stdout);
  printf("DUMPTHREAD %p\n",(void*)thread);
  if (thread->transvtable) {
    pdl_transvtable *vtable = thread->transvtable;
    psp; printf("Funcname: %s\n",vtable->name);
    psp; printf("Types: ");
    found=0; sz=0;
    for (i=0;vtable->gentypes[i]!=-1; i++) {
      if (sz>PDL_MAXLIN) {sz=0; printf("\n");psp;psp;}
      printf("%s%s",found ? ",":"",typechar[i]);
      found = 1;
      sz += strlen(typechar[i]);
    }
    printf("\n");
    psp; printf("Parameters:\n");
    for (i=0;i<vtable->npdls;i++) {
      psp; psp; printf("%s(",vtable->par_names[i]);
      found=0;
      for (j=0;j<vtable->par_realdims[i];j++) {
        if (found) printf(","); found=1;
        printf("%s",vtable->ind_names[PDL_IND_ID(vtable, i, j)]);
      }
      printf(") (");
      if (vtable->par_types[i] < 0) printf("no type");
      else {
	for (j=0;typeval[j]>=0; j++)
	  if (vtable->par_types[i] == typeval[j]) {
	    printf("%s",typechar[j]);
	    break;
	  }
      }
      printf("): ");
      found=0; sz=0;
      for (j=0;paramflagval[j]!=0; j++)
        if (vtable->par_flags[i] & paramflagval[j]) {
          if (sz>PDL_MAXLIN) {sz=0; printf("\n");psp;psp;psp;}
          printf("%s",found ? "|":""); found = 1;
          printf("%s",paramflagchar[j]);
          sz += strlen(paramflagchar[j]);
        }
      if (!found) printf("(no flags set)");
      printf("\n");
    }
    psp; printf("Indices: ");
    for (i=0;i<vtable->ninds;i++)
      printf("%s ",vtable->ind_names[i]);
    printf("\n");
    psp; printf("Realdims: "); pdl_print_iarr(vtable->par_realdims,thread->npdls); printf("\n");
  }
  psp; printf("Flags: ");
  found=0; sz=0;
  for (i=0;flagval[i]!=0; i++)
    if (thread->gflags & flagval[i]) {
      if (sz>PDL_MAXLIN) {sz=0; printf("\n");psp;}
      printf("%s%s",found ? "|":"",flagchar[i]);
      found = 1;
      sz += strlen(flagchar[i]);
    }
  printf("\n");
  psp; printf("Ndims: %"IND_FLAG", Nimplicit: %"IND_FLAG", Npdls: %"IND_FLAG", Nextra: %"IND_FLAG"\n",
	 thread->ndims,thread->nimpl,thread->npdls,thread->nextra);
  psp; printf("Mag_nth: %"IND_FLAG", Mag_nthpdl: %"IND_FLAG", Mag_nthr: %"IND_FLAG", Mag_skip: %"IND_FLAG", Mag_stride: %"IND_FLAG"\n",
	 thread->mag_nth,thread->mag_nthpdl,thread->mag_nthr,thread->mag_skip,thread->mag_stride);
  if (thread->mag_nthr <= 0) {
    psp; printf("Dims: "); pdl_print_iarr(thread->dims,thread->ndims); printf("\n");
    psp; printf("Inds: "); pdl_print_iarr(thread->inds,thread->ndims); printf("\n");
    psp; printf("Offs: "); pdl_print_iarr(thread->offs,thread->npdls); printf("\n");
  } else {
    psp; printf("Dims (per thread):\n");
    for (i=0;i<thread->mag_nthr;i++) {
      psp; psp; pdl_print_iarr(thread->dims + i*thread->ndims,thread->ndims);
      printf("\n");
    }
    psp; printf("Inds (per thread):\n");
    for (i=0;i<thread->mag_nthr;i++) {
      psp; psp; pdl_print_iarr(thread->inds + i*thread->ndims,thread->ndims);
      printf("\n");
    }
    psp; printf("Offs (per thread):\n");
    for (i=0;i<thread->mag_nthr;i++) {
      psp; psp; pdl_print_iarr(thread->offs + i*thread->npdls,thread->npdls);
      printf("\n");
    }
  }
  psp; printf("Incs (per pdl):\n");
  for (i=0;i<thread->npdls;i++) {
    psp; psp; pdl_print_iarr(thread->incs + i*thread->ndims,thread->ndims);
    printf("\n");
  }
  psp; printf("Realdims: "); pdl_print_iarr(thread->realdims,thread->npdls); printf("\n");
  psp; printf("Pdls: (");
  for (i=0;i<thread->npdls;i++)
    printf("%s%p",(i?" ":""),(void*)(thread->pdls[i]));
  printf(")\n");
  psp; printf("Per pdl flags: (");
  for (i=0;i<thread->npdls;i++)
    printf("%s%d",(i?" ":""),thread->flags[i]);
  printf(")\n");
  fflush(stdout);
}

void pdl_dump_threading_info(
  int npdls, PDL_Indx* creating, int target_pthread,
  PDL_Indx *nthreadedDims, PDL_Indx **threadedDims, PDL_Indx **threadedDimSizes,
  int maxPthreadPDL, int maxPthreadDim, int maxPthread
) {
  PDL_Indx j, k;
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    printf("PDL %"IND_FLAG":\n", j);
    for( k=0; k < nthreadedDims[j]; k++){
      printf("Thread dim %"IND_FLAG", Dim No %"IND_FLAG", Size %"IND_FLAG"\n",
        k, threadedDims[j][k], threadedDimSizes[j][k]);
    }
  }
  printf("\nTarget Pthread = %d\n"
    "maxPthread = %d, maxPthreadPDL = %d, maxPthreadDim = %d\n",
    target_pthread, maxPthread, maxPthreadPDL, maxPthreadDim);
}
