#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#define msgptr_advance()                        \
do {                                            \
  int N      = strlen(msgptr);                  \
  msgptr    += N;                               \
  remaining -= N;                               \
} while(0)
#define SET_SPACE(s, nspac) char spaces[PDL_MAXSPACE]; do { \
  int i; \
  if (nspac >= PDL_MAXSPACE) { \
    printf("too many spaces requested: %d" \
           "  (increase PDL_MAXSPACE in pdlapi.c), returning\n",nspac); \
    return; \
  } \
  for(i=0; i<nspac; i++) spaces[i]=' '; \
  spaces[i] = '\0'; \
} while (0)
#define psp printf("%s",spaces)

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

void pdl_thread_mismatch_msg(
  char *s,
  pdl **pdls, pdl_thread *thread,
  PDL_Indx i, PDL_Indx j, PDL_Indx nimpl,
  PDL_Indx *realdims,PDL_Indx *creating
) {
  /* This probably uses a lot more lines than necessary */
  int ii,jj,maxrealdims;
  sprintf(s,
    "  Mismatched implicit thread dimension %"IND_FLAG": size %"IND_FLAG" vs. %"IND_FLAG"\nThere are %"IND_FLAG" PDLs in the expression; %"IND_FLAG" thread dim%s.\n",
    i,thread->dims[i],pdls[j]->dims[i+realdims[j]],
    thread->npdls,nimpl,(nimpl==1)?"":"s"
  );
  s += strlen(s);
  for(ii=maxrealdims=0; ii<thread->npdls; ii++)
    if(thread->realdims[ii]>maxrealdims)
      maxrealdims=thread->realdims[ii];
  sprintf(s,  "   PDL IN EXPR.    "); s += strlen(s);
  if(maxrealdims > 0) {
    char format[80];
    sprintf(format,"%%%ds",8 * maxrealdims + 3);
    sprintf(s,format,"ACTIVE DIMS | ");
    s += strlen(s);
  }
  sprintf(s,"THREAD DIMS\n");
  s += strlen(s);
  for(ii=0; ii<thread->npdls; ii++) {
    sprintf(s,"   #%3d (%s",ii,creating[ii]?"null)\n":"normal): ");
    s += strlen(s);
    if(creating[ii])
      continue;
    if(maxrealdims == 1) {
      sprintf(s,"    ");
      s += strlen(s);
    }
    for(jj=0; jj< maxrealdims - thread->realdims[ii]; jj++) {
      sprintf(s,"%8s"," ");
      s += strlen(s);
    }
    for(jj=0; jj< thread->realdims[ii]; jj++) {
      sprintf(s,"%8"IND_FLAG,pdls[ii]->dims[jj]);
      s += strlen(s);
    }
    if(maxrealdims) {
      sprintf(s," | ");
      s += strlen(s);
    }
    for(jj=0; jj<nimpl && jj + thread->realdims[ii] < pdls[ii]->ndims; jj++) {
      sprintf(s,"%8"IND_FLAG,pdls[ii]->dims[jj+thread->realdims[ii]]);
      s += strlen(s);
    }
    sprintf(s,"\n");
    s += strlen(s);
  }
}

void pdl_dump_flags_fixspace(int flags, int nspac, pdl_flags type)
{
	int i;
	int found = 0;
	size_t sz = 0;
	int pdlflagval[] = {
#define X(f) f,
PDL_LIST_FLAGS_PDLSTATE(X)
#undef X
	    0
	};
	char *pdlflagchar[] = {
#define X(f) #f,
PDL_LIST_FLAGS_PDLSTATE(X)
#undef X
	    NULL
	};
	int transflagval[] = {
#define X(f) f,
PDL_LIST_FLAGS_PDLTRANS(X)
#undef X
	    0
	};
	char *transflagchar[] = {
#define X(f) #f,
PDL_LIST_FLAGS_PDLTRANS(X)
#undef X
	  NULL
	};
	int vtableflagval[] = {
#define X(f) f,
PDL_LIST_FLAGS_PDLVTABLE(X)
#undef X
	    0
	};
	char *vtableflagchar[] = {
#define X(f) #f,
PDL_LIST_FLAGS_PDLVTABLE(X)
#undef X
	  NULL
	};
	int *flagval;
	char **flagchar;
	SET_SPACE(spaces, nspac);
	switch (type) {
          case PDL_FLAGS_PDL: {
            flagval = pdlflagval;
            flagchar = pdlflagchar;
            break; }
          case PDL_FLAGS_VTABLE: {
            flagval = vtableflagval;
            flagchar = vtableflagchar;
            break; }
          default: {
            flagval = transflagval;
            flagchar = transflagchar;
          }
        }
	printf("%sState: (%d) ",spaces,flags);
	found = 0; sz = 0;
	for (i=0;flagval[i]!=0; i++)
	  if (flags & flagval[i]) {
	    if (sz>PDL_MAXLIN) {sz=0; printf("\n       %s",spaces);}
	    printf("%s%s",found ? "|":"",flagchar[i]);
	    found = 1;
	    sz += strlen(flagchar[i]);
	  }
	printf("\n");
}

/* Dump a transformation (don't dump the pdls, just pointers to them */
void pdl_dump_trans_fixspace (pdl_trans *it, int nspac) {
	PDL_Indx i;
	SET_SPACE(spaces, nspac);
	printf("%sDUMPTRANS %p (%s)\n",spaces,(void*)it,it->vtable->name);
	pdl_dump_flags_fixspace(it->flags,nspac+3,PDL_FLAGS_TRANS);
	printf("%s   vtable flags ",spaces);
	pdl_dump_flags_fixspace(it->vtable->flags,nspac+3,PDL_FLAGS_VTABLE);
	if(it->flags & PDL_ITRANS_ISAFFINE) {
		if(it->pdls[1]->state & PDL_PARENTDIMSCHANGED) {
			printf("%s   AFFINE, BUT DIMSCHANGED\n",spaces);
		} else {
			printf("%s   AFFINE: o:%"IND_FLAG", i:",spaces,it->offs);
			if (it->incs)
			  pdl_print_iarr(it->incs, it->pdls[1]->ndims);
			printf(" d:");
			pdl_print_iarr(it->pdls[1]->dims, it->pdls[1]->ndims);
			printf("\n");
		}
	}
/*	if(it->vtable->dump) {it->vtable->dump(it);} */
	printf("%s   ind_sizes: ",spaces);
	pdl_print_iarr(it->ind_sizes, it->vtable->ninds); printf("\n");
	printf("%s   inc_sizes: ",spaces);
	pdl_print_iarr(it->inc_sizes, it->vtable->nind_ids); printf("\n");
	printf("%s   INPUTS: (",spaces);
	for(i=0; i<it->vtable->nparents; i++)
		printf("%s%p",(i?" ":""),(void*)(it->pdls[i]));
	printf(")     OUTPUTS: (");
	for(;i<it->vtable->npdls; i++)
		printf("%s%p",(i?" ":""),(void*)(it->pdls[i]));
	printf(")\n");
}

void pdl_dump_fixspace(pdl *it,int nspac)
{
	PDL_DECL_CHILDLOOP(it)
	PDL_Indx i;
	SET_SPACE(spaces, nspac);
	printf("%sDUMPING %p     datatype: %d\n",spaces,(void*)it,it->datatype);
	pdl_dump_flags_fixspace(it->state,nspac+3,PDL_FLAGS_PDL);
	printf("%s   transvtable: %p, trans: %p, sv: %p\n",spaces,
		(void*)(it->trans_parent?it->trans_parent->vtable:0), (void*)(it->trans_parent), (void*)(it->sv));
	if(it->datasv)
		printf("%s   datasv: %p, Svlen: %d\n", spaces,
			(void*)it->datasv, (int)SvCUR((SV*)it->datasv));
	if(it->data)
		printf("%s   data: %p, nvals: %"IND_FLAG"\n", spaces,
			(void*)(it->data), it->nvals);
	if(it->hdrsv)
		printf("%s   hdrsv: %p, reftype %s\n", spaces,
			(void*)it->hdrsv, sv_reftype((SV*)it->hdrsv, TRUE));
	printf("%s   Dims: %p ",spaces,(void*)it->dims);
	pdl_print_iarr(it->dims, it->ndims);
	printf("\n%s   ThreadIds: %p ",spaces,(void*)(it->threadids));
	pdl_print_iarr(it->threadids, it->nthreadids);
	if(PDL_VAFFOK(it)) {
		printf("\n%s   Vaffine ok: %p (parent), o:%"IND_FLAG", i:",
			spaces,(void*)(it->vafftrans->from),it->vafftrans->offs);
		pdl_print_iarr(it->vafftrans->incs, it->ndims);
	}
	if(it->state & PDL_ALLOCATED) {
		printf("\n%s   First values: (",spaces);
		for(i=0; i<it->nvals && i<10; i++) {
                       printf("%s%f",(i?" ":""),pdl_get_offs(it,i).value.D);
		}
	} else {
		printf("\n%s   (not allocated",spaces);
	}
	printf(")\n");
	if(it->trans_parent) {
		pdl_dump_trans_fixspace(it->trans_parent,nspac+3);
	}
	printf("%s   CHILDREN:\n",spaces);
	PDL_START_CHILDLOOP(it)
		pdl_dump_trans_fixspace(PDL_CHILDLOOP_THISCHILD(it),nspac+4);
	PDL_END_CHILDLOOP(it)
	/* XXX phys etc. also */
}

void pdl_dump (pdl *it) {
	pdl_dump_fixspace(it,0);
}
