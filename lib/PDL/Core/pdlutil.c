#include "pdl.h"      /* Data structure declarations */
#define PDL_IN_CORE
#include "pdlcore.h"  /* Core declarations */
#include <stdarg.h>

extern Core PDL; /* for PDL_TYPENAME */

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

/* modified from glibc printf(3) */
pdl_error pdl_make_error(pdl_error_type e, const char *fmt, ...) {
  pdl_error PDL_err = {PDL_EFATAL, "make_error problem", 0};
  PDLDEBUG_f(printf("pdl_make_error called: "));
  va_list ap;
  /* Determine required size */
  va_start(ap, fmt);
  int size = vsnprintf(NULL, 0, fmt, ap);
  va_end(ap);
  if (size < 0) return PDL_err;
  char needs_free = 1;
  char *p = NULL;
  if (pdl_pthread_main_thread()) {
    size++;             /* For '\0' */
    p = malloc(size);
    if (p == NULL) return PDL_err;
    va_start(ap, fmt);
    size = vsnprintf(p, size, fmt, ap);
    va_end(ap);
    if (size < 0) {
      free(p);
      return PDL_err;
    }
  } else {
    size_t len = 0;
    va_start(ap, fmt);
    pdl_pthread_realloc_vsnprintf(&p, &len, size, fmt, &ap, 0);
    va_end(ap);
    needs_free = 2;
  }
  PDLDEBUG_f(printf("%s\n", p));
  return (pdl_error){e, p, needs_free};
}

pdl_error pdl_make_error_simple(pdl_error_type e, const char *msg) {
  pdl_error PDL_err = {e, msg, 0};
  return PDL_err;
}

pdl_error pdl_param_error(
  pdl_transvtable *vtable, int paramIndex,
  pdl **pdls, PDL_Indx nimpl, PDL_Indx *creating,
  char *pat, ...
) {
  // I make a pdl_error with a string such as "function(a,b,c): parameter 'b': errormessage"
  char message  [4096] = {'\0'};
  int i,j,maxrealdims;
  va_list args;
  char* msgptr    = message;
  int   remaining = sizeof(message);
  if(paramIndex < 0 || paramIndex >= vtable->npdls) {
    strcat(msgptr, "ERROR: UNKNOWN PARAMETER");
    msgptr_advance();
  } else {
    snprintf(msgptr, remaining, "%s(", vtable->name);
    msgptr_advance();
    for(i=0; i<vtable->npdls; i++) {
      snprintf(msgptr, remaining, "%s", vtable->par_names[i]);
      msgptr_advance();
      if(i < vtable->npdls-1) {
        snprintf(msgptr, remaining, ",");
        msgptr_advance();
      }
    }
    snprintf(msgptr, remaining, "): parameter '%s': ",
             vtable->par_names[paramIndex]);
    msgptr_advance();
  }
  va_start(args,pat);
  vsnprintf(msgptr, remaining, pat, args);
  va_end(args);
  msgptr_advance();
  snprintf(msgptr, remaining,
    "\nThere are %"IND_FLAG" PDLs in the expression; %"IND_FLAG" broadcast dim%s.\n",
    vtable->npdls,nimpl,(nimpl==1)?"":"s"
  );
  msgptr_advance();
  for(i=maxrealdims=0; i<vtable->npdls; i++)
    if(vtable->par_realdims[i]>maxrealdims)
      maxrealdims=vtable->par_realdims[i];
  snprintf(msgptr, remaining, "   PDL IN EXPR.    ");
  msgptr_advance();
  if(maxrealdims > 0) {
    char format[80];
    snprintf(format, sizeof(format)-1, "%%%ds", 8 * maxrealdims + 3);
    snprintf(msgptr,remaining,format,"ACTIVE DIMS | ");
    msgptr_advance();
  }
  snprintf(msgptr, remaining,"BROADCAST DIMS\n");
  msgptr_advance();
  for(i=0; i<vtable->npdls; i++) {
    snprintf(msgptr,remaining,"   #%3d (%s",i,creating[i]?"null)\n":"normal): ");
    msgptr_advance();
    if(creating[i])
      continue;
    if(maxrealdims == 1) {
      snprintf(msgptr,remaining,"    ");
      msgptr_advance();
    }
    for(j=0; j< maxrealdims - vtable->par_realdims[i]; j++) {
      snprintf(msgptr,remaining,"%8s"," ");
      msgptr_advance();
    }
    for(j=0; j< vtable->par_realdims[i]; j++) {
      snprintf(msgptr,remaining,"%8"IND_FLAG,pdls[i]->dims[j]);
      msgptr_advance();
    }
    if(maxrealdims) {
      snprintf(msgptr,remaining," | ");
      msgptr_advance();
    }
    for(j=0; j<nimpl && j + vtable->par_realdims[i] < pdls[i]->ndims; j++) {
      snprintf(msgptr,remaining,"%8"IND_FLAG,pdls[i]->dims[j+vtable->par_realdims[i]]);
      msgptr_advance();
    }
    snprintf(msgptr,remaining,"\n");
    msgptr_advance();
  }
  return pdl_make_error(PDL_EUSERERROR, "%s", message);
}

void pdl_print_iarr(PDL_Indx *iarr, int n) {
  int i;
  printf("(");
  for (i=0;i<n;i++) printf("%s%"IND_FLAG,(i?" ":""),iarr[i]);
  printf(")");
}

void pdl_dump_transvtable(pdl_transvtable *vtable, int nspac) {
  int i, j, found=0, sz=0;
  SET_SPACE(spaces, nspac);
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
#define X(sym, ...) sym,
PDL_TYPELIST_ALL(X)
#undef X
    -1
  };
  char *typechar[] = {
#define X(sym, ...) #sym,
PDL_TYPELIST_ALL(X)
#undef X
    NULL
  };
  psp; printf("Funcname: %s\n",vtable->name);
  psp; printf("Types: ");
  found=0; sz=0;
  for (i=0;vtable->gentypes[i]!=-1; i++) {
    char *this_str = typechar[vtable->gentypes[i]] + 4; /* "PDL_" */
    size_t thislen = strlen(this_str);
    if ((sz+thislen)>PDL_MAXLIN) {sz=nspac+4; printf("\n%s    ",spaces);}
    printf("%s%s",found ? ",":"",this_str); found = 1;
    sz += thislen;
  }
  printf("\n");
  psp; printf("Parameters:\n");
  for (i=0;i<vtable->npdls;i++) {
    psp; sz = nspac + printf("  %s(",vtable->par_names[i]);
    found=0;
    for (j=0;j<vtable->par_realdims[i];j++) {
      if (found) sz += printf(","); found=1;
      sz += printf("%s",vtable->ind_names[PDL_IND_ID(vtable, i, j)]);
    }
    if (vtable->par_flags[i] & PDL_PARAM_ISTYPED)
      sz += printf(") (%s", PDL_TYPENAME(vtable->par_types[i]));
    sz += printf("): ");
    found=0;
    for (j=0;paramflagval[j]!=0; j++)
      if (vtable->par_flags[i] & paramflagval[j]) {
        char *this_str = paramflagchar[j];
        size_t thislen = strlen(this_str);
        if ((sz+thislen)>PDL_MAXLIN) {sz=nspac+8; printf("\n%s        ",spaces);}
        sz += printf("%s%s",found ? "|":"",this_str); found = 1;
      }
    if (!found) printf("(no flags set)");
    printf("\n");
  }
  psp; printf("Indices:");
  for (i=0;i<vtable->ninds;i++)
    printf(" %s",vtable->ind_names[i]);
  printf("\n");
}

void pdl_dump_broadcast(pdl_broadcast *broadcast) {
  int i, j, found=0, sz=0, nspac=4;
  SET_SPACE(spaces, nspac);
  int flagval[] = {
#define X(f) f,
PDL_LIST_FLAGS_PDLBROADCAST(X)
#undef X
    0
  };
  char *flagchar[] = {
#define X(f) #f,
PDL_LIST_FLAGS_PDLBROADCAST(X)
#undef X
    NULL
  };
  printf("DUMPBROADCAST %p\n",broadcast);
  if (broadcast->transvtable)
    pdl_dump_transvtable(broadcast->transvtable, 4);
  psp; printf("Flags: ");
  found=0; sz=0;
  for (i=0;flagval[i]!=0; i++)
    if (broadcast->gflags & flagval[i]) {
      char *this_str = flagchar[i];
      size_t thislen = strlen(this_str);
      if ((sz+thislen)>PDL_MAXLIN) {sz=nspac; printf("\n%s",spaces);}
      printf("%s%s",found ? "|":"",this_str); found = 1;
      sz += thislen;
    }
  printf("\n");
  psp; printf("Ndims: %"IND_FLAG", Nimplicit: %"IND_FLAG", Npdls: %"IND_FLAG", Nextra: %"IND_FLAG"\n",
	 broadcast->ndims,broadcast->nimpl,broadcast->npdls,broadcast->nextra);
  psp; printf("Mag_nth: %"IND_FLAG", Mag_nthpdl: %"IND_FLAG", Mag_nthr: %"IND_FLAG", Mag_skip: %"IND_FLAG", Mag_stride: %"IND_FLAG"\n",
	 broadcast->mag_nth,broadcast->mag_nthpdl,broadcast->mag_nthr,broadcast->mag_skip,broadcast->mag_stride);
  if (broadcast->mag_nthr <= 0) {
    psp; printf("Dims: "); pdl_print_iarr(broadcast->dims,broadcast->ndims); printf("\n");
    psp; printf("Inds: "); pdl_print_iarr(broadcast->inds,broadcast->ndims); printf("\n");
    psp; printf("Offs: "); pdl_print_iarr(broadcast->offs,broadcast->npdls); printf("\n");
  } else {
    psp; printf("Dims (per thread):\n");
    for (i=0;i<broadcast->mag_nthr;i++) {
      psp; psp; pdl_print_iarr(broadcast->dims + i*broadcast->ndims,broadcast->ndims);
      printf("\n");
    }
    psp; printf("Inds (per thread):\n");
    for (i=0;i<broadcast->mag_nthr;i++) {
      psp; psp; pdl_print_iarr(broadcast->inds + i*broadcast->ndims,broadcast->ndims);
      printf("\n");
    }
    psp; printf("Offs (per thread):\n");
    for (i=0;i<broadcast->mag_nthr;i++) {
      psp; psp; pdl_print_iarr(broadcast->offs + i*broadcast->npdls,broadcast->npdls);
      printf("\n");
    }
  }
  psp; printf("Incs (per dim):\n");
  for (i=0;i<broadcast->ndims;i++) {
    psp; psp; pdl_print_iarr(&PDL_BRC_INC(broadcast->incs, broadcast->npdls, 0, i),broadcast->npdls);
    printf("\n");
  }
  psp; printf("Realdims: "); pdl_print_iarr(broadcast->realdims,broadcast->npdls); printf("\n");
  psp; printf("Pdls: (");
  for (i=0;i<broadcast->npdls;i++)
    printf("%s%p",(i?" ":""),(broadcast->pdls[i]));
  printf(")\n");
  psp; printf("Per pdl flags: (");
  for (i=0;i<broadcast->npdls;i++)
    printf("%s%d",(i?" ":""),broadcast->flags[i]);
  printf(")\n");
}

void pdl_dump_broadcasting_info(
  int npdls, PDL_Indx* creating, int target_pthread,
  PDL_Indx *nbroadcastedDims, PDL_Indx **broadcastedDims, PDL_Indx **broadcastedDimSizes,
  int maxPthreadPDL, int maxPthreadDim, int maxPthread
) {
  PDL_Indx j, k;
  for(j=0; j<npdls; j++) {
    if(creating[j]) continue;
    printf("PDL %"IND_FLAG":\n", j);
    for( k=0; k < nbroadcastedDims[j]; k++){
      printf("Broadcast dim %"IND_FLAG", Dim No %"IND_FLAG", Size %"IND_FLAG"\n",
        k, broadcastedDims[j][k], broadcastedDimSizes[j][k]);
    }
  }
  printf("\nTarget Pthread = %d\n"
    "maxPthread = %d, maxPthreadPDL = %d, maxPthreadDim = %d\n",
    target_pthread, maxPthread, maxPthreadPDL, maxPthreadDim);
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
	printf("(%d) ",flags);
	found = 0; sz = 0;
	for (i=0;flagval[i]!=0; i++)
	  if (flags & flagval[i]) {
	    char *this_str = flagchar[i];
	    size_t thislen = strlen(this_str);
	    if ((sz+thislen)>PDL_MAXLIN) {sz=7+nspac; printf("\n       %s",spaces);}
	    printf("%s%s",found ? "|":"",this_str); found = 1;
	    sz += thislen;
	  }
	printf("\n");
}

/* Dump a transformation (don't dump the pdls, just pointers to them */
void pdl_dump_trans_fixspace (pdl_trans *it, int nspac) {
	PDL_Indx i;
	SET_SPACE(spaces, nspac);
	printf("%sDUMPTRANS %p (%s)\n%s   Flags: ",spaces,it,it->vtable->name,spaces);
	pdl_dump_flags_fixspace(it->flags,nspac+3, PDL_FLAGS_TRANS);
	printf("%s   bvalflag: %d\n",spaces,it->bvalflag);
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
		printf("%s%p",(i?" ":""),(it->pdls[i]));
	printf(")     OUTPUTS: (");
	for(;i<it->vtable->npdls; i++)
		printf("%s%p",(i>it->vtable->nparents?" ":""),(it->pdls[i]));
	printf(")\n");
}

void pdl_dump_fixspace(pdl *it,int nspac) {
  PDL_DECL_CHILDLOOP(it)
  PDL_Indx i;
  SET_SPACE(spaces, nspac);
  printf("%sDUMPING %p     datatype: %d\n%s   State: ",spaces,it,it->datatype,spaces);
  pdl_dump_flags_fixspace(it->state,nspac+3,PDL_FLAGS_PDL);
  printf("%s   transvtable: %p, trans: %p, sv: %p\n",spaces,
    it->trans_parent?it->trans_parent->vtable:0, it->trans_parent, it->sv);
  if (it->datasv)
    printf("%s   datasv: %p, Svlen: %lld, refcnt: %lld\n", spaces,
      it->datasv, (long long)SvCUR((SV*)it->datasv), (long long)SvREFCNT((SV*)it->datasv));
  if (it->data)
    printf("%s   data: %p, nbytes: %"IND_FLAG", nvals: %"IND_FLAG"\n", spaces,
      it->data, it->nbytes, it->nvals);
  if (it->hdrsv)
    printf("%s   hdrsv: %p, reftype %s\n", spaces,
      it->hdrsv, sv_reftype((SV*)it->hdrsv, TRUE));
  printf("%s   Dims: %p ",spaces,it->dims);
  pdl_print_iarr(it->dims, it->ndims);
  printf("\n%s   BroadcastIds: %p ",spaces,it->broadcastids);
  pdl_print_iarr(it->broadcastids, it->nbroadcastids);
  if (it->vafftrans) {
    printf("\n%s   Vafftrans: %p (parent), o:%"IND_FLAG", i:",
      spaces,it->vafftrans->from,it->vafftrans->offs);
    pdl_print_iarr(PDL_REPRINCS(it), it->vafftrans->ndims);
  }
  if (it->state & PDL_BADVAL) {
          printf("\n%s   Badvalue (%s): ",spaces, it->has_badvalue ? "bespoke" : "orig");
          PDL_Anyval result = { PDL_INVALID, {0} };
          if (!(it->has_badvalue && it->badvalue.type != it->datatype)) {
            if (it->has_badvalue)
              result = it->badvalue;
            else {
#define X(datatype, ctype, ppsym, shortctype, defbval, ...) \
  result.type = datatype; result.value.ppsym = defbval;
              PDL_GENERICSWITCH(PDL_TYPELIST_ALL, it->datatype, X, )
#undef X
            }
          }
          if (result.type < 0)
            printf("ERROR getting badval");
          else
            pdl_dump_anyval(result);
  }
  if (it->state & PDL_ALLOCATED) {
    printf("\n%s   First values: (",spaces);
    for (i=0; i<it->nvals && i<10; i++) {
                       if (i) printf(" ");
                       PDL_Anyval result = { PDL_INVALID, {0} };
                       ANYVAL_FROM_CTYPE_OFFSET(result, it->datatype, it->data, i);
                       pdl_dump_anyval(result);
    }
  } else {
    printf("\n%s   (not allocated",spaces);
  }
  printf(")\n");
  if (it->trans_parent) {
    pdl_dump_trans_fixspace(it->trans_parent,nspac+3);
  }
  if (it->ntrans_children) {
    printf("%s   CHILDREN:\n",spaces);
    PDL_START_CHILDLOOP(it)
      pdl_dump_trans_fixspace(PDL_CHILDLOOP_THISCHILD(it),nspac+4);
    PDL_END_CHILDLOOP(it)
  }
}

void pdl_dump (pdl *it) {
	pdl_dump_fixspace(it,0);
	fflush(stdout);
}

void pdl_dump_anyval(PDL_Anyval v) {
  if (v.type < PDL_CF) {
#define X(datatype, ctype, ppsym, ...) \
    printf("%Lg", (long double)v.value.ppsym);
    PDL_GENERICSWITCH(PDL_TYPELIST_REAL, v.type, X, printf("(UNKNOWN PDL_Anyval type=%d)", v.type))
#undef X
  } else {
#define X(datatype, ctype, ppsym, ...) \
    printf("%Lg%+Lgi", creall((complex long double)v.value.ppsym), cimagl((complex long double)v.value.ppsym));
    PDL_GENERICSWITCH(PDL_TYPELIST_COMPLEX, v.type, X, printf("(UNKNOWN PDL_Anyval type=%d)", v.type))
#undef X
  }
}

void pdl_error_free(pdl_error e) {
  if (e.needs_free == 1) {
    free((void *)e.message);
  } else {
    /* needs mutex-protected and de-Perl-ified */
    pdl_pthread_free((void *)e.message);
  }
}

void pdl_barf_if_error(pdl_error err) {
  if (!err.error) return;
  const char *msg = err.message;
  if (err.needs_free) {
    char *cpy = pdl_smalloc(strlen(msg) + 1);
    strcpy(cpy, err.message);
    pdl_error_free(err);
    msg = cpy;
  }
  pdl_pdl_barf(msg);
}

pdl_error pdl_error_accumulate(pdl_error err_current, pdl_error err_new) {
  if (!err_new.error) return err_current;
  if (!err_current.error) return err_new;
  pdl_error PDL_err = pdl_make_error(
    PDLMAX(err_current.error, err_current.error),
    "%s\n%s", err_current.message, err_new.message
  );
  if (err_current.needs_free) pdl_error_free(err_current);
  if (err_new.needs_free) pdl_error_free(err_new);
  return PDL_err;
}
