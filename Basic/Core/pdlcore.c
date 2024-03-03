#include "pdl.h"      /* Data structure declarations */
#define PDL_IN_CORE /* access funcs directly not through PDL-> */
#include "pdlcore.h"  /* Core declarations */
#include "pdlperl.h"

void pdl_SetSV_PDL ( SV *sv, pdl *it ) {
        SV *newref;
        if(!it->sv) {
                newref = newRV_noinc(it->sv = newSViv(PTR2IV(it)));
                (void)sv_bless(newref,gv_stashpv("PDL",TRUE));
        } else {
                newref = newRV_inc(it->sv);
                SvAMAGIC_on(newref);
        }
        sv_setsv(sv,newref);
        SvREFCNT_dec(newref);
}

/* Size of data type information */
size_t pdl_howbig (int datatype) {
#define X(datatype, ctype, ...) \
    return sizeof(ctype);
  PDL_GENERICSWITCH(PDL_TYPELIST2_ALL, datatype, X, croak("Not a known data type code=%d", datatype))
#undef X
}

/*
  "Convert" a perl SV into a pdl (alright more like a mapping as
   the data block is not actually copied in the most common case
   of a single scalar) scalars are automatically converted to PDLs.
*/
pdl* pdl_SvPDLV ( SV* sv ) {

   pdl* ret;
   SV *sv2;

   if(sv_derived_from(sv, "PDL") && !SvROK(sv)) {
      /* object method called as class method */
      pdl_pdl_barf("called object method on 'PDL' or similar");
   }

   if ( !SvROK(sv) ) {
      /* The scalar is not a ref, so we can use direct conversion. */
      PDL_Anyval data;
      ANYVAL_FROM_SV(data, sv, TRUE, -1);
      PDLDEBUG_f(printf("pdl_SvPDLV type: %d\n", data.type));
      return pdl_scalar(data);
   } /* End of scalar case */

   if(sv_derived_from(sv, "Math::Complex")) {
      dSP;
      int i;
      NV retval;
      double vals[2];
      char *meths[] = { "Re", "Im" };
      PDL_Anyval data;
      ENTER; SAVETMPS;
      for (i = 0; i < 2; i++) {
        PUSHMARK(sp); XPUSHs(sv); PUTBACK;
        int count = perl_call_method(meths[i], G_SCALAR);
        SPAGAIN;
        if (count != 1) croak("Failed Math::Complex method '%s'", meths[i]);
        retval = POPn;
        vals[i] = (double)retval;
        PUTBACK;
      }
      FREETMPS; LEAVE;
      data.type = PDL_CD;
      data.value.C = (PDL_CDouble)(vals[0] + I * vals[1]);
      return pdl_scalar(data);
   }

   /* If execution reaches here, then sv is NOT a scalar
    * (i.e. it is a ref).
    */

   if(SvTYPE(SvRV(sv)) == SVt_PVHV) {
        HV *hash = (HV*)SvRV(sv);
        SV **svp = hv_fetchs(hash,"PDL",0);
        if(svp == NULL) {
                croak("Hash given as a pdl (%s) - but not {PDL} key!", sv_reftype(SvRV(sv), TRUE));
        }
        if(*svp == NULL) {
                croak("Hash given as a pdl (%s) - but not {PDL} key (*svp)!", sv_reftype(SvRV(sv), TRUE));
        }

        /* This is the magic hook which checks to see if {PDL}
        is a code ref, and if so executes it. It should
        return a standard ndarray. This allows
        all kinds of funky objects to be derived from PDL,
        and allow normal PDL functions to still work so long
        as the {PDL} code returns a standard ndarray on
        demand - KGB */

        if (SvROK(*svp) && SvTYPE(SvRV(*svp)) == SVt_PVCV) {
           dSP;
           ENTER ;
           SAVETMPS ;
           PUSHMARK(sp) ;

           int count = perl_call_sv(*svp, G_SCALAR|G_NOARGS);

           SPAGAIN ;

           if (count != 1)
              croak("Execution of PDL structure failed to return one value\n") ;

           sv=newSVsv(POPs);

           PUTBACK ;
           FREETMPS ;
           LEAVE ;
        }
        else {
           sv = *svp;
        }

        if(SvGMAGICAL(sv)) {
                mg_get(sv);
        }

        if ( !SvROK(sv) ) {   /* Got something from a hash but not a ref */
                croak("Hash given as pdl - but PDL key is not a ref!");
        }
    }
      
    if(SvTYPE(SvRV(sv)) == SVt_PVAV) {
        /* This is similar to pdl_avref in Core.xs.PL -- we do the same steps here. */
        int datalevel = -1;
        AV *av = (AV *)SvRV(sv);
        AV *dims = (AV *)sv_2mortal((SV *)newAV());
        av_store(dims,0,newSViv( (IV) av_len(av)+1 ) );
        
        /* Pull sizes using av_ndcheck */
        av_ndcheck(av,dims,0,&datalevel);

        return pdl_from_array(av, dims, -1, NULL); /* -1 means pdltype autodetection */

    } /* end of AV code */
    
    if (SvTYPE(SvRV(sv)) != SVt_PVMG)
      croak("Error - tried to use an unknown data structure as a PDL");
    else if( !( sv_derived_from( sv, "PDL") ) )
      croak("Error - tried to use an unknown Perl object type as a PDL");

    sv2 = (SV*) SvRV(sv);

    /* Return the pdl * pointer */
    ret = INT2PTR(pdl *, SvIV(sv2));
    if (!ret) croak("Fatal error: ndarray address is NULL");

    /* Final check -- make sure it has the right magic number */
    if(ret->magicno != PDL_MAGICNO) {
        croak("Fatal error: argument is probably not an ndarray, or\
 magic no overwritten. You're in trouble, guv: %p %p %lu\n",sv2,ret,ret->magicno);
   }

   return ret;
}

/* Pack dims array - returns dims[] (pdl_smalloced) and ndims */
PDL_Indx* pdl_packdims ( SV* sv, PDL_Indx *ndims ) {
   if (!(SvROK(sv) && SvTYPE(SvRV(sv))==SVt_PVAV))  /* Test */
       return NULL;
   AV *array = (AV *) SvRV(sv);   /* dereference */
   *ndims = (PDL_Indx) av_len(array) + 1;  /* Number of dimensions */
   PDL_Indx *dims = (PDL_Indx *) pdl_smalloc( (*ndims) * sizeof(*dims) ); /* Array space */
   if (dims == NULL) return NULL;
   PDL_Indx i;
   for(i=0; i<(*ndims); i++) {
      dims[i] = (PDL_Indx) SvIV(*(av_fetch( array, i, 0 )));
   }
   return dims;
}

/* Pack array of pdl* - returns pdls[] (pdl_smalloced) and npdls */
pdl ** pdl_packpdls( SV* sv, PDL_Indx *npdls ) {
  if (!SvOK(sv)) { /* undef is OK, treat as empty */
    *npdls = 0;
    return NULL;
  }
  if (!SvROK(sv)) pdl_pdl_barf("Gave a non-reference as array-ref of PDLs");
  if (SvTYPE(SvRV(sv))!=SVt_PVAV)
    pdl_pdl_barf("Gave a non-array-reference as array-ref of PDLs");
  AV *array = (AV *) SvRV(sv);
  if (!array) pdl_pdl_barf("Failed to get AV from reference");
  *npdls = (PDL_Indx) av_len(array) + 1;
  if (!*npdls) return NULL;
  pdl **pdls = (pdl **) pdl_smalloc( (*npdls) * sizeof(*pdls) );
  if (!pdls) pdl_pdl_barf("Failed to allocate memory for pointers to PDLs");
  PDL_Indx i;
  for(i=0; i<(*npdls); i++) {
    SV **s = av_fetch( array, i, 0 );
    if (!s) pdl_pdl_barf("Failed to fetch SV #%"IND_FLAG, i);
    pdls[i] = pdl_SvPDLV(*s);
  }
  return pdls;
}

/* Unpack array of pdl* into SV* */
SV* pdl_unpackpdls( pdl **pdls, PDL_Indx npdls ) {
   AV *array = newAV();
   if (!array) return NULL;
   av_extend(array, npdls + 1);
   PDL_Indx i;
   for(i=0; i<npdls; i++) {
      SV *sv = newSV(0);
      pdl_SetSV_PDL(sv, pdls[i]);
      av_push(array, sv);
   }
   return sv_2mortal(newRV_noinc((SV *)array));
}

PDL_Indx pdl_safe_indterm( PDL_Indx dsz, PDL_Indx at, char *file, int lineno)
{
  if (!(at >= 0 && at < dsz))
    pdl_pdl_barf("access [%d] out of range [0..%d] (inclusive) at %s line %d",
          at, dsz-1, file?file:"?", lineno);
  return at;
}

/*
   pdl_smalloc - utility to get temporary memory space. Uses
   a mortal *SV for this so it is automatically freed when the current
   context is terminated without having to call free(). Naughty but
   nice!
*/
void* pdl_smalloc ( STRLEN nbytes ) {
   SV* work = sv_2mortal(newSVpv("", 0));
   SvGROW( work, nbytes);
   return (void *) SvPV_nolen(work);
}

/*********** Stuff for barfing *************/
/*
   This routine barfs/warns in a thread-safe manner. If we're in the main thread,
   this calls the perl-level barf/warn. If in a worker thread, we save the
   message to barf/warn in the main thread later
   For greppability: this is where pdl_pdl_barf and pdl_pdl_warn are defined
*/

#define GEN_PDL_BARF_OR_WARN_I_STDARG(type, iswarn)     \
  void pdl_pdl_##type(const char* pat, ...)           \
  { \
    va_list args;                                   \
    va_start(args, pat);                            \
    /* If we're in a worker thread, we queue the \
     * barf/warn for later, and exit the thread ... \
     */ \
    if( pdl_pthread_barf_or_warn(pat, iswarn, &args) ) \
      return; \
    /* ... otherwise we fall through and barf by calling \
     * the perl-level PDL::barf() or PDL::cluck() \
     */ \
    dSP; \
    ENTER; \
    SAVETMPS; \
    PUSHMARK(SP); \
    SV *sv = sv_2mortal(newSV(0)); \
    va_start(args, pat); \
    int size = vsnprintf(NULL, 0, pat, args); \
    va_end(args); \
    if (size < 0) { \
      sv_setpv(sv, "vsnprintf error"); \
    } else { \
      size += 2;             /* For '\0' + 1 as CentOS 7 is off by 1 */ \
      char buf[size]; \
      va_start(args, pat); \
      size = vsnprintf(buf, size, pat, args); \
      va_end(args); \
      sv_setpv(sv, size < 0 ? "vsnprintf error" : buf); \
    } \
    XPUSHs(sv); \
    PUTBACK; \
    call_pv(iswarn ? "PDL::cluck" : "PDL::barf", G_DISCARD); \
    FREETMPS; \
    LEAVE; \
  }

GEN_PDL_BARF_OR_WARN_I_STDARG(barf, 0)
GEN_PDL_BARF_OR_WARN_I_STDARG(warn, 1)

/**********************************************************************
 *
 * CONSTRUCTOR/INGESTION HELPERS
 *
 * The following routines assist with the permissive constructor,
 * which is designed to build a PDL out of basically anything thrown at it.
 *
 * They are all called by pdl_avref in Core.xs, which in turn is called by the constructors
 * in Core.pm.PL.  The main entry point is pdl_from_array(), which calls 
 * av_ndcheck() to identify the necessary size of the output PDL, and then dispatches
 * the copy into pdl_setav_<type> according to the type of the output PDL.
 *
 */

/******************************
 * av_ndcheck -
 *  traverse a Perl array ref recursively, following down any number of
 *  levels of references, and generate a minimal PDL dim list that can
 *  encompass them all according to permissive-constructor rules.
 *
 *  Scalars, array refs, and PDLs may be mixed in the incoming AV.
 *
 *  The routine works out the dimensions of a corresponding
 *  ndarray (in the AV dims) in reverse notation (vs PDL conventions).
 *
 *  It does not enforce a rectangular array on the input, the idea being that
 *  omitted values will be set to zero or the undefval in the resulting ndarray,
 *  i.e. we can make ndarrays from 'sparse' array refs.
 *
 *  Empty PDLs are treated like any other dimension -- i.e. their 
 *  0-length dimensions are thrown into the mix just like nonzero 
 *  dimensions would be.
 *
 *  The possible presence of empty PDLs forces us to pad out dimensions
 *  to unity explicitly in cases like
 *         [ Empty[2x0x2], 5 ]
 *  where simple parsing would yield a dimlist of 
 *         [ 2,0,2,2 ]
 *  which is still Empty.
 */

PDL_Indx av_ndcheck(AV* av, AV* dims, int level, int *datalevel)
{
  PDL_Indx i, len, oldlen;
  int newdepth, depth = 0;
  int n_scalars = 0;
  SV *el, **elp;
  pdl *dest_pdl;           /* Stores PDL argument */

  if(dims==NULL) {
    pdl_pdl_barf("av_ndcheck - got a null dim array! This is a bug in PDL.");
  }

  /* Start with a clean slate */
   if(level==0) {
    av_clear(dims);
  }

  len = av_len(av);                         /* Loop over elements of the AV */
  for (i=0; i<= len; i++) {
    
    newdepth = 0;                           /* Each element - find depth */
    elp = av_fetch(av,i,0);
    
    el = elp ? *elp : 0;                    /* Get the ith element */
    if (el && SvROK(el)) {                  /* It is a reference */
      if (SvTYPE(SvRV(el)) == SVt_PVAV) {   /* It is an array reference */
        
        /* Recurse to find depth inside the array reference */
        newdepth = 1 + av_ndcheck((AV *) SvRV(el), dims, level+1, datalevel);
        
      } else if ( (dest_pdl = pdl_SvPDLV(el)) ) {
        /* It is a PDL - walk down its dimension list, exactly as if it
         * were a bunch of nested array refs.  We pull the ndims and dims
         * fields out to local variables so that nulls can be treated specially.
         */
        int j;
        short pndims;
        PDL_Indx *dest_dims;
        pdl_barf_if_error(pdl_make_physdims(dest_pdl));
        pndims = dest_pdl->ndims;
        dest_dims = dest_pdl->dims;
        for(j=0;j<pndims;j++) {
          int jl = pndims-j+level;
          
          PDL_Indx siz = dest_dims[j];
          
          if(  av_len(dims) >= jl &&
               av_fetch(dims,jl,0) != NULL &&
               SvIOK(*(av_fetch(dims,jl,0)))) {
            
            /* We have already found something that specifies this dimension -- so */ 
            /* we keep the size if possible, or enlarge if necessary.              */
            oldlen=(PDL_Indx)SvIV(*(av_fetch(dims,jl,0)));
            if(siz > oldlen) {
              sv_setiv(*(av_fetch(dims,jl,0)),(IV)(dest_dims[j]));
            }
            
          } else {
            /* Breaking new dimensional ground here -- if this is the first element */
            /* in the arg list, then we can keep zero elements -- but if it is not  */
            /* the first element, we have to pad zero dims to unity (because the    */
            /* prior object had implicit size of 1 in all implicit dimensions)      */
            av_store(dims, jl, newSViv((IV)(siz?siz:(i?1:0))));
          }
        }
        
        /* We have specified all the dims in this PDL.  Now pad out the implicit */
        /* dims of size unity, to wipe out any dims of size zero we have already */
        /* marked. */
        
        for(j=pndims+1; j <= av_len(dims); j++) {
          SV **svp = av_fetch(dims,j,0);

          if(!svp){
            av_store(dims, j, newSViv((IV)1));
          } else if( (int)SvIV(*svp) == 0 ) {
            sv_setiv(*svp, (IV)1);
          }
        }
        
        newdepth= pndims;
        
      } else {
        croak("av_ndcheck: non-array, non-PDL ref in structure\n\t(this is usually a problem with a pdl() call)");
      }

    } else { 
      /* got a scalar (not a ref) */
      n_scalars++;

    }

      if (newdepth > depth)
        depth = newdepth;
  }
  
  len++; // convert from funky av_len return value to real count
  
    if (av_len(dims) >= level && av_fetch(dims, level, 0) != NULL
      && SvIOK(*(av_fetch(dims, level, 0)))) {
    oldlen = (PDL_Indx) SvIV(*(av_fetch(dims, level, 0)));
    
    if (len > oldlen)
      sv_setiv(*(av_fetch(dims, level, 0)), (IV) len);
    }
    else
      av_store(dims,level,newSViv((IV) len));
  
  /* We found at least one element -- so pad dims to unity at levels earlier than this one */
  if(n_scalars) {
    for(i=0;i<level;i++) {
      SV **svp = av_fetch(dims, i, 0);
      if(!svp) {
        av_store(dims, i, newSViv((IV)1));
      } else if( (PDL_Indx)SvIV(*svp) == 0) {
        sv_setiv(*svp, (IV)1);
      }
    }
    
    for(i=level+1; i <= av_len(dims); i++) {
      SV **svp = av_fetch(dims, i, 0);
      if(!svp) {
        av_store(dims, i, newSViv((IV)1));
      } else if( (PDL_Indx)SvIV(*svp) == 0) {
        sv_setiv(*svp, (IV)1);
      }
    }
  }

  return depth;
}

/* helper function used in pdl_from_array */
static int _detect_datatype(AV *av) {
  SV **item;
  AV *array;
  int count, i;
  if (!av) return PDL_D;
  count = av_len(av);
  for (i = 0; i < count; i++) {
    item = av_fetch(av, i, 0);
    if (*item) {
      if (SvROK(*item)) {
        array = (AV*)SvRV(*item);
        if (_detect_datatype(array) == PDL_D) {
          return PDL_D;
        }
      }
      if (SvOK(*item) && !SvIOK(*item)) {
        return PDL_D;
      }
    }
  }
#if IVSIZE == 8
  return PDL_LL;
#else
  return PDL_L;
#endif
}

/**********************************************************************
 * pdl_from_array - dispatcher gets called only by pdl_avref (defined in
 * Core.xs) - it breaks out to pdl_setav_<type>, below, based on the 
 * type of the destination PDL.
 */
pdl* pdl_from_array(AV* av, AV* dims, int dtype, pdl* dest_pdl)
{
  int ndims, i, level=0;
  PDL_Anyval undefval = { PDL_INVALID, {0} };

  ndims = av_len(dims)+1;
  PDL_Indx dest_dims[ndims];
  for (i=0; i<ndims; i++) {
     dest_dims[i] = SvIV(*(av_fetch(dims, ndims-1-i, 0))); /* reverse order */
  }

  if (dest_pdl == NULL)
     dest_pdl = pdl_pdlnew();
  if (!dest_pdl) return dest_pdl;
  pdl_error err = pdl_setdims (dest_pdl, dest_dims, ndims);
  if (err.error) return NULL;
  if (dtype == -1) {
    dtype = _detect_datatype(av);
  }
  dest_pdl->datatype = dtype;
  err = pdl_allocdata (dest_pdl);
  if (err.error) return NULL;
  err = pdl_make_physical(dest_pdl);
  if (err.error) return NULL;

  /******
   * Copy the undefval to fill empty spots in the ndarray...
   */
  PDLDEBUG_f(printf("pdl_from_array type: %d\n", dtype));
  ANYVAL_FROM_SV(undefval, NULL, TRUE, dtype);
#define X(dtype, ctype, ppsym, ...) \
    pdl_setav_ ## ppsym(dest_pdl->data,av,dest_dims,ndims,level, undefval.value.ppsym, dest_pdl);
  PDL_GENERICSWITCH(PDL_TYPELIST2_ALL, dtype, X, return NULL)
#undef X
  return dest_pdl;
}

/* Compute offset of (x,y,z,...) position in row-major list */
PDL_Indx pdl_get_offset(PDL_Indx* pos, PDL_Indx* dims, PDL_Indx *incs, PDL_Indx offset, PDL_Indx ndims) {
   PDL_Indx i;
   PDL_Indx result;
   for(i=0; i<ndims; i++) { /* Check */
      if(pos[i]<-dims[i] || pos[i]>=dims[i])
         return -1;
   }
   result = offset;
   for (i=0; i<ndims; i++) {
       result = result + (pos[i]+((pos[i]<0)?dims[i]:0))*incs[i];
   }
   return result;
}

/* wrapper for pdl_at where only want first item, cf sclr_c */
PDL_Anyval pdl_at0( pdl* it ) {
    PDL_Anyval result = { PDL_INVALID, {0} };
    PDL_Indx nullp = 0;
    PDL_Indx dummyd = 1;
    PDL_Indx dummyi = 1;
    pdl_error err = pdl_make_physvaffine( it );
    if (err.error) { return result; }
    if (it->nvals < 1) { return result; }
    return pdl_at(PDL_REPRP(it), it->datatype, &nullp, &dummyd,
            &dummyi, PDL_REPROFFS(it),1);
}

/* Return value at position (x,y,z...) */
PDL_Anyval pdl_at( void* x, int datatype, PDL_Indx* pos, PDL_Indx* dims,
	PDL_Indx* incs, PDL_Indx offset, PDL_Indx ndims) {
   PDL_Anyval result = { PDL_INVALID, {0} };
   PDL_Indx ioff = pdl_get_offset(pos, dims, incs, offset, ndims);
   if (ioff < 0) return result;
   ANYVAL_FROM_CTYPE_OFFSET(result, datatype, x, ioff);
   return result;
}

/* Set value at position (x,y,z...) */
pdl_error pdl_set( void* x, int datatype, PDL_Indx* pos, PDL_Indx* dims, PDL_Indx* incs, PDL_Indx offs, PDL_Indx ndims, PDL_Anyval value){
   pdl_error PDL_err = {0, NULL, 0};
   PDL_Indx ioff = pdl_get_offset(pos, dims, incs, offs, ndims);
   if (ioff < 0) return pdl_make_error_simple(PDL_EUSERERROR, "Position out of range");
   ANYVAL_TO_CTYPE_OFFSET(x, ioff, datatype, value);
   return PDL_err;
}

/*
 * pdl_kludge_copy_<type>  - copy a PDL into a part of a being-formed PDL.
 * It is only used by pdl_setav_<type>, to handle the case where a PDL is part
 * of the argument list. 
 *
 * kludge_copy recursively walks down the dim list of both the source and dest
 * pdls, copying values in as we go.  It differs from PP copy in that it operates
 * on only a portion of the output pdl.
 *
 * (If I were Lazier I would have popped up into the perl level and used broadcastloops to
 * assign to a slice of the output pdl -- but this is probably a little faster.)
 *
 * -CED 17-Jun-2004
 *
 * Arguments:
 * dest_off  is an integer indicating which element along the current direction is being treated (for padding accounting)
 * dest_data is a pointer into the destination PDL's data;
 * dest_dims is a pointer to the destination PDL's dim list;
 * ndims is the size of the destination PDL's dimlist;
 * level is the conjugate dimension along which copying is happening (indexes dest_dims).
 *    "conjugate" means that it counts backward through the dimension array.
 * stride is the increment in the data array corresponding to this dimension;
 *
 * pdl is the input PDL.
 * plevel is the dim number for the input PDL, which works in the same sense as level.
 *   It is offset to account for the difference in dimensionality between the input and
 *   output PDLs. It is allowed to be negative (which is equivalent to the "permissive
 *   slicing" that treats missing dimensions as present and having size 1), but should
 *   not match or exceed pdl->ndims. 
 * source_data is the current offset data pointer into pdl->data.
 *
 * Kludge-copy works backward through the dim lists, so that padding is simpler:  if undefval
 * padding is required at any particular dimension level, the padding occupies a contiguous
 * block of memory.
 */

#define INNERLOOP_X(datatype, ctype, ppsym, ...) \
      /* copy data (unless the source pointer is null) */ \
      i=0; \
      if(source_data && dest_data && pdlsiz) { \
        found_bad = 0; \
        for(; i<pdlsiz; i++) { \
          if(source_pdl->has_badvalue || (source_pdl->state & PDL_BADVAL)) { \
              /* Retrieve directly from .value.* instead of using ANYVAL_EQ_ANYVAL */ \
              if( ((ctype *)source_data)[i] == source_badval.value.ppsym || PDL_ISNAN_ ## ppsym(((ctype *)source_data)[i]) ) { \
                  /* bad value in source PDL -- use our own type's bad value instead */ \
                  ANYVAL_TO_CTYPE(dest_data[i], ctype, dest_badval); \
                  found_bad = 1; \
              } else { \
                  dest_data[i] = ((ctype *)source_data)[i]; \
              } \
          } else { \
            dest_data[i] = ((ctype *)source_data)[i]; \
          } \
        } /* end of loop over pdlsiz */ \
        if (found_bad) dest_pdl->state |= PDL_BADVAL; /* just once */ \
      } else {  \
        /* source_data or dest_data or pdlsiz are 0 */ \
        if(dest_data) \
          dest_data[i] = undefval; \
      } \
        /* pad out, in the innermost dimension */ \
      if( !oob ) { \
        undef_count += dest_dims[0]-dest_off-i; \
        for(; i< dest_dims[0]-dest_off; i++) dest_data[i] = undefval; \
      }

#define PDL_KLUDGE_COPY_X(X, datatype_out, ctype_out, ppsym_out, ...) \
PDL_Indx pdl_kludge_copy_ ## ppsym_out(PDL_Indx dest_off, /* Offset into the dest data array */ \
  ctype_out* dest_data,  /* Data pointer in the dest data array */ \
  PDL_Indx* dest_dims,/* Pointer to the dimlist for the dest pdl */ \
  PDL_Indx ndims,    /* Number of dimensions in the dest pdl */ \
  PDL_Indx level,    /* Recursion level */ \
  PDL_Indx stride,   /* Stride through memory for the current dim */ \
  pdl* source_pdl,   /* pointer to the source pdl */ \
  PDL_Indx plevel,   /* level within the source pdl */ \
  void* source_data, /* Data pointer in the source pdl */ \
  ctype_out undefval,/* undefval for the dest pdl */ \
  pdl* dest_pdl      /* pointer to the dest pdl */ \
) { \
  PDL_Indx i; \
  PDL_Indx undef_count = 0; \
  /* Can't copy into a level deeper than the number of dims in the output PDL */ \
  if(level > ndims ) { \
    fprintf(stderr,"pdl_kludge_copy: level=%"IND_FLAG"; ndims=%"IND_FLAG"\n",level,ndims); \
    croak("Internal error - please submit a bug report at https://github.com/PDLPorters/pdl/issues:\n  pdl_kludge_copy: Assertion failed; ndims-1-level (%"IND_FLAG") < 0!.",ndims-1-level); \
  } \
  if(level >= ndims - 1) { \
    /* We are in as far as we can go in the destination PDL, so direct copying is in order. */ \
    PDL_Indx pdldim = source_pdl->ndims - 1 - plevel;  /* which dim are we working in the source PDL? */ \
    PDL_Indx pdlsiz; \
    int oob = (ndims-1-level < 0);         /* out-of-bounds flag */ \
    /* Do bounds checking on the source dimension -- if we wander off the end of the \
     * dimlist, we are doing permissive-slicing kind of stuff (not enough dims in the \
     * source to fully account for the output dimlist); if we wander off the beginning, we \
     * are doing dimensional padding.  In either case, we just iterate once. \
     */ \
    if(pdldim < 0 || pdldim >= source_pdl->ndims) { \
      pdldim = (pdldim < 0) ? (0) : (source_pdl->ndims - 1); \
      pdlsiz = 1; \
    } else { \
      pdlsiz = source_pdl->dims[pdldim]; \
    } \
    /* This is used inside the switch in order to detect badvalues. */ \
    PDL_Anyval source_badval = pdl_get_pdl_badvalue(source_pdl); \
    if (source_badval.type < 0) barf("Error getting badvalue, type=%d", source_badval.type); \
    PDL_Anyval dest_badval = pdl_get_pdl_badvalue(dest_pdl); \
    if (dest_badval.type < 0) barf("Error getting badvalue, type=%d", dest_badval.type); \
    char found_bad = 0; \
    PDL_GENERICSWITCH(PDL_TYPELIST2_ALL_, source_pdl->datatype, X, croak("Not a known data type code=%d", source_pdl->datatype)) \
    return undef_count; \
  } \
  /* If we are here, we are not at the bottom level yet.  So walk \
   *  across this dim and handle copying one dim deeper via recursion. \
   *  The loop is placed in a convenience block so we can define the  \
   *  dimensional boundscheck flag -- that avoids having to evaluate the complex  \
   *  ternary expression for every loop iteration. \
   */ \
  PDL_Indx limit = \
    (plevel >= 0 &&  \
     (source_pdl->ndims - 1 - plevel >= 0) \
    ) \
    ? (source_pdl->dims[ source_pdl->ndims-1-plevel ]) \
    : 1; \
  for(i=0; i < limit ; i++) \
    undef_count += pdl_kludge_copy_ ## ppsym_out(0, dest_data + stride * i, \
      dest_dims, \
      ndims, \
      level+1, \
      stride / ((dest_dims[ndims-2-level]) ? (dest_dims[ndims-2-level]) : 1), \
      source_pdl, \
      plevel+1, \
      ((PDL_Byte *) source_data) + source_pdl->dimincs[source_pdl->ndims-1-plevel] * i * pdl_howbig(source_pdl->datatype), \
      undefval, \
      dest_pdl \
    ); \
  if(i >= dest_dims[ndims - 1 - level]) return undef_count; \
  /* pad the rest of this dim to zero if there are not enough elements in the source PDL... */ \
  PDL_Indx cursor, target; \
  cursor = i * stride; \
  target = dest_dims[ndims-1-level]*stride; \
  undef_count += target - cursor; \
  for (; cursor < target; cursor++) dest_data[cursor] = undefval; \
  return undef_count; \
}
PDL_TYPELIST2_ALL(PDL_KLUDGE_COPY_X, INNERLOOP_X)
#undef PDL_KLUDGE_COPY_X

/*
 * pdl_setav_<type> loads a new PDL with values from a Perl AV, another PDL, or
 * a mix of both.  Heterogeneous sizes are handled by padding the new PDL's
 * values out to size with the undefval.  It is only called by pdl_setav in Core.XS,
 * via the trampoline pdl_from_array just above. pdl_from_array dispatches execution
 * to pdl_setav_<type> according to the type of the destination PDL.
 *
 * The code is complicated by the "bag-of-stuff" nature of AVs.  We handle
 * Perl scalars, AVs, *and* PDLs (via pdl_kludge_copy).
 *
 *   -  dest_data is the data pointer from a PDL
 *   -  av is the array ref (or PDL) to use to fill the data with,
 *   -  dest_dims is the dimlist
 *   -  ndims is the size of the dimlist
 *   -  level is the recursion level, which is also the dimension that we are filling
 */
#define PDL_SETAV_X(X, datatype_out, ctype_out, ppsym_out, ...) \
PDL_Indx pdl_setav_ ## ppsym_out(ctype_out* dest_data, AV* av, \
                     PDL_Indx* dest_dims, PDL_Indx ndims, PDL_Indx level, ctype_out undefval, pdl *dest_pdl) \
{ \
  PDL_Indx cursz = dest_dims[ndims-1-level]; /* we go from the highest dim inward */ \
  PDL_Indx len = av_len(av); \
  PDL_Indx i,stride=1; \
  PDL_Indx undef_count = 0; \
  for (i=0;i<ndims-1-level;i++) { \
    stride *= dest_dims[i]; \
  } \
  for (i=0;i<=len;i++,dest_data += stride) { /* note len is actually highest index, not element count */ \
    /* Fetch the next value from the AV */ \
    SV **elp = av_fetch(av,i,0); \
    SV *el = (elp ? *elp : 0); \
    if ( el && SVavref(el) ) { \
      /* If the element was an AV ref, recurse to walk through that AV, one dim lower */ \
      undef_count += pdl_setav_ ## ppsym_out(dest_data, (AV *) SvRV(el), dest_dims, ndims, level+1, undefval, dest_pdl); \
 \
    } else if( el && SvROK(el) ) { \
      /* If the element was a ref but not an AV, then it should be a PDL */ \
      pdl *pdl; \
      if( !(pdl = pdl_SvPDLV(el)) ) { \
        /* The element is a non-PDL, non-AV ref.  Not allowed. */ \
        croak("Non-array, non-PDL element in list"); \
      } \
      /* The element was a PDL - use pdl_kludge_copy to copy it into the destination */ \
      pdl_barf_if_error(pdl_make_physical(pdl)); \
      PDL_Indx pddex = ndims - 2 - level; \
      PDL_Indx pd = (pddex >= 0 && pddex < ndims ? dest_dims[ pddex ] : 0); \
      if(!pd) \
          pd = 1; \
      undef_count += pdl_kludge_copy_ ## ppsym_out(0, dest_data,dest_dims,ndims, level+1, stride / pd , pdl, 0, pdl->data, undefval, dest_pdl); \
    } else { /* el==0 || SvROK(el)==0: this is a scalar or undef element */ \
      if( PDL_SV_IS_UNDEF(el) ) {  /* undef case */ \
        *dest_data = (ctype_out) undefval; \
        undef_count++; \
      } else {              /* scalar case */ \
        *dest_data = SvIOK(el) ? (ctype_out) SvIV(el) : (ctype_out) SvNV(el); \
      } \
      /* Pad dim if we are not deep enough */ \
      if(level < ndims-1) { \
        ctype_out *cursor = dest_data; \
        ctype_out *target = dest_data + stride; \
        undef_count += stride; \
        for( cursor++;  cursor < target; cursor++ ) \
          *cursor = (ctype_out)undefval; \
      } \
    } \
  } /* end of element loop through the supplied AV */ \
  /* in case this dim is incomplete set any remaining elements to the undefval */ \
  if(len < cursz-1 ) { \
    ctype_out *target = dest_data + stride * (cursz - 1 - len); \
    undef_count += target - dest_data; \
    for( ; dest_data < target; dest_data++ ) \
      *dest_data = (ctype_out) undefval; \
  } \
  /* If the Perl scalar PDL::debug is set, announce padding */ \
  if(level==0 && undef_count) { \
    if(SvTRUE(get_sv("PDL::debug",0))) { \
      fflush(stdout); \
      fprintf(stderr,"Warning: pdl_setav_" #ppsym_out " converted undef to $PDL::undefval (%g) %"IND_FLAG" time%s\\n",(double)undefval,undef_count,undef_count==1?"":"s"); \
      fflush(stderr); \
    } \
  } \
  return undef_count; \
}

PDL_TYPELIST2_ALL(PDL_SETAV_X, INNERLOOP_X)
#undef PDL_SETAV_X
#undef INNERLOOP_X

SV *pdl_hdr_copy(SV *hdrp) {
  /* call the perl routine _hdr_copy */
  dSP;
  ENTER;
  SAVETMPS;
  PUSHMARK(SP);
  XPUSHs( hdrp );
  PUTBACK;
  int count = call_pv("PDL::_hdr_copy",G_SCALAR);
  SPAGAIN;
  if (count != 1)
      croak("PDL::_hdr_copy didn\'t return a single value - please report this bug (B).");
  SV *retval = (SV *) POPs ;
  if (SvROK(retval))
      (void)SvREFCNT_inc(retval);
  FREETMPS;
  LEAVE;
  return retval;
}

/*
examine the various PDLs that form the output PDL,
and copy headers as necessary.  The first header found with the hdrcpy
bit set is used.  This used to do just a simple ref copy but now
it uses the perl routine PDL::_hdr_copy to do the dirty work.  That
routine makes a deep copy of the header.  Copies of the deep copy
are distributed to all the names of the ndarray that are not the source
of the header.  I believe that is the Right Thing to do but I could be
wrong.
Here's the flow:
  - Check the hdrcpy flag.  If it's set, then check the header
    to see if it exists.  If it does, we need to call the
    perl-land PDL::_hdr_copy routine.  There are some shenanigans
    to keep the return value from evaporating before we've had a
    chance to do our bit with it.
  - For each output argument in the function signature, try to put
    a reference to the new header into that argument's header slot.
    (For functions with multiple outputs, this produces multiple linked
    headers -- that could be Wrong; fixing it would require making
    yet more explicit copies!)
  - Remortalize the return value from PDL::_hdr_copy, so that we don't
    leak memory.
  --CED 12-Apr-2003
*/
void pdl_hdr_childcopy(pdl_trans *trans) {
  void *hdrp = NULL;
  char propagate_hdrcpy = 0;
  pdl_transvtable *vtable = trans->vtable;
  pdl **pdls = trans->pdls;
  PDL_Indx i;
  for (i=0; i<vtable->npdls; i++) {
    pdl *pdl = pdls[i];
    short flags = vtable->par_flags[i];
    if (!(flags & PDL_PARAM_ISTEMP) && (!(flags & PDL_PARAM_ISCREAT) ||
         ((flags & PDL_PARAM_ISCREAT) && !PDL_DIMS_FROM_TRANS(trans,pdl))) &&
        pdl->hdrsv && (pdl->state & PDL_HDRCPY)
    ) {
      hdrp = pdl->hdrsv;
      propagate_hdrcpy = ((pdl->state & PDL_HDRCPY) != 0);
      break;
    }
  }
  if (!hdrp) return;
  SV *hdr_copy = ((hdrp == &PL_sv_undef) ? &PL_sv_undef : pdl_hdr_copy(hdrp));
  /* Found the header -- now copy it into all the right places */
  for (i=0; i<vtable->npdls; i++) {
    pdl *pdl = pdls[i];
    short flags = vtable->par_flags[i];
    if (!(flags & PDL_PARAM_ISCREAT)) continue;
    if (pdl->hdrsv != hdrp) {
      if (pdl->hdrsv && pdl->hdrsv != &PL_sv_undef)
        (void)SvREFCNT_dec( pdl->hdrsv );
      if (hdr_copy != &PL_sv_undef) (void)SvREFCNT_inc(hdr_copy);
      pdl->hdrsv = hdr_copy;
    }
    if (propagate_hdrcpy) pdl->state |= PDL_HDRCPY;
  }
  if (hdr_copy != &PL_sv_undef)
    SvREFCNT_dec(hdr_copy); /* make hdr_copy mortal again */
}

void pdl_dump_slice_args(pdl_slice_args* args) {
  printf("slice_args (%p):\n", args);
  while (args) {
    printf("  start=%"IND_FLAG", end=%"IND_FLAG", inc=%"IND_FLAG", squish=%d, dummy=%d, next=%p\n",
      args->start, args->end, args->inc, args->squish, args->dummy, args->next
    );
    args = args->next;
  }
}

/****************************************************************/
/*** String handling part of slice is here.  Parse out each   ***/
/*** term:                                                    ***/
/***   <n> (or NV) - extract one element <n> from this dim    ***/
/***   <n>:<m>     - extract <n> to <m>; autoreverse if nec.  ***/
/***   <n>:<m>:<s> - extract <n> to <m>, stepping by <s>      ***/
/***  (<n>)        - extract element and discard this dim     ***/
/***  *<n>         - insert a dummy dimension of size <n>     ***/
/***  :            - keep this dim in its entirety            ***/
/***  X            - keep this dim in its entirety            ***/
/****************************************************************/
pdl_error pdl_slice_args_parse_string(char* s, pdl_slice_args *retvalp) {
  PDLDEBUG_f(printf("slice_args_parse_string input: '%s'\n", s));
  pdl_error PDL_error = {0, NULL, 0};
  int subargno = 0;
  char flagged = 0;
  char squish_closed = 0, squish_flag = 0, dummy_flag = 0;
  pdl_slice_args this_arg = {0,-1,0}; /* start,end,inc 0=do in RedoDims */
  PDL_Indx i = 0;
  while(*s) {
    if( isspace( *s ) ) {
      s++;  /* ignore and loop again */
      continue;
    }
    /* not whitespace */
    switch(*(s++)) {
      case '*':
        if(flagged || subargno)
          return pdl_make_error(PDL_EUSERERROR, "slice: Erroneous '*' (arg %d)",i);
        dummy_flag = flagged = 1;
        this_arg.start = 1;  /* default this number to 1 (size 1); '*0' yields an empty */
        this_arg.end = 1;  /* no second arg allowed - default to 1 so start is element count */
        this_arg.inc = -1; /* -1 so we count down to end from start */
        break;
      case '(':
        if(flagged || subargno)
          return pdl_make_error(PDL_EUSERERROR, "slice: Erroneous '(' (arg %d)",i);
        squish_flag = flagged = 1;
        break;
      case 'X': case 'x':
        if(flagged || subargno > 1)
          return pdl_make_error(PDL_EUSERERROR, "slice: Erroneous 'X' (arg %d)",i);
        if(subargno==0) {
          flagged = 1;
        } else /* subargno is 1 - squish */ {
          squish_flag = squish_closed = flagged = 1;
        }
        break;
      case '+': case '-':
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        switch(subargno) {
          case 0: /* first arg - change default to 1 element */
            this_arg.end = this_arg.start = strtoll(--s, &s, 10);
            if(dummy_flag)
              this_arg.start = 1;
            break;
          case 1: /* second arg - parse and keep end */
            this_arg.end = strtoll(--s, &s, 10);
            break;
          case 2: /* third arg - parse and keep inc */
            if ( squish_flag || dummy_flag )
              return pdl_make_error(PDL_EUSERERROR, "slice: erroneous third field in slice specifier (arg %d)",i);
            this_arg.inc = strtoll(--s, &s, 10);
            break;
          default: /* oops */
            return pdl_make_error(PDL_EUSERERROR, "slice: too many subargs in scalar slice specifier %d",i);
            break;
        }
        break;
      case ')':
        if( squish_closed || !squish_flag || subargno > 0)
          return pdl_make_error(PDL_EUSERERROR, "slice: erroneous ')' (arg %d)",i);
        squish_closed = 1;
        break;
      case ':':
        if(squish_flag && !squish_closed)
          return pdl_make_error(PDL_EUSERERROR, "slice: must close squishing parens (arg %d)",i);
        if( subargno == 0 )
          this_arg.end = -1;   /* Set "<n>:" default to get the rest of the range */
        if( subargno > 1 )
          return pdl_make_error(PDL_EUSERERROR, "slice: too many ':'s in scalar slice specifier %d",i);
        subargno++;
        break;
      case ',':
        return pdl_make_error_simple(PDL_EUSERERROR, "slice: ','  not allowed (yet) in scalar slice specifiers!");
        break;
      default:
        return pdl_make_error(PDL_EUSERERROR, "slice: unexpected '%c' in slice specifier (arg %d)",*s,i);
        break;
    }
    i++;
  } /* end of parse loop */
  this_arg.squish = squish_flag;
  this_arg.dummy = dummy_flag;
  PDLDEBUG_f(pdl_dump_slice_args(&this_arg));
  *retvalp = this_arg;
  return PDL_error;
}

pdl_slice_args* pdl_slice_args_parse_sv(SV* sv) {
  /*** Make sure we got an array ref as input and extract its corresponding AV ***/
  if(!(sv && SvROK(sv) && SvTYPE(SvRV(sv))==SVt_PVAV))
    barf("slice requires an ARRAY ref containing zero or more arguments");
  pdl_slice_args* retval = NULL, *this_arg_ptr = NULL;
  AV *arglist = (AV *)(SvRV(sv));
  /* Detect special case of a single comma-delimited string; in that case, */
  /* split out our own arglist.                                            */
  if (av_len(arglist) == 0) {
    /***   single-element list: pull first element ***/
    SV **svp = av_fetch(arglist, 0, 0);
    if(svp && *svp && *svp != &PL_sv_undef && SvPOKp(*svp)) {
      /*** The element exists and is not undef and has a cached string value ***/
      char *s,*ss;
      s = ss = SvPVbyte_nolen(*svp);
      for(;  *ss && *ss != ',';  ss++) {}
      if(*ss == ',') {
        char *s1;
        /* the string contains at least one comma.  ATTACK!      */
        /* We make a temporary array and populate it with        */
        /* SVs containing substrings -- basically split(/\,/)... */
        AV *al = (AV *)sv_2mortal((SV *)(newAV()));
        do {
          for(s1=s; *s1 && *s1 != ','; s1++);
          av_push(al, newSVpvn(s, s1-s));
          s = (*s1==',') ? ++s1 : s1;
        } while(*s);
        arglist = al; /* al is ephemeral and will evaporate at the next perl gc */
      } /* end of contains-comma case */
    } /* end of nontrivial single-element detection */
  }/* end of single-element detection */
  PDL_Indx nargs = av_len( arglist ) + 1;
  /**********************************************************************/
  /**** Loop over the elements of the AV input and parse into values ****/
  /**** in the start/inc/end array                                   ****/
  PDL_Indx i;
  for(i=0; i<nargs; i++) {
    char all_flag = 0;
    pdl_slice_args this_arg = {0,-1,0}; /* start,end,inc 0=do in RedoDims */
    SV **thisp = av_fetch( arglist, i, 0 );
    SV *this = (thisp  ?  *thisp  : 0 );
    /** Keep the whole dimension if the element is undefined or missing **/
    all_flag = (  (!this)   ||   (this==&PL_sv_undef)  );
    if(!all_flag) {
      /* Main branch -- this element is not an empty string */
      if(SvROK(this)) {
        /*** It's a reference - it better be an array ref. ***/
        if( SvTYPE(SvRV(this)) != SVt_PVAV )
          barf("slice: non-ARRAY ref in the argument list!");
        /*** It *is* an array ref!  Expand it into an AV so we can read it. ***/
        AV *sublist = (AV *)(SvRV(this));
        int nelem = !sublist ? 0 : av_len(sublist) + 1;
        if(nelem > 3) barf("slice: array refs can have at most 3 elements!");
        if(nelem==0) {      /* No elements - keep it all */
          all_flag = 1;
        } else /* Got at least one element */{
          /* Load the first into start and check for dummy or all-clear */
          /* (if element is missing use the default value already in start) */
          SV **svp = av_fetch(sublist, 0, 0);
          if(svp && *svp && *svp != &PL_sv_undef) {
            /* There is a first element.  Check if it's a string, then an IV */
            if( SvPOKp(*svp)) {
              char *str = SvPVbyte_nolen(*svp);
              switch(*str) {
                case 'X':
                  all_flag = 1; break;
                case '*':
                  this_arg.dummy = 1;
                  this_arg.start = 1;    /* start is 1 so 2nd field is element count */
                  this_arg.end = 1;    /* size defaults to 1 for dummy dims */
                  this_arg.inc = 1;    /* inc is forced to 1 so ['*',0] gives an empty */
                  break;
                default:     /* Doesn't start with '*' or 'X' */
                  this_arg.end = this_arg.start = SvIV(*svp); /* end defaults to start if start is present */
                  break;
              }
            } else /* the element has no associated string - just parse */ {
              this_arg.end = this_arg.start = SvIV(*svp); /* end defaults to start if start is present */
            }
          } /* end of defined check.  if it's undef, leave the n's at their default value. */
          /* Read the second element into end and check for alternate squish syntax */
          if( (nelem > 1) && (!all_flag) ) {
            svp = av_fetch(sublist, 1, 0);
            if( svp && *svp && *svp != &PL_sv_undef ) {
              if( SvPOKp(*svp) ) {
                /* Second element has a string - make sure it's not 'X'. */
                char *str = SvPVbyte_nolen(*svp);
                if(*str == 'X') {
                  this_arg.squish = 1;
                  this_arg.end = this_arg.start;
                } else {
                  this_arg.end = SvIV(*svp);
                }
              } else {
                /* Not a PDL, no string -- just get the IV */
                this_arg.end = SvIV(*svp);
              }
            } /* If not defined, leave at the default */
          } /* End of second-element check */
          /*** Now try to read the third element (inc).  ***/
          if ((nelem > 2) && !(all_flag) && !(this_arg.squish) && !(this_arg.dummy)) {
            svp = av_fetch(sublist, 2, 0);
            if ( svp && *svp && *svp != &PL_sv_undef ) {
              STRLEN len;
              SvPV( *svp, len );
              if(len>0) {           /* nonzero length -> actual value given */
                this_arg.inc = SvIV(*svp);    /* if the step is passed in as 0, it is a squish */
                if(this_arg.inc==0) {
                  this_arg.end = this_arg.start;
                  this_arg.squish = 1;
                }
              }
            } /* end of nontrivial third-element parsing */
          } /* end of third-element parsing  */
        } /* end of nontrivial sublist parsing */
      } else /* this argument is not an ARRAY ref - parse as a scalar */ {
        if(SvPOKp(this)) {
          /* this argument has a cached string */
          STRLEN len;
          char *s = SvPVbyte(this, len);
          pdl_barf_if_error(pdl_slice_args_parse_string(s, &this_arg));
        } else /* end of string parsing */ {
          /* Simplest case -- there's no cached string, so it   */
          /* must be a number.  In that case it's a simple      */
          /* extraction.  Treated as a separate case for speed. */
          this_arg.end = this_arg.start = SvNV(this);
          this_arg.inc = 0;
        }
      } /* end of scalar handling */
    } /* end of defined-element handling (!all_flag) */
    if( (!all_flag) + (!this_arg.squish) + (!this_arg.dummy) < 2 )
      barf("Looks like you triggered a bug in  slice.  two flags set in dim %d",i);
    /* Force all_flag case to be a "normal" slice */
    if(all_flag) {
      this_arg.start = 0;
      this_arg.end = -1;
      this_arg.inc = 1;
    }
    if (!retval)
      this_arg_ptr = retval = pdl_smalloc(sizeof(*retval));
    else {
      this_arg_ptr = this_arg_ptr->next = pdl_smalloc(sizeof(*retval));
    }
    if (!this_arg_ptr) croak("Out of Memory\n");
    /* Copy parsed values into the limits */
    *this_arg_ptr = this_arg;
  } /* end of arg-parsing loop */
  PDLDEBUG_f(pdl_dump_slice_args(retval));
  return retval;
}

/* pdl_seed() - prefix as "seed" #define-d by Perl
 *
 * Used to seed PDL's built-in RNG.
 */
uint64_t pdl_pdl_seed() {
	/* This implementation is from section 7.1 Seeding of
	 *
	 * Helmut G. Katzgraber. "Random Numbers in Scientific Computing:
	 * An Introduction". <https://arxiv.org/abs/1005.4117v1>.
	 */
	uint64_t s, pid;
	/* Start of Perl-specific symbols */
	Time_t seconds;
	pid = (uint64_t)PerlProc_getpid();
	(void)time(&seconds);
	/* End of Perl-specific symbols */
	s = (uint64_t)seconds;
	return ((s*181)*((pid-83)*359))%104729;
}

/* Pack strings array - returns strings[] (pdl_smalloced) and nstrings */
char ** pdl_packstrings ( SV* sv, PDL_Indx *nstrings ) {
   if (!(SvROK(sv) && SvTYPE(SvRV(sv))==SVt_PVAV))  /* Test */
       return NULL;
   AV *array = (AV *) SvRV(sv);   /* dereference */
   *nstrings = (PDL_Indx) av_len(array) + 1;  /* Number of entities */
   char **strings = pdl_smalloc( (*nstrings) * sizeof(*strings) ); /* Array space */
   if (strings == NULL) return NULL;
   PDL_Indx i;
   for(i=0; i<(*nstrings); i++) {
      strings[i] = SvPV_nolen(*(av_fetch( array, i, 0 )));
   }
   return strings;
}
