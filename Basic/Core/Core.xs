#ifndef WIN32
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>
#define USE_MMAP
#endif

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */

#if defined(CONTEXT)
#undef CONTEXT
#endif

#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#if BADVAL
#  if !BADVAL_USENAN
#include <float.h>
#  endif
#include <limits.h>
#endif

/* Return a integer or numeric scalar as approroate */

#define setflag(reg,flagval,val) (val?(reg |= flagval):(reg &= ~flagval))

Core PDL; /* Struct holding pointers to shared C routines */

#ifdef FOO
Core *pdl__Core_get_Core() /* INTERNAL TO CORE! DONT CALL FROM OUTSIDE */
{
	return PDL;
}
#endif

int pdl_debugging=0;
int pdl_autopthread_targ   = 0; /* No auto-pthreading unless set using the set_autopthread_targ */
int pdl_autopthread_actual = 0;
int pdl_autopthread_size   = 1;

#define CHECKP(p)    if ((p) == NULL) croak("Out of memory")

static PDL_Indx* pdl_packint( SV* sv, int *ndims ) {

   SV*  bar;
   AV*  array;
   int i;
   PDL_Indx *dims;

   if (!(SvROK(sv) && SvTYPE(SvRV(sv))==SVt_PVAV))  /* Test */
       return NULL;
   array = (AV *) SvRV(sv);   /* dereference */
     *ndims = (int) av_len(array) + 1;  /* Number of dimensions */
   /* Array space */
   dims = (PDL_Indx *) pdl_malloc( (*ndims) * sizeof(*dims) );
   CHECKP(dims);

   for(i=0; i<(*ndims); i++) {
      bar = *(av_fetch( array, i, 0 )); /* Fetch */
      dims[i] = (PDL_Indx) SvIV(bar);
   }
   return dims;
}

static SV* pdl_unpackint ( PDL_Indx *dims, int ndims ) {

   AV*  array;
   int i;

   array = newAV();

   for(i=0; i<ndims; i++) /* if ndims == 0, nothing stored -> ok */
         av_store( array, i, newSViv( (IV)dims[i] ) );

   return (SV*) array;
}

/*
 * Free the data if possible; used by mmapper
 * Moved from pdlhash.c July 10 2006 DJB
 */
static void pdl_freedata (pdl *a) {
	if(a->datasv) {
		SvREFCNT_dec(a->datasv);
		a->datasv=0;
		a->data=0;
	} else if(a->data) {
		die("Trying to free data of untouchable (mmapped?) pdl");
	}
}

#if BADVAL

#ifdef FOOFOO_PROPAGATE_BADFLAG

/*
 * this seems to cause an infinite loop in between tests 42 & 43 of
 * t/bad.t - ie
 *
 * $a = sequence( byte, 2, 3 );
 * $b = $a->slice("(1),:");
 * my $mask = sequence( byte, 2, 3 );
 * $mask = $mask->setbadif( ($mask % 3) == 2 );
 * print "a,b == ", $a->badflag, ",", $b->badflag, "\n";
 * $a->inplace->copybad( $mask );                          <-- think this is the call
 * print "a,b == ", $a->badflag, ",", $b->badflag, "\n";
 * print "$a $b\n";
 * ok( $b->badflag, 1 );
 *
 */

/* used by propagate_badflag() */

void propagate_badflag_children( pdl *it, int newval ) {
    PDL_DECL_CHILDLOOP(it)
    PDL_START_CHILDLOOP(it)
    {
	pdl_trans *trans = PDL_CHILDLOOP_THISCHILD(it);
	int i;

	for( i = trans->vtable->nparents;
	     i < trans->vtable->npdls;
	     i++ ) {

	    pdl *child = trans->pdls[i];

	    if ( newval ) child->state |=  PDL_BADVAL;
            else          child->state &= ~PDL_BADVAL;

	    /* make sure we propagate to grandchildren, etc */
	    propagate_badflag_children( child, newval );

        } /* for: i */
    }
    PDL_END_CHILDLOOP(it)
} /* propagate_badflag_children */

/* used by propagate_badflag() */

void propagate_badflag_parents( pdl *it ) {
    PDL_DECL_CHILDLOOP(it)
    PDL_START_CHILDLOOP(it)
    {
	pdl_trans *trans = PDL_CHILDLOOP_THISCHILD(it);
	int i;

	for( i = 0;
	     i < trans->vtable->nparents;
	     i++ ) {

	    pdl *parent = trans->pdls[i];

	    /* only sets allowed here */
	    parent->state |= PDL_BADVAL;

	    /* make sure we propagate to grandparents, etc */
	    propagate_badflag_parents( parent );

        } /* for: i */
    }
    PDL_END_CHILDLOOP(it)
} /* propagate_badflag_parents */

/*
 * we want to change the bad flag of the children
 * (newval = 1 means set flag, 0 means clear it).
 * If newval == 1, then we also loop through the
 * parents, setting their bad flag
 *
 * thanks to Christian Soeller for this
 */

void propagate_badflag( pdl *it, int newval ) {
   /* only do anything if the flag has changed - do we need this check ? */
   if ( newval ) {
      if ( (it->state & PDL_BADVAL) == 0 ) {
         propagate_badflag_parents( it );
         propagate_badflag_children( it, newval );
      }
   } else {
      if ( (it->state & PDL_BADVAL) > 0 ) {
         propagate_badflag_children( it, newval );
      }

   }

} /* propagate_badflag */

#else        /* FOOFOO_PROPAGATE_BADFLAG */

/* newval = 1 means set flag, 0 means clear it */
/* thanks to Christian Soeller for this */

void propagate_badflag( pdl *it, int newval ) {
    PDL_DECL_CHILDLOOP(it)
    PDL_START_CHILDLOOP(it)
    {
	pdl_trans *trans = PDL_CHILDLOOP_THISCHILD(it);
	int i;

	for( i = trans->vtable->nparents;
	     i < trans->vtable->npdls; i++ ) {

	    pdl *child = trans->pdls[i];

	    if ( newval ) child->state |=  PDL_BADVAL;
            else          child->state &= ~PDL_BADVAL;

	    /* make sure we propagate to grandchildren, etc */
	    propagate_badflag( child, newval );

        } /* for: i */
    }
    PDL_END_CHILDLOOP(it)
} /* propagate_badflag */

#endif    /* FOOFOO_PROPAGATE_BADFLAG */

void propagate_badvalue( pdl *it ) {
    PDL_DECL_CHILDLOOP(it)
    PDL_START_CHILDLOOP(it)
    {
	pdl_trans *trans = PDL_CHILDLOOP_THISCHILD(it);
	int i;

	for( i = trans->vtable->nparents;
	     i < trans->vtable->npdls; i++ ) {

	    pdl *child = trans->pdls[i];

            child->has_badvalue = 1;
            child->badvalue = it->badvalue;

	    /* make sure we propagate to grandchildren, etc */
	    propagate_badvalue( child );

        } /* for: i */
    }
    PDL_END_CHILDLOOP(it)
} /* propagate_badvalue */


/* this is horrible - the routines from bad should perhaps be here instead ? */
PDL_Anyval pdl_get_badvalue( int datatype ) {
    PDL_Anyval retval = { -1, 0 };
    switch ( datatype ) {

#include "pdldataswitch.c"

      default:
	croak("Unknown type sent to pdl_get_badvalue\n");
    }
    return retval;
} /* pdl_get_badvalue() */


PDL_Anyval pdl_get_pdl_badvalue( pdl *it ) {
    PDL_Anyval retval = { -1, 0 };
    int datatype;

#if BADVAL_PER_PDL
    if (it->has_badvalue) {
        retval = it->badvalue;
    } else {
        datatype = it->datatype;
        retval = pdl_get_badvalue( datatype );
    }
#else
    datatype = it->datatype;
    retval = pdl_get_badvalue( datatype );
#endif
    return retval;
} /* pdl_get_pdl_badvalue() */

#endif

MODULE = PDL::Core     PACKAGE = PDL


# Destroy a PDL - note if a hash do nothing, the $$x{PDL} component
# will be destroyed anyway on a separate call

void
DESTROY(sv)
  SV *	sv;
  PREINIT:
    pdl *self;
  CODE:
    if (  !(  (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVHV) )  ) {
       self = SvPDLV(sv);
       PDLDEBUG_f(printf("DESTROYING %p\n",(void*)self);)
       if (self != NULL)
          pdl_destroy(self);
    }

# Return the transformation object or an undef otherwise.

SV *
get_trans(self)
	pdl *self;
	CODE:
	ST(0) = sv_newmortal();
	if(self->trans)  {
		sv_setref_pv(ST(0), "PDL::Trans", (void*)(self->trans));
	} else {
               ST(0) = &PL_sv_undef;
	}


MODULE = PDL::Core	PACKAGE = PDL

int
iscontig(x)
   pdl*	x
   CODE:
      RETVAL = 1;
      pdl_make_physvaffine( x );
	if PDL_VAFFOK(x) {
	   int i;
           PDL_Indx inc=1;
	   PDLDEBUG_f(printf("vaff check...\n");)
  	   for (i=0;i<x->ndims;i++) {
     	      if (PDL_REPRINC(x,i) != inc) {
		     RETVAL = 0;
		     break;
              }
     	      inc *= x->dims[i];
  	   }
        }
  OUTPUT:
    RETVAL

# using "perl" not $^X because that doesn't work on "perl in space"
# TODO: switching back to $^X since using "perl" is not a viable fix
INCLUDE_COMMAND: $^X -e "require q{./Dev.pm}; PDL::Core::Dev::generate_core_flags()"

#if 0
=begin windows_mmap

I found this at http://mollyrocket.com/forums/viewtopic.php?p=2529&sid=973b8e0a1e639e3008d7ef05f686c6fa
and thougt we might consider using it to make windows mmapping possible.

-David Mertens

 /*
 This code was placed in the public domain by the author,
 Sean Barrett, in November 2007. Do with it as you will.
 (Seee the page for stb_vorbis or the mollyrocket source
 page for a longer description of the public domain non-license).
 */

 #define WIN32_LEAN_AND_MEAN
 #include <windows.h>

 typedef struct
 {
    HANDLE f;
    HANDLE m;
    void *p;
 } SIMPLE_UNMMAP;

 // map 'filename' and return a pointer to it. fill out *length and *un if not-NULL
 void *simple_mmap(const char *filename, int *length, SIMPLE_UNMMAP *un)
 {
    HANDLE f = CreateFile(filename, GENERIC_READ, FILE_SHARE_READ,  NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    HANDLE m;
    void *p;
    if (!f) return NULL;
    m = CreateFileMapping(f, NULL, PAGE_READONLY, 0,0, NULL);
    if (!m) { CloseHandle(f); return NULL; }
    p = MapViewOfFile(m, FILE_MAP_READ, 0,0,0);
    if (!p) { CloseHandle(m); CloseHandle(f); return NULL; }
    if (n) *n = GetFileSize(f, NULL);
    if (un) {
       un->f = f;
       un->m = m;
       un->p = p;
    }
    return p;
 }

 void simple_unmmap(SIMPLE_UNMMAP *un)
 {
    UnmapViewOfFile(un->p);
    CloseHandle(un->m);
    CloseHandle(un->f);
 }

=end windows_mmap

=cut

#endif /* 0 - commented out */

void
set_inplace(self,val)
  pdl *self;
  int val;
  CODE:
    setflag(self->state,PDL_INPLACE,val);

IV
address(self)
  pdl *self;
  CODE:
    RETVAL = PTR2IV(self);
  OUTPUT:
    RETVAL

pdl *
pdl_hard_copy(src)
	pdl *src;

pdl *
sever(src)
	pdl *src;
	CODE:
		if(src->trans) {
			pdl_make_physvaffine(src);
			pdl_destroytransform(src->trans,1);
		}
		RETVAL=src;
	OUTPUT:
		RETVAL

int
set_data_by_mmap(it,fname,len,shared,writable,creat,mode,trunc)
	pdl *it
	char *fname
	STRLEN len
	int writable
	int shared
	int creat
	int mode
	int trunc
	CODE:
#ifdef USE_MMAP
       int fd;
       pdl_freedata(it);
       fd = open(fname,(writable && shared ? O_RDWR : O_RDONLY)|
               (creat ? O_CREAT : 0),mode);
       if(fd < 0) {
               croak("Error opening file");
       }
       if(trunc) {
               int error = ftruncate(fd,0);   /* Clear all previous data */
               
               if(error)
               {
					fprintf(stderr,"Failed to set length of '%s' to %d. errno=%d",fname,(int)len,(int)error);
					croak("set_data_by_mmap: first ftruncate failed");
               }
               
               error = ftruncate(fd,len); /* And make it long enough */
               
               if(error)
               {
					fprintf(stderr,"Failed to set length of '%s' to %d. errno=%d",fname,(int)len,(int)error);
					croak("set_data_by_mmap: second ftruncate failed");
               }
       }
       if(len) {
		it->data = mmap(0,len,PROT_READ | (writable ?
					PROT_WRITE : 0),
				(shared ? MAP_SHARED : MAP_PRIVATE),
				fd,0);
		if(!it->data)
			croak("Error mmapping!");
       } else {
               /* Special case: zero-length file */
               it->data = NULL;
       }
       PDLDEBUG_f(printf("PDL::MMap: mapped to %p\n",it->data);)
       it->state |= PDL_DONTTOUCHDATA | PDL_ALLOCATED;
       pdl_add_deletedata_magic(it, pdl_delete_mmapped_data, len);
       close(fd);
#else
	croak("mmap not supported on this architecture");
#endif
       RETVAL = 1;
OUTPUT:
       RETVAL

int
set_state_and_add_deletedata_magic(it,len)
      pdl *it
      STRLEN len
      CODE:
            it->state |= PDL_DONTTOUCHDATA | PDL_ALLOCATED;
            pdl_add_deletedata_magic(it, pdl_delete_mmapped_data, len);
            RETVAL = 1;
      OUTPUT:
            RETVAL

int
set_data_by_offset(it,orig,offset)
      pdl *it
      pdl *orig
      STRLEN offset
      CODE:
              pdl_freedata(it);
              it->data = ((char *) orig->data) + offset;
	      it->datasv = orig->sv;
              (void)SvREFCNT_inc(it->datasv);
              it->state |= PDL_DONTTOUCHDATA | PDL_ALLOCATED;
              RETVAL = 1;
      OUTPUT:
              RETVAL

PDL_Indx
nelem(x)
	pdl *x
	CODE:
		pdl_make_physdims(x);
		RETVAL = x->nvals;
	OUTPUT:
		RETVAL

# Convert PDL to new datatype (called by float(), int() etc.)

# SV *
# convert(a,datatype)
#    pdl*	a
#    int	datatype
#    CODE:
#     pdl* b;
#     pdl_make_physical(a);
#     RETVAL = pdl_copy(a,""); /* Init value to return */
#     b = SvPDLV(RETVAL);      /* Map */
#     pdl_converttype( &b, datatype, PDL_PERM );
#     PDLDEBUG_f(printf("converted %d, %d, %d, %d\n",a, b, a->datatype, b->datatype));

#     OUTPUT:
#      RETVAL


# Call my howbig function

int
howbig_c(datatype)
   int	datatype
   CODE:
     RETVAL = pdl_howbig(datatype);
   OUTPUT:
     RETVAL


int
set_autopthread_targ(i)
	int i;
	CODE:
	RETVAL = i;
	pdl_autopthread_targ = i;
	OUTPUT:
	RETVAL

int
get_autopthread_targ()
	CODE:
	RETVAL = pdl_autopthread_targ;
	OUTPUT:
	RETVAL


int
set_autopthread_size(i)
	int i;
	CODE:
	RETVAL = i;
	pdl_autopthread_size = i;
	OUTPUT:
	RETVAL

int
get_autopthread_size()
	CODE:
	RETVAL = pdl_autopthread_size;
	OUTPUT:
	RETVAL

int
get_autopthread_actual()
	CODE:
	RETVAL = pdl_autopthread_actual;
	OUTPUT:
	RETVAL

MODULE = PDL::Core     PACKAGE = PDL::Core

unsigned int
is_scalar_SvPOK(arg)
	SV* arg;
	CODE:
	RETVAL = SvPOK(arg);
	OUTPUT:
	RETVAL


int
set_debugging(i)
	int i;
	CODE:
	RETVAL = pdl_debugging;
	pdl_debugging = i;
	OUTPUT:
	RETVAL



SV *
sclr_c(it)
   pdl* it
   PREINIT:
	PDL_Indx nullp = 0;
	PDL_Indx dummyd = 1;
	PDL_Indx dummyi = 1;
	PDL_Anyval result = { -1, 0 };
   CODE:
        /* get the first element of a piddle and return as
         * Perl scalar (autodetect suitable type IV or NV)
         */
        pdl_make_physvaffine( it );
	if (it->nvals < 1)
           croak("piddle must have at least one element");
	/* offs = PDL_REPROFFS(it); */
        /* result = pdl_get_offs(PDL_REPRP(it),offs); */
        result=pdl_at(PDL_REPRP(it), it->datatype, &nullp, &dummyd,
        &dummyi, PDL_REPROFFS(it),1);
        ANYVAL_TO_SV(RETVAL, result);

    OUTPUT:
        RETVAL


SV *
at_c(x,position)
   pdl*	x
   SV*	position
   PREINIT:
    PDL_Indx * pos;
    int npos;
    int ipos;
    PDL_Anyval result = { -1, 0 };
   CODE:
    pdl_make_physvaffine( x );

    pos = pdl_packdims( position, &npos);

    if (pos == NULL || npos < x->ndims)
       croak("Invalid position");

    /*  allow additional trailing indices
     *  which must be all zero, i.e. a
     *  [3,1,5] piddle is treated as an [3,1,5,1,1,1,....]
     *  infinite dim piddle
     */
    for (ipos=x->ndims; ipos<npos; ipos++)
      if (pos[ipos] != 0)
         croak("Invalid position");

    result=pdl_at(PDL_REPRP(x), x->datatype, pos, x->dims,
        (PDL_VAFFOK(x) ? x->vafftrans->incs : x->dimincs), PDL_REPROFFS(x),
	x->ndims);

    ANYVAL_TO_SV(RETVAL, result);

    OUTPUT:
     RETVAL

SV *
at_bad_c(x,position)
   pdl*	x
   SV *	position
   PREINIT:
    PDL_Indx * pos;
    int npos;
    int ipos;
    int badflag;
    PDL_Anyval result = { -1, 0 };
   CODE:
    pdl_make_physvaffine( x );

    pos = pdl_packdims( position, &npos);

    if (pos == NULL || npos < x->ndims)
       croak("Invalid position");

    /*  allow additional trailing indices
     *  which must be all zero, i.e. a
     *  [3,1,5] piddle is treated as an [3,1,5,1,1,1,....]
     *  infinite dim piddle
     */
    for (ipos=x->ndims; ipos<npos; ipos++)
      if (pos[ipos] != 0)
         croak("Invalid position");

    result=pdl_at(PDL_REPRP(x), x->datatype, pos, x->dims,
        (PDL_VAFFOK(x) ? x->vafftrans->incs : x->dimincs), PDL_REPROFFS(x),
	x->ndims);
#if BADVAL
   badflag = (x->state & PDL_BADVAL) > 0;
#  if BADVAL_USENAN
   /* do we have to bother about NaN's? */
   if ( badflag &&
        ( ( x->datatype < PDL_F && ANYVAL_EQ_ANYVAL(result, pdl_get_badvalue(x->datatype)) ) ||
          ( x->datatype == PDL_F && finite(result.value.F) == 0 ) ||
          ( x->datatype == PDL_D && finite(result.value.D) == 0 )
        )
      ) {
	 RETVAL = newSVpvn( "BAD", 3 );
   } else
#  else
   if ( badflag &&
        ANYVAL_EQ_ANYVAL( result, pdl_get_badvalue( x->datatype ) )
      ) {
	 RETVAL = newSVpvn( "BAD", 3 );
   } else
#  endif
#endif

    ANYVAL_TO_SV(RETVAL, result);

    OUTPUT:
     RETVAL


void
list_c(x)
	pdl *x
      PREINIT:
	PDL_Indx *inds;
      PDL_Indx *incs;
      PDL_Indx offs;
	void *data;
	int ind;
	int stop = 0;
	SV *sv;
	PPCODE:
      pdl_make_physvaffine( x );
	inds = pdl_malloc(sizeof(PDL_Indx) * x->ndims); /* GCC -> on stack :( */

	data = PDL_REPRP(x);
	incs = (PDL_VAFFOK(x) ? x->vafftrans->incs : x->dimincs);
	offs = PDL_REPROFFS(x);
	EXTEND(sp,x->nvals);
	for(ind=0; ind < x->ndims; ind++) inds[ind] = 0;
	while(!stop) {
		PDL_Anyval pdl_val = { -1, 0 };
		pdl_val = pdl_at( data, x->datatype, inds, x->dims, incs, offs, x->ndims);
		ANYVAL_TO_SV(sv,pdl_val);
		PUSHs(sv_2mortal(sv));
		stop = 1;
		for(ind = 0; ind < x->ndims; ind++)
			if(++(inds[ind]) >= x->dims[ind])
				inds[ind] = 0;
			else
				{stop = 0; break;}
	}

# returns the string 'BAD' if an element is bad
#

SV *
listref_c(x)
   pdl *x
  PREINIT:
   PDL_Indx * inds;
   PDL_Indx * incs;
   PDL_Indx offs;
   void *data;
   int ind;
   int lind;
   int stop = 0;
   AV *av;
   SV *sv;
   PDL_Anyval pdl_val =    { -1, 0 };
   PDL_Anyval pdl_badval = { -1, 0 };
  CODE:
#if BADVAL
    /*
    # note:
    #  the badvalue is stored in a PDL_Anyval, but that's what pdl_at()
    #  returns
    */

   int badflag = (x->state & PDL_BADVAL) > 0;
#  if BADVAL_USENAN
    /* do we have to bother about NaN's? */
   if ( badflag && x->datatype < PDL_F ) {
      pdl_badval = pdl_get_pdl_badvalue( x );
   }
#  else
   if ( badflag ) {
      pdl_badval = pdl_get_pdl_badvalue( x );
   }
#  endif
#endif

   pdl_make_physvaffine( x );
   inds = pdl_malloc(sizeof(PDL_Indx) * x->ndims); /* GCC -> on stack :( */
   data = PDL_REPRP(x);
   incs = (PDL_VAFFOK(x) ? x->vafftrans->incs : x->dimincs);
   offs = PDL_REPROFFS(x);
   av = newAV();
   av_extend(av,x->nvals);
   lind=0;
   for(ind=0; ind < x->ndims; ind++) inds[ind] = 0;
   while(!stop) {
#if BADVAL
      pdl_val = pdl_at( data, x->datatype, inds, x->dims, incs, offs, x->ndims );
      if ( badflag && 
#  if BADVAL_USENAN
        ( (x->datatype < PDL_F && ANYVAL_EQ_ANYVAL(pdl_val, pdl_badval)) ||
          (x->datatype == PDL_F && finite(pdl_val.value.F) == 0) ||
          (x->datatype == PDL_D && finite(pdl_val.value.D) == 0) )
#  else
        ANYVAL_EQ_ANYVAL(pdl_val, pdl_badval)
#  endif
      ) {
	 sv = newSVpvn( "BAD", 3 );
      } else {
	 ANYVAL_TO_SV(sv, pdl_val);
      }
      av_store( av, lind, sv );
#else
      pdl_val = pdl_at( data, x->datatype, inds, x->dims, incs, offs, x->ndims );
      ANYVAL_TO_SV(sv, pdl_val);
      av_store(av, lind, sv);
#endif

      lind++;
      stop = 1;
      for(ind = 0; ind < x->ndims; ind++) {
	 if(++(inds[ind]) >= x->dims[ind]) {
       	    inds[ind] = 0;
         } else {
       	    stop = 0; break;
         }
      }
   }
   RETVAL = newRV_noinc((SV *)av);
  OUTPUT:
   RETVAL

void
set_c(x,position,value)
    pdl*	x
    SV*	position
    PDL_Anyval	value
   PREINIT:
    PDL_Indx * pos;
    int npos;
    int ipos;
   CODE:
    pdl_make_physvaffine( x );

    pos = pdl_packdims( position, &npos);
    if (pos == NULL || npos < x->ndims)
       croak("Invalid position");

    /*  allow additional trailing indices
     *  which must be all zero, i.e. a
     *  [3,1,5] piddle is treated as an [3,1,5,1,1,1,....]
     *  infinite dim piddle
     */
    for (ipos=x->ndims; ipos<npos; ipos++)
      if (pos[ipos] != 0)
         croak("Invalid position");

    pdl_children_changesoon( x , PDL_PARENTDATACHANGED );
    pdl_set(PDL_REPRP(x), x->datatype, pos, x->dims,
        (PDL_VAFFOK(x) ? x->vafftrans->incs : x->dimincs), PDL_REPROFFS(x),
	x->ndims,value);
    if (PDL_VAFFOK(x))
       pdl_vaffinechanged(x, PDL_PARENTDATACHANGED);
    else
       pdl_changed( x , PDL_PARENTDATACHANGED , 0 );

BOOT:
{
#if NVSIZE > 8
   fprintf(stderr, "Your perl NV has more precision than PDL_Double.  There will be loss of floating point precision!\n");
#endif

   /* Initialize structure of pointers to core C routines */

   PDL.Version     = PDL_CORE_VERSION;
   PDL.SvPDLV      = SvPDLV;
   PDL.SetSV_PDL   = SetSV_PDL;
   PDL.create      = pdl_create;
   PDL.pdlnew      = pdl_external_new;
   PDL.tmp         = pdl_external_tmp;
   PDL.destroy     = pdl_destroy;
   PDL.null        = pdl_null;
   PDL.copy        = pdl_copy;
   PDL.hard_copy   = pdl_hard_copy;
   PDL.converttype = pdl_converttype;
   PDL.twod        = pdl_twod;
   PDL.smalloc     = pdl_malloc;
   PDL.howbig      = pdl_howbig;
   PDL.packdims    = pdl_packdims;
   PDL.unpackdims  = pdl_unpackdims;
   PDL.setdims     = pdl_setdims;
   PDL.grow        = pdl_grow;
   PDL.flushcache  = NULL;
   PDL.reallocdims = pdl_reallocdims;
   PDL.reallocthreadids = pdl_reallocthreadids;
   PDL.resize_defaultincs = pdl_resize_defaultincs;
   PDL.get_threadoffsp = pdl_get_threadoffsp;
   PDL.thread_copy = pdl_thread_copy;
   PDL.clearthreadstruct = pdl_clearthreadstruct;
   PDL.initthreadstruct = pdl_initthreadstruct;
   PDL.startthreadloop = pdl_startthreadloop;
   PDL.iterthreadloop = pdl_iterthreadloop;
   PDL.freethreadloop = pdl_freethreadloop;
   PDL.thread_create_parameter = pdl_thread_create_parameter;
   PDL.add_deletedata_magic = pdl_add_deletedata_magic;

   PDL.setdims_careful = pdl_setdims_careful;
   PDL.put_offs = pdl_put_offs;
   PDL.get_offs = pdl_get_offs;
   PDL.get = pdl_get;
   PDL.set_trans_childtrans = pdl_set_trans_childtrans;
   PDL.set_trans_parenttrans = pdl_set_trans_parenttrans;

   PDL.get_convertedpdl = pdl_get_convertedpdl;

   PDL.make_trans_mutual = pdl_make_trans_mutual;
   PDL.trans_mallocfreeproc = pdl_trans_mallocfreeproc;
   PDL.make_physical = pdl_make_physical;
   PDL.make_physdims = pdl_make_physdims;
   PDL.make_physvaffine = pdl_make_physvaffine;
   PDL.pdl_barf      = pdl_barf;
   PDL.pdl_warn      = pdl_warn;
   PDL.allocdata     = pdl_allocdata;
   PDL.safe_indterm  = pdl_safe_indterm;
   PDL.children_changesoon = pdl_children_changesoon;
   PDL.changed       = pdl_changed;
   PDL.vaffinechanged = pdl_vaffinechanged;

   PDL.NaN_float  = union_nan_float.f;
   PDL.NaN_double = union_nan_double.d;
#if BADVAL
   PDL.propagate_badflag = propagate_badflag;
   PDL.propagate_badvalue = propagate_badvalue;
   PDL.get_pdl_badvalue = pdl_get_pdl_badvalue;
#include "pdlbadvalinit.c"
#endif
   /*
      "Publish" pointer to this structure in perl variable for use
       by other modules
   */
   sv_setiv(get_sv("PDL::SHARE",TRUE|GV_ADDMULTI), PTR2IV(&PDL));
}

# make piddle belonging to 'class' and of type 'type'
# from avref 'array_ref' which is checked for being
# rectangular first

SV*
pdl_avref(array_ref, class, type)
     SV* array_ref
     char* class
     int type
  PREINIT:
     AV *dims, *av;
     int i, depth;
     int datalevel = -1;
     SV* psv;
     pdl* p;
  CODE:
     /* make a piddle from a Perl array ref */

     if (!SvROK(array_ref))
       croak("pdl_avref: not a reference");


     if (SvTYPE(SvRV(array_ref)) != SVt_PVAV)
       croak("pdl_avref: not an array reference");

     // Expand the array ref to a list, and allocate a Perl list to hold the dimlist
     av = (AV *) SvRV(array_ref);
     dims = (AV *) sv_2mortal( (SV *) newAV());

     av_store(dims,0,newSViv((IV) av_len(av)+1));

     /* even if we contain nothing depth is one */
     depth = 1 + av_ndcheck(av,dims,0,&datalevel);

     /* printf("will make type %s\n",class); */
     /*
	at this stage start making a piddle and populate it with
	values from the array (which has already been checked in av_check)
     */
     if (strcmp(class,"PDL") == 0) {
        p = pdl_from_array(av,dims,type,NULL); /* populate with data */
        ST(0) = sv_newmortal();
        SetSV_PDL(ST(0),p);
     } else {
       /* call class->initialize method */
       PUSHMARK(SP);
       XPUSHs(sv_2mortal(newSVpv(class, 0)));
       PUTBACK;
       perl_call_method("initialize", G_SCALAR);
       SPAGAIN;
       psv = POPs;
       PUTBACK;
       p = SvPDLV(psv); /* and get piddle from returned object */
       ST(0) = psv;
       pdl_from_array(av,dims,type,p); /* populate ;) */
     }

MODULE = PDL::Core	PACKAGE = PDL

# pdl_null is created/imported with no PREFIX  as pdl_null.
#  'null' is supplied in Core.pm that calls 'initialize' which calls
#   the pdl_null here

pdl *
pdl_null(...)


MODULE = PDL::Core     PACKAGE = PDL::Core     PREFIX = pdl_

int
pdl_pthreads_enabled()

MODULE = PDL::Core	PACKAGE = PDL	PREFIX = pdl_

int
isnull(self)
	pdl *self;
	CODE:
		RETVAL= !!(self->state & PDL_NOMYDIMS);
	OUTPUT:
		RETVAL

pdl *
make_physical(self)
	pdl *self;
	CODE:
		pdl_make_physical(self);
		RETVAL = self;
	OUTPUT:
		RETVAL

pdl *
make_physvaffine(self)
	pdl *self;
	CODE:
		pdl_make_physvaffine(self);
		RETVAL = self;
	OUTPUT:
		RETVAL


pdl *
make_physdims(self)
	pdl *self;
	CODE:
		pdl_make_physdims(self);
		RETVAL = self;
	OUTPUT:
		RETVAL

void
pdl_dump(x)
  pdl *x;

void
pdl_add_threading_magic(it,nthdim,nthreads)
	pdl *it
	int nthdim
	int nthreads

void
pdl_remove_threading_magic(it)
	pdl *it
	CODE:
		pdl_add_threading_magic(it,-1,-1);

MODULE = PDL::Core	PACKAGE = PDL

SV *
initialize(class)
	SV *class
        PREINIT:
	HV *bless_stash;
        PPCODE:
        if (SvROK(class)) { /* a reference to a class */
	  bless_stash = SvSTASH(SvRV(class));
        } else {            /* a class name */
          bless_stash = gv_stashsv(class, 0);
        }
        ST(0) = sv_newmortal();
        SetSV_PDL(ST(0),pdl_null());   /* set a null PDL to this SV * */
        ST(0) = sv_bless(ST(0), bless_stash); /* bless appropriately  */
	XSRETURN(1);

SV *
get_dataref(self)
	pdl *self
	CODE:
	if(self->state & PDL_DONTTOUCHDATA) {
		croak("Trying to get dataref to magical (mmaped?) pdl");
	}
	pdl_make_physical(self); /* XXX IS THIS MEMLEAK WITHOUT MORTAL? */
	RETVAL = (newRV(self->datasv));
	OUTPUT:
	RETVAL

int
get_datatype(self)
	pdl *self
	CODE:
	RETVAL = self->datatype;
	OUTPUT:
	RETVAL

int
upd_data(self)
	pdl *self
      PREINIT:
       STRLEN n_a;
	CODE:
	if(self->state & PDL_DONTTOUCHDATA) {
		croak("Trying to touch dataref of magical (mmaped?) pdl");
	}
       self->data = SvPV((SV*)self->datasv,n_a);
	XSRETURN(0);

void
set_dataflow_f(self,value)
	pdl *self;
	int value;
	CODE:
	if(value)
		self->state |= PDL_DATAFLOW_F;
	else
		self->state &= ~PDL_DATAFLOW_F;

void
set_dataflow_b(self,value)
	pdl *self;
	int value;
	CODE:
	if(value)
		self->state |= PDL_DATAFLOW_B;
	else
		self->state &= ~PDL_DATAFLOW_B;

int
getndims(x)
	pdl *x
	ALIAS:
	     PDL::ndims = 1
	CODE:
		pdl_make_physdims(x);
		RETVAL = x->ndims;
	OUTPUT:
		RETVAL

PDL_Indx
getdim(x,y)
	pdl *x
	int y
	ALIAS:
	     PDL::dim = 1
	CODE:
		pdl_make_physdims(x);
		if (y < 0) y = x->ndims + y;
		if (y < 0) croak("negative dim index too large");
		if (y < x->ndims)
                   RETVAL = x->dims[y];
                else
		   RETVAL = 1; /* return size 1 for all other dims */
	OUTPUT:
		RETVAL

int
getnthreadids(x)
	pdl *x
	CODE:
		pdl_make_physdims(x);
		RETVAL = x->nthreadids;
	OUTPUT:
		RETVAL

int
getthreadid(x,y)
	pdl *x
	int y
	CODE:
		RETVAL = x->threadids[y];
	OUTPUT:
		RETVAL

void
setdims(x,dims_arg)
	pdl *x
      SV * dims_arg
      PREINIT:
	 PDL_Indx * dims;
	 int ndims;
       int i;
	CODE:
	{
	        /* This mask avoids all kinds of subtle dereferencing bugs (CED 11/2015) */
	        if(x->trans || x->vafftrans || x->children.next ) {
		  pdl_barf("Can't setdims on a PDL that already has children");
		}

		/* not sure if this is still necessary with the mask above... (CED 11/2015)  */
		pdl_children_changesoon(x,PDL_PARENTDIMSCHANGED|PDL_PARENTDATACHANGED);
		dims = pdl_packdims(dims_arg,&ndims);
		pdl_reallocdims(x,ndims);
		for(i=0; i<ndims; i++) x->dims[i] = dims[i];
		pdl_resize_defaultincs(x);
		x->threadids[0] = ndims;
		x->state &= ~PDL_NOMYDIMS;
		pdl_changed(x,PDL_PARENTDIMSCHANGED|PDL_PARENTDATACHANGED,0);
	}

void
dowhenidle()
	CODE:
		pdl_run_delayed_magic();
		XSRETURN(0);

void
bind(p,c)
	pdl *p
	SV *c
	PROTOTYPE: $&
	CODE:
		pdl_add_svmagic(p,c);
		XSRETURN(0);

void
sethdr(p,h)
	pdl *p
	SV *h
      PREINIT:
	HV* hash;
	CODE:
		if(p->hdrsv == NULL) {
		      p->hdrsv =  &PL_sv_undef; /*(void*) newSViv(0);*/
		}

		/* Throw an error if we're not either undef or hash */
                if ( (h != &PL_sv_undef && h != NULL) &&
		     ( !SvROK(h) || SvTYPE(SvRV(h)) != SVt_PVHV )
		   )
		      croak("Not a HASH reference");

		/* Clear the old header */
		SvREFCNT_dec(p->hdrsv);

		/* Put the new header (or undef) in place */
		if(h == &PL_sv_undef || h == NULL)
		   p->hdrsv = NULL;
		else
		   p->hdrsv = (void*) newRV( (SV*) SvRV(h) );

SV *
hdr(p)
	pdl *p
	CODE:
		pdl_make_physdims(p);

                /* Make sure that in the undef case we return not */
                /* undef but an empty hash ref. */

                if((p->hdrsv==NULL) || (p->hdrsv == &PL_sv_undef)) {
	            p->hdrsv = (void*) newRV_noinc( (SV*)newHV() );
                }

		RETVAL = newRV( (SV*) SvRV((SV*)p->hdrsv) );

	OUTPUT:
	 RETVAL

# fhdr(p) is implemented in perl; see Core.pm.PL if you're looking for it
#   --CED 9-Feb-2003
#

SV *
gethdr(p)
	pdl *p
	CODE:
		pdl_make_physdims(p);

                if((p->hdrsv==NULL) || (p->hdrsv == &PL_sv_undef)) {
	            RETVAL = &PL_sv_undef;
                } else {
		    RETVAL = newRV( (SV*) SvRV((SV*)p->hdrsv) );
                }

	OUTPUT:
	 RETVAL

void
set_datatype(a,datatype)
   pdl *a
   int datatype
   CODE:
    pdl_make_physical(a);
    if(a->trans)
	    pdl_destroytransform(a->trans,1);
/*     if(! (a->state && PDL_NOMYDIMS)) { */
    pdl_converttype( &a, datatype, PDL_PERM );
/*     } */

void
threadover_n(...)
   PREINIT:
   int npdls;
   SV *sv;
   CODE:
   {
    npdls = items - 1;
    if(npdls <= 0)
    	croak("Usage: threadover_n(pdl[,pdl...],sub)");
    {
	    int i,sd;
	    pdl **pdls = malloc(sizeof(pdl *) * npdls);
	    PDL_Indx *realdims = malloc(sizeof(PDL_Indx) * npdls);
	    pdl_thread pdl_thr;
	    SV *code = ST(items-1);
	    for(i=0; i<npdls; i++) {
		pdls[i] = SvPDLV(ST(i));
		/* XXXXXXXX Bad */
		pdl_make_physical(pdls[i]);
		realdims[i] = 0;
	    }
	    PDL_THR_CLRMAGIC(&pdl_thr);
	    pdl_initthreadstruct(0,pdls,realdims,realdims,npdls,NULL,&pdl_thr,NULL, 1);
	    pdl_startthreadloop(&pdl_thr,NULL,NULL);
	    sd = pdl_thr.ndims;
	    do {
	    	dSP;
		PUSHMARK(sp);
		EXTEND(sp,items);
		PUSHs(sv_2mortal(newSViv((sd-1))));
		for(i=0; i<npdls; i++) {
			PDL_Anyval pdl_val = { -1, 0 };
			pdl_val = pdl_get_offs(pdls[i],pdl_thr.offs[i]);
			ANYVAL_TO_SV(sv, pdl_val);
			PUSHs(sv_2mortal(sv));
		}
	    	PUTBACK;
		perl_call_sv(code,G_DISCARD);
	    } while( (sd = pdl_iterthreadloop(&pdl_thr,0)) );
	    pdl_freethreadloop(&pdl_thr);
	    free(pdls);
	    free(realdims);
    }
   }

void
threadover(...)
   PREINIT:
    int npdls;
    int targs;
    int nothers = -1;
   CODE:
   {
        targs = items - 4;
    if (items > 0) nothers = SvIV(ST(0));
    if(targs <= 0 || nothers < 0 || nothers >= targs)
    	croak("Usage: threadover(nothers,pdl[,pdl...][,otherpars..],realdims,creating,sub)");
    npdls = targs-nothers;
    {
	    int i,nd1,nd2,dtype=0;
	    PDL_Indx j,nc=npdls;
	    SV* rdimslist = ST(items-3);
	    SV* cdimslist = ST(items-2);
	    SV *code = ST(items-1);
	    pdl_thread pdl_thr;
	    pdl **pdls = malloc(sizeof(pdl *) * npdls);
	    pdl **child = malloc(sizeof(pdl *) * npdls);
	    SV **csv = malloc(sizeof(SV *) * npdls);
	    SV **dims = malloc(sizeof(SV *) * npdls);
	    SV **incs = malloc(sizeof(SV *) * npdls);
	    SV **others = malloc(sizeof(SV *) * nothers);
	    PDL_Indx *creating = pdl_packint(cdimslist,&nd2);
	    PDL_Indx *realdims = pdl_packint(rdimslist,&nd1);
	    CHECKP(pdls); CHECKP(child); CHECKP(dims);
	    CHECKP(incs); CHECKP(csv);

	    if (nd1 != npdls || nd2 < npdls)
		croak("threadover: need one realdim and creating flag "
		      "per pdl!");
	    for(i=0; i<npdls; i++) {
		pdls[i] = SvPDLV(ST(i+1));
		if (creating[i])
		  nc += realdims[i];
		else {
		  pdl_make_physical(pdls[i]); /* is this what we want?XXX */
		  dtype = PDLMAX(dtype,pdls[i]->datatype);
		}
	    }
	    for (i=npdls+1; i<=targs; i++)
		others[i-npdls-1] = ST(i);
	    if (nd2 < nc)
		croak("Not enough dimension info to create pdls");
#ifdef DEBUG_PTHREAD
		for (i=0;i<npdls;i++) { /* just for debugging purposes */
		printf("pdl %d Dims: [",i);
		for (j=0;j<realdims[i];j++)
			printf("%d ",pdls[i]->dims[j]);
		printf("] Incs: [");
		for (j=0;j<realdims[i];j++)
			printf("%d ",PDL_REPRINC(pdls[i],j));
		printf("]\n");
	        }
#endif
	    PDL_THR_CLRMAGIC(&pdl_thr);
	    pdl_initthreadstruct(0,pdls,realdims,creating,npdls,
				NULL,&pdl_thr,NULL, 1);
	    for(i=0, nc=npdls; i<npdls; i++)  /* create as necessary */
              if (creating[i]) {
		PDL_Indx *cp = creating+nc;
		pdls[i]->datatype = dtype;
		pdl_thread_create_parameter(&pdl_thr,i,cp,0);
		nc += realdims[i];
		pdl_make_physical(pdls[i]);
		PDLDEBUG_f(pdl_dump(pdls[i]));
		/* And make it nonnull, now that we've created it */
		pdls[i]->state &= (~PDL_NOMYDIMS);
	      }
	    pdl_startthreadloop(&pdl_thr,NULL,NULL);
	    for(i=0; i<npdls; i++) { /* will the SV*'s be properly freed? */
		dims[i] = newRV(pdl_unpackint(pdls[i]->dims,realdims[i]));
		incs[i] = newRV(pdl_unpackint(PDL_VAFFOK(pdls[i]) ?
		pdls[i]->vafftrans->incs: pdls[i]->dimincs,realdims[i]));
		/* need to make sure we get the vaffine (grand)parent */
		if (PDL_VAFFOK(pdls[i]))
		   pdls[i] = pdls[i]->vafftrans->from;
		child[i]=pdl_null();
		/*  instead of pdls[i] its vaffine parent !!!XXX */
		PDL.affine_new(pdls[i],child[i],pdl_thr.offs[i],dims[i],
						incs[i]);
		pdl_make_physical(child[i]); /* make sure we can get at
						the vafftrans          */
		csv[i] = sv_newmortal();
		SetSV_PDL(csv[i], child[i]); /* pdl* into SV* */
	    }
	    do {  /* the actual threadloop */
		pdl_trans_affine *traff;
	    	dSP;
		PUSHMARK(sp);
		EXTEND(sp,npdls);
		for(i=0; i<npdls; i++) {
		   /* just twiddle the offset - quick and dirty */
		   /* we must twiddle both !! */
		   traff = (pdl_trans_affine *) child[i]->trans;
		   traff->offs = pdl_thr.offs[i];
		   child[i]->vafftrans->offs = pdl_thr.offs[i];
		   child[i]->state |= PDL_PARENTDATACHANGED;
		   PUSHs(csv[i]);
		}
		for (i=0; i<nothers; i++)
		  PUSHs(others[i]);   /* pass the OtherArgs onto the stack */
	    	PUTBACK;
		perl_call_sv(code,G_DISCARD);
	    } while (pdl_iterthreadloop(&pdl_thr,0));
	    pdl_freethreadloop(&pdl_thr);
	    free(pdls);  /* should all these be done with pdl_malloc */
	    free(dims);  /* in case the sub barfs ? XXXX            */
	    free(child);
	    free(csv);
	    free(incs);
	    free(others);
    }
   }
