use strict;
use warnings;

require './Types.pm';
my $PDL_DATATYPES = PDL::Types::datatypes_header();

require './Config.pm';
die "No PDL::Config found" unless %PDL::Config;
my $mymalloc = $PDL::Config{MALLOCDBG}->{define} // '';

my $file = shift @ARGV;
print "Extracting $file\n";
open OUT,">$file" or die "Can't create $file: $!";
chmod 0644, $file;

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<"!GROK!THIS!";

/*
 * THIS FILE IS GENERATED FROM pdl.h.PL! Do NOT edit!
 */

#ifndef __PDL_H
#define __PDL_H

#include <complex.h>

#define PDL_DEBUGGING 1

#ifdef PDL_DEBUGGING
extern int pdl_debugging;
#define PDLDEBUG_f(a)           if(pdl_debugging) a
#else
#define PDLDEBUG_f(a)
#endif

#define ANYVAL_ISBAD(inany,pdl,badval) \\
  (ANYVAL_ISNAN(badval) ? ANYVAL_ISNAN(inany) : ANYVAL_EQ_ANYVAL(inany, badval))

typedef struct pdl pdl;

$PDL_DATATYPES

#include "pdlthread.h"

/* Auto-PThreading (i.e. multi-threading) settings for PDL functions */
/*  Target number of pthreads: Actual will be this number or less.
    A 0 here means no pthreading */
extern int pdl_autopthread_targ;

/*  Actual number of pthreads: This is the number of pthreads created for the last
    operation where pthreading was used
    A 0 here means no pthreading */
extern int pdl_autopthread_actual;
/* Minimum size of the target PDL involved in pdl function to attempt pthreading (in MBytes )
    For small PDLs, it probably isn't worth starting multiple pthreads, so this variable
    is used to define that threshold (in M-elements, or 2^20 elements ) */
extern int pdl_autopthread_size;
extern PDL_Indx pdl_autopthread_dim;

$mymalloc

#define ANYVAL_EQ_ANYVAL(x,y) (_anyval_eq_anyval(x,y))

!GROK!THIS!

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'EOF';
static inline int _anyval_eq_anyval(PDL_Anyval x, PDL_Anyval y) {
  switch (x.type) {
EOF
for my $o (PDL::Types::types()) { # outer
  print OUT "    case @{[$o->sym]}:\n      switch (y.type) {\n";
  for my $i (PDL::Types::types()) { # inner
    print OUT "        case @{[$i->sym]}: return (x.value.".$o->ppsym." == y.value.".$i->ppsym.") ? 1 : 0;\n";
  }
  print OUT "        default: return 0;\n      }\n";
}
print OUT <<'EOF';
    default:
      return 0;
  }
}
EOF

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'EOF';
#define ANYVAL_FROM_CTYPE(outany,avtype,inval) do { switch (avtype) { \
EOF
print OUT "case @{[$_->sym]}: outany.type = avtype; outany.value.@{[$_->ppsym]} = (@{[$_->ctype]})(inval); break; \\\n"
  for PDL::Types::types();
print OUT <<'EOF';
   default:      outany.type = -1;     outany.value.B = 0; \
  } \
 } while (0)
EOF

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'EOF';
#define ANYVAL_TO_CTYPE(outval,ctype,inany) do { switch (inany.type) { \
EOF
print OUT "case @{[$_->sym]}: outval = (ctype)(inany.value.@{[$_->ppsym]}); break; \\\n"
  for PDL::Types::types();
print OUT <<'EOF';
default:      outval = 0; \
            } \
        } while (0)
EOF

print OUT <<'EOF';
#define ANYVAL_FROM_CTYPE_OFFSET(outany,avtype,indata,ioff) do { switch (avtype) { \
EOF
print OUT "case @{[$_->sym]}: outany.type = avtype; outany.value.@{[$_->ppsym]} = (@{[$_->ctype]})((@{[$_->ctype]} *)indata)[ioff]; break; \\\n"
  for PDL::Types::types();
print OUT <<'EOF';
   default:      outany.type = -1;     outany.value.B = 0; \
  } \
 } while (0)
EOF

print OUT <<'EOF';
/* input vars have to be called exactly these as not expanded */
#define ANYVAL_TO_CTYPE_OFFSET_X(datatype, generic, generic_ppsym, shortctype, defbval) \
    ANYVAL_TO_CTYPE(((generic *)x)[ioff], generic, value);
#define ANYVAL_TO_CTYPE_OFFSET(x,ioff,datatype,value) \
    PDL_GENERICSWITCH(datatype, ANYVAL_TO_CTYPE_OFFSET_X)
EOF

my $isnan = join " \\\n  ",
  map "(x).type == ".$_->sym." ? @{[$_->isnan('(x).value.'.$_->ppsym)]} :",
  grep $_->usenan, PDL::Types::types();
print OUT <<EOF;
#define ANYVAL_ISNAN(x) (\\
  (x).type < PDL_F ? 0 : \\
  $isnan \\
  (croak("ANYVAL_ISNAN should not get here, had type %d", (x).type), 1) \\
)
EOF

for my $type (PDL::Types::types()) {
  my ($ppsym) = map $type->$_, qw(ppsym);
  my $expr = !$type->usenan ? 'FALSE' : $type->isnan('x');
  print OUT "#define PDL_ISNAN_$ppsym(x) ($expr)\n";
}

my @methods = qw(symbol ctype ppsym shortctype defbval);
my @generics = map { my $t = $_; [map $t->$_, @methods] } PDL::Types::types();
print OUT "#define PDL_GENERICLIST(X) \\\n";
print OUT (map ' X('.join(',', @$_).")\\\n", @generics), "\n\n";
print OUT "#define PDL_GENERICLIST2(X, X2) \\\n";
print OUT (map ' X(X2, '.join(',', @$_).")\\\n", @generics), "\n\n";
# extra as macro gets expanded twice, gets painted blue
print OUT "#define PDL_GENERICLIST2_(X, X2) \\\n";
print OUT (map ' X(X2, '.join(',', @$_).")\\\n", @generics), "\n\n";
my @generics_real = map { my $t = $_; [map $t->$_, @methods] } grep $_->real, PDL::Types::types();
print OUT "#define PDL_GENERICLIST_REAL(X) \\\n";
print OUT (map ' X('.join(',', @$_).")\\\n", @generics_real), "\n\n";

print OUT <<'EOF';
typedef struct badvals {
#define X(symbol, ctype, ppsym, shortctype, defbval) ctype shortctype;
PDL_GENERICLIST(X)
#undef X
} badvals;

#define PDL_GENERICSWITCH_CASE(X, symbol, ...) \
  case symbol: { X(symbol, __VA_ARGS__) } break;
#define PDL_GENERICSWITCH(typevar, X2) \
  switch (typevar) { \
    PDL_GENERICLIST2(PDL_GENERICSWITCH_CASE, X2) \
    default: croak("Not a known data type code=%d", typevar); \
  }
/* extra as first one gets painted blue */
#define PDL_GENERICSWITCH_CASE2(X, symbol, ...) \
  case symbol: { X(symbol, __VA_ARGS__) } break;
/* for use in inner loops avoiding extreme lambda-calculus stuff */
#define PDL_GENERICSWITCH2(typevar, X2) \
  switch (typevar) { \
    PDL_GENERICLIST2_(PDL_GENERICSWITCH_CASE2, X2) \
    default: croak("Not a known data type code=%d", typevar); \
  }
EOF

print OUT <<'!NO!SUBS!';

/*
 * Define the pdl C data structure which maps onto the original PDL
 * perl data structure.
 *
 * Note: pdl.sv is defined as a void pointer to avoid having to
 * include perl.h in C code which just needs the pdl data.
 *
 * We start with the meanings of the pdl.flags bitmapped flagset,
 * continue with a prerequisite "trans" structure that represents
 * transformations between linked PDLs, and finish withthe PD
 * structure itself.
*/

#define PDL_NDIMS      6 /* Number of dims[] to preallocate */
#define PDL_NCHILDREN  8 /* Number of children ptrs to preallocate */
#define PDL_NTHREADIDS 4 /* Number of different threadids/pdl to preallocate */

/* Constants for pdl.state - not all combinations make sense */

  /* data allocated for this pdl.  this implies that the data               */
  /* is up to date if !PDL_PARENTCHANGED                                    */
#define  PDL_ALLOCATED           0x0001
  /* Parent data has been altered without changing this pdl                 */
#define  PDL_PARENTDATACHANGED   0x0002
  /* Parent dims or incs has been altered without changing this pdl.        */
#define  PDL_PARENTDIMSCHANGED   0x0004
  /* Physical data representation of the parent has changed (e.g.           */
  /* physical transposition), so incs etc. need to be recalculated.         */
#define  PDL_PARENTREPRCHANGED   0x0008
#define  PDL_ANYCHANGED          (PDL_PARENTDATACHANGED|PDL_PARENTDIMSCHANGED|PDL_PARENTREPRCHANGED)
  /* Dataflow tracking flags -- F/B for forward/back.  These get set        */
  /* by transformations when they are set up.                               */
#define  PDL_DATAFLOW_F          0x0010
#define  PDL_DATAFLOW_B          0x0020
#define  PDL_DATAFLOW_ANY        (PDL_DATAFLOW_F|PDL_DATAFLOW_B)
  /* Was this PDL null originally?                                          */
#define  PDL_NOMYDIMS            0x0040
  /* Dims should be received via trans.                                     */
#define  PDL_MYDIMS_TRANS        0x0080
  /* OK to attach a vaffine transformation (i.e. a slice)                   */
#define  PDL_OPT_VAFFTRANSOK     0x0100
#define  PDL_OPT_ANY_OK          (PDL_OPT_VAFFTRANSOK)
  /* This is the hdrcpy flag                                                */
#define  PDL_HDRCPY              0x0200
  /* This is a badval flag for this PDL (hmmm -- there is also a flag       */
  /* in the struct itself -- must be clearer about what this is for. --CED) */
#define  PDL_BADVAL              0x0400
  /* Debugging flag                                                         */
#define  PDL_TRACEDEBUG          0x0800
  /* inplace flag                                                           */
#define  PDL_INPLACE             0x1000
  /* Flag indicating destruction in progress                                */
#define         PDL_DESTROYING          0x2000
  /* If this flag is set, you must not alter the data pointer nor           */
  /* free this ndarray nor use datasv (which should be null).                */
  /* This means e.g. that the ndarray is mmapped from a file                 */
#define         PDL_DONTTOUCHDATA       0x4000
  /* whether the given pdl is getting its dims from the given trans */
#define PDL_DIMS_FROM_TRANS(wtrans,pdl) (((pdl)->state & PDL_MYDIMS_TRANS) \
                && (pdl)->trans_parent == (pdl_trans *)(wtrans))

#define PDL_LIST_FLAGS_PDLSTATE(X) \
 X(PDL_ALLOCATED) \
 X(PDL_PARENTDATACHANGED) \
 X(PDL_PARENTDIMSCHANGED) \
 X(PDL_PARENTREPRCHANGED) \
 X(PDL_DATAFLOW_F) \
 X(PDL_DATAFLOW_B) \
 X(PDL_NOMYDIMS) \
 X(PDL_MYDIMS_TRANS) \
 X(PDL_OPT_VAFFTRANSOK) \
 X(PDL_HDRCPY) \
 X(PDL_BADVAL) \
 X(PDL_TRACEDEBUG) \
 X(PDL_INPLACE) \
 X(PDL_DESTROYING) \
 X(PDL_DONTTOUCHDATA)

/**************************************************
 *
 * Transformation structure
 *
 * The structure is general enough to deal with functional transforms
 * (which were originally intended) but only slices and retype transforms
 * were implemented.
 *
 */

/* Transformation flags */
#define         PDL_TRANS_DO_THREAD        0x0001

#define PDL_LIST_FLAGS_PDLVTABLE(X) \
  X(PDL_TRANS_DO_THREAD)

/* Transpdl flags */
#define         PDL_TPDL_VAFFINE_OK     0x01

typedef struct pdl_trans pdl_trans;

typedef struct pdl_transvtable {
        int flags;
        PDL_Indx nparents;
        PDL_Indx npdls;
        char *per_pdl_flags;
        PDL_Indx *par_realdims; /* quantity of dimensions each par has */
        char **par_names;
        char noPthreadFlag;
        void (*redodims)(pdl_trans *tr);      /* Only dims and internal trans (makes phys) */
        void (*readdata)(pdl_trans *tr);      /* Only data, to "data" ptr  */
        void (*writebackdata)(pdl_trans *tr); /* "data" ptr to parent or granny */
        void (*freetrans)(pdl_trans *tr);     /* Free both the contents and it of
                                                 the trans_parent member */
        int structsize;
        char *name;                           /* For debuggers, mostly */
} pdl_transvtable;

/* All trans must start with this */

/* Trans flags */

        /* Reversible transform -- flag indicates data can flow both ways.  */
        /* This is critical in routines that both input from and output to  */
        /* a non-single-valued pdl: updating must occur.  (Note that the    */
        /* transform is not necessarily mathematically reversible)          */
#define  PDL_ITRANS_REVERSIBLE        0x0001
        /* Whether, if a child is changed, this trans should be destroyed or not */
        /* (flow if set; sever if clear) */
#define  PDL_ITRANS_DO_DATAFLOW_F     0x0002
#define  PDL_ITRANS_DO_DATAFLOW_B     0x0004
#define  PDL_ITRANS_DO_DATAFLOW_ANY   (PDL_ITRANS_DO_DATAFLOW_F|PDL_ITRANS_DO_DATAFLOW_B)

#define  PDL_ITRANS_ISAFFINE          0x1000
#define  PDL_ITRANS_VAFFINEVALID      0x2000
#define  PDL_ITRANS_NONMUTUAL         0x4000  /* flag for destruction */

#define PDL_LIST_FLAGS_PDLTRANS(X) \
 X(PDL_ITRANS_REVERSIBLE) \
 X(PDL_ITRANS_DO_DATAFLOW_F) \
 X(PDL_ITRANS_DO_DATAFLOW_B) \
 X(PDL_ITRANS_ISAFFINE) \
 X(PDL_ITRANS_VAFFINEVALID) \
 X(PDL_ITRANS_NONMUTUAL)

#define PDL_MAXSPACE 256 /* maximal number of prefix spaces in dump routines */
#define PDL_MAXLIN 60

// These define struct pdl_trans and all derived structures. There are many
// structures that defined in other parts of the code that can be referenced
// like a pdl_trans* because all of these structures have the same pdl_trans
// initial piece. These structures can contain multiple pdl* elements in them.
// Thus pdl_trans itself ends with a flexible pdl*[] array, which can be used to
// reference any number of pdl objects. As a result pdl_trans itself can NOT be
// instantiated

// vparent is the "virtual parent" which is either
// the parent or grandparent or whatever. The trans -structure must store
// both the relationship with our current parent and, if necessary, the
// virtual parent.

#define PDL_TRANS_START_COMMON                                          \
  unsigned int magicno;                                                 \
  short flags;                                                          \
  pdl_transvtable *vtable;                                              \
  void (*freeproc)(struct pdl_trans *);  /* Call to free this           \
                                          (means whether malloced or not) */ \
  int bvalflag;                                                         \
  int has_badvalue;                                                     \
  PDL_Anyval badvalue;                                                  \
  pdl_thread pdlthread;                                                 \
  char dims_redone;                                                     \
  int __datatype

#define PDL_TRANS_START(np) \
  PDL_TRANS_START_COMMON; \
  /* The pdls involved in the transformation */ \
  pdl *pdls[np]

#define PDL_TRANS_START_FLEXIBLE() \
  PDL_TRANS_START_COMMON; \
  /* The pdls involved in the transformation */ \
  pdl *pdls[]

#define PDL_CHKMAGIC_GENERAL(it,this_magic,type) if((it)->magicno != this_magic) croak("INVALID " #type "MAGIC NO 0x%p %d%s\n",it,(int)((it)->magicno), ((it)->magicno) == PDL_CLEARED_MAGICNO ? " (cleared)" : ""); else (void)0

#define PDL_TR_MAGICNO      0x91827364
#define PDL_TR_SETMAGIC(it) it->magicno = PDL_TR_MAGICNO
#define PDL_TR_CLRMAGIC(it) it->magicno = PDL_CLEARED_MAGICNO
#define PDL_TR_CHKMAGIC(it) PDL_CHKMAGIC_GENERAL(it, PDL_TR_MAGICNO, "TRANS ")

// This is a generic parent of all the trans structures. It is a flexible array
// (can store an arbitrary number of pdl objects). Thus this can NOT be
// instantiated, only "child" structures can
struct pdl_trans {
  PDL_TRANS_START_FLEXIBLE();
} ;

typedef struct pdl_trans_affine {
        PDL_TRANS_START(2);
/* affine relation to parent */
	PDL_Indx *incs; PDL_Indx offs;
} pdl_trans_affine;

/* Need to make compatible with pdl_trans_affine */
typedef struct pdl_vaffine {
	PDL_TRANS_START(2);
	PDL_Indx *incs; PDL_Indx offs;
	PDL_Indx ndims;
	PDL_Indx def_incs[PDL_NDIMS];
	pdl *from;
} pdl_vaffine;

#define PDL_VAFFOK(pdl) (pdl->state & PDL_OPT_VAFFTRANSOK)
#define PDL_REPRINC(pdl,which) (PDL_VAFFOK(pdl) ? \
                pdl->vafftrans->incs[which] : pdl->dimincs[which])

#define PDL_REPROFFS(pdl) (PDL_VAFFOK(pdl) ? pdl->vafftrans->offs : 0)

#define PDL_REPRP(pdl) (PDL_VAFFOK(pdl) ? pdl->vafftrans->from->data : pdl->data)

#define PDL_REPRP_TRANS(pdl,flag) ((PDL_VAFFOK(pdl) && \
      (flag & PDL_TPDL_VAFFINE_OK)) ? pdl->vafftrans->from->data : pdl->data)

#define VAFFINE_FLAG_OK(flags,i) ((flags == NULL) ? 1 : (flags[i] & \
                                  PDL_TPDL_VAFFINE_OK))

typedef struct pdl_children {
        pdl_trans *trans_parent[PDL_NCHILDREN];
        struct pdl_children *next;
} pdl_children;

struct pdl_magic;

/****************************************
 * PDL structure
 * Should be kept under 250 bytes if at all possible, for
 * easier segmentation...
 * See current size (360 bytes at time of writing) with:
      perl -Mblib -MInline=with,PDL \
        -MInline=C,'size_t f() { return sizeof(struct pdl); }' -e 'die f()'
 *
 * The 'sv', 'datasv', and 'hdrsv' fields are all void * to avoid having to
 * load perl.h for C codes that only use PDLs and not the Perl API.
 *
 * Similarly, the 'magic' field is void * to avoid having to typedef pdl_magic
 * here -- it is declared in "pdl_magic.h".
 */

#define PDL_MAGICNO 0x24645399
#define PDL_CLEARED_MAGICNO 0x99876134 /* value once "cleared" */
#define PDL_CHKMAGIC(it) PDL_CHKMAGIC_GENERAL(it,PDL_MAGICNO,"")

struct pdl {
   unsigned long magicno; /* Always stores PDL_MAGICNO as a sanity check */
     /* This is first so most pointer accesses to wrong type are caught */
   int state;        /* What's in this pdl */

   pdl_trans *trans_parent; /* Opaque pointer to internals of transformation from
                        parent */

   pdl_vaffine *vafftrans; /* pointer to vaffine transformation
                              a vafftrans is an optimization that is possible
                              for some types of trans (slice etc)
                              - unused for non-affine transformations
                            */

   void*    sv;      /* (optional) pointer back to original sv.
                          ALWAYS check for non-null before use.
                          We cannot inc refcnt on this one or we'd
                          never get destroyed */
   void *datasv;        /* Pointer to SV containing data. We own one inc of refcnt */
   void *data;            /* Pointer to actual data (in SV), or NULL if we have no data      */
   /* bad value stored as double, since get_badvalue returns a double */
   PDL_Anyval badvalue; /* BAD value is stored as a PDL_Anyval for portability */
   int has_badvalue;     /* flag is required by pdlapi.c (compare to PDL_BADVAL above -- why two? --CED) */
   PDL_Indx nvals; /* Actual size of data (not quite nelem in case of dummy) */
   pdl_datatypes datatype; /* One of the usual suspects (PDL_L, PDL_D, etc.) */
   PDL_Indx   *dims;      /* Array of data dimensions - could point below or to an allocated array */
   PDL_Indx   *dimincs;   /* Array of data default increments, aka strides through memory for each dim (0 for dummies) */
   PDL_Indx    ndims;     /* Number of data dimensions in dims and dimincs */

   PDL_Indx *threadids;  /* Starting index of the thread index set n */
   PDL_Indx nthreadids;

   pdl_children children;

   PDL_Indx   def_dims[PDL_NDIMS];   /* Preallocated space for efficiency */
   PDL_Indx   def_dimincs[PDL_NDIMS];   /* Preallocated space for efficiency */
   PDL_Indx   def_threadids[PDL_NTHREADIDS];

   struct pdl_magic *magic;

   void *hdrsv; /* "header", settable from Perl */
};


/*************
 * Some macros for looping over the children of a given PDL
 */
#define PDL_DECL_CHILDLOOP(p) \
                int p##__i; pdl_children *p##__c;
#define PDL_START_CHILDLOOP(p) \
                p##__c = &p->children; \
                do { \
                        for(p##__i=0; p##__i<PDL_NCHILDREN; p##__i++) { \
                                if(p##__c->trans_parent[p##__i]) {
#define PDL_CHILDLOOP_THISCHILD(p) p##__c->trans_parent[p##__i]
#define PDL_END_CHILDLOOP(p) \
                                } \
                        } \
                        if(!p##__c) break; \
                        if(!p##__c->next) break; \
                        p##__c=p##__c->next; \
                } while(1);


#define PDLMAX(a,b) ((a) > (b) ? (a) : (b))
#define PDLMIN(a,b) ((a) < (b) ? (a) : (b))

/***************
 * Some macros to guard against dataflow infinite recursion.
 */
#define DECL_RECURSE_GUARD static int __nrec=0;
#define START_RECURSE_GUARD __nrec++; if(__nrec > 1000) {__nrec=0; die("PDL:Internal Error: data structure recursion limit exceeded (max 1000 levels)\n\tThis could mean that you have found an infinite-recursion error in PDL, or\n\tthat you are building data structures with very long dataflow dependency\n\tchains.  You may want to try using sever() to break the dependency.\n");}
#define ABORT_RECURSE_GUARD __nrec=0;
#define END_RECURSE_GUARD __nrec--;

#define PDL_ENSURE_ALLOCATED(it) ( (void)((it->state & PDL_ALLOCATED) || ((pdl_allocdata(it)),1)) )
#define PDL_ENSURE_VAFFTRANS(it) \
  ( ((!it->vafftrans) || (it->vafftrans->ndims < it->ndims)) && \
    (pdl_vafftrans_alloc(it),1) )

/* for use with PDL_GENERICLIST_REAL */
#define PDL_QSORT(symbol, ctype, ppsym, shortctype, defbval) \
  void qsort_ ## ppsym(ctype* xx, PDL_Indx a, PDL_Indx b) { \
     PDL_Indx i,j; \
     ctype t, median; \
     i = a; j = b; \
     median = xx[(i+j) / 2]; \
     do { \
        while (xx[i] < median) \
           i++; \
        while (median < xx[j]) \
           j--; \
        if (i <= j) { \
           t = xx[i]; xx[i] = xx[j]; xx[j] = t; \
           i++; j--; \
        } \
     } while (i <= j); \
     if (a < j) \
        qsort_ ## ppsym(xx,a,j); \
     if (i < b) \
        qsort_ ## ppsym(xx,i,b); \
  }

/* __PDL_H */
#endif

!NO!SUBS!
