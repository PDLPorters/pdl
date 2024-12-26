use strict;
use warnings;

require './lib/PDL/Types.pm';

my $file = shift @ARGV;
print "Extracting $file\n";
open OUT,">$file" or die "Can't create $file: $!";
chmod 0644, $file;

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'EOF';
/*
 * THIS FILE IS GENERATED FROM pdl.h.PL! Do NOT edit!
 */

#ifndef __PDL_H
#define __PDL_H

#include <complex.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>

#define IND_FLAG "td"

#define PDL_DEBUGGING 1

#ifdef PDL_DEBUGGING
extern int pdl_debugging;
#define PDLDEBUG_f(a) do {if (pdl_debugging) { a; fflush(stdout); }} while (0)
#else
#define PDLDEBUG_f(a)
#endif

typedef struct pdl pdl;
EOF

my @methods = qw(symbol ctype ppsym shortctype defbval realctype convertfunc floatsuffix);
sub makelister {
  my ($name, $underscore, $pred) = @_;
  my @list = map { my $t = $_; [map $t->$_, @methods] } grep $pred->($_), PDL::Types::types();
  $underscore = $underscore ? '_' : '';
  ("#define PDL_TYPELIST_$name$underscore(X, ...) \\\n",
    (map " X(__VA_ARGS__ ".join(',', @$_).")\\\n", @list), "\n\n");
}
my $pred_all = sub {1};
my $pred_real = sub {$_[0]->real};
my $pred_floatreal = sub {$_[0]->real && !$_[0]->integer};
my $pred_complex = sub {!$_[0]->real};
my $pred_signed = sub {$_[0]->integer && !$_[0]->unsigned};
my $pred_unsigned = sub {$_[0]->unsigned};
my $pred_integer = sub {$_[0]->integer};
print OUT makelister('ALL', 0, $pred_all);
# extra as macro gets expanded twice, gets painted blue
print OUT makelister('ALL', 1, $pred_all);
print OUT makelister('REAL', 0, $pred_real);
print OUT makelister('FLOATREAL', 0, $pred_floatreal);
print OUT makelister('COMPLEX', 0, $pred_complex);
print OUT makelister('SIGNED', 0, $pred_signed);
print OUT makelister('UNSIGNED', 0, $pred_unsigned);
print OUT makelister('INTEGER', 0, $pred_integer);

sub makepredicate {
  my ($name, $pred) = @_;
  map "#define PDL_GENTYPE_IS_${name}_".$_->ppsym." ".(0+$pred->($_))."\n", PDL::Types::types();
}
print OUT makepredicate('REAL', $pred_real);
print OUT makepredicate('FLOATREAL', $pred_floatreal);
print OUT makepredicate('COMPLEX', $pred_complex);
print OUT makepredicate('SIGNED', $pred_signed);
print OUT makepredicate('UNSIGNED', $pred_unsigned);
print OUT makepredicate('INTEGER', $pred_integer);

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'EOF';

#define X(sym, ...) \
  , sym
typedef enum {
   PDL_INVALID=-1
PDL_TYPELIST_ALL(X)
} pdl_datatypes;
#undef X

#define X(sym, ctype, ppsym, shortctype, defbval, realctype, ...) \
  typedef realctype ctype;
PDL_TYPELIST_ALL(X)
#undef X

typedef union {
#define X(sym, ctype, ppsym, shortctype, defbval, realctype, ...) \
  ctype ppsym;
PDL_TYPELIST_ALL(X)
#undef X
} PDL_Value;

typedef struct {
  pdl_datatypes type;
  PDL_Value value;
} PDL_Anyval;

#define PDL_CHKMAGIC_GENERAL(it,this_magic,type) \
  if((it)->magicno != this_magic) \
    return pdl_make_error(PDL_EFATAL, \
      "INVALID MAGICNO in " type "=%p got 0x%lx instead of 0x%lx%s", \
      it,(unsigned long)((it)->magicno),this_magic, \
      ((it)->magicno) == PDL_CLEARED_MAGICNO ? " (cleared)" : "" \
    ); \
  else (void)0
#define PDL_CLEARED_MAGICNO 0x99876134 /* value once "cleared" */
#define PDL_CLRMAGIC(it) (it)->magicno = PDL_CLEARED_MAGICNO

#include "pdlbroadcast.h"

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
EOF

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'EOF';
#define PDL_EMPTY()
#define PDL_DEFER(id) id PDL_EMPTY()
#define PDL_OBSTRUCT(...) __VA_ARGS__ PDL_DEFER(PDL_EMPTY)()
#define PDL_EXPAND(...) PDL_EXPAND2(PDL_EXPAND2(PDL_EXPAND2(__VA_ARGS__)))
#define PDL_EXPAND2(...) __VA_ARGS__
#define PDL_EXPAND_(...) PDL_EXPAND2_(PDL_EXPAND2_(PDL_EXPAND2_(__VA_ARGS__)))
#define PDL_EXPAND2_(...) __VA_ARGS__
#define PDL_GENERICSWITCH_CASE(X, extraargs, symbol, ...) \
  case symbol: { PDL_EXPAND(PDL_OBSTRUCT(X)(PDL_EXPAND extraargs symbol, __VA_ARGS__)) } break;
#define PDL_GENERICSWITCH(LISTER, typevar, X, dflt, ...) \
  switch (typevar) { \
    LISTER(PDL_GENERICSWITCH_CASE, X, (__VA_ARGS__),) \
    default: dflt; \
  }
#define PDL_GENERICSWITCH_CASEout(Xout, extraargs, LISTERin, typevarin, Xin, dfltin, symbol, ...) \
  case symbol: { PDL_EXPAND_(PDL_OBSTRUCT(Xout)(PDL_EXPAND_ extraargs symbol, __VA_ARGS__)) \
    PDL_EXPAND_(PDL_OBSTRUCT(PDL_GENERICSWITCH)(LISTERin, typevarin, Xin, dfltin, PDL_EXPAND_ extraargs)) \
  } break;
/* two-level */
#define PDL_GENERICSWITCH2(LISTERout, typevarout, Xout, dfltout, LISTERin, typevarin, Xin, dfltin, ...) \
  switch (typevarout) { \
    LISTERout(PDL_GENERICSWITCH_CASEout, Xout, (__VA_ARGS__), LISTERin, typevarin, Xin, dfltin,) \
    default: dfltout; \
  }

#define ANYVAL_FROM_CTYPE_X(outany, inval, datatype, ctype, ppsym, ...) \
  (outany).type = datatype; (outany).value.ppsym = (inval);
#define ANYVAL_FROM_CTYPE(outany,avtype,inval) \
  PDL_GENERICSWITCH(PDL_TYPELIST_ALL, avtype, ANYVAL_FROM_CTYPE_X, \
    outany.type = -1; outany.value.B = 0, \
    outany, inval, \
  )
EOF

for my $type (PDL::Types::types()) {
  my ($ppsym) = map $type->$_, qw(ppsym);
  my $expr = !$type->usenan ? 0 : $type->isnan('x') . '?1:0'; # isnan can return any non-0
  print OUT "#define PDL_ISNAN_$ppsym(x) ($expr)\n";
  my $expr2 = !$type->usenan ? 1 : $type->isfinite('x') . '?1:0';
  print OUT "#define PDL_ISFINITE_$ppsym(x) ($expr2)\n";
}

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'!NO!SUBS!';
#define ANYVAL_TO_ANYVAL_NEWTYPE_X_OUTER(from_val, to_val, newtype, datatype_from, ctype_from, ppsym_from, ...) \
  ctype_from cvalue_from = from_val.value.ppsym_from;
#define ANYVAL_TO_ANYVAL_NEWTYPE_X_INNER(from_val, to_val, newtype, datatype_to, ctype_to, ppsym_to, ...) \
  to_val.value.ppsym_to = cvalue_from; to_val.type = newtype;
#define ANYVAL_TO_ANYVAL_NEWTYPE(from_val, to_val, newtype) \
  PDL_GENERICSWITCH2( \
    PDL_TYPELIST_ALL, from_val.type, ANYVAL_TO_ANYVAL_NEWTYPE_X_OUTER, to_val.type = PDL_INVALID, \
    PDL_TYPELIST_ALL_, newtype, ANYVAL_TO_ANYVAL_NEWTYPE_X_INNER, to_val.type = PDL_INVALID, \
    from_val, to_val, newtype,)

#define ANYVAL_TO_CTYPE_X(outval, inany, datatype, ctype, ppsym, ...) \
  outval = (ctype)(inany.value.ppsym);
#define ANYVAL_TO_CTYPE(outval,ctype,inany) \
  PDL_GENERICSWITCH(PDL_TYPELIST_ALL_, inany.type, ANYVAL_TO_CTYPE_X, \
    outval = 0, \
    outval, inany, \
  )

#define ANYVAL_TO_CTYPE_OFFSET_X(x, ioff, inany, datatype, ctype, ppsym, ...) \
  ((ctype *)(x))[ioff] = (inany).value.ppsym;
#define ANYVAL_TO_CTYPE_OFFSET(x,ioff,datatype,value) \
  PDL_GENERICSWITCH(PDL_TYPELIST_ALL, datatype, ANYVAL_TO_CTYPE_OFFSET_X, \
    return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", datatype), \
    x, ioff, value,)

#define ANYVAL_FROM_CTYPE_OFFSET_X(indata, ioff, outany, datatype, ctype, ppsym, ...) \
  (outany).type = datatype; (outany).value.ppsym = ((ctype *)(indata))[ioff];
#define ANYVAL_FROM_CTYPE_OFFSET(outany,avtype,indata,ioff) \
  PDL_GENERICSWITCH(PDL_TYPELIST_ALL, avtype, ANYVAL_FROM_CTYPE_OFFSET_X, \
    (outany).type = -1; outany.value.B = 0;, \
    indata, ioff, outany,)

#define ANYVAL_ISNAN(x) _anyval_isnan(x)
static inline int _anyval_isnan(PDL_Anyval x) {
#define X(datatype, ctype, ppsym, ...) \
  return PDL_ISNAN_ ## ppsym(x.value.ppsym);
  PDL_GENERICSWITCH(PDL_TYPELIST_ALL, x.type, X, return -1)
#undef X
}

#define ANYVAL_EQ_ANYVAL(x,y) (_anyval_eq_anyval(x,y))
static inline int _anyval_eq_anyval(PDL_Anyval x, PDL_Anyval y) {
#define X_OUTER(datatype_x, ctype_x, ppsym_x, ...) \
  ctype_x cvalue_x = x.value.ppsym_x;
#define X_INNER(datatype_y, ctype_y, ppsym_y, ...) \
  return (cvalue_x == y.value.ppsym_y) ? 1 : 0;
  PDL_GENERICSWITCH2(PDL_TYPELIST_ALL, x.type, X_OUTER, return -1, PDL_TYPELIST_ALL_, y.type, X_INNER, return -1)
#undef X_INNER
#undef X_OUTER
}

#define ANYVAL_ISBAD(inany,badval) _anyval_isbad(inany,badval)
static inline int _anyval_isbad(PDL_Anyval inany, PDL_Anyval badval) {
  int isnan_badval = ANYVAL_ISNAN(badval);
  if (isnan_badval == -1) return -1;
  return isnan_badval ? ANYVAL_ISNAN(inany) : ANYVAL_EQ_ANYVAL(inany, badval);
}

#define PDL_ISBAD(inval,badval,ppsym) \
  (PDL_ISNAN_ ## ppsym(badval) ? PDL_ISNAN_ ## ppsym(inval) : inval == badval)
#define PDL_ISBAD2(inval,badval,ppsym,badval_isnan) \
  (badval_isnan ? PDL_ISNAN_ ## ppsym(inval) : inval == badval)

typedef struct pdl_badvals {
#define X(symbol, ctype, ppsym, ...) ctype ppsym;
PDL_TYPELIST_ALL(X)
#undef X
} pdl_badvals;

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
#define PDL_NCHILDREN  8 /* Number of trans_children ptrs to preallocate */
#define PDL_NBROADCASTIDS 4 /* Number of different broadcastids/pdl to preallocate */

/* Constants for pdl.state - not all combinations make sense */

  /* data allocated for this pdl.  this implies that the data               */
  /* is up to date if !PDL_PARENTCHANGED                                    */
#define  PDL_ALLOCATED           (1 << 0)
  /* Parent data has been altered without changing this pdl                 */
#define  PDL_PARENTDATACHANGED   (1 << 1)
  /* Parent dims or incs has been altered without changing this pdl.        */
#define  PDL_PARENTDIMSCHANGED   (1 << 2)
  /* Physical data representation of the parent has changed (e.g.           */
  /* physical transposition), so incs etc. need to be recalculated.         */
#define  PDL_ANYCHANGED          (PDL_PARENTDATACHANGED|PDL_PARENTDIMSCHANGED)
  /* Dataflow forward flag request        */
#define  PDL_DATAFLOW_F          (1 << 4)
  /* Was this PDL null originally?                                          */
#define  PDL_NOMYDIMS            (1 << 6)
  /* Dims should be received via trans.                                     */
#define  PDL_MYDIMS_TRANS        (1 << 7)
  /* OK to attach a vaffine transformation (i.e. a slice)                   */
#define  PDL_OPT_VAFFTRANSOK     (1 << 8)
#define  PDL_OPT_ANY_OK          (PDL_OPT_VAFFTRANSOK)
  /* This is the hdrcpy flag                                                */
#define  PDL_HDRCPY              (1 << 9)
  /* This is a badval flag for this PDL as set by "badflag"                 */
#define  PDL_BADVAL              (1 << 10)
  /* If ndarray was created by PDL API but then got SV attached             */
#define  PDL_DYNLANG_NODESTROY   (1 << 11)
  /* inplace flag                                                           */
#define  PDL_INPLACE             (1 << 12)
  /* Flag indicating destruction in progress                                */
#define  PDL_DESTROYING          (1 << 13)
  /* If this flag is set, you must not alter the data pointer nor           */
  /* free this ndarray nor use datasv (which should be null).               */
  /* This means e.g. that the ndarray is mmapped from a file                */
#define  PDL_DONTTOUCHDATA       (1 << 14)
  /* whether the given pdl is getting its dims from the given trans */
#define PDL_DIMS_FROM_TRANS(wtrans,pdl) (((pdl)->state & PDL_MYDIMS_TRANS) \
                && (pdl)->trans_parent == (pdl_trans *)(wtrans))

#define PDL_LIST_FLAGS_PDLSTATE(X) \
 X(PDL_ALLOCATED) \
 X(PDL_PARENTDATACHANGED) \
 X(PDL_PARENTDIMSCHANGED) \
 X(PDL_DATAFLOW_F) \
 X(PDL_NOMYDIMS) \
 X(PDL_MYDIMS_TRANS) \
 X(PDL_OPT_VAFFTRANSOK) \
 X(PDL_HDRCPY) \
 X(PDL_BADVAL) \
 X(PDL_DYNLANG_NODESTROY) \
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
#define         PDL_TRANS_DO_BROADCAST            (1 << 0)
#define         PDL_TRANS_BADPROCESS              (1 << 1)
#define         PDL_TRANS_BADIGNORE               (1 << 2)
#define         PDL_TRANS_NO_PARALLEL             (1 << 3)
#define         PDL_TRANS_OUTPUT_OTHERPAR         (1 << 4)

#define PDL_LIST_FLAGS_PDLVTABLE(X) \
  X(PDL_TRANS_DO_BROADCAST) \
  X(PDL_TRANS_BADPROCESS) \
  X(PDL_TRANS_BADIGNORE) \
  X(PDL_TRANS_NO_PARALLEL) \
  X(PDL_TRANS_OUTPUT_OTHERPAR)

typedef struct pdl_trans pdl_trans;

typedef enum {
  PDL_ENONE = 0, /* usable as boolean */
  PDL_EUSERERROR, /* user error, no need to destroy */
  PDL_EFATAL
} pdl_error_type;
typedef struct {
  pdl_error_type error;
  const char *message; /* if error but this NULL, parsing/alloc error */
  char needs_free;
} pdl_error;

typedef struct pdl_transvtable {
        int flags;
        int iflags; /* flags that are starting point for pdl_trans.flags */
        pdl_datatypes *gentypes; /* ordered list of types handled, ending -1 */
        PDL_Indx nparents;
        PDL_Indx npdls;
        char *per_pdl_flags; /*CORE21*/
        PDL_Indx *par_realdims; /* quantity of dimensions each par has */
        char **par_names;
        short *par_flags;
        pdl_datatypes *par_types;
        PDL_Indx *par_realdim_ind_start; /* each par, where do its inds start in array above */
        PDL_Indx *par_realdim_ind_ids; /* each realdim, which ind is source */
        PDL_Indx nind_ids;
        PDL_Indx ninds;
        char **ind_names; /* sorted names of "indices", eg for a(m), the "m" */
        pdl_error (*redodims)(pdl_trans *tr); /* Only dims and internal trans (makes phys) */
        pdl_error (*readdata)(pdl_trans *tr); /* Only data, to "data" ptr  */
        pdl_error (*writebackdata)(pdl_trans *tr); /* "data" ptr to parent or granny */
        pdl_error (*freetrans)(pdl_trans *tr, char);
        int structsize;
        char *name;                           /* For debuggers, mostly */
} pdl_transvtable;

/* offset into either par_realdim_ind_ids or inc_sizes */
#define PDL_INC_ID(vtable, i, j) \
  ((vtable)->par_realdim_ind_start[i] + j)
/* which ind_id (named dim) for the i-th pdl (aka param) in a vtable, the j-th dim on that param */
#define PDL_IND_ID(vtable, i, j) \
  ((vtable)->par_realdim_ind_ids[PDL_INC_ID(vtable, i, j)])

#define PDL_PARAM_ISREAL             (1 << 0)
#define PDL_PARAM_ISCOMPLEX          (1 << 1)
#define PDL_PARAM_ISTYPED            (1 << 2)
#define PDL_PARAM_ISTPLUS            (1 << 3)
#define PDL_PARAM_ISCREAT            (1 << 4)
#define PDL_PARAM_ISCREATEALWAYS     (1 << 5)
#define PDL_PARAM_ISOUT              (1 << 6)
#define PDL_PARAM_ISTEMP             (1 << 7)
#define PDL_PARAM_ISWRITE            (1 << 8)
#define PDL_PARAM_ISPHYS             (1 << 9)
#define PDL_PARAM_ISIGNORE           (1 << 10)
#define PDL_PARAM_ISNOTCOMPLEX       (1 << 11)

#define PDL_LIST_FLAGS_PARAMS(X) \
 X(PDL_PARAM_ISREAL) \
 X(PDL_PARAM_ISCOMPLEX) \
 X(PDL_PARAM_ISTYPED) \
 X(PDL_PARAM_ISTPLUS) \
 X(PDL_PARAM_ISCREAT) \
 X(PDL_PARAM_ISCREATEALWAYS) \
 X(PDL_PARAM_ISOUT) \
 X(PDL_PARAM_ISTEMP) \
 X(PDL_PARAM_ISWRITE) \
 X(PDL_PARAM_ISPHYS) \
 X(PDL_PARAM_ISIGNORE) \
 X(PDL_PARAM_ISNOTCOMPLEX)

/* All trans must start with this */

/* Trans flags */

        /* Reversible transform -- flag indicates data can flow both ways.  */
        /* This is critical in routines that both input from and output to  */
        /* a non-single-valued pdl: updating must occur.  (Note that the    */
        /* transform is not necessarily mathematically reversible)          */
#define  PDL_ITRANS_TWOWAY            (1 << 0)
#define  PDL_ITRANS_DO_DATAFLOW_F     (1 << 1)
#define  PDL_ITRANS_DO_DATAFLOW_B     (1 << 2)
#define  PDL_ITRANS_DO_DATAFLOW_ANY   (PDL_ITRANS_DO_DATAFLOW_F|PDL_ITRANS_DO_DATAFLOW_B)

#define  PDL_ITRANS_ISAFFINE          (1 << 12)

#define PDL_LIST_FLAGS_PDLTRANS(X) \
 X(PDL_ITRANS_TWOWAY) \
 X(PDL_ITRANS_DO_DATAFLOW_F) \
 X(PDL_ITRANS_DO_DATAFLOW_B) \
 X(PDL_ITRANS_ISAFFINE)

#define PDL_MAXSPACE 256 /* maximal number of prefix spaces in dump routines */
#define PDL_MAXLIN 75

// These define struct pdl_trans and all derived structures. There are many
// structures that defined in other parts of the code that can be referenced
// like a pdl_trans* because all of these structures have the same pdl_trans
// initial piece. These structures can contain multiple pdl* elements in them.
// Thus pdl_trans itself ends with a flexible pdl*[] array, which can be used to
// reference any number of pdl objects. As a result pdl_trans itself can NOT be
// instantiated

#define PDL_TRANS_START_COMMON                                          \
  unsigned int magicno;                                                 \
  short flags;                                                          \
  pdl_transvtable *vtable;                                              \
  int bvalflag;                                                         \
  pdl_broadcast broadcast;                                              \
  PDL_Indx *ind_sizes;                                                  \
  PDL_Indx *inc_sizes;                                                  \
  char dims_redone;                                                     \
  PDL_Indx *incs; PDL_Indx offs; /* only used for affine */             \
  void *params;                                                         \
  pdl_datatypes __datatype

#define PDL_TRANS_START(np) \
  PDL_TRANS_START_COMMON; \
  /* The pdls involved in the transformation */ \
  pdl *pdls[np]

#define PDL_TRANS_START_FLEXIBLE() \
  PDL_TRANS_START_COMMON; \
  /* The pdls involved in the transformation */ \
  pdl *pdls[]

#define PDL_TR_MAGICNO      0x91827364
#define PDL_TR_CHKMAGIC(it) PDL_CHKMAGIC_GENERAL(it, PDL_TR_MAGICNO, "TRANS")
#define PDL_TR_SETMAGIC(it) (it)->magicno = PDL_TR_MAGICNO

// This is a generic parent of all the trans structures. It is a flexible array
// (can store an arbitrary number of pdl objects). Thus this can NOT be
// instantiated, only "child" structures can
struct pdl_trans {
  PDL_TRANS_START_FLEXIBLE();
} ;

typedef struct pdl_vaffine {
	PDL_TRANS_START(2);
	PDL_Indx ndims;
	pdl *from;
} pdl_vaffine;

#define PDL_VAFFOK(pdl) (pdl->state & PDL_OPT_VAFFTRANSOK)
#define PDL_REPRINCS(pdl) (PDL_VAFFOK(pdl) ? pdl->vafftrans->incs : pdl->dimincs)
#define PDL_REPRINC(pdl,which) (PDL_REPRINCS(pdl)[which])

#define PDL_REPROFFS(pdl) (PDL_VAFFOK(pdl) ? pdl->vafftrans->offs : 0)

#define PDL_REPRP(pdl) (PDL_VAFFOK(pdl) ? pdl->vafftrans->from->data : pdl->data)

typedef struct pdl_trans_children {
        pdl_trans *trans[PDL_NCHILDREN];
        struct pdl_trans_children *next;
} pdl_trans_children;

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
#define PDL_CHKMAGIC(it) PDL_CHKMAGIC_GENERAL(it,PDL_MAGICNO,"PDL")
#define PDL_SETMAGIC(it) (it)->magicno = PDL_MAGICNO

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
   PDL_Anyval badvalue; /* BAD value is stored as a PDL_Anyval for portability */
   int has_badvalue;     /* whether this pdl has non-default badval CORE21 make into state flag */
   PDL_Indx nvals; /* Real number of elements (not quite nelem in case of dummy) */
   PDL_Indx nbytes; /* number of bytes allocated in data */
   pdl_datatypes datatype; /* One of the usual suspects (PDL_L, PDL_D, etc.) */
   PDL_Indx   *dims;      /* Array of data dimensions - could point below or to an allocated array */
   PDL_Indx   *dimincs;   /* Array of data default increments, aka strides through memory for each dim (0 for dummies) */
   PDL_Indx    ndims;     /* Number of data dimensions in dims and dimincs */

   PDL_Indx *broadcastids;  /* Starting index of the broadcast index set n */
   PDL_Indx nbroadcastids;

   pdl_trans_children trans_children;

   PDL_Indx   def_dims[PDL_NDIMS];   /* Preallocated space for efficiency */
   PDL_Indx   def_dimincs[PDL_NDIMS];   /* Preallocated space for efficiency */
   PDL_Indx   def_broadcastids[PDL_NBROADCASTIDS];

   struct pdl_magic *magic;

   void *hdrsv; /* "header", settable from Perl */
   PDL_Value value; /* to store at least one value */
   PDL_Indx ntrans_children; /* CORE21 put next to other trans-tracking stuff */
};

typedef struct pdl_slice_args {
  PDL_Indx start; /* maps to start index of slice range (inclusive) */
  PDL_Indx end; /* maps to end index of slice range (inclusive) */
  PDL_Indx inc; /* maps to increment of slice range */
  char dummy, squish; /* boolean */
  struct pdl_slice_args *next; /* NULL is last */
} pdl_slice_args;

/*************
 * Some macros for looping over the trans_children of a given PDL
 */
#define PDL_DECL_CHILDLOOP(p) \
  int p##__i; pdl_trans_children *p##__c; PDL_Indx p##__unfound;
#define PDL_START_CHILDLOOP(p) \
  p##__c = &p->trans_children; p##__unfound = p->ntrans_children; \
  do { \
    if (p##__unfound <= 0) break; \
    for (p##__i=0; p##__i<PDL_NCHILDREN; p##__i++) { \
      if (p##__c->trans[p##__i]) { \
        p##__unfound--;
#define PDL_CHILDLOOP_THISCHILD(p) p##__c->trans[p##__i]
#define PDL_END_CHILDLOOP(p) \
      } \
    } \
    if (!p##__c) break; \
    if (!p##__c->next) break; \
    p##__c=p##__c->next; \
  } while (1);

#define PDL_USESTRUCTVALUE(it) \
        (it->nbytes <= sizeof(it->value))

#define PDLMAX(a,b) ((a) > (b) ? (a) : (b))
#define PDLMIN(a,b) ((a) < (b) ? (a) : (b))
#define PDL_ABS(A) ( (A)>=0 ? (A) : -(A) )

#define PDL_RETERROR2(rv, expr, iferr) \
  do { rv = expr; if (rv.error) { iferr } } while (0)
#define PDL_RETERROR(rv, expr) PDL_RETERROR2(rv, expr, return rv;)
#define PDL_ACCUMERROR(rv, expr) \
  do { \
    pdl_error rv##_local = expr; \
    if (rv##_local.error) rv = pdl_error_accumulate(rv, rv##_local); \
  } while (0)

#define PDL_ENSURE_ALLOCATED(it) \
  if (!(it->state & PDL_ALLOCATED)) { \
    PDL_RETERROR(PDL_err, pdl_allocdata(it)); \
  }

/* for use with PDL_TYPELIST_REAL */
#define PDL_QSORT(symbol, ctype, ppsym, ...) \
  static inline void qsort_ ## ppsym(ctype* xx, PDL_Indx a, PDL_Indx b) { \
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

#define PDL_BROADCASTLOOP_START(funcName, brc, vtable, ptrStep1, ptrStep2, ptrStep3) \
  __brcloopval = PDL->startbroadcastloop(&(brc),(vtable)->funcName, __privtrans, &PDL_err); \
  if (PDL_err.error) return PDL_err; \
  if ( __brcloopval < 0 ) return PDL->make_error_simple(PDL_EFATAL, "Error starting broadcastloop"); \
  if ( __brcloopval ) return PDL_err; \
  do { \
    PDL_Indx *__tdims = PDL->get_broadcastdims(&(brc)); \
    if (!__tdims) return PDL->make_error_simple(PDL_EFATAL, "Error in get_broadcastdims"); \
    register PDL_Indx __tdims0 = __tdims[0]; \
    register PDL_Indx __tdims1 = __tdims[1]; \
    register PDL_Indx *__offsp = PDL->get_threadoffsp(&(brc)); \
    if (!__offsp ) return PDL->make_error_simple(PDL_EFATAL, "Error in get_threadoffsp"); \
    /* incs are each pdl's stride, declared at func start */ \
    /* offs are each pthread's starting offset into each pdl */ \
    ptrStep1 \
    for( __tind1 = 0 ; \
      __tind1 < __tdims1 ; \
      __tind1++ \
      /* step by tinc1, undoing inner-loop of tinc0*tdims0 */ \
      PDL_EXPAND ptrStep2 \
    ) \
    { \
      for( __tind0 = 0 ; \
        __tind0 < __tdims0 ; \
        __tind0++ \
        PDL_EXPAND ptrStep3 \
      ) { \
        /* This is the tightest loop. Make sure inside is optimal. */
#define PDL_BROADCASTLOOP_END(brc, ptrStep1) \
      } \
    } \
    /* undo outer-loop of tinc1*tdims1, and original per-pthread offset */ \
    ptrStep1 \
    __brcloopval = PDL->iterbroadcastloop(&(brc),2); \
    if ( __brcloopval < 0 ) return PDL->make_error_simple(PDL_EFATAL, "Error in iterbroadcastloop"); \
  } while(__brcloopval);

/* __PDL_H */
#endif

!NO!SUBS!
