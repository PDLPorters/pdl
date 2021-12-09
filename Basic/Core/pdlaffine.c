#include "pdl.h"
#define PDL_IN_CORE
#include "pdlcore.h"

/* generated from:
pp_def(
       'affineinternal',
       HandleBad => 1,
       AffinePriv => 1,
       P2Child => 1,
       ReadDataFuncName => "pdl_readdata_affineinternal",
       WriteBackDataFuncName => "pdl_writebackdata_affineinternal",
       EquivCPOffsCode => '
          if ($PDL(CHILD)->state & $PDL(PARENT)->state & PDL_ALLOCATED) {
            PDL_Indx i, poffs=$PRIV(offs), nd;
            for(i=0; i<$PDL(CHILD)->nvals; i++) {
              $EQUIVCPOFFS(i,poffs);
              for(nd=0; nd<$PDL(CHILD)->ndims; nd++) {
                poffs += $PRIV(incs[nd]);
                if( (nd<$PDL(CHILD)->ndims-1 &&
                     (i+1)%$PDL(CHILD)->dimincs[nd+1]) ||
                   nd == $PDL(CHILD)->ndims-1)
                        break;
                poffs -= $PRIV(incs[nd]) *
                        $PDL(CHILD)->dims[nd];
              }
            }
          }',
       Doc => undef,    # 'internal',
);
*/

#define COPYDATA(ctype, from_id, to_id) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype, (trans->vtable->per_pdl_flags[to_id]), to_pdl, (trans->pdls[to_id])) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype, (trans->vtable->per_pdl_flags[from_id]), from_pdl, (trans->pdls[from_id])) \
  PDL_Indx i, poffs=trans->offs, nd; \
  for (i=0; i<trans->pdls[to_id]->nvals ; i++) { \
    to_pdl_physdatap[i] = (trans->bvalflag && from_pdl_physdatap[poffs] == from_pdl_badval) \
      ? to_pdl_badval : from_pdl_physdatap[poffs]; \
    for (nd=0; nd<trans->pdls[to_id]->ndims ; nd++) { \
      poffs += trans->incs[nd]; \
      if ((nd<trans->pdls[to_id]->ndims -1 && \
          (i+1)%trans->pdls[to_id]->dimincs[nd+1]) || \
         nd == trans->pdls[to_id]->ndims -1) \
              break; \
      poffs -= trans->incs[nd] * trans->pdls[to_id]->dims[nd]; \
    } \
  }

void pdl_readdata_affine(pdl_trans *trans) {
  if (!(trans->pdls[0]->state & trans->pdls[1]->state & PDL_ALLOCATED)) return;
#define X(sym, ctype, ppsym, shortctype, defbval) COPYDATA(ctype, 0, 1)
  PDL_GENERICSWITCH(trans->__datatype, X, croak("Not a known data type code=%d", trans->__datatype))
#undef X
}

void pdl_writebackdata_affine(pdl_trans *trans) {
  if (!(trans->pdls[0]->state & trans->pdls[1]->state & PDL_ALLOCATED)) return;
#define X(sym, ctype, ppsym, shortctype, defbval) COPYDATA(ctype, 1, 0)
  PDL_GENERICSWITCH(trans->__datatype, X, croak("Not a known data type code=%d", trans->__datatype))
#undef X
}

/* generated from:
pp_def( 'affine',
        P2Child => 1,
        TwoWay => 1,
        AffinePriv => 1,
        GlobalNew => 'affine_new',
        OtherPars => 'PDL_Indx offspar; SV *dimlist; SV *inclist;',
        Comp => 'PDL_Indx nd; PDL_Indx offset; PDL_Indx sdims[$COMP(nd)];
                PDL_Indx sincs[$COMP(nd)];',
        MakeComp => '
                PDL_Indx i = 0, n2 = 0;
                PDL_Indx *tmpi = pdl_packdims(inclist,&n2);
                if (!tmpi) $CROAK("Failed to packdims for tmpi");
                PDL_Indx *tmpd = pdl_packdims(dimlist,&($COMP(nd)));
                if (!tmpd) $CROAK("Failed to packdims for tmpd");
                if ($COMP(nd) < 0) {
                      $CROAK("Affine: can not have negative no of dims");
                }
                if ($COMP(nd) != n2)
                      $CROAK("Affine: number of incs does not match dims");
                $DOCOMPALLOC();
                $COMP(offset) = offspar;
                for (i=0; i<$COMP(nd); i++) {
                        $COMP(sdims)[i] = tmpd[i];
                        $COMP(sincs)[i] = tmpi[i];
                }
                ',
        RedoDims => '
                PDL_Indx i;
                $SETNDIMS($COMP(nd));
                $DOPRIVALLOC();
                $PRIV(offs) = $COMP(offset);
                for (i=0;i<$CHILD(ndims);i++) {
                        $PRIV(incs)[i] = $COMP(sincs)[i];
                        $CHILD(dims)[i] = $COMP(sdims)[i];
                }
                $SETDIMS();
                ',
        Doc => undef,
);
*/

typedef struct pdl_params_affine {
  PDL_Indx  nd;
  PDL_Indx  offset;
  PDL_Indx  *sdims;
  PDL_Indx  *sincs;
} pdl_params_affine;

void pdl_affine_redodims(pdl_trans *trans) {
  pdl_params_affine *params = trans->params;
  pdl *__it = trans->pdls[1];
  pdl_hdr_childcopy(trans);
  PDL_Indx i;
  pdl_reallocdims(__it, params->nd);
  trans->incs = malloc(sizeof(*trans->incs) * trans->pdls[1]->ndims);
  if (!trans->incs) croak("Out of Memory\n");
  trans->offs = params->offset;
  for (i=0;i<trans->pdls[1]->ndims;i++) {
    trans->incs[i] = params->sincs[i];
    trans->pdls[1]->dims[i] = params->sdims[i];
  }
  pdl_setdims_careful(__it);
  trans->dims_redone = 1;
}

void pdl_affine_free(pdl_trans *trans, char destroy) {
  pdl_params_affine *params = trans->params;
  if (destroy) {
    free(params->sdims);
    free(params->sincs);
  }
  if ((trans)->dims_redone) free(trans->incs);
}

static pdl_datatypes pdl_affine_vtable_gentypes[] = { PDL_B, PDL_S, PDL_US, PDL_L, PDL_IND, PDL_LL, PDL_F, PDL_D, PDL_CF, PDL_CD, -1 };
static char pdl_affine_vtable_flags[] = {
  PDL_TPDL_VAFFINE_OK, PDL_TPDL_VAFFINE_OK
};
static PDL_Indx pdl_affine_vtable_realdims[] = { 0, 0 };
static char *pdl_affine_vtable_parnames[] = { "PARENT","CHILD" };
static short pdl_affine_vtable_parflags[] = {
  0,
  PDL_PARAM_ISCREAT|PDL_PARAM_ISCREATEALWAYS|PDL_PARAM_ISOUT|PDL_PARAM_ISWRITE
};
static pdl_datatypes pdl_affine_vtable_partypes[] = { -1, -1 };
static PDL_Indx pdl_affine_vtable_realdims_starts[] = { 0, 0 };
static PDL_Indx pdl_affine_vtable_realdims_ind_ids[] = { 0 };
static char *pdl_affine_vtable_indnames[] = { "" };
pdl_transvtable pdl_affine_vtable = {
  0, PDL_ITRANS_ISAFFINE|PDL_ITRANS_TWOWAY|PDL_ITRANS_DO_DATAFLOW_F|PDL_ITRANS_DO_DATAFLOW_B, pdl_affine_vtable_gentypes, 1, 2, pdl_affine_vtable_flags,
  pdl_affine_vtable_realdims, pdl_affine_vtable_parnames,
  pdl_affine_vtable_parflags, pdl_affine_vtable_partypes,
  pdl_affine_vtable_realdims_starts, pdl_affine_vtable_realdims_ind_ids, 0,
  0, pdl_affine_vtable_indnames,
  pdl_affine_redodims, NULL, NULL,
  pdl_affine_free,
  sizeof(pdl_params_affine),"affine_new"
};

void pdl_affine_new(pdl *PARENT,pdl *CHILD,PDL_Indx offspar,SV *dimlist,SV *inclist) {
  pdl_trans *trans = (void *)pdl_create_trans(&pdl_affine_vtable);
  pdl_params_affine *params = trans->params;
  trans->pdls[0] = PARENT;
  trans->pdls[1] = CHILD;
  char badflag_cache = pdl_trans_badflag_from_inputs((pdl_trans *)trans);
  pdl_type_coerce((pdl_trans *)trans);
  PARENT = trans->pdls[0];
  CHILD = trans->pdls[1];
  PDL_Indx i = 0, n2 = 0;
  PDL_Indx *tmpi = pdl_packdims(inclist,&n2);
  if (!tmpi) pdl_pdl_barf("Failed to packdims for tmpi");
  PDL_Indx *tmpd = pdl_packdims(dimlist,&(params->nd));
  if (!tmpd) pdl_pdl_barf("Failed to packdims for tmpd");
  if (params->nd < 0)
    pdl_pdl_barf("Error in affine: can not have negative no of dims");
  if (params->nd != n2)
    pdl_pdl_barf("Error in affine: number of incs does not match dims");
  params->sdims = malloc(sizeof(* params->sdims) * params->nd);
  if (!params->sdims) croak("Out of Memory\n");
  params->sincs = malloc(sizeof(* params->sincs) * params->nd);
  if (!params->sincs) croak("Out of Memory\n");
  params->offset = offspar;
  for (i=0; i<params->nd; i++) {
    params->sdims[i] = tmpd[i];
    params->sincs[i] = tmpi[i];
  }
  pdl_make_trans_mutual((pdl_trans *)trans);
  if (badflag_cache)
    CHILD->state |= PDL_BADVAL;
}

/* generated from:
pp_def(
        'converttypei',
        GlobalNew => 'converttypei_new',
        OtherPars => 'int totype;',
        Identity => 1,
# Forced types
        FTypes => {CHILD => '$COMP(totype)'},
        Doc => 'internal',
);
*/

typedef struct pdl_params_converttypei {
  int  totype;
} pdl_params_converttypei;

void pdl_converttypei_redodims(pdl_trans *trans) {
  pdl *__it = trans->pdls[1];
  pdl_hdr_childcopy(trans);
  PDL_Indx i;
  pdl_reallocdims(__it, trans->pdls[0]->ndims);
  for (i=0; i<trans->pdls[1]->ndims; i++)
    trans->pdls[1]->dims[i] = trans->pdls[0]->dims[i];
  pdl_setdims_careful(__it);
  pdl_reallocthreadids(trans->pdls[1], trans->pdls[0]->nthreadids);
  for (i=0; i<trans->pdls[0]->nthreadids; i++)
    trans->pdls[1]->threadids[i] = trans->pdls[0]->threadids[i];
  trans->dims_redone = 1;
}

#define COPYCONVERT(from_pdl, to_pdl) \
  { \
    PDL_Indx i; \
    for(i=0; i<trans->pdls[1]->nvals; i++) { \
      to_pdl ## _physdatap[i] = trans->bvalflag && from_pdl ## _physdatap[i] == from_pdl ## _badval \
        ? to_pdl ## _badval \
        : from_pdl ## _physdatap[i]; \
      ; \
    } \
  }

void pdl_converttypei_readdata(pdl_trans *trans) {
  pdl_params_converttypei *params = trans->params;
#define X_OUTER(datatype_out, ctype_out, ppsym_out, shortctype_out, defbval_out) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype_out, (trans->vtable->per_pdl_flags[1]), CHILD, (trans->pdls[1])) \
  PDL_GENERICSWITCH2(trans->__datatype, X_INNER, croak("Not a known data type code=%d", trans->__datatype))
#define X_INNER(datatype_in, ctype_in, ppsym_in, shortctype_in, defbval_in) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype_in, (trans->vtable->per_pdl_flags[0]), PARENT, (trans->pdls[0])) \
  COPYCONVERT(PARENT, CHILD)
  PDL_GENERICSWITCH(params->totype, X_OUTER, croak("Not a known data type code=%d", params->totype))
#undef X_INNER
}

void pdl_converttypei_writebackdata(pdl_trans *trans) {
  pdl_params_converttypei *params = trans->params;
#define X_INNER(datatype_in, ctype_in, ppsym_in, shortctype_in, defbval_in) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype_in, (trans->vtable->per_pdl_flags[0]), PARENT, (trans->pdls[0])) \
  COPYCONVERT(CHILD, PARENT)
  PDL_GENERICSWITCH(params->totype, X_OUTER, croak("Not a known data type code=%d", params->totype))
#undef X_INNER
#undef X_OUTER
}

static pdl_datatypes pdl_converttypei_vtable_gentypes[] = { PDL_B, PDL_S, PDL_US, PDL_L, PDL_IND, PDL_LL, PDL_F, PDL_D, PDL_CF, PDL_CD, -1 };
static char pdl_converttypei_vtable_flags[] = {
  0, 0
};
static PDL_Indx pdl_converttypei_vtable_realdims[] = { 0, 0 };
static char *pdl_converttypei_vtable_parnames[] = { "PARENT","CHILD" };
static short pdl_converttypei_vtable_parflags[] = {
  0,
  PDL_PARAM_ISCREAT|PDL_PARAM_ISCREATEALWAYS|PDL_PARAM_ISIGNORE|PDL_PARAM_ISOUT|PDL_PARAM_ISWRITE
};
static pdl_datatypes pdl_converttypei_vtable_partypes[] = { -1, -1 };
static PDL_Indx pdl_converttypei_vtable_realdims_starts[] = { 0, 0 };
static PDL_Indx pdl_converttypei_vtable_realdims_ind_ids[] = { 0 };
static char *pdl_converttypei_vtable_indnames[] = { "" };
pdl_transvtable pdl_converttypei_vtable = {
  PDL_TRANS_BADPROCESS, PDL_ITRANS_TWOWAY|PDL_ITRANS_DO_DATAFLOW_F|PDL_ITRANS_DO_DATAFLOW_B, pdl_converttypei_vtable_gentypes, 1, 2, pdl_converttypei_vtable_flags,
  pdl_converttypei_vtable_realdims, pdl_converttypei_vtable_parnames,
  pdl_converttypei_vtable_parflags, pdl_converttypei_vtable_partypes,
  pdl_converttypei_vtable_realdims_starts, pdl_converttypei_vtable_realdims_ind_ids, 0,
  0, pdl_converttypei_vtable_indnames,
  pdl_converttypei_redodims, pdl_converttypei_readdata, pdl_converttypei_writebackdata,
  NULL,
  sizeof(pdl_params_converttypei),"converttypei_new"
};

void pdl_converttypei_new(pdl  *PARENT,pdl  *CHILD,int  totype) {
  pdl_trans *trans = (void *)pdl_create_trans(&pdl_converttypei_vtable);
  pdl_params_converttypei *params = trans->params;
  trans->pdls[0] = PARENT;
  trans->pdls[1] = CHILD;
  char badflag_cache = pdl_trans_badflag_from_inputs((pdl_trans *)trans);
  pdl_type_coerce((pdl_trans *)trans);
  PARENT = trans->pdls[0];
  CHILD = trans->pdls[1];
  CHILD->datatype = params->totype = totype;
  pdl_make_trans_mutual((pdl_trans *)trans);
  if (badflag_cache)
    CHILD->state |= PDL_BADVAL;
}
