#include "pdl.h"
#define PDL_IN_CORE
#include "pdlcore.h"

#define PDL_ALL_GENTYPES { PDL_SB, PDL_B, PDL_S, PDL_US, PDL_L, PDL_UL, PDL_IND, PDL_ULL, PDL_LL, PDL_F, PDL_D, PDL_LD, PDL_CF, PDL_CD, PDL_CLD, -1 }

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

pdl_error pdl_readdata_affine(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  if (!(trans->pdls[0]->state & trans->pdls[1]->state & PDL_ALLOCATED)) return PDL_err;
#define X(sym, ctype, ...) COPYDATA(ctype, 0, 1)
  PDL_GENERICSWITCH(PDL_TYPELIST2_ALL, trans->__datatype, X, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", trans->__datatype))
#undef X
  return PDL_err;
}

pdl_error pdl_writebackdata_affine(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  if (!(trans->pdls[0]->state & trans->pdls[1]->state & PDL_ALLOCATED)) return PDL_err;
#define X(sym, ctype, ...) COPYDATA(ctype, 1, 0)
  PDL_GENERICSWITCH(PDL_TYPELIST2_ALL, trans->__datatype, X, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", trans->__datatype))
#undef X
  return PDL_err;
}

/* generated from:
pp_def( 'affine',
        P2Child => 1,
        TwoWay => 1,
        AffinePriv => 1,
        GlobalNew => 'affine_new',
        OtherPars => 'PDL_Indx offspar; PDL_Indx dims[]; PDL_Indx incs[]',
        Comp => 'PDL_Indx nd; PDL_Indx offset; PDL_Indx sdims[$COMP(nd)];
                PDL_Indx sincs[$COMP(nd)];',
        MakeComp => '
                PDL_Indx i = 0;
                $COMP(nd) = dims_count;
                if ($COMP(nd) < 0)
                      $CROAK("Affine: can not have negative no of dims");
                if ($COMP(nd) != incs_count)
                      $CROAK("Affine: number of incs does not match dims");
                $DOCOMPALLOC();
                $COMP(offset) = offspar;
                for (i=0; i<$COMP(nd); i++) {
                        $COMP(sdims)[i] = dims[i];
                        $COMP(sincs)[i] = incs[i];
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

pdl_error pdl_affine_redodims(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  pdl_params_affine *params = trans->params;
  pdl *__it = trans->pdls[1];
  pdl_hdr_childcopy(trans);
  PDL_Indx i;
  PDL_RETERROR(PDL_err, pdl_reallocdims(__it, params->nd));
  trans->incs = malloc(sizeof(*trans->incs) * trans->pdls[1]->ndims);
  if (!trans->incs) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
  trans->offs = params->offset;
  for (i=0;i<trans->pdls[1]->ndims;i++) {
    trans->incs[i] = params->sincs[i];
    trans->pdls[1]->dims[i] = params->sdims[i];
  }
  PDL_RETERROR(PDL_err, pdl_setdims_careful(__it));
  trans->dims_redone = 1;
  return PDL_err;
}

pdl_error pdl_affine_free(pdl_trans *trans, char destroy) {
  pdl_error PDL_err = {0, NULL, 0};
  pdl_params_affine *params = trans->params;
  if (destroy) {
    free(params->sdims);
    free(params->sincs);
  }
  if ((trans)->dims_redone) free(trans->incs);
  return PDL_err;
}

static pdl_datatypes pdl_affine_vtable_gentypes[] = PDL_ALL_GENTYPES;
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
  0, PDL_ITRANS_ISAFFINE|PDL_ITRANS_TWOWAY|PDL_ITRANS_DO_DATAFLOW_ANY, pdl_affine_vtable_gentypes, 1, 2, pdl_affine_vtable_flags,
  pdl_affine_vtable_realdims, pdl_affine_vtable_parnames,
  pdl_affine_vtable_parflags, pdl_affine_vtable_partypes,
  pdl_affine_vtable_realdims_starts, pdl_affine_vtable_realdims_ind_ids, 0,
  0, pdl_affine_vtable_indnames,
  pdl_affine_redodims, NULL, NULL,
  pdl_affine_free,
  sizeof(pdl_params_affine),"affine_new"
};

pdl_error pdl_affine_new(pdl *PARENT,pdl *CHILD,PDL_Indx offspar,PDL_Indx *dims,PDL_Indx dims_count, PDL_Indx *incs, PDL_Indx incs_count) {
  pdl_error PDL_err = {0, NULL, 0};
  pdl_trans *trans = (void *)pdl_create_trans(&pdl_affine_vtable);
  pdl_params_affine *params = trans->params;
  trans->pdls[0] = PARENT;
  trans->pdls[1] = CHILD;
  PDL_RETERROR(PDL_err, pdl_trans_check_pdls(trans));
  char badflag_cache = pdl_trans_badflag_from_inputs((pdl_trans *)trans);
  pdl_type_coerce((pdl_trans *)trans);
  PARENT = trans->pdls[0];
  CHILD = trans->pdls[1];
  PDL_Indx i = 0;
  params->nd = dims_count;
  if (params->nd < 0)
    return pdl_make_error_simple(PDL_EUSERERROR, "Error in affine: can not have negative no of dims");
  if (params->nd != incs_count)
    return pdl_make_error_simple(PDL_EUSERERROR, "Error in affine: number of incs does not match dims");
  params->sdims = malloc(sizeof(* params->sdims) * params->nd);
  if (!params->sdims) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
  params->sincs = malloc(sizeof(* params->sincs) * params->nd);
  if (!params->sincs) return pdl_make_error_simple(PDL_EFATAL, "Out of Memory\n");
  params->offset = offspar;
  for (i=0; i<params->nd; i++) {
    params->sdims[i] = dims[i];
    params->sincs[i] = incs[i];
  }
  PDL_RETERROR(PDL_err, pdl_make_trans_mutual((pdl_trans *)trans));
  if (badflag_cache)
    CHILD->state |= PDL_BADVAL;
  return PDL_err;
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

pdl_error pdl_converttypei_redodims(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  pdl *__it = trans->pdls[1];
  pdl_hdr_childcopy(trans);
  PDL_Indx i;
  PDL_RETERROR(PDL_err, pdl_reallocdims(__it, trans->pdls[0]->ndims));
  for (i=0; i<trans->pdls[1]->ndims; i++)
    trans->pdls[1]->dims[i] = trans->pdls[0]->dims[i];
  PDL_RETERROR(PDL_err, pdl_setdims_careful(__it));
  pdl_reallocbroadcastids(trans->pdls[1], trans->pdls[0]->nbroadcastids);
  for (i=0; i<trans->pdls[0]->nbroadcastids; i++)
    trans->pdls[1]->broadcastids[i] = trans->pdls[0]->broadcastids[i];
  trans->dims_redone = 1;
  return PDL_err;
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

pdl_error pdl_converttypei_readdata(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  pdl_params_converttypei *params = trans->params;
#define X_OUTER(datatype_out, ctype_out, ...) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype_out, (trans->vtable->per_pdl_flags[1]), CHILD, (trans->pdls[1])) \
  PDL_GENERICSWITCH2(PDL_TYPELIST2_ALL_, trans->__datatype, X_INNER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", trans->__datatype))
#define X_INNER(datatype_in, ctype_in, ...) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype_in, (trans->vtable->per_pdl_flags[0]), PARENT, (trans->pdls[0])) \
  COPYCONVERT(PARENT, CHILD)
  PDL_GENERICSWITCH(PDL_TYPELIST2_ALL, params->totype, X_OUTER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", params->totype))
#undef X_INNER
  return PDL_err;
}

pdl_error pdl_converttypei_writebackdata(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  pdl_params_converttypei *params = trans->params;
#define X_INNER(datatype_in, ctype_in, ...) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype_in, (trans->vtable->per_pdl_flags[0]), PARENT, (trans->pdls[0])) \
  COPYCONVERT(CHILD, PARENT)
  PDL_GENERICSWITCH(PDL_TYPELIST2_ALL, params->totype, X_OUTER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", params->totype))
#undef X_INNER
#undef X_OUTER
  return PDL_err;
}

static pdl_datatypes pdl_converttypei_vtable_gentypes[] = PDL_ALL_GENTYPES;
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
  PDL_TRANS_BADPROCESS, PDL_ITRANS_TWOWAY|PDL_ITRANS_DO_DATAFLOW_ANY, pdl_converttypei_vtable_gentypes, 1, 2, pdl_converttypei_vtable_flags,
  pdl_converttypei_vtable_realdims, pdl_converttypei_vtable_parnames,
  pdl_converttypei_vtable_parflags, pdl_converttypei_vtable_partypes,
  pdl_converttypei_vtable_realdims_starts, pdl_converttypei_vtable_realdims_ind_ids, 0,
  0, pdl_converttypei_vtable_indnames,
  pdl_converttypei_redodims, pdl_converttypei_readdata, pdl_converttypei_writebackdata,
  NULL,
  sizeof(pdl_params_converttypei),"converttypei_new"
};

pdl_error pdl_converttypei_new(pdl  *PARENT,pdl  *CHILD,int  totype) {
  pdl_error PDL_err = {0, NULL, 0};
  pdl_trans *trans = (void *)pdl_create_trans(&pdl_converttypei_vtable);
  pdl_params_converttypei *params = trans->params;
  trans->pdls[0] = PARENT;
  trans->pdls[1] = CHILD;
  PDL_RETERROR(PDL_err, pdl_trans_check_pdls(trans));
  char badflag_cache = pdl_trans_badflag_from_inputs((pdl_trans *)trans);
  pdl_type_coerce((pdl_trans *)trans);
  PARENT = trans->pdls[0];
  CHILD = trans->pdls[1];
  CHILD->datatype = params->totype = totype;
  PDL_RETERROR(PDL_err, pdl_make_trans_mutual((pdl_trans *)trans));
  if (badflag_cache)
    CHILD->state |= PDL_BADVAL;
  return PDL_err;
}
