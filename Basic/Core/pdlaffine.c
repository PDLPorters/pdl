#include "pdl.h"
#define PDL_IN_CORE
#include "pdlcore.h"

/* also in pdlconv.c */
#define PDL_ALL_GENTYPES { PDL_SB, PDL_B, PDL_S, PDL_US, PDL_L, PDL_UL, PDL_IND, PDL_ULL, PDL_LL, PDL_F, PDL_D, PDL_LD, PDL_CF, PDL_CD, PDL_CLD, -1 }

/* delete these in CORE21 */
pdl_error pdl_readdata_affine(pdl_trans *trans) {
  return (pdl_error){PDL_EUSERERROR, "readdata called with no vtable entry", 0};
}
pdl_error pdl_writebackdata_affine(pdl_trans *trans) {
  return (pdl_error){PDL_EUSERERROR, "writebackdata called with no vtable entry", 0};
}

/* generated from:
pp_def( 'affine',
        P2Child => 1,
        TwoWay => 1,
        AffinePriv => 1,
        GlobalNew => 'affine_new',
        OtherPars => 'PDL_Indx offset; PDL_Indx dims[]; PDL_Indx incs[]',
        Comp => 'PDL_Indx nd; PDL_Indx sdims[$COMP(nd)];
                PDL_Indx sincs[$COMP(nd)];',
        MakeComp => '
                PDL_Indx i = 0;
                $COMP(nd) = dims_count;
                if ($COMP(nd) < 0)
                      $CROAK("Affine: can not have negative no of dims");
                if ($COMP(nd) != incs_count)
                      $CROAK("Affine: number of incs does not match dims");
                $DOCOMPALLOC();
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
                for (i=0;i<$PDL(CHILD)->ndims;i++) {
                        $PRIV(incs)[i] = $COMP(sincs)[i];
                        $PDL(CHILD)->dims[i] = $COMP(sdims)[i];
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
static char pdl_affine_vtable_flags[] = { 0, 0 }; /*CORE21*/
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
  0, PDL_ITRANS_ISAFFINE|PDL_ITRANS_TWOWAY|PDL_ITRANS_DO_DATAFLOW_ANY, pdl_affine_vtable_gentypes, 1, 2, pdl_affine_vtable_flags /*CORE21*/,
  pdl_affine_vtable_realdims, pdl_affine_vtable_parnames,
  pdl_affine_vtable_parflags, pdl_affine_vtable_partypes,
  pdl_affine_vtable_realdims_starts, pdl_affine_vtable_realdims_ind_ids, 0,
  0, pdl_affine_vtable_indnames,
  pdl_affine_redodims, NULL, NULL,
  pdl_affine_free,
  sizeof(pdl_params_affine),"affine_new"
};

pdl_error pdl_affine_new(pdl *PARENT,pdl *CHILD,PDL_Indx offset,PDL_Indx *dims,PDL_Indx dims_count, PDL_Indx *incs, PDL_Indx incs_count) {
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
  params->offset = offset;
  for (i=0; i<params->nd; i++) {
    params->sdims[i] = dims[i];
    params->sincs[i] = incs[i];
  }
  PDL_RETERROR(PDL_err, pdl_make_trans_mutual((pdl_trans *)trans));
  if (badflag_cache)
    CHILD->state |= PDL_BADVAL;
  return PDL_err;
}
