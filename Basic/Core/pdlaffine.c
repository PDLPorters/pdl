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
          if ($CHILD_P(state) & $PARENT_P(state) & PDL_ALLOCATED) {
            PDL_Indx i, poffs=$PRIV(offs), nd;
            for(i=0; i<$CHILD_P(nvals); i++) {
              $EQUIVCPOFFS(i,poffs);
              for(nd=0; nd<$CHILD_P(ndims); nd++) {
                poffs += $PRIV(incs[nd]);
                if( (nd<$CHILD_P(ndims)-1 &&
                     (i+1)%$CHILD_P(dimincs[nd+1])) ||
                   nd == $CHILD_P(ndims)-1)
                        break;
                poffs -= $PRIV(incs[nd]) *
                        $CHILD_P(dims[nd]);
              }
            }
          }',
       Doc => undef,    # 'internal',
);
*/

#define COPYDATA(ctype, from_id, to_id) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype *, (ctype *), ctype, (trans->vtable->per_pdl_flags[to_id]), to_pdl, (trans->pdls[to_id])) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype *, (ctype *), ctype, (trans->vtable->per_pdl_flags[from_id]), from_pdl, (trans->pdls[from_id])) \
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
  PDL_GENERICSWITCH(trans->__datatype, X)
#undef X
}

void pdl_writebackdata_affine(pdl_trans *trans) {
  if (!(trans->pdls[0]->state & trans->pdls[1]->state & PDL_ALLOCATED)) return;
#define X(sym, ctype, ppsym, shortctype, defbval) COPYDATA(ctype, 1, 0)
  PDL_GENERICSWITCH(trans->__datatype, X)
#undef X
}
