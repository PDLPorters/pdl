#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#define msgptr_advance()                        \
do {                                            \
  int N      = strlen(msgptr);                  \
  msgptr    += N;                               \
  remaining -= N;                               \
} while(0)

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
