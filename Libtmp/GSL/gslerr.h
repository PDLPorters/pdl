#include <gsl/gsl_errno.h>

#define GSLERR(x,y) \
  { \
    int status = x y; \
    if (status) \
      return PDL->make_error(PDL_EUSERERROR, "Error in %s: %s", #x, gsl_strerror(status)); \
  }
