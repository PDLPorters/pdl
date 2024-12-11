#include "EXTERN.h"
#include "perl.h"
#include "pdl.h"

#define SWALLOWLINE(fp) while ((s = PerlIO_getc(fp)) != '\n' && s != EOF)
#define PBM 1
#define PGM 2
#define PPM 3

int getint(PerlIO *fp, PDL_Long *ip);
