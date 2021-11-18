#include <stdint.h>
#include "EXTERN.h"
#include "perl.h"

int pdl_srand_called = 0;

void pdl_srand(pTHX_ uint64_t *s, unsigned int seed) {
  (void)s;
  seedDrand01((Rand_seed_t)seed);
  PL_srand_called = TRUE;
  pdl_srand_called = 1;
}
