#include "MachDeps.h"

#include "HsFFI.h"

#include <stdint.h>

#if WORD_SIZE_IN_BYTES > 32
#define HSINTTYPE int32_t
#else
#define HSINTTYPE int64_t
#endif

struct distrib {
  // 0 -> unlocked; 1 -> locked
  HSINTTYPE lock;
  int64_t count;
  double mean;
  double sum_sq_delta;
  double sum;
  double min;
  double max;
};
