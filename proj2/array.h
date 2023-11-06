#ifndef _array_h_
#define _array_h_

#include "symbol.h"

typedef const Symbol *value_t;

typedef struct {
  int count;
  int capacity;
  value_t *values;
} ValueArray;

void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, value_t value);
void freeValueArray(ValueArray *array);
value_t popValueArray(ValueArray *array);
value_t popleftValueArray(ValueArray *array);
value_t peakValueArray(ValueArray *array);
value_t peakleftValueArray(ValueArray *array);

#endif  // _array_h_
