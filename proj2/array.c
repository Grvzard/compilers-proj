#include "array.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

static inline int _grow_capacity(int old_capacity) {
    return (old_capacity < 8 ? 8 : old_capacity * 2);
}

static inline value_t *_grow_array(value_t *old_array, int old_size,
                                   int new_size) {
    if (new_size == 0) {
        free(old_array);
        return NULL;
    }

    value_t *new_array =
        (value_t *)realloc(old_array, sizeof(value_t) * new_size);
    if (new_array == NULL) {
        exit(1);
    }
    return new_array;
}

void initValueArray(ValueArray *array) {
    array->count = 0;
    array->capacity = 0;
    array->values = NULL;
}

void writeValueArray(ValueArray *array, value_t value) {
    if (array->capacity < array->count + 1) {
        int old_cap = array->capacity;
        array->capacity = _grow_capacity(old_cap);
        array->values = _grow_array(array->values, old_cap, array->capacity);
    }
    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray *array) {
    if (array->values != NULL) {
        free(array->values);
    }
    initValueArray(array);
}

value_t popValueArray(ValueArray *array) {
    assert(array->count != 0);
    array->count--;
    return array->values[array->count];
}

value_t popleftValueArray(ValueArray *array) {
    assert(array->count != 0);
    for (int i = 0; i < array->count - 1; i++) {
        array->values[i] = array->values[i + 1];
    }
    array->count--;
    return array->values[array->count + 1];
}

value_t peekValueArray(ValueArray *array) {
    assert(array->count != 0);
    return array->values[array->count - 1];
}

value_t peekleftValueArray(ValueArray *array) {
    assert(array->count != 0);
    return array->values[0];
}
