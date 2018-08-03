#define _POSIX_C_SOURCE 1
#include <stdlib.h>

#include "sqlite-functions.h"

void randomFun(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    void *auxWasSet;
    unsigned int *seed;
    sqlite3_int64 result;

    if (nArgs != 1) {
        sqlite3_result_error(ctxt, "Wrong number of arguments!", -1);
        return;
    }

    auxWasSet = seed = sqlite3_get_auxdata(ctxt, 0);
    if (!seed) seed = sqlite3_malloc(sizeof *seed);
    if (!seed) {
        sqlite3_result_error(ctxt, "Seed allocation failed!", -1);
        return;
    }

    *seed = sqlite3_value_int64(args[0]);
    result = rand_r(seed);

    if (!auxWasSet) sqlite3_set_auxdata(ctxt, 0, seed, sqlite3_free);

    sqlite3_result_int64(ctxt, result);
}

typedef struct vector
{
    double *data;
    size_t size;
    size_t idx;
} vector_t;

void vector_step(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    if (nArgs != 3) {
        sqlite3_result_error(ctxt, "Incorrect argument count!", -1);
        return;
    }

    vector_t *vector = sqlite3_aggregate_context(ctxt, sizeof *vector);
    if (!vector) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    }

    if (!vector->data) {
        vector->size = sqlite3_value_int(args[2]);
        vector->idx = 0;
        vector->data = sqlite3_malloc(vector->size * sizeof *vector->data);

        if (!vector->data) {
            sqlite3_result_error(ctxt, "Vector allocation failed!", -1);
            return;
        }

        for (size_t i = 0; i < vector->size; i++) {
            vector->data[i] = 0.0;
        }
    }

    size_t idx = sqlite3_value_int(args[1]) - 1;
    vector->data[idx] = sqlite3_value_double(args[0]);
}

void vector_finalise(sqlite3_context *ctxt)
{
    vector_t *vector = sqlite3_aggregate_context(ctxt, 0);
    if (!vector) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    } else if (!vector->data) {
        sqlite3_result_error(ctxt, "Vector construction failed!", -1);
        return;
    }

    sqlite3_result_blob(ctxt, vector->data,
            vector->size * sizeof *vector->data, &sqlite3_free);
}
