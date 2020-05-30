#if (__STDC_VERSION__ < 201112L)
#error "C11 is required!"
#endif

#define _POSIX_C_SOURCE 1
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "sqlite-functions.h"

typedef struct
{
    int64_t key;
    double val;
} key_value;

struct aggregate
{
    int64_t size;
    key_value *vector;
};

static_assert
(sizeof (int64_t) == 8, "int64_t is not 8 bytes!");

static_assert
(sizeof (int64_t) == sizeof (double), "double not same size as int64_t!");

static_assert
(sizeof (key_value) == 2 * sizeof (double), "struct isn't tightly packed!");

void
init_key_value_vector_step
(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    (void) nArgs;
    struct aggregate *data = sqlite3_aggregate_context(ctxt, sizeof *data);

    if (!data) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    }

    if (!data->vector) {
        data->size = sqlite3_value_int(args[2]);
        if (data->size < 0) {
            sqlite3_result_error(ctxt, "Vector size can't be negative!", -1);
            return;
        }

        data->vector = sqlite3_malloc(data->size * sizeof *data->vector);
        if (!data->vector) {
            sqlite3_result_error(ctxt, "Vector allocation failed!", -1);
            return;
        }

        double defaultVal = *((double*) sqlite3_user_data(ctxt));
        for (int64_t i = 0; i < data->size; i++) {
            data->vector[i].key = 0;
            data->vector[i].val = defaultVal;
        }
    }

    int64_t idx = sqlite3_value_int(args[1]) - 1;
    if (idx > data->size) {
        sqlite3_result_error(ctxt, "Out of bounds initialisation!", -1);
        sqlite3_free(data->vector);
        data->vector = NULL;
    }

    data->vector[idx].key = sqlite3_value_int64(args[0]);
}

void
update_key_value_vector_step
(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    (void) nArgs;
    struct aggregate *data = sqlite3_aggregate_context(ctxt, sizeof *data);

    if (!data) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    }

    if (!data->vector) {
        int vectorSize = sqlite3_value_bytes(args[0]);
        data->size = vectorSize / sizeof *data->vector;
        if (data->size < 0) {
            sqlite3_result_error(ctxt, "Vector size can't be negative!", -1);
            return;
        }

        data->vector = sqlite3_malloc(data->size * sizeof *data->vector);
        if (!data->vector) {
            sqlite3_result_error(ctxt, "Vector allocation failed!", -1);
            return;
        }

        const void *original = sqlite3_value_blob(args[0]);
        memcpy(data->vector, original, data->size * sizeof *data->vector);
    }

    // No-op when attempting to update NULL values
    if (sqlite3_value_type(args[1]) == SQLITE_NULL) return;
    if (sqlite3_value_type(args[2]) == SQLITE_NULL) return;
    if (sqlite3_value_type(args[3]) == SQLITE_NULL) return;

    int64_t idx = sqlite3_value_int(args[1]) - 1;
    if (idx > data->size || idx < 0) {
        sqlite3_result_error(ctxt, "Out of bounds initialisation!", -1);
        sqlite3_free(data->vector);
        data->vector = NULL;
        return;
    }

    if (data->vector[idx].key != sqlite3_value_int64(args[2])) {
        sqlite3_result_error(ctxt, "Key mismatch!", -1);
        sqlite3_free(data->vector);
        data->vector = NULL;
        return;
    }

    data->vector[idx].val = sqlite3_value_double(args[3]);
}

void
key_value_vector_finalise(sqlite3_context *ctxt)
{
    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (!data) {
        sqlite3_result_null(ctxt);
        return;
    }

    if (!data->vector) {
        sqlite3_result_error(ctxt, "Vector construction failed!", -1);
        return;
    }

    bool uninitialised = false;
    for (int i = 0; i < data->size && !uninitialised; i++) {
        if (data->vector[i].key == 0) uninitialised = true;
    }

    if (uninitialised) {
        sqlite3_free(data->vector);
        sqlite3_result_error(ctxt, "Returning uninitialised key!", -1);
        return;
    }

    sqlite3_result_blob(ctxt, data->vector,
            data->size * sizeof *data->vector, &sqlite3_free);
}
