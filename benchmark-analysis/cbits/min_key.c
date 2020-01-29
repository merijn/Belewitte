#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "sqlite-functions.h"

static inline int64_t min(int64_t a, int64_t b)
{ return a < b ? a : b; }

bool compare_values(int *result, sqlite3_value *val1, sqlite3_value *val2)
{
    *result = sqlite3_value_type(val1) - sqlite3_value_type(val2);
    if (*result != 0) return result;

    switch (sqlite3_value_type(val1)) {
        case SQLITE_INTEGER: {
            int64_t leftVal = sqlite3_value_int64(val1);
            int64_t rightVal = sqlite3_value_int64(val2);

            *result = leftVal - rightVal;
            return true;
        }

        case SQLITE_FLOAT: {
            double leftVal = sqlite3_value_double(val1);
            double rightVal = sqlite3_value_double(val2);

            if (leftVal < rightVal) *result = -1;
            else if (leftVal > rightVal) *result = 1;
            else *result = 0;
            return true;
        }

        case SQLITE_BLOB: {
            size_t leftLength = sqlite3_value_bytes(val1);
            size_t rightLength = sqlite3_value_bytes(val2);

            const void *leftVal = sqlite3_value_blob(val1);
            const void *rightVal = sqlite3_value_blob(val2);
            *result = memcmp(leftVal, rightVal, min(leftLength, rightLength));
            if (*result == 0) {
                if (leftLength < rightLength) *result = -1;
                else if (leftLength > rightLength) *result = 1;
            }
            return true;
        }

        case SQLITE_TEXT: {
            size_t leftLength = sqlite3_value_bytes(val1);
            size_t rightLength = sqlite3_value_bytes(val2);

            const void *leftVal = sqlite3_value_text(val1);
            const void *rightVal = sqlite3_value_text(val2);
            *result = memcmp(leftVal, rightVal, min(leftLength, rightLength));
            if (*result == 0) {
                if (leftLength < rightLength) *result = -1;
                else if (leftLength > rightLength) *result = 1;
            }
            return true;
        }

        case SQLITE_NULL: {
            *result = 0;
            return true;
        }

        default:
          return false;
    }
}

struct aggregate
{
    int64_t minKey;
    size_t size;
    sqlite3_value **values;
};

static void cleanup_aggregate(sqlite3_context *ctxt)
{
    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (!data) return;

    if (!data->values) {
        for (size_t i = 0; i < data->size; i++) {
            sqlite3_value_free(data->values[i]);
            data->values[i] = NULL;
        }
    }

    sqlite3_free(data->values);
    data->values = NULL;
}

void min_key_step(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    if (nArgs < 1) {
        sqlite3_result_error(ctxt, "Insufficient arguments!", -1);
        cleanup_aggregate(ctxt);
        return;
    }

    if (sqlite3_value_type(args[0]) != SQLITE_INTEGER) {
        sqlite3_result_error(ctxt, "First argument is not an integer key!", -1);
        cleanup_aggregate(ctxt);
        return;
    }

    struct aggregate *data = sqlite3_aggregate_context(ctxt, sizeof *data);

    if (!data) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        cleanup_aggregate(ctxt);
        return;
    }

    if (!data->values) {
        data->minKey = sqlite3_value_int64(args[0]);
        data->size = nArgs - 1;

        data->values = sqlite3_malloc(data->size * sizeof *data->values);
        if (!data->values) {
            sqlite3_result_error(ctxt, "Allocation failed!", -1);
            cleanup_aggregate(ctxt);
            return;
        }

        for (size_t i = 0; i < data->size; i++) {
            data->values[i] = NULL;
        }

        for (size_t i = 0; i < data->size; i++) {
            data->values[i] = sqlite3_value_dup(args[i+1]);
            if (!data->values[i]) {
                sqlite3_result_error(ctxt, "Failed to copy SQLite value!", -1);
                cleanup_aggregate(ctxt);
                return;
            }
        }
    }

    int result = 0;
    for (size_t i = 0; i < data->size && !result; i++) {
        if (!compare_values(&result, data->values[i], args[i+1])) {
            sqlite3_result_error(ctxt, "Error trying to compare SQLite values!", -1);
            cleanup_aggregate(ctxt);
            return;
        }
    }

    if (result == 0) result = data->minKey - sqlite3_value_int64(args[0]);

    if (result > 0) {
        data->minKey = sqlite3_value_int64(args[0]);
        for (size_t i = 0; i < data->size; i++) {
            sqlite3_value_free(data->values[i]);
            data->values[i] = sqlite3_value_dup(args[i+1]);
            if (!data->values[i]) {
                sqlite3_result_error(ctxt, "Failed to copy SQLite value!", -1);
                cleanup_aggregate(ctxt);
                return;
            }
        }
    }
}

void min_key_finalise(sqlite3_context *ctxt)
{
    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (!data) sqlite3_result_null(ctxt);
    else sqlite3_result_int64(ctxt, data->minKey);

    cleanup_aggregate(ctxt);
}
