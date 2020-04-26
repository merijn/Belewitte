#include <stddef.h>
#include <stdint.h>

#include "sqlite-functions.h"

struct aggregate
{
    sqlite3_value *value;
    int64_t count;
};

void
count_transitions_step(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    (void) nArgs;
    struct aggregate *data = sqlite3_aggregate_context(ctxt, sizeof *data);

    if (!data) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    }

    if (!data->value) {
        data->value = sqlite3_value_dup(args[0]);
        if (data->value == NULL) {
            sqlite3_result_error(ctxt, "Failed to copy SQLite value!", -1);
            return;
        }

        data->count = 1;
        return;
    }

    int result = 0;
    if (!compare_values(&result, args[0], data->value)) {
        sqlite3_result_error(ctxt, "Encountered unknown SQLite type!", -1);
        sqlite3_value_free(data->value);
        data->value = NULL;
        return;
    }

    if (result) {
        sqlite3_value_free(data->value);

        data->value = sqlite3_value_dup(args[0]);
        if (data->value == NULL) {
            sqlite3_result_error(ctxt, "Failed to copy SQLite value!", -1);
            return;
        }

        data->count++;
    }
}

void
count_transitions_finalise(sqlite3_context *ctxt)
{
    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (data) sqlite3_result_int64(ctxt, data->count);
    else sqlite3_result_null(ctxt);

    if (data) sqlite3_value_free(data->value);
}

void
count_transitions_value(sqlite3_context *ctxt)
{
    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (!data) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    }

    sqlite3_result_int64(ctxt, data->count);
}

void
count_transitions_inverse(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    (void) ctxt;
    (void) nArgs;
    (void) args;

    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (!data) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    }
}
