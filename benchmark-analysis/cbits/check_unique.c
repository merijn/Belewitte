#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "sqlite-functions.h"

struct aggregate
{
    sqlite3_value *value;
    bool isUnique;
};

void check_unique_step(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
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

        data->isUnique = true;
        return;
    }

    if (!data->isUnique) return;

    int result = 0;
    if (!compare_values(&result, args[0], data->value)) {
        sqlite3_result_error(ctxt, "Encountered unknown SQLite type!", -1);
        sqlite3_value_free(data->value);
        data->value = NULL;
        return;
    }

    if (!result) data->isUnique = false;
}

void check_unique_finalise(sqlite3_context *ctxt)
{
    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (data && data->isUnique) sqlite3_result_value(ctxt, data->value);
    else sqlite3_result_null(ctxt);

    if (data) sqlite3_value_free(data->value);
}
