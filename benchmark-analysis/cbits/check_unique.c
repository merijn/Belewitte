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
    } else if (data->value == NULL) {
        data->value = sqlite3_value_dup(args[0]);
        if (data->value == NULL) {
            sqlite3_result_error(ctxt, "Failed to copy SQLite value!", -1);
            return;
        }

        data->isUnique = true;
    } else if (!data->isUnique) {
        return;
    } else if (sqlite3_value_type(args[0]) != sqlite3_value_type(data->value)) {
        data->isUnique = false;
    } else {
        switch (sqlite3_value_type(args[0])) {
          case SQLITE_INTEGER: {
              int64_t currentVal = sqlite3_value_int64(data->value);
              int64_t newVal = sqlite3_value_int64(args[0]);
              data->isUnique = currentVal == newVal;
              break;
          }
          case SQLITE_FLOAT: {
              double currentVal = sqlite3_value_double(data->value);
              double newVal = sqlite3_value_double(args[0]);
              data->isUnique = currentVal == newVal;
              break;
          }
          case SQLITE_BLOB: {
              size_t currentLength = sqlite3_value_bytes(data->value);
              size_t newLength = sqlite3_value_bytes(args[0]);

              if (currentLength != newLength) {
                  data->isUnique = false;
                  break;
              }

              const void *currentVal = sqlite3_value_blob(data->value);
              const void *newVal = sqlite3_value_blob(args[0]);
              data->isUnique = !memcmp(currentVal, newVal, currentLength);
              break;
          }

          case SQLITE_TEXT: {
              size_t currentLength = sqlite3_value_bytes(data->value);
              size_t newLength = sqlite3_value_bytes(args[0]);
              if (currentLength != newLength) {
                  data->isUnique = false;
                  break;
              }

              const void *currentVal = sqlite3_value_text(data->value);
              const void *newVal = sqlite3_value_text(args[0]);
              data->isUnique = !memcmp(currentVal, newVal, currentLength);
          }
          case SQLITE_NULL:
            break;
          default:
            sqlite3_result_error(ctxt, "Encountered unexpected data type", -1);
            return;
        }
    }
}

void check_unique_finalise(sqlite3_context *ctxt)
{
    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (!data) {
        sqlite3_result_null(ctxt);
        return;
    }

    if (data->isUnique) {
        sqlite3_result_value(ctxt, data->value);
    } else {
        sqlite3_result_null(ctxt);
    }

    sqlite3_value_free(data->value);
}
