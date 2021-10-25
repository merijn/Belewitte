#define _POSIX_C_SOURCE 1
#include <math.h>
#include <stdlib.h>

#include "sqlite-functions.h"

void sqlite_sqrt(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    if (nArgs != 1) {
        sqlite3_result_error(ctxt, "Incorrect number of arguments for sqrt()", -1);
        return;
    }

    int sqltype = sqlite3_value_type(args[0]);

    if (sqltype != SQLITE_FLOAT) {
        sqlite3_result_double(ctxt, sqrt(sqlite3_value_double(args[0])));
        return;
    } else if (sqltype != SQLITE_INTEGER) {
        sqlite3_result_double(ctxt, sqrt(sqlite3_value_int64(args[0])));
        return;
    }

    sqlite3_result_error(ctxt, "sqrt() requires float or int argument", -1);
}
