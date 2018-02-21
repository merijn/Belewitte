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
    }

    auxWasSet = seed = sqlite3_get_auxdata(ctxt, 0);
    if (!seed) seed = sqlite3_malloc(sizeof *seed);
    if (!seed) sqlite3_result_error(ctxt, "Failed to allocate memory!", -1);

    *seed = sqlite3_value_int64(args[0]);
    result = rand_r(seed);

    if (!auxWasSet) sqlite3_set_auxdata(ctxt, 0, seed, sqlite3_free);

    sqlite3_result_int64(ctxt, result);
}
