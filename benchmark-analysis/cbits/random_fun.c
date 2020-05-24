#define _POSIX_C_SOURCE 1
#include <stdlib.h>

#include "sqlite-functions.h"

void randomFun(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    (void) nArgs;
    void *auxWasSet;
    unsigned int *seed;
    sqlite3_int64 result;

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
