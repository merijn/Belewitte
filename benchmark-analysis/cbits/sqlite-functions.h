#include <sqlite3.h>

void randomFun(sqlite3_context *ctxt, int nArgs, sqlite3_value **args);
void vector_step(sqlite3_context *ctxt, int nArgs, sqlite3_value **args);
void vector_finalise(sqlite3_context *ctxt);
