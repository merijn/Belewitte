#include <sqlite3.h>

void randomFun(sqlite3_context *ctxt, int nArgs, sqlite3_value **args);
void double_vector_step(sqlite3_context *ctxt, int nArgs, sqlite3_value **args);
void double_vector_finalise(sqlite3_context *ctxt);

void key_value_vector_step(sqlite3_context *, int nArgs, sqlite3_value **);
void key_value_vector_finalise(sqlite3_context *);

void check_unique_step(sqlite3_context *, int nArgs, sqlite3_value **);
void check_unqique_finalise(sqlite3_context *);
