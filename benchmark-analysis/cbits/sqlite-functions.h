#include <stdbool.h>
#include <sqlite3.h>

void sqlite_sqrt(sqlite3_context *ctxt, int nArgs, sqlite3_value **args);

void randomFun(sqlite3_context *ctxt, int nArgs, sqlite3_value **args);
void double_vector_step(sqlite3_context *ctxt, int nArgs, sqlite3_value **args);
void double_vector_finalise(sqlite3_context *ctxt);

void init_key_value_vector_step(sqlite3_context *, int nArgs, sqlite3_value **);
void update_key_value_vector_step(sqlite3_context *, int nArgs, sqlite3_value **);
void key_value_vector_finalise(sqlite3_context *);

void check_unique_step(sqlite3_context *, int nArgs, sqlite3_value **);
void check_unqique_finalise(sqlite3_context *);

bool compare_values(int*, sqlite3_value*, sqlite3_value*);

void count_transitions_step(sqlite3_context *ctxt, int nArgs, sqlite3_value **);
void count_transitions_finalise(sqlite3_context *ctxt);
void count_transitions_value(sqlite3_context *ctxt);
void count_transitions_inverse(sqlite3_context *ctxt, int nArgs, sqlite3_value **);

void min_key_step(sqlite3_context *, int nArgs, sqlite3_value **);
void min_key_finalise(sqlite3_context *);

void random_sample_step(sqlite3_context *, int nArgs, sqlite3_value **);
void random_sample_finalise(sqlite3_context *);
void random_sample_value(sqlite3_context *);
void random_sample_inverse(sqlite3_context *, int nArgs, sqlite3_value **);
