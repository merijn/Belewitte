#include "pcg_basic.h"
#include "sqlite-functions.h"

struct aggregate
{
    int64_t selectCount;
    int64_t remainingCount;
    pcg32_random_t pcg_state;
};

static bool
sample_stream(struct aggregate *data)
{
    int64_t sample = pcg32_boundedrand_r(&data->pcg_state, data->remainingCount);
    bool keep = sample < data->selectCount;

    if (keep) data->selectCount--;

    data->remainingCount--;

    return keep;
}

static void
check_sample_state(sqlite3_context *ctxt, struct aggregate *data)
{
    if (data->remainingCount < data->selectCount) {
        sqlite3_result_error(ctxt, "Insufficient elements for selection!", -1);
        return;
    } else if (data->selectCount < 0) {
        sqlite3_result_error(ctxt,
                "Can't select negative number of elements!", -1);
        return;
    } else if (data->remainingCount < 0) {
        sqlite3_result_error(ctxt,
                "Can't have negative number of remaining elements!", -1);
        return;
    }
}

void
random_sample_step(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    (void) nArgs;
    struct aggregate *data = sqlite3_aggregate_context(ctxt, sizeof *data);

    if (!data) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    }

    if (!data->pcg_state.inc) {
        int64_t sequence = sqlite3_value_int64(args[0]);
        int64_t seed = sqlite3_value_int64(args[1]);

        data->remainingCount = sqlite3_value_int64(args[2]);
        if (sqlite3_value_type(args[3]) == SQLITE_FLOAT) {
            double percentage = sqlite3_value_double(args[3]);
            if (percentage > 1.0) {
                sqlite3_result_error(ctxt,
                        "Selection percentage can't be more than 100%!", -1);
                return;
            } else if (percentage < 0.0) {
                sqlite3_result_error(ctxt,
                        "Selection percentage can't be less than 0%!", -1);
                return;
            }

            data->selectCount = percentage * data->remainingCount;
        } else {
            data->selectCount = sqlite3_value_int64(args[3]);
        }

        pcg32_srandom_r(&data->pcg_state, seed, sequence);
    }

    check_sample_state(ctxt, data);
}

void
random_sample_finalise(sqlite3_context *ctxt)
{
    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (!data) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    }

    check_sample_state(ctxt, data);
    if (data->selectCount > 0) {
        sqlite3_result_error(ctxt, "Sampled insufficient elements!", -1);
        return;
    } else if (data->remainingCount > 0) {
        sqlite3_result_error(ctxt, "Didn't inspect all elements!", -1);
        return;
    } else {
        sqlite3_result_null(ctxt);
    }
}

void
random_sample_value(sqlite3_context *ctxt)
{
    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (!data) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    }

    sqlite3_result_int64(ctxt, sample_stream(data));
    check_sample_state(ctxt, data);
}

void
random_sample_inverse(sqlite3_context *ctxt, int nArgs, sqlite3_value **args)
{
    (void) ctxt;
    (void) nArgs;
    (void) args;

    struct aggregate *data = sqlite3_aggregate_context(ctxt, 0);

    if (!data) {
        sqlite3_result_error(ctxt, "Aggregate allocation failed!", -1);
        return;
    }

    check_sample_state(ctxt, data);
}
