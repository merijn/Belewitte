#include "Util.hpp"

extern "C" const char *__asan_default_options();
extern "C" const char *__asan_default_options()
{ return "color=always:protect_shadow_gap=0:replace_intrin=0"
         ":suppress_equal_pcs=1"; }

extern "C" const char *__lsan_default_options();
extern "C" const char *__lsan_default_options()
{ return "suppressions=suppressions/leak.supp:print_suppressions=0"; }

extern "C" const char *__tsan_default_options();
extern "C" const char *__tsan_default_options()
{ return "suppressions=suppressions/thread.supp"; }

void
out_of_memory(void)
{ dump_stack_trace(EXIT_FAILURE); }

void __attribute__((noreturn))
dump_stack_trace(int exit_code)
{
    Dl_info info;
    void *callstack[128];
    const int max_frames = sizeof(callstack) / sizeof(callstack[0]);

    int frame_count = backtrace(callstack, max_frames);
    char **symbols = backtrace_symbols(callstack, frame_count);

    for (int i = 1; i < frame_count; i++) {
        if (dladdr(callstack[i], &info) && info.dli_sname) {
            char *demangled = nullptr;
            int status = -1;

            if (info.dli_sname[0] == '_')
                demangled = abi::__cxa_demangle(info.dli_sname, nullptr,
                                                nullptr, &status);

            fprintf(stderr,"%-3d %s\n", i,
                status == 0 ? demangled :
                    info.dli_sname == nullptr ? symbols[i] : info.dli_sname);

            free(demangled);
        } else {
            fprintf(stderr, "%-3d %s\n", i, symbols[i]);
        }
    }

    free(symbols);

    if (frame_count == max_frames) fprintf(stderr, "[truncated]\n");
    exit(exit_code | (1 << 7));
}

void printVals(void) {}
