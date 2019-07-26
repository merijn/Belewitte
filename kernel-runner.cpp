#include <algorithm>
#include <array>
#include <cstring>
#include <stdexcept>
#include <getopt.h>
#include <wordexp.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <dlfcn.h>

#include <boost/filesystem.hpp>
#include <boost/regex.hpp>

#include "AlgorithmConfig.hpp"
#include "Backend.hpp"
#include "CUDA.hpp"
#include "OpenCL.hpp"
#include "options/Options.hpp"
#include "Timer.hpp"
#include "utils/Util.hpp"

#define TO_STRING(a) xstr(a)
#define xstr(a) #a
#ifndef VERSION
#define VERSION
#endif

using namespace std;
using namespace boost::filesystem;

enum class framework { cuda, opencl };

static const char *exeName = "kernel-runner";
static Options options('h', "help", cout, [](ostream& out)
{
    out << "Usage:" << endl;
    out << exeName << " list platforms [-v | --verbose]" << endl;

    out << exeName << " list devices [-p NUM | --platform NUM] "
        << "[-v | --verbose]" << endl;

    out << exeName << " list algorithms [-v | --verbose] "
        << "[-L PATH | --lib PATH]" << endl;

    out << exeName << " list implementations [-a NAME | --algorithm NAME] "
        << "[-v | --verbose] [-L PATH | --lib PATH]" << endl;

    out << exeName << " query platform [-p NUM | --platform NUM] "
        << "[-v | --verbose]" << endl;

    out << exeName << " query device [-p NUM | --platform NUM] "
        << "[-d NUM | --device NUM] [-v | --verbose]" << endl;

    out << exeName << " -S" << endl;
    out << exeName << " -a ALGORITHM -k KERNEL [OPTIONS] <graph file(s)>"
        << endl;
    out << endl << "Options:" << endl;
});

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    options.usage(cout, "    ");
    exit(exitCode);
}

static map<string, map<string, AlgorithmConfig*>>
loadAlgorithms
(const char *sym, vector<string> &paths, bool warn, bool debug)
{
    map<string, string> libs;
    map<string, map<string, AlgorithmConfig*>> result;

    if (is_directory("./.build/kernels/")) {
        paths.insert(paths.begin(), "./.build/kernels/");
    }

    boost::smatch match;
    const char *regex = "lib(.*)kernel" TO_STRING(VERSION) "\\.so";
    const char *debug_regex = "lib(.*)kerneldebug" TO_STRING(VERSION) "\\.so";
    const boost::regex lib_regex(debug ? debug_regex : regex);

    for (auto p_str : paths) {
        if (!is_directory(p_str)) continue;

        for (auto& entry : directory_iterator(p_str)) {
            if (!is_regular_file(entry)) continue;

            auto p = entry.path();
            const string fileNameString = p.filename().string();
            if (boost::regex_match(fileNameString, match, lib_regex)) {
                libs.emplace(match[1], p.string());
            }
        }
    }

    for (auto lib : libs) {
        void *hnd = dlopen(lib.second.c_str(), RTLD_NOW);
        if (!hnd) {
            if (warn) {
                cerr << "dlopen() failed: " << lib.second << endl
                     << dlerror() << endl;
            }
            continue;
        }

        auto dispatch = reinterpret_cast<kernel_register_t*>(dlsym(hnd, sym));

        if (dispatch != nullptr) dispatch(result[lib.first]);
        else if (warn) {
            cerr << "dlsym() failed: " << sym << " (" << lib.second << ") "
                 << endl << dlerror() << endl;
        }
    }

    return result;
}

static AlgorithmConfig&
getConfig
( map<string, map<string, AlgorithmConfig*>>& algorithms
, string algorithmName
, string kernelName)
{
    std::string errorMsg;
    std::ostringstream names;

    try {
        auto algorithm = algorithms.at(algorithmName);
        try {
            return *algorithm.at(kernelName);
        } catch (const out_of_range &) {
            for (auto kernel : algorithm) {
                names << "    " << kernel.first << endl;
            }

            if (kernelName.empty()) {
                errorMsg = "Kernel name not specified!";
            } else {
                errorMsg = "No kernel named \"" + kernelName +
                           "\" for algorithm \"" + algorithmName + "\"!";
            }

            reportError(errorMsg, "\n\nSupported kernels:\n", names.str());
        }
    } catch (const out_of_range &) {
        for (auto algorithm : algorithms) {
            names << "    " << algorithm.first << endl;
        }

        if (algorithmName.empty()) {
            errorMsg = "Algorithm name not specified!";
        } else {
            errorMsg = "No algorithm named \"" + algorithmName + "\"!";
        }

        reportError(errorMsg, "\n\nSupported algorithms:\n", names.str());
    }
}

static map<string, map<string, AlgorithmConfig*>> algorithms;
static bool debug = false;
static bool verbose = false;
static bool warnings = false;
static bool printStdOut = false;
static bool fromStdin = false;
static framework fw = framework::cuda;
static int device = 0;
static size_t platform = 0;
static string outputDir(".");
static string algorithmName = "";
static string kernelName = "";
static vector<string> paths = { "." };

static void print_query_results(Backend& backend, const vector<string>& args)
{
    if (args.size() < 1) return;
    if (args[0] != "list" && args[0] != "query") return;

    if (args.size() < 2) usage();

    if (args[0] == "list" && args[1] ==  "platforms") {
        backend.listPlatforms(verbose);
        exit(static_cast<int>(backend.platformCount()));
    } else if (args[0] == "list" && args[1] == "devices") {
        backend.listDevices(platform, verbose);
        exit(backend.deviceCount(platform));
    } else if (args[0] == "list" && args[1] == "algorithms") {
        for (auto algorithm : algorithms) {
            cout << algorithm.first << endl;
            if (verbose) {
                for (auto &kernel : algorithm.second) {
                    cout << "    " << kernel.first << endl;
                    kernel.second->help(cout, "\t");
                }
            }
        }
        exit(EXIT_SUCCESS);
    } else if (args[0] == "list" && args[1] == "implementations") {
        for (auto &kernel : algorithms[algorithmName]) {
            cout << kernel.first << endl;
            if (verbose) kernel.second->help(cout, "    ");
        }
        exit(EXIT_SUCCESS);
    } else if (args[0] == "query" && args[1] == "platform") {
        backend.queryPlatform(platform, verbose);
        exit(backend.deviceCount(platform));
    } else if (args[0] == "query" && args[1] == "device") {
        backend.queryDevice(platform, device, verbose);
        exit(EXIT_SUCCESS);
    } else {
        usage();
    }
}

static void
runJob
(AlgorithmConfig& kernel, vector<string> args, const string& tag = string())
{
    auto graphs = kernel.setup(args);

    if (!tag.empty() && graphs.size() > 1) {
        reportError("Tagged output with more than 1 graph!");
    }

    for (auto graph : graphs) {
        auto label = tag.empty() ? path(graph).stem().string() : tag;
        auto basePath = outputDir / path(label);
        auto timeFile = basePath.replace_extension("timings");
        auto outputFile = basePath.replace_extension("output");

        {
            Epoch epoch(printStdOut ? "/dev/stdout" : timeFile.string(), verbose);
            kernel(graph, outputFile.string());
        }

        cout << label << endl;
    }
}

int main(int argc, char * const *argv)
{
    std::reference_wrapper<Backend> activeBackend(CUDA);

    options.add('d', "device", "NUM", device, "Device to use.")
           .add('f', "framework", fw, framework::opencl, "Use OpenCL.")
           .add('L', "lib", "PATH", paths, "\".\"",
                "Search path for algorithm libraries.")
           .add('o', "output-dir", "DIR", outputDir,
                "Location to use for writing algorithm output.")
           .add('O', "output", printStdOut, true,
                "Print timings to stdout, inhibits timings file creation.")
           .add('p', "platform", "NUM", platform, "Platform to use.")
           .add('g', "debug", debug, true, "Use debug kernels.")
           .add('v', "verbose", verbose, true, "Verbose output.")
           .add('W', "warn", warnings, true, "Verbose/debug warnings.")
           .add('S', "stdin", fromStdin, true, "Read work from stdin.");

    Options kernelParser(options);

    kernelParser.add('a', "algorithm", "NAME", algorithmName,
                     "Algorithm to use.")
                .add('k', "kernel", "NAME", kernelName,
                     "Which algorithm implementation to use.");

    set_new_handler(out_of_memory);
    locale::global(locale(""));
    cout.imbue(locale());

    struct rlimit limits;
    limits.rlim_cur = RLIM_INFINITY;
    limits.rlim_max = RLIM_INFINITY;
    setrlimit(RLIMIT_CORE, &limits);

    auto optionResult = options.parseArgsNoUsage(argc, argv);

    switch (fw) {
      case framework::opencl: {
        activeBackend = OpenCL;
        algorithms = loadAlgorithms("openclDispatch", paths, warnings, debug);
        {
            //array<const char*,1> files {{&_binary_kernel_cl_start}};
            //array<size_t,1> sizes {{(size_t) &_binary_kernel_cl_size}};
            //cl_kernel initKernel = opencl.createKernel("init", files, sizes);
            //cl_kernel runKernel = opencl.createKernel("BFS", files, sizes);
            //benchmark<OpenCL>(opencl, initKernel, runKernel, graph, root, run_count, outputFile);
        }
        break;
      }
      case framework::cuda: {
        algorithms = loadAlgorithms("cudaDispatch", paths, warnings, debug);
        break;
      }
    }

    Backend& backend = activeBackend;

    print_query_results(backend, optionResult.remainingArgs);

    if (optionResult.usageRequested) {
        options.usage(cout, "    ");
        if (!algorithmName.empty() && !kernelName.empty()) {
            auto& kernel = getConfig(algorithms, algorithmName, kernelName);
            kernel.help(cout, "    ");
        }

        exit(EXIT_SUCCESS);
    }

    if (!backend.initialised) {
        cerr << "Backend not initialised: No devices?" << endl;
        exit(EXIT_FAILURE);
    }
    backend.setDevice(platform, device);

    if (fromStdin) {
        string line;
        wordexp_t newArgv;
        vector<string> remainingArgs;

        while (getline(cin, line)) {
            if (wordexp(line.c_str(), &newArgv, WRDE_NOCMD | WRDE_UNDEF)) {
                reportError("Failed to expand commandline with wordexp(3)!");
            }

            remainingArgs = kernelParser.parseArgs
                    (static_cast<int>(newArgv.we_wordc), newArgv.we_wordv);

            auto& kernel = getConfig(algorithms, algorithmName, kernelName);
            runJob(kernel, remainingArgs, string(newArgv.we_wordv[0]));

            kernelParser.reset();
            wordfree(&newArgv);
        }
    } else {
        auto& kernel = getConfig(algorithms, algorithmName, kernelName);
        runJob(kernel, optionResult.remainingArgs);
    }

    for (auto& [key, kvmap] : algorithms) {
        for (auto& [key, val] : kvmap) {
            delete val;
        }
    }
    return 0;
}
