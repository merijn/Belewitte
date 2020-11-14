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

#ifdef __linux__
#include <sched.h>
#include <pthread.h>
#endif

#include "Algorithm.hpp"
#include "Backend.hpp"
#include "CUDA.hpp"
#include "ImplementationTemplate.hpp"
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

ImplementationTemplateBase<false>::~ImplementationTemplateBase()
{}

ImplementationTemplateBase<true>::~ImplementationTemplateBase()
{}

enum class framework { cuda, opencl };

static map<string, Algorithm> algorithms;
static bool debug = false;
static bool verbose = false;
static bool warnings = false;
static bool noOutput = false;
static bool printStdOut = false;
static bool fromStdin = false;
static framework fw = framework::cuda;
static int device = 0;
static size_t platform = 0;
static string outputDir(".");
static string algorithmName = "";
static string kernelName = "";
static vector<string> libPaths = { "." };

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

    out << exeName << " query algorithm-version [-a NAME | --algorithm NAME]"
        << endl;

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

static map<string, Algorithm>
loadAlgorithms
(const char *sym, vector<string> &paths)
{
    map<string, string> libs;
    map<string, Algorithm> result;

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
            if (warnings) {
                cerr << "dlopen() failed: " << lib.second << endl
                     << dlerror() << endl;
            }
            continue;
        }

        auto getAlgo = reinterpret_cast<register_algorithm_t*>(dlsym(hnd, sym));

        if (getAlgo != nullptr) getAlgo(result[lib.first]);
        else if (warnings) {
            cerr << "dlsym() failed: " << sym << " (" << lib.second << ") "
                 << endl << dlerror() << endl;
        }
    }

    return result;
}

static Algorithm&
getAlgorithm(string algoName)
{
    std::string errorMsg;
    std::ostringstream names;

    try {
        return algorithms.at(algoName);
    } catch (const out_of_range &) {
        for (auto& algorithm : algorithms) {
            names << "    " << algorithm.first << endl;
        }

        if (algoName.empty()) {
            errorMsg = "Algorithm name not specified!";
        } else {
            errorMsg = "No algorithm named \"" + algoName + "\"!";
        }

        reportError(errorMsg, "\n\nSupported algorithms:\n", names.str());
    }
}

static void
handle_subcommands(Backend& backend, const vector<string>& args)
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
        for (auto& [algoName, algorithm] : algorithms) {
            cout << algoName << endl;

            if (verbose) {
                for (auto & [kernelName, kernel] : algorithm) {
                    cout << "    " << kernelName << endl;
                    kernel->help(cout, "\t");
                }
            }
        }
        exit(EXIT_SUCCESS);
    } else if (args[0] == "list" && args[1] == "implementations") {
        auto& algorithm = getAlgorithm(algorithmName);
        for (auto & [kernelName, kernel] : algorithm) {
            cout << kernelName << endl;
            if (verbose) kernel->help(cout, "    ");
        }
        exit(EXIT_SUCCESS);
    } else if (args[0] == "query" && args[1] == "platform") {
        backend.queryPlatform(platform, verbose);
        exit(backend.deviceCount(platform));
    } else if (args[0] == "query" && args[1] == "device") {
        backend.queryDevice(platform, device, verbose);
        exit(EXIT_SUCCESS);
    } else if (args[0] == "query" && args[1] == "algorithm-version") {
        auto& algorithm = getAlgorithm(algorithmName);
        cout << algorithm.commit() << endl;
        exit(EXIT_SUCCESS);
    } else {
        usage();
    }
}

static void
runJob
( std::string algoName
, std::string kernName
, vector<string> args
, const string& tag = string()
)
{
    auto& algorithm = getAlgorithm(algoName);
    algorithm.selectKernel(kernName);
    auto graphs = algorithm.setup(args);

    if (!tag.empty() && graphs.size() > 1) {
        reportError("Tagged output with more than 1 graph!");
    }

    for (auto graph : graphs) {
        auto label = tag.empty() ? path(graph).stem().string() : tag;
        auto basePath = outputDir / path(label);
        auto timeFile = basePath.replace_extension("timings");
        auto outputFile = basePath.replace_extension("output");
        if (noOutput) {
            timeFile = "/dev/null";
            outputFile = "/dev/null";
        }

        {
            Epoch epoch(printStdOut ? "/dev/stdout" : timeFile.string(), verbose);
            algorithm(graph, outputFile.string());
        }

        cout << algorithm.commit() << ":" << label << endl;
    }
}

static void
pin_cpu()
{
#ifdef __linux__
    cpu_set_t cpuset;
    pthread_t thread = pthread_self();

    CPU_ZERO(&cpuset);
    CPU_SET(0, &cpuset);
    int err = pthread_setaffinity_np(thread, sizeof(cpu_set_t), &cpuset);
    if (err != 0) reportError("pthread_setaffinity_np failed!");

    err = pthread_getaffinity_np(thread, sizeof(cpu_set_t), &cpuset);
    if (err != 0) reportError("pthread_getaffinity_np failed!");

    if (!CPU_ISSET(0, &cpuset)) reportError("Not set to CPU 0!");

    for (int i = 1; i < CPU_SETSIZE; i++) {
        if (CPU_ISSET(i, &cpuset)) reportError("Affinity for CPU #", i, "!");
    }
#endif
}

int main(int argc, char * const *argv)
{
    pin_cpu();

    std::reference_wrapper<Backend> activeBackend(CUDA);

    options.add('d', "device", "NUM", device, "Device to use.")
           .add('f', "framework", fw, framework::opencl, "Use OpenCL.")
           .add('L', "lib", "PATH", libPaths, "\".\"",
                "Search path for algorithm libraries.")
           .add('o', "output-dir", "DIR", outputDir,
                "Location to use for writing algorithm output.")
           .add('O', "output", printStdOut, true,
                "Print timings to stdout, inhibits timings file creation.")
           .add('p', "platform", "NUM", platform, "Platform to use.")
           .add('g', "debug", debug, true, "Use debug kernels.")
           .add('v', "verbose", verbose, true, "Verbose output.")
           .add('W', "warn", warnings, true, "Verbose/debug warnings.")
           .add('S', "stdin", fromStdin, true, "Read work from stdin.")
           .add('q', "quiet", noOutput, true,
                "Inhibit creation of output and timing files.");

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
        algorithms = loadAlgorithms("registerOpenCL", libPaths);
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
        algorithms = loadAlgorithms("registerCUDA", libPaths);
        break;
      }
    }

    Backend& backend = activeBackend;

    handle_subcommands(backend, optionResult.remainingArgs);

    bool missingArgs = !fromStdin && optionResult.remainingArgs.empty();
    if (optionResult.usageRequested || missingArgs) {
        options.usage(cout, "    ");
        if (!algorithmName.empty()) {
            auto& algorithm = getAlgorithm(algorithmName);
            algorithm.help(cout, "    ");
        }

        exit(EXIT_SUCCESS);
    }

    if (!backend.initialised) {
        cerr << "Backend not initialised: No devices?" << endl;
        exit(EXIT_FAILURE);
    }
    backend.setDevice(platform, device);

    if (fromStdin) {
        string tag;
        string line;
        wordexp_t newArgv;
        vector<string> remainingArgs;

        while (getline(cin, line)) {
            if (wordexp(line.c_str(), &newArgv, WRDE_NOCMD | WRDE_UNDEF)) {
                reportError("Failed to expand commandline with wordexp(3)!");
            }

            remainingArgs = kernelParser.parseArgs
                    (static_cast<int>(newArgv.we_wordc), newArgv.we_wordv);

            tag = newArgv.we_wordv[0];
            runJob(algorithmName, kernelName, remainingArgs, tag);

            kernelParser.reset();
            wordfree(&newArgv);
        }
    } else {
        runJob(algorithmName, kernelName, optionResult.remainingArgs);
    }

    return 0;
}
