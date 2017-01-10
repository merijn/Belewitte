#include <algorithm>
#include <array>
#include <cstring>
#include <stdexcept>
#include <getopt.h>

#include <dlfcn.h>

#include <boost/filesystem.hpp>
#include <boost/regex.hpp>

#include "AlgorithmConfig.hpp"
#include "Backend.hpp"
#include "CUDA.hpp"
#include "OpenCL.hpp"
#include "Options.hpp"
#include "Timer.hpp"
#include "Util.hpp"

using namespace std;
using namespace boost::filesystem;

enum class framework { cuda, opencl };
enum class listing { nothing, algorithms, implementations };

static const char *exeName = "main";
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

    out << exeName << " <graph file(s)>" << endl << endl;
    out << "Options:" << endl;
});

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    options.usage(cout, "    ");
    exit(exitCode);
}

static map<string, map<string, AlgorithmConfig*>>
loadAlgorithms
    ( const char *sym
    , vector<const char*> &paths
    , const Options& opts
    , size_t run_count
    , bool warn)
{
    map<string, string> libs;
    map<string, map<string, AlgorithmConfig*>> result;

    if (is_directory("./build/")) {
        for (auto& entry : directory_iterator("./build/")) {
            if (!is_directory(entry)) continue;

            auto p = entry.path();
            auto filename = p.filename().string();
            p /= ("lib" + filename + ".so");
            libs.emplace(filename, p.string());
        }
    }

    const boost::regex lib_regex("lib(.*)\\.so" );
    boost::cmatch match;

    for (auto p_str : paths) {
        if (!is_directory(p_str)) continue;

        for (auto& entry : directory_iterator(p_str)) {
            if (!is_regular_file(entry)) continue;

            auto p = entry.path();
            if (boost::regex_match(p.filename().c_str(), match, lib_regex)) {
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

        if (dispatch != nullptr) {
            dispatch(result[lib.first], opts, run_count);
        } else if (warn) {
            cerr << "dlsym() failed: " << sym << " (" << lib.second << ") "
                 << endl << dlerror() << endl;
        }
    }

    return result;
}

static listing
run_with_backend
( Backend& backend
, size_t platform
, int device
, bool verbose
, vector<char*> args)
{
    if (args.size() < 1) usage();

    if (!strcmp(args[0], "list")) {
        if (args.size() < 2) {
            usage();
        } else if (!strcmp(args[1], "platforms")) {

            backend.listPlatforms(verbose);
            exit(static_cast<int>(backend.platformCount()));

        } else if (!strcmp(args[1], "devices")) {

            backend.listDevices(platform, verbose);
            exit(backend.deviceCount(platform));

        } else if (!strcmp(args[1], "algorithms")) {
            return listing::algorithms;
        } else if (!strcmp(args[1], "implementations")) {
            return listing::implementations;
        } else {
            usage();
        }

    } else if (!strcmp(args[0], "query")) {
        if (args.size() < 2) {
            usage();
        } else if (!strcmp(args[1], "platform")) {

            backend.queryPlatform(platform, verbose);
            exit(backend.deviceCount(platform));

        } else if (!strcmp(args[1], "device")) {

            backend.queryDevice(platform, device, verbose);
            exit(EXIT_SUCCESS);
        } else {
            usage();
        }
    }

    if (!backend.initialised) {
        cerr << "Backend not initialised: No devices?" << endl;
        exit(EXIT_FAILURE);
    }
    backend.setDevice(platform, device);
    return listing::nothing;
}

int main(int argc, char **argv)
{
    listing list;
    bool verbose = false;
    bool warnings = false;
    bool printStdOut = false;
    framework fw = framework::cuda;
    int device = 0;
    size_t platform = 0, run_count = 1;
    string outputSuffix;
    string outputDir;
    string timingsFile;
    string algorithmName;
    string kernelName;
    vector<const char*> paths = { "." };
    vector<char*> remainingArgs;

    options.add('a', "algorithm", "NAME", algorithmName, "Algorithm to use.")
           .add('d', "device", "NUM", device, "Device to use.")
           .add('f', "framework", fw, framework::opencl, "Use OpenCL.")
           .add('k', "kernel", "NAME", kernelName,
                "Which algorithm implementation to use.")
           .add('L', "lib", "PATH", paths, "\".\"",
                "Search path for algorithm libraries.")
           .add('n', "count", "NUM", run_count,
                "Number of times to run algorithm.")
           .add('o', "output-dir", "DIR", outputDir,
                "Location to use for writing algorithm output.")
           .add('O', "output", printStdOut, true,
                "Print result to stdout, inhibits output file creation.")
           .add('p', "platform", "NUM", platform, "Platform to use.")
           .add('s', "suffix", "EXT", outputSuffix,
                "Extension to use for writing algorithm output.")
           .add('t', "timings", "FILE", timingsFile,
                "Where to write timing output.")
           .add('v', "verbose", verbose, true, "Verbose output.")
           .add('W', "warn", warnings, true, "Verbose/debug warnings.");

    set_new_handler(out_of_memory);
    locale::global(locale(""));
    cout.imbue(locale());

    remainingArgs = options.parseArgs(argc, argv);

    map<string, map<string, AlgorithmConfig*>> algorithms;
    switch (fw) {
        case framework::opencl: {
            list = run_with_backend(OpenCL, platform, device,
                                    verbose, remainingArgs);

            algorithms = loadAlgorithms("openclDispatch", paths, options,
                                        run_count, warnings);
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
            list = run_with_backend(CUDA, platform, device,
                                    verbose, remainingArgs);

            algorithms = loadAlgorithms("cudaDispatch", paths, options,
                                        run_count, warnings);
            break;
        }
    }

    switch (list) {
        case listing::algorithms:
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
        case listing::implementations:
            for (auto &kernel : algorithms[algorithmName]) {
                cout << kernel.first << endl;
                if (verbose) kernel.second->help(cout, "    ");
            }
            exit(EXIT_SUCCESS);
        case listing::nothing: break;
    }

    if (algorithmName.empty()) {
        cerr << "Algorithm name not specified!" << endl;
        exit(EXIT_FAILURE);
    } else if (kernelName.empty()) {
        cerr << "Kernel name not specified!" << endl;
        exit(EXIT_FAILURE);
    } else {
        try {
            auto algorithm = algorithms.at(algorithmName);
            try {
                AlgorithmConfig &kernel = *algorithm.at(kernelName);
                remainingArgs = kernel.setup(remainingArgs);

                if (!timingsFile.empty()) {
                    TimerRegister::set_output(timingsFile);
                }
                TimerRegister::set_human_readable(verbose);
                TimerRegister::set_direct_printed(true);

                for (auto graph : remainingArgs) {
                    TimerRegister::start_epoch(path(graph).stem().string());
                    string output;
                    if (printStdOut) {
                        output = "/dev/stdout";
                    } else if (!outputSuffix.empty()) {
                        auto filename = outputDir / path(graph).filename();
                        filename.replace_extension(outputSuffix);

                        output = filename.string();
                    }
                    kernel(graph, output);
                }

                TimerRegister::print_results();
            } catch (const out_of_range &) {
                reportError("No kernel named \"", kernelName,
                            "\" for algorithm \"", algorithmName, "\"!");
            }
        } catch (const out_of_range &) {
            reportError("No algorithm named \"", algorithmName, "\"!");
        }
    }
    return 0;
}
