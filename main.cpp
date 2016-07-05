#include <algorithm>
#include <array>
#include <cstring>
#include <stdexcept>
#include <getopt.h>

#include "Backend.hpp"
#include "CUDA.hpp"
#include "Interface.hpp"
#include "OpenCL.hpp"
#include "Options.hpp"
#include "Timer.hpp"
#include "Util.hpp"

#include "bfs/bfs.hpp"
#include "pagerank/pagerank.hpp"

using namespace std;

enum class framework { cuda, opencl };

static const char *exeName = "main";
static Options options('h', "help", cout, [](ostream& out)
{
    out << "Usage:" << endl;
    out << exeName << " list platforms [-v | --verbose]" << endl;
    out << exeName << " list devices <platform id> [-v | --verbose]" << endl;
    out << exeName << " query platform <platform id> [-v | --verbose]" << endl;
    out << exeName << " query device <platform id> <device id> "
        << "[-v | --verbose]" << endl;
    out << exeName << " <graph file>" << endl;
    out << "Options:" << endl;
});

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    options.usage(cout, "    ");
    exit(exitCode);
}

template<typename T>
static void
run_with_backend
( Backend<T>& backend
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
            exit(static_cast<int>(backend.devicesPerPlatform.size()));

        } else if (!strcmp(args[1], "devices")) {

            backend.listDevices(platform, verbose);
            exit(backend.devicesPerPlatform[platform]);

        } else {
            usage();
        }

    } else if (!strcmp(args[0], "query")) {
        if (args.size() < 2) {
            usage();
        } else if (!strcmp(args[1], "platform")) {

            backend.queryPlatform(platform, verbose);
            exit(backend.devicesPerPlatform[platform]);

        } else if (!strcmp(args[1], "device")) {

            backend.queryDevice(platform, device, verbose);
            exit(EXIT_SUCCESS);
        } else {
            usage();
        }
    }

    backend.setDevice(platform, device);
}

int main(int argc, char **argv)
{
    int algorithm = 0;
    int variant = 0;
    size_t warp = 32;
    size_t chunk = 32;
    bool verbose = false;
    framework fw = framework::cuda;
    int device = 0;
    size_t platform = 0, run_count = 1;
    std::string outputFile;
    std::string timingsFile;
    vector<char*> remainingArgs;

    options.add('a', "algorithm", "NUM", algorithm, "Algorithm to use.")
           .add('d', "device", "NUM", device, "Device to use.")
           .add('f', "framework", fw, framework::opencl, "Use OpenCL.")
           .add('n', "count", "NUM", run_count,
                "Number of times to run algorithm.")
           .add('o', "output", "FILE", outputFile,
                "Where to write algorithm output.")
           .add('p', "platform", "NUM", platform, "Platform to use.")
           .add('t', "timings", "FILE", timingsFile,
                "Where to write timing output.")
           .add('s', "variant", "NUM", variant, "Variant to use.")
           .add('v', "verbose", verbose, true, "Verbose output.")
           .add( 'w', "warp", "NUM", warp,
                "Virtual warp size for warp variants.")
           .add('c', "chunk", "NUM", chunk,
                "Work chunk size for warp variants.");

    std::set_new_handler(out_of_memory);
    std::locale::global(std::locale(""));
    cout.imbue(std::locale());

    remainingArgs = options.parseArgs(argc, argv);

    TimerRegister timers(verbose);

    switch (fw) {
        case framework::opencl: {
            OpenCL opencl;
            run_with_backend(opencl, platform, device, verbose, remainingArgs);
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
            CUDA cuda;
            run_with_backend(cuda, platform, device, verbose, remainingArgs);
            switch (algorithm) {
                case 0:
                    bfs(cuda, timers, run_count, remainingArgs[0], outputFile, variant, warp, chunk);
                    break;
                case 1:
                    pagerank(cuda, timers, run_count, remainingArgs[0], outputFile, variant, warp, chunk);
                    break;
                default:
                    break;
            }
            break;
        }
    }

    if (timingsFile.empty()) {
        timers.print_results(cout);
    } else {
        std::ofstream timings(timingsFile);
        timers.print_results(timings);
    }

    return 0;
}
