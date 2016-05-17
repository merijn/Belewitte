#include <algorithm>
#include <array>
#include <cstring>
#include <stdexcept>
#include <getopt.h>

#include "Backend.hpp"
#include "CUDA.hpp"
#include "Interface.hpp"
#include "OpenCL.hpp"
#include "Timer.hpp"
#include "Util.hpp"

#include "bfs/bfs.hpp"
#include "pagerank/pagerank.hpp"

using namespace std;

enum class framework { cuda, opencl };

static const char *execName = "main";

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    cout << "Usage:" << endl;
    cout << execName << " list platforms [-v | --verbose]" << endl;
    cout << execName << " list devices <platform id> [-v | --verbose]" << endl;
    cout << execName << " query platform <platform id> [-v | --verbose]" << endl;
    cout << execName << " query device <platform id> <device id> [-v | --verbose]" << endl;
    cout << execName << " <graph file>" << endl;
    cout << "Options:" << endl;
    cout << "\t" << "-a NUM | --algorithm NUM" << endl;
    cout << "\t\t" << "Algorithm to use." << endl << endl;
    cout << "\t" << "-d NUM | --device NUM" << endl;
    cout << "\t\t" << "Device to use." << endl << endl;
    cout << "\t" << "-f | --framework" << endl;
    cout << "\t\t" << "Use OpenCL." << endl << endl;
    cout << "\t" << "-n NUM | --count NUM" << endl;
    cout << "\t\t" << "Number of times to run algorithm." << endl << endl;
    cout << "\t" << "-o FILE | --output FILE" << endl;
    cout << "\t\t" << "Where to write algorithm output." << endl << endl;
    cout << "\t" << "-p NUM | --platform NUM" << endl;
    cout << "\t\t" << "Platform to use." << endl << endl;
    cout << "\t" << "-t FILE | --timings FILE" << endl;
    cout << "\t\t" << "Where to write timing output." << endl << endl;
    cout << "\t" << "-s NUM | --variant NUM" << endl;
    cout << "\t\t" << "Algorithm variant to run." << endl << endl;
    cout << "\t" << "-v | --verbose" << endl;
    cout << "\t\t" << "Verbose output." << endl << endl;
    cout << "\t" << "-h | --help" << endl;
    cout << "\t\t" << "This help." << endl << endl;
    cout << "\t" << "-w NUM | --warp NUM" << endl;
    cout << "\t\t" << "Virtual warp size for warp variants." << endl << endl;
    cout << "\t" << "-c NUM | --chunk NUM" << endl;
    cout << "\t\t" << "Work chunk size for warp variants." << endl;
    exit(exitCode);
}

template<typename T>
static void
run_with_backend
( Backend<T>& backend
, int argc
, char **argv
, size_t platform
, int device
, bool verbose)
{
    if (argc < 2) usage();

    if (optind < argc && !strcmp(argv[optind], "list")) {
        optind++;

        if (optind < argc && !strcmp(argv[optind], "platforms")) {

            backend.listPlatforms(verbose);
            exit(static_cast<int>(backend.devicesPerPlatform.size()));

        } else if (optind  < argc && !strcmp(argv[optind], "devices")) {

            backend.listDevices(platform, verbose);
            exit(backend.devicesPerPlatform[platform]);

        } else {
            usage();
        }

    } else if (optind < argc && !strcmp(argv[optind], "query")) {
        optind++;

        if (optind < argc && !strcmp(argv[optind], "platform")) {

            backend.queryPlatform(platform, verbose);
            exit(backend.devicesPerPlatform[platform]);

        } else if (optind < argc && !strcmp(argv[optind], "device")) {

            backend.queryDevice(platform, device, verbose);
            exit(EXIT_SUCCESS);
        } else {
            usage();
        }
    }

    backend.setDevice(platform, device);
    cout << "Running " << backend.name << ":" << endl;
}

int main(int argc, char **argv)
{
    int algorithm = 0;
    int variant = 0;
    size_t warp = 32;
    size_t chunk = 32;
    int verbose = false;
    framework fw = framework::cuda;
    int device = 0;
    size_t platform = 0, run_count = 1;
    char *outputFile = NULL;
    char *timingsFile = NULL;
    const char *optString = ":a:d:fn:o:p:s:t:vh?";
    static const struct option longopts[] = {
        { "algorithm", required_argument, nullptr, 'a' },
        { "device", required_argument, nullptr, 'd' },
        { "framework", no_argument, nullptr, 'f' },
        { "count", required_argument, nullptr, 'n' },
        { "output", required_argument, nullptr, 'o' },
        { "platform", required_argument, nullptr, 'p' },
        { "timings", required_argument, nullptr, 't' },
        { "variant", required_argument, nullptr, 's' },
        { "verbose", no_argument, &verbose, 1},
        { "help", no_argument, nullptr, 'h' },
        { "warp", required_argument, nullptr, 'w' },
        { "chunk", required_argument, nullptr, 'c' },
        { nullptr, 0, nullptr, 0 },
    };

    execName = argv[0];
    std::set_new_handler(out_of_memory);
    std::locale::global(std::locale(""));
    cout.imbue(std::locale());

    for (;;) {
        int longIndex;
        int opt = getopt_long(argc, argv, optString, longopts, &longIndex);
        if (opt == -1) break;

        switch (opt) {
            case 'a':
                algorithm = stoi(optarg);
                break;

            case 'd':
                device = stoi(optarg);
                break;

            case 'f':
                fw = framework::opencl;
                break;

            case 'n':
                run_count = static_cast<size_t>(stoi(optarg));
                break;

            case 'o':
                outputFile = optarg;
                break;

            case 'p':
                platform = static_cast<size_t>(stoi(optarg));
                break;

            case 's':
                variant = stoi(optarg);
                break;

            case 't':
                timingsFile = optarg;
                break;

            case 'v':
                verbose = true;
                break;

            case 'w':
                warp = static_cast<size_t>(stoi(optarg));
                break;

            case 'c':
                chunk = static_cast<size_t>(stoi(optarg));
                break;

            case 'h':
            case '?':
                usage(EXIT_SUCCESS);

            case 0: break;

            case ':':
                cerr << "Missing option for flag '" << optopt << "'." << endl;
                FALLTHROUGH;
            default:
                usage();
        }
    }

    TimerRegister timers(verbose);

    switch (fw) {
        case framework::opencl: {
            OpenCL opencl;
            run_with_backend(opencl, argc, argv, platform, device, verbose);
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
            run_with_backend(cuda, argc, argv, platform, device, verbose);
            switch (algorithm) {
                case 0:
                    bfs(cuda, timers, run_count, argv[optind], outputFile, variant, warp, chunk);
                    break;
                case 1:
                    pagerank(cuda, timers, run_count, argv[optind], outputFile, variant, warp, chunk);
                    break;
                default:
                    break;
            }
            break;
        }
    }

    if (timingsFile) {
        std::ofstream timings(timingsFile);
        timers.print_results(timings);
    } else {
        timers.print_results(cout);
    }

    return 0;
}
