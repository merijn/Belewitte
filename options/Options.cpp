#include <getopt.h>
#include <map>

#include "Options.hpp"
#include "utils/Util.hpp"

using namespace std;

set<char> Options::globalReservedShort;
set<string> Options::globalReservedLong;

Options&
Options::add(Option o)
{
    set<char> shortUnion(globalReservedShort);
    set<string> longUnion(globalReservedLong);

    shortUnion.insert(reservedShort.begin(), reservedShort.end());
    longUnion.insert(reservedLong.begin(), reservedLong.end());

    if (options.count(o.shortOption) || shortUnion.count(o.shortOption)) {
        reportError("Flag '-", o.shortOption, "' is already reserved.");
    } else if (longUnion.count(o.longOption)) {
        reportError("Flag '--", o.longOption, "' is already reserved.");
    }

    options.emplace(o.shortOption, o);
    if (isGlobal) {
        globalReservedShort.emplace(o.shortOption);
        globalReservedLong.emplace(o.longOption);
    } else {
        reservedShort.emplace(o.shortOption);
        reservedLong.emplace(o.longOption);
    }

    if (parent) parent->add(o);
    return *this;
}

Options&
Options::add(char so, const char *lo, string arg, string &var, string help)
{
    auto action = [&](const string& s) { var = s; };
    auto reset = [&,initial{var}]() { var = initial; };
    auto opt = Option(so, lo, action, reset, arg, help);
    opt.defaultVal = var;
    opt.hasArg = true;
    return add(opt);
}

vector<string>
Options::parseArgs(vector<string>& args)
{ return parseArgs(args, false); }

vector<string>
Options::parseArgs(int argc, char * const *argv)
{ return parseArgs(argc, argv, false); }

vector<string>
Options::parseArgsFinal(vector<string>& args)
{ return parseArgs(args, true); }

vector<string>
Options::parseArgsFinal(int argc, char * const *argv)
{ return parseArgs(argc, argv, true); }

vector<string>
Options::parseArgs(vector<string> args, bool exitUnknown)
{
    vector<const char*> tmp;
    tmp.reserve(args.size() + 1);
    tmp.push_back(const_cast<char*>("dummy"));
    for (auto& s : args ){
        tmp.push_back(s.c_str());
    }
    tmp.push_back(nullptr);
    return parseArgs(static_cast<int>(tmp.size()) - 1,
                     const_cast<char * const *>(tmp.data()), exitUnknown);
}

vector<string>
Options::parseArgs(int argc, char * const *argv, bool exitUnknown)
{
    vector<string> remainingArgs;
    map<int, function<void(const string&)>> actions;
    string shortopts = "-:";
    vector<option> longopts;

    if (hasUsage) {
        shortopts += usageFlag.shortOption;
        longopts.push_back(
            { usageFlag.longOption
            , no_argument
            , nullptr
            , usageFlag.shortOption
            }
        );
    }

    for (auto kv : options) {
        auto opt = kv.second;
        if (actions.count(opt.shortOption)) {
            exit(EXIT_FAILURE);
        }

        shortopts += opt.shortOption;
        longopts.push_back(
            { opt.longOption
            , no_argument
            , nullptr
            , opt.shortOption
            }
        );

        if (opt.hasArg) {
            shortopts += ':';
            longopts.back().has_arg = required_argument;
        }

        actions[opt.shortOption] = opt.action;
    }

    longopts.push_back({nullptr, 0, nullptr, 0});

    optind = 1;
#ifdef __APPLE__
    optreset = 1;
#endif

    for (;;) {
        int opt = getopt_long(argc, argv, shortopts.c_str(), longopts.data(), nullptr);
        if (opt == -1) break;

        if (hasUsage && opt == usageFlag.shortOption) {
            usage(usageOutput);
            exit(EXIT_SUCCESS);
        }

        switch (opt) {
            case '?':
                if (exitUnknown) {
                    cerr << "Unknown option '" << argv[optind-1] << "'!" << endl;
                    usage(usageOutput);
                    exit(EXIT_FAILURE);
                }
                remainingArgs.push_back(argv[optind-1]);
                break;

            case 1:
                remainingArgs.push_back(optarg);
                break;

            case 0: break;

            case ':':
                cerr << "Missing option for flag '"
                     << static_cast<char>(optopt) << "'." << endl;
                usage(usageOutput);
                exit(EXIT_FAILURE);

            default:
                actions[opt](optarg ? string(optarg) : string(""));
        }
    }

    return remainingArgs;
}

void
Options::usage(ostream& out, string prefix)
{
    auto renderOpt = [&](Option opt) {
        out << prefix << '-' << opt.shortOption;
        if (opt.hasArg) out << " " << opt.argName;
        out << " | " << "--" << opt.longOption;
        if (opt.hasArg) out << " " << opt.argName;
        out << endl << prefix << '\t' << opt.helpString << endl;

        if (opt.defaultVal.size()) {
            out << prefix << "\t(default: " << opt.defaultVal << ')' << endl;
        }
        out << endl;
    };

    if (hasUsage) {
        usagePreamble(out);
        renderOpt(usageFlag);
    }

    for (auto kv : options) renderOpt(kv.second);
}

void
Options::reset()
{
    for (auto &child : children) {
        child->reset();
    }

    for (auto& [key, option] : options) {
        option.reset();
    }
}
