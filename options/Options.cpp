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
    auto action = [&](auto s) { var = s; };
    auto opt = Option(so, lo, action, arg, help);
    opt.defaultVal = var;
    opt.hasArg = true;
    return add(opt);
}

Options&
Options::add
( char so
, const char *lo
, std::string arg
, std::string help
, std::function<void(const char *)> action
)
{
    auto opt = Option(so, lo, action, arg, help);
    opt.hasArg = true;
    return add(opt);
}

vector<char *>
Options::parseArgs(vector<char *>& args)
{
    vector<char*> tmp;
    tmp.reserve(args.size() + 1);
    tmp.push_back(const_cast<char*>("dummy"));
    tmp.insert(tmp.begin()+1, args.begin(), args.end());
    return parseArgs(static_cast<int>(tmp.size()), tmp.data());
}

vector<char *>
Options::parseArgs(int argc, char **argv)
{ return parseArgs(argc, argv, false); }

vector<char *>
Options::parseArgsFinal(vector<char *>& args)
{
    vector<char*> tmp;
    tmp.reserve(args.size() + 1);
    tmp.push_back(const_cast<char*>("dummy"));
    tmp.insert(tmp.begin()+1, args.begin(), args.end());
    return parseArgsFinal(static_cast<int>(tmp.size()), tmp.data());
}

vector<char *>
Options::parseArgsFinal(int argc, char **argv)
{ return parseArgs(argc, argv, true); }

vector<char *>
Options::parseArgs(int argc, char **argv, bool exitUnknown)
{
    vector<char*> remainingArgs;
    map<int, function<void(const char*)>> actions;
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
                cerr << "Missing option for flag '" << static_cast<char>(optopt) << "'." << endl;
                usage(usageOutput);
                exit(EXIT_FAILURE);

            default:
                actions[opt](optarg);
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
