#include <map>
#include <boost/algorithm/string/predicate.hpp>

#include "Options.hpp"
#include "utils/Util.hpp"

using namespace std;

set<char> Options::globalReservedShort;
set<string> Options::globalReservedLong;

Options&
Options::add(const Option& o)
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
    auto reset = [&,initial=string(var)]() { var = initial; };
    auto opt = Option(so, lo, action, reset, arg, help);
    opt.defaultVal = var;
    opt.hasArg = true;
    return add(opt);
}

vector<string>
Options::parseArgs(const vector<string>& args)
{ return parseArgs(args, false); }

vector<string>
Options::parseArgs(int argc, char * const *argv)
{ return parseArgs(argc, argv, false); }

vector<string>
Options::parseArgsFinal(const vector<string>& args)
{ return parseArgs(args, true); }

vector<string>
Options::parseArgsFinal(int argc, char * const *argv)
{ return parseArgs(argc, argv, true); }

vector<string>
Options::parseArgs(int argc, char * const *argv, bool exitUnknown)
{
    vector<string> args;
    args.reserve(static_cast<unsigned long>(argc));
    for (int i = 1; i < argc; i++) args.emplace_back(argv[i]);
    return parseArgs(args, exitUnknown);
}

vector<string>
Options::parseArgs(const vector<string>& args, bool exitUnknown)
{
    string empty = "";
    vector<string> remainingArgs;
    map<string, Option> optionParsers;

    pair<map<string, Option>::iterator, bool> result;
    auto addOption = [&](string flag, const Option& opt) {
        result = optionParsers.emplace(make_pair(flag, opt));
        if (!result.second) {
            reportError("Option ", flag, " is already in use!");
        }
    };

    if (hasUsage) {
        addOption("-" + string(1,usageFlag.shortOption), usageFlag);
        addOption("--" + usageFlag.longOption, usageFlag);
    }

    for (auto kv : options) {
        auto opt = kv.second;
        addOption("-" + string(1,opt.shortOption), opt);
        addOption("--" + opt.longOption, opt);
    }

    for (unsigned i = 0; i < args.size(); i++) {
        auto it = optionParsers.find(args[i]);
        if (it == optionParsers.end()) {
            if (boost::starts_with(args[i], "--") && exitUnknown) {
                cerr << "Unknown option '" << args[i] << "'!" << endl;
                usage(usageOutput);
                exit(EXIT_FAILURE);
            }

            remainingArgs.push_back(args[i]);
            continue;
        }

        auto opt = it->second;
        if (opt.hasArg) {
            if (args.size() <= i+1) {
                reportError("Option ", args[i], " doesn't have an argument!");
            }
            opt.action(args[i+1]);
            i++;
        } else {
            opt.action(empty);
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
