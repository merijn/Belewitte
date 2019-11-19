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
Options::add(const char *lo, string arg, string &var, string help)
{ return add('\0', lo, arg, var, help); }

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

Options&
Options::add(const char *lo, string arg, double &var, string help)
{ return add('\0', lo, arg, var, help); }

Options&
Options::add(char so, const char *lo, string arg, double &var, string help)
{
    auto action = [&](auto s) { var = std::stod(s); };
    auto reset = [&,initial=var]() { var = initial; };
    auto opt = Option(so, lo, action, reset, arg, help);
    opt.defaultVal = std::to_string(var);
    opt.hasArg = true;
    return add(opt);
}

vector<string>
Options::parseArgs(const vector<string>& args)
{
    auto result = parseArgs(args, false, true);
    assert(!result.usageRequested);
    return result.remainingArgs;
}

vector<string>
Options::parseArgs(int argc, char * const *argv)
{
    auto result = parseArgs(argc, argv, false, true);
    assert(!result.usageRequested);
    return result.remainingArgs;
}

Options::OptionResult
Options::parseArgsNoUsage(const vector<string>& args)
{ return parseArgs(args, false, false); }

Options::OptionResult
Options::parseArgsNoUsage(int argc, char * const *argv)
{ return parseArgs(argc, argv, false, false); }

vector<string>
Options::parseArgsFinal(const vector<string>& args)
{
    auto result = parseArgs(args, true, true);
    assert(!result.usageRequested);
    return result.remainingArgs;
}

vector<string>
Options::parseArgsFinal(int argc, char * const *argv)
{
    auto result = parseArgs(argc, argv, true, true);
    assert(!result.usageRequested);
    return result.remainingArgs;
}

Options::OptionResult
Options::parseArgs
(int argc, char * const *argv, bool exitUnknown, bool exitUsage)
{
    vector<string> args;
    args.reserve(static_cast<unsigned long>(argc));
    for (int i = 1; i < argc; i++) args.emplace_back(argv[i]);
    return parseArgs(args, exitUnknown, exitUsage);
}

Options::OptionResult
Options::parseArgs
(const vector<string>& args, bool exitUnknown, bool exitUsage)
{
    bool usageRequested = false;
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
        if (exitUsage) {
            usageFlag.action = [this](std::string) {
                this->usage(usageOutput);
                exit(EXIT_SUCCESS);
            };
        } else {
            usageFlag.action = [&usageRequested](std::string) {
                    usageRequested = true;
            };
        }

        if (usageFlag.shortOption != '\0') {
            addOption("-" + string(1,usageFlag.shortOption), usageFlag);
        }
        addOption("--" + usageFlag.longOption, usageFlag);
    }

    for (auto kv : options) {
        auto opt = kv.second;
        if (opt.shortOption != '\0') {
            addOption("-" + string(1,opt.shortOption), opt);
        }
        addOption("--" + opt.longOption, opt);
    }

    for (unsigned i = 0; i < args.size(); i++) {
        auto it = optionParsers.find(args[i]);
        if (it == optionParsers.end()) {
            if (args[i] == "--") {
                // Dump everything into remainingArgs and exit parse loop
                for (unsigned j = i+1; j < args.size(); j++) {
                    remainingArgs.push_back(args[j]);
                }
                break;
            }

            if (boost::starts_with(args[i], "-") && exitUnknown) {
                cerr << "Unknown option '" << args[i] << "'!" << endl << endl;
                cerr << "Possible options:" << endl;
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

    return {remainingArgs, usageRequested};
}

void
Options::usage(ostream& out, string prefix)
{
    auto renderOpt = [&](const Option& opt) {
        out << prefix;
        if (opt.shortOption != '\0') {
            out << '-' << opt.shortOption;
            if (opt.hasArg) out << " " << opt.argName;
            out << " | ";
        }
        out << "--" << opt.longOption;
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
