#ifndef OPTIONS_HPP
#define OPTIONS_HPP

#include <functional>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <vector>

class Options {
    struct OptionResult {
        std::vector<std::string> remainingArgs;
        bool usageRequested;
    };

    class Option {
        friend class Options;

        char shortOption;
        std::string longOption;
        std::function<void(std::string)> action;
        std::function<void()> resetAction;
        std::string argName;
        std::string helpString;
        std::string defaultVal;

        bool hasArg;
        bool multiFlag;

        Option() : Option('\0', "", [](auto){}, [](){}, "", "")
        {}

        Option
        ( char shortOpt
        , const char *longOpt
        , std::function<void(std::string)> act
        , std::function<void()> resetAct
        , std::string arg
        , std::string help
        ) : shortOption(shortOpt), longOption(longOpt), action(act)
          , resetAction(resetAct), argName(arg), helpString(help)
          , defaultVal(""), hasArg(false), multiFlag(false)
        {}

        void reset()
        { if (resetAction != nullptr) resetAction(); }
    };

    Options(const Options&) = delete;
    void operator=(const Options&) = delete;

    static std::set<char> globalReservedShort;
    static std::set<std::string> globalReservedLong;

    Options* parent;
    std::set<Options*> children;
    std::set<char> reservedShort;
    std::set<std::string> reservedLong;

    std::ostream &usageOutput;
    std::function<void(std::ostream&)> usagePreamble;
    Option usageFlag;
    bool hasUsage, isGlobal;

    std::map<int, Option> options;

    OptionResult
    parseArgs(const std::vector<std::string>&, bool, bool);

    OptionResult
    parseArgs(int, char * const *, bool, bool);

  public:
    Options()
    : parent(nullptr), usageOutput(std::cerr), hasUsage(false), isGlobal(false)
    {}

    Options
    ( char c
    , const char *l
    , std::ostream &out
    , std::function<void(std::ostream&)> f
    ) : parent(nullptr), usageOutput(out), usagePreamble(f), hasUsage(true)
      , isGlobal(true)
    {
        usageFlag.shortOption = c;
        usageFlag.longOption = l;
        usageFlag.helpString = "This help.";
    }

    Options(Options& o)
     : parent(&o), usageOutput(std::cerr), hasUsage(false), isGlobal(false)
    { if (parent) parent->children.insert(this); }

    ~Options()
    { if (parent) parent->children.erase(this); }

    Options& add(const Option& o);
    Options& add(const char *, std::string, std::string &, std::string);
    Options& add(char, const char *, std::string, std::string &, std::string);
    Options& add(const char *, std::string, double &, std::string);
    Options& add(char, const char *, std::string, double &, std::string);

    template<typename T>
    Options& add(char so, const char *lo, T& var, T val, std::string help)
    {
        auto action = [&,val](auto) { var = val; };
        auto reset = [&,initial=T(var)]() { var = initial; };
        return add(Option(so, lo, action, reset, "", help));
    }

    template<typename T>
    Options& add(const char *lo, T& var, T val, std::string help)
    { return add('\0', lo, var, val, help); }

    template<typename T>
    Options& add
    ( char so
    , const char *lo
    , std::string arg
    , T& var
    , std::string help
    , typename std::enable_if<std::is_integral<T>::value>::type* = nullptr)
    {
        auto action = [&](auto s) { var = static_cast<T>(std::stoi(s)); };
        auto reset = [&,initial=T(var)]() { var = initial; };
        auto opt = Option(so, lo, action, reset, arg, help);
        opt.defaultVal = std::to_string(var);
        opt.hasArg = true;
        return add(opt);
    }

    template<typename T>
    Options& add
    ( const char *lo
    , std::string arg
    , T& var
    , std::string help
    , typename std::enable_if<std::is_integral<T>::value>::type* = nullptr)
    { return add('\0', lo, arg, var, help); }

    template<typename T>
    Options& add
    ( char so
    , const char *lo
    , std::string arg
    , std::vector<T>& var
    , std::string def
    , std::string help
    , std::function<T(std::string)> fun = [](auto s) { return s; }
    )
    {
        auto action = [&](std::string s) { var.push_back(fun(s)); };
        auto reset = [&]() { var.clear(); };
        auto opt = Option(so, lo, action, reset, arg, help);
        opt.defaultVal = def;
        opt.hasArg = true;
        opt.multiFlag = true;
        return add(opt);
    }

    template<typename T>
    Options& add
    ( const char *lo
    , std::string arg
    , std::vector<T>& var
    , std::string def
    , std::string help
    , std::function<T(std::string)> fun = [](auto s) { return s; }
    )
    { return add('\0', lo, arg, var, def, help, fun); }

    std::vector<std::string> parseArgs(const std::vector<std::string>&);
    std::vector<std::string> parseArgs(int, char * const *);

    OptionResult parseArgsNoUsage(const std::vector<std::string>&);
    OptionResult parseArgsNoUsage(int, char * const *);

    std::vector<std::string> parseArgsFinal(const std::vector<std::string>&);
    std::vector<std::string> parseArgsFinal(int, char * const *);

    void usage(std::ostream&, std::string = "");

    void reset();
};
#endif
