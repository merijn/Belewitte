#ifndef IMPLEMENTATIONBASE_HPP
#define IMPLEMENTATIONBASE_HPP

#include "options/Options.hpp"

struct ImplementationBase {
    const std::string commit;

  protected:
    ImplementationBase(const std::string& commitHash)
      : commit(commitHash)
      , run_count(1)
    {
        options.add('n', "count", "NUM", run_count,
                    "Number of times to run algorithm.");
    }

    ImplementationBase(const ImplementationBase&) = delete;
    void operator=(const ImplementationBase&) = delete;

    Options options;
    size_t run_count;

    virtual void loadGraph(const std::string) = 0;
    virtual void prepareRun() {}
    virtual void runImplementation(std::ofstream&) = 0;
    virtual void cleanupRun() {}
    virtual void freeGraph() = 0;

  public:
    void operator()(const std::string& graphFile, std::ofstream&& output);
    void operator()(const std::string& graphFile, const std::string& output);
    void help(std::ostream& out, std::string prefix);
    std::vector<std::string> setup(std::vector<std::string> args);

    virtual ~ImplementationBase();
};
#endif
