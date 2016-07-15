#ifndef __ALGORITHMCONFIG_HPP__
#define __ALGORITHMCONFIG_HPP__

#include "Options.hpp"

struct AlgorithmConfig {
  protected:
    AlgorithmConfig(const Options& opts, size_t count, std::string out)
     : options(opts, true), run_count(count), outputFile(out)
    {}

    AlgorithmConfig(const AlgorithmConfig& o)
     : options(o.options), run_count(o.run_count), outputFile(o.outputFile)
    {}

    virtual ~AlgorithmConfig();

    Options options;
    size_t run_count;
    std::string outputFile;

    virtual void transferGraph(const std::string) = 0;
    virtual void runImplementation() = 0;

  public:
    void operator()(const std::string filename);
    void help(std::ostream& out, std::string prefix);
    std::vector<char*> setup(std::vector<char*> args);
};
#endif
