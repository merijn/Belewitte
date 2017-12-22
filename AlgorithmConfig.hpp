#ifndef ALGORITHMCONFIG_HPP
#define ALGORITHMCONFIG_HPP

#include "options/Options.hpp"

struct AlgorithmConfig {
  protected:
    AlgorithmConfig(const Options& opts, size_t count)
     : options(opts, true), run_count(count)
    {}

    AlgorithmConfig(const AlgorithmConfig&) = delete;
    void operator=(const AlgorithmConfig&) = delete;

    virtual ~AlgorithmConfig();

    Options options;
    size_t run_count;
    std::string outputFile;

    virtual void loadGraph(const std::string) = 0;
    virtual void runImplementation() = 0;

  public:
    void operator()(const std::string filename, const std::string outputFile);
    void help(std::ostream& out, std::string prefix);
    std::vector<char*> setup(std::vector<char*> args);
};
#endif
