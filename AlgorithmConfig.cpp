#include "AlgorithmConfig.hpp"

AlgorithmConfig::~AlgorithmConfig() {}

void
AlgorithmConfig::operator()(const std::string filename, const std::string out)
{
    loadGraph(filename);
    outputFile = out;
    runImplementation();
    outputFile.clear();
}

void
AlgorithmConfig::help(std::ostream& out, std::string prefix)
{ options.usage(out, prefix); }

std::vector<char*>
AlgorithmConfig::setup(std::vector<char*> args)
{ return options.parseArgsFinal(args); }
