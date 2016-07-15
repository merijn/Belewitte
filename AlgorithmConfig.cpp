#include "AlgorithmConfig.hpp"

AlgorithmConfig::~AlgorithmConfig() {}

void
AlgorithmConfig::operator()(const std::string filename)
{
    transferGraph(filename);
    runImplementation();
}

void
AlgorithmConfig::help(std::ostream& out, std::string prefix)
{ options.usage(out, prefix); }

std::vector<char*>
AlgorithmConfig::setup(std::vector<char*> args)
{ return options.parseArgsFinal(args); }
