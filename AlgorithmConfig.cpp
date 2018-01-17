#include <fstream>

#include "AlgorithmConfig.hpp"

using namespace std;

AlgorithmConfig::~AlgorithmConfig() {}

void
AlgorithmConfig::operator()(const std::string& filename, ofstream&& outputFile)
{
    prepareRun();
    loadGraph(filename);
    runImplementation(outputFile);
    freeGraph();
    cleanupRun();
    options.reset();
}

void
AlgorithmConfig::operator()(const string& filename, const string& outputFile)
{ operator()(filename, ofstream(outputFile)); }

void
AlgorithmConfig::help(std::ostream& out, std::string prefix)
{ options.usage(out, prefix); }

std::vector<char*>
AlgorithmConfig::setup(std::vector<char*> args)
{ return options.parseArgsFinal(args); }
