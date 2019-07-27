#include <fstream>

#include "ImplementationBase.hpp"

using namespace std;

ImplementationBase::~ImplementationBase() {}

void
ImplementationBase::operator()
(const std::string& filename, ofstream&& outputFile)
{
    prepareRun();
    loadGraph(filename);
    runImplementation(outputFile);
    freeGraph();
    cleanupRun();
    options.reset();
}

void
ImplementationBase::operator()
(const string& filename, const string& outputFile)
{ operator()(filename, ofstream(outputFile)); }

void
ImplementationBase::help(std::ostream& out, std::string prefix)
{ options.usage(out, prefix); }

std::vector<string>
ImplementationBase::setup(std::vector<string> args)
{ return options.parseArgsFinal(args); }
