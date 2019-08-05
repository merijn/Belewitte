#include <stdexcept>
#include <sstream>
#include <utility>

#include "Algorithm.hpp"
#include "utils/Util.hpp"

Algorithm::Algorithm()
  : selectedKernel(nullptr)
{}

Algorithm::Algorithm(const std::string& commit)
  : commitHash(commit), selectedKernel(nullptr)
{}

Algorithm::Algorithm(Algorithm&& other)
  : commitHash(std::move(other.commitHash))
  , selectedKernel(std::move(other.selectedKernel))
  , implementations(std::move(other.implementations))
{}

Algorithm&
Algorithm::operator=(Algorithm&& other)
{
    commitHash = std::move(other.commitHash);
    selectedKernel = std::move(other.selectedKernel);
    implementations = std::move(other.implementations);
    return *this;
}

void
Algorithm::selectKernel(const std::string& kernelName)
{
    bool kernelMissing = false;
    std::string errorMsg;

    try {
        selectedKernel = implementations.at(kernelName).get();
        if (!selectedKernel) kernelMissing = true;
    } catch (const std::out_of_range &) {
        kernelMissing = true;
    }

    if (commitHash.empty()) reportError("Algorithm version not specified!");
    if (kernelMissing) {
        if (kernelName.empty()) {
            errorMsg = "Kernel name not specified!";
        } else {
            errorMsg = "No kernel named \"" + kernelName + "\" exists!";
        }

        reportError(errorMsg, "\n\nSupported kernels:\n", kernelNames());
    }
}

void
Algorithm::addImplementation
(std::string name, std::unique_ptr<ImplementationBase>&& impl)
{
    auto result = implementations.emplace(name, std::move(impl));
    if (!result.second) {
        reportError("Implementation with name \"" + name + "\" already exists!");
    }
}

void
Algorithm::operator()(const std::string& graphFile, std::ofstream&& output)
{
    if (!selectedKernel) reportError("No kernel selected to run!");
    selectedKernel->operator()(graphFile, std::move(output));
}

void
Algorithm::operator()(const std::string& graphFile, const std::string& output)
{
    if (!selectedKernel) reportError("No kernel selected to run!");
    selectedKernel->operator()(graphFile, output);
}

void
Algorithm::help(std::ostream& out, std::string prefix)
{
    if (!selectedKernel) reportError("No kernel selected!");
    selectedKernel->help(out, prefix);
}

std::vector<std::string>
Algorithm::setup(std::vector<std::string> args)
{
    if (!selectedKernel) reportError("No kernel selected!");
    return selectedKernel->setup(args);
}

std::string
Algorithm::commit()
{ return commitHash; }

std::string
Algorithm::kernelNames()
{
    std::ostringstream names;
    for (auto& kernel : implementations) {
        names << "    " << kernel.first << std::endl;
    }

    return names.str();
}
