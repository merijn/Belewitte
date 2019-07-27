#ifndef ALGORITHM_HPP
#define ALGORITHM_HPP

#include <string>

#include "ImplementationBase.hpp"

class Algorithm {
    using ImplMap = std::map<std::string, std::unique_ptr<ImplementationBase>>;

    typedef ImplMap::iterator iterator;
    typedef ImplMap::const_iterator const_iterator;
    typedef ImplMap::reverse_iterator reverse_iterator;
    typedef ImplMap::const_reverse_iterator const_reverse_iterator;

  public:
    Algorithm();
    Algorithm(const std::string& commit);
    Algorithm(Algorithm&& other);

    Algorithm& operator=(Algorithm&& other);

    void selectKernel(const std::string& kernelName);

    void addImplementation(std::string, std::unique_ptr<ImplementationBase>&&);

    void operator()(const std::string& graphFile, std::ofstream&& output);
    void operator()(const std::string& graphFile, const std::string& output);
    void help(std::ostream& out, std::string prefix);
    std::vector<std::string> setup(std::vector<std::string> args);

    std::string commit();

    iterator begin() { return implementations.begin(); }
    const_iterator begin() const { return implementations.begin(); }
    const_iterator cbegin() const { return implementations.cbegin(); }

    iterator end() { return implementations.end(); }
    const_iterator end() const { return implementations.end(); }
    const_iterator cend() const { return implementations.cend(); }

    reverse_iterator rbegin() { return implementations.rbegin(); }
    const_reverse_iterator rbegin() const { return implementations.rbegin(); }
    const_reverse_iterator crbegin() const { return implementations.crbegin(); }

    reverse_iterator rend() { return implementations.rend(); }
    const_reverse_iterator rend() const { return implementations.rend(); }
    const_reverse_iterator crend() const { return implementations.crend(); }

  private:
    std::string kernelNames();

    std::string commitHash;
    ImplementationBase* selectedKernel;
    std::map<std::string, std::unique_ptr<ImplementationBase>> implementations;
};
#endif
