#ifndef STATISTICALSUMMARY_HPP
#define STATISTICALSUMMARY_HPP

#include <cmath>
#include <vector>

template<typename T>
struct StatisticalSummary
{
  private:
    static std::pair<size_t,size_t>
    medianIndices(size_t count)
    {
        if (count % 2 == 0) return { (count/2) - 1, count/2 };
        return { count/2, count/2 };
    }

    template<typename V>
    static double toDouble(const V& val)
    { return static_cast<double>(val); }

  public:
    template<typename V>
    StatisticalSummary(const std::vector<V>& values)
     : StatisticalSummary(std::vector<V>(values))
    {}

    template<typename V>
    StatisticalSummary(std::vector<V>&& values)
    {
        std::pair<size_t,size_t> medianPair, lower, upper;

        size_t half = values.size()/2;

        if (values.empty()) {
            throw std::domain_error("Empty vector has no summary!");
        } else if (values.size() > 1) {
            medianPair = medianIndices(values.size());
            lower = medianIndices(half);
            upper = lower;

            upper.first += half + (values.size() % 2);
            upper.second += half + (values.size() % 2);
        } else {
            medianPair = lower = upper = {0, 0};
        }

        std::sort(values.begin(), values.end());

        double total = 0;
        double M = 0.0;
        double S = 0.0;
        int k = 1;
        for (auto val : values) {
            double v = toDouble<V>(val);
            total += v;
            double tmpM = M;
            M += (v - tmpM) / k;
            S += (v - tmpM) * (v - M);
            k++;
        }

        min = values.front();
        lowerQuantile = (values[lower.first] + values[lower.second])/2.0;
        median = (values[medianPair.first] + values[medianPair.second])/2.0;
        mean = total / values.size();
        upperQuantile = (values[upper.first] + values[upper.second])/2.0;
        max = values.back();
        if (k-2 == 0) stdDev = 0.0;
        else stdDev = sqrt(S / (k-2));
    }

    T min;
    T lowerQuantile;
    T median;
    T mean;
    T upperQuantile;
    T max;
    T stdDev;
};
#endif
