#ifndef TIMER_HPP
#define TIMER_HPP

#include <chrono>
#include <fstream>
#include <memory>
#include <string>
#include <vector>

#include "utils/StatisticalSummary.hpp"

namespace TimerRegister {
    using namespace std;
    using namespace chrono;
    using clock = high_resolution_clock;
    using nanoseconds = duration<double, nano>;

    struct Timing {
        Timing();
        Timing(nanoseconds time);
        explicit operator nanoseconds() const;
        explicit operator string() const;
        Timing& operator=(const double &time);

        static vector<string>
        align_timings(const vector<pair<string,Timing>> &data);

        hours::rep hrs;
        minutes::rep mins;
        seconds::rep secs;
        milliseconds::rep msecs;
        microseconds::rep usecs;
        nanoseconds::rep nsecs;
        nanoseconds::rep total_time;
    };

    struct Epoch {
        struct TimerData {
            TimerData(const string& name, const StatisticalSummary<Timing>&);
            string name;
            StatisticalSummary<Timing> results;
        };

        Epoch(const string& name, vector<TimerData>&&);

        string name;
        vector<TimerData> data;
    };

    vector<Epoch> get_epochs();
};

class Epoch
{
    public:
        Epoch() = delete;
        Epoch(const Epoch&) = delete;
        Epoch(Epoch&&) = delete;

        Epoch(bool humanReadable = false, const std::string& = std::string());

        Epoch
        ( std::ofstream&&
        , const std::string& = std::string()
        , bool humanReadable = false
        );

        Epoch
        ( std::ofstream&&
        , bool humanReadable = false
        , const std::string& = std::string()
        );

        Epoch
        ( const std::string&
        , bool humanReadable = false
        , const std::string& = std::string()
        );

        Epoch
        ( const std::string&
        , const std::string& = std::string()
        , bool humanReadable = false
        );

        ~Epoch();

        void set_output(const std::string&);
        void set_output(std::ofstream&&);

        void print_results(std::ostream&, bool humanReadable = false);

    private:
        bool direct_printed;
        bool human_readable;
        std::ofstream output;
        size_t index;
};

class Timer
{
    public:
        Timer(const std::string&, size_t = 10);
        Timer(const Timer&) = delete;
        Timer(Timer&&) = default;

        void start();
        void stop();
        void reserve(size_t);

    private:
        TimerRegister::clock::time_point begin;
        std::shared_ptr<std::vector<TimerRegister::nanoseconds>> timings;
};
#endif
