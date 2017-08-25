#ifndef TIMER_HPP
#define TIMER_HPP

#include <chrono>
#include <cmath>
#include <iomanip>
#include <ios>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

namespace TimerRegister {
    using namespace std;
    using namespace chrono;
    using clock = high_resolution_clock;
    using nanoseconds = duration<double, nano>;

    struct Timing {
        Timing(nanoseconds time);
        explicit operator nanoseconds() const;
        explicit operator string() const;
        Timing& operator+=(const nanoseconds &time);
        Timing& operator*=(const double &time);
        Timing& operator/=(const double &time);

        bool operator==(const Timing &time) const;
        bool operator!=(const Timing &time) const;
        bool operator<(const Timing &time) const;
        bool operator<=(const Timing &time) const;
        bool operator>(const Timing &time) const;
        bool operator>=(const Timing &time) const;

        template<typename T>
        using Convert = function<pair<string,Timing>(const T&)>;

        template<typename T = pair<string,Timing>>
        static vector<string>
        align_timings(const vector<T> &data,
                      Convert<T> fun = [](auto &p) { return p; })
        {
            size_t labelLen = 0;
            bool hrs = false, mins = false, secs = false, msecs = false,
                usecs = false, nsecs = false;

            vector<string> labels;
            vector<Timing> timings;
            vector<stringstream> outputs(data.size());

            labels.reserve(data.size());
            timings.reserve(data.size());

            for (auto &elem : data) {
                auto result = fun(elem);
                labelLen = max(result.first.length(), labelLen);
                if (result.second.hrs) hrs = true;
                if (result.second.mins) mins = true;
                if (result.second.secs) secs = true;
                if (result.second.msecs) msecs = true;
                if (result.second.usecs) usecs = true;
                if (result.second.nsecs != 0.0) nsecs = true;
                labels.emplace_back(result.first);
                timings.emplace_back(result.second);
            }

            for (size_t i = 0; i < labels.size(); i++) {
                string label = labels[i] + ":";
                outputs[i] << left << setfill(' ')
                           << setw(static_cast<int>(labelLen + 1)) << label
                           << right;
            }

            labels.clear();
            labels.shrink_to_fit();

            for (size_t i = 0; hrs && i < timings.size(); i++) {
                outputs[i] << setw(3) << timings[i].hrs << " h";
            }

            for (size_t i = 0; mins && i < timings.size(); i++) {
                outputs[i] << "   " << setw(2) << timings[i].mins << " m";
            }

            for (size_t i = 0; secs && i < timings.size(); i++) {
                outputs[i] << "   " << setw(3) << timings[i].secs << " s";
            }

            for (size_t i = 0; msecs && i < timings.size(); i++) {
                outputs[i] << "   " << setw(3) << timings[i].msecs << " ms";
            }

            for (size_t i = 0; usecs && i < timings.size(); i++) {
                outputs[i] << "   " << setw(3) << timings[i].usecs << " us";
            }

            for (size_t i = 0; nsecs && i < timings.size(); i++) {
                outputs[i] << "   " << setw(3) << lrint(timings[i].nsecs)
                           << " ns";
            }

            timings.clear();
            timings.shrink_to_fit();

            vector<string> result;
            result.reserve(outputs.size());
            for (auto &stream : outputs) {
                result.emplace_back(stream.str());
            }

            return result;
        }

        hours::rep hrs;
        minutes::rep mins;
        seconds::rep secs;
        milliseconds::rep msecs;
        microseconds::rep usecs;
        nanoseconds::rep nsecs;
        nanoseconds::rep total_time;
    };

    struct Measurements {
        Measurements(const vector<nanoseconds> &timings);
        Timing min, avg, max, std;
    };

    struct Epoch {
        struct TimerData {
            TimerData(string name, Measurements results);
            string name;
            Measurements results;
        };

        Epoch(string name, vector<TimerData>&&);

        string name;
        vector<TimerData> data;
    };

    void start_epoch(const string&);
    void set_output(string file);
    void set_output(ostream &output);
    void set_human_readable(bool b);
    void set_direct_printed(bool b);
    void print_results();

    vector<Epoch> get_epochs();
};

class Timer {
    public:
        Timer(std::string, size_t = 10);

        void start();
        void stop();
        void reserve(size_t);

    private:
        TimerRegister::clock::time_point begin;
        std::shared_ptr<std::vector<TimerRegister::nanoseconds>> timings;
};
#endif
