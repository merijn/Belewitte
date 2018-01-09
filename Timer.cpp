#include <algorithm>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>

#include "Timer.hpp"

template<> template<>
double
StatisticalSummary<TimerRegister::Timing>::toDouble
(const TimerRegister::nanoseconds& v)
{ return static_cast<double>(v.count()); }

namespace TimerRegister {
using namespace std;

    struct timer_state {
        timer_state(string timer_name, size_t count)
            : name(timer_name)
            , timings(make_shared<vector<nanoseconds>>())
        { timings->reserve(count);}

        timer_state(const timer_state& other)
            : name(other.name), timings(other.timings)
        {}

        string name;
        shared_ptr<vector<nanoseconds>> timings;
    };

struct EpochState {
    string name;
    bool printed;
    vector<timer_state> timers;

    EpochState(const string &id)
        : name(id), printed(false)
    {}
};

Timing::Timing() : Timing::Timing(nanoseconds(0))
{}

Timing::Timing(nanoseconds time)
{
    total_time = duration_cast<nanoseconds>(time).count();

    hrs = duration_cast<hours>(time).count();
    time -= duration_cast<hours>(time);

    mins = duration_cast<minutes>(time).count();
    time -= duration_cast<minutes>(time);

    secs = duration_cast<seconds>(time).count();
    time -= duration_cast<seconds>(time);

    msecs = duration_cast<milliseconds>(time).count();
    time -= duration_cast<milliseconds>(time);

    usecs = duration_cast<microseconds>(time).count();
    time -= duration_cast<microseconds>(time);

    nsecs = duration_cast<nanoseconds>(time).count();
}

Timing::operator nanoseconds() const
{ return nanoseconds(total_time); }

Timing::operator string() const
{
    stringstream result;
    if (hrs) result << setw(3) << hrs << " h   ";
    if (mins) result << setw(3) << mins << " m   ";
    if (secs) result << setw(3) << secs << " s   ";
    if (msecs) result << setw(3) << msecs << " ms   ";
    if (usecs) result << setw(3) << usecs << " us   ";
    if (nsecs != 0.0) result << setw(3) << nsecs << " ns";

    return result.str();
}

Timing& Timing::operator=(const double &time)
{ return operator=(Timing(nanoseconds(time))); }

vector<string>
Timing::align_timings(const vector<pair<string,Timing>> &data)
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
        labelLen = max(elem.first.length(), labelLen);
        if (elem.second.hrs) hrs = true;
        if (elem.second.mins) mins = true;
        if (elem.second.secs) secs = true;
        if (elem.second.msecs) msecs = true;
        if (elem.second.usecs) usecs = true;
        if (elem.second.nsecs != 0.0) nsecs = true;
        labels.emplace_back(elem.first);
        timings.emplace_back(elem.second);
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

Epoch::TimerData::TimerData(const string& id, StatisticalSummary<Timing> m)
    : name(id), results(m)
{}

Epoch::Epoch(const string& id, vector<TimerData>&& timer_data)
    : name(id), data(timer_data)
{}

static vector<EpochState> epochs;

static void reportPrecision(std::ostream& out)
{
    out << "Timer results with precision: " << clock::period::den
        << endl << endl;
}

static shared_ptr<vector<nanoseconds>>
register_timer(string name, size_t count)
{
    auto &timers = epochs.back().timers;
    timers.emplace_back(name, count);
    return timers.back().timings;
}

vector<Epoch> get_epochs()
{
    vector<Epoch> result;
    result.reserve(epochs.size());
    for (auto state : epochs) {
        if (state.timers.empty()) continue;
        vector<Epoch::TimerData> timer_data;
        for (auto data : state.timers) {
            timer_data.emplace_back(data.name, StatisticalSummary<Timing>(*data.timings));
        }
        result.emplace_back(state.name, move(timer_data));
    }

    return result;
}
};

static size_t
registerEpoch(const std::string& name)
{
    size_t result = TimerRegister::epochs.size();
    TimerRegister::epochs.emplace_back(name);
    return result;
}

Epoch::Epoch(bool humanReadable, const std::string& name)
    : direct_printed(false), human_readable(humanReadable)
    , output("/dev/null"), index(registerEpoch(name))
{}

Epoch::Epoch
    ( std::ofstream&& timingFile
    , bool humanReadable
    , const std::string& name)
    : direct_printed(true), human_readable(humanReadable)
    , output(std::move(timingFile)), index(registerEpoch(name))
{}

Epoch::Epoch
    ( std::ofstream&& timingFile
    , const std::string& name
    , bool humanReadable)
    : Epoch(std::move(timingFile), humanReadable, name)
{}

Epoch::Epoch
    ( const std::string& timingFile
    , const std::string& name
    , bool humanReadable)
    : Epoch(std::ofstream(timingFile), humanReadable, name)
{}

Epoch::Epoch
    ( const std::string& timingFile
    , bool humanReadable
    , const std::string& name)
    : Epoch(timingFile, name, humanReadable)
{}

Epoch::~Epoch()
{ if (direct_printed) print_results(output, human_readable); }

void
Epoch::set_output(const std::string& fileName)
{ output = std::ofstream(fileName); }

void
Epoch::set_output(std::ofstream&& fileStream)
{ output = std::move(fileStream); }

void
Epoch::print_results(std::ostream& out, bool humanReadable)
{
    typedef TimerRegister::Timing Timing;

    auto& epoch = TimerRegister::epochs[index];

    if (humanReadable) TimerRegister::reportPrecision(output);

    for (auto &timer : epoch.timers) {
        if (timer.timings->size() != 0) {
            StatisticalSummary<Timing> result(*timer.timings);
            if (!epoch.name.empty() && epoch.name != "") {
                out << epoch.name << ":";
            }

            if (humanReadable) {
                auto times = Timing::align_timings(
                    { { "Min", result.min }
                    , { "Avg", result.mean }
                    , { "Max", result.max }
                    , { "Std", result.stdDev }
                });

                out << timer.name << " (" << timer.timings->size() << "): "
                    << std::endl;

                for (auto &str : times) {
                    out << str << std::endl;
                }
                out << std::endl;
            } else {
                out.imbue(std::locale("C"));
                out << timer.name << ":" << result.min.total_time << " "
                    << result.mean.total_time << " "
                    << result.max.total_time << " "
                    << result.stdDev.total_time << std::endl;
            }
        }
    }
}

Timer::Timer(const std::string& name, size_t count)
    : timings(TimerRegister::register_timer(name, count))
{}

void
Timer::start()
{ begin = TimerRegister::clock::now(); }

void
Timer::stop()
{ timings->push_back(TimerRegister::clock::now() - begin); }

void
Timer::reserve(size_t count)
{
    if (timings->capacity() - timings->size() < count) {
        timings->reserve(timings->size() + std::max(count, 100UL));
    }
}
