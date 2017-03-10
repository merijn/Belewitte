#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>

#include "Timer.hpp"

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

Timing& Timing::operator+=(const nanoseconds &time)
{
    *this = Timing(nanoseconds(total_time) + time);
    return *this;
}

Timing& Timing::operator*=(const double &time)
{
    *this = Timing(nanoseconds(total_time) * time);
    return *this;
}

Timing& Timing::operator/=(const double &time)
{
    *this = Timing(nanoseconds(total_time) / time);
    return *this;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
bool Timing::operator==(const Timing &time) const
{ return total_time == time.total_time; }
#pragma GCC diagnostic pop

bool Timing::operator!=(const Timing &time) const
{ return !this->operator==(time); }

bool Timing::operator<(const Timing &time) const
{ return total_time < time.total_time; }

bool Timing::operator<=(const Timing &time) const
{ return total_time <= time.total_time; }

bool Timing::operator>(const Timing &time) const
{ return total_time > time.total_time; }

bool Timing::operator>=(const Timing &time) const
{ return total_time >= time.total_time; }

Measurements::Measurements(const vector<nanoseconds> &timings)
    : min(nanoseconds(0)), avg(nanoseconds(0)), max(nanoseconds(0))
    , std(nanoseconds(0))
{
    min = *min_element(timings.begin(), timings.end());
    max = *max_element(timings.begin(), timings.end());

    for (auto time : timings) {
        avg += time;
    }

    avg /= timings.size();

    for (auto time : timings) {
        nanoseconds diff = time - nanoseconds(avg);
        std += nanoseconds(diff.count() * diff.count());
    }

    if (timings.size() > 1) {
        std *= 1.0 / (timings.size() - 1);
        std = nanoseconds(sqrt(std.total_time));
    } else {
        std = nanoseconds(0);
    }
}

Epoch::TimerData::TimerData(string id, Measurements m)
    : name(id), results(m)
{}

Epoch::Epoch(string id, vector<TimerData>&& timer_data)
    : name(id), data(timer_data)
{}

static vector<EpochState> epochs = { {""} };
static ostream out(cout.rdbuf());
static bool human_readable = true;
static bool direct_printed_epoch = true;

static void reportPrecision()
{
    static bool printed = false;
    if (!printed) {
        out << "Timer results with precision: " << clock::period::den
            << endl << endl;
        printed = true;
    }
}

void
start_epoch(const string &name)
{
    if (direct_printed_epoch && !epochs.empty()) {
        print_results();
    }

    epochs.emplace_back(name);
}

void set_output(string file)
{
    filebuf *fileBuffer = new filebuf();
    fileBuffer->open(file.c_str(), ios::out | ios::app);
    out.rdbuf(fileBuffer);
}

void set_output(ostream &file)
{ out.rdbuf(file.rdbuf()); }

void set_human_readable(bool b)
{ human_readable = b; }

void set_direct_printed(bool b)
{
    if (human_readable) reportPrecision();
    direct_printed_epoch = b;
}

void print_results()
{
    if (human_readable) reportPrecision();

    for (auto &epoch : epochs) {
        if (epoch.printed) continue;

        for (auto &timer : epoch.timers) {
            if (timer.timings->size() != 0) {
                Measurements result(*timer.timings);
                if (human_readable) {
                    auto output = Timing::align_timings(
                        { { "Min", result.min }
                        , { "Avg", result.avg }
                        , { "Max", result.max }
                        , { "Std", result.std }
                    });

                    out << epoch.name << ":" << timer.name << " ("
                        << timer.timings->size() << "): " << endl;
                    for (auto &str : output) {
                        out << str << endl;
                    }
                    out << endl;
                } else {
                    out.imbue(locale("C"));
                    out << epoch.name << ":" << timer.name << ":"
                        << result.min.total_time << " "
                        << result.avg.total_time << " "
                        << result.max.total_time << " "
                        << result.std.total_time << endl;
                }
            }
        }

        epoch.printed = true;
    }
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
            timer_data.emplace_back(data.name, Measurements(*data.timings));
        }
        result.emplace_back(state.name, move(timer_data));
    }

    return result;
}
};

Timer::Timer(std::string name, size_t count)
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
