#include <algorithm>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <sstream>

#include "Timer.hpp"

using std::endl;
using namespace std::chrono;

TimerRegister& TimerRegister::get()
{
    static TimerRegister timers;
    return timers;
}

void TimerRegister::print_results(std::ostream& out, bool readable)
{ get()._print_results(out, readable); }

TimerRegister::TimerRegister()
{}

TimerRegister::~TimerRegister()
{}

std::shared_ptr<std::vector<TimerRegister::nanoseconds>>
TimerRegister::register_timer(std::string name, size_t count)
{
    timers.emplace_back(name, count);
    return timers.back().timings;
}

struct time {
    hours::rep hours;
    minutes::rep mins;
    seconds::rep secs;
    milliseconds::rep msecs;
    microseconds::rep usecs;
    nanoseconds::rep nsecs;
    nanoseconds::rep total_time;
};

static struct time
split_time(TimerRegister::nanoseconds time)
{
    struct time result;

    result.total_time = duration_cast<nanoseconds>(time).count();

    result.hours = duration_cast<hours>(time).count();
    time -= duration_cast<hours>(time);

    result.mins = duration_cast<minutes>(time).count();
    time -= duration_cast<minutes>(time);

    result.secs = duration_cast<seconds>(time).count();
    time -= duration_cast<seconds>(time);

    result.msecs = duration_cast<milliseconds>(time).count();
    time -= duration_cast<milliseconds>(time);

    result.usecs = duration_cast<microseconds>(time).count();
    time -= duration_cast<microseconds>(time);

    result.nsecs = duration_cast<nanoseconds>(time).count();
    time -= duration_cast<nanoseconds>(time);

    return result;
}

struct measurements {
    struct time min, avg, max, std;
};

measurements
TimerRegister::timer_stats(timer_state &timer)
{
    nanoseconds min, max, avg = nanoseconds(0), std = nanoseconds(0);

    min = *std::min_element(timer.timings->begin(), timer.timings->end());
    max = *std::max_element(timer.timings->begin(), timer.timings->end());

    for (auto time : *timer.timings) {
        avg += time;
    }

    avg /= timer.timings->size();

    for (auto time : *timer.timings) {
        nanoseconds diff = time - avg;
        std += nanoseconds(diff.count() * diff.count());
    }

    if (timer.timings->size() > 1) {
        std *= 1.0 / (timer.timings->size() - 1);
        std = nanoseconds(std::sqrt(std.count()));
    } else {
        std = nanoseconds(0);
    }

    return { split_time(min)
           , split_time(avg)
           , split_time(max)
           , split_time(std)
           };
}

static void
print_aligned_times(std::ostream& out, measurements time)
{
    std::stringstream min, avg, max, std;

    if (time.min.hours || time.avg.hours || time.max.hours || time.std.hours) {
        min << std::setw(3) << time.min.hours << " h   ";
        avg << std::setw(3) << time.avg.hours << " h   ";
        max << std::setw(3) << time.max.hours << " h   ";
        std << std::setw(3) << time.std.hours << " h   ";
    }

    if (time.min.mins || time.avg.mins || time.max.mins || time.std.mins) {
        min << std::setw(2) << time.min.mins << " m   ";
        avg << std::setw(2) << time.avg.mins << " m   ";
        max << std::setw(2) << time.max.mins << " m   ";
        std << std::setw(2) << time.std.mins << " m   ";
    }

    if (time.min.secs || time.avg.secs || time.max.secs || time.std.secs) {
        min << std::setw(3) << time.min.secs << " s   ";
        avg << std::setw(3) << time.avg.secs << " s   ";
        max << std::setw(3) << time.max.secs << " s   ";
        std << std::setw(3) << time.std.secs << " s   ";
    }

    if (time.min.msecs || time.avg.msecs || time.max.msecs || time.std.msecs) {
        min << std::setw(3) << time.min.msecs << " ms   ";
        avg << std::setw(3) << time.avg.msecs << " ms   ";
        max << std::setw(3) << time.max.msecs << " ms   ";
        std << std::setw(3) << time.std.msecs << " ms   ";
    }

    if (time.min.usecs || time.avg.usecs || time.max.usecs || time.std.usecs) {
        min << std::setw(3) << time.min.usecs << " us   ";
        avg << std::setw(3) << time.avg.usecs << " us   ";
        max << std::setw(3) << time.max.usecs << " us   ";
        std << std::setw(3) << time.std.usecs << " us   ";
     }

    if (time.min.nsecs || time.avg.nsecs || time.max.nsecs || time.std.nsecs) {
        min << std::setw(3) << time.min.nsecs << " ns";
        avg << std::setw(3) << time.avg.nsecs << " ns";
        max << std::setw(3) << time.max.nsecs << " ns";
        std << std::setw(3) << time.std.nsecs << " ns";
    }

    out << "Min: " << min.str() << endl;
    out << "Avg: " << avg.str() << endl;
    out << "Max: " << max.str() << endl;
    out << "Std: " << std.str() << endl;
}

void
TimerRegister::_print_results(std::ostream& out, bool human_readable)
{
    if (human_readable) {
        out << "Timer results with precision: " << clock::period::den << endl;
        out << endl;
    }

    for (auto &timer : timers) {
        if (timer.timings->size() != 0) {
            measurements result = timer_stats(timer);
            if (human_readable) {
                out << timer.name << " (" << timer.timings->size() << "): " << endl;
                print_aligned_times(out, result);
                out << endl;
            } else {
                out.imbue(std::locale("C"));
                out << timer.name << ":"
                    << result.min.total_time << " "
                    << result.avg.total_time << " "
                    << result.max.total_time << " "
                    << result.std.total_time << endl;
            }
        }
    }
}

Timer::Timer(std::string name, size_t count)
    : timings(TimerRegister::get().register_timer(name, count))
{}

void
Timer::start()
{ begin = clock::now(); }

void
Timer::stop()
{ timings->push_back(clock::now() - begin); }
