#include <algorithm>
#include <cmath>
#include <fstream>
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

void TimerRegister::start_epoch(const std::string& s)
{ get()._start_epoch(s); }

void TimerRegister::set_output(std::string file, bool readable)
{ get()._set_output(file, readable); }

void TimerRegister::finalise()
{ get().print_results(); }

TimerRegister::TimerRegister() : out(nullptr)
{ out.rdbuf(std::cout.rdbuf()); }

TimerRegister::~TimerRegister()
{}

std::shared_ptr<std::vector<TimerRegister::nanoseconds>>
TimerRegister::register_timer(std::string name, size_t count)
{
    std::string prefix = "";
    if (!epoch_name.empty()) prefix = epoch_name + ":";
    timers.emplace_back(prefix + name, count);
    return timers.back().timings;
}

struct time {
    hours::rep hrs;
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

    result.hrs = duration_cast<hours>(time).count();
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
    auto timings = *timer.timings;
    nanoseconds min, max, avg = nanoseconds(0), std = nanoseconds(0);

    min = *std::min_element(timings.begin(), timings.end());
    max = *std::max_element(timings.begin(), timings.end());

    for (auto time : timings) {
        avg += time;
    }

    avg /= timings.size();

    for (auto time : timings) {
        nanoseconds diff = time - avg;
        std += nanoseconds(diff.count() * diff.count());
    }

    if (timings.size() > 1) {
        std *= 1.0 / (timings.size() - 1);
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

    if (time.min.hrs || time.avg.hrs || time.max.hrs || time.std.hrs) {
        min << std::setw(3) << time.min.hrs << " h   ";
        avg << std::setw(3) << time.avg.hrs << " h   ";
        max << std::setw(3) << time.max.hrs << " h   ";
        std << std::setw(3) << time.std.hrs << " h   ";
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
TimerRegister::print_results()
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

void
TimerRegister::_start_epoch(const std::string &name)
{
    epoch_name = name;
    print_results();
}

void TimerRegister::_set_output(std::string file, bool readable)
{
    human_readable = readable;
    if (!file.empty()) {
        std::filebuf *fileBuffer = new std::filebuf();
        fileBuffer->open(file.c_str(), std::ios::out | std::ios::app);
        out.rdbuf(fileBuffer);
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
