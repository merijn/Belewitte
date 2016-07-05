#ifndef __TIMER_HPP__
#define __TIMER_HPP__

#include <chrono>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

class Timer;

class TimerRegister {
    friend Timer;
    public:
        using clock = std::chrono::high_resolution_clock;
        using nanoseconds = std::chrono::duration<double, std::nano>;

        static void print_results(std::ostream&, bool);

    private:
        TimerRegister();
        ~TimerRegister();

        static TimerRegister& get();

        void _print_results(std::ostream&, bool);

        struct timer_state {
            timer_state(std::string timer_name, size_t count)
                : name(timer_name)
                , timings(std::make_shared<std::vector<nanoseconds>>())
            { timings->reserve(count);}

            timer_state(const timer_state& other)
                : name(other.name), timings(other.timings)
            {}

            std::string name;
            std::shared_ptr<std::vector<nanoseconds>> timings;
        };

        std::shared_ptr<std::vector<nanoseconds>> register_timer(std::string, size_t);

        struct measurements timer_stats(timer_state &timer);

        std::vector<timer_state> timers;
};

class Timer {
    friend TimerRegister;

    public:
        using clock = TimerRegister::clock;
        using nanoseconds = TimerRegister::nanoseconds;

        Timer(std::string, size_t = 10);

        void start();
        void stop();

    private:
        clock::time_point begin;
        std::shared_ptr<std::vector<nanoseconds>> timings;
};
#endif
