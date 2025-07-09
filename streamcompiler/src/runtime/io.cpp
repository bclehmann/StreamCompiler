#include <iostream>
#include <iomanip>

static bool initialized = false;

extern "C" void print_double_newline(double value) {
    if (!initialized) {
        constexpr auto max_precision{std::numeric_limits<long double>::digits10 + 1}; 

        std::ios_base::sync_with_stdio(false);
        std::cout.tie(nullptr);
        std::cout << std::setprecision(max_precision);
        initialized = true;
    }

    std::cout << value << '\n';
}