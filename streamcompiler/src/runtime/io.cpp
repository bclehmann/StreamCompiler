#ifdef PRETTY_OUTPUT

#include <iostream>
#include <iomanip>

static bool initialized = false;

// This implementation gives prettier output, it won't print trailing zeroes but it'll still give you full precision
// However, it is meaningfully slower than printf
extern "C" void print_double_newline(double value) {
    if (!initialized) {
        constexpr auto max_precision{std::numeric_limits<double>::digits10 + 1}; 

        std::ios_base::sync_with_stdio(false);
        std::cout.tie(nullptr);
        std::cout << std::setprecision(max_precision);
        initialized = true;
    }

    std::cout << value << '\n';
}

extern "C" void print_double_newline_variable_precision(double value, int precision) {
    if (!initialized) {
        std::ios_base::sync_with_stdio(false);
        std::cout.tie(nullptr);
        std::cout << std::setprecision(precision);
        initialized = true;
    }

    std::cout << value << '\n';
}

#else
#include <cstdio>

extern "C" void print_double_newline(double value) {
    // TODO: the precision here seems to have a meaningful performance impact :/
    printf("%.17f\n", value); // 17 digits is roughly the max meaningful precision for a double
}

extern "C" void print_double_newline_variable_precision(double value, int precision) {
    printf("%.*f\n", precision, value); // I didn't know about the * format specifier either :)
}
#endif