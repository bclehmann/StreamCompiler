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

extern "C" void print_4doubles_newline_variable_precision(double v1, double v2, double v3, double v4, int precision) {
    printf("%.*f\n%.*f\n%.*f\n%.*f\n", precision, v1, precision, v2, precision, v3, precision, v4);
}

extern "C" void print_8doubles_newline_variable_precision(double v1, double v2, double v3, double v4, double v5, double v6, double v7, double v8, int precision) {
    printf("%.*f\n%.*f\n%.*f\n%.*f\n%.*f\n%.*f\n%.*f\n%.*f\n", precision, v1, precision, v2, precision, v3, precision, v4, // You may not like it, but this is what peak performance looks like
           precision, v5, precision, v6, precision, v7, precision, v8);
}
#endif