#include "machine.hpp"
#include <iostream>
#include <limits>
#include <cmath>

// Machine-dependent parameters
double TENMAX;
double EXPMAX;
double EXPMIN;
double PRECIS;

// Debug flag
extern bool LDBPG[3];

void setmc() {
    TENMAX = std::numeric_limits<double>::max_exponent10;
    EXPMAX = std::log(std::numeric_limits<double>::max());
    EXPMIN = std::log(std::numeric_limits<double>::min());
    PRECIS = std::numeric_limits<double>::epsilon();

    if (LDBPG[0]) {
        std::cout << "From SUBROUTINE SETMC:" << std::endl;
        std::cout << " TENMAX (maximum exponent of 10): " << TENMAX << std::endl;
        std::cout << " EXPMAX (maximum exponent of e): " << EXPMAX << std::endl;
        std::cout << " EXPMIN (minimum exponent of e): " << EXPMIN << std::endl;
        std::cout << " PRECIS (machine precision): " << PRECIS << std::endl;
    }
}
