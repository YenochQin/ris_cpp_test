#include "facts.hpp"
#include <vector>
#include <cmath>

const int MFACT = 500;
std::vector<double> GAM(MFACT);

void factt() {
    GAM[0] = 1.0;
    GAM[1] = 1.0;
    double x = 2.0;

    for (int i = 2; i < 30; ++i) {
        GAM[i] = GAM[i-1] * x;
        x = x + 1.0;
    }

    for (int i = 0; i < 30; ++i) {
        GAM[i] = std::log(GAM[i]);
    }

    x = 30.0;

    for (int i = 30; i < MFACT; ++i) {
        GAM[i] = GAM[i-1] + std::log(x);
        x = x + 1.0;
    }
}
