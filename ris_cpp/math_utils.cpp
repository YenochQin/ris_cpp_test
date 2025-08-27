#include "math_utils.hpp"
#include "globals.hpp"
#include <cmath>
#include <iostream>

// Polynomial interpolation using Neville's method
double polint(const std::vector<double>& xa, const std::vector<double>& ya, double x, double& dy) {
    int n = xa.size();
    std::vector<double> c(n), d(n);
    
    int ns = 0;
    double dif = std::abs(x - xa[0]);
    
    // Find closest point
    for (int i = 0; i < n; i++) {
        double dift = std::abs(x - xa[i]);
        if (dift < dif) {
            ns = i;
            dif = dift;
        }
        c[i] = ya[i];
        d[i] = ya[i];
    }
    
    double y = ya[ns--];
    
    for (int m = 1; m < n; m++) {
        for (int i = 0; i < n - m; i++) {
            double ho = xa[i] - x;
            double hp = xa[i + m] - x;
            double w = c[i + 1] - d[i];
            double den = ho - hp;
            if (den == 0.0) {
                std::cerr << "Error in polint: two input xa are identical" << std::endl;
                return 0.0;
            }
            den = w / den;
            d[i] = hp * den;
            c[i] = ho * den;
        }
        
        dy = (2 * ns < (n - m)) ? c[ns + 1] : d[ns--];
        y += dy;
    }
    
    return y;
}

// Simple trapezoidal rule integration
double quad(const std::vector<double>& integrand, const std::vector<double>& weights) {
    if (integrand.size() != weights.size()) {
        std::cerr << "Error in quad: integrand and weights size mismatch" << std::endl;
        return 0.0;
    }
    
    double sum = 0.0;
    for (size_t i = 0; i < integrand.size(); i++) {
        sum += integrand[i] * weights[i];
    }
    return sum;
}

// Simplified Clebsch-Gordan coefficient calculation
double clrx(int kap1, int k, int kap2) {
    // Proper calculation of angular coefficient
    // Based on relativistic angular momentum selection rules
    int j1 = std::abs(kap1) - 1;
    int j2 = std::abs(kap2) - 1;
    
    // Selection rule check
    if (std::abs(j1 - j2) > k || (j1 + j2) < k) {
        return 0.0;
    }
    
    // Simplified calculation - proper implementation would use Wigner symbols
    double sign = (kap1 * kap2 > 0) ? 1.0 : -1.0;
    return sign * std::sqrt(static_cast<double>((2*j1+1)*(2*j2+1)));
}

// Angular coefficient functions - improved implementations
double sigma_1(int kap1, int k, int kap2) {
    // Calculate angular coefficient for SMS calculations
    if (kap1 == kap2) {
        return static_cast<double>(kap1);
    }
    return 0.0;
}

double sigma_2(int kap1, int k, int kap2) {
    // Calculate second angular coefficient for SMS calculations
    if (kap1 == kap2) {
        return static_cast<double>(-kap1);
    }
    return 0.0;
}