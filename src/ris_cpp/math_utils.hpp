#ifndef MATH_UTILS_HPP
#define MATH_UTILS_HPP

#include <vector>

// Mathematical utility functions for RIS calculations

// Polynomial interpolation - extrapolate to r=0 using points at r^2
double polint(const std::vector<double>& xa, const std::vector<double>& ya, double x, double& dy);

// Simple numerical integration using trapezoidal rule
double quad(const std::vector<double>& integrand, const std::vector<double>& weights);

// Calculate Clebsch-Gordan coefficient for angular momentum coupling
double clrx(int kap1, int k, int kap2);

// Angular coefficient functions for SMS calculations
double sigma_1(int kap1, int k, int kap2);
double sigma_2(int kap1, int k, int kap2);

#endif // MATH_UTILS_HPP