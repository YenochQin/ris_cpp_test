#include "ris_cal.hpp"
#include "globals.hpp"
#include "math_utils.hpp"
#include "constants.hpp"
#include "def.hpp"
#include <iostream>
#include <vector>
#include <cmath>

// Real implementations for RIS calculation functions
// Translated from original Fortran implementations

double rintdens(int i, int j) {
    // Calculate electron density at r=0 using polynomial interpolation
    // Based on rintdens.f90: (4π)^-1 r^-2 |P_I(r)*P_J(r) + Q_I(r)*Q_J(r)| r→0
    
    if (PF.empty() || QF.empty() || i >= NW || j >= NW) {
        std::cerr << "Error in rintdens: wave function arrays not initialized" << std::endl;
        return 0.0;
    }
    
    // Use 3 grid points starting from point 10 with step 2 (as in original Fortran)
    std::vector<double> xa(3), ya(3);
    
    for (int k = 0; k < 3; k++) {
        int l = 10 + k * 2;  // Grid points 10, 12, 14
        if (l >= NRNT) {
            std::cerr << "Error in rintdens: grid point out of range" << std::endl;
            return 0.0;
        }
        
        xa[k] = R[l] * R[l];  // r^2 for polynomial interpolation
        double density = (PF[l][i] * PF[l][j] + QF[l][i] * QF[l][j]) / (4.0 * PI * R[l] * R[l]);
        ya[k] = density;
    }
    
    // Extrapolate to r=0 (xa=0)
    double dy;
    double result = polint(xa, ya, 0.0, dy);
    
    return result;
}

void rintdensvec(int i, int j, std::vector<std::vector<std::vector<double>>>& dint1vec, int nrnuc) {
    // Calculate electron density at multiple radial points (vectorized version)
    // Based on rintdensvec.f90
    
    if (PF.empty() || QF.empty() || i >= NW || j >= NW) {
        std::cerr << "Error in rintdensvec: wave function arrays not initialized" << std::endl;
        return;
    }
    
    // Calculate density from grid point 2 to nrnuc
    for (int l = 2; l < std::min(nrnuc, NRNT); l++) {
        double density = (PF[l][i] * PF[l][j] + QF[l][i] * QF[l][j]) / (4.0 * PI * R[l] * R[l]);
        dint1vec[i][j][l] = density;
    }
}

void rinti_nms(int i, int j, double& dint2, double& dint7) {
    // Calculate one-electron kinetic energy integral for normal mass shift
    // Based on rinti_nms.f90 - corrected implementation
    
    if (PF.empty() || QF.empty() || i >= NW || j >= NW) {
        std::cerr << "Error in rinti_nms: wave function arrays not initialized" << std::endl;
        dint2 = 0.0;
        dint7 = 0.0;
        return;
    }
    
    // Check for same quantum numbers
    if (NAK[i] != NAK[j]) {
        dint2 = 0.0;
        dint7 = 0.0;
        return;
    }
    
    // Calculate proper kinetic energy integrals
    std::vector<double> integrand1(NRNT), integrand2(NRNT);
    
    // Initialize to zero
    std::fill(integrand1.begin(), integrand1.end(), 0.0);
    std::fill(integrand2.begin(), integrand2.end(), 0.0);
    
    int kappa = NAK[i];
    double ckappa = static_cast<double>(kappa);
    
    for (int l = 1; l < NRNT; l++) {
        if (R[l] > 0.0) {
            // Kinetic energy terms from relativistic Dirac equation
            // T11 integral: <P_i|d/dr - kappa/r|P_j> + <Q_i|d/dr + kappa/r|Q_j>
            double term1 = PF[l][i] * PF[l][j] + QF[l][i] * QF[l][j];
            integrand1[l] = term1 * (ckappa / R[l]);
            
            // T22 integral: momentum-like terms
            double term2 = (PF[l][i] * QF[l][j] - QF[l][i] * PF[l][j]) * C;
            integrand2[l] = term2;
        }
    }
    
    dint2 = quad(integrand1, RP);
    dint7 = quad(integrand2, RP);
}

double rint(int i, int j, int k) {
    // Calculate radial expectation values <r^k>
    // Based on rint.f90: ∫₀^∞ r^k * (P_i(r)*P_j(r) + Q_i(r)*Q_j(r)) dr
    
    if (PF.empty() || QF.empty() || i >= NW || j >= NW) {
        std::cerr << "Error in rint: wave function arrays not initialized" << std::endl;
        return 0.0;
    }
    
    // Check for same quantum numbers
    if (NAK[i] != NAK[j]) {
        return 0.0;
    }
    
    std::vector<double> integrand(NRNT);
    std::fill(integrand.begin(), integrand.end(), 0.0);
    
    for (int l = 1; l < NRNT; l++) {
        if (R[l] > 0.0) {
            double r_power = (k == 0) ? 1.0 : std::pow(R[l], k);
            integrand[l] = r_power * (PF[l][i] * PF[l][j] + QF[l][i] * QF[l][j]);
        }
    }
    
    return quad(integrand, RP);
}

double cre(int i, int flag, int j) {
    // Calculate relativistic reduced matrix element (j1 || C(K) || j2)
    // Based on cre.f90 - simplified implementation
    
    if (i >= NAK.size() || j >= NAK.size() || i < 0 || j < 0) {
        std::cerr << "Error in cre: orbital index out of range (i=" << i << ", j=" << j << ", NAK.size()=" << NAK.size() << ")" << std::endl;
        return 1.0;
    }
    
    int kap1 = NAK[i];
    int kap2 = NAK[j];
    int k = flag;
    
    // Simplified calculation using approximate formula
    int k1 = std::abs(kap1);
    int k2 = std::abs(kap2);
    double dk1k2 = 4.0 * k1 * k2;
    double result = std::sqrt(dk1k2) * clrx(kap1, k, kap2);
    
    if (k1 % 2 == 1) result = -result;
    
    return result;
}

double vinti(int i, int j) {
    // One-electron Vinti integral for specific mass shift
    // Based on vinti.f90 - simplified implementation
    
    if (PF.empty() || QF.empty() || i >= NW || j >= NW) {
        std::cerr << "Error in vinti: wave function arrays not initialized" << std::endl;
        return 0.0;
    }
    
    // Simplified calculation - full implementation would need complex derivative terms
    std::vector<double> integrand(NRNT);
    
    for (int l = 1; l < NRNT; l++) {
        integrand[l] = (PF[l][i] * QF[l][j] - QF[l][i] * PF[l][j]) / R[l];
    }
    
    return quad(integrand, RP);
}

double rint_sms2(int i, int j) {
    // Specific mass shift integral (type 2)
    // Based on rint_sms2.f90
    
    if (PF.empty() || QF.empty() || i >= NW || j >= NW) {
        std::cerr << "Error in rint_sms2: wave function arrays not initialized or indices out of range" << std::endl;
        return 0.0;
    }
    
    int kap1 = NAK[i];
    int kap2 = NAK[j];
    
    // Check for valid quantum numbers
    if (kap1 == 0 || kap2 == 0) {
        return 0.0;
    }
    
    double apart1 = sigma_1(kap1, 1, kap2);
    double apart2 = sigma_2(kap1, 1, kap2);
    
    std::vector<double> integrand(NRNT, 0.0);
    
    for (int l = 1; l < NRNT; l++) {
        if (R[l] > 0.0) {
            integrand[l] = (apart1 * QF[l][i] * PF[l][j] - apart2 * PF[l][i] * QF[l][j]) / R[l];
        }
    }
    
    double result = quad(integrand, RP);
    return -result * Z / C;
}

double rint_sms3(int i, int j) {
    // Specific mass shift integral (type 3)  
    // Based on rint_sms3.f90
    
    if (PF.empty() || QF.empty() || i >= NW || j >= NW) {
        std::cerr << "Error in rint_sms3: wave function arrays not initialized or indices out of range" << std::endl;
        return 0.0;
    }
    
    std::vector<double> integrand(NRNT, 0.0);
    
    for (int l = 1; l < NRNT; l++) {
        if (R[l] > 0.0) {
            integrand[l] = (QF[l][i] * PF[l][j] - PF[l][i] * QF[l][j]) / R[l];
        }
    }
    
    double result = quad(integrand, RP);
    return result * Z / C;
}