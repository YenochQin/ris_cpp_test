#include "constants.hpp"
#include <iostream>
#include <cmath>

// Physical constants from setcon.f90
const double AINFCM = 0.52917721067e-08;
const double ALFAI = 137.035999139;
const double CCMPS = 2.99792458e10;
const double EESU = 4.803204673e-10;
const double EMEG = 9.10938356e-28;
const double EMEAMU = 5.48579909070e-04;
const double EMPAMU_CONST = 1.007276466879;
const double HBARES = 1.054571800e-27;
const double RINFEV = 13.605693009;
const double RINFK = 109737.31568508;

// Global variables to hold calculated values
double EMPAM;
double RBCM;
double CVAC;
double AUMAMU;
double AUCM;
double AUEV;
double CCMS;
double FASI;
double FBSI;
double FMTOAU;
double PI;

// Debug flag
extern bool LDBPG[3];

void setcon() {
    EMPAM = EMPAMU_CONST;
    RBCM = AINFCM;

    CVAC = ALFAI;
    AUMAMU = EMEAMU;

    AUCM = 2.0 * RINFK;
    AUEV = 2.0 * RINFEV;
    CCMS = CCMPS;
    FASI = (EMEG / HBARES) * std::pow((EESU * EESU / HBARES), 2);
    FBSI = (10.0 * std::pow(AINFCM, 3) / HBARES) * FASI;

    FMTOAU = 1.0e-13 / AINFCM;

    PI = 4.0 * std::atan(1.0);

    if (LDBPG[1]) {
        std::cout << "From SUBROUTINE SETCON:" << std::endl;
        std::cout << " AINFCM (Bohr radius in cm): " << AINFCM << "," << std::endl;
        std::cout << " ALFAI (Inverse of the fine-structure constant): " << ALFAI << "," << std::endl;
        std::cout << " CCMPS (Speed of light in cm/s): " << CCMPS << "," << std::endl;
        std::cout << " EESU (Electron charge in esu): " << EESU << "," << std::endl;
        std::cout << " EMEG (Electron mass in g): " << EMEG << "," << std::endl;
        std::cout << " EMEAMU (Electron mass in u): " << EMEAMU << "," << std::endl;
        std::cout << " EMPAMU (Proton mass in u): " << EMPAMU_CONST << "," << std::endl;
        std::cout << " HBARES (Rationalized Planck constant in erg s): " << HBARES << "," << std::endl;
        std::cout << " RINFEV (Rydberg in eV): " << RINFEV << "," << std::endl;
        std::cout << " RINFK (Rydberg in Kaysers): " << RINFK << "." << std::endl;
    }
}
