#include "smd.hpp"
#include "io.hpp"
#include "state.hpp"
#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>

// Stubs for called subroutines
void setiso(const std::string& name) {}
void setqic() {}
void radgrd() {}
void nucpot() {}
void setrwfa(const std::string& name) {}

// Global variables from other modules
extern double CVAC;
#include "def.hpp"

// Local variables
double C;
bool LFORDR;
int ICCUT;
double RNT;
double H;
double HP;
int N;
double ACCY;

void getsmd(const std::string& name) {
    setiso("isodata");

    if (NDEF != 0) {
        std::cout << "The physical speed of light in" << std::endl;
        std::cout << " atomic units is " << CVAC << ";" << std::endl;
        std::cout << " revise this value?" << std::endl;
        if (getyn()) {
            std::cout << "Enter the revised value:" << std::endl;
            std::cin >> C;
        } else {
            C = CVAC;
        }
    } else {
        C = CVAC;
    }

    if (NDEF != 0) {
        std::cout << "Treat contributions of some CSFs" << std::endl;
        std::cout << " as first-order perturbations?" << std::endl;
        if (getyn()) {
            LFORDR = true;
            std::cout << "The contribution of CSFs" << std::endl;
            std::cout << " 1 -- ICCUT will be treated" << std::endl;
            std::cout << " variationally; the remainder" << std::endl;
            std::cout << " perturbatively; enter ICCUT:" << std::endl;
            std::cin >> ICCUT;
        } else {
            LFORDR = false;
            ICCUT = 0;
        }
    } else {
        LFORDR = false;
        ICCUT = 0;
    }

    if (NPARM == 0) {
        RNT = std::exp((-65.0 / 16.0)) / Z;
        H = std::pow(0.5, 4);
        N = std::min(220, NNNP);
    } else {
        RNT = 2.0e-06 / Z;
        H = 5.0e-02;
        N = NNNP;
    }
    HP = 0.0;
    if (NDEF != 0) {
        std::cout << "The default radial grid parameters" << std::endl;
        std::cout << " for this case are:" << std::endl;
        std::cout << " RNT = " << RNT << ";" << std::endl;
        std::cout << " H = " << H << ";" << std::endl;
        std::cout << " HP = " << HP << ";" << std::endl;
        std::cout << " N = " << N << ";" << std::endl;
        std::cout << " revise these values?" << std::endl;
        if (getyn()) {
            std::cout << "Enter RNT:" << std::endl;
            std::cin >> RNT;
            std::cout << "Enter H:" << std::endl;
            std::cin >> H;
            std::cout << "Enter HP:" << std::endl;
            std::cin >> HP;
            std::cout << "Enter N:" << std::endl;
            std::cin >> N;
        }
    }

    ACCY = std::pow(H, 6);

    setqic();
    radgrd();
    nucpot();
    setrwfa(name + ".w");
}

