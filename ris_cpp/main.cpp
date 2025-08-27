#include <iostream>
#include <string>
#include <vector>
#include <limits>
#include "io.hpp"
#include "state.hpp"
#include "debug.hpp"
#include "summary.hpp"
#include "constants.hpp"
#include "machine.hpp"
#include "facts.hpp"
#include "smd.hpp"
#include "csl.hpp"
#include "mixblock.hpp"
#include "ris_cal.hpp"


int main() {
    std::cout << "RIS: Execution begins ..." << std::endl;
    std::cout << std::endl;
    std::cout << "Default settings?" << std::endl;
    NDEF = 1;
    if (getyn()) {
        NDEF = 0;
    }
    std::cout << std::endl;

    std::string name;
    while (true) {
        std::cout << "Name of state" << std::endl;
        std::getline(std::cin, name);
        if (name.length() > 0 && name[0] != ' ') {
            break;
        }
        std::cout << "Names may not start with a blank" << std::endl;
    }

    std::cout << std::endl;
    std::cout << "Mixing coefficients from a CI calc.?" << std::endl;
    int nci = 1;
    if (getyn()) {
        nci = 0;
    }
    std::cout << std::endl;


    setdbg();
    setmc();
    setcon();
    setsum(name, nci);
    int ncore_not_used = 0;
    setcsla(name, ncore_not_used);
    getsmd(name);
    
    // Get mixing coefficients
    MixingData mixing_data;
    if (!getmixblock(name, nci, mixing_data)) {
        std::cerr << "Failed to read mixing coefficients" << std::endl;
        return 1;
    }
    
    factt();
    
    // Perform RIS calculation
    if (!ris_cal(name, mixing_data)) {
        std::cerr << "RIS calculation failed" << std::endl;
        return 1;
    }

    std::cout << "RIS: Execution complete." << std::endl;

    return 0;
}
