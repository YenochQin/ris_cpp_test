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
#include "csl.hpp"


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
    //   CALL GETSMD(NAME)
    //   CALL GETMIXBLOCK(NAME,NCI)
    //   CALL FACTT
    //   CALL RIS_CAL(NAME)

    std::cout << "RIS: Execution complete." << std::endl;

    return 0;
}
