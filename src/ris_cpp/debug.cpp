#include "debug.hpp"
#include "io.hpp"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

// Global debug flags (for now)
bool LDBPA[3];
bool LDBPG[3];
bool LDBPR[4];
#include "state.hpp"

void setdbg() {
    for (int i = 0; i < 3; ++i) LDBPA[i] = false;
    for (int i = 0; i < 3; ++i) LDBPG[i] = false;
    for (int i = 0; i < 4; ++i) LDBPR[i] = false;

    if (NDEF == 0) return;

    std::cout << "Generate debug printout?" << std::endl;
    if (getyn()) {
        std::string filename = "rdensity.dbg";
        std::cout << "File  rdensity.dbg  will be created as the" << std::endl;
        std::cout << " rdensity DeBuG Printout File; enter another" << std::endl;
        std::cout << " file name if this is not acceptable;" << std::endl;
        std::cout << " null otherwise:" << std::endl;

        std::string input_filename;
        std::getline(std::cin, input_filename);

        if (!input_filename.empty()) {
            filename = input_filename;
        }

        std::ofstream dbg_file(filename);
        if (!dbg_file.is_open()) {
            std::cerr << "Error opening debug file." << std::endl;
            return;
        }

        std::cout << " Print out the machine constants used?" << std::endl;
        if (getyn()) LDBPG[0] = true;
        std::cout << " Print out the physical constants used?" << std::endl;
        if (getyn()) LDBPG[1] = true;

        std::cout << " Printout from radial modules?" << std::endl;
        if (getyn()) {
            std::cout << " Printout from RADGRD?" << std::endl;
            if (getyn()) LDBPR[0] = true;
            std::cout << " Printout from NUCPOT?" << std::endl;
            if (getyn()) LDBPR[1] = true;
            std::cout << " Printout from LODRWF?" << std::endl;
            if (getyn()) LDBPR[2] = true;
        }

        std::cout << " Printout from angular modules?" << std::endl;
        if (getyn()) {
            std::cout << " Printout from LODCSL?" << std::endl;
            if (getyn()) LDBPA[0] = true;
            std::cout << " Print out V coefficients?" << std::endl;
            if (getyn()) LDBPA[1] = true;
        }
    }
}
