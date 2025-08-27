#include "csl.hpp"
#include <iostream>
#include <fstream>
#include <string>

void lodcsl(int& ncore, std::ifstream& csl_file) {
    // To be implemented
}

void setcsla(const std::string& name, int& ncore) {
    std::string filename = name + ".c";

    std::ifstream csl_file(filename);
    if (!csl_file.is_open()) {
        std::cerr << "Error when opening " << filename << std::endl;
        exit(1);
    }

    std::string record;
    std::getline(csl_file, record);

    if (record.substr(0, 15) != "Core subshells:") {
        std::cerr << "Not a Configuration Symmetry List File;" << std::endl;
        csl_file.close();
        exit(1);
    }

    lodcsl(ncore, csl_file);

    csl_file.close();
}
