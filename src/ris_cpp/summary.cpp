#include "summary.hpp"
#include <iostream>
#include <fstream>
#include <string>

void setsum(const std::string& name, int nci) {
    std::string filename;
    if (nci == 0) {
        filename = name + ".ci";
    } else {
        filename = name + ".i";
    }

    std::ofstream sum_file(filename);
    if (!sum_file.is_open()) {
        std::cerr << "Error when opening " << filename << std::endl;
        exit(1);
    }
}
