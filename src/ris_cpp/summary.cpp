#include "summary.hpp"
#include "globals.hpp"
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

    // Open the global output file stream
    output_file.open(filename);
    if (!output_file.is_open()) {
        std::cerr << "Error when opening " << filename << std::endl;
        exit(1);
    }
    
    std::cout << "Output file " << filename << " opened successfully" << std::endl;
}
