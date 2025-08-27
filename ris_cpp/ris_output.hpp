#ifndef RIS_OUTPUT_HPP
#define RIS_OUTPUT_HPP

#include <string>
#include <vector>
#include "mixblock.hpp"

// Helper functions for output formatting
std::string get_j_label(int iatjpo);
std::string get_parity_label(int iaspar);

void write_fortran_output(const std::vector<double>& DENS1, 
                         const std::vector<double>& DENS2,
                         const std::vector<double>& DENS7,
                         const std::vector<double>& SMSC1,
                         const std::vector<double>& SMSC2,
                         const MixingData& mixing_data);

#endif // RIS_OUTPUT_HPP