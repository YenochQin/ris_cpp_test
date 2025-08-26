#ifndef RIS_OUTPUT_HPP
#define RIS_OUTPUT_HPP

#include <string>
#include "mixblock.hpp"
#include "ris_cal.hpp"

// Helper functions for output formatting
std::string get_j_label(int iatjpo);
std::string get_parity_label(int iaspar);
void write_fortran_style_output(RisCalData& data, const MixingData& mixing_data);

#endif // RIS_OUTPUT_HPP