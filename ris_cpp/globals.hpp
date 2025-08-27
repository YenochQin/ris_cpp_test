#ifndef GLOBALS_HPP
#define GLOBALS_HPP

#include <vector>
#include <fstream>

// Global variables shared across the RIS4 application
// These correspond to Fortran common blocks and module variables

// From parameter definitions
extern int NNNW, NVEC, NCF, NW;

// From various Fortran modules  
extern int NELEC;
extern double EAV, CUTOFF;

// Grid variables (from grid_C module)
extern std::vector<double> PARM, R, RP;  // Radial grid and weights
extern int NRNT;  // Number of grid points

// Orbital variables (from orb_C module)
extern std::vector<int> NAK, NP, NH;    // Quantum numbers

// Wave function arrays (from wave_C module)
extern std::vector<std::vector<double>> PF, QF;  // Large and small components: PF(NRNT,NNNW), QF(NRNT,NNNW)

// Physical constants (from def_C module and constants_C module)
// PI is defined in constants.cpp
// Z is defined in def.cpp
extern double C, CONV;

// Unit conversion constants (defined in constants.cpp and globals.cpp)
// AUMAMU, AUCM, CCMS are defined in constants.cpp
extern double AU2FM;  // Only this one is defined in globals.cpp

// Global output file stream (corresponds to Fortran unit 24)
extern std::ofstream output_file;

#endif // GLOBALS_HPP