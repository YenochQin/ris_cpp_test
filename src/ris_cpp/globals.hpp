#ifndef GLOBALS_HPP
#define GLOBALS_HPP

#include <vector>

// Global variables shared across the RIS4 application
// These correspond to Fortran common blocks and module variables

// From parameter definitions
extern int NNNW, NVEC, NCF, NW;

// From various Fortran modules  
extern int NELEC;
extern double EAV, CUTOFF;

// Arrays from grid and orbital modules
extern std::vector<double> PARM, R;
extern std::vector<int> NAK;

#endif // GLOBALS_HPP