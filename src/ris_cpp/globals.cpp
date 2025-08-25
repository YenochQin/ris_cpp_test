#include "globals.hpp"

// Global variable definitions
// These would normally come from Fortran modules or common blocks

int NNNW = 100;      // Maximum number of orbitals (from parameter_def)  
int NVEC = 0;        // Number of eigenvectors (set dynamically)
int NCF = 0;         // Number of configuration state functions (set dynamically) 
int NW = 0;          // Number of orbitals (set dynamically)
int NELEC = 0;       // Number of electrons

double EAV = 0.0;    // Average energy
double CUTOFF = 1.0e-10; // Cutoff for matrix elements

// Arrays (would be populated from file input or calculations)
std::vector<double> PARM(10, 0.0);    // Nuclear parameters
std::vector<double> R(1000, 0.0);     // Radial grid
std::vector<int> NAK(100, 0);         // Orbital quantum numbers