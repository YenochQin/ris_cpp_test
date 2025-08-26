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

// Grid variables
std::vector<double> PARM(10, 0.0);         // Nuclear parameters
std::vector<double> R(1000, 0.0);          // Radial grid
std::vector<double> RP(1000, 0.0);         // Radial grid weights
int NRNT = 1000;                           // Number of grid points

// Orbital variables
std::vector<int> NAK(100, 0);              // Relativistic quantum numbers (kappa)
std::vector<int> NP(100, 0);               // Principal quantum numbers
std::vector<int> NH(100, 0);               // Another quantum number

// Wave function arrays (initialized empty - populated when reading .w file)
std::vector<std::vector<double>> PF;       // Large component radial functions
std::vector<std::vector<double>> QF;       // Small component radial functions  

// Physical constants - these are already defined elsewhere, just declare them
// PI is defined in constants.cpp
// Z is defined in def.cpp  
double C = 137.0359998;                    // Speed of light in atomic units
double CONV = 2.1947463136320e5;           // Conversion factor cm^-1 to Hartree

// Additional constant not defined elsewhere
double AU2FM = 5.2917721067e-05;           // Bohr radius in fm

// Global output file stream (corresponds to Fortran unit 24)
std::ofstream output_file;