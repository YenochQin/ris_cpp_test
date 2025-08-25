#ifndef RIS_CAL_HPP
#define RIS_CAL_HPP

#include <string>
#include <vector>
#include "mixblock.hpp"

struct RisCalData {
    // Core dimensions
    int nrnuc = 0;  // Nuclear radius grid point
    
    // Multi-dimensional arrays for density integrals and vectors
    std::vector<std::vector<std::vector<double>>> dint1vec;  // DINT1VEC(NNNW,NNNW,NRNUC)
    std::vector<std::vector<double>> dens1vec;               // DENS1VEC(NVEC,NRNUC)
    std::vector<double> densfit;                             // DENSFIT(NRNUC)
    
    // Matrix for field shift calculations  
    std::vector<std::vector<double>> fmat;                   // FMAT(6,NVEC)
    std::vector<double> rho;                                 // RHO(NVEC)
    std::vector<double> res;                                 // RES(NVEC)
    
    // SMS and density arrays for each eigenstate
    std::vector<double> smsc1, smsc2;
    std::vector<double> dens1, dens2, dens3, dens4, dens5, dens6, dens7;
    
    // Work arrays for integrals 
    std::vector<double> tshell;                              // TSHELL(NNNW)
    std::vector<std::vector<double>> vint, vint2;            // VINT(NNNW,NNNW), VINT2(NNNW,NNNW)
    std::vector<std::vector<double>> dint1, dint2, dint3, dint4, dint5, dint6, dint7; // Density integrals
    
    // Constructor
    RisCalData(int nw, int nvec, int nrnuc_val);
    
    // Initialize all arrays to zero
    void initialize();
    
    // Destructor (RAII handles cleanup automatically)
    ~RisCalData() = default;
};

// Main RIS calculation function  
bool ris_cal(const std::string& name, const MixingData& mixing_data);

// Helper functions for different calculation phases
void calculate_density_integrals(RisCalData& data, bool compute_higher_order);
void calculate_vinti_integrals(RisCalData& data);
void calculate_angular_data(RisCalData& data);
void process_eigenstates(RisCalData& data, const MixingData& mixing_data);

#endif // RIS_CAL_HPP