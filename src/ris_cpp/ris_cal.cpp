#include "ris_cal.hpp"
#include "globals.hpp"
#include "def.hpp"
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <cmath>
#include <stdexcept>

// External function declarations (these would be implemented elsewhere or linked from Fortran libraries)
extern double rintdens(int i, int j);
extern void rintdensvec(int i, int j, std::vector<std::vector<std::vector<double>>>& dint1vec, int nrnuc);
extern void rinti_nms(int i, int j, double& dint2, double& dint7);
extern double rint(int i, int j, int k);
extern double cre(int i, int flag, int j);
extern double vinti(int i, int j);
extern double rint_sms2(int i, int j);
extern double rint_sms3(int i, int j);
extern bool getyn();

RisCalData::RisCalData(int nw, int nvec, int nrnuc_val) 
    : nrnuc(nrnuc_val),
      dint1vec(nw, std::vector<std::vector<double>>(nw, std::vector<double>(nrnuc_val))),
      dens1vec(nvec, std::vector<double>(nrnuc_val)),
      densfit(nrnuc_val),
      fmat(6, std::vector<double>(nvec)),
      rho(nvec),
      res(nvec),
      smsc1(nvec), smsc2(nvec),
      dens1(nvec), dens2(nvec), dens3(nvec), dens4(nvec), 
      dens5(nvec), dens6(nvec), dens7(nvec),
      tshell(nw),
      vint(nw, std::vector<double>(nw)),
      vint2(nw, std::vector<double>(nw)),
      dint1(nw, std::vector<double>(nw)),
      dint2(nw, std::vector<double>(nw)),
      dint3(nw, std::vector<double>(nw)),
      dint4(nw, std::vector<double>(nw)),
      dint5(nw, std::vector<double>(nw)),
      dint6(nw, std::vector<double>(nw)),
      dint7(nw, std::vector<double>(nw))
{
    initialize();
}

void RisCalData::initialize() {
    // Initialize all arrays to zero (modern C++ containers are already zero-initialized for numeric types)
    
    // Multi-dimensional arrays
    for (auto& mat : dint1vec) {
        for (auto& row : mat) {
            std::fill(row.begin(), row.end(), 0.0);
        }
    }
    
    for (auto& row : dens1vec) {
        std::fill(row.begin(), row.end(), 0.0);
    }
    
    // Single vectors
    std::fill(densfit.begin(), densfit.end(), 0.0);
    std::fill(rho.begin(), rho.end(), 0.0);
    std::fill(res.begin(), res.end(), 0.0);
    
    std::fill(smsc1.begin(), smsc1.end(), 0.0);
    std::fill(smsc2.begin(), smsc2.end(), 0.0);
    std::fill(dens1.begin(), dens1.end(), 0.0);
    std::fill(dens2.begin(), dens2.end(), 0.0);
    std::fill(dens3.begin(), dens3.end(), 0.0);
    std::fill(dens4.begin(), dens4.end(), 0.0);
    std::fill(dens5.begin(), dens5.end(), 0.0);
    std::fill(dens6.begin(), dens6.end(), 0.0);
    std::fill(dens7.begin(), dens7.end(), 0.0);
    
    std::fill(tshell.begin(), tshell.end(), 0.0);
    
    // 2D matrices
    for (auto& row : vint) {
        std::fill(row.begin(), row.end(), 0.0);
    }
    for (auto& row : vint2) {
        std::fill(row.begin(), row.end(), 0.0);
    }
    for (auto& row : fmat) {
        std::fill(row.begin(), row.end(), 0.0);
    }
    for (auto& row : dint1) {
        std::fill(row.begin(), row.end(), 0.0);
    }
    for (auto& row : dint2) {
        std::fill(row.begin(), row.end(), 0.0);
    }
    for (auto& row : dint3) {
        std::fill(row.begin(), row.end(), 0.0);
    }
    for (auto& row : dint4) {
        std::fill(row.begin(), row.end(), 0.0);
    }
    for (auto& row : dint5) {
        std::fill(row.begin(), row.end(), 0.0);
    }
    for (auto& row : dint6) {
        std::fill(row.begin(), row.end(), 0.0);
    }
    for (auto& row : dint7) {
        std::fill(row.begin(), row.end(), 0.0);
    }
}

void calculate_density_integrals(RisCalData& data, bool compute_higher_order) {
    std::cout << "Calculating density integrals..." << std::endl;
    
    for (int i = 0; i < NW; ++i) {
        for (int j = 0; j < NW; ++j) {
            if (NAK[i] == NAK[j]) {
                // Calculate various density integrals
                data.dint1[i][j] = rintdens(i, j);
                
                if (compute_higher_order) {
                    rintdensvec(i, j, data.dint1vec, data.nrnuc);
                }
                
                rinti_nms(i, j, data.dint2[i][j], data.dint7[i][j]);
                data.dint3[i][j] = rint(i, j, 1);
                data.dint4[i][j] = rint(i, j, 2);
                data.dint5[i][j] = rint(i, j, -1);
                data.dint6[i][j] = rint(i, j, -2);
            } else {
                // Zero out if quantum numbers don't match
                data.dint1[i][j] = 0.0;
                for (int k = 0; k < data.nrnuc; ++k) {
                    data.dint1vec[i][j][k] = 0.0;
                }
                data.dint2[i][j] = 0.0;
                data.dint3[i][j] = 0.0;
                data.dint4[i][j] = 0.0;
                data.dint5[i][j] = 0.0;
                data.dint6[i][j] = 0.0;
                data.dint7[i][j] = 0.0;
            }
        }
    }
}

void calculate_vinti_integrals(RisCalData& data) {
    std::cout << "Calculating Vinti integrals..." << std::endl;
    
    for (int i = 0; i < NW; ++i) {
        for (int j = 0; j < NW; ++j) {
            if (i != j) {
                double rcre = cre(NAK[i], 1, NAK[j]);
                if (std::abs(rcre) > CUTOFF) {
                    data.vint[i][j] = vinti(i, j);
                    data.vint2[i][j] = data.vint[i][j] + rint_sms2(i, j) / rcre + rint_sms3(i, j) / rcre;
                } else {
                    data.vint[i][j] = 0.0;
                    data.vint2[i][j] = 0.0;
                }
            } else {
                data.vint[i][j] = 0.0;
                data.vint2[i][j] = 0.0;
            }
        }
    }
}

bool ris_cal(const std::string& name, const MixingData& mixing_data) {
    try {
        std::cout << "-------------------------------" << std::endl;
        std::cout << "RIS_CAL: Execution Begins ..." << std::endl;
        std::cout << "-------------------------------" << std::endl;

        // Determine nuclear radius grid endpoint for density fit
        double fo90 = PARM[0] + 2.0 * std::log(3.0) * PARM[1]; // PARM(1) = c, PARM(2) = a
        int nrnuc = 0;
        
        for (int i = 0; i < NNNP - 1; ++i) {
            if (R[i + 1] > fo90 && R[i] <= fo90) {
                nrnuc = i + 1;
                break;
            }
        }
        
        if (nrnuc == 0) {
            throw std::runtime_error("Could not determine nuclear radius grid point");
        }

        // Create data structure with RAII memory management
        RisCalData data(NW, NVEC, nrnuc);

        // Ask user about higher-order field shift calculations
        std::cout << " Compute higher order field shift electronic factors?" << std::endl;
        bool compute_higher_order = getyn();

        // Calculate all required integrals
        calculate_density_integrals(data, compute_higher_order);
        calculate_vinti_integrals(data);

        // Process eigenstates and calculate final results
        process_eigenstates(data, mixing_data);

        std::cout << "-------------------------------" << std::endl;
        std::cout << "RIS_CAL: Execution Finished ..." << std::endl;
        std::cout << "-------------------------------" << std::endl;
        
        return true;

    } catch (const std::exception& e) {
        std::cerr << "Error in ris_cal: " << e.what() << std::endl;
        return false;
    }
}

void process_eigenstates(RisCalData& data, const MixingData& mixing_data) {
    std::cout << "Processing eigenstates..." << std::endl;
    
    // This would implement the main calculation loop over eigenstates
    // For now, this is a placeholder showing the structure
    
    for (int ivec = 0; ivec < NVEC; ++ivec) {
        // Process each eigenstate using the mixing coefficients and calculated integrals
        // This involves summing over configuration state functions weighted by mixing coefficients
        
        // Placeholder calculations - would be filled in with actual physics
        data.smsc1[ivec] = 0.0;  // SMS field shift coefficient 1
        data.smsc2[ivec] = 0.0;  // SMS field shift coefficient 2
        
        for (int k = 1; k <= 7; ++k) {
            // Calculate different density contributions
            switch(k) {
                case 1: data.dens1[ivec] = 0.0; break;
                case 2: data.dens2[ivec] = 0.0; break;
                case 3: data.dens3[ivec] = 0.0; break;
                case 4: data.dens4[ivec] = 0.0; break;
                case 5: data.dens5[ivec] = 0.0; break;
                case 6: data.dens6[ivec] = 0.0; break;
                case 7: data.dens7[ivec] = 0.0; break;
            }
        }
    }
    
    std::cout << "Eigenstate processing complete." << std::endl;
}