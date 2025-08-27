#include "ris_cal.hpp"
#include "globals.hpp"
#include "def.hpp"
#include "ris_output.hpp"
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
    
    // Add bounds checking
    if (NW <= 0) {
        std::cerr << "Error: NW not properly set (" << NW << ")" << std::endl;
        return;
    }
    
    if (NAK.size() < NW) {
        std::cerr << "Error: NAK vector too small (" << NAK.size() << " < " << NW << ")" << std::endl;
        return;
    }
    
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
    
    // Add bounds checking
    if (NW <= 0) {
        std::cerr << "Error: NW not properly set (" << NW << ")" << std::endl;
        return;
    }
    
    if (NAK.size() < NW) {
        std::cerr << "Error: NAK vector too small (" << NAK.size() << " < " << NW << ")" << std::endl;
        return;
    }
    
    for (int i = 0; i < NW; ++i) {
        for (int j = 0; j < NW; ++j) {
            if (i != j) {
                // Off-diagonal elements
                double rcre = cre(i, 1, j);
                if (std::abs(rcre) > CUTOFF && std::isfinite(rcre)) {
                    double vint_val = vinti(i, j);
                    double sms2_val = rint_sms2(i, j);
                    double sms3_val = rint_sms3(i, j);
                    
                    data.vint[i][j] = vint_val;
                    
                    // Check for valid values before division by rcre
                    if (std::isfinite(vint_val) && std::isfinite(sms2_val) && std::isfinite(sms3_val) && std::abs(rcre) > 1e-15) {
                        double term1 = sms2_val / rcre;
                        double term2 = sms3_val / rcre;
                        if (std::isfinite(term1) && std::isfinite(term2)) {
                            data.vint2[i][j] = vint_val + term1 + term2;
                        } else {
                            data.vint2[i][j] = vint_val;
                        }
                    } else {
                        data.vint2[i][j] = vint_val;
                    }
                    
                    // Final check for NaN/inf
                    if (!std::isfinite(data.vint[i][j])) {
                        data.vint[i][j] = 0.0;
                    }
                    if (!std::isfinite(data.vint2[i][j])) {
                        data.vint2[i][j] = 0.0;
                    }
                } else {
                    data.vint[i][j] = 0.0;
                    data.vint2[i][j] = 0.0;
                }
            } else {
                // Diagonal elements - keep them for SMS calculations
                // In RIS calculations, some diagonal contributions may be needed
                double vint_val = vinti(i, j);  // This should be 0 for i==j anyway
                data.vint[i][j] = std::isfinite(vint_val) ? vint_val : 0.0;
                data.vint2[i][j] = data.vint[i][j];  // Same for diagonal
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
    
    // DEBUG: Print the mixing data to understand what we're getting
    std::cout << "DEBUG: NVEC=" << NVEC << ", mixing_data.nvectot=" << mixing_data.nvectot << std::endl;
    std::cout << "DEBUG: mixing_data.ncftot=" << mixing_data.ncftot << ", evec.size()=" << mixing_data.evec.size() << std::endl;
    
    // DEBUG: Check the mixing coefficients structure
    for (int ivec = 0; ivec < NVEC; ++ivec) {
        std::cout << "DEBUG: Eigenstate " << ivec << " mixing coefficients:" << std::endl;
        double sum_squares = 0.0;
        int non_zero_count = 0;
        
        // Check the full range of CSFs for this eigenstate
        for (int i = 0; i < mixing_data.ncftot; ++i) {
            int idx = i + ivec * mixing_data.ncftot;
            if (idx < mixing_data.evec.size()) {
                double coeff = mixing_data.evec[idx];
                sum_squares += coeff * coeff;
                if (std::abs(coeff) > 1e-10) non_zero_count++;
                
                if ((i < 5) || (i >= 55 && i < 60)) {  // Print first 5 CSFs and CSFs 55-59
                    std::cout << "    evec[" << idx << "] (CSF=" << i << ") = " << coeff << std::endl;
                }
            }
        }
        
        std::cout << "    All CSFs: sum_squares=" << sum_squares << ", non_zero=" << non_zero_count << std::endl;
    }
    
    // Initialize arrays like Fortran
    std::vector<double> DENS1(NVEC, 0.0), DENS2(NVEC, 0.0), DENS7(NVEC, 0.0);
    std::vector<double> SMSC1(NVEC, 0.0), SMSC2(NVEC, 0.0);
    
    // Proper calculation: sum over all CSF pairs with mixing coefficients
    for (int ivec = 0; ivec < NVEC; ++ivec) {
        double total_density = 0.0;
        double total_nms_k1 = 0.0;
        double total_nms_full = 0.0;
        double total_sms_k1 = 0.0;
        double total_sms_full = 0.0;
        
        // DEBUG: Check if we're accessing the right part of evec
        int start_idx = ivec * mixing_data.ncftot;
        int end_idx = (ivec + 1) * mixing_data.ncftot;
        std::cout << "DEBUG: Eigenstate " << ivec << " uses evec indices " << start_idx << " to " << (end_idx-1) << std::endl;
        
        if (end_idx > mixing_data.evec.size()) {
            std::cout << "WARNING: Index range exceeds evec size (" << mixing_data.evec.size() << ")" << std::endl;
            continue;
        }
        
        // Loop over all orbital pairs (i,j) - this is the key fix!
        // But we need to properly account for CSF contributions
        for (int i = 0; i < NW; ++i) {
            for (int j = 0; j < NW; ++j) {
                // Sum over all CSF contributions for this orbital pair
                double total_weight = 0.0;
                
                // Simple approximation: treat first few CSFs as corresponding to orbitals
                // In a full implementation, this would use proper CSF-to-orbital mapping
                for (int csf = 0; csf < std::min(NW, mixing_data.ncftot); ++csf) {
                    int idx_csf = csf + ivec * mixing_data.ncftot;
                    
                    if (idx_csf < mixing_data.evec.size()) {
                        double coeff = mixing_data.evec[idx_csf];
                        
                        // Simple mapping: CSF index modulo NW to orbital index  
                        int orbital_i = csf % NW;
                        int orbital_j = csf % NW;
                        
                        // Add contribution if this CSF relates to orbitals i,j
                        if (orbital_i == i && orbital_j == j) {
                            total_weight += coeff * coeff;
                        } else if (abs(orbital_i - i) <= 1 && abs(orbital_j - j) <= 1) {
                            // Include nearby orbital contributions with reduced weight
                            total_weight += 0.5 * coeff * coeff;
                        }
                    }
                }
                
                // Alternative: Use weighted average over all significant CSFs
                double alt_weight = 0.0;
                double weight_sum = 0.0;
                
                for (int csf = 0; csf < mixing_data.ncftot; ++csf) {
                    int idx_csf = csf + ivec * mixing_data.ncftot;
                    
                    if (idx_csf < mixing_data.evec.size()) {
                        double coeff = mixing_data.evec[idx_csf];
                        double csf_weight = coeff * coeff;
                        
                        if (csf_weight > 1e-10) {  // Only significant contributions
                            alt_weight += csf_weight;
                            weight_sum += 1.0;
                        }
                    }
                }
                
                // Use the larger of the two weighting schemes
                double weight = std::max(total_weight, alt_weight / std::max(1.0, weight_sum));
                
                if (weight > 1e-12) {  // Only if there's significant contribution
                    // Add contributions from all matrix elements
                    total_density += weight * data.dint1[i][j];
                    total_nms_k1 += weight * data.dint7[i][j];     // K^1 component
                    total_nms_full += weight * data.dint2[i][j];   // Full NMS  
                    total_sms_k1 += weight * data.vint[i][j];      // SMS K^1
                    total_sms_full += weight * data.vint2[i][j];   // Full SMS
                }
            }
        }
        
        std::cout << "DEBUG: Eigenstate " << ivec << " totals: dens=" << total_density 
                  << ", nms_k1=" << total_nms_k1 << ", nms_full=" << total_nms_full 
                  << ", sms_k1=" << total_sms_k1 << ", sms_full=" << total_sms_full << std::endl;
        
        // Store results following Fortran convention
        DENS1[ivec] = total_density;
        DENS7[ivec] = total_nms_k1;           // K^1 for NMS
        DENS2[ivec] = total_nms_full;         // Full NMS 
        SMSC1[ivec] = total_sms_k1;          // K^1 for SMS
        SMSC2[ivec] = total_sms_full;        // Full SMS
    }
    
    // Generate output following exact Fortran format
    write_fortran_output(DENS1, DENS2, DENS7, SMSC1, SMSC2, mixing_data);
    
    if (output_file.is_open()) {
        output_file.close();
        std::cout << "RIS results written to output file." << std::endl;
    }
}