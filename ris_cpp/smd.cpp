#include "smd.hpp"
#include "io.hpp"
#include "state.hpp"
#include "globals.hpp"
#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>
#include <fstream>
#include <cstdint>

// Stubs for called subroutines
void setiso(const std::string& name) {}
void setqic() {}
void radgrd() {}
void nucpot() {}

void setrwfa(const std::string& name) {
    try {
        std::ifstream file(name, std::ios::binary);
        if (!file.is_open()) {
            std::cerr << "Error when opening " << name << std::endl;
            return;
        }

        // Read G92RWF header from Fortran record
        int32_t record_len_before, record_len_after;
        file.read(reinterpret_cast<char*>(&record_len_before), sizeof(int32_t));
        
        char g92rwf[7] = {0};
        file.read(g92rwf, 6);
        
        file.read(reinterpret_cast<char*>(&record_len_after), sizeof(int32_t));
        
        if (record_len_before != record_len_after || record_len_before != 6) {
            std::cerr << "Invalid radial wavefunction file format" << std::endl;
            file.close();
            return;
        }
        
        if (std::string(g92rwf) != "G92RWF") {
            std::cerr << "Not a G92RWF radial wavefunction file" << std::endl;
            file.close();
            return;
        }

        std::cout << "Reading radial wavefunctions from " << name << std::endl;

        // Initialize wave function arrays
        PF.resize(NRNT, std::vector<double>(NNNW, 0.0));
        QF.resize(NRNT, std::vector<double>(NNNW, 0.0));
        
        // Ensure quantum number arrays are properly sized
        NAK.resize(NNNW, 0);
        NP.resize(NNNW, 0);
        NH.resize(NNNW, 0);
        
        int orbital_count = 0;

        // Read orbital data until end of file
        while (true) {
            // Try to read the next record length - if this fails, we've reached EOF
            if (!file.read(reinterpret_cast<char*>(&record_len_before), sizeof(int32_t))) {
                break; // End of file
            }

            // Read orbital parameters: nn, laky, energy, npts
            int nn, laky, npts;
            double energy;
            
            file.read(reinterpret_cast<char*>(&nn), sizeof(int));
            file.read(reinterpret_cast<char*>(&laky), sizeof(int));
            file.read(reinterpret_cast<char*>(&energy), sizeof(double));
            file.read(reinterpret_cast<char*>(&npts), sizeof(int));
            
            file.read(reinterpret_cast<char*>(&record_len_after), sizeof(int32_t));
            
            if (record_len_before != record_len_after || record_len_before != 20) { // 2*int + 1*double + 1*int = 4+4+8+4 = 20
                std::cerr << "Invalid orbital header record format" << std::endl;
                break;
            }

            std::cout << "Orbital: n=" << nn << ", kappa=" << laky << ", energy=" << energy << ", npts=" << npts << std::endl;

            // Store orbital quantum numbers (with bounds checking)
            if (orbital_count < NNNW && orbital_count < NAK.size()) {
                NAK[orbital_count] = laky;  // kappa quantum number
                NP[orbital_count] = nn;     // principal quantum number
            } else if (orbital_count >= NNNW) {
                std::cerr << "Warning: More orbitals than NNNW (" << NNNW << "), truncating at orbital " << orbital_count << std::endl;
                break;
            }

            // Read second record: a0, pg[], qg[]
            file.read(reinterpret_cast<char*>(&record_len_before), sizeof(int32_t));
            
            double a0;
            file.read(reinterpret_cast<char*>(&a0), sizeof(double));
            
            std::vector<double> pg(npts), qg(npts);
            for (int i = 0; i < npts; ++i) {
                file.read(reinterpret_cast<char*>(&pg[i]), sizeof(double));
            }
            for (int i = 0; i < npts; ++i) {
                file.read(reinterpret_cast<char*>(&qg[i]), sizeof(double));
            }
            
            file.read(reinterpret_cast<char*>(&record_len_after), sizeof(int32_t));
            
            if (record_len_before != record_len_after || record_len_before != (1 + 2 * npts) * 8) {
                std::cerr << "Invalid wavefunction data record format" << std::endl;
                break;
            }

            // Read third record: rg[]
            file.read(reinterpret_cast<char*>(&record_len_before), sizeof(int32_t));
            
            std::vector<double> rg(npts);
            for (int i = 0; i < npts; ++i) {
                file.read(reinterpret_cast<char*>(&rg[i]), sizeof(double));
            }
            
            file.read(reinterpret_cast<char*>(&record_len_after), sizeof(int32_t));
            
            if (record_len_before != record_len_after || record_len_before != npts * 8) {
                std::cerr << "Invalid radial grid record format" << std::endl;
                break;
            }

            // Store radial grid (use grid from first orbital) 
            if (orbital_count == 0) {
                std::copy(rg.begin(), rg.begin() + std::min(npts, NRNT), R.begin());
                // Set up radial weights (simplified - should be more sophisticated)
                for (int i = 1; i < std::min(npts, NRNT); i++) {
                    RP[i] = R[i] - R[i-1];  // Simple differences as weights
                }
                RP[0] = RP[1];  // Set first weight
            }

            // Store wave functions in global arrays
            if (orbital_count < NNNW) {
                for (int i = 0; i < std::min(npts, NRNT); i++) {
                    PF[i][orbital_count] = pg[i];  // Large component
                    QF[i][orbital_count] = qg[i];  // Small component
                }
            }
            
            orbital_count++;
        }

        NW = orbital_count;  // Set number of orbitals
        std::cout << "Successfully read " << NW << " orbitals from radial wavefunction file" << std::endl;

        file.close();
    } catch (const std::exception& e) {
        std::cerr << "Error reading radial wavefunction file: " << e.what() << std::endl;
    }
}

void getsmd(const std::string& name) {
    // Main SMD function - sets up molecular data
    setiso(name);
    setqic();
    radgrd();
    nucpot();
    
    std::string wfile = name + ".w";
    setrwfa(wfile);
}