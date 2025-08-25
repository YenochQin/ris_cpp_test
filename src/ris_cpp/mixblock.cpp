#include "mixblock.hpp"
#include "globals.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <stdexcept>


bool getmixblock(const std::string& name, int nci, MixingData& mixing_data) {
    try {
        // Construct filename based on NCI flag
        std::string filnam;
        size_t k = name.find(' ');
        if (k == std::string::npos) k = name.length();
        
        if (nci == 0) {
            filnam = name.substr(0, k) + ".cm";
        } else {
            filnam = name.substr(0, k) + ".m";
        }

        // Open binary file for reading
        std::ifstream file(filnam, std::ios::binary);
        if (!file.is_open()) {
            std::cerr << "Error when opening " << filnam << std::endl;
            return false;
        }

        // Check header
        char g92mix[7] = {0};
        file.read(g92mix, 6);
        if (std::string(g92mix) != "G92MIX") {
            std::cerr << "Not a GRASP92 MIXing Coefficients File" << std::endl;
            file.close();
            return false;
        }

        // Read main parameters
        file.read(reinterpret_cast<char*>(&mixing_data.nelec), sizeof(int));
        file.read(reinterpret_cast<char*>(&mixing_data.ncftot), sizeof(int));
        file.read(reinterpret_cast<char*>(&mixing_data.nw), sizeof(int));
        file.read(reinterpret_cast<char*>(&mixing_data.nvectot), sizeof(int));
        file.read(reinterpret_cast<char*>(&mixing_data.nvecsiz), sizeof(int));
        file.read(reinterpret_cast<char*>(&mixing_data.nblock), sizeof(int));

        std::cout << "   nelec  = " << mixing_data.nelec << std::endl;
        std::cout << "   ncftot = " << mixing_data.ncftot << std::endl;
        std::cout << "   nw     = " << mixing_data.nw << std::endl;
        std::cout << "   nblock = " << mixing_data.nblock << std::endl;
        std::cout << std::endl;

        // Allocate memory using modern C++ containers
        mixing_data.eval.resize(mixing_data.nvectot);
        mixing_data.evec.resize(mixing_data.ncftot * mixing_data.nvectot, 0.0);
        mixing_data.ivec.resize(mixing_data.nvectot);
        mixing_data.iatjpo.resize(mixing_data.nvectot);
        mixing_data.iaspar.resize(mixing_data.nvectot);

        // Initialize counters and sum registers
        int nvecpat = 0;
        int ncfpat = 0;
        int nvecsizpat = 0;
        int neavsum = 0;
        double eavsum = 0.0;

        std::cout << "  block     ncf     nev    2j+1  parity" << std::endl;

        // Process each block
        for (int jb = 1; jb <= mixing_data.nblock; ++jb) {
            int nb, ncfblk, nevblk, iatjp, iaspa;
            
            file.read(reinterpret_cast<char*>(&nb), sizeof(int));
            file.read(reinterpret_cast<char*>(&ncfblk), sizeof(int));
            file.read(reinterpret_cast<char*>(&nevblk), sizeof(int));
            file.read(reinterpret_cast<char*>(&iatjp), sizeof(int));
            file.read(reinterpret_cast<char*>(&iaspa), sizeof(int));

            std::cout << std::setw(8) << nb << std::setw(8) << ncfblk 
                     << std::setw(8) << nevblk << std::setw(8) << iatjp 
                     << std::setw(8) << iaspa << std::endl;

            if (jb != nb) {
                throw std::runtime_error("Block numbering error: jb != nb");
            }

            if (nevblk > 0) {
                // Read IVEC values for this block
                for (int i = 0; i < nevblk; ++i) {
                    file.read(reinterpret_cast<char*>(&mixing_data.ivec[nvecpat + i]), sizeof(int));
                }

                // Set quantum numbers for this block
                for (int i = 0; i < nevblk; ++i) {
                    mixing_data.iatjpo[nvecpat + i] = iatjp;
                    mixing_data.iaspar[nvecpat + i] = iaspa;
                }

                // Read average energy and eigenvalues
                double block_eav;
                file.read(reinterpret_cast<char*>(&block_eav), sizeof(double));
                
                for (int i = 0; i < nevblk; ++i) {
                    file.read(reinterpret_cast<char*>(&mixing_data.eval[nvecpat + i]), sizeof(double));
                    mixing_data.eval[nvecpat + i] += block_eav; // Add average energy
                }

                // Update overall average energy calculation
                eavsum += block_eav * ncfblk;
                neavsum += ncfblk;

                // Read eigenvector coefficients
                for (int j = 0; j < nevblk; ++j) {
                    for (int i = 0; i < ncfblk; ++i) {
                        int index = nvecsizpat + ncfpat + i + j * mixing_data.ncftot;
                        file.read(reinterpret_cast<char*>(&mixing_data.evec[index]), sizeof(double));
                    }
                }
            }

            nvecpat += nevblk;
            ncfpat += ncfblk;
            nvecsizpat += nevblk * mixing_data.ncftot;
        }

        file.close();

        // Calculate overall average energy
        double overall_eav = eavsum / neavsum;
        
        if (mixing_data.ncftot != neavsum) {
            std::cout << "Not all blocks are diagonalized --- Average E not correct" << std::endl;
        }

        // Subtract overall average energy from all eigenvalues
        for (int i = 0; i < mixing_data.nvectot; ++i) {
            mixing_data.eval[i] -= overall_eav;
        }

        // Set global variables
        NELEC = mixing_data.nelec;
        NCF = mixing_data.ncftot;
        NVEC = mixing_data.nvectot;
        NW = mixing_data.nw;
        EAV = overall_eav;

        return true;

    } catch (const std::exception& e) {
        std::cerr << "Error in getmixblock: " << e.what() << std::endl;
        return false;
    }
}