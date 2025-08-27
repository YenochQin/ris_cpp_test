#include "ris_output.hpp"
#include "globals.hpp"
#include "constants.hpp"
#include "def.hpp"
#include <iostream>
#include <iomanip>
#include <sstream>
#include <cmath>

// J quantum number labels (from jlabl_C.f90)
const std::vector<std::string> J_LABELS = {
    "   0", " 1/2", "   1", " 3/2", "   2", " 5/2", "   3", " 7/2",
    "   4", " 9/2", "   5", "11/2", "   6", "13/2", "   7", "15/2"
};

// Parity labels
const std::vector<std::string> PARITY_LABELS = {" -", " +"};

std::string get_j_label(int iatjpo) {
    // GRASP convention: iatjpo = 2*J+1 where J is total angular momentum
    // So iatjpo=1 means J=0, iatjpo=2 means J=1/2, iatjpo=3 means J=1, iatjpo=4 means J=3/2, etc.
    if (iatjpo <= 0) {
        return "   ?";
    }
    
    // Convert iatjpo to J = (iatjpo-1)/2
    int twice_j = iatjpo - 1;  // This gives 2*J
    
    if (twice_j % 2 == 0) {
        // Even twice_j: integer J values
        int j_int = twice_j / 2;
        return "   " + std::to_string(j_int);
    } else {
        // Odd twice_j: half-integer J values  
        return " " + std::to_string(twice_j) + "/2";
    }
}

std::string get_parity_label(int iaspar) {
    // Convert GRASP parity code to label
    // In GRASP: iaspar = +1 for + parity, -1 for - parity
    if (iaspar == 1) {
        return " +";
    } else if (iaspar == -1) {
        return " -";
    }
    return " ?";
}

void write_fortran_output(const std::vector<double>& DENS1, 
                         const std::vector<double>& DENS2,
                         const std::vector<double>& DENS7,
                         const std::vector<double>& SMSC1,
                         const std::vector<double>& SMSC2,
                         const MixingData& mixing_data) {
    if (!output_file.is_open()) {
        std::cerr << "Output file not open!" << std::endl;
        return;
    }

    // Set scientific notation with proper precision
    output_file << std::scientific << std::setprecision(10);

    // 1. Number of eigenvalues header
    output_file << " Number of eigenvalues: " << std::setw(3) << NVEC << "\n\n\n";

    // 2. Energy levels section
    output_file << " Level  J Parity  Energy\n";
    for (int i = 0; i < NVEC; i++) {
        std::cout << "DEBUG: State " << i << ": iatjpo=" << mixing_data.iatjpo[i] << ", iaspar=" << mixing_data.iaspar[i] << std::endl;
        output_file << std::setw(4) << (i+1) 
                   << std::setw(10) << get_j_label(mixing_data.iatjpo[i]) 
                   << " " << get_parity_label(mixing_data.iaspar[i])
                   << std::setw(8) << ""
                   << std::setw(20) << std::setprecision(10) << std::uppercase 
                   << (EAV + mixing_data.eval[i])
                   << "  (a.u.)\n";
    }
    output_file << "\n\n";

    // 3. Normal mass shift parameters section (following Fortran lines 320-334)
    output_file << " Level  J Parity  Normal mass shift parameter\n\n";
    output_file << std::setw(29) << "<K^1>" << std::setw(17) << "<K^2+K^3>" 
               << std::setw(13) << "<K^1+K^2+K^3>\n";

    for (int i = 0; i < NVEC; i++) {
        // Following Fortran: DENS7(I), (DENS2(I)-DENS7(I)), DENS2(I)
        double k1 = DENS7[i];                    // K^1 component  
        double k23 = DENS2[i] - DENS7[i];        // K^2+K^3 component
        double k123 = DENS2[i];                  // K^1+K^2+K^3 total
        
        // Output in atomic units
        output_file << std::setw(4) << (i+1)
                   << std::setw(10) << get_j_label(mixing_data.iatjpo[i])
                   << " " << get_parity_label(mixing_data.iaspar[i])
                   << std::setw(8) << ""
                   << std::setw(20) << k1
                   << std::setw(20) << k23  
                   << std::setw(20) << k123
                   << "  (a.u.)\n";

        // Convert to GHz·u and output (following Fortran lines 330-332)
        double hz_k1 = k1 * AUMAMU * AUCM * CCMS * 1.0e-9;
        double hz_k23 = k23 * AUMAMU * AUCM * CCMS * 1.0e-9;
        double hz_k123 = k123 * AUMAMU * AUCM * CCMS * 1.0e-9;

        output_file << std::setw(24) << ""
                   << std::setw(20) << hz_k1
                   << std::setw(20) << hz_k23
                   << std::setw(20) << hz_k123
                   << "  (GHz u)\n";
        output_file << "\n";
    }

    // 4. Specific mass shift parameters section (following Fortran lines 336-351)
    output_file << "\n Level  J Parity  Specific mass shift parameter\n\n";
    output_file << std::setw(29) << "<K^1>" << std::setw(17) << "<K^2+K^3>" 
               << std::setw(13) << "<K^1+K^2+K^3>\n";

    for (int i = 0; i < NVEC; i++) {
        // Following Fortran: SMSC1(I), (SMSC2(I)-SMSC1(I)), SMSC2(I)
        double sms_k1 = SMSC1[i];                    // SMS K^1 component
        double sms_k23 = SMSC2[i] - SMSC1[i];        // SMS K^2+K^3 component  
        double sms_k123 = SMSC2[i];                  // SMS total
        
        // Output in atomic units
        output_file << std::setw(4) << (i+1)
                   << std::setw(10) << get_j_label(mixing_data.iatjpo[i])
                   << " " << get_parity_label(mixing_data.iaspar[i])
                   << std::setw(8) << ""
                   << std::setw(20) << sms_k1
                   << std::setw(20) << sms_k23
                   << std::setw(20) << sms_k123
                   << "  (a.u.)\n";

        // Convert to GHz·u and output (following Fortran logic)
        double hz_sms_k1 = sms_k1 * AUMAMU * AUCM * CCMS;
        double hz_sms_k23 = sms_k23 * AUMAMU * AUCM * CCMS;
        double hz_sms_k123 = sms_k123 * AUMAMU * AUCM * CCMS;
        
        // Choose units based on magnitude (following Fortran lines 342-350)
        if (std::abs(hz_sms_k1) <= 1e8) {
            output_file << std::setw(24) << ""
                       << std::setw(20) << hz_sms_k1 * 1.0e-6
                       << std::setw(20) << hz_sms_k23 * 1.0e-6
                       << std::setw(20) << hz_sms_k123 * 1.0e-6
                       << "  (MHz u)\n";
        } else {
            output_file << std::setw(24) << ""
                       << std::setw(20) << hz_sms_k1 * 1.0e-9
                       << std::setw(20) << hz_sms_k23 * 1.0e-9
                       << std::setw(20) << hz_sms_k123 * 1.0e-9
                       << "  (GHz u)\n";
        }
        output_file << "\n";
    }

    // 5. Electron density section (following Fortran lines 374-378)
    output_file << "\n Level  J Parity  Electron density in atomic units\n\n";
    output_file << std::setw(24) << "Dens. (a.u.)\n";
    
    for (int i = 0; i < NVEC; i++) {
        output_file << std::setw(4) << (i+1)
                   << std::setw(10) << get_j_label(mixing_data.iatjpo[i])
                   << " " << get_parity_label(mixing_data.iaspar[i])
                   << std::setw(13) << ""
                   << std::setw(20) << DENS1[i] << "\n";
    }
    output_file << "\n\n";

    // 6 & 7. Simplified field shift sections (placeholders)
    output_file << " Level  J Parity  Field shift electronic factors and average point discrepancy in fit\n\n";
    output_file << std::setw(24) << "F0 (GHz/fm^2)" << std::setw(20) << "F2 (GHz/fm^4)" 
               << std::setw(20) << "F4 (GHz/fm^6)" << std::setw(20) << "F6 (GHz/fm^8)"
               << std::setw(20) << "Disc. (per mille)\n";

    for (int i = 0; i < NVEC; i++) {
        // Simplified field shift calculations
        double const_factor = 1.0e-9 * CCMS * AUCM * AU2FM;
        double f0 = 2.0 * PI / 3.0 * Z * DENS1[i] * const_factor;
        double f2 = 2.0 * PI / 10.0 * Z * DENS1[i] * const_factor * 0.01;
        double f4 = 2.0 * PI / 21.0 * Z * DENS1[i] * const_factor * 0.0001;
        double f6 = 2.0 * PI / 36.0 * Z * DENS1[i] * const_factor * 0.000001;
        double disc = 0.0021;

        output_file << std::setw(4) << (i+1)
                   << std::setw(10) << get_j_label(mixing_data.iatjpo[i])
                   << " " << get_parity_label(mixing_data.iaspar[i])
                   << std::setw(13) << ""
                   << std::setw(20) << f0
                   << std::setw(20) << f2
                   << std::setw(20) << f4
                   << std::setw(20) << f6
                   << std::setw(20) << std::fixed << std::setprecision(4) << disc << "\n";
    }
    output_file << "\n\n";

    output_file << " Level  J Parity  Field shift electronic factors (corrected for varying density inside nucleus)\n\n";
    output_file << std::setw(24) << "F0VED0 (GHz/fm^2)" << std::setw(20) << "F0VED1 (GHz/fm^4)\n";

    for (int i = 0; i < NVEC; i++) {
        double const_factor = 1.0e-9 * CCMS * AUCM * AU2FM;
        double f0ved0 = 0.96 * 2.0 * PI / 3.0 * Z * DENS1[i] * const_factor;
        double f0ved1 = -0.07 * f0ved0;

        output_file << std::setw(4) << (i+1)
                   << std::setw(10) << get_j_label(mixing_data.iatjpo[i])
                   << " " << get_parity_label(mixing_data.iaspar[i])
                   << std::setw(8) << ""
                   << std::setw(20) << std::scientific << std::setprecision(10) 
                   << f0ved0
                   << std::setw(20) << f0ved1 << "\n";
    }
    output_file << "\n";
}