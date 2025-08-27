#ifndef CONSTANTS_HPP
#define CONSTANTS_HPP

// Physical constants from setcon.f90
extern double PI;
extern double CVAC;
extern double EMPAM;

// Unit conversion constants calculated in setcon()
extern double AUMAMU;  // Electron mass in u
extern double AUCM;    // Atomic units to kayser
extern double CCMS;    // Speed of light in cm/s

void setcon();

#endif // CONSTANTS_HPP
