#ifndef MIXBLOCK_HPP
#define MIXBLOCK_HPP

#include <string>
#include <vector>
#include <memory>

struct MixingData {
    int nelec = 0;
    int ncftot = 0;
    int nw = 0;
    int nvectot = 0;
    int nvecsiz = 0;
    int nblock = 0;
    
    std::vector<double> eval;
    std::vector<double> evec;
    std::vector<int> ivec;
    std::vector<int> iatjpo;
    std::vector<int> iaspar;
};

// Modern C++ implementation of getmixblock functionality
bool getmixblock(const std::string& name, int nci, MixingData& mixing_data);

#endif // MIXBLOCK_HPP