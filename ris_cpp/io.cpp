#include "io.hpp"
#include <iostream>
#include <limits>

bool getyn() {
    char response;
    while (true) {
        std::cin >> response;
        if (response == 'y' || response == 'Y') {
            std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            return true;
        } else if (response == 'n' || response == 'N') {
            std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            return false;
        } else {
            std::cout << "Expecting <y><cr> or <n><cr> ..." << std::endl;
            std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        }
    }
}
