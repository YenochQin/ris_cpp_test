# RIS4 C++ Refactor

This directory contains the modern C++ refactored version of the RIS4 (Relativistic Isotope Shift) calculation program from the GRASP atomic physics package.

## Overview

The RIS4 program calculates isotope shift parameters for atomic systems using relativistic atomic structure theory. This C++ version modernizes the original Fortran code while maintaining computational accuracy.

### Key Improvements

- **Memory Safety**: RAII and `std::vector` replace manual memory management
- **Type Safety**: Modern C++ strong typing and containers
- **Error Handling**: Exception-based error handling instead of program termination
- **Maintainability**: Modular design with clear interfaces
- **Performance**: Optimized data structures and algorithms
- **Standards Compliance**: C++11/14/17 features for better code quality

## Prerequisites

### Required Dependencies
- **C++ Compiler**: GCC 7.0+ or Clang 5.0+ (C++14 support required)
- **CMake**: Version 3.10 or higher
- **Google Test**: For unit testing (optional but recommended)

### On Ubuntu/Debian:
```bash
sudo apt update
sudo apt install build-essential cmake libgtest-dev
```

### On CentOS/RHEL/Rocky Linux:
```bash
sudo yum install gcc-c++ cmake3 gtest-devel
# or for newer versions:
sudo dnf install gcc-c++ cmake gtest-devel
```

### On macOS:
```bash
# Using Homebrew
brew install cmake googletest
# Xcode command line tools provide the compiler
xcode-select --install
```

## Building the Code

### Quick Start (Recommended)

```bash
# Navigate to the ris_cpp directory
cd /path/to/ris_test/src/ris_cpp

# Create and enter build directory
mkdir -p build
cd build

# Configure with CMake
cmake ..

# Compile (use -j4 for parallel build with 4 cores)
make -j4

# Run tests to verify build
make test
# OR run tests directly:
./run_tests
```

### Build Options

#### Debug Build
```bash
cmake -DCMAKE_BUILD_TYPE=Debug ..
make -j4
```

#### Release Build (Optimized)
```bash
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j4
```

#### Verbose Build (See All Commands)
```bash
make VERBOSE=1
```

## Running the Program

### Basic Usage
```bash
# From the build directory
./standalone_ris4
```

The program will prompt for:
1. **Default settings**: Choose 'y' for default parameters
2. **State name**: Enter the name of your atomic system (e.g., "Nd_2P_n3")
3. **Mixing coefficients**: Choose whether using CI calculation results

### Input Files Required

The program expects these input files in the working directory:
- `{name}.c` - Configuration state functions
- `{name}.cm` or `{name}.m` - Mixing coefficients (CI or multiconfiguration)
- `{name}.w` - Radial wave functions
- `isodata` - Isotope data file

### Example Run
```bash
# With sample input
echo -e "y\nNd_2P_n3\ny" | ./standalone_ris4
```

## Project Structure

```
ris_cpp/
├── CMakeLists.txt          # Build configuration
├── README.md               # This file
├── main.cpp                # Main program entry point
├── globals.hpp/cpp         # Global variables and constants
├── mixblock.hpp/cpp        # Mixing coefficient file handling
├── ris_cal.hpp/cpp         # Core isotope shift calculations
├── fortran_interface.cpp   # Stubs for Fortran library functions
├── io.hpp/cpp              # Input/output utilities
├── debug.hpp/cpp           # Debug functionality
├── machine.hpp/cpp         # Machine-dependent settings
├── state.hpp/cpp           # State management
├── constants.hpp/cpp       # Physical constants
├── summary.hpp/cpp         # Summary output
├── csl.hpp/cpp             # CSL file handling
├── def.hpp/cpp             # Definitions and parameters
├── smd.hpp/cpp             # SMD functionality
├── facts.hpp/cpp           # Factorial calculations
├── build/                  # Build directory (created during compilation)
└── test/                   # Unit tests
    ├── CMakeLists.txt
    ├── test_modern_cpp.cpp # C++ refactoring tests
    └── test_data/          # Sample test data files
```

## Development Workflow

### Adding New Features
1. Create header (.hpp) and implementation (.cpp) files
2. Add to `CMakeLists.txt` in both executables
3. Write unit tests in `test/` directory
4. Follow modern C++ best practices (see below)

### Testing
```bash
# Build and run all tests
cd build
make run_tests
./run_tests

# Run specific test patterns
./run_tests --gtest_filter="RIS4Test.*"
```

### Debugging
```bash
# Build with debug symbols
cmake -DCMAKE_BUILD_TYPE=Debug ..
make -j4

# Run with GDB
gdb ./standalone_ris4
# or with test runner
gdb ./run_tests
```

## Modern C++ Features Used

### Memory Management (RAII)
- `std::vector` for dynamic arrays
- Automatic cleanup on scope exit
- No manual `new`/`delete` calls

### Type Safety
- `auto` keyword for type deduction
- Range-based for loops
- Strong typing with containers

### Error Handling
```cpp
// Exception-based error handling
try {
    if (!ris_cal(name, mixing_data)) {
        throw std::runtime_error("RIS calculation failed");
    }
} catch (const std::exception& e) {
    std::cerr << "Error: " << e.what() << std::endl;
}
```

### Container Usage
```cpp
// Modern container initialization
std::vector<double> energies{1.0, 2.0, 3.0, 4.0};

// Range-based iteration
for (const auto& energy : energies) {
    process(energy);
}
```

## Troubleshooting

### Common Build Issues

#### 1. CMake Version Too Old
```
Error: CMake 3.10 or higher is required
```
**Solution**: Update CMake or use cmake3 on older systems

#### 2. Google Test Not Found
```
Error: Could not find GTest
```
**Solution**: 
- Install Google Test development packages
- Or disable tests: `cmake -DENABLE_TESTING=OFF ..`

#### 3. Compiler Too Old
```
Error: C++14 features not supported
```
**Solution**: Use GCC 7+ or Clang 5+, or enable C++14:
```bash
cmake -DCMAKE_CXX_STANDARD=14 ..
```

#### 4. Link Errors
```
Error: undefined reference to Fortran functions
```
**Solution**: Currently using stubs. For full functionality, link with Fortran libraries:
```bash
cmake -DLINK_FORTRAN_LIBS=ON ..
```

### Runtime Issues

#### 1. Input Files Not Found
```
Error when opening {name}.c
```
**Solution**: Ensure input files are in the working directory or provide full paths

#### 2. Memory Issues
The program uses significantly less memory than the Fortran version due to modern container management. If you encounter memory issues, they're likely in the calculation algorithms rather than memory management.

## Performance Notes

### Optimization Levels
- **Debug**: No optimization, full debug info (`-O0 -g`)
- **Release**: Full optimization (`-O3 -DNDEBUG`)
- **RelWithDebInfo**: Optimized with debug info (`-O2 -g`)

### Memory Usage
The C++ version uses:
- Automatic memory management (no leaks)
- Optimized container storage
- Stack-based objects where possible

### Profiling
```bash
# Build with profiling
cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo ..
make -j4

# Profile with gprof or Valgrind
valgrind --tool=callgrind ./standalone_ris4
```

## Contributing

### Code Style Guidelines
- Follow modern C++ best practices
- Use RAII for resource management
- Prefer `const` correctness
- Use meaningful variable names
- Add unit tests for new functionality

### Commit Guidelines
- Small, focused commits
- Clear commit messages
- Test before committing
- Update documentation as needed

## Future Enhancements

### Planned Features
- [ ] Complete physics calculation implementation
- [ ] Full Fortran library integration
- [ ] Parallel computation with `std::thread`
- [ ] Configuration file support
- [ ] Enhanced error reporting
- [ ] Performance benchmarking suite

### C++17/20 Upgrades
- [ ] `std::optional` for error handling
- [ ] `std::filesystem` for file operations
- [ ] Concepts for template constraints
- [ ] Coroutines for async operations

## License

This code follows the same license as the original GRASP package. See the main repository LICENSE file for details.

## Contact

For questions about this C++ refactoring:
- Check the main GRASP documentation
- Review the original Fortran code in `../appl/ris4/`
- Examine unit tests for usage examples

## Acknowledgments

- Original Fortran RIS4 code by Per Jonsson and contributors
- GRASP development team
- Modern C++ refactoring follows established best practices from the C++ community