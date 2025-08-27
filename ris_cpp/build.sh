#!/bin/bash

# RIS4 C++ Build Script
# Automated build script for the C++ refactored RIS4 program

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if we're in the right directory
if [[ ! -f "CMakeLists.txt" || ! -f "main.cpp" ]]; then
    print_error "This script must be run from the ris_cpp directory"
    exit 1
fi

# Parse command line arguments
BUILD_TYPE="Release"
CLEAN_BUILD=false
RUN_TESTS=false
VERBOSE=false
HELP=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --debug)
            BUILD_TYPE="Debug"
            shift
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --test)
            RUN_TESTS=true
            shift
            ;;
        --verbose)
            VERBOSE=true
            shift
            ;;
        --help|-h)
            HELP=true
            shift
            ;;
        *)
            print_error "Unknown option: $1"
            HELP=true
            break
            ;;
    esac
done

if [[ "$HELP" == true ]]; then
    echo "RIS4 C++ Build Script"
    echo ""
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  --debug     Build in Debug mode (default: Release)"
    echo "  --clean     Clean build directory before building"
    echo "  --test      Run tests after successful build"
    echo "  --verbose   Enable verbose build output"
    echo "  --help|-h   Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                    # Standard release build"
    echo "  $0 --debug --test    # Debug build with tests"
    echo "  $0 --clean --verbose # Clean verbose build"
    exit 0
fi

print_status "Building RIS4 C++ ($BUILD_TYPE mode)"

# Clean build if requested
if [[ "$CLEAN_BUILD" == true ]]; then
    print_status "Cleaning build directory..."
    rm -rf build
fi

# Create build directory
print_status "Setting up build directory..."
mkdir -p build
cd build

# Check for required tools
if ! command -v cmake &> /dev/null; then
    print_error "CMake is required but not installed"
    print_status "Install with: sudo apt install cmake  (Ubuntu/Debian)"
    print_status "            : sudo dnf install cmake  (Fedora/CentOS)"
    print_status "            : brew install cmake      (macOS)"
    exit 1
fi

# Configure with CMake
print_status "Configuring with CMake..."
CMAKE_ARGS="-DCMAKE_BUILD_TYPE=$BUILD_TYPE"

if [[ "$VERBOSE" == true ]]; then
    CMAKE_ARGS="$CMAKE_ARGS -DCMAKE_VERBOSE_MAKEFILE=ON"
fi

if cmake $CMAKE_ARGS .. 2>/dev/null; then
    print_success "CMake configuration completed"
else
    print_error "CMake configuration failed"
    print_warning "Make sure you have all required dependencies installed"
    print_warning "Check the README.md for installation instructions"
    exit 1
fi

# Build
print_status "Compiling..."
MAKE_ARGS=""
if [[ "$VERBOSE" == true ]]; then
    MAKE_ARGS="VERBOSE=1"
fi

# Use parallel build with number of CPU cores
NCORES=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2)
print_status "Using $NCORES parallel jobs"

if make -j$NCORES $MAKE_ARGS; then
    print_success "Build completed successfully!"
else
    print_error "Build failed"
    exit 1
fi

# Check if executables were created
if [[ -f "standalone_ris4" ]]; then
    print_success "Main executable: standalone_ris4"
else
    print_error "Main executable was not created"
    exit 1
fi

if [[ -f "run_tests" ]]; then
    print_success "Test executable: run_tests"
    
    # Run tests if requested
    if [[ "$RUN_TESTS" == true ]]; then
        print_status "Running tests..."
        if ./run_tests; then
            print_success "All tests passed!"
        else
            print_error "Some tests failed"
            exit 1
        fi
    fi
else
    print_warning "Test executable was not created (Google Test may not be installed)"
fi

print_success "Build process completed!"
print_status "Executables are in the build/ directory:"
print_status "  ./standalone_ris4  - Main RIS4 program"
if [[ -f "run_tests" ]]; then
    print_status "  ./run_tests        - Unit tests"
fi

echo ""
print_status "To run the program:"
print_status "  cd build && ./standalone_ris4"
echo ""
print_status "For help:"
print_status "  $0 --help"