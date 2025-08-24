# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains a GRASP (General-purpose Relativistic Atomic Structure Package) project focused on atomic physics calculations.

## Build Commands

### GRASP Main Package
```bash
# CMake build (recommended)
./configure.sh
cd build && make install

# Parallel build
cd build && make -jN install # Replace N with number of cores

# Test
cd build && ctest
```

## Code Architecture

The main GRASP package follows a modular Fortran 90 architecture. Key components include:

**Core Libraries** (`src/lib/`):
- `libmod`
- `lib9290`
- `libdvd90`
- `libmcp90`
- `librang90`
- `mpi90`

**Applications** (`src/appl/`):
- Fortran 90 applications like `ris4` are located here.

## Build Dependencies

**Required:**
- Fortran 90+ compiler (gfortran)
- BLAS/LAPACK libraries
- CMake 3.6+

## Development Workflow

### Compiler Configuration
- Default compiler settings are configured by `configure.sh`.

### File Organization
- Modules often have `_I.f90` (interface) and `_C.f90` (implementation) files.

## Typical Workflow

Build the project using `configure.sh` and then `make install` in the `build/` directory.
