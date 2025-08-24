# RIS Test Project

This repository seems to be a test environment for a GRASP (General-purpose Relativistic Atomic Structure Package) project, possibly focusing on relativistic isotope shift (RIS) calculations.

## Project Overview

The `ris_test` directory likely contains components or a specific configuration of the GRASP package. Based on the file structure (`src/ris_cpp/`) and the presence of `ris_workflow.md` and `CLAUDE.md`, this project appears to be a focused instance perhaps for development or testing of calculation workflows related to isotope shifts.

## Build Commands

To build this project, navigate to the project's root directory and execute the following commands:

```bash
./configure.sh
cd build && make install
```

For parallel compilation, replace `make install` with `make -jN install`, where `N` is the number of cores to use (e.g., `make -j4 install`).

## Code Architecture

The project uses a modular Fortran 90 architecture, typical of GRASP. Key components would involve:

- **Core Libraries**: Located in `src/lib/`, these provide fundamental functionalities like `libmod`, `lib9290`, etc.
- **Applications**: `src/appl/` would contain applications such as `ris4` which is likely related to isotope shift calculations.
- **C++ Integration**: The presence of `src/ris_cpp/` suggests there might be C++ components or wrappers interacting with the Fortran codebase, possibly for specific parts of the isotope shift calculation or for interfacing with other systems.

## Dependencies

- **Fortran 90+ compiler**: Such as `gfortran`.
- **BLAS/LAPACK libraries**: For linear algebra operations.
- **CMake 3.6+**: For managing the build process.

## Development Workflow

- The `configure.sh` script sets up the build environment. 
- Fortran modules often separate interface (`_I.f90`) and implementation (`_C.f90`) files.

## What is ris_workflow.md?

`ris_workflow.md` likely describes the specific workflow or sequence of steps for performing relativistic isotope shift calculations within this test environment. It might detail how to prepare input, run the `ris4` application, and interpret results.

## What is CLAUDE.md?

`CLAUDE.md` provides specific instructions and context to the Claude Code AI assistant, detailing project structure, build procedures, and general guidance for interacting with this repository. It serves as a directive for the AI to understand and operate within the project's conventions and scope.
