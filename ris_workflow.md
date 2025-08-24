### RIS4 Application Module Execution Flow

The `ris4` application, based on the `ris.f90` source file, is a Fortran 90 program designed for calculating isotope shift (RIS) parameters, electron density at the origin, and radial expectation values.

#### `ris.f90` (Main Program `RIS`)

1.  **Initialization:** Sets a `CUTOFF` value, prompts for default settings and the name of the state.
2.  **Setup and Data Loading:** Calls subroutines like `SETDBG`, `SETMC`, `SETCON`, `SETSUM`, `SETCSLA`, `GETSMD`, `GETMIXBLOCK`, and `FACTT` to set up parameters, load input data (from `.i`, `.csl` files, and eigenvectors), and prepare for calculations.
3.  **Core Calculation:** Calls the `RIS_CAL` subroutine to perform the main computational tasks.
4.  **Completion:** Prints a completion message and terminates.

#### `ris_cal.f90` (Subroutine `RIS_CAL`)

This subroutine controls the main sequence of calculations for isotope shift parameters.

1.  **Initialization and Memory Allocation:** Determines `NRNUC`, allocates and initializes dynamic arrays for various densities and matrices (e.g., `DENS1VEC`, `DINT1VEC`, `SMSC1`, `DENS1` to `DENS7`).
2.  **Integral Calculations:** Computes various integrals (`RINTDENS`, `RINTDENSVEC`, `RINTI_NMS`, `RINT`, `VINTI`, `RINT_SMS2`, `RINT_SMS3`) required for the calculations, often within nested loops iterating over wave functions.
3.  **Angular Data Handling:** Checks for and reads/generates one-body (`.IOB`) and two-body (`.ITB`) angular data using `ANGDATA`, `DENSREAD_SELTZ`/`DENSNEW_SELTZ`, and `SMSREAD`/`SMSNEW`.
4.  **Printouts:** Writes calculated results, including energy, mass shift parameters, electron density values, and field shift electronic factors, to an output file (unit 24).
5.  **Memory Deallocation:** Deallocates all dynamically allocated arrays and prints completion messages.

#### `ris_C.f90` and `ris_cal_I.f90`

*   `ris_C.f90`: Defines global `REAL(DOUBLE)` pointer arrays (`DENS1`-`DENS7`, `SMSC1`, `SMSC2`) used for shared data in `ris_cal`.
*   `ris_cal_I.f90`: Provides the interface for the `ris_cal` subroutine, specifying its `NAME` argument.