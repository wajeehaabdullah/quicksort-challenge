# Fortran Quicksort Challenge

## Overview
A Fortran module implementing iterative Quicksort with strict memory constraints (no internal allocation, PURE procedures).

## Requirements
* GNU Fortran 15
* Docker (for reproducible build environment)

## Build & Test
1.  **Build Docker Image:**
    `docker build -t arch-gfortran -f gfortran.docker .`

2.  **Run Tests:**
    `docker run --rm -it -v "$(pwd):/opt" arch-gfortran fpm test`