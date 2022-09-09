# Tally Extract 
[![release](https://github.com/kclapper/tally-extract/actions/workflows/release.yml/badge.svg)](https://github.com/kclapper/tally-extract/actions/workflows/release.yml)

This is a command line program used to extract tally data from MCNP
output files. It is primarily used by the Reactor Engineer at the MIT
Nuclear Reactor Laboratory to help analyze data generated in the course
of reactor physics simulations.

## Usage
From the command line run `tally-extract`. It takes the path to an MCNP
output file as it's argument and generates an Excel workbook as it's
output. You can optionally specify the output file name to write.

    tally-extract -o [output-file] <input-file>
    
## Installation
From the releases tab on this repository, download either the `tally-extract`
file for Linux systems or the `tally-extract.exe` file for Windows systems.
These only support x86_64 based machines. Place the downloaded files 
somewhere on your `$PATH`.

## Bugs
All bug reports and suggestions should be submitted by opening an issue
in this repository.

## Limitations
This program can only extract a small subset of tally types from MCNP 
output files. As a result, the general MCNP community should NOT use this
program, it likely won't be able to do what you want it to.
