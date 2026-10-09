#!/usr/bin/python3
import wrf_hydro_tools
import ctypes as ct
import numpy as np

import argparse
import sys


def main():
    print('--- Starting Python Decomposition Tool---')
    np_s, python_index = parseCLA()
    np_int = int(np_s)

    print("- Reporting decomposition of", np_int, "processes")
    if (python_index):
        print("- Using Python indexing 0,...,n-1")
    else:
        print("- Using Fortran indexing 1,...,n")
    print("")

    x_np, y_np, nx, ny, start_x, start_y, end_x, end_y  = \
        wrf_hydro_tools.get_domain_decomposition(np_int)

    # if python indexing, index starts at 0 instead of 1
    if (python_index):
        start_x -=1
        start_y -=1
        end_x -=1
        end_y -=1

    print('--- Finished calling get_domain_decomposition, result are:')
    print('x_np =', x_np)
    print('y_np =', y_np)
    print('nx =', nx)
    print('ny =', ny)
    print("start_x =", start_x)
    print("start_y =", start_y)
    print("end_x =", end_x)
    print("end_y =", end_y)


# parse command line arguments
def parseCLA():
    parser = argparse.ArgumentParser(description="Report domain decomposition from WRF-Hydro.")
    parser.add_argument("-np", help="Number of processes")
    parser.add_argument("-p", "--python_index", action='store_true',
                       help="Python index 0...n-1, instead of Fortran 1...n")

    # Parse the arguments
    args = parser.parse_args()

    return args.np, args.python_index


if __name__ == "__main__":
    main()
