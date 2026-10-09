#!/usr/bin/env python3
import os
import sys
current_dir = os.path.dirname(os.path.abspath(__file__))
tools_dir = os.path.abspath(os.path.join(current_dir, '../../tools'))
sys.path.insert(0, tools_dir)

import wrf_hydro_tools
import ctypes as ct
import numpy as np
import argparse

# requirement for MODFLOW decomposition
from pathlib import Path
from tempfile import TemporaryDirectory
import matplotlib.pyplot as plt
import numpy as np
import flopy
from flopy.mf6.utils import Mf6Splitter
from flopy.plot import styles
from flopy.utils.geometry import LineString, Polygon


def main():
    print('- Starting Python Decomposition Tool -')
    np_s, verbose = parseCLA()
    np_int = int(np_s)
    python_index = True

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
    if verbose:
        print("start_x =", start_x)
        print("start_y =", start_y)
        print("end_x =", end_x)
        print("end_y =", end_y)

    print("Creating new domain for", np_int, "processes")
    print("-- load case --")
    cwd = os.getcwd() # directory from where the script was called
    simulation_ws = Path(cwd)

    sim = flopy.mf6.MFSimulation.load(sim_ws=simulation_ws)
    workspace = Path("./split_domain"+"_"+np_s+"np")

    sim.set_sim_path(workspace)
    sim.write_simulation()

    # success, buff = sim.run_simulation(silent=True)
    # assert success

    gwf = sim.get_model()
    modelgrid = gwf.modelgrid

    print("-- split MODFLOW domain --")
    n = modelgrid.ncol
    m = modelgrid.nrow
    # array = np.ones((modelgrid.nrow, modelgrid.ncol), dtype=int)

    print("np =", np_int)
    print("ncol x mrow", modelgrid.ncol, modelgrid.nrow)
    # Create an n by m array filled with zeros
    array = np.zeros((m, n), dtype=int)
    array[:,:] = -999

    # --- Fill the array with values from 1 to np in subsquares
    for p in range(np_int):
        print("p =", p+1)
        p_start_x = start_x[p]
        p_start_y = start_y[p]
        p_end_x = end_x[p] + 1
        p_end_y = end_y[p] + 1
        if verbose:
            print("process", p, "bounds:",
                  p_start_x,":",p_end_x,",", p_start_y,":",p_end_y)
        # WRF-Hydro y index starts at the south edge, MODFLOW row 0 is the
        # north edge, so mirror the y range
        array[m - p_end_y:m - p_start_y, p_start_x:p_end_x] = p+1

    if np.any(array == -999):
        print("Decomposition has failed")
        print(array)
        sys.exit()

    # split the simulation
    mfsplit = Mf6Splitter(sim)
    new_sim = mfsplit.split_model(array)
    print("-- Writing split domain to:", workspace)
    new_sim.set_sim_path(workspace)
    new_sim.write_simulation()

    # write image of decomposition
    decomp_img = str(workspace)+'/decomposition_'+np_s+'np.png'
    plt.imshow(array[:,:], cmap='viridis')
    plt.colorbar()
    plt.savefig(decomp_img)
    print("-- image of decompisition save to ", decomp_img)

    print("- Decomposition Finished")



# parse command line arguments
def parseCLA():
    parser = argparse.ArgumentParser(
        description="Report domain decomposition from WRF-Hydro.")
    parser.add_argument("-np", help="Number of processes", required=True)
    parser.add_argument("-v", "--verbose", help="Verbose mode",
                        action='store_true')
    # parser.add_argument("-p", "--python_index", action='store_true',
    #                    help="Python index 0...n-1, instead of Fortran 1...n")

    # Parse the arguments
    args = parser.parse_args()

    return args.np, args.verbose


if __name__ == "__main__":
    main()
