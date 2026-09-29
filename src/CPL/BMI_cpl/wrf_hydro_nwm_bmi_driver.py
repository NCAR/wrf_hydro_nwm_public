#!/usr/bin/env python3
import bmi_wrf_hydro_nwm as wrf_hydro
from bmi_wrf_hydro_nwm import bmi
import ctypes as ct


def main():
    print('--- Running in Python---')

    name = wrf_hydro.get_component_name()
    print("Starting", name)

    wrf_hydro.initialize()

    current_time = wrf_hydro.get_start_time()
    end_time = wrf_hydro.get_end_time()
    print("start time is", current_time, "end time is", end_time)

    while (current_time < end_time):
        wrf_hydro.update()
        current_time = wrf_hydro.get_current_time()
        if current_time%10 == 0:
            print("time =", current_time)

    wrf_hydro.finalize()
    print('--- Finished running Python test driver ---')

if __name__ == "__main__":
    main()
