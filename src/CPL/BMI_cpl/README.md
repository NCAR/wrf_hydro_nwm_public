# WRF-Hydro BMI Coupling
The [Basic Model Interface](https://bmi.readthedocs.io/en/stable/) ([BMI](https://github.com/csdms/bmi)) is a standardized set of control and query functions.
This allows for different models to use the BMI functions to couple and interact together.

See [readthedocs BMI documentation](https://wrf-hydro.readthedocs.io/en/latest/appendices.html#a20-introduction-to-the-basic-model-interface-bmi)
  for instructions on building and running WRF-Hydro BMI.


### Exposing Additional Variables
Here are the steps for exposing additional WRF-Hydro variables to BMI.
Each new variable will need a grid rank,

1. The variable will need an identifying grid number. Add a new `case` block in the function `wrf_hydro_var_grid` and choose the next number.
2. Using that grid number or the variable name, add the variable case in the following functions:
   - `get_grid_rank`
   - `get_grid_shape`
   - `get_var_size`
   - `wrf_hydro_var_units`
   - `wrf_hydro_var_type`
   - `wrf_hydro_grid_type`
   - `wrf_hydro_set_{int,float,double}`
   - `wrf_hydro_get_{int,float,double}`
   - `wrf_hydro_set_at_indices_{int,float,double}`
   - `wrf_hydro_get_at_indices_{int,float,double}`
3. Add variable name to this README's "List variables exposed to the BMI" table


## License
BMI is open source software released under the [MIT License](LICENSE.txt).
