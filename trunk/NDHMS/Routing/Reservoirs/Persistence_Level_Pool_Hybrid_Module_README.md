# Peristence-Level Pool Hybrid Module

### General Purpose and Functionality

The purpose of this module is to persist observed reservoir discharges in order to improve the accuracy of discharge predictions. This
module simultaneously runs the Level Pool module to calculate a release at each timestep.

The observed discharge and the calculated
level pool release are each multiplied by a fractional weight and then summed to give a final calculated release. The fractional weights
are determined by the length of model time between the observed discharge and the current release calculation. For example, if a release
is calculated 12 hours in model time after an observation is read, then a weight of 1.0 might be applied to this observed discharge, and
a corresponding weight of 0.0 would be applied to the level pool calculated release. The resulting summed release would be the same as
the observed discharge. If a release is calculated 5 days in model time after an observation is read, then a weight of 0.2 might be
applied to the observed discharge, and a corresponding weight of 0.8 would be applied to the level pool calculated release. These weighted
values would then be summed to give a final release. If observations are missing or are not good quality for a given time window, then
full weight of 1.0 is given to the level pool release.

Partial autocorrelation is performed in advance from historical reservoir data to determine the appropriate weights for each reservoir.
These weights are read in from a reservoir persistence parameter file whenever a reservoir of this type is initialized.

Before the final calculated release/outflow is returned from this module back to the model at each timestep, mass balance checks are
performed to ensure the calculated release does not cause the reservoir storage to fall below the minimum or exceed the max, and the
release is modified accordingly.


### Input Parameters

This module requires five input parameters that are set in hydro.namelist.

* ```reservoir_persistence``` is a boolean parameter that will need to be set to ```.TRUE.``` for this module to be activated. This will cause the model to read the Q_Type variable
from LAKEPARM.nc, the lake parameter file. The Q_Type for a hybrid reservoir is currently set to '2' in the lake parameter file.

* ```reservoir_persistence_parameter_file``` is the NetCDF parameter file that holds the weights and corresponding gage ID for each lake ID.

* ```reservoir_timeslice_path``` is the path to all the
timeslice files used by this module.

* ```reservoir_observation_lookback_hours``` is an integer parameter that specifies how many hours before the model start time the module will
search for a corresponding timeslice file, and '18' would be a typical value and is the default.

* ```reservoir_observation_update_time_interval_seconds``` is an integer parameter that determines how often the reservoirs will look for a new timeslice
observation, and '86400', the number of seconds in a day, would be a typical value and is the default. If the model is set to run any
type of forecast, then this parameter will automatically be set to 1,000,000,000
seconds in order to prevent the module from reading any new timeslice observations after the first timestep.


### Module Architecture

This module builds off of the same class structure and architecture of the Level Pool module. As in Level Pool, each reservoir in this
module is instantiated into an object at model initialization. The Persistence_Level_Pool_Hybrid directory contains
module_persistence_levelpool_hybrid.F, module_persistence_levelpool_hybrid.F, module_persistence_levelpool_hybrid_parameters.F,
module_persistence_levelpool_hybrid_state.F, module_persistence_levelpool_hybrid_tests.F, and a Makefile.


The module_persistence_levelpool_hybrid.F module defines and instantiates objects for a hybrid persistence levelpool type
reservoir. The module_RT.F module will call and pass parameters to the constructor in this module to instantiate the hybrid reservoir
object and its sub-objects. The hybrid reservoir struct inherits input and output stucts from the reservoir base module and calls
instantiation of these into sub-objects. The hybrid reservoir struct also points to structs for hybrid parameters and state and calls
instantiation of these into sub-objects. A pointer to a levelpool reservoir object is also held in state, and this module
instantiates that levelpool object. There is also a subroutine to run hybrid release that is derived from the reservoir base
struct interface to run release. The release function will periodically call a function in module_read_timeslice_data.F that
will read a timeslice file and return a corresponding observed discharge. The timeslice files will be read at a particular update
time. The first hybrid reservoir on each processor to reach an update time will call the function to read the timeslice files, and
the resulting observations will be used by each reservoir on that processor. The release function also peforms the functionality
described above including calling level pool release along with weighting and combining that release with the weighted observed
discharge, and finally calling mass balance checks before returning the release/outflow back to the model.

The module_persistence_levelpool_hybrid_parameters.F module defines and instantiates objects for a hybrid type reservoir's
parameters/properties. Parameters holds static/unchanging variables that are set when the given reservoir object is
initialized/instantiated.

The module_persistence_levelpool_hybrid_state.F module defines and instantiates objects for a hybrid type reservoir's state.
State holds and tracks dynamic/changing variables that are only relevant to the given hybrid reservoir object and not other
modules or areas of the system.

The module_persistence_levelpool_hybrid_tests.F module holds unit tests that test for all components of a hybrid reservoir
are properly initialized.

Within the Reservoirs directory, the module_read_timeslice_data.F module reads USGS timeslice files to get gage discharge
values that will be used by reservoirs. Future development will either expand this module or replicate its functionality to
another module to read in other forms of observation and forecast data. An observation lookback period is passed in to
determine how far back in time from the current model time the module will look for timeslice files. The observation
resolution determines the time increments the module will look back. For instance, a standard lookback period would be
18 hours with an observation resolution of 15 minutes, where a model current time of 8:00 PM would search for timeslice
files at every 15 minute increment between 2:00 AM and 8:00 PM that day. The module will first search for the most recent
timeslice files and grab the discharge for a particular lake/reservoir if the gage quality standard is met at that time.
If a gage discharge is missing or if the gage quality standard is not met for any particular lake/reservoir in the given
timeslice file, the module will continue to look back at every observation resolution increment until either all
lakes/reservoirs have a good quality discharge or the end of the lookback period is reached. The total lookback seconds
from current model time that the discharge is read will also be returned.

Also within the Reservoirs directory, the module_reservoir_utilities.F module contains multiple functions used by the Hybrid
module. The modify_for_projected_storage function is called from the hybrid release function and modifies the outflow if a
projected storage falls below the minimum or exceeds the maximum for the reservoir. There are also multiple functions used
for reading variables from the reservoir persistence parameter and timeslice NetCDF files.

### Testing

To compile and run the unit tests, go to the Reservoir directory in the terminal and type

```
make test
```

and hit enter. Then type

```
./reservoir_tests
```

and hit enter.
The user should see "All Reservoir Tests Passed".

