# RFC Forecasts Module

### General Purpose and Functionality

The purpose of this module is to integrate RFC (River Forecast Center) produced forecasts for individual reservoirs into
the Reservoir module in real-time. This utilizes the expertise that each RFC has for forecasting reservoirs in their domain.
This module replaces the normal Level Pool object for a particular reservoir that is selected to run as RFC_Forecast.


### Module Architecture

This module builds off of the same class structure and architecture of the Level Pool module. As in Level Pool, each reservoir in this
module is instantiated into an object at model initialization. The **RFC_Forecasts** directory contains the following files:

* **module_rfc_forecasts.F** defines and instantiates objects for an rfc forecasts type
reservoir. **module_RT.F** will call and pass parameters to the constructor in this module to instantiate the rfc forecasts reservoir
object and its sub-objects. The rfc forecasts reservoir type inherits input and output types from the reservoir base module and calls
instantiation of these into sub-objects. The rfc forecasts reservoir type also points to types for parameters and state and calls
instantiation of these into sub-objects.

* **module_rfc_forecasts_parameters.F** defines and instantiates objects for an rfc forecasts type reservoir's
parameters/properties. Parameters holds static/unchanging variables that are set when the given reservoir object is
initialized/instantiated.

* **module_rfc_forecasts_state.F** defines and instantiates objects for an rfc forecasts type reservoir's state.
State holds and tracks dynamic/changing variables that are only relevant to the given reservoir object and not other
modules or areas of the system.

* **module_rfc_forecasts_tests.F** holds unit tests that test for all components of an rfc forecasts reservoir
are properly initialized.

* **module_reservoir_read_rfc_time_series_data.F**, within the **Reservoirs** directory, reads RFC time series forecast files for an
array of time series forecast values.

### Input Parameters

This module requires one input parameter that is set in hydro.namelist.
* ```reservoir_rfc_forecasts``` is a boolean parameter that will need to be set to ```.TRUE.``` for this module to be activated.
This will set the variable ```reservoir_type_specified``` to ```TRUE``` and cause the model to read the reservoir_type variable from the
reservoir parameter file. The reservoir_type for a RFC (River Forecast Center) Forecast reservoir is currently set to '4' in the reservoir
parameter file.
