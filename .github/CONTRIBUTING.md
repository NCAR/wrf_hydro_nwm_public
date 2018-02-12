#  Contributing to WRF-Hydro
## The contribution process
The general process of contributing code changes to the WRF-Hydro project is as follows:
1. Submit an issue or contact a WRF-Hydro developer to discuss your proposed changes and ensure they can be integrated into the codebase (see the [section below](#the-two-repository-system))
2. Fork the NCAR/wrf_hydro_nwm_public repository
3. Clone your forked repository, create a new branch, and make changes
4. Test your changes
5. Submit a pull request to NCAR/wrf_hydro_nwm_public

Additional guidance regarding the contribution process, including testing procedures, as well as a project style guide will be added in the upcoming weeks.

## The two repository system
For intellectual property reasons, we are required to maintain two GitHub repositories:
* NCAR/wrf_hydro_nwm, a private repository containing ongoing development for upcoming versions of the National Water Model
* NCAR/wrf_hydro_nwm_public, a public repository where all community development takes place and public versions of the code are released

In order to keep the private repository up to date with ongoing development in the public repository and ensure that changes can be integrated into the codebase without causing problems, pull requests to the public repository will need to be tested in the private NCAR/wrf_hydro_nwm repository before being merged.  To facilitate this testing we require contributors to work with a point person at NCAR.  We suggest either opening an issue or contacting us via the [support form](https://ral.ucar.edu/projects/wrf_hydro/contact) on our website to initiate this process.
