# To run the tests included in the wrf_hydro_nwm_public/tests directory:

# Requirements:
All requirements for WRF-Hydro
Docker with the following docker images:
  wrfhydro/dev:conda with wrfhydropy=0.0.2
  wrfhydro/domains:croton_NY

# Commands to run testing
## Get docker images
docker pull wrfhydro/dev:conda
docker pull wrfhydro/domains:croton_NY

## Create data volume
docker create --name croton_NY wrfhydro/domains:croton_NY

## Start docker
docker run --volumes-from croton_NY -v YOUR_WRF_HYDRO_NWM_CODE_DIRECTORY:/home/docker/wrf_hydro_nwm_public -it wrfhydro/dev:conda

### Running inside of docker issue the following commands
#### Install correct version of wrfhydropy
pip uninstall -y wrfhydropy
pip install wrfhydropy=0.0.2

#### Change directory to wrf_hydro_nwm and run tests
cd /home/docker/wrf_hydro_nwm_public
pytest -v --domain_dir=/home/docker/domain/croton_NY 
          --candidate_dir=YOUR_REFERENCE_CODE_DIRECTORY/trunk/NDHMS
          --reference_dir=YOUR_CANDIDATE_CODE_DIRECTORY/trunk/NDHMS 
          --output_dir=/home/docker/test_out"
