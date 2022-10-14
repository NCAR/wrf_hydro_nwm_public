from boltons import iterutils
import os
import pathlib
import subprocess
from wrfhydropy import JSONNamelist

# Example: python get_config_file_meta_data
#
# 1) Run script file in-place, in this directory, as above.
# 2) Typically, this will be run on cheyenne since that's where CONUS domain
#    files will live.
# 3) Configure the list of domains paths and configs below.
# 4) The dirs in this directory must be removed to be refreshed.
# 5) This script skips non-existent files and ignores timeslices.

domain_paths = [
    "/glade/p/cisl/nwc/model_testing_domains/croton_NY"
]

configs = [
    'nwm_ana',
    'nwm_hi_ana',
    'nwm_long_range'
]

# -----------------------------------
domain_paths = [pathlib.PosixPath(pp) for pp in domain_paths]
this_path = pathlib.PosixPath(os.getcwd())
code_path = this_path.parent.parent / 'trunk/NDHMS/'

def get_nlst_file_meta(
    namelist: dict,
    dom_dir: str
):

    def visit_file(path, key, value):

        # Only treat strings
        if type(value) is not str:
            return False

        # Convert to pathlib object, kee        
        the_file_rel = pathlib.PosixPath(value)
        the_file_abs = dom_dir / the_file_rel

        # Dirs: get the md5sum of every file inside
        if the_file_abs.is_dir():
            if the_file_abs.name not in ['FORCING','nudgingTimeSliceObs']:
                return False

            print('            ' + str(the_file_abs))
            if the_file_abs.exists() is False:
                raise ValueError("The file does not exist: " + str(the_file_abs))
            
            meta_path_rel = the_file_rel
            the_cmd = 'meta_path=' + str(meta_path_rel) + ".md5"
            the_cmd += ' && data_path=' + str(the_file_abs)
            the_cmd += ' && md5sum $data_path/* > $meta_path'
            proc = subprocess.run(
                the_cmd,
                cwd=config_dir,
                shell=True,
                executable='/bin/bash'
            )
            return True

        # Regular files...
        print('            ' + str(the_file_abs))

        if the_file_abs.exists() is False:
            raise ValueError("The file does not exist: " + str(the_file_abs))
        
        # The sub process command is executed in the root of the meta path,
        # use the relative data path/
        meta_path_rel = the_file_rel
        if meta_path_rel.suffix == '.nc':
            meta_path_base = meta_path_rel.with_suffix("")
        else:
            meta_path_base = meta_path_rel
        the_cmd = 'meta_path=' + str(meta_path_rel)
        the_cmd += ' && meta_path_md5=' + str(meta_path_base) + '.md5'
        the_cmd += ' && meta_path_cdl=' + str(meta_path_base) + '.cdl'
        the_cmd += ' && data_path=' + str(the_file_abs)
        the_cmd += ' && mkdir -p $(dirname $meta_path)'
        the_cmd += ' && md5sum $data_path > $meta_path_md5'
        the_cmd += ' && ncdump -h $data_path > $meta_path_cdl'
        proc = subprocess.run(
            the_cmd,
            cwd=config_dir,
            shell=True,
            executable='/bin/bash'
        )
        
        return True

    _ = iterutils.remap(namelist, visit=visit_file)

    
for dd in domain_paths:

    print('')
    print('Domain: ' + str(dd))

    domain_tag = dd.name
    
    for cc in configs:

        # Make a meta data output dir for each configuration.
        config_dir = (this_path / domain_tag) / cc
        
        # Create the namelists
        domain_nlsts = ['hydro_namelists.json', 'hrldas_namelists.json']
        code_nlsts = ['hydro_namelist_patches.json', 'hrldas_namelist_patches.json']
        file_names = ['hydro.namelist', 'namelist.hrldas']
        
        for code, dom, ff in zip(domain_nlsts, code_nlsts, file_names):

            domain_namelists = JSONNamelist(dd / dom)

            # If the configuartion is not found in the domain, skip it silently
            try:
                domain_config = domain_namelists.get_config(cc)
            except KeyError:
                continue

            if not config_dir.exists() and ff == file_names[0]:
                config_dir.mkdir(parents=True, exist_ok=False)
                print('')
                print('    Config: ' + str(cc))

            print('        Namelist: ' + str(ff))
            
            repo_namelists = JSONNamelist(code_path / code)
            repo_config = repo_namelists.get_config(cc)               
            patched_namelist = repo_config.patch(domain_config)

            # Write them out for completeness.
            patched_namelist.write(str(config_dir / ff))         
            
            # This function does the work.
            get_nlst_file_meta(patched_namelist, dd)


