from boltons import iterutils
import os
import pathlib
import shlex
import subprocess
from wrfhydropy import JSONNamelist


# 1) Run script file in-place.
# 2) Typically, this will be run on cheyenne since that's where CONUS domain
# files will live.
# 3)

domain_paths = [
    "/glade/work/jamesmcc/domains/public/croton_NY_v5.0.1",
    "/glade/work/jamesmcc/domains/private/CONUS"
]

configs = [
    'nwm_ana',
    'nwm_medium_range'
]


domain_paths = [domain_paths[0]]
configs = [configs[0]]

# -----------------------------------
domain_paths = [pathlib.PosixPath(pp) for pp in domain_paths]
this_path = pathlib.PosixPath(os.getcwd())
code_path = this_path.parent.parent / 'trunk/NDHMS/'


def get_file_meta(
    namelist: dict,
    dom_dir: str
):

    def visit_is_file(path, key, value):
        if value is None:
            return False
        return type(value) is str or type(value) is dict

    def visit_not_none(path, key, value):
        return bool(value)

    def visit_str_posix_exists(path, key, value):
        if type(value) is dict:
            return True
        #return key, (dom_dir / pathlib.PosixPath(value)).exists()
        return key, (dom_dir / pathlib.PosixPath(value))

    def remap_nlst(nlst):
        # The outer remap removes empty dicts
        files = iterutils.remap(nlst,  visit=visit_is_file)
        files = iterutils.remap(files, visit=visit_not_none)
        exists = iterutils.remap(files, visit=visit_str_posix_exists)
        return exists

    file_dict = remap_nlst(namelist)

    def visit_missing_file(path, key, value):
        #print(path, key, value)
        if value.name == 'nudgingTimeSliceObs':
            return False
        data_path = value
        meta_path = '/'.join(str(value).split('/')[-3:])
        the_cmd = 'meta_path=' + meta_path
        the_cmd += ' && data_path=' + str(data_path)
        the_cmd += ' && mkdir -p $(dirname $meta_path)'
        the_cmd += ' && echo "md5sum: $(md5sum $data_path)" > $meta_path'
        the_cmd += ' && echo "ncdump -h: $(ncdump -h $data_path)" >> $meta_path'
        #subprocess.run(shlex.split(the_cmd), cwd=this_path)#, shell=True, executable='/bin/bash')
        subprocess.run(
            the_cmd,
            cwd=this_path / domain_tag,
            shell=True,
            executable='/bin/bash'
        )
        return True

    bob = iterutils.remap(file_dict, visit=visit_missing_file)
    fooo
         

for dd in domain_paths:

    domain_tag = dd.name
    
    for cc in configs:
        
        config_dir = (this_path / domain_tag) / cc
        config_dir.mkdir(parents=True, exist_ok=False)
        
        # Create the namelists
        
        domain_nlsts = ['hydro_namelists.json', 'hrldas_namelists.json']
        code_nlsts = ['hydro_namelist_patches.json', 'hrldas_namelist_patches.json']
        file_names = ['hydro.namelist', 'namelist.hrldas']
        
        for code, dom, ff in zip(domain_nlsts, code_nlsts, file_names):
            
            repo_namelists = JSONNamelist(code_path / code)
            domain_namelists = JSONNamelist(dd / dom)
            
            repo_ana = repo_namelists.get_config(cc)
            domain_ana = domain_namelists.get_config(cc)
            
            patched_namelist = repo_ana.patch(domain_ana)

            # Write them out for completeness.
            patched_namelist.write(str(config_dir / ff))

            # All files, 
            get_file_meta(patched_namelist, dd)


