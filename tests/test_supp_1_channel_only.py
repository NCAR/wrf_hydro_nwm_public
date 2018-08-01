def test_order():
    pass

# # #################################
# # Channel-only tests below:
#
# # Channel-only Run
# def test_run_candidate_channel_only(
#     candidate_setup,
#     candidate_channel_only_setup,
#     output_dir,
#     job_default,
#     scheduler,
#     capsys
# ):
#
#     with capsys.disabled():
#         print("\nQuestion: The candidate channel-only mode runs successfully?", end='')
#
#     # Dont recompile the model, just use the candidate's model.
#     candidate_channel_only_setup.model = candidate_setup.model
#
#     # Set the forcing directory
#     candidate_channel_only_setup.namelist_hrldas['noahlsm_offline']['indir'] = \
#         str(output_dir / 'run_candidate')
#
#     # Set run directory
#     run_dir = output_dir / 'run_candidate_channel_only'
#
#     candidate_channel_only_job = job_default
#     candidate_channel_only_job.scheduler = scheduler
#
#     # Run
#     candidate_channel_only_run = wrfhydropy.WrfHydroRun(
#         wrf_hydro_setup=candidate_channel_only_setup,
#         run_dir=run_dir,
#         jobs=candidate_channel_only_job
#     )
#     check_run_dir = candidate_channel_only_run.run_jobs()
#
#     if scheduler is not None:
#         # This function waits for the completed run.
#         candidate_channel_only_run = \
#             wrfhydropy.job_tools.restore_completed_scheduled_job(check_run_dir)
#
#     # Check subprocess and model run status
#     assert candidate_channel_only_run.jobs_completed[0].exit_status == 0, \
#         "Candidate code run exited with non-zero status"
#     assert candidate_channel_only_run.jobs_completed[0].job_status == 'completed success', \
#         "Candidate code run did not complete"
#
#
# # Channel-only matches full-model?
# def test_channel_only_matches_full(
#     candidate_channel_only_setup,
#     output_dir,
#     capsys
# ):
#
#     if candidate_channel_only_setup is None:
#         pytest.skip("Unsupported configuration for channel-only.")
#
#     with capsys.disabled():
#         print("\nQuestion: The candidate channel-only run restarts and CHRTOUT files match " +
#               "those of the full model?",
#               end="")
#
#     # Check for existence of run objects
#     candidate_run_file = output_dir / 'run_candidate' / 'WrfHydroRun.pkl'
#     candidate_channel_only_run_file = output_dir / 'run_candidate_channel_only' / 'WrfHydroRun.pkl'
#
#     if candidate_run_file.is_file() is False:
#         pytest.skip('Candidate run object not found, skipping test')
#     if candidate_channel_only_run_file.is_file() is False:
#         pytest.skip('candidate_channel_only run object not found, skipping test')
#
#     # Load run objects
#     candidate_run_expected = pickle.load(open(candidate_run_file,"rb"))
#     candidate_channel_only_run_expected = pickle.load(open(candidate_channel_only_run_file,"rb"))
#
#     exclude_vars = [
#         'stc1',
#         'smc1',
#         'sh2ox1',
#         'stc2',
#         'smc2',
#         'sh2ox2',
#         'stc3',
#         'smc3',
#         'sh2ox3',
#         'stc4',
#         'smc4',
#         'sh2ox4',
#         'infxsrt',
#         'soldrain',
#         'sfcheadrt',
#         'QBDRYRT',
#         'infxswgt',
#         'sfcheadsubrt',
#         'sh2owgt1',
#         'sh2owgt2',
#         'sh2owgt3',
#         'sh2owgt4',
#         'qstrmvolrt',
#         'hlink',
#         'lake_inflort'
#     ]
#
#     # We still compare these:
#     # 'qlink1'
#     # 'qlink2'
#     # 'resht'
#     # 'qlakeo'
#     # 'z_gwsubbas'
#
#     # Dont compare metadata in this case, there are different dimensions
#     # in the files that always result in a return code of 1.
#     nccmp_options = ['--data', '--force', '--quiet'] #, '--metadata']
#
#     # Check diffs
#     regression_diffs = wrfhydropy.RestartDiffs(
#         candidate_run_expected,
#         candidate_channel_only_run_expected,
#         nccmp_options=nccmp_options,
#         exclude_vars=exclude_vars
#     )
#
#     # Check hydro restarts
#     for diff in regression_diffs.hydro:
#         if diff is not None:
#             with capsys.disabled():
#                 print(diff)
#         assert diff is None, \
#             "Candidate channel-only hydro restart files do not match full restart files"
#
#     # Check nudging restarts
#     for diff in regression_diffs.nudging:
#         if diff is not None:
#             with capsys.disabled():
#                 print(diff)
#         assert diff is None, \
#             "Candidate channel-only nudging restart files do not match full restart files"
#
#
# # Channel-only ncores question
# def test_ncores_candidate_channel_only(
#     candidate_channel_only_setup,
#     output_dir,
#     job_ncores,
#     scheduler,
#     capsys
# ):
#
#     if candidate_channel_only_setup is None:
#         pytest.skip("unsupported configuration")
#
#     with capsys.disabled():
#         print("\nQuestion: The candidate_channel-only restarts from a 1 core run match restarts from standard run?",
#               end='')
#
#     candidate_channel_only_run_file = output_dir / 'run_candidate_channel_only' / 'WrfHydroRun.pkl'
#     if candidate_channel_only_run_file.is_file() is False:
#         pytest.skip('candidate_channel_only run object not found, skipping test.')
#
#     # Load initial run model object
#     candidate_channel_only_run_expected = pickle.load(open(candidate_channel_only_run_file, "rb"))
#     # Set run directory
#     run_dir = output_dir.joinpath('ncores_candidate_channel_only')
#
#     candidate_channel_only_ncores_job = job_ncores
#     candidate_channel_only_ncores_job.scheduler = scheduler
#
#     # Run
#     candidate_channel_only_ncores_run = wrfhydropy.WrfHydroRun(
#         wrf_hydro_setup=candidate_channel_only_setup,
#         run_dir=run_dir,
#         jobs=candidate_channel_only_ncores_job
#     )
#     check_run_dir = candidate_channel_only_ncores_run.run_jobs()
#
#     if scheduler is not None:
#         candidate_channel_only_ncores_run = wrfhydropy.job_tools.restore_completed_scheduled_job(check_run_dir)
#
#     #Check against initial run
#     ncores_restart_diffs = wrfhydropy.RestartDiffs(
#         candidate_channel_only_ncores_run,
#         candidate_channel_only_run_expected
#     )
#
#     ## Check hydro restarts
#     for diff in ncores_restart_diffs.hydro:
#         if diff is not None:
#             with capsys.disabled():
#                 print(diff)
#         assert diff == None, "candidate_channel-only hydro restart files do not match when run with different number of cores"
#
#     ## Check nudging restarts
#     for diff in ncores_restart_diffs.nudging:
#         if diff is not None:
#             with capsys.disabled():
#                 print(diff)
#         assert diff == None, "candidate_channel-only nudging restart files do not match when run with different number of cores"
#
#
# # Channel-only perfect restarts question
# def test_perfrestart_candidate_channel_only(
#     candidate_channel_only_setup,
#     output_dir,
#     job_default,
#     scheduler,
#     capsys
# ):
#
#     if candidate_channel_only_setup is None:
#         pytest.skip("unsupported configuration")
#
#     with capsys.disabled():
#         print("\nQuestion: The candidate_channel_only restarts from a restart run match the restarts from standard run?",
#               end='')
#
#     candidate_channel_only_run_file = output_dir / 'run_candidate_channel_only' / 'WrfHydroRun.pkl'
#     if candidate_channel_only_run_file.is_file() is False:
#         pytest.skip('candidate_channel_only run object not found, skipping test')
#
#     # Load initial run model object
#     candidate_channel_only_run_expected = \
#         pickle.load(open(output_dir / 'run_candidate_channel_only' / 'WrfHydroRun.pkl', "rb"))
#
#     #Make deep copy since changing namelist optoins
#     perfrestart_setup = copy.deepcopy(candidate_channel_only_setup)
#
#     # Set run directory
#     run_dir = output_dir / 'restart_candidate_channel_only'
#
#     # Establish the run (run after setting external files)
#     candidate_channel_only_perfrestart_job = job_default
#     # TODO(JLM): edit scheduler names
#     candidate_channel_only_perfrestart_job.scheduler = scheduler
#
#     # Add the jobs after determining the restart time.
#     candidate_channel_only_perfrestart_run = wrfhydropy.WrfHydroRun(
#         wrf_hydro_setup=perfrestart_setup,
#         run_dir=run_dir,
#         mode='r'
#     )
#
#     # Symlink restarts files to new directory and modify namelistrestart files
#     # Hydro
#     hydro_rst = candidate_channel_only_run_expected.restart_hydro[0]
#     new_hydro_rst_path = run_dir.joinpath(hydro_rst.name)
#     new_hydro_rst_path.unlink()
#     new_hydro_rst_path.symlink_to(hydro_rst)
#
#     perfrestart_setup.hydro_namelist['hydro_nlist'].update(
#         {'restart_file': str(new_hydro_rst_path)})
#
#     # Nudging
#     if candidate_channel_only_run_expected.restart_nudging is not None and \
#        len(candidate_channel_only_run_expected.restart_nudging) > 0:
#         nudging_rst = candidate_channel_only_run_expected.restart_nudging[0]
#         new_nudging_rst_path = run_dir.joinpath(nudging_rst.name)
#         new_nudging_rst_path.unlink()
#         new_nudging_rst_path.symlink_to(nudging_rst)
#
#         perfrestart_setup.hydro_namelist['nudging_nlist'].update(
#             {'nudginglastobsfile': str(run_dir.joinpath(nudging_rst.name))})
#
#
#     # Setup the restart in the run.
#     orig_start_time, orig_end_time = wrfhydropy.job_tools.solve_model_start_end_times(
#         None,
#         None,
#         candidate_channel_only_perfrestart_run.setup
#     )
#
#     restart_dt = hydro_rst.open()
#     restart_time = dt.datetime.strptime(restart_dt.Restart_Time,'%Y-%m-%d_%H:%M:%S')
#
#     candidate_channel_only_perfrestart_job.model_start_time = restart_time
#     candidate_channel_only_perfrestart_job.model_end_time = orig_end_time
#
#     # Run
#     with warnings.catch_warnings():
#         warnings.simplefilter("ignore")
#         candidate_channel_only_perfrestart_run.add_jobs(candidate_channel_only_perfrestart_job)
#         check_run_dir = candidate_channel_only_perfrestart_run.run_jobs()
#         if scheduler is not None:
#             candidate_channel_only_perfrestart_run = \
#                 wrfhydropy.job_tools.restore_completed_scheduled_job(check_run_dir)
#
#     #Check against initial run
#     perfstart_restart_diffs = wrfhydropy.RestartDiffs(
#         candidate_channel_only_perfrestart_run,
#         candidate_channel_only_run_expected
#     )
#     ## Check hydro restarts
#     for diff in perfstart_restart_diffs.hydro:
#         if diff is not None:
#             with capsys.disabled():
#                 print(diff)
#         assert diff is None, \
#             "candidate_channel_only hydro restart files do not match when starting from a restart"
#
#     ## Check nudging restarts
#     for diff in perfstart_restart_diffs.nudging:
#         if diff is not None:
#             with capsys.disabled():
#                 print(diff)
#         assert diff is None, \
#             "candidate_channel_only nudging restart files do not match when starting from a restart"