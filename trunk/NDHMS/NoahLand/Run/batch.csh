#!/usr/bin/csh  -f
#BSUB -J hydrology
#BSUB -n  64  ##yw total mpi tasks
#BSUB -q regular  ##yw regular for 6 hour wall clock time 
#BSUB -a mpich_gm
#BSUB -R "span[ptile=2]"   ###run 2 tasks per node
#BSUB -W  6:00   ##yw wall clock time. Maximum 2 hour for standby queue.
#BSUB -o out.%J
#BSUB -e err.%J
#BSUB -x    ##exlusive use of node(not shared)
#BSUB -p 48500032
time mpirun.lsf ./Noah_wrfdriver_beta_mpp >& log
