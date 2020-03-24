#!/bin/sh -l
## 1) Interpretor: Specifies the shell that will be interpreting the commands in your script
## 2) Slurm Options: a) Request cluster resources. 
##                   b) Lines that begin with #SBATCH will be ignored by the interpreter and read by the job scheduler 
##                   c) Parallel run options: 
##                      #SBATCH --ntasks=<number>: specifies the number of tasks (processes) that will run in this job. 
##                      In this example, 8 tasks will run.
## 3) 
#SBATCH                       # -a, --array=indexes         job array index values
#SBATCH -A coastal            # -A, --account=name          charge job to specified account
#SBATCH -q debug              # coastal group can only submit to this Q
#SBATCH                       # --bb=<spec>                 burst buffer specifications
#SBATCH                       # --bbf=<file_name>           burst buffer specification file
#SBATCH                       # --begin=time            defer job until HH:MM MM/DD/YY
#SBATCH                       # --comment=name          arbitrary comment
#SBATCH                       # --cpu-freq=min[-max[:gov]] requested cpu frequency (and governor)
#SBATCH                       # -c, --cpus-per-task=ncpus   number of cpus required per task
#SBATCH                       # -d, --dependency=type:jobid defer job until condition on jobid is satisfied
#SBATCH                       # --deadline=time         remove the job if no ending possible before
                              # this deadline (start > (deadline - time[-min]))
#SBATCH                       # --delay-boot=mins       delay boot for desired node features
#SBATCH                       # -D, --chdir=directory       set working directory for batch script
#SBATCH -e nwm-slurm.error    # -e, --error=err             file for batch script's standard error
#SBATCH                       # --export[=names]        specify environment variables to export
#SBATCH                       # --export-file=file|fd   specify environment variables file or file
                              # descriptor to export
#SBATCH                       # --get-user-env          load environment from local cluster
#SBATCH                       # --gid=group_id          group ID to run job as (user root only)
#SBATCH                       # --gres=list             required generic resources
#SBATCH                       # --gres-flags=opts       flags related to GRES management
#SBATCH                       # -H, --hold                  submit job in held state
#SBATCH --ignore-pbs          # --ignore-pbs            Ignore #PBS options in the batch script
#SBATCH                       # -i, --input=in              file for batch script's standard input
#SBATCH                       # --jobid=id              run under already allocated job
#SBATCH -J nwm-conus-assim    # -J, --job-name=jobname      name of job
#SBATCH                       # -k, --no-kill               do not kill job on node failure
#SBATCH                       # -L, --licenses=names        required license, comma separated
#SBATCH                       # -M, --clusters=names        Comma separated list of clusters to issue
                              # commands to.  Default is current cluster.
                              # Name of 'all' will submit to run on all clusters.
                              # NOTE: SlurmDBD must up.
#SBATCH                       # -m, --distribution=type     distribution method for processes to nodes
                              # (type = block|cyclic|arbitrary)
#SBATCH --mail-type=FAIL      # --mail-type=type        notify on state change: BEGIN, END, FAIL or ALL
#SBATCH --mail-user=beheen.m.trimble@noaa.gov     # --mail-user=user        who to send email notification for job state
                              # changes
#SBATCH                       # --mcs-label=mcs         mcs label if mcs plugin mcs/group is used
#SBATCH                       # -n, --ntasks=ntasks     number of tasks to run (num process)
#SBATCH                       # --nice[=value]          decrease scheduling priority by value
#SBATCH                       # --no-requeue            if set, do not permit the job to be requeued

#SBATCH --ntasks-per-node=24 # --ntasks-per-node=n     number of tasks to invoke on each node
#SBATCH -N 32                # -N, --nodes=N           number of nodes on which to run (N = min[-max])

#SBATCH                       # -o, --output=out            file for batch script's standard output
#SBATCH                       # -O, --overcommit            overcommit resources
#SBATCH                       # -p, --partition=partition   partition requested
#SBATCH --parsable            # --parsable              outputs only the jobid and cluster name (if present),
                              # separated by semicolon, only on successful submission.
#SBATCH                       # --power=flags           power management options
#SBATCH                       # --priority=value        set the priority of the job to value
#SBATCH                       # --profile=value         enable acct_gather_profile for detailed data
                              # value is all or none or any combination of
                              # energy, lustre, network or task
#SBATCH                       # --propagate[=rlimits]   propagate all [or specific list of] rlimits
#SBATCH                       # -q, --qos=qos               quality of service
#SBATCH                       # -Q, --quiet                 quiet mode (suppress informational messages)
#SBATCH                       # --reboot                reboot compute nodes before starting job
#SBATCH                       # --requeue               if set, permit the job to be requeued
#SBATCH                       # -s, --oversubscribe         over subscribe resources with other jobs
#SBATCH                       # -S, --core-spec=cores       count of reserved cores
#SBATCH                       # --signal=[B:]num[@time] send signal when time limit within time seconds
#SBATCH                       # --spread-job            spread job across as many nodes as possible
#SBATCH                       # --switches=max-switches{@max-time-to-wait}
                              # Optimum switches and max time to wait for optimum
#SBATCH                       # --thread-spec=threads   count of reserved threads
#SBATCH -t 420                # -t, --time=minutes          time limit
#SBATCH                       # --time-min=minutes      minimum time limit (if distinct)
#SBATCH                       # --uid=user_id           user ID to run job as (user root only)
#SBATCH                       # --use-min-nodes         if a range of node counts is given, prefer the
                              # smaller count
#SBATCH                       # -v, --verbose               verbose mode (multiple -v's increase verbosity)
#SBATCH                       # -W, --wait                  wait for completion of submitted job
#SBATCH                       # --wckey=wckey           wckey to run job under
#SBATCH                       # --wrap[=command string] wrap command string in a sh script and submit

# Constraint options:
#SBATCH                       # --cluster-constraint=[!]list specify a list of cluster constraints
#SBATCH                       # --contiguous            demand a contiguous range of nodes
#SBATCH                       # -C, --constraint=list       specify a list of constraints
#SBATCH                       # -F, --nodefile=filename     request a specific list of hosts
#SBATCH                       # --mem=MB                minimum amount of real memory
#SBATCH                       # --mincpus=n             minimum number of logical processors (threads)
                              # per node
#SBATCH                       # --reservation=name      allocate resources from named reservation
#SBATCH                       # --tmp=MB                minimum amount of temporary disk
#SBATCH                       # -w, --nodelist=hosts...     request a specific list of hosts
#SBATCH                       # -x, --exclude=hosts...      exclude a specific list of hosts

# Consumable resources related options:
#SBATCH                       # --exclusive[=user]      allocate nodes in exclusive mode when
                              # cpu consumable resource is enabled
#SBATCH                       # --exclusive[=mcs]       allocate nodes in exclusive mode when
                              # cpu consumable resource is enabled
                              # and mcs plugin is enabled
#SBATCH                       # --mem-per-cpu=MB        maximum amount of real memory per allocated
                              # cpu required by the job.
#SBATCH                       # --mem >= --mem-per-cpu if --mem is specified.

# Affinity/Multi-core options: (when the task/affinity plugin is enabled)
#SBATCH                       # -B  --extra-node-info=S[:C[:T]]            Expands to:
#SBATCH                       # --sockets-per-node=S   number of sockets per node to allocate
#SBATCH                       # --cores-per-socket=C   number of cores per socket to allocate
#SBATCH                       # --threads-per-core=T   number of threads per core to allocate
                              # each field can be 'min' or wildcard '*'
                              # total cpus requested = (N x S x C x T)
#
#SBATCH                       # --ntasks-per-core=n     number of tasks to invoke on each core
#SBATCH                       # --ntasks-per-socket=n   number of tasks to invoke on each socket
#SBATCH                       # --hint=                 Bind tasks according to application hints
                              # (see "--hint=help" for options)
#SBATCH                       # -mem-bind=             Bind memory to locality domains (ldom)
                              # (see "--mem-bind=help" for options)


############################### main - to run: $sbatch nwm-conus-slurm.job ##########################
set -x
echo $SLURM_SUBMIT_DIR            # (in Slurm, jobs start in "current dir")       
echo $SLURM_JOBID                                                      
echo $SLURM_JOB_NAME
echo $SLURM_NNODES                                                     
echo $SLURM_TASKS_PER_NODE

# PBS_NODEFILE:  There is not a direct equivalent for this, but 
echo $SLURM_NODELIST              # give you the list of assigned nodes.

# cd $SLURM_SUBMIT_DIR
echo "STARTING THE JOB AT"
date

## MPI executables are launched using the SLURM srun command with the appropriate options. 
## For example, to launch an 8-process MPI job split across two different nodes in the pdebug pool
## srun -N2 -n8 -ppdebug a.out
## --export: To establish a clean environment
## Each task inherits the parameters specified for the batch script: srun sleep 10 & srun sleep 12 & wait 
## if -n=2 srun0 runs first, then srun1 runs for total of 22 sec. Versus srun -n 1 sleep 10 & srun -n 1 sleep 12 & wait
## In this case each task runs simultaneoulsy for the total of 12 sec. Use --export=ALL for each srun.

srun ./nwm.exe
date
