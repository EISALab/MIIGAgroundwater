# Sample batch script for a mpi job (advanced users). 
#
# This script is the same as the basic script except it also saves files to 
# UniTree before the job reaches its wall clock limit.
#
# Note: this script will still fail to save files if:
#
#   1) UniTree is unavailable when wall clock limit is reached.
#   2) It takes longer than (hard_limit - soft_limit) to save the files. 
#   3) There is a syntax error in the msscmd commands.
#   4) The job is killed for any reason.
#   5) The system crashes while the job is running.
#
# Submit this script using the command: llsubmit mpi_wallclock.ll
#
# Use the "llq" command to check the status of a job.
#
# Lines starting with #@ are embedded loadleveler directives.
# To comment out a loadleveler directive, put # in front of #@ 

# The shell is set to sh instead of csh because sh can trap the wall
# clock soft limit signal.

#@ shell = /usr/bin/sh

#@ job_type = parallel

# copy all environment variables to batch job
#@ environment = COPY_ALL

# Send email when job completes
# (notification options: always,start,error,complete,never)
#@ notification = complete

# Specify job class
#@ class = batch

# Charge job to project abc (recommended for users with multiple projects)
# [If project is invalid, a valid project will be automatically selected]
#@ account_no = orq

# Specify number of MPI processes
#@ tasks_per_node = 16

# Specify memory = amount per MPI process
# For mpi, ConsumableCpus is always 1, and it must be specified.
#@ resources = ConsumableCpus(1) ConsumableMemory(500Mb)

# Set the soft wall clock limit as well as the hard limit.
# When the soft limit is reached, the batch system sends
# an XCPU signal to the script.

# Specify the wall clock limit (hard limit, soft limit) = hrs:min:sec
#@ wall_clock_limit = 7:00:00, 6:45:00

# Specify the name of the job
#@ job_name = multiscale2

# Specify the standard output and standard error for the job
# They will be written in the subdirectory from which llsubmit is run
#@ output = multiscale2.out
#@ error = multiscale2.err

# This has to be the last loadleveler directive
#@ queue
##########################################################

# The "save_files" function gives the commands used to 
# save files to UniTree at the end of the batch job, or 
# when the soft wall clock limit is reached.
save_files () {
  echo "** Saving files in $SCR to mss:output.tgz"
  cd $SCR
  msscmd "cd test1, tar cf output.tgz *"
  exit
}

# change to the scratch directory
# (This directory is removed when the job finishes)
cd $SCR

# If the soft wall clock limit is reached (XCPU signal is sent to 
# this script), run save_files to save files to UniTree.
trap 'save_files' XCPU

# get executable and input file from unitree
msscmd "cd test1,get a.out,mget *.input" 

# due to unitree "feature", must set executable bit
chmod u+x multiscale2

# run mpi executable 
poe ./multiscale2 -labelio yes

# Save files to UniTree (see save_files above)
save_files
