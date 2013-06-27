# Sample batch script for a mpi job
#
# Submit this script using the "llsubmit" command: llsubmit mpi.ll
#
# Use the "llq" command to check the status of a job.
#
# Lines starting with #@ are embedded loadleveler directives.
# To comment out a loadleveler directive, put # in front of #@ 

#@ shell = /usr/bin/csh
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
#@ tasks_per_node = 1

# Specify memory = amount per MPI process
# For mpi, ConsumableCpus is always 1, and it must be specified.
#@ resources = ConsumableCpus(1) ConsumableMemory(500Mb)

# Specify the wall clock limit = hrs:min:sec
#@ wall_clock_limit = 04:00:00

# Specify the name of the job
#@ job_name = sga

# Specify the standard output and standard error for the job
# They will be written in the subdirectory from which llsubmit is run
#@ output = sga.out
#@ error = sga.err

# This has to be the last loadleveler directive
#@ queue
##########################################################


# change to the scratch directory
# (This directory is removed when the job finishes)
cd $SCR

# get executable and input file from unitree
msscmd cd /u/ac/evasinha/sga-test, get sga 
msscmd cd /u/ac/evasinha/sga-test, get caseinp1.dat
msscmd cd /u/ac/evasinha/sga-test, get ga.restart
msscmd cd /u/ac/evasinha/sga-test, get test100.cr0
msscmd "cd /u/ac/evasinha/sga-test, mget HEAD2002_fine.UFH*"
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.adv*"
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.btn*" 
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.dsp*" 
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.ftl*"
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.gcg*"
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.lmt*"
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.lst*" 
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.oc*"
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.pcg*"
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.rch*" 
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.rct*"
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.ssm*"
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo_fine.ufh*"
msscmd cd /u/ac/evasinha/sga-test, get pstobj_fine.in
msscmd cd /u/ac/evasinha/sga-test, get sconc_fine.arr 
msscmd "cd /u/ac/evasinha/sga-test, mget TEST_fine.ba6*" 
msscmd "cd /u/ac/evasinha/sga-test, mget TEST_fine.bc6*" 
msscmd "cd /u/ac/evasinha/sga-test, mget TEST_fine.dis*"
msscmd "cd /u/ac/evasinha/sga-test, mget TEST_fine.nam*"
msscmd "cd /u/ac/evasinha/sga-test, mget test_mt3d_fine.nam*"
msscmd "cd /u/ac/evasinha/sga-test, mget optdemo.wel*"
msscmd "cd /u/ac/evasinha/sga-test, mget uma4_fine.res*"
msscmd cd /u/ac/evasinha/sga-test, get uma_fine.txt
msscmd cd /u/ac/evasinha/sga-test, get umatilla_input_fine.fil


# due to unitree "feature", must set executable bit
chmod u+x sga

# run mpi executable 
ls -al
poe ./sga

# tar up output files onto unitree

msscmd cd /u/ac/evasinha/sga-test, put test101.res
msscmd cd /u/ac/evasinha/sga-test, put test101.chn
msscmd cd /u/ac/evasinha/sga-test, put test101.out 
msscmd cd /u/ac/evasinha/sga-test, put test101.cr0
msscmd cd /u/ac/evasinha/sga-test, put nn_fine
msscmd cd /u/ac/evasinha/sga-test, put ga.restart
