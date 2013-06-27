MODULE MPI_MODULE
  include 'mpif.h'

  character*4 :: MPI_PID
  integer mpi_ierr, mpi_processor_id, mpi_numprocs, mpi_id
  integer mpi_status(MPI_STATUS_SIZE), mpi_slave_returns, mpi_index
  integer, allocatable :: mpi_subsiz(:)
END MODULE MPI_MODULE




