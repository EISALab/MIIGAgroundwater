MODULE PARAMS

  IMPLICIT NONE

  integer, parameter :: MxYr1=20
  integer, parameter :: MxWel1=150
  integer, parameter :: PeriodLength=5
  integer, parameter :: NoSpecies=2
  ! grid parameters
  integer, parameter :: Nlay1=5
  integer, parameter :: Nrow1=66
  integer, parameter :: Ncol1=64          
  !         Added by Eva Sinha on Dec, 02, 2002
  integer, parameter :: OriNlay1=5
  integer, parameter :: OriNrow1=132
  integer, parameter :: OriNcol1=125
  integer, parameter :: Nstress = 1
  ! new wells and basin constraints
  integer, parameter :: MaxNewWells=4
  integer, parameter :: MaxNewBasin=3
  ! unit numbers for the various input files
  integer, parameter :: iin=55
  integer, parameter :: iwel=88
  integer, parameter :: isc=89
  integer, parameter :: irdx=104
  integer, parameter :: itnt=105
  integer, parameter :: ihds=106
  integer, parameter :: iout=77
  ! standard concentrations for RDX and TNT
  real, parameter :: Cl_Rdx=2.1
  real, parameter :: Cl_Tnt=2.8
  ! total pumping capacity constraints
  real, parameter :: cap_gac(3)=(/1300,1625,1950/)
  ! cost parameters
  real, parameter :: AnnualCostLabor=237.0
  real, parameter :: AnnualCostElec=3.6


  integer, parameter :: newwells = 7
  double precision :: pen_wt_Rdx 
  double precision :: pen_wt_Tnt
  integer :: idum1 = -43003
  integer :: idum2 = -53214
  integer, parameter :: oldwells = 7
  double precision, parameter :: alphamax = 0.90

  integer :: nchrome

  double precision, dimension(10000) :: pout
  double precision, dimension(newwells,200) :: maxpumpnew,maxpumpold,pumpflagnew,pumpflagold

  double precision,allocatable :: parent(:,:), child(:,:), g0(:),g1(:),pardel(:)

  integer :: npossum,ig2sum,nichflg(1000),nposibl(1000),ncomp(10)
  integer,allocatable :: ig2(:),iparent(:,:), ichild(:,:)

  integer, dimension(newwells):: possibnodes,binpossibnodes,binmaxpumprate,namenew
  integer, dimension(oldwells):: nameold
  integer, dimension(newwells,200,6):: rwlcnew
  integer, dimension(oldwells,10,6)::rwlcold
  integer, dimension(7) :: temp

  integer, parameter :: indmax = 500
  ! maximum # of individuals, i.e. max population size
  ! if using Stochastic Remainder Selection should make
  ! indmax equal to approximately 2 times larger than
  ! the population size

  integer, parameter :: nchrmax = 700
  ! maximum # of chromosomes (binary bits) per individual

  integer :: nparam
  ! Number of parameters (groups of bits) of each individual.
  ! Make sure that nparam matches the number of values in the 
  ! parmin, parmax and nposibl input arrays.

  integer :: npopsiz
  !actual popultaion size

  double precision, dimension(1000) :: parmin
  ! array of the minimum allowed values of the parameters

  double precision, dimension(1000) :: parmax
  ! array of the maximum allowed values of the parameters

  integer :: nparmax
  ! maximum # of parameters which the chromosomes make up
  ! must equal nparam

  logical :: irestrt = .FALSE.
  ! = .false. for a new GA run or for a single function evaluation
  ! = .true. for a restart continuation of a GA run

  integer :: kountmx = 3
  ! the maximum value of count before a new restart file is written
  ! presently set to write every fifth generation
  ! increasign this value will reduce the I/o time requirements
  ! and reduce wear and tear on your storage device

  logical :: iniche = .false.
  ! = .false. for no niching
  ! = .true.  for niching; niching is recommended.

  integer  :: maxgen
  ! The maximum number of generations to run by the GA.

  integer, parameter :: maxobj = 1
  ! maximum # of objectives: if 1 then Simple GA
  !		             if > 1 use NSGA
  ! if > 1 must formulate problem assuming minimization of 
  ! all objectives

  integer :: itourny = 2
  ! = 1  for tournament selection 
  ! = 0  for stochastic remainder selection (used only with NSGA) 
  ! = 2  for mu-lambda selection

  integer :: tsize 
  ! tournament size

  logical :: iunifrm = .true.
  ! = .false. for single point crossover
  ! = .true. for uniform crossover; uniform crossover is recommended     

  logical :: replacement = .true.
  ! = .true.  for SGA with replacement
  ! = .false. for SGA without replacement

  logical :: ielite = .false.
  ! = .false. for no elitism (best individual(s) not necessarily 
  !           replicated from one generation to the next).
  ! = .true.  for elitism to be invoked (best individual(s) replicated 
  !           into next generation); elitism is recommended.
  logical :: cross = .false.
  ! = .true. set pcroos to (s-1)/s
  ! = .false. set by user
  double precision :: pcross = 0.5d0
  ! The crossover probability.  For single-point crossover, a 
  ! value of 0.6 or 0.7 is recommended. For uniform crossover,
  ! a value of 0.5 is suggested.

  integer :: nchild = 2
  ! = 1 for one child per pair of parents (this is what I 
  !     typically use).
  ! = 2 for two children per pair of parents (2 is more common 
  !     in GA work).

  logical :: mutation = .true.
  ! = .true. set pmutate to 1/npopsiz
  ! = .false. set by user

  double precision :: pmutate = 0.001d0
  ! The jump mutation probability.  Typically set = 1/npopsiz.

  logical :: icreep = .false.
  ! = .false. for no creep mutations
  ! = .true.  for creep mutations; creep mutations are recommended.

  double precision :: pcreep = 0.02d0
  ! The creep mutation probability.  Typically set this
  ! = (nchrome/nparam)/npopsiz.

  logical :: microga = .false.
  ! = .false. for normal conventional GA operation
  ! = .true.  for micro-GA operation

  double precision, parameter :: cstop = 90.0d0
  ! convergence criteria. Percentage of population required to converge before ga stops.

  double precision :: tol1 = 1d-5
  !Tolerance for stopping criterian: difference

  double precision :: tol2 = 1d-5
  !Tolerance for stopping criterian: ratio

  double precision, parameter :: grid_var = 2.59545d+11
  !grid noise variance found by initial external monte carlo experiments

  double precision, parameter :: var_factor = 6.0d0
  !relation between fitness noise and grid noise before change in grid size takes place

  logical :: grid_change = .false.
  !change of grid sizes within the GA run

  integer, parameter :: change_gen = 60
  !generation for changing to finer mesh

  ! **********************************************************************
  ! Variables for Multiscale strategy 2 where a certain fraction of population is
  ! evaluated on fine grid, and tournament selection scheme for picking mates is biased 
  ! towards preferring individuals that have been evaluated on the fine grid, mu+lambda
  ! selection for final population (after creating children) is done separately for fine 
  ! coarse individuals. Proportion of individuals individuals tested on fine grid is
  ! slowly increased in the run.

  ! Whether multiscale strategy 2 is tested
  logical :: multiscale2 = .true.

  ! Fraction of individuals in coarse population tested on fine grid
  real, parameter :: best_frac = 0.15 ! make this 12.5% for pop size of 120

  ! Proportion of fine individuals selected from fine section of population, for final mu+lambda
  ! selection scheme.
  real :: best_selec_frac = 0.5

  ! Flag for individual tested on fine
  ! 2 if tested on fine.
  ! 1 if tested on coarse.
  integer, allocatable :: fine_flag(:) 

  ! Flag for age of individuals tested on fine
  ! 1 if individual has already been tested on fine
  ! 0 if individual is best on coarse and needs to be tested on fine.
  integer, allocatable :: fine_age(:)   

end module PARAMS
   

