program uma

  use PARAMS
  use MPI_MODULE 

  IMPLICIT NONE
  save


  integer :: istart, kount,ipick, ncross, num_mate_pop, mate_pop(indmax), index(2*indmax)
  integer :: i,j,k,a,b,c, jbest(indmax), drycell_c, drycell_h
  integer :: kelite, mate1, mate2
  integer :: ibest(indmax,nchrmax), num_elite
  integer :: execval, gridflagx, gridflagy
  double precision :: Fitness1,Total_cost, Pen_Rdx, Pen_Tnt
  double precision :: rand, bestf1
  double precision,allocatable :: ftemp(:)
  double precision :: fitness(maxobj,indmax)
  double precision:: mean, variance

  ! Added by Felipe Espinoza (4/9/03)
  integer, allocatable :: drycell(:,:),tdrycell(:,:)
  double precision,allocatable :: pen1(:),pen2(:),pen3(:),tpen1(:),tpen2(:),tpen3(:),cost(:),tcost(:)
  character (80) :: name1, name2, name3, name4, name5, namefile
  logical :: feed

  ! End added


  !  mate_pop = integer array of the mating population used in Stch Rem Sel
  !  num_mate_pop =the number of indiv in mating population in Stch Rem Sel

  ! MPI INITIALIZATION 


  call MPI_INIT(mpi_ierr) ! establishes MPI environment and returns an error code if there is a problem.
  call MPI_COMM_RANK(MPI_COMM_WORLD, mpi_processor_id, mpi_ierr) ! Determines rank for processors in a communicator
  call MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_numprocs, mpi_ierr) ! Determines size/ no. of processors in a communicator.

  !Added by Eva Sinha to convert the processor id (integer) into character

  if(mpi_processor_id < 10) then
     MPI_PID = char(mpi_processor_id + 48)
  else
     if(mpi_processor_id >= 10 .and. mpi_processor_id < 100)then
        a = int(mpi_processor_id/10)
        b = mod(mpi_processor_id, 10)
        MPI_PID = char(a+48) // char (b+48)
     else
        if(mpi_processor_id >= 100 .and. mpi_processor_id < 1000)then
           a = int(mpi_processor_id/100)
           b = int((mod(mpi_processor_id, 100))/10)
           c = mod((mod(mpi_processor_id, 100)),10)
           MPI_PID = char(a+48) // char (b+48) // char(c+48)
        end if
     end if
  end if


  ! Main program designed only to test the routines: remedwell, parinit, and prepfunc

  ! cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  gridflagx = 2 ! 2 refers to fine grid which has 132rows and 125 cols
  gridflagy = 2 ! 1 refers to coarse grid which has 66 rows and  64 cols

  ! This is where the array for remediation wells is read from the data file 'rwelloc.dat'
  ! added by Meghna Babbar, Jan 04, 2001
  call remedwell(gridflagx, gridflagy)
  ! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 

  ! This is where the chromosome is created. (Meghna Babbar, Jan04,2001)
  call parinit


  allocate(parent(indmax,nparmax))
  allocate(child(indmax,nparmax))
  allocate(iparent(indmax,nchrmax))
  allocate(ichild(indmax,nchrmax))
  allocate(g0(nparmax))
  allocate(g1(nparmax))
  allocate(pardel(nparmax))
  allocate(ig2(nparmax))
  allocate(ftemp(2*indmax))
  allocate(drycell(2,indmax))
  allocate(tdrycell(2,2*indmax))

  ! Added by Felipe Espinoza (4/9/03)
  allocate(pen1(indmax))
  allocate(pen2(indmax))
  allocate(pen3(indmax))
  allocate(tpen1(2*indmax))
  allocate(tpen2(2*indmax))
  allocate(tpen3(2*indmax))
  allocate(cost(indmax))
  allocate(tcost(2*indmax))


  if ((gridflagx .eq. 1) .and. (gridflagy.eq.1))then
     namefile = "caseinp_coarse.dat"
  end if
  if ((gridflagx .eq. 2) .and. (gridflagy.eq.2))then
     namefile = "caseinp_fine.dat"
  end if

  OPEN(UNIT=136, FILE=namefile, STATUS='old')            !Modified by F. Espinoza (4/9/03)
  read(136,*) npopsiz,tsize,pcross,feed,idum1,name1,name2,name3,name4,name5  !Modified by F. Espinoza (4/9/03)
  CLOSE(136)   

  !  Perform necessary initialization and read the ga.restart file.   
  call initial
  ! Added by F. Espinoza (4/9/03)

  open (unit=74,  file=name1, status='unknown')
  rewind 74

  open (unit=124, file=name2, status='unknown')
  rewind 124

  open (unit=552, file=name3, status='unknown')
  rewind 552

  open (unit=555, file=name4, status='unknown')
  rewind 555

  open (unit=557, file=name5, status='unknown')
  rewind 557
  ! End added

  if(mutation) then
     pmutate=1.0d0/npopsiz
  endif

  if(cross) then
     pcross=dble(tsize-1)/dble(tsize)
  endif

  call MPI_BARRIER(MPI_COMM_WORLD, mpi_ierr)         

  if (maxobj.eq.1) then
     call sga
  end if

  ! This is where the arrays are deallocated
  deallocate(parent)
  deallocate(child)
  deallocate(iparent)
  deallocate(ichild)
  deallocate(g0)
  deallocate(g1)
  deallocate(pardel)
  deallocate(ig2)
  deallocate(ftemp)
  deallocate(drycell)
  deallocate(tdrycell)

  ! Added by Felipe Espinoza (4/9/03)
  deallocate(pen1)
  deallocate(pen2)
  deallocate(pen3)
  deallocate(tpen1)
  deallocate(tpen2)
  deallocate(tpen3)
  deallocate(cost)
  deallocate(tcost)

  ! End added

  ! MPI termination 

  CALL MPI_FINALIZE(mpi_ierr)     


  !This command makes the subsequent subroutines internal procedures
  !with host association to variables in the main program
contains 

  !###################################################################
  subroutine sga
    !Main processing loop for the simple genetic algorithm

    IMPLICIT NONE
    save
    integer :: n, runend,j,p, nnchild,igen
    integer :: ni,nj,mm,gen,nBald,nind,fev,feve,nstop,ils,nbest,ifin,geneq,npop
    integer :: ls(indmax),itemp(2*indmax,nchrmax),bestj(1)

    double precision :: ff(indmax), rtime(2)
    double precision :: best,av0,std,dif,DeltaSGA,CVR
    double precision ::t1, t2, totaltime, converge	
    double precision :: rtime2(2), rtime3(2), rtime4(2), rtime5(2), rtime6(2)

    ! Added by Felipe Espinoza (4/9/03)

    integer :: feedchar(nchrmax),k, ind
    double precision :: f0

    rtime(1) = MPI_WTIME()

1112 format(/'#################  Generation',i5,'  #################')
1115 format(/'#################  Generation',i5,'  #################',f20.3, '####################')
1113 format(i5,2x,5(f20.10,2x),7i4,2x,7i2,3x,7i2,2x,i5,2x,i5)
1116 format(7f5.0,2x,14f3.0)
    ! End added
    kount = 0
    execval = 0
    converge = 0.d0
    runend = 0

    fev=0
    feve=0

    do igen = istart,maxgen+istart-1  ! GENERATIONS LOOPS
       rtime6(1) = MPI_WTIME() 
       ! felipe changes......====================================== 26 feb 2002
       ! evaluating fitnesses for generation zero.
       if(igen.eq.1) then                            !---------IGEN=1 LOOP BEGINS--------------
          !print *, "**************** 1st call *********************"

          if (mpi_processor_id .eq. 0)then    !--------------MASTER LOOP BEGINS-------------
             write(124,1112) 0
          end if                              !--------------MASTER LOOP ENDS-------------

          call sGA_evalout                    !-------all processors call sga_evalout
          rtime(2) = MPI_WTIME()

          if (mpi_processor_id .eq. 0) then          !---------MASTER LOOP BEGINS----------
             write(*,1115) 0, rtime(2) - rtime(1)
             do j = 1, npopsiz
                write(124, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),(nint(parent(j,k)), k = 1, nparam),drycell(1,j),drycell(2,j)
                write(*, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),(nint(parent(j,k)), k = 1, nparam),drycell(1,j),drycell(2,j)
             end do
             ! Added by Felipe Espinoza (4/9/03)
             if(feed) then
                call ran2(1,rand)
                ind = 1 + dint(rand*(dble(npopsiz-1)))
                read(557,*) k,f0,(feedchar(j),j=1,nchrome)
                do j=1,nchrmax
                   iparent(ind,j)=0
                enddo
                do j=1,nchrome
                   iparent(ind,j)=feedchar(j)
                enddo
                close(557)
                fitness(1,ind)=f0
             endif
             fev=fev+npopsiz
             call BestValue(best)
             call Avg(av0,std,nbest,best)
             write(74,2223) 0,av0,std,std/av0,best,nbest,fev,feve
          end if                                    !-------------MASTER LOOP ENDS-----------------
2223      format(i4,2x,f18.8,2x,f25.12,2x,f18.12,2x,f15.2,2x,i6,2x,i10,2x,i10)

          ! End Felipe
       endif                                        !------------IGEN=1 LOOP ENDS------------------

       if(igen.eq.istart .and. irestrt) then                            !---------IGEN=ISTART LOOP BEGINS--------------
          !print *, "**************** 1st call *********************"
          if (mpi_processor_id .eq. 0)then    !--------------MASTER LOOP BEGINS-------------
             write(124,1112) istart
          end if                              !--------------MASTER LOOP ENDS-------------

          call sGA_evalout
          rtime(2) = MPI_WTIME()

          if (mpi_processor_id .eq. 0) then          !---------MASTER LOOP BEGINS----------
             write(*,1115) 0, rtime(2) - rtime(1)
             do j = 1, npopsiz
                write(124, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),(nint(parent(j,k)), k = 1, nparam),drycell(1,j),drycell(2,j)
                write(*, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),(nint(parent(j,k)), k = 1, nparam),drycell(1,j),drycell(2,j)
             end do
             ! Added by Felipe Espinoza (4/9/03)

             fev=fev+npopsiz
             call BestValue(best)
             call Avg(av0,std,nbest,best)
             write(74,2223) 0,av0,std,std/av0,best,nbest,fev,feve
          end if                                    !-------------MASTER LOOP ENDS-----------------
          ! End Felipe
       endif                                        !------------IGEN=ISTART LOOP ENDS------------------



       !Master performs niche, selectn, crosovr, ...
       !==================== GA TASKS ===================================
       if (mpi_processor_id .eq. 0) rtime(1) = MPI_WTIME()
       if (igen .le. maxgen+istart-1) then ! *** check for i

          IF (mpi_processor_id == 0) THEN  ! %%%%%%%%%%%%%%%% MASTER IF LOOP 1 %%%%%%%%%%%%%%%%%%%
             rtime2(1) = MPI_WTIME()

             !       Implement "niching".
             !       if (iniche) call niche 
             !
             !       Enter selection, crossover and mutation loop.
             ncross = 0
             ipick = npopsiz

             !Perform selection & then perform crossover between the randomly selected pair.
             if(replacement) then
                do j = 1,npopsiz,nchild
                   call selectwr(ipick,j,mate1,mate2)
                   call crosovr(ncross,j,mate1,mate2)
                enddo
             else
                nnchild=1

                do p=1,tsize
                   do j=1,npopsiz
                      index(j)=j
                   enddo
                   npop=npopsiz
                   do j = 1,npopsiz,nchild*tsize
                      call selectwor(npop,index,mate1,mate2,nnchild)
                      call crosovr(ncross,nnchild,mate1,mate2)
                      nnchild=nnchild+nchild
                   enddo
                enddo
             endif

             !  Now perform random mutations.
             !  If running micro-GA, skip mutation.
             !        if (.not. microga) call mutate   

             call mutate  ! Modified F. Espinoza (4/9/03)

             ! End eliminated & Modified (4/9/03)


             !  Write child array back into parent array for new generation.  
             !  Check to see if the best parent was replicated.
             rtime2(2) = MPI_WTIME()
          END IF ! %%%%%%%%%%%%%%%% MASTER IF LOOP 1 END %%%%%%%%%%%%%%%%%%%




          ! ***************** NEW POPULATION CREATION (MU+LAMBDA OR TOURNAMENT SELECTION) **********************

          if(itourny.eq.1) then
             call newgen(npossum,ig2sum)

          else
             if (mpi_processor_id == 0) then  ! %%%%%%%%%%%%%%%% MASTER IF LOOP 2 %%%%%%%%%%%%%%%%%%% 
                rtime3(1) = MPI_WTIME()
                do n=1,2*npopsiz
                   index(n)=n
                enddo
                do n=1,npopsiz
                   ftemp(n)=fitness(1,n)
                   tcost(n)=cost(n) ! Added by F. Espinoza (9/4/03)
                   tpen1(n)=pen1(n) ! Added by F. Espinoza (9/4/03)
                   tpen2(n)=pen2(n) ! Added by F. Espinoza (9/4/03)
                   tpen3(n)=pen3(n) ! Added by F. Espinoza (9/4/03)
                   tdrycell(1,n) = drycell(1,n)
                   tdrycell(2,n) = drycell(2,n)
                   do j=1,nchrome
                      itemp(n,j)=iparent(n,j)
                   end do
                   do j=1,nchrome
                      iparent(n,j)=ichild(n,j)
                   enddo
                enddo
                execval = 1

                rtime3(2) = MPI_WTIME()
             end if  ! %%%%%%%%%%%%%%%% MASTER IF LOOP 2 END %%%%%%%%%%%%%%%%%%% 


             ! %%%%%%%%%%%%%% ALL PROCESSORS GO INTO SGA_EVALOUT %%%%%%%%%%%%%%%%%%%%
             ! put mpi_barrier !!!!!!
             call MPI_BARRIER(MPI_COMM_WORLD, mpi_ierr)                
             call sGA_evalout  
             ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
             if (mpi_processor_id == 0) then  ! %%%%%%%%%%%%%%%% MASTER IF LOOP 3 %%%%%%%%%%%%%%%%%%% 
                rtime4(1) = MPI_WTIME()
                write(124,1112) igen  
                do j = 1, npopsiz
                   write(124, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),(nint(parent(j,k)), k = 1, nparam),drycell(1,j),drycell(2,j)
                   write(*, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),(nint(parent(j,k)), k = 1, nparam),drycell(1,j),drycell(2,j)
                end do

                do n=1,npopsiz
                   ftemp(n+npopsiz)=fitness(1,n)
                   tcost(n+npopsiz)=cost(n) ! Added by F. Espinoza (9/4/03)
                   tpen1(n+npopsiz)=pen1(n) ! Added by F. Espinoza (9/4/03)
                   tpen2(n+npopsiz)=pen2(n) ! Added by F. Espinoza (9/4/03)
                   tpen3(n+npopsiz)=pen3(n) ! Added by F. Espinoza (9/4/03)

                   tdrycell(1,n+npopsiz) = drycell(1,n)
                   tdrycell(2,n+npopsiz) = drycell(2,n)

                   do j=1,nchrome
                      itemp(n+npopsiz,j)=iparent(n,j)
                   enddo
                enddo

                call bubblesort1(2*npopsiz,ftemp,index)

                do n=1,npopsiz
                   fitness(1,n)=ftemp(n)
                   cost(n)=tcost(index(n)) ! Added by F. Espinoza (9/4/03)
                   pen1(n)=tpen1(index(n)) ! Added by F. Espinoza (9/4/03)
                   pen2(n)=tpen2(index(n)) ! Added by F. Espinoza (9/4/03)
                   pen3(n)=tpen3(index(n)) ! Added by F. Espinoza (9/4/03)
                   drycell(1,n) = tdrycell(1,index(n))
                   drycell(2,n) = tdrycell(2,index(n))

                   do j=1,nchrome
                      iparent(n,j)=itemp(index(n),j)
                   end do
                   call decode(n, parent, iparent)
                enddo

                rtime(2) = MPI_WTIME()
                write(*,1115) igen, (rtime(2)-rtime(1))
                rtime(1) = rtime(2)   

                ! writing in the output file
                write(552,1112) igen
                !    write(*,1112) igen
                do j = 1, npopsiz
                   write(552, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),(nint(parent(j,k)), k = 1, nparam),drycell(1,j),drycell(2,j)
                   write(*,1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),(nint(parent(j,k)), k = 1, nparam),drycell(1,j),drycell(2,j)
                end do

                ! Added by F. Espinoza

                fev=fev+npopsiz
                call BestValue(best)
                call Avg(av0,std,nbest,best)
                write(74,2223) igen,av0,std,std/av0,best,nbest,fev,feve
                ! End added

                rtime4(2) = MPI_WTIME()
             end if  ! %%%%%%%%%%%%%%%% MASTER IF LOOP 3 END %%%%%%%%%%%%%%%%%%% 

          endif
          ! ********************************* NEW POPULATION CREATION END ************************************



          if (mpi_processor_id == 0) then  ! %%%%%%%%%%%%%%%% MASTER IF LOOP 4 %%%%%%%%%%%%%%%%%%% 
             rtime5(1) = MPI_WTIME()          
             !  Implement micro-GA if enabled.
             !          if (microga) call gamicro(i,npossum,ig2sum)
             !   Write to restart file
             call restart(igen, istart,kount)

             ! ------------------------------------------------
             !      added for grid noise ### meghna jan 15, 2002 ###
             !      subroutine 'stats' calculates mean and variance of population in a generation.
             !      var_ actor ; defined in the module fitfunc_input
             !      grid_var ; defined in the module fitfunc_input
             call stats(fitness, mean, variance, converge)
             !         write(49,*)' ########### generation', i,'###########'
             !         write(49,*)' mean', mean  
             !         write(49,*)' variance', variance
             !         write(49,*)' percentage population converged', converge



             !       Check for convergence for quitting sga.           
             if (converge .gt. cstop) then
                runend = 1111
             end if

             if (grid_change) then
                if (igen .eq. (change_gen-1))then
                   gridflagx = 2   ! 2 refers to fine grid which has 132 rows and 125 cols
                   gridflagy = 2
                   call remedwell(gridflagx, gridflagy)
                endif
             endif
             rtime5(2) = MPI_WTIME()
             !---------------------------------------------------------------------------
          end if ! %%%%%%%%%%%%%%%% MASTER IF LOOP 4 END %%%%%%%%%%%%%%%%   
          ! t1= MPI_WTIME() !rtime(1) = MPI_WTIME()
          call MPI_BCAST(runend, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierr)
          ! call MPI_BCAST(gridflagx, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierr)
          ! call MPI_BCAST(gridflagy, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_ierr)
          ! t2= MPI_WTIME() 
          ! PRINT *, 'time for broadcasting grid flags', t2-t1

          rtime6(2) = MPI_WTIME()
          IF(mpi_processor_id .eq.0) then
             write(*,432) igen, rtime2(2)-rtime2(1),rtime3(2)-rtime3(1), rtime4(2)-rtime4(1),rtime5(2)-rtime5(1),rtime6(2)-rtime6(1)
          end IF

       END IF ! ga tasks end if

       ! ======================  GA TASKS ============================================

       !  convergence check -----------------------------------
       !  print *,'end is ',end, 'at igen', i
       if (runend .eq. 1111) then
          go to 9988
       end if
       ! ------------------------------------------------------


432    format(i4,2x,5(f15.7,2x))
    end do ! GENERATION END DO LOOP +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
9988 bestj = minloc(fitness(1,1:npopsiz))
    write(555,556) igen, fitness(1,bestj(1)), (iparent(bestj(1),j), j = 1, nchrome)
556 format(i4,2x,f15.5,2x,200i2)    

    runend = 1001
    !print *,'PROGRAM ENDED'

1050 format(1x,' #      Binary Code',16x,'Param1  Param2  Fitness')
1111 format(//'#################  Generation',i5,' #################')
1225 format(/'  Number of Crossovers      =',i5)


    return
  end subroutine sga

  !#######################################################################
  subroutine sGA_evalout
    !
    !  This subroutine evaluates the population, assigns fitness, 
    !  establishes the best individual, and outputs information. 
    IMPLICIT NONE
    save

    integer :: order
    double precision :: paramsm(nparam),paramav(nparam)
    double precision :: fitsum,fbar,funcval,best,pw,MB
    double precision :: par(nparam)
    !****************
    ! FOR MPI 
    double precision  :: mpi_all(8)
    !double precision :: mpi_subfitness(npopsiz),mpi_sub_drycell(2,npopsiz),mpi_sub_dry1(npopsiz), mpi_sub_dry2(npopsiz)
    !double precision :: mpi_subcost(npopsiz), mpi_subpen_rdx(npopsiz), mpi_subpen_tnt(npopsiz)
    double precision :: rtime(2), rtime2(2), rtime3(2),totaltime
    !****************    
    integer :: j,k,i,l,m,f,g,p,remaining, z,nc

    totaltime=0.0d0 
    best = huge(best)
    fitsum = 0.d0
    paramsm(1:nparam) = 0.0d0
    num_elite = 0
    pw=10.0d0

    !    mpi_subfitness = 0.0d0
    !    mpi_subcost = 0.0d0
    !    mpi_subpen_rdx = 0.0d0
    !    mpi_subpen_tnt = 0.0d0
    !print *, "+++++++++++++++++ inside sga_eval++++++++++++++++++++"
    ! calculates number of individuals each processor is going to evaluate
    rtime2(1) = MPI_WTIME()
    mpi_subsiz = npopsiz/(mpi_numprocs-1)   
    mpi_all = 0.0d0

    ! Master decodes the whole population first
    if (mpi_processor_id == 0) then       !------------MASTER LOOP BEGINS------------
       do k=1,npopsiz
          call decode(k,parent,iparent)
          !call prepfunc1(k) !prepfunc is called by master so that mass balance is performed with the same random number
          !          write(*,1116) k,(parent(k,j), j = 1, nparam)
1116      format(i4,2x,7f5.0,2x,14f3.0)
       end do
    end if                                !------------MASTER LOOP ENDS------------

    !Master broadcasts the entire decoded population
    rtime3(1) = MPI_WTIME()
    do i=1, npopsiz
       call MPI_BCAST(parent(i,:), nparam, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, mpi_ierr)
    end do
    rtime3(2) = MPI_WTIME()


    !Master processor waits for function evaluations and also evaluates the remianing popluation  
    if (mpi_processor_id .eq. 0) then               !--------------- MASTER LOOP BEGINS--------------

       !after dividing the population and distributing to the slaves
       !master does the remaining population's function evaluations

       !remainign changed by Felipe 04/24/03
       remaining = npopsiz - mpi_subsiz*(mpi_numprocs-1)
       !remaining = mpi_subsiz
       if (remaining .gt. 0) then
          do z = (mpi_subsiz * (mpi_numprocs-1) + 1), npopsiz

             rtime(1) = MPI_WTIME(1)
             call prepfunc(z,MB,gridflagx,gridflagy)
             !fitfess function for the coarse grid
             if ((gridflagx.eq.1) .and. (gridflagy.eq.1))then
                call Obj_Func_Coarse(Fitness1,Total_cost,Pen_Rdx,Pen_Tnt,gridflagx, gridflagy,drycell_c, drycell_h)
             endif

             !fitfess function for the fine grid
             if ((gridflagx.eq.2) .and. (gridflagy.eq.2))then
                call Obj_Func_Fine(Fitness1,Total_cost,Pen_Rdx,Pen_Tnt, gridflagx, gridflagy,drycell_c, drycell_h)
             endif

             fitness(1,z) = Fitness1+pw*MB
             cost(z) = Total_cost
             pen1(z) = Pen_Rdx
             pen2(z) = Pen_Tnt
             pen3(z) = pw*MB
             drycell(1,z) = drycell_h
             drycell(2,z) = drycell_c

             rtime(2) = MPI_WTIME()
             write(*,*) z, fitness(1,z), rtime(2)-rtime(1)

             !Commented By Eva Sinha
             !      do i= 1,noremwells
             !       mpi_Qw(z,i)=Qw(i)
             !      end do

             !call func1(j,funcval)
             !fitness(1,j) = funcval
             !write(24,1075)j,(iparent(j,k), k=1,nchrome),(parent(j,l),l=1,nparam),&
             !fitness(1,j) 

             fitsum=fitsum+fitness(1,z)

             ! paramsm(1:nparam)=paramsm(1:nparam)+parent(z,1:nparam)	
             !Check and see if individual j is the best fitness
             if (fitness(1,z).lt.best) then !This assume minimiziing function
                best = fitness(1,z)
                jbest(1) = z
                !Writing the coded form of the best individual
                do m = 1,nchrome
                   ibest(1,m)=iparent(z,m)
                end do
             endif
          end do ! z loop
       end if    ! if remaining.gt.0 loop ends

       !Master recevies the individuals as soon as they are send (the receving is not based on processor id rather on the speed at which individuals gets calculated first)

       do order = 1, npopsiz-remaining  !master recevies all individuals apart form those evaluated by itself

          !       do order = 1 , mpi_subsiz
          !          do mpi_index = 1, mpi_numprocs-1
          !    do order = (mpi_subsiz * mpi_index - mpi_subsiz + 1), mpi_index*mpi_subsiz

          call MPI_RECV(mpi_all, 8, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, mpi_status, mpi_ierr)


          !putting the returned fitness values into appropriate place in fitness array
          !do j = (mpi_subsiz * mpi_id - mpi_subsiz + 1), mpi_id*mpi_subsiz
          j = mpi_all(1)
          fitness(1,j) = mpi_all(2)
          cost(j) = mpi_all(3)
          pen1(j) = mpi_all(4)
          pen2(j) = mpi_all(5)
          pen3(j) = mpi_all(8)
          drycell(1,j) = mpi_all(7)
          drycell(2,j) = mpi_all(6)

          fitsum=fitsum+fitness(1,j)
          paramsm(1:nparam)=paramsm(1:nparam)+parent(j,1:nparam)	
          !Check and see if individual j is the best fitness
          if (fitness(1,j).lt.best) then !This assume minimiziing function
             best = fitness(1,j)
             jbest(1) = j
             !Writing the coded form of the best individual
             do m = 1,nchrome
                ibest(1,m)=iparent(j,m)
             end do
          endif
          !      end do
       end do
    end if                    ! -------------------- MASTER LOOP ENDS ------------------------

    ! slave processors call the fitfunc
    ! each slave starts evaluating its own population, depending upon the index in main 
    ! population decided by mpi_subsiz


    if (mpi_processor_id .ne. 0) then      !----------------- loop for slaves begins--------------------

       do j = (mpi_subsiz * mpi_processor_id - mpi_subsiz + 1), mpi_processor_id*mpi_subsiz

          rtime(1) = MPI_WTIME()
          call prepfunc(j,MB,gridflagx, gridflagy)
          !fitfess function for the coarse grid
          if ((gridflagx.eq.1) .and. (gridflagy.eq.1))then
             call Obj_Func_Coarse(Fitness1, Total_cost, Pen_Rdx, Pen_Tnt, gridflagx, gridflagy,drycell_c, drycell_h)
          endif

          !fitfess function for the fine grid
          if ((gridflagx.eq.2) .and. (gridflagy.eq.2))then
             call Obj_Func_Fine(Fitness1, Total_cost, Pen_Rdx, Pen_Tnt, gridflagx,gridflagy,drycell_c,drycell_h)
          endif

          pw=1.0d0
          mpi_all(1) = j
          mpi_all(2) = Fitness1+pw*MB
          mpi_all(3) = Total_cost
          mpi_all(4) = Pen_Rdx
          mpi_all(5) = Pen_Tnt
          mpi_all(6) = drycell_c
          mpi_all(7) = drycell_h
          mpi_all(8) = pw*MB

          call MPI_SEND(mpi_all, 8, MPI_DOUBLE_PRECISION, 0, mpi_processor_id, MPI_COMM_WORLD, mpi_status, mpi_ierr)

          rtime(2) = MPI_WTIME()
          write(*,*) j, Fitness1, rtime(2)-rtime(1)
          !write(*,*) mpi_processor_id, j, Fitness1
          !Commented By Eva Sinha
          !do i= 1, noremwells
          !   mpi_Qw(j,i)= Qw(i)
          !end do    

       end do

       !after all the function evaluations are done, send the array containing their results to the master
       !TAG = returning slave's processor id


       !      Commented By Eva Sinha
       !     do i= 1, noremwells
       !       call MPI_SEND(mpi_Qw((mpi_subsiz * mpi_processor_id - mpi_subsiz + 1):(mpi_processor_id*mpi_subsiz) &
       !                   & ,i), mpi_subsiz, MPI_DOUBLE_PRECISION, 0, mpi_processor_id, &
       !                   MPI_COMM_WORLD, mpi_status, mpi_ierr)
       !     end do
    end if               !------------------------LOOP FOR SLAVE ENDS----------------------

    rtime2(2) = MPI_WTIME()
    if(mpi_processor_id .eq. 0) write(*,*) rtime2(2) - rtime2(1), rtime3(2) - rtime3(1)

    return
  end subroutine sGA_evalout

  !#######################################################################

!!!!  Please eliminate this subroutine    


  !  subroutine selectn(ipick,j,mate1,mate2)
  !
  !  Subroutine for selection operator.  Presently, tournament selection
  !  is the only option available.
  !
  !   IMPLICIT NONE
  !  save
  !
  ! integer :: n,j,mate1,mate2,ipick,ind1,ind2, idum3
  !
  !  If tournament selection is chosen (i.e. itourny=1), then
  !  implement "tournament" selection for selection of new population.
  !  Otherwise proportionate selection will be invoked
  !    idum3=1
  !   if (itourny.eq.1 .or. itourny.eq.2) then
  !     call tourn_select(mate1,ipick)
  !    call tourn_select(mate2,ipick)
  !    else 
  !Stoch. rem. selectn. randomly draws individuals from mating pop.
  !      call ran1(idum3,rand)
  !     ind1 = 1 + dint(rand*(dble(num_mate_pop)-1))
  !    mate1 = mate_pop(ind1)
  !   call ran1(idum3,rand)
  !     ind2 = 1 + dnint(rand*(dble(num_mate_pop)-1))
  !    mate2 = mate_pop(ind2)
  !   end if
  !        write(3,*) mate1,mate2,fitness(mate1),fitness(mate2)
  !  do n=1,nchrome
  !       ichild(j,n)=iparent(mate1,n)
  !      if(nchild.eq.2) ichild(j+1,n)=iparent(mate2,n)
  !  end do
  !
  ! return
  ! end subroutine selectn
  ! 

  !##################################################################################

  !============================= COMMON PROCESSES ===================================
  !================================ SGA & HGA ======================================

  !##################################################################################
  !  Subroutine for selection operator.  Presently, tournament selection
  !  is the only option available.

  subroutine selectwr(ipick,j,mate1,mate2)

    IMPLICIT none
    save

    integer :: n,j,mate1,mate2,ipick,ind1,ind2

    !  If tournament selection is chosen (i.e. itourny=1), then
    !  implement "tournament" selection for selection of new population.
    !  Otherwise proportionate selection will be invoked
    if (itourny.eq.1.or.itourny.eq.2) then
       call tswr(mate1,ipick)
       call tswr(mate2,ipick)
    else 
       !Stoch. rem. selectn. randomly draws individuals from mating pop.
       call ran1(1,rand)
       ind1 = 1 + dint(rand*(dble(num_mate_pop)-1))
       mate1 = mate_pop(ind1)
       call ran1(1,rand)
       ind2 = 1 + dnint(rand*(dble(num_mate_pop)-1))
       mate2 = mate_pop(ind2)
    endif
    do n=1,nchrome
       ichild(j,n)=iparent(mate1,n)
       if(nchild.eq.2) ichild(j+1,n)=iparent(mate2,n)
    enddo

    return

  end subroutine selectwr

  !#######################################################################
  !  Subroutine for selection operator. 

  subroutine selectwor(npop,index,mate1,mate2,j)

    IMPLICIT none
    save

    integer :: npop,mate1,mate2,index(npop),i,j,n

    call tswor(npop,mate1,index)
    npop=npop-tsize
    call tswor(npop,mate2,index)
    npop=npop-tsize

    do n=1,nchrome
       ichild(j,n)=iparent(mate1,n)
       if(nchild.eq.2) ichild(j+1,n)=iparent(mate2,n)
    enddo

    return

  end subroutine selectwor


  !#######################################################################
  !  TOURNAMENT SELECTION
  !  This routine selects the better of two possible parents for mating.

  subroutine tswr(mate,ipick)

    IMPLICIT none
    save

    integer :: i,ind(20),bestloc(1),mate,ipick

    double precision :: ttfitness(tsize)

    do i=1,tsize
       call ran2(1,rand)
       ind(i)=1+dint(dble(npopsiz-1)*rand)
       ttfitness(i)=fitness(1,ind(i))
    enddo
    bestloc=minloc(ttfitness)
    mate=ind(bestloc(1))

  end subroutine tswr

  !#######################################################################
  !  TOURNAMENT SELECTION
  !  This routine selects the better of two possible parents for mating.

  subroutine tswor(npop,mate,index)

    IMPLICIT none
    save

    integer :: i,ind(tsize),bestloc(1),mate,ipick,npop,index(npopsiz),indiv

    double precision :: ttfitness(tsize)

    do i=1,tsize
5      call ran2(1,rand)
       indiv=nint(1+(npop-1)*rand)
       if(index(indiv).eq.-1) goto 5
       ind(i)=index(indiv)
       ttfitness(i)=fitness(1,ind(i))
       index(indiv)=index(npop-i+1)
       index(npop-i+1)=-1
    enddo

    bestloc=minloc(ttfitness)
    mate=ind(bestloc(1))

    return  


  end subroutine tswor


  !##################################################################################
  !This subroutine establishes the best individual.
  subroutine BestValue(best)

    IMPLICIT none
    save

    integer :: m
    double precision :: best

    best = huge(best)

    do j=1,npopsiz
       if(fitness(1,j).lt.best) then 
          best = fitness(1,j)
          jbest(1) = j
          !Writing the coded form of the best individual
          do m = 1,nchrome
             ibest(1,m)=iparent(j,m)
          enddo
       endif
    enddo

    return

  end subroutine BestValue

  !##################################################################################
  subroutine Avg(av0,std,nbest,best)

    IMPLICIT none
    save

    double precision :: av0,std,best,dif,tol

    integer :: nbest,mm

    nbest=0
    av0=0.d0
    std=0.d0

    do mm=1,npopsiz
       if(dabs(best).lt.1d-3) then
          dif=dabs(best-fitness(1,mm))
          tol=tol1
       else
          dif=dabs(fitness(1,mm)/best-1.0d0)
          tol=tol2
       endif
       if(dif.lt.tol) nbest=nbest+1
       av0=av0+fitness(1,mm)
    enddo

    av0=av0/dble(npopsiz)

    do mm=1,npopsiz
       std=std+(fitness(1,mm)-av0)**2.0d0
    enddo

    std=(std/dble(npopsiz-1))**0.5d0

    return

  end subroutine Avg


  ! End subroutine additions



  !#######################################################################
  !  subroutine selectwor(npop,index,mate1,mate2)
  !
  !  Subroutine for selection operator.  Presently, tournament selection
  !  is the only option available.
  !
  !   IMPLICIT NONE
  !  save
  !
  ! integer :: npop,mate1,mate2,index(npop),n

  !   call tswor(npop,mate1,index)
  !  call reorder(index,mate1,npop)
  !   call bubblesort2(npop,index)

  ! npop=npop-1

  !  call tswor(npop,mate2,index)
  ! call reorder(index,mate2,npop)
  !call bubblesort2(npop,index)

  !      npop=npop-1


  !return
  !end subroutine selectwor

  !#######################################################################
  ! subroutine reorder(index,mate,npop)
  !
  !  Subroutine for selection operator.  Presently, tournament selection
  !  is the only option available.
  !
  ! IMPLICIT NONE
  ! save
  !
  ! integer :: npop,mate,index(npop),n

  !  do n=1,npop
  !  if(mate.eq.index(n)) then
  !      index(n)=-1
  !       return
  !     endif
  !    enddo

  !return
  !end subroutine reorder

  !#######################################################################
  subroutine crosovr(ncross,j,mate1,mate2)
    !
    !  Subroutine for crossover between the randomly selected pair.
    IMPLICIT NONE
    save
    !
    integer j,ncross,icross,n,mate1,mate2, idum3
    !
    idum3=1
    if (.not. iunifrm) then
       !  Single-point crossover at a random chromosome point.
       call ran1(idum3,rand)
       if(rand.gt.pcross) goto 69
       ncross=ncross+1
       call ran1(idum3,rand)
       icross=2+dint(dble(nchrome-1)*rand)
       do n=icross,nchrome
          ichild(j,n)=iparent(mate2,n)
          if(nchild.eq.2) ichild(j+1,n)=iparent(mate1,n)
       end do
    else
       !  Perform uniform crossover between the randomly selected pair.
       do n=1,nchrome
          call ran1(idum3,rand)
          if(rand.le.pcross) then
             ncross=ncross+1
             ichild(j,n)=iparent(mate2,n)
             if(nchild.eq.2) ichild(j+1,n)=iparent(mate1,n)
          endif
       end do
    end if
69  continue
    !
    return
  end subroutine crosovr
  !
  !#######################################################################
  subroutine mutate
    !
    IMPLICIT NONE
    save
    !
    integer j,k,nmutate,ncreep, idum3
    double precision :: creep
    !  This subroutine performs mutations on the children generation.
    !  Perform random jump mutation if a random number is less than pmutate.
    !  Perform random creep mutation if a different random number is less
    !  than pcreep.  

    idum3=1
    nmutate=0
    ncreep=0
    do j=1,npopsiz
       do k=1,nchrome
          !  Jump mutation
          call ran1(idum3,rand)
          if (rand.le.pmutate) then
             nmutate=nmutate+1
             if(ichild(j,k).eq.0) then
                ichild(j,k)=1
             else
                ichild(j,k)=0
             endif
             !               if (nowrite.eq.0) write(6,1300) j,k
          endif
       end do
       !  Creep mutation (one discrete position away).
       if (icreep) then
          do k=1,nparam
             call ran1(idum3,rand)
             if(rand.le.pcreep) then
                call decode(j,child,ichild)
                ncreep=ncreep+1
                creep=1.0
                call ran1(idum3,rand)  
                if (rand.lt.0.5) creep=-1.0
                child(j,k)=child(j,k)+g1(k)*creep
                if (child(j,k).gt.parmax(k)) then
                   child(j,k)=parmax(k)-1.0*g1(k)
                elseif (child(j,k).lt.parmin(k)) then
                   child(j,k)=parmin(k)+1.0*g1(k)
                endif
                call code(j,k,child,ichild)
             endif
          end do
       endif
    end do
1250 format(/'  Number of Jump Mutations  =',i5/ &
         &        '  Number of Creep Mutations =',i5)
1300 format('*** Jump mutation performed on individual  ',i4, &
         &       ', chromosome ',i3,' ***')
1350 format('*** Creep mutation performed on individual ',i4, &
         &       ', parameter  ',i3,' ***') 
    !
    return
  end subroutine mutate

  !#######################################################################
  subroutine newgen(npossum,ig2sum)
    !
    !  Write child array back into parent array for new generation.  Check
    !  to see if the best parent was replicated; if not, and if ielite=.true.,
    !  then reproduce the best parent into a random slot.
    !
    IMPLICIT NONE
    save
    !
    integer :: npossum,ig2sum,jelite,n,irand,k,worst(1),idum3
    double precision ::  best
    double precision :: ff(npopsiz),f1,bestf0

    !
    ! Elitist reproduction for SGA

    idum3=1
    bestf0=bestf1

    if(maxobj.eq.1) then
       if (mpi_processor_id == 0) then  ! master loop 1 ======== 

          if (npossum.lt.ig2sum) call possibl(child,ichild)
          kelite=0
          do j=1,npopsiz
             jelite=0
             do n=1,nchrome
                iparent(j,n)=ichild(j,n)
                if (iparent(j,n).eq.ibest(1,n)) jelite=jelite+1
                if (jelite.eq.nchrome) kelite=1
             end do
          end do
       end if ! master loop 1 =======

       ! all processors go into sga_evalout ------------   
       print  *, 'newgen'   
       call SGA_evalout
       ! -----------------------------------------------

       if (mpi_processor_id == 0) then  ! master loop 2 ======== 

          ff(:)=fitness(1,:)
          worst=maxloc(ff)
          f1=maxval(ff)

          if (ielite .and. kelite .eq. 0) then

             call ran1(idum3,rand)
             irand=1+anint(dble(npopsiz-1)*rand)
             iparent(irand,1:nchrome) = ibest(1,1:nchrome)
             fitness(1,worst(1))=bestf0
          endif
       end if ! master loop 2 ========================

    else

       if (mpi_processor_id == 0) then  ! master loop 3 ===================
          !Elitist reproduction for NSGA
          if (npossum.lt.ig2sum) call possibl(child,ichild)
          if(num_elite.lt.1) num_elite = 1
          do k = 1,num_elite
             kelite = 0
             do j=1,npopsiz
                jelite=0
                do n = 1,nchrome
                   if(k.eq.1)then
                      iparent(j,n) = ichild(j,n)
                   endif
                   if(iparent(j,n).eq.ibest(k,n)) jelite=jelite+1
                   if(jelite.eq.nchrome) kelite=1
                end do
             end do
             if(ielite .and. kelite .eq. 0) then
                call ran1(idum3,rand)
                irand=1+dint(dble(npopsiz)*rand)
                iparent(irand,1:nchrome)=ibest(k,1:nchrome)
             end if
          end do
       end if ! master loop 3 ===================

    end if
1260 format('  Elitist Reproduction on Individual in next generation ',i4)
1266 format('  Invidual',i4,'in the current generation has index',i4,'in next generation')
    !
    return
  end subroutine newgen

  !#######################################################################

  subroutine possibl(array,iarray)
    !
    !  This subroutine determines whether or not all parameters are within
    !  the specified range of possibility.  If not, the parameter is
    !  randomly reassigned within the range.  This subroutine is only
    !  necessary when the number of possibilities per parameter is not
    !  optimized to be 2**n, i.e. if npossum < ig2sum.
    !
    IMPLICIT NONE
    save
    !
    integer i,j,idum3
    double precision :: array(indmax,nparmax)
    integer :: iarray(indmax,nchrmax)
    integer :: n2ig2j,irand
    !
    !      print *,'possibl'

    idum3=1
    do i=1,npopsiz
       call decode(i,array,iarray)
       do j=1,nparam
          n2ig2j=2**ig2(j)
          !            if(nposibl(j).ne.n2ig2j .and. array(i,j).gt.parmax(j) .and. &
          !                 & array(i,j).lt.parmin(j)) then
          if(nposibl(j).ne.n2ig2j .and. array(i,j).gt.parmax(j)) then
             call ran1(idum3,rand)
             irand=dint(dble(nposibl(j))*rand)
             array(i,j)=g0(j)+dble(irand)*g1(j)
             call code(i,j,array,iarray)
             !               if (nowrite.eq.0) write(6,1000) i,j
          endif
       end do
    end do
    !
1000 format('*** Parameter adjustment to individual     ',i4, &
         &       ', parameter  ',i3,' ***')
    !
    return
  end subroutine possibl

  !##################################################################################

  subroutine bubblesort1(npop,old,index1)

    IMPLICIT NONE
    save

    integer :: npop,j,n,k,tmp1,index1(npop)

    double precision :: old(npop),new(npop),tmp2

    new=old

    do n=1,npop
       do j=1,npop-n
          if(new(j+1)<new(j)) then
             tmp1=index1(j)
             index1(j)=index1(j+1)
             index1(j+1)=tmp1
             tmp2=new(j)
             new(j)=new(j+1)
             new(j+1)=tmp2
          endif
       enddo
    enddo

    do j=1,npop

       !  write(6,*) j,old(j),old(index1(j)),new(j)

    enddo

    old=new

    return

  end subroutine bubblesort1

  !####################################################################
  !subroutine bubblesort2(npop, index)
  ! IMPLICIT NONE
  ! save

  ! integer :: npop, j, n, tmp, index(npop)

  ! do n = 1, npop
  !  do j = 1, npop-n
  !  if(index(j+1) > index(j)) then
  !  tmp = index(j)
  !   index(j) = index(j+1)
  !    index(j+1) = tmp
  !   endif
  !   enddo
  ! enddo

  ! return

  !end subroutine bubblesort2
  !#######################################################################!
  !  subroutine tourn_select(mate,ipick)
  !  TOURNAMENT SELECTION
  !  This routine selects the better of two possible parents for mating.
  !  fespinoz

  !   IMPLICIT NONE
  !  save

  !   integer :: n,ind(20),bestloc(1),mate,ipick,idum3

  !  double precision :: tfitness(tsize)
  ! 
  !! idum3=1
  !do n=1,tsize
  ! call ran1(idum3,rand)
  !      ind(n)=1+dint(dble(npopsiz-1)*rand)
  !     tfitness(n)=fitness(1,ind(n))
  ! print *, i,ind(i),tfitness(i)
  !  enddo
  ! bestloc=minloc(tfitness)
  !   mate=ind(bestloc(1))
  !   print *, bestloc,mate

  ! end subroutine tourn_select

  !#######################################################################
  !  subroutine tswor(npop,mate,index)
  !  TOURNAMENT SELECTION
  !  This routine selects the better of two possible parents for mating.
  !  fespinoz

  !   IMPLICIT NONE
  !  save

  ! integer :: n,ind(20),bestloc(1),mate,ipick,npop,index(npop),indiv,idum3

  !double precision :: tfitness(tsize)
  ! 
  !   idum3=1
  !   do n=1,tsize
  !    call ran1(idum3,rand)
  !   indiv=1+dint(dble(npop)*rand)
  !  ind(n)=index(indiv)
  ! tfitness(n)=fitness(1,ind(n))
  !     enddo
  !    bestloc=minloc(tfitness)
  !   mate=ind(bestloc(1))


  ! end subroutine tswor
  ! ##################################################################################################################################

  subroutine stats (fit, avg, var, conv) 

    IMPLICIT NONE
    save

    double precision, intent(inout):: fit(1,npopsiz), avg, var, conv
    integer :: s,q, c, mm,j
    double precision :: best,dif,tol

    avg = 0.d0
    do s= 1, npopsiz
       avg= avg + fit(1,s)
    end do
    avg = avg/npopsiz

    var=0.d0
    do s= 1, npopsiz
       var= var+ ((fit(1,s)-avg)**2)
    end do
    var= var/(npopsiz-1)



    best = fit(1,1)
    do j= 2,npopsiz
       if (fit(1,j).le. best) then
          best = fit(1,j)
       end if
    end do

    conv = 0.0d0  

    do mm=1,npopsiz
       if(dabs(best).lt.1d-3) then
          dif=dabs(best-fitness(1,mm))
          tol=tol1
       else
          dif=dabs(fitness(1,mm)/best-1.0d0)
          tol=tol2
       endif
       if(dif.lt.tol) conv=conv+1.0d0

    enddo

    conv = conv/dble(npopsiz)*(100.0d0)

  end subroutine stats



  !################################################################################	                
  ! Felipe Espinoza 12/01/02
  ! This is the subroutine that reads the data for the remediation wells. 
  ! the total no. of possible locations for the remediation wells and their pumping flags
  ! decide the length of the string for the GA.
  ! Data is read from rwelloc.dat, which is a single file that stores the possible locations of all remediation wells.
  ! In serial order, it stores the 'name of the well', 'injection/extraction/both pumping flag'(that decides what kind 
  ! of pumping well it is),'maximum pumping capacity of a well', 'possible number of nodal locations for the well', 
  ! and finally followed by the 'actual list of
  ! possible locations'

  subroutine remedwell (gridflagx, gridflagy)

    IMPLICIT none
    save

    integer, intent(in) :: gridflagx, gridflagy
    character (80) :: flname1, flname2

    !Input data. First old wells and then new ones. No modifications are necessary to work with the double precision problem.
    if ( (gridflagx.eq.1) .and. (gridflagy.eq.1))then
       open(unit = 36, file='uma_coarse.txt',status='old')
    endif
    if ( (gridflagx.eq.2) .and. (gridflagy.eq.2))then
       open(unit = 36, file='uma_fine.txt' ,status='old')
    endif

    do i=1,oldwells
       nameold(i)=i
       read(36,*) ncomp(i)
       do j=1,ncomp(i)
          read(36,*) (temp(k),k=1,6) 
          rwlcold(i,j,1)=temp(1)
          rwlcold(i,j,2)=temp(2)
          rwlcold(i,j,3)=temp(3)
          rwlcold(i,j,4)=temp(4)
          maxpumpold(i,j)=dble(nint(dble(temp(5))/(1.4238d-5))) ! changes the unit to ft^3/yr
          pumpflagold(i,j)=temp(6)
       enddo
    enddo
    do i=1,newwells
       namenew(i)=i
       read(36,*) possibnodes(i)
       do j=1,possibnodes(i)
          read(36,*) (temp(k),k=1,6) 
          rwlcnew(i,j,1)=temp(1)
          rwlcnew(i,j,2)=temp(2)
          rwlcnew(i,j,3)=temp(3)
          rwlcnew(i,j,4)=temp(4)
          maxpumpnew(i,j)=dble(nint(dble(temp(5))/(1.4238d-5)))  ! changes the unit to ft^3/yr
          pumpflagnew(i,j)=temp(6)
       enddo
    enddo
    close(36)

    return

  end subroutine remedwell

  ! ########################################################################################################
  ! Felipe Espinoza 12/01/02! This is a subroutine that initializes some of the variables defined in par.f90
  ! Since these variables are dependant on the case study, thus they are initialized here.
  ! Also note that the dimension '15' given to some of the variables should be as close to the
  ! actual number of variables, to avoid arraybound problems.
  ! This also creates the chromosome, once the details of the case study are known.

  subroutine parinit

    Implicit none
    save   

    integer :: st
    ! nparmax stores the maximum no. of parameters in the chromosome
    ! which is equal to well locations, pumping rates, whether to install wells,
    ! and parameters for the wells which have pumpflag=3, i.e. wells that can be 
    ! either injection or extraction.


    ! no. of possibilities for parameters for well locations,puming rates
    ! pumping type (inj/ext),whether to install
    ! the wells or not, and finally an extra parameter for memory buffering.

    !Evaluates the # of bits for new well locations     

    do i=1,newwells
       binpossibnodes(i) = 2**( ceiling( log10( real( possibnodes(i) ) ) / log10( 2.0 ) ) )
    enddo

    !Initialization of counters and arrays

    nparmax=0
    j=0

    parmax=0
    parmin=0
    nichflg=1
    nposibl=0

    !Parameter setting performed stress period after stress period

    do st=1,nstress
       !Set parameters for new well locations
       do i=1,newwells
          j=j+1
          nposibl(j)=binpossibnodes(i)
          parmin(j)=1
          parmax(j)=possibnodes(i)
          nparmax=nparmax+1
       enddo
       !Set installation flags for new wells & basins
       do i=1,newwells
          j=j+1
          nposibl(j)=2
          parmin(j)=0
          parmax(j)=1
          nparmax=nparmax+1
       enddo
       !Set installation flags for old wells & basins
       do i=1,oldwells
          j=j+1
          nposibl(j)=2
          parmin(j)=0
          parmax(j)=1
          nparmax=nparmax+1
       enddo
    enddo

    nparam=nparmax

    !    open(unit = 37, file='uma1.res',status='unknown')
    !    write(37,100) (nposibl(i),i=1,nparmax)
    !    write(37,110) (parmin(i),i=1,nparmax)
    !    write(37,110) (parmax(i),i=1,nparmax)
    !    write(37,100) (nichflg(i),i=1,nparmax)

100 format(100i4)
110 format(100f4.0)

    !    close(37)        		 

    return

  end subroutine parinit

  !#######################################################################################################################
  !    ninj1	=	number of injection wells which are being installed in new wells
  !    ninj2	=	number of injection wells which are being installed in old wells 
  !               ..... remember that ninj2 already takes the various componenst of the injection basin into account
  !    next1	=	number of extraction wells which are being installed in new wells
  !    next2	=	number of extraction wells which are being installed in old wells
  !    next	=	total number of extraction wells (new + old)
  !    ncount2 =	counter for checking if the one of the parameter value in the parent is changed in a given stress period
  !    ncount1=	counter for checking if the parent is changed in any of the stress periods
  !    ncount0 =	counter for checking if the one of the parameter value in the parent is changed in a given stress period
  !               if ncount0=1 then the value of inj1, inj2 etc is recalculated.

  subroutine prepfunc(indiv,MB, gridflagx, gridflagy)

    IMPLICIT none
    save

    integer :: ni,ns,nw,nwell,indiv,j,k,p,m,next,next1,next2,ninj,ninj1,ninj2,ncount,totwell,ncount0,ncount1, ncount2,np,ncc
    integer ns1, ns2, flag, ni1, ni2, nwell_1, nwell_2, new,nw1,nw2,gridflagx,gridflagy
    integer, dimension(nstress) :: NumNewBasin
    integer, dimension(newwells*nstress) :: ndrwx,ndrwy,ndrwz
    character(80) :: flname1, flname2
    double precision :: Qext,Qinjf,rand,alpha,DeltaQext,DeltaQinj,MB

    if(gridflagx .eq. 1 .and. gridflagy.eq.1) flname1 = 'uma4_coarse.res'
    if(gridflagx .eq. 2 .and. gridflagy.eq.2) flname1 = 'uma4_fine.res'

    flname2 = 'optdemo.wel'
    call APPEND_PID(flname1)
    call APPEND_PID(flname2)
    open(unit = 88, file=flname1,status='unknown') 
    open(unit = 13, file=flname2,status='old')
    rewind(13)
    read(13,*)       !ignoring the first line of the file
    rewind(88)    
    ! This is where the preparation for the fitness function takes place
    ! The "array" (or "parent") has a list of rem well locations (for each well), 

    ! if parent(1) and parent(2) are same change them

    call decode(indiv,parent,iparent)

    do ns=1,nstress

       alpha=1.0d0
       ncount2=0

10     ncount0=0
       ninj1=0
       next1=0
       ninj2=0
       next2=0
       Qext=0.d0

       do nw=1,newwells
          ni=nw+(2*newwells+oldwells)*(ns-1)
          nwell=int(anint(parent(indiv,ni)))
          if(pumpflagnew(nw,nwell).eq.2) then 
             ninj1=ninj1+parent(indiv,ni+newwells)		! check whether or not the well is installed
          else
             next1=next1+parent(indiv,ni+newwells)		! check whether or not the well is installed
             Qext=Qext+parent(indiv,ni+newwells)*maxpumpnew(nw,nwell)
          endif
       enddo
       do nw=1,oldwells
          ni=nw+(2*newwells+oldwells)*(ns-1)
          if(pumpflagold(nw,1).eq.2) then 
             ninj2=ninj2+ncomp(nw)*parent(indiv,ni+2*newwells)
          else
             next2=next2+parent(indiv,ni+2*newwells)
             Qext=Qext+parent(indiv,ni+2*newwells)*maxpumpold(nw,1)
          endif
       enddo

       next=next1+next2
       ninj=ninj1+ninj2

       Qinjf=0.0d0

       if(ninj.ne.0) then
          do k=1,5	! k  represents the number of component of the inhection basin k=1 means two wells, k=2 means four wells and k=3 means 6 wells
             Qinjf=Qext/dble(2*(k-1)*ninj1+ninj2)
             if(Qinjf.lt. (dble(nint(dble(Qmax)/(1.4238d-5))))) goto 20   ! converts the Qmax to ft^3/yr)
          enddo
          k=5
          Qinjf=Qext/dble(2*(k-1)*ninj1+ninj2)
20        totwell=2*(k-1)*ninj1+ninj2+next
          DeltaQinj=(Qinjf*1.4238d-5-Qmax)*(2*(k-1)*ninj1+ninj2)
          if(DeltaQinj .lt. 0.0d0) DeltaQinj=0.0d0
       else
          k=1
          DeltaQinj=Qext*1.4238d-5
          Totwell=next
       endif

       write(221,*) k,ninj1,ninj2,totwell,Qinjf*1.4238d-5,Qmax,DeltaQinj

       DeltaQext=0.d0
       nw1=parent(indiv,1)*parent(indiv,newwells+1)
       nw2=parent(indiv,2)*parent(indiv,newwells+2)

       if(nw1.eq.nw2) DeltaQext=parent(indiv,newwells+2)*maxpumpnew(nw1,nwell)*1.4238d-5

       MB=DeltaQext+DeltaQinj

       write(88,4) totwell

       do nw=1,newwells
          ni=nw+(2*newwells+oldwells)*(ns-1)
          nwell=int(anint(parent(indiv,ni)))
          if(pumpflagnew(nw,nwell).eq.1.and.parent(indiv,ni+newwells).eq.1) then	! check whether it was an extraction well and that it has been installed 
             write(88,4) -namenew(nw)
             write(88,4) rwlcnew(nw,nwell,4)
             write(88,4) rwlcnew(nw,nwell,3)
             write(88,4) rwlcnew(nw,nwell,2)
             write(88,5) -maxpumpnew(nw,nwell)
          endif
          if(pumpflagnew(nw,nwell).eq.2.and.parent(indiv,ni+newwells).eq.1) then	! check whether it was an injection well and that it has been installed
             if(k.eq.2) then	
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf			
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+1
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf
             endif
             if(k.eq.3) then
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+1
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)
                write(88,4) rwlcnew(nw,nwell,2)+1
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+1
                write(88,4) rwlcnew(nw,nwell,2)+1
                write(88,5) Qinjf
             endif
             if(k.eq.4) then
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+1
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)
                write(88,4) rwlcnew(nw,nwell,2)+1
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+1
                write(88,4) rwlcnew(nw,nwell,2)+1
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+2
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+2
                write(88,4) rwlcnew(nw,nwell,2)+1
                write(88,5) Qinjf
             endif
             if(k.eq.5) then
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+1
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)
                write(88,4) rwlcnew(nw,nwell,2)+1
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+1
                write(88,4) rwlcnew(nw,nwell,2)+1
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+2
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+2
                write(88,4) rwlcnew(nw,nwell,2)+1
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+3
                write(88,4) rwlcnew(nw,nwell,2)
                write(88,5) Qinjf
                write(88,4) namenew(nw)
                write(88,4) rwlcnew(nw,nwell,4)
                write(88,4) rwlcnew(nw,nwell,3)+3
                write(88,4) rwlcnew(nw,nwell,2)+1
                write(88,5) Qinjf
             endif

          endif
       enddo

       do nw=1,oldwells
          ni=nw+(2*newwells+oldwells)*(ns-1)
          if(pumpflagold(nw,1).eq.1.and.parent(indiv,ni+2*newwells).eq.1) then	! check whether it was an extraction well and that it has been installed
             write(88,4) -nameold(nw)
             write(88,4) rwlcold(nw,1,4)
             write(88,4) rwlcold(nw,1,3)
             write(88,4) rwlcold(nw,1,2)
             write(88,5) -maxpumpold(nw,1)
          endif
          if(pumpflagold(nw,1).eq.2.and.parent(indiv,ni+2*newwells).eq.1) then		! check whether it was an injection well and that it has been installed
             do nwell=1,ncomp(nw)		
                write(88,4) nameold(nw)
                write(88,4) rwlcold(nw,nwell,4)
                write(88,4) rwlcold(nw,nwell,3)
                write(88,4) rwlcold(nw,nwell,2)
                write(88,5) Qinjf
             enddo
          endif
       enddo

       ncount1=ncount1+ncount2

       !       writing in the optdemo.wel file
4      FORMAT (I3)
5      FORMAT (F25.5)
6      FORMAT(I2,I4,I4,F25.5) ! new modflow files
       write(13,'(I3)') totwell
       do nw=1,newwells
          ni=nw+(2*newwells+oldwells)*(ns-1)
          nwell=int(anint(parent(indiv,ni)))
          if(pumpflagnew(nw,nwell).eq.1.and.parent(indiv,ni+newwells).eq.1) then	! check whether it was an extraction well and that it has been installed
             write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2), -maxpumpnew(nw,nwell)
          endif
          if(pumpflagnew(nw,nwell).eq.2.and.parent(indiv,ni+newwells).eq.1) then	! check whether it was an injection well and that it has been installed
             if(k.eq.2) then
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2), Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2), Qinjf
             endif
             if(k.eq.3) then
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2), Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2), Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2)+1, Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2)+1, Qinjf
             endif
             if(k.eq.4) then
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2), Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2), Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2)+1, Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2)+1, Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+2, rwlcnew(nw,nwell,2), Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+2, rwlcnew(nw,nwell,2)+1, Qinjf
             endif
             if(k.eq.5) then
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2), Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2), Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2)+1, Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2)+1, Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+2, rwlcnew(nw,nwell,2), Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+2, rwlcnew(nw,nwell,2)+1, Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+3, rwlcnew(nw,nwell,2), Qinjf
                write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+3, rwlcnew(nw,nwell,2)+1, Qinjf
             endif
          endif
       enddo

       do nw=1,oldwells
          ni=nw+(2*newwells+oldwells)*(ns-1)
          if(pumpflagold(nw,1).eq.1.and.parent(indiv,ni+2*newwells).eq.1) then	! check whether it was an extraction well and that it has been installed
             write(13,6) rwlcold(nw,1,4), rwlcold(nw,1,3), rwlcold(nw,1,2), -maxpumpold(nw,1)
          endif
          if(pumpflagold(nw,1).eq.2.and.parent(indiv,ni+2*newwells).eq.1) then		! check whether it was an injection well and that it has been installed
             do nwell=1,ncomp(nw)
                write(13,6) rwlcold(nw,nwell,4), rwlcold(nw,nwell,3), rwlcold(nw,nwell,2), Qinjf
             enddo
          endif
       enddo
       !		writing in the optdemo file finishes

    enddo		!loop for stress period

    ! -------------------------------------------------------------------------------------
    !		Calculating the number of new basins in each stress period
    do ns = 1, nstress
       NumNewBasin(ns) = 0
    end do
    ! assigning number of new basins for the first stress period
    do nw = 1, newwells
       nwell=int(anint(parent(indiv,nw)))
       if(pumpflagnew(nw,nwell).eq.2 .and. parent(indiv, nw+newwells) .eq. 1) then
          NumNewBasin(1) = NumNewBasin(1) + 1
       end if
    end do

    ! assigning number of new basins for the remaining stress periods
    do ns2 = 2, nstress
       do nw = 1, newwells
          flag = 0
          do ns1 = 1, (ns2-1)
             ni1 = nw+(2*newwells+oldwells)*(ns1-1)
             ni2 = nw+(2*newwells+oldwells)*(ns2-1)
             nwell_1=int(anint(parent(indiv,ni1)))
             nwell_2=int(anint(parent(indiv,ni2)))
             if(pumpflagnew(nw,nwell_2).eq.2 .and. parent(indiv, ni2+newwells) .eq. 1) then 
                if(pumpflagnew(nw,nwell_1).eq.2 .and. parent(indiv, ni1+newwells) .eq. 1) then
                   if (nwell_1 .eq. nwell_2) then		
                      ! the basin already  existed in a previous stress period
                      flag = 1
                      goto 60		! move for the next well ...this basin is an old one	
                   end if
                end if
             else
                goto 60
             end if
50        end do		!loop for ns1
          if (flag.eq.0) then
             NumNewBasin(ns2) = NumNewBasin(ns2) + 1
          end if
60     end do		!loop for newwells
    end do			!loop for ns1
    !-------------------------------------------------------------------------------------------
    ! Writing the number of new basin for each stress period in the uma file
    do ns = 1, nstress
       write (88,*) NumNewBasin(ns)
    end do
    !-------------------------------------------------------------------------------------------
    ! printing the parent(indiv)

    !   write (*,321) mpi_processor_id, indiv, (nint(parent(indiv,i)), i = 1, nparam)
321 format (i4,2x,i4,2x, 7i5, 2x,7i2,2x,7i2)
    close (88)		!	closing the uma.res file
    endfile(13)
    close (13)		!	closing the OPTDEMO.wel file  

    return

  end subroutine prepfunc

  !#####################################################################
  !  This subroutine sets up the program by generating the g0, g1 and 
  !  ig2 arrays, and counting the number of chromosomes required for the
  !  specified input.  The subroutine also initializes the random number 
  !  generator, parent and iparent arrays (reads the ga.restart file).
  !
  !  g0       = lower bound values of the parameter array to be optimized.  
  !  g1       = the increment by which the parameter array is increased 
  !             from the lower bound values in the g0 array.  The minimum 
  !             parameter value is g0 and the maximum parameter value 
  !             equals g0+g1*(2**g2-1), i.e. g1 is the incremental value 
  !             between min and max.
  !  ig2      = array of the number of bits per parameter, i.e. the number
  !             of possible values per parameter.  For example, ig2=2 is 
  !             equivalent to 4 (=2**2) possibilities, ig2=4 is equivalent 
  !             to 16 (=2**4) possibilities.
  !  ig2sum   = sum of the number of possibilities of ig2 array
  !  npossum  = sum of the number of possible values of all parameters

  subroutine initial

    IMPLICIT none
    save

    integer :: npossum,ig2sum
    integer :: i,j,k,l,n2j,ind
    character(80) ::  FLNAME

    double precision :: rand

    do i=1,nparam
       g0(i)=parmin(i)
       pardel(i)=parmax(i)-parmin(i)
       g1(i)=pardel(i)/dble(nposibl(i)-1)
    enddo

    do i=1,nparam
       do j=1,30
          n2j=2**j
          if (n2j.ge.nposibl(i)) then
             ig2(i)=j
             goto 8
          endif
          if (j.ge.30) then
             write(6,2000)
             write(126,2000)
             close(126)
             stop
          endif
       enddo
8      continue
    enddo

    !  Count the total number of chromosomes (bits) required
    nchrome=0
    npossum=0
    ig2sum=0
    do i=1,nparam
       nchrome=nchrome+ig2(i)
       npossum=npossum+nposibl(i)
       ig2sum=ig2sum+(2**ig2(i))
    enddo

    !  Initialize random number generator
    call ran2(idum1,rand)  !Changed by F. Espinoza (4/10/03)

    !    Added By Eva Sinha
    maxgen = nchrome * 2
    !    maxgen = 1


    !Initialize the random distribution of parameters in the individual
    !parents when irestrt=0.
    if(.not.irestrt) then
       istart=1
       do i=1,npopsiz
          do j=1,nchrome
             call ran2(1,rand) !Changed by F. Espinoza (4/10/03)
             !call random_number(rand)  !Changed by F. Espinoza (4/10/03)
             iparent(i,j)=1
             if(rand .lt. 0.5) iparent(i,j)=0
          enddo
       enddo
    else
       !if irestrt.ne.0 read from restart file
       FLNAME = 'ga.restart'
       open (unit = 25, file = FLNAME, status = 'old')
       rewind 25
       read(25, *) istart, npopsiz
       do j = 1, npopsiz
          read(25,*) k, (iparent(j,l), l =1,nchrome)
       end do
       close (25)
    end if
111 format(i3,2x,60i2)

    !   open(unit = 38, file='uma2.res',status='unknown')

    !   do i=1,npopsiz
    !write(38,100) i,(iparent(i,j),j=1,nchrome)
    !   enddo
100 format(i4,2x,200i2)
    !   close (38)



    return

1800 format(1x,'ERROR: nchrome > nchrmax.  Set nchrmax = ',i6)
2000 format(1x,'ERROR: You have a parameter with a number of '/ &
         &       1x,'   possibilities > 2**30!  If you double precisionly desire this,'/ &
         &       1x,'   change the DO loop 7 statement and recompile.'// &
         &       1x,'   You may also need to alter the code to work with'/ &
         &       1x,'   DOUBLE PRECISION numbers rather than INTEGER numbers; Fortran'/ &
         &       1x,'   does not like to compute 2**j when j>30.') 
2100 format(1x,'WARNING: for some cases, a considerable performance'/ &
         &       1x,'   reduction has been observed when running a non-'/ &
         &       1x,'   optimal number of bits with the micro-GA.'/ &
         &       1x,'   If possible, use values for nposibl of 2**n,'/ &
         &       1x,'   e.g. 2, 4, 8, 16, 32, 64, etc.  See ReadMe file.')

  end subroutine initial

  !##################################################################################
  subroutine decode(i,array,iarray)

    IMPLICIT none
    save

    double precision :: array(indmax,nparam)

    integer :: iarray(indmax,nchrmax)
    integer i,l,k,m,j,iparam

    l=1
    do k=1,nparam
       iparam=0
       m=l
       do j=m,m+ig2(k)-1
          l=l+1
          iparam=iparam+iarray(i,j)*(2**(m+ig2(k)-1-j))
       enddo
       array(i,k)=g0(k)+g1(k)*dble(iparam)
    enddo

    return

  end subroutine decode

  !#######################################################################
  !  This routine codes a parameter into a binary string.

  subroutine code(j,k,array,iarray)

    IMPLICIT none
    save

    double precision :: array(indmax,nparmax)

    integer :: iarray(indmax,nchrmax)
    integer i,k,m,j,iparam,cstart

    !  First, establish the beginning location of the parameter string of
    !  interest.

    cstart=1
    do i=1,k-1
       cstart=cstart+ig2(i)
    enddo

    !  Find the equivalent coded parameter value, and back out the binary
    !  string by factors of two.
    m=ig2(k)-1
    if (g1(k).eq.0.0) return
    iparam=nint((array(j,k)-g0(k))/g1(k))
    do i=cstart,cstart+ig2(k)-1
       iarray(j,i)=0
       if ((iparam+1).gt.(2**m)) then
          iarray(j,i)=1
          iparam=iparam-2**m
       endif
       m=m-1
    enddo

    return

  end subroutine code

  !##################################################################################

  !================================ RANDOM NUMBERS ==================================

  !##################################################################################
  !Returns a uniform random deviate between 0.0 and 1.0.  Set idum to 
  !any negative value to initialize or reinitialize the sequence.
  !This function is taken from W.H. Press', "Numerical Recipes" p. 199.!

  subroutine ran1(idum,rand)

    IMPLICIT none
    save

    double precision :: mbig=8800000.d0,mseed=1618033.d0,mz=0.d0,fac=1./8800000.d0
    double precision :: mj,mk,ma,rand

    integer :: idum,iff,ii,k,inext,inextp

    dimension ma(55)

    data iff /0/

    if(idum.lt.0 .or. iff.eq.0) then
       iff=1
       mj=mseed-dble(iabs(idum))
       mj=dmod(mj,mbig)
       ma(55)=mj
       mk=1
       do i=1,54
          ii=mod(21*i,55)
          ma(ii)=mk
          mk=mj-mk
          if(mk.lt.mz) mk=mk+mbig
          mj=ma(ii)
       enddo
       do k=1,4
          do i=1,55
             ma(i)=ma(i)-ma(1+mod(i+30,55))
             if(ma(i).lt.mz) ma(i)=ma(i)+mbig
          enddo
       enddo
       inext=0
       inextp=31
       idum=1
    endif

    inext=inext+1
    if(inext.eq.56) inext=1
    inextp=inextp+1
    if(inextp.eq.56) inextp=1
    mj=ma(inext)-ma(inextp)
    if(mj.lt.mz) mj=mj+mbig
    ma(inext)=mj
    rand=mj*fac

    return

  end subroutine ran1


  !##################################################################################
  !Returns a uniform random deviate between 0.0 and 1.0.  Set idum to 
  !any negative value to initialize or reinitialize the sequence.
  !This function is taken from W.H. Press', "Numerical Recipes" p. 199.!

  subroutine ran2(idum,rand)

    IMPLICIT none
    save

    double precision :: mbig=8800000.d0,mseed=1618033.d0,mz=0.d0,fac=1./8800000.d0
    double precision :: mj,mk,ma,rand

    integer :: idum,iff,ii,k,inext,inextp

    dimension ma(55)

    data iff /0/

    if(idum.lt.0 .or. iff.eq.0) then
       iff=1
       mj=mseed-dble(iabs(idum))
       mj=dmod(mj,mbig)
       ma(55)=mj
       mk=1
       do i=1,54
          ii=mod(21*i,55)
          ma(ii)=mk
          mk=mj-mk
          if(mk.lt.mz) mk=mk+mbig
          mj=ma(ii)
       enddo
       do k=1,4
          do i=1,55
             ma(i)=ma(i)-ma(1+mod(i+30,55))
             if(ma(i).lt.mz) ma(i)=ma(i)+mbig
          enddo
       enddo
       inext=0
       inextp=31
       idum=1
    endif

    inext=inext+1
    if(inext.eq.56) inext=1
    inextp=inextp+1
    if(inextp.eq.56) inextp=1
    mj=ma(inext)-ma(inextp)
    if(mj.lt.mz) mj=mj+mbig
    ma(inext)=mj
    rand=mj*fac

    return

  end subroutine ran2

  ! ######################################################################################
  subroutine restart(i,istart,kount)
    !
    !  This subroutine writes restart information to the ga.restart file. 
    !
    IMPLICIT NONE
    save
    !
    integer :: kount,istart,i,l
    character(80):: FLNAME

    FLNAME = 'ga.restart'

    kount=kount+1
    if(i.eq.maxgen+istart-1 .or. kount.eq.kountmx) then
       OPEN (UNIT=25, file=FLNAME, STATUS='OLD')
       rewind 25
       write(25,*) i+1,npopsiz
       write(*,*) i+1, npopsiz
       do j=1,npopsiz
          write(*,1500)j, (iparent(j,l), l = 1, nchrome)
          write(25,1500) j,(iparent(j,l),l=1,nchrome)
       end do
       CLOSE (25)
       kount=0
    endif
    !
1500 format(i5,3x,51i2)
    !
    return
  end subroutine restart
  !#######################################################################################
end program uma
 










