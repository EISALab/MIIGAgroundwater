program uma


  use PARAMS
  use condor_helper
  use condorio
  use randx1
  use randx2

  IMPLICIT NONE
  save
  
     integer :: istart, kount,ipick, ncross, num_mate_pop, mate_pop(indmax), index(2*indmax)
     integer :: i,j,k,a,b,c, jbest(indmax), drycell_c, drycell_h,gaflag
     integer :: kelite, mate1, mate2
     integer :: ibest(indmax,nchrmax), num_elite
     integer :: execval, gridflagx, gridflagy
     double precision :: Fitness1,Total_cost, Pen_Rdx, Pen_Tnt
     double precision :: rand_x, bestf1
     double precision,allocatable :: ftemp(:)
     double precision :: fitness(maxobj,indmax)
     double precision:: mean, variance

! Added by Felipe Espinoza (4/9/03)
     integer, allocatable :: drycell(:,:),tdrycell(:,:)
     double precision,allocatable :: pen1(:),pen2(:),pen3(:),pen4(:),tpen1(:),tpen2(:),tpen3(:),tpen4(:),cost(:),tcost(:)
     character (80) :: name1, name2, name3, name4, name5, namefile
     logical :: feed
	 integer, allocatable :: temp_fine_flag(:) 

! End added

! cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  ! Added by Eva Sinha 
  if (multiscale2) then
     gridflagx = 1 ! 2 refers to fine grid which has 132rows and 125 cols
     gridflagy = 1 ! 1 refers to coarse grid which has 66 rows and  64 cols
  else
     gridflagx = 2
     gridflagy = 2
  end if

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
	 allocate(pen4(indmax))
     allocate(tpen1(2*indmax))
     allocate(tpen2(2*indmax))
     allocate(tpen3(2*indmax))
	 allocate(tpen4(2*indmax))
     allocate(cost(indmax))
     allocate(tcost(2*indmax))
  !variables for Multiscale_II
  allocate(fine_flag(indmax))
  allocate(fine_age(indmax))
  allocate(temp_fine_flag(indmax))

	call InitSocket
	call MakeStaticFolder

  if (multiscale2) then	
     fine_flag(:) = 1 ! 1 for coarse grid
  else
     fine_flag(:) = 2 ! 2 for fine grid
  end if

  if ((gridflagx .eq. 1) .and. (gridflagy.eq.1))then
     namefile = "caseinp_coarse.dat"
  end if
  if ((gridflagx .eq. 2) .and. (gridflagy.eq.2))then
     namefile = "caseinp_fine.dat"
  end if
  
     OPEN(UNIT=136, FILE=namefile, STATUS='old')             !Modified by F. Espinoza (4/9/03)
     read(136,*) gaflag,npopsiz,tsize,pcross,feed,idum1,name1,name2,name3,name4,name5  !Modified by F. Espinoza (4/9/03)
     CLOSE(136)

     
	 !print *,name5

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
     
     !  Perform necessary initialization and read the ga.restart file.   
     call initial

     if(mutation) then
        pmutate=1.0d0/npopsiz
     endif
     
     if(cross) then
        pcross=dble(tsize-1)/dble(tsize)
     endif

     if (gaflag.eq.1) then
        call sga
	 else
	    call hga
     end if

	call ClearSocket

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
	 deallocate(pen4)
     deallocate(tpen1)
     deallocate(tpen2)
     deallocate(tpen3)
	 deallocate(tpen4)
     deallocate(cost)
     deallocate(tcost)
  !variables for Multiscale_II
  deallocate(fine_flag)
  deallocate(fine_age)
  deallocate(temp_fine_flag)

     ! End added
  ! MPI termination 
     
     !This command makes the subsequent subroutines internal procedures
     !with host association to variables in the main program
   contains 
     
!###################################################################
subroutine sga
!Main processing loop for the simple genetic algorithm
       
  IMPLICIT NONE
  save
  integer :: n, runend,j,p, nnchild,igen, nk, n1
  integer :: ni,nj,mm,gen,nBald,nind,fev,feve,nstop,ils,nbest,ifin,geneq,npop
  integer :: itemp(2*indmax,nchrmax),bestj(1),idum3
  integer :: ls(indmax), item1(indmax,nchrmax)
  integer :: counter, numfine, numcoarse, numc, numf, fine_gen(maxgen), tfine_age(npopsiz), tfine_flag(npopsiz)   
  double precision :: ff(indmax)
  double precision :: best,av0,std,dif,DeltaSGA,CVR
  double precision ::t1, t2, t3,t4, time_pre, totaltime, converge
  double precision :: tfit1(indmax), tcost1(indmax), tpen11(indmax), tpen12(indmax),tpen13(indmax),tpen14(indmax), tdry(2,indmax), temp_flag(indmax)


  
  
  ! Added by Felipe Espinoza (4/9/03)

  integer :: feedchar(nchrmax),k, ind
  double precision :: f0

1112 format(/'#################  Generation',i5,'  #################')
1113 format(i5,2x,i2,2x,6(f20.10,2x), 7i4,2x,4(f5.3,2x),7i2,3x,4(f5.3,2x),7i2,2x,i5,2x,i5)
1115 format(/'#################  Generation ',i5,'  ######### ',f20.3, ' ########')
1116 format(7f5.0,2x,14f3.0)
1117 format(f20.3,2x,i5,2x,i2,2x,6(f20.10,2x),7i4,2x,4(f5.3,2x),7i2,3x,4(f5.3,2x),7i2,2x,i5,2x,i5)
  ! End added
  kount = 0
  execval = 0
  converge = 0.d0
  runend = 0
    time_pre = 0.d0

  
  fev=0
  feve=0

  call cpu_time(t1)

  write(*,*) 'npopsiz, maxgen, multiscale2, gridflagx, best_frac*npopsiz, feed, irestrt'
  write(*,*)  npopsiz, maxgen, multiscale2, gridflagx, best_frac*npopsiz, feed, irestrt
  write(74,*) 'npopsiz, maxgen, multiscale2, gridflagx, best_frac*npopsiz, feed, irestrt'
  write(74,*)  npopsiz, maxgen, multiscale2, gridflagx, best_frac*npopsiz, feed, irestrt
  write (74,*)
  write (74,*)

  do igen = istart,maxgen+istart-1  ! GENERATIONS LOOPS

     ! felipe changes......====================================== 26 feb 2002
     ! evaluating fitnesses for generation zero.
     if(igen.eq.1) then                            !---------IGEN=1 LOOP BEGINS--------------

		write(124,1112) 0
		! Added by Felipe Espinoza (4/9/03)
		if (.not. multiscale2) then
			if(feed) then
			idum3=1
			call ran1(idum3,rand_x)
			ind = 1 + dint(rand_x*(dble(npopsiz-1)))
			read(557,*) k,f0,(feedchar(j),j=1,nchrome)
			iparent(ind,:)=0
			do j=1,nchrome
			iparent(ind,j)=feedchar(j)
			enddo
			close(557)
			fitness(1,ind)=f0
			endif
		end if

		temp_fine_flag(1:npopsiz) = fine_flag(1:npopsiz)
		call sGA_evalout(npopsiz, temp_fine_flag)
	

		call cpu_time(t2)
		write(124,1115) 0, t2 - t1
		write(*,1115) 0, t2 - t1
		do j = 1,npopsiz
			write(124, 1113) j, temp_fine_flag(j),fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
			& (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			& (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
			& drycell(1,j),drycell(2,j)
			write(*, 1113) j, temp_fine_flag(j),fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
			& (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			& (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
			& drycell(1,j),drycell(2,j)
		end do


		if (.not.(multiscale2)) then
			fev=fev+npopsiz
			call BestValue(best)
			call Avg(av0,std,nbest,best)
			write(74,2223) 0,t2-t1, av0,std,std/av0,best,nbest
		end if

2223    format(i4,2x,f20.3, 2x, f18.8,2x,f25.12,2x,f18.12,2x,f15.2,2x,i6,2x,i10,2x,i10)

        ! End Felipe
     endif                                        !------------IGEN=1 LOOP ENDS------------------

     if(igen.eq.istart.and.irestrt) then          !---------IGEN=ISTART LOOP BEGINS--------------

		write(124,1112) istart

		temp_fine_flag(1:npopsiz) = fine_flag(1:npopsiz)
		call sGA_evalout(npopsiz, temp_fine_flag)

		call cpu_time(t2)
		write(124,1115) istart, t2-t1
		write(*,1115) istart, t2-t1

		do j = 1, npopsiz
			write(124, 1113) j, temp_fine_flag(j),fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
			& (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			& (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
			& drycell(1,j),drycell(2,j)
			write(*, 1113) j, temp_fine_flag(j),fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
			& (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			& (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
			& drycell(1,j),drycell(2,j)
		end do

		! Added by Felipe Espinoza (4/9/03)
		if (.not.(multiscale2)) then
			fev=fev+npopsiz
			call BestValue(best)
			call Avg(av0,std,nbest,best)
			write(74,2223) istart,t2 - t1,av0,std,std/av0,best,nbest
		end if
	endif                                        !------------IGEN=istart LOOP ENDS------------------

     
     !Master performs niche, selectn, crosovr, ...
     !==================== GA TASKS ===================================
     if (igen .le. maxgen+istart-1) then ! *** check for i
        
		!Enter selection, crossover and mutation loop.
		ncross = 0
		ipick = npopsiz
              
		!Perform selection & then perform crossover between the rand_xomly selected pair.
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
              
		call mutate  ! Modified F. Espinoza (4/9/03)
              
		!  Write child array back into parent array for new generation.  
		!  Check to see if the best parent was replicated.                    
		! ***************** NEW POPULATION CREATION (MU+LAMBDA OR TOURNAMENT SELECTION) **********************
           
		if(itourny.eq.1) then
			call newgen(npossum,ig2sum)
              
		else
			do n=1,2*npopsiz
				index(n)=n
			enddo
			do n=1,npopsiz
				ftemp(n)=fitness(1,n)
				tcost(n)=cost(n) ! Added by F. Espinoza (9/4/03)
				tpen1(n)=pen1(n) ! Added by F. Espinoza (9/4/03)
				tpen2(n)=pen2(n) ! Added by F. Espinoza (9/4/03)
				tpen3(n)=pen3(n) ! Added by F. Espinoza (9/4/03)
				tpen4(n)=pen4(n) ! Added by F. Espinoza (9/4/03)
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

			write(124,1112) igen  
			!write(552,1112) igen

			call sGA_evalout(npopsiz, temp_fine_flag)
			call cpu_time(t3)
			write(*,1115) igen, t3-t1
			write(124,1115) igen, t3-t1
												    
			do j = 1, npopsiz
				write(124, 1113) j, temp_fine_flag(j),fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
				& (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
				& (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
				& drycell(1,j),drycell(2,j)
			end do

			! SORTING CHILDREN
			! code 1
			! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
			! multiscale 2 strategy update of fine_flag
			! identify best individuals and age of fine individuals
			if (multiscale2) then     !------------MULTISCALE2 LOOP------------------
                   do n=1,npopsiz
                      index(n) = n
                      tfit1(n) = fitness(1,n)
                      tcost1(n) = cost(n)
                      tpen11(n) = pen1(n)
                      tpen12(n) = pen2(n)
                      tpen13(n) = pen3(n)
                      tpen14(n) = pen4(n)
                      tdry(1,n) = drycell(1,n)
                      tdry(2,n) = drycell(2,n)
                      temp_flag(n) = temp_fine_flag(n)
                      do j=1,nchrome
                         item1(n,j)=iparent(n,j)
                      enddo
                   enddo

                   call bubblesort1(npopsiz,tfit1,index)

                   do n = 1, npopsiz
                      fitness(1,n) = tfit1(n)
                      cost(n)=tcost1(index(n)) 
                      pen1(n)=tpen11(index(n)) 
                      pen2(n)=tpen12(index(n)) 
                      pen3(n)=tpen13(index(n)) 
                      pen4(n)=tpen14(index(n))
                      drycell(1,n) = tdry(1,index(n))
                      drycell(2,n) = tdry(2,index(n))
                      temp_fine_flag(n) = temp_flag(index(n))
                      do j=1,nchrome
                         iparent(n,j)=item1(index(n),j)
                      end do
                   end do

                   ! update individual fine_flags
                   nk = 1
                   n1 = 1
                   if (igen .eq. 1 .and. feed .eq. .true.) then  ! igen equal to 1 loop
					  write (*,*) 'check'
                      ! Added by Eva Sinha
                      ! if feed then individual=int(best_frac*npopsiz) is read from the feed file
                      do while ((n1 <= ( int(best_frac*npopsiz) -1 ) )  .and. (nk <= npopsiz))
                         if (temp_fine_flag(nk) == 1) then
                            temp_fine_flag(nk) = 2
                            fine_age(nk) = 0
                            n1 = n1+1
                         else 
                            fine_age(nk) = 1
                         end if
                         nk = nk+1
                      end do
                      call ran8(1,rand_x)
                      ind = int(best_frac*npopsiz)
                      fine_age(ind) = 0
                      temp_fine_flag(ind) = 2
                      read(557,*) k,f0,(feedchar(j),j=1,nchrome)
                      do j=1,nchrmax
                         iparent(ind,j)=0
                      enddo
                      do j=1,nchrome
                         iparent(ind,j)=feedchar(j)
                      enddo
                      close(557)
                      fitness(1,ind)=f0

                   else
                      do while ((n1 <= int(best_frac*npopsiz))  .and. (nk <= npopsiz))
                         if (temp_fine_flag(nk) == 1) then
                            temp_fine_flag(nk) = 2
                            fine_age(nk) = 0
                            n1 = n1+1
                         else 
                            fine_age(nk) = 1
                         end if
                         nk = nk+1
                      end do
                   end if			! End of igen equal to 1 loop
                end if				!------------END OF MULTISCALE2 LOOP------------------


                ! Storing Children in ftemp, itemp arrays.
                do n=1,npopsiz
                   ftemp(n+npopsiz)=fitness(1,n)
                   tcost(n+npopsiz)=cost(n) 
                   tpen1(n+npopsiz)=pen1(n)
                   tpen2(n+npopsiz)=pen2(n) 
                   tpen3(n+npopsiz)=pen3(n)
                   tpen4(n+npopsiz)=pen4(n)
                   tdrycell(1,n+npopsiz) = drycell(1,n)
                   tdrycell(2,n+npopsiz) = drycell(2,n)
                   fine_flag(n+npopsiz) = temp_fine_flag(n)
                   do j=1,nchrome
                      itemp(n+npopsiz,j)=iparent(n,j)
                   enddo
                enddo

             ! code 3
             ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
             ! Evalating best children on fine
             if (multiscale2) then   !------------MULTISCALE2 LOOP------------------
                call sGA_evalout(int(best_frac*npopsiz), temp_fine_flag)

				call cpu_time(t4)
                write(124,1115) igen, t4 - t1
                write(*,1115) igen, t4 - t1
                do j = 1,int(best_frac*npopsiz)
                      write(124, 1113) j,temp_fine_flag(j), fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
                           & (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
                           & (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
                           & drycell(1,j),drycell(2,j)
                      write(*, 1113) j,temp_fine_flag(j), fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
                           & (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
                           & (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
                           & drycell(1,j),drycell(2,j)
                end do
                   ! updating fine children
                   do n=1,int(best_frac*npopsiz)
                      ftemp(n+npopsiz)=fitness(1,n)
                      tcost(n+npopsiz)=cost(n) 
                      tpen1(n+npopsiz)=pen1(n) 
                      tpen2(n+npopsiz)=pen2(n) 
                      tpen3(n+npopsiz)=pen3(n)
                      tpen4(n+npopsiz)=pen4(n)
                      tdrycell(1,n+npopsiz) = drycell(1,n)
                      tdrycell(2,n+npopsiz) = drycell(2,n)
                   end do

                   ! finding and updating same individuals for fine parents in original parents+children
                   do nk = 1, int(best_frac*npopsiz)
                      do n = 1, 2*npopsiz
                         ! if (n .ne. nk+npopsiz) then
                         n1 = 0
                         do j=1,nchrome
                            if (itemp(n,j) .eq. iparent(npopsiz+nk,j)) then
                               n1=n1+1
                            end if
                         end do
                         if (n1 == nchrome) then
                            ftemp(n) = fitness(1,npopsiz+nk)
                            tcost(n) = cost(npopsiz+nk)
                            tpen1(n) = pen1(npopsiz+nk)
                            tpen2(n) = pen2(npopsiz+nk)
                            tpen3(n) = pen3(npopsiz+nk)
                            tpen4(n) = pen4(npopsiz+nk)
                            tdrycell(1,n) = drycell(1,npopsiz+nk)
                            tdrycell(2,n) = drycell(2,npopsiz+nk)
                            fine_flag(n) = 2
                            fine_age(n) = 1
                         end if
                         ! end if
                      end do
                   end do
                ! selective mu + lambda for multiscale2  

                   ! Number of fines in the entire parent + child population
                   counter = 0
                   do n = 1, 2*npopsiz
                      if (fine_flag(n) .eq. 2) then
                         counter = counter +1
                      end if
                   end do

		  
                   ! number of fine and coarse individuals to be picked up from population
                   numfine = int(best_selec_frac * counter)
				   if (numfine .gt. npopsiz) then !Added by Eva Sinha
						numfine = npopsiz
				   end if
                   numcoarse = npopsiz - numfine

                   ! check for repetition of number of fines selected every generation. 
                   ! This check occurs every generation and checks for repetition for the past 5 generations
                   fine_gen(igen) = numfine
                   if (igen .gt. 5) then
                      counter = 0
                      do n = 1, igen-1
                         if (fine_gen(n) .eq. fine_gen(igen)) then
                            counter = counter + 1
                         end if
                      end do
                      if (counter .ge. 5) then
                         best_selec_frac = best_selec_frac + 0.25
                         if (best_selec_frac .gt. 1.0d0) then
                            best_selec_frac= 1.0d0
                         end if
                      end if
                   end if

                   ! Sorting final combined population
                   do n=1,2*npopsiz
                      index(n)=n
                   enddo
                   call bubblesort1(2*npopsiz,ftemp,index)

                   ! collecting all coarse individuals
                   numc = 0
                   tfine_flag(:) = 0
                   tfine_age(:) = 0
                   if (numcoarse .gt. 0) then   !Added by Eva Sinha
                      do n=1,2*npopsiz
                         if (fine_flag(index(n)) == 1) then 
                            numc = numc + 1
                            tfine_flag(numc)= fine_flag(index(n))
                            tfine_age(numc) = 0
                            fitness(1,numc) = ftemp(n)
                            cost(numc) = tcost(index(n)) 
                            pen1(numc) = tpen1(index(n)) 
                            pen2(numc) = tpen2(index(n)) 
                            pen3(numc) = tpen3(index(n))
                            pen4(numc) = tpen4(index(n))
                            drycell(1,numc) = tdrycell(1,index(n))
                            drycell(2,numc) = tdrycell(2,index(n))
                            do j=1,nchrome
                               iparent(numc,j) = itemp(index(n),j)
                            end do
                         end if
                         if (numc .ge. numcoarse) then
                            goto 1110
                         end if
                      enddo
                   end if

1110               numf = 0
                   if (numfine .gt. 0) then     ! Added by Eva Sinha
                      do n=1,2*npopsiz
                         if (fine_flag(index(n)) == 2) then 
                            numf = numf + 1
                            tfine_flag(numc+numf) = fine_flag(index(n))
                            tfine_age(numc+numf) = 1
                            fitness(1,numc+numf) = ftemp(n)
                            cost(numc+numf) = tcost(index(n)) 
                            pen1(numc+numf) = tpen1(index(n)) 
                            pen2(numc+numf) = tpen2(index(n)) 
                            pen3(numc+numf) = tpen3(index(n))
                            pen4(numc+numf) = tpen4(index(n))
                            drycell(1,numc+numf) = tdrycell(1,index(n))
                            drycell(2,numc+numf) = tdrycell(2,index(n))
                            do j=1,nchrome
                               iparent(numc+numf,j)=itemp(index(n),j)
                            end do
                         end if
                         if (numf .ge. numfine) then
                            goto 1118
                         end if
                      enddo
                   end if

1118               do n = 1, npopsiz
                      call decode(n, parent, iparent)
                   end do
                   fine_flag(:) = 1
                   fine_age(:) = 0
                   do n = 1, npopsiz
                      fine_flag(n) = tfine_flag(n)
                      fine_age(n) = tfine_age(n)
                   end do


                else       !----------------- if not multiscale2 -------------------------

                 do n=1,npopsiz
                    ftemp(n+npopsiz)=fitness(1,n)
                    tcost(n+npopsiz)=cost(n) ! Added by F. Espinoza (9/4/03)
                    tpen1(n+npopsiz)=pen1(n) ! Added by F. Espinoza (9/4/03)
                    tpen2(n+npopsiz)=pen2(n) ! Added by F. Espinoza (9/4/03)
                    tpen3(n+npopsiz)=pen3(n) ! Added by F. Espinoza (9/4/03)
					tpen4(n+npopsiz)=pen4(n) ! Added by F. Espinoza (9/4/03)
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
					pen4(n)=tpen4(index(n)) ! Added by F. Espinoza (9/4/03)
                    drycell(1,n) = tdrycell(1,index(n))
                    drycell(2,n) = tdrycell(2,index(n))
                    do j=1,nchrome
                       iparent(n,j)=itemp(index(n),j)
                    end do
					call decode(n,parent,iparent)
                 enddo
			end if       !------------END OF MULTISCALE2 LOOP------------------                 
                 ! writing in the output file

				call cpu_time(t3)
				write(552,1115) igen, t3 - t1
				write(6,1115) igen, t3 - t1

                 do j = 1, npopsiz
		                 write(552, 1113) j, fine_flag(j),fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
			  & (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			  & (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
			  & drycell(1,j),drycell(2,j)
		                 write(*, 1113) j, fine_flag(j),fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
			  & (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			  & (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
			  & drycell(1,j),drycell(2,j)
            end do
                !writing the best individual on fine scale in .cr0 file
                !j is the location of  best fitness on fine scale
                if (multiscale2) then
                   if (numfine < npopsiz ) then
                      j = npopsiz - numfine + 1 
                   else
                      j = 1
                   end if
                   write(555, 1117) (t4 - t1), igen, fine_flag(j), fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
                        & (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
                        & (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
                        & drycell(1,j),drycell(2,j)
                else
                   ! Added by F. Espinoza		
                   fev=fev+npopsiz
                   call BestValue(best)
                   call Avg(av0,std,nbest,best)
                   !write(74,2223) igen,av0,std,std/av0,best,nbest,fev,feve
                   write(6,*)
                   write(6,*)
                   !write(6,2223) igen,av0,std,std/av0,best,nbest,fev,feve
                   write(6,*)
                   ! End added
                end if                
                
                 
			end if       
           ! ********************************* NEW POPULATION CREATION END ************************************
           
           
           
              !  Implement micro-GA if enabled.
              !          if (microga) call gamicro(i,npossum,ig2sum)
              !   Write to restart file
              call restart(igen, istart,kount)

             call BestValue(best)
             call Avg(av0,std,nbest,best)
             call stats(fitness, mean, variance, converge, fine_flag)
             write (74,2224) igen, t3 - time_pre, t4 - t3, av0, std, std/av0, best, nbest
             write(*,*)
             write (*,2224) igen, t3 - time_pre, t4 - t3, av0, std, std/av0, best, nbest
             write(*,*)              ! ------------------------------------------------

             time_pre = t4

2224         format(i4,2x,f20.3,2x,f20.3,2x,f18.8,2x,f25.12,2x,f18.12,2x,f15.2,2x,i6)
              !      added for grid noise ### meghna jan 15, 2002 ###
              !      subroutine 'stats' calculates mean and variance of population in a generation.
              !      var_ actor ; defined in the module fitfunc_input
              !      grid_var ; defined in the module fitfunc_input
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
              
              !---------------------------------------------------------------------------
           
        END IF ! ga tasks end if
        
        ! ======================  GA TASKS ============================================
        
        !  convergence check -----------------------------------
        !  print *,'end is ',end, 'at igen', i
        if (runend .eq. 1111) then
           go to 9988
        end if
        ! ------------------------------------------------------
        
        
     end do ! GENERATION END DO LOOP +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
9988 bestj=minloc(fitness(1,1:npopsiz))
     write(555,556) igen,fitness(1,bestj(1)),(iparent(bestj(1),j),j=1,nchrome)
556  format(i4,2x,f15.5,2x,200i2)
     
     
99999     runend = 1001
     !print *,'PROGRAM ENDED'
     
1050 format(1x,' #      Binary Code',16x,'Param1  Param2  Fitness')
1111 format(//'#################  Generation',i5,' #################')
1225 format(/'  Number of Crossovers      =',i5)
          
     return
   end subroutine sga

!###################################################################
subroutine hga
!Main processing loop for the simple genetic algorithm
       
  IMPLICIT NONE
  save
  integer :: n, runend,j,p, nnchild,igen
  integer :: ni,nj,mm,gen,nBald,nind,fev,feve,nstop,ils,nbest,ifin,geneq,npop
  integer :: itemp(2*indmax,nchrmax),bestj(1),idum3,Succ(maxlsind),ls(npopsiz),lb(maxlsind),lsind(maxlsind)
     
  double precision :: ff(indmax), rtime(2),CV(maxgen),Avgval(maxgen)
  double precision :: best,av0,std,dif,DeltaSGA,CVR,bestdif
  double precision ::t1, t2, totaltime, converge,bestf(maxgen+1)	

  
  ! Added by Felipe Espinoza (4/9/03)

  integer :: feedchar(nchrmax),k, ind
  double precision :: f0

1112 format(/'*******  Local Search after Generation',i5,'  ********')
1115 format(/'#################  Generation',i5,'  #################')
1113 format(i5,2x,6(f20.10,2x), 7i4,2x,4(f5.3,2x),7i2,3x,4(f5.3,2x),7i2,2x,i5,2x,i5)
1116 format(7f5.0,2x,14f3.0)
  ! End added
  kount = 0
  execval = 0
  converge = 0.d0
  runend = 0
  
  fev=0
  feve=0

  do igen = istart,maxgen+istart-1  ! GENERATIONS LOOPS

     ! felipe changes......====================================== 26 feb 2002
     ! evaluating fitnesses for generation zero.
     if(igen.eq.1) then                            !---------IGEN=1 LOOP BEGINS--------------

     write(124,1115) 0
  !   write(*,1115) 0
	 write(6,1115) 0

        !print *, "**************** 1st call *********************"


           ! Added by Felipe Espinoza (4/9/03)
           FEED=.FALSE.
           if(feed) then
		      idum3=-45215
              call ran1(idum3,rand_x)
              ind = 1 + dint(rand_x*(dble(npopsiz-1)))
			  ind=1
              read(557,*) k,f0
			  print *,k,f0
			  read(557,*) (feedchar(j),j=1,nchrome)
              iparent(ind,:)=0
              do j=1,nchrome
                 iparent(ind,j)=feedchar(j)
              enddo
              close(557)
              fitness(1,ind)=f0
           endif

           !call sGA_evalout(0)

           do j = 1,npopsiz
		   	  write(124, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
			  & (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			  & (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
			  & drycell(1,j),drycell(2,j)
           end do

           fev=fev+npopsiz
           call BestValue(best)
           call Avg(av0,std,nbest,best)
		   write(6,*)
		   write(6,*)
           write(74,2223) 0,av0,std,std/av0,best,nbest,fev,feve
           write(6,2223) 0,av0,std,std/av0,best,nbest,fev,feve
		   write(6,*)
		   CV(igen)=std/av0
           Avgval(igen)=av0

2223    format(i4,2x,f18.8,2x,f25.12,2x,f18.12,2x,f15.2,2x,i6,2x,i10,2x,i10)

        ! End Felipe
     endif                                        !------------IGEN=1 LOOP ENDS------------------

     if(igen.eq.istart.and.irestrt) then                            !---------IGEN=1 LOOP BEGINS--------------
        !print *, "**************** 1st call *********************"

     write(124,1115) istart
  !   write(*,1115) 0
	 write(6,1115) istart

       ! call sGA_evalout(istart)

           do j = 1, npopsiz
		   	  write(124, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
			  & (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			  & (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
			  & drycell(1,j),drycell(2,j)
           end do
     endif                                        !------------IGEN=istart LOOP ENDS------------------


     
     !Master performs niche, selectn, crosovr, ...
     !==================== GA TASKS ===================================
     if (igen .le. maxgen+istart-1) then ! *** check for i

! EVA: I COMMENTED OUT THIS PART OF THE CODE TO CREATE THE SGA CODE
! PLEASE CHECK THAT EVERYTHING IS OK BY COMPARING WITH SGA-D1
!
!
!		   ils=0     
!           if(igen.ge.3) then
!              CVR=CV(igen-1)/CV(igen-2)
!              bestdif=dabs(bestf(igen-1)-Avgval(igen-1))/Avgval(igen-1)
!              if(CVR.ge.LGfac.and.bestdif.gt.difcrit) then
!                 DeltaSGA=dabs(Avgval(igen-2)-Avgval(igen-1))
!                 ils=1	
!                 write(6,1112) igen-1
!              endif
!           endif
!           if(ils.eq.1) then
!              call InitEvolution(nind,lb,lsind)
!              call SAHGA(fev,feve,nind,lsind,Succ,DeltaSGA,bestf(igen-1))
!              call Baldwin(nind,lsind,Succ,lb)   
!              call BestValue(best)
!              call Avg(av0,std,nbest,best)
!              write(6,*)
!              write(6,2223) igen-1,av0,std,std/av0,best,nbest,fev,feve
!              write(6,*)
!           endif

           write (6,1111) igen  
              !       Enter selection, crossover and mutation loop.
              ncross = 0
              ipick = npopsiz
              
              !Perform selection & then perform crossover between the rand_xomly selected pair.
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
              
              call mutate  ! Modified F. Espinoza (4/9/03)
              
             !  Write child array back into parent array for new generation.  
              !  Check to see if the best parent was replicated.
              
           
           ! ***************** NEW POPULATION CREATION (MU+LAMBDA OR TOURNAMENT SELECTION) **********************
           
           if(itourny.eq.1) then
              call newgen(npossum,ig2sum)
              
           else
                 do n=1,2*npopsiz
                    index(n)=n
                 enddo
                 do n=1,npopsiz
                    ftemp(n)=fitness(1,n)
                    tcost(n)=cost(n) ! Added by F. Espinoza (9/4/03)
                    tpen1(n)=pen1(n) ! Added by F. Espinoza (9/4/03)
                    tpen2(n)=pen2(n) ! Added by F. Espinoza (9/4/03)
                    tpen3(n)=pen3(n) ! Added by F. Espinoza (9/4/03)
					tpen4(n)=pen4(n) ! Added by F. Espinoza (9/4/03)
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

                 write(124,1115) igen  
                 write(552,1115) igen
    
              !call sGA_evalout(igen)  
                 do j = 1, npopsiz
		                 write(124, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
			  & (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			  & (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
			  & drycell(1,j),drycell(2,j)
             end do
                 do n=1,npopsiz
                    ftemp(n+npopsiz)=fitness(1,n)
                    tcost(n+npopsiz)=cost(n) ! Added by F. Espinoza (9/4/03)
                    tpen1(n+npopsiz)=pen1(n) ! Added by F. Espinoza (9/4/03)
                    tpen2(n+npopsiz)=pen2(n) ! Added by F. Espinoza (9/4/03)
                    tpen3(n+npopsiz)=pen3(n) ! Added by F. Espinoza (9/4/03)
					tpen4(n+npopsiz)=pen4(n) ! Added by F. Espinoza (9/4/03)
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
					pen4(n)=tpen4(index(n)) ! Added by F. Espinoza (9/4/03)
                    drycell(1,n) = tdrycell(1,index(n))
                    drycell(2,n) = tdrycell(2,index(n))
                    do j=1,nchrome
                       iparent(n,j)=itemp(index(n),j)
                    end do
					call decode(n,parent,iparent)
                 enddo
                 
                 ! writing in the output file

                 do j = 1, npopsiz
		                 write(552, 1113) j, fitness(1,j),cost(j),pen1(j),pen2(j),pen3(j),pen4(j),(nint(parent(j,k)), k = 1, 7),&
			  & (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			  & (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29),&
			  & drycell(1,j),drycell(2,j)
                 end do
                 
                 ! Added by F. Espinoza
                 
                 fev=fev+npopsiz
                 call BestValue(best)
                 bestf(igen)=best
			     bestf1=best
				 ff(:)=fitness(1,:)
				 bestj=minloc(ff)
                 call Avg(av0,std,nbest,best)
		         CV(igen)=std/av0
                 Avgval(igen)=av0
                 write(74,2223) igen,av0,std,std/av0,best,nbest,fev,feve
				 write(6,*)
                 write(6,*)
                 write(6,2223) igen,av0,std,std/av0,best,nbest,fev,feve
                 write(6,*)
                 ! End added
                 
                 
           endif
           ! ********************************* NEW POPULATION CREATION END ************************************
           
           
           
              !  Implement micro-GA if enabled.
              !          if (microga) call gamicro(i,npossum,ig2sum)
              !   Write to restart file
              call restart(igen, istart,kount)
              
              ! ------------------------------------------------
              !      added for grid noise ### meghna jan 15, 2002 ###
              !      subroutine 'stats' calculates mean and variance of population in a generation.
              !      var_ actor ; defined in the module fitfunc_input
              !      grid_var ; defined in the module fitfunc_input
              !call stats(fitness, mean, variance, converge)
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
              
              !---------------------------------------------------------------------------
           
        END IF ! ga tasks end if
        
        ! ======================  GA TASKS ============================================
        
        !  convergence check -----------------------------------
        !  print *,'end is ',end, 'at igen', i
        if (runend .eq. 1111) then
           go to 9988
        end if
        ! ------------------------------------------------------
        
        
     end do ! GENERATION END DO LOOP +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
9988 bestj=minloc(fitness(1,1:npopsiz))
     write(555,556) igen,fitness(1,bestj(1)),(iparent(bestj(1),j),j=1,nchrome)
556  format(i4,2x,f15.5,2x,200i2)
     
     
99999     runend = 1001
     !print *,'PROGRAM ENDED'
     
1050 format(1x,' #      Binary Code',16x,'Param1  Param2  Fitness')
1111 format(//'#################  Generation',i5,' #################')
1225 format(/'  Number of Crossovers      =',i5)
     
     
     return
   end subroutine hga
   
!#######################################################################
subroutine sGA_evalout(npop, temp_fine_flag)
use condorio
    !
    !  This subroutine evaluates the population, assigns fitness, 
    !  establishes the best individual, and outputs information. 
    IMPLICIT NONE
    save
    integer, intent(in) :: temp_fine_flag(indmax) , npop  
    double precision :: paramsm(nparam),paramav(nparam)
    double precision :: fitsum,fbar,funcval,best
    double precision :: par(nparam),t1,t2,Th,Tc,MB1,MB2,pw
    !****************    
    integer :: j,k,i,l,m,f,g,p,remaining, z,nc
	integer :: arrJobIds( npopsiz )
	integer :: nJobCount

    
    best = huge(best)
    fitsum = 0.d0
    paramsm(1:nparam) = 0.0d0
    num_elite = 0
	MB1= 0.0d0
	MB2 = 0.0d0

1116      format(i4,2x,7i5,2x,7i3,3x,7i3)

		nJobCount = 0
        do z = 1,npop

		     call cpu_time(t1) 

             call decode(z,parent,iparent)

             if (multiscale2) then
                gridflagx = temp_fine_flag(z)
                gridflagy = temp_fine_flag(z)
                call remedwell(gridflagx, gridflagy)
                call parinit
             end if
			 	         
             call prepfunc(z,MB1,MB2)

             !fitfess function for the coarse grid

			!broadcast the individual fitness evaluations.
			arrJobIds( nJobCount+1 ) = z
			nJobCount = nJobCount + 1
			call BroadcastSlave(z, npop+1, gridflagx, gridflagy )
		enddo

		!route the jobs
		if( nJobCount>0 )then 
			call WaitForSlaves( arrJobIds, nJobCount )
		endif

		!collect the slavles
        do z = 1,npop

             call decode(z,parent,iparent)
             if (multiscale2) then
                gridflagx = temp_fine_flag(z)
                gridflagy = temp_fine_flag(z)
                call remedwell(gridflagx, gridflagy)
                call parinit
             end if
             call prepfunc(z,MB1,MB2)

			call CollectSlave( z, npop+1, Fitness1, Total_cost,Pen_Rdx,Pen_Tnt, drycell_c, drycell_h)

!             if ((gridflagx.eq.1) .and. (gridflagy.eq.1))then
!                call Obj_Func_Coarse(Fitness1,Total_cost,Pen_Rdx,Pen_Tnt,gridflagx, gridflagy,drycell_c, drycell_h)
!             endif
             
             !fitfess function for the fine grid
!             if ((gridflagx.eq.2) .and. (gridflagy.eq.2))then
!                call Obj_Func_Fine(Fitness1,Total_cost,Pen_Rdx,Pen_Tnt, gridflagx, gridflagy,drycell_c, drycell_h)
!             endif
             
             fitness(1,z) = Fitness1+MB1+MB2
             cost(z) = Total_cost
             pen1(z) = Pen_Rdx
             pen2(z) = Pen_Tnt
			 pen3(z) = MB1
             pen4(z) = MB2
             drycell(1,z) = drycell_h
             drycell(2,z) = drycell_c

             fitsum=fitsum+fitness(1,z)
		     call cpu_time(t2)

            !write(6,111) ig,z,t2-t1,Fitness1+MB1+MB2,Total_cost,Pen_Rdx,Pen_Tnt,MB1,MB2,drycell_h,drycell_c
			 111 format(i4,2x,i5,2x,(f7.2,2x),6(f12.4,2x),2(i6,2x))

             !write(124, 1113) z, fitness(1,z),cost(z),pen1(z),pen2(z),pen3(z),pen4(z),(nint(parent(z,k)), k = 1, 7),&
			 ! & (parent(z,k), k = 8, 11),(nint(parent(z,k)), k = 12, 18),&
			 ! & (parent(z,k), k = 19, 22),(nint(parent(z,k)), k = 23, 29),&
			 ! & drycell(1,z),drycell(2,z)

1113 format(i5,2x,6(f20.10,2x), 7i4,2x,4(f6.4,2x),7i2,3x,4(f6.4,2x),7i2,2x,i5,2x,i5)

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

	call REFRESHHOSTS
    return

  end subroutine sGA_evalout

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
!Stoch. rem. selectn. rand_xomly draws individuals from mating pop.
        call ran1(1,rand_x)
        ind1 = 1 + dint(rand_x*(dble(num_mate_pop)-1))
        mate1 = mate_pop(ind1)
        call ran1(1,rand_x)
        ind2 = 1 + dnint(rand_x*(dble(num_mate_pop)-1))
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
  !  Modified for multiscale2 by Eva Sinha

  subroutine tswr(mate,ipick)

    IMPLICIT none
    save

    integer :: i,n,nj,ind(20),bestloc(1),mate,ipick,tmpvar

    double precision :: ttfitness(tsize), tmp1
    integer	:: ttfine_flag(tsize),tmp2

    if (multiscale2) then
       do i=1,tsize
          call ran8(1,rand_x)
			ind(i)=1+rand_x*(npopsiz-1)
          ttfitness(i)=fitness(1,ind(i))
          ttfine_flag(i) = fine_flag(ind(i))
       enddo
       ! sorting tournaments
       do n=1,tsize
          do nj=1,tsize-n
             if(ttfitness(nj+1) < ttfitness(nj)) then
                tmp1=ttfitness(nj)
                ttfitness(nj)=ttfitness(nj+1)
                ttfitness(nj+1)=tmp1

                tmp2=ttfine_flag(nj)
                ttfine_flag(nj)=ttfine_flag(nj+1)
                ttfine_flag(nj+1)=tmp2

                tmp2=ind(nj)
                ind(nj)=ind(nj+1)
                ind(nj+1)=tmp2
             end if
          end do
       end do

       ! picking up the mate
       mate=ind(1)
       do n=1,tsize
          if (ttfine_flag(n) == 2) then
             mate = ind(n)
             goto 1012
          end if
       end do
1012   tmpvar = 1

    else   !------------if not multiscale2 ------------------
       do i=1,tsize
          call ran8(1,rand_x)
			ind(i)=1+rand_x*(npopsiz-1)
          ttfitness(i)=fitness(1,ind(i))
       enddo
       bestloc=minloc(ttfitness)
       mate=ind(bestloc(1))
    end if

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
5       call ran1(1,rand_x)
        indiv=nint(1+(npop-1)*rand_x)
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
  subroutine Avg(av0,std,nbest,best)
    ! Modified By Eva Sinha for Multiscale_II
    IMPLICIT none
    save

    double precision :: av0,std,best,dif,tol

    integer :: nbest,mm, numf

    nbest=0
    av0=0.d0
    std=0.d0

    numf = 0
    if (multiscale2) then
       do mm=1,npopsiz
          if (fine_flag(mm) .eq. 2) then
             numf = numf + 1
             if(dabs(best).lt.1d-3) then
                dif=dabs(best-fitness(1,mm))
                tol=tol1
             else
                dif=dabs(fitness(1,mm)/best-1.0d0)
                tol=tol2
             endif
             if(dif.lt.tol) nbest=nbest+1
             av0=av0+fitness(1,mm)
          end if
       enddo
       av0=av0/dble(numf)
    else
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
    end if

    numf = 0
    if (multiscale2) then
       do mm=1,npopsiz
          if (fine_flag(mm) .eq. 2) then
             numf = numf + 1
             std=std+(fitness(1,mm)-av0)**2.0d0
          end if
       enddo
       std=(std/dble(numf-1))**0.5d0
    else
       do mm=1,npopsiz
          std=std+(fitness(1,mm)-av0)**2.0d0
       enddo
       std=(std/dble(npopsiz-1))**0.5d0
    end if

    return

  end subroutine Avg


!#######################################################################
subroutine crosovr(ncross,j,mate1,mate2)
    !
    !  Subroutine for crossover between the rand_xomly selected pair.
    IMPLICIT NONE
    save
    !
    integer j,ncross,icross,n,mate1,mate2, idum3
    !
    idum3=1
    if (.not. iunifrm) then
       !  Single-point crossover at a rand_xom chromosome point.
       call ran1(idum3,rand_x)
       if(rand_x.gt.pcross) goto 69
       ncross=ncross+1
       call ran1(idum3,rand_x)
       icross=2+dint(dble(nchrome-1)*rand_x)
       do n=icross,nchrome
          ichild(j,n)=iparent(mate2,n)
          if(nchild.eq.2) ichild(j+1,n)=iparent(mate1,n)
       end do
    else
       !  Perform uniform crossover between the rand_xomly selected pair.
       do n=1,nchrome
          call ran1(idum3,rand_x)
          if(rand_x.le.pcross) then
             ncross=ncross+1
             ichild(j,n)=iparent(mate2,n)
             if(nchild.eq.2) ichild(j+1,n)=iparent(mate1,n)
          endif
       end do
   end if
69 continue
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
    !  Perform rand_xom jump mutation if a rand_xom number is less than pmutate.
    !  Perform rand_xom creep mutation if a different rand_xom number is less
    !  than pcreep.  
    
    idum3=1
    nmutate=0
    ncreep=0
    do j=1,npopsiz
       do k=1,nchrome
          !  Jump mutation
          call ran1(idum3,rand_x)
          if (rand_x.le.pmutate) then
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
             call ran1(idum3,rand_x)
             if(rand_x.le.pcreep) then
                call decode(j,child,ichild)
                ncreep=ncreep+1
                creep=1.0
                call ran1(idum3,rand_x)  
                if (rand_x.lt.0.5) creep=-1.0
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
    !  then reproduce the best parent into a rand_xom slot.
    !
    IMPLICIT NONE
    save
    !
    integer :: npossum,ig2sum,jelite,n,irand_x,k,worst(1),idum3
    double precision ::  best
    double precision :: ff(npopsiz),f1,bestf0

    !
    ! Elitist reproduction for SGA
  
    idum3=1
    bestf0=bestf1

    if(maxobj.eq.1) then
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
      
        ! all processors go into sga_evalout ------------   
       ! call SGA_evalout(-999)
        ! -----------------------------------------------
     
          ff(:)=fitness(1,:)
          worst=maxloc(ff)
          f1=maxval(ff)
          
          if (ielite .and. kelite .eq. 0) then
         
            call ran1(idum3,rand_x)
            irand_x=1+anint(dble(npopsiz-1)*rand_x)
            iparent(irand_x,1:nchrome) = ibest(1,1:nchrome)
            fitness(1,worst(1))=bestf0
          endif

      else
  
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
              call ran1(idum3,rand_x)
              irand_x=1+dint(dble(npopsiz)*rand_x)
              iparent(irand_x,1:nchrome)=ibest(k,1:nchrome)
            end if
          end do
    
      end if      
1260  format('  Elitist Reproduction on Individual in next generation ',i4)
1266  format('  Invidual',i4,'in the current generation has index',i4,'in next generation')
    !
      return
  end subroutine newgen

!#######################################################################

  subroutine possibl(array,iarray)
    !
    !  This subroutine determines whether or not all parameters are within
    !  the specified range of possibility.  If not, the parameter is
    !  rand_xomly reassigned within the range.  This subroutine is only
    !  necessary when the number of possibilities per parameter is not
    !  optimized to be 2**n, i.e. if npossum < ig2sum.
    !
    IMPLICIT NONE
    save
    !
    integer i,j,idum3
    double precision :: array(indmax,nparmax)
    integer :: iarray(indmax,nchrmax)
    integer :: n2ig2j,irand_x
    !
    
    idum3=1
    do i=1,npopsiz
       call decode(i,array,iarray)
       do j=1,nparam
          n2ig2j=2**ig2(j)
          !            if(nposibl(j).ne.n2ig2j .and. array(i,j).gt.parmax(j) .and. &
          !                 & array(i,j).lt.parmin(j)) then
          if(nposibl(j).ne.n2ig2j .and. array(i,j).gt.parmax(j)) then
             call ran1(idum3,rand_x)
             irand_x=dint(dble(nposibl(j))*rand_x)
             array(i,j)=g0(j)+dble(irand_x)*g1(j)
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

! ##################################################################################################################################
  !Modified By Eva Sinha for Multiscale_II
  subroutine stats (fit, avg, var, conv, fine_flag) 

    IMPLICIT NONE
    save

    double precision, intent(inout):: fit(1,npopsiz), avg, var, conv
    integer, intent(inout) :: fine_flag(indmax)
    integer :: s,q, c, mm,j, numf, f_fine
    double precision :: best,dif,tol

    numf = 0
    avg = 0.d0

    if (multiscale2) then
       do s= 1, npopsiz
          if (fine_flag(s) .eq. 2) then
             numf = numf + 1
             avg= avg + fit(1,s)
          end if
       end do
       avg = avg/numf	

       numf = 0
       var=0.d0
       do s= 1, npopsiz
          if (fine_flag(s) .eq. 2) then
             numf = numf + 1
             var= var + ((fit(1,s)-avg)**2)
          end if
       end do
       var= var/(numf-1)

       f_fine = 1 !locating the first fine 
10     if (fine_flag(f_fine) .eq. 2) then
          best = fit(1,f_fine)
       else
          f_fine = f_fine + 1
          goto 10
       end if

       do j= f_fine+1,npopsiz
          if (fine_flag(j) .eq. 2) then
             if (fit(1,j).le. best) then
                best = fit(1,j)
             end if
          end if
       end do

       conv = 0.0d0
       numf = 0
       do mm=1,npopsiz
          if (fine_flag(mm) .eq. 2) then
             numf = numf + 1
             if(dabs(best).lt.1d-3) then
                dif=dabs(best-fitness(1,mm))
                tol=tol1
             else
                dif=dabs(fitness(1,mm)/best-1.0d0)
                tol=tol2
             endif
             if(dif.lt.tol) conv=conv+1.0d0
          end if
       enddo

       conv = conv/dble(numf)*(100.0d0)

    else       ! not multiscale2
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
    end if

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
		   write(66,*) temp(5), temp(5)/1.4238d-5
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
        !Set pumping rate possibilities for new wells
        do i=1,4
           j=j+1
           nposibl(j)=2**10
           parmin(j)=0.0d0
           parmax(j)=1.0d0
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
        !Set pumping rate possibilities for old wells
        do i=1,4
           j=j+1
           nposibl(j)=2**10
           parmin(j)=0.0d0
           parmax(j)=1.0d0
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
    		
100  format(100i4)
110  format(100f4.0)

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

     subroutine prepfunc(indiv,MB1,MB2)
          
     IMPLICIT none
     save

     integer :: ni,ns,nw,nwell,indiv,j,k,p,m,next,next1,next2,ninj,ninj1,ninj2,ncount,totwell,ncount0,ncount1, ncount2,np,ncc
     integer ns1, ns2, flag, ni1, ni2, nwell_1, nwell_2, new,nw1,nw2
     integer, dimension(nstress) :: NumNewBasin
     integer, dimension(newwells*nstress) :: ndrwx,ndrwy,ndrwz
     character(80) :: flname1, flname2
     double precision :: Qext,rand_x,alpha,DeltaQext,DeltaQinj,MB,QQext,Qinj1,Qinj2,MB1,MB2

         flname1 = 'uma4.res'
         flname2 = 'optdemo.wel'

!         call APPEND_PID(flname1)
!         call APPEND_PID(flname2)

         open(unit = 88, file=flname1,status='unknown') 
         open(unit = 13, file=flname2,status='old')
         rewind(13)
         read(13,*)       !ignoring the first line of the file
         rewind(88)    
! This is where the preparation for the fitness function takes place
! The "array" (or "parent") has a list of rem well locations (for each well), 
 
! if parent(1) and parent(2) are same change them

         do ns=1,nstress
            
10          ninj1=0
            next1=0
            ninj2=0
            next2=0
            Qext=0.d0
			Qinj1=0.0d0
			Qinj2=0.0d0
            
            do nw=1,newwells
               ni=nw+(2*newwells+8+oldwells)*(ns-1)
               nwell=int(anint(parent(indiv,ni)))
               if(pumpflagnew(nw,nwell).eq.2) then 
                  ninj1=ninj1+parent(indiv,ni+4+newwells)		! check whether or not the well is installed
                  Qinj1=Qinj1+parent(indiv,4+ni+newwells)*maxpumpnew(nw,nwell)		
               else
                  next1=next1+parent(indiv,ni+4+newwells)		! check whether or not the well is installed
                  Qext=Qext+parent(indiv,ni+newwells)*parent(indiv,4+ni+newwells)*maxpumpnew(nw,nwell)
			 write(66,*) "maxpumpnew,  Qext = ", maxpumpnew(nw,nwell), Qext
               endif
            enddo
        do nw=1,oldwells
           ni=nw+(2*newwells+oldwells)*(ns-1)
           if(pumpflagold(nw,1).eq.2) then 
              ninj2=ninj2+ncomp(nw)*parent(indiv,ni+8+2*newwells)
			  Qinj2=Qinj2+ncomp(nw)*parent(indiv,8+ni+2*newwells)*maxpumpold(nw,1)
           else
              next2=next2+parent(indiv,ni+8+2*newwells)
              Qext=Qext+parent(indiv,ni+4+2*newwells)*parent(indiv,8+ni+2*newwells)*maxpumpold(nw,1)
           endif
        enddo

        next=next1+next2
        ninj=ninj1+ninj2

        if(ninj.ne.0) then
           do k=1,5	! k  represents the number of component of the inhection basin k=1 means two wells, k=2 means four wells and k=3 means 6 wells
              alpha=Qext/dble(2*(k-1)*Qinj1+Qinj2)
              if(alpha.le.alphamax) goto 20   ! converts the Qmax to ft^3/yr)
           enddo
	       k=5
		   alpha=Qext/dble(2*(k-1)*Qinj1+Qinj2)
     20    totwell=2*(k-1)*ninj1+ninj2+next
    	   DeltaQinj=(alpha-alphamax)*(2*(k-1)*Qinj1+Qinj2)*1.4238d-5
 		   if(DeltaQinj .lt. 0.0d0) DeltaQinj=0.0d0
		 else
		   k=1
		   DeltaQinj=Qext*1.4238d-5
		   Totwell=next
		   alpha=0.d0
         endif

         nw1=nint(parent(indiv,1))*parent(indiv,newwells+5)
         nw2=nint(parent(indiv,2))*parent(indiv,newwells+6)

		 QQext=parent(indiv,newwells+1)*parent(indiv,newwells+5)*maxpumpnew(1,nint(parent(indiv,1)))+parent(indiv,newwells+2)*parent(indiv,newwells+6)*maxpumpnew(2,nint(parent(indiv,2)))

         DeltaQext=0.d0
		 
         if(nw1.eq.nw2) then
		    DeltaQext=QQext - maxpumpnew(1,nint(parent(indiv,1)))
			if(DeltaQext.lt.0.0) DeltaQext=0.d0
		 endif

		 DeltaQext = DeltaQext*1.4238d-5

		 MB1=DeltaQext+DeltaQinj

		 MB2=100.0*(Qext*1.4238d-5-1170.d0)

		 if(MB2.lt.0.d0) MB2=0.d0

		 MB=MB1+MB2

        write(88,4) totwell

        do nw=1,newwells
           ni=nw+(2*newwells+8+oldwells)*(ns-1)
           nwell=int(anint(parent(indiv,ni)))
           if(pumpflagnew(nw,nwell).eq.1.and.parent(indiv,4+ni+newwells).eq.1) then	! check whether it was an extraction well and that it has been installed 
              write(88,4) -namenew(nw)
              write(88,4) rwlcnew(nw,nwell,4)
              write(88,4) rwlcnew(nw,nwell,3)
              write(88,4) rwlcnew(nw,nwell,2)
              write(88,5) -parent(indiv,nw+newwells)*maxpumpnew(nw,nwell)
           endif
           if(pumpflagnew(nw,nwell).eq.2.and.parent(indiv,4+ni+newwells).eq.1) then	! check whether it was an injection well and that it has been installed
              if(k.eq.2) then	
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+1
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
              endif
              if(k.eq.3) then
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+1
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)
                 write(88,4) rwlcnew(nw,nwell,2)+1
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+1
                 write(88,4) rwlcnew(nw,nwell,2)+1
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
              endif
              if(k.eq.4) then
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+1
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)
                 write(88,4) rwlcnew(nw,nwell,2)+1
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+1
                 write(88,4) rwlcnew(nw,nwell,2)+1
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+2
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+2
                 write(88,4) rwlcnew(nw,nwell,2)+1
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
              endif
              if(k.eq.5) then
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+1
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)
                 write(88,4) rwlcnew(nw,nwell,2)+1
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+1
                 write(88,4) rwlcnew(nw,nwell,2)+1
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+2
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+2
                 write(88,4) rwlcnew(nw,nwell,2)+1
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+3
                 write(88,4) rwlcnew(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
                 write(88,4) namenew(nw)
                 write(88,4) rwlcnew(nw,nwell,4)
                 write(88,4) rwlcnew(nw,nwell,3)+3
                 write(88,4) rwlcnew(nw,nwell,2)+1
                 write(88,5) alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)			
              endif

           endif           
        enddo
 
        do nw=1,oldwells
           ni=nw+(2*newwells+oldwells)*(ns-1)
           if(pumpflagold(nw,1).eq.1.and.parent(indiv,8+ni+2*newwells).eq.1) then	! check whether it was an extraction well and that it has been installed
              write(88,4) -nameold(nw)
              write(88,4) rwlcold(nw,1,4)
              write(88,4) rwlcold(nw,1,3)
              write(88,4) rwlcold(nw,1,2)
              write(88,5) -parent(indiv,4+ni+2*newwells)*maxpumpold(nw,1)
           endif
           if(pumpflagold(nw,1).eq.2.and.parent(indiv,8+ni+2*newwells).eq.1) then		! check whether it was an injection well and that it has been installed
              do nwell=1,ncomp(nw)		
                 write(88,4) nameold(nw)
                 write(88,4) rwlcold(nw,nwell,4)
                 write(88,4) rwlcold(nw,nwell,3)
                 write(88,4) rwlcold(nw,nwell,2)
                 write(88,5) alpha*parent(indiv,8+ni+2*newwells)*maxpumpold(nw,1)
              enddo
           endif
        enddo

!       writing in the optdemo.wel file
4       FORMAT (I3)
5       FORMAT (F25.5)
6       FORMAT(I2,I4,I4,F25.5) ! new modflow files
        write(13,'(I3)') totwell
        do nw=1,newwells
           ni=nw+(2*newwells+oldwells)*(ns-1)
           nwell=int(anint(parent(indiv,ni)))
           if(pumpflagnew(nw,nwell).eq.1.and.parent(indiv,4+ni+newwells).eq.1) then	! check whether it was an extraction well and that it has been installed
              write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2), -parent(indiv,ni+newwells)*maxpumpnew(nw,nwell)
           endif
           if(pumpflagnew(nw,nwell).eq.2.and.parent(indiv,4+ni+newwells).eq.1) then	! check whether it was an injection well and that it has been installed
              if(k.eq.2) then
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
              endif
              if(k.eq.3) then
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2)+1, alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2)+1, alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
              endif
              if(k.eq.4) then
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2)+1, alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2)+1, alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+2, rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+2, rwlcnew(nw,nwell,2)+1, alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
              endif
              if(k.eq.5) then
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3), rwlcnew(nw,nwell,2)+1, alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+1, rwlcnew(nw,nwell,2)+1, alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+2, rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+2, rwlcnew(nw,nwell,2)+1, alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+3, rwlcnew(nw,nwell,2), alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
                 write(13,6) rwlcnew(nw,nwell,4), rwlcnew(nw,nwell,3)+3, rwlcnew(nw,nwell,2)+1, alpha*parent(indiv,nw+4+newwells)*maxpumpnew(nw,nwell)
              endif
           endif           
        enddo
 
        do nw=1,oldwells
           ni=nw+(2*newwells+oldwells)*(ns-1)
           if(pumpflagold(nw,1).eq.1.and.parent(indiv,8+ni+2*newwells).eq.1) then	! check whether it was an extraction well and that it has been installed
              write(13,6) rwlcold(nw,1,4), rwlcold(nw,1,3), rwlcold(nw,1,2), -parent(indiv,4+ni+2*newwells)*maxpumpold(nw,1)
           endif
           if(pumpflagold(nw,1).eq.2.and.parent(indiv,8+ni+2*newwells).eq.1) then		! check whether it was an injection well and that it has been installed
              do nwell=1,ncomp(nw)
                 write(13,6) rwlcold(nw,nwell,4), rwlcold(nw,nwell,3), rwlcold(nw,nwell,2),alpha*parent(indiv,8+ni+2*newwells)*maxpumpold(nw,1) 
              enddo
           endif
        enddo
!		writing in the optdemo file finishes

     enddo		!loop for stress period

!
!	 check is been made whether the parent was being changed in any of the stress period
!    and if so then the chromosome is re-coded
!


! -------------------------------------------------------------------------------------
!		Calculating the number of new basins in each stress period
   do ns = 1, nstress
      NumNewBasin(ns) = 0
   end do
   ! assigning number of new basins for the first stress period
   do nw = 1, newwells
     nwell=int(anint(parent(indiv,nw)))
     if(pumpflagnew(nw,nwell).eq.2 .and. parent(indiv, 4+nw+newwells) .eq. 1) then
       NumNewBasin(1) = NumNewBasin(1) + 1
     end if
   end do



   ! assigning number of new basins for the remaining stress periods
   do ns2 = 2, nstress
    do nw = 1, newwells
      flag = 0   
      do ns1 = 1, (ns2-1)
         ni1 = nw+(2*newwells+8+oldwells)*(ns1-1)
         ni2 = nw+(2*newwells+8+oldwells)*(ns2-1)
         nwell_1=int(anint(parent(indiv,ni1)))
         nwell_2=int(anint(parent(indiv,ni2)))
         if(pumpflagnew(nw,nwell_2).eq.2 .and. parent(indiv, ni2+4+newwells) .eq. 1) then 
            if(pumpflagnew(nw,nwell_1).eq.2 .and. parent(indiv, ni1+4+newwells) .eq. 1) then
               if (nwell_1 .eq. nwell_2) then		
                  ! the basin already  existed in a previous stress period
                  flag = 1
                  goto 60		! move for the next well ...this basin is an old one	
               end if
            end if
         else
            goto 60
         end if
50    end do		!loop for ns1
      if (flag.eq.0) then
         NumNewBasin(ns2) = NumNewBasin(ns2) + 1
      end if
60 end do		!loop for newwells
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
!  specified input.  The subroutine also initializes the rand_xom number 
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
    
     integer :: npossum,ig2sum, temp
     integer :: i,j,k,l,m, n2j,ind
     character(80) ::  FLNAME

     double precision :: rand_x
    
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

!  Initialize rand_xom number generator
	 write (*,*) 'idum1 ', idum1 
	 write (74,*) 'idum1 ', idum1 
    call ran8(idum1,rand_x)  !Changed by F. Espinoza (4/10/03)

!    Added By Eva Sinha
    maxgen = nchrome * 2
	!maxgen = 15

 

!Initialize the rand_xom distribution of parameters in the individual
!parents when irestrt=0.
     if(.not.irestrt) then
        istart=1
        do i=1,npopsiz
           do j=1,nchrome
              call ran8(1,rand_x) !Changed by F. Espinoza (4/10/03)
              !call rand_xom_number(rand_x)  !Changed by F. Espinoza (4/10/03)
              iparent(i,j)=1
              if(rand_x .lt. 0.5) iparent(i,j)=0
           enddo
        enddo
     else
        !if irestrt.ne.0 read from restart file
        FLNAME = 'ga.restart'
        open (unit = 25, file = FLNAME, status = 'old')
        rewind 25
        if(multiscale2) then
			read(25, *) istart, npopsiz, best_selec_frac
		else
			read(25, *) istart, npopsiz
		end if
        do j = 1, npopsiz
          if(multiscale2) then
             read(25,*) k, fine_flag(j), (iparent(j,l), l =1,nchrome) !Modified by Eva Sinha for Multiscale_II
          else
             read(25,*) k, (iparent(j,l), l =1,nchrome)
		  end if
        end do

		call load_ran1(25)
		call load_ran2(25)

        close (25)
     end if
111 format(i3,2x,60i2)

   open(unit = 38, file='uma2.res',status='unknown')
	
   do i=1,npopsiz
   write(38,100) i,(iparent(i,j),j=1,nchrome)
   enddo
100 format(i4,2x,200i2)

write(38,*)
write(38,*)

   do j=1,npopsiz
   call decode(j,parent,iparent)
		                 write(38, 1113) j, (nint(parent(j,k)), k = 1, 7),&
			  & (parent(j,k), k = 8, 11),(nint(parent(j,k)), k = 12, 18),&
			  & (parent(j,k), k = 19, 22),(nint(parent(j,k)), k = 23, 29)
			 

			  enddo
1113 format(i5,7i4,2x,4(f5.3,2x),7i2,3x,4(f5.3,2x),7i2)



   close (38)



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

!================================ rand_xOM NUMBERS ==================================

!##################################################################################
!Returns a uniform rand_xom deviate between 0.0 and 1.0.  Set idum to 
!any negative value to initialize or reinitialize the sequence.
!This function is taken from W.H. Press', "Numerical Recipes" p. 199.!

   subroutine ran3(idum,rand_x)

     IMPLICIT none
     save

     double precision :: mbig=8800000.d0,mseed=1618033.d0,mz=0.d0,fac=1./8800000.d0
     double precision :: mj,mk,ma,rand_x

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
     rand_x=mj*fac

     return

   end subroutine ran3
   

   !##################################################################################
!Returns a uniform rand_xom deviate between 0.0 and 1.0.  Set idum to 
!any negative value to initialize or reinitialize the sequence.
!This function is taken from W.H. Press', "Numerical Recipes" p. 199.!

   subroutine ran5(idum,rand_x)

     IMPLICIT none
     save

     double precision :: mbig=8800000.d0,mseed=1618033.d0,mz=0.d0,fac=1./8800000.d0
     double precision :: mj,mk,ma,rand_x

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
     rand_x=mj*fac

     return

   end subroutine ran5
      
   !##################################################################################
!Returns a uniform rand_xom deviate between 0.0 and 1.0.  Set idum to 
!any negative value to initialize or reinitialize the sequence.
!This function is taken from W.H. Press', "Numerical Recipes" p. 199.!

   subroutine ran6(idum,rand_x)

     IMPLICIT none
     save

     double precision :: mbig=8800000.d0,mseed=1618033.d0,mz=0.d0,fac=1./8800000.d0
     double precision :: mj,mk,ma,rand_x

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
     rand_x=mj*fac

     return

   end subroutine ran6   

  !##################################################################################
  !Returns a uniform random deviate between 0.0 and 1.0.  Set idum to 
  !any negative value to initialize or reinitialize the sequence.
  !This function is taken from W.H. Press', "Numerical Recipes" p. 199.!

  subroutine ran8(idum,rand)

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

  end subroutine ran8
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
     if (multiscale2) then
		write(25,*) i+1,npopsiz, best_selec_frac
	 else
		write(25,*) i+1,npopsiz
	 end if
     do j=1,npopsiz 
          if (multiscale2) then
             write(*,1500) j, fine_flag(j), (iparent(j,l), l = 1, nchrome) !Modified by Eva Sinha for Multiscale_II
             write(25,1500) j, fine_flag(j), (iparent(j,l),l = 1, nchrome)    !Modified by Eva Sinha for Multiscale_II
          else
             write(*,1500) j, (iparent(j,l), l = 1, nchrome) 
             write(25,1500) j, (iparent(j,l),l = 1, nchrome)
          end if
       end do

	 call save_ran1(25)
	 call save_ran2(25)

     CLOSE (25)
     kount=0
  endif
!
1500 format(i5,3x,i5,3x,51i2)
  !
  return
end subroutine restart

!##################################################################################

!============================== HGA SUBROUTINES ==================================

!##################################################################################
   subroutine SAHGA(fev,feve,nind,lsind,Succ,DeltaSGA,best0)

     IMPLICIT NONE
     save

     integer :: ni,nj,nind,nls,np,fev,feve,fevhga,fevhgae,mm,nw,nind0,j,nn,ninls
     integer :: lsind(maxlsind),Succ(maxlsind),jj,npp,initls,nwell

     double precision :: DeltaSGA,best0,Ratio1,Ratio2,MB1,MB2, &
          &  bestHGA,DeltaHGA,finit,ffin,Qtt,norm,nfitness
     double precision :: Qw0(maxlsind,nowells),Qw1(maxlsind,nowells),Qwp(nowells)
     double precision :: par(maxlsind,nowells)

     nls=0
     nw=nowells
     nind0=nind
     Ratio1=dabs(DeltaSGA)/dble(npopsiz)
     bestHGA=best0

	 par=0.d0
     fevhga=0
     fevhgae=0
     
     do ni=1,nind+1
	    Succ(ni)=0
	    call decode(lsind(ni),parent,iparent)
        do np=1,4
  		   nwell=nint(parent(lsind(ni),np))
           Qw0(ni,np)=parent(lsind(ni),newwells+np)*parent(lsind(ni),newwells+4+np)
           Qw0(ni,4+np)=parent(lsind(ni),2*newwells+4+np)*parent(lsind(ni),2*newwells+8+np)
		   par(ni,np)=parent(lsind(ni),newwells+np)*parent(lsind(ni),newwells+4+np)
           par(ni,4+np)=parent(lsind(ni),2*newwells+4+np)*parent(lsind(ni),2*newwells+8+np)
		enddo
     enddo

     do nj=1,maxlsiter

        if(nj.gt.1) nind0=int(nind0*(1.0d0-eps))   !changed ceiling for int
	    nind0=max(2,nind0)

        finit=0.d0
        do ni=1,nind+1
           finit=finit+fitness(1,lsind(ni))  !moved outside of do loop
        enddo
        
        do ni=1,nind0+1

           write(777,*) 'Evaluating :', nj,ni,lsind(ni)

           if(nj.eq.1) then
              do np=1,nw
                 Qw1(ni,np)=Qw0(ni,np)
              enddo
           endif

              do np=1,nw
                 call ran5(idumlsi,rand_x)
                 Qwp(np)=Qw1(ni,np)*(1.0d0-alphals+2.0*alphals*rand_x)
                 Qwp(np)=min(Qwp(np),1.0d0)
                 Qwp(np)=max(Qwp(np),0.0d0)
              enddo
 
              Qtt=maxval(Qwp)
              if(Qtt.eq.0.d0) then
                 call ran5(idumlsi,rand_x)
                 nn=nint(nowells*rand_x)
				 nn=min(nowells,nn)
				 nn=max(1,nn)
                 Qwp(nn)=1.0d0
			  endif

              do np=1,4
			     write(777,100) np,Qwp(np),Qw1(ni,np),parent(lsind(ni),newwells+4+np)
				 100 format(i3,2x,2(f25.5,2x),f3.0)
			  enddo

              do np=5,8
			     write(777,100) np,Qwp(np),Qw1(ni,np),parent(lsind(ni),2*newwells+8+np)
			  enddo

			  do np=1,4
			     parent(lsind(ni),newwells+np)=Qwp(np)
                 parent(lsind(ni),2*newwells+4+np)=Qwp(np+4)
			  enddo

             call prepfunc(lsind(ni),MB1,MB2)

             !fitfess function for the coarse grid
             if ((gridflagx.eq.1) .and. (gridflagy.eq.1))then
!                call Obj_Func_Coarse(Fitness1,Total_cost,Pen_Rdx,Pen_Tnt,gridflagx, gridflagy,drycell_c, drycell_h)
             endif
             
             !fitfess function for the fine grid
             if ((gridflagx.eq.2) .and. (gridflagy.eq.2))then
!                 call Obj_Func_Fine(Fitness1,Total_cost,Pen_Rdx,Pen_Tnt,gridflagx,gridflagy,drycell_c,drycell_h)
             endif

			 fevhga=fevhga+1

             nfitness = Fitness1+MB1+MB2

10         if(optflag.eq.1) then
              if(fitness(1,lsind(ni)).gt.nfitness) then
			     if(ni.eq.1) print *, 'success 1',nfitness,fitness(1,lsind(ni))
			     if(ni.ne.1) print *, 'success 2',nfitness,fitness(1,lsind(ni))
                 fitness(1,lsind(ni))=nfitness 
				 cost(lsind(ni))=Total_cost
				 pen1(lsind(ni))=Pen_Rdx
				 pen2(lsind(ni))=Pen_Tnt
				 pen3(lsind(ni))=MB1
                 pen4(lsind(ni))=MB2
                 fevhgae=fevhgae+1
                 bestHGA=min(bestHGA,nfitness)
                 Succ(ni)=1
                 do np=1,4
                    Qw1(ni,np)=Qwp(np)
                    Qw1(ni,np+4)=Qwp(np+4)
                    if(parent(lsind(ni),newwells+4+np).eq.1) par(ni,np)=dabs(Qwp(np))
                    if(parent(lsind(ni),2*newwells+8+np).eq.1) par(ni,np+4)=dabs(Qwp(np+4))
                 enddo
              endif
           else
              if(fitness(1,lsind(ni)).lt.nfitness) then
			     if(ni.eq.1) print *, 'success 1',nfitness,fitness(1,lsind(ni))
			     if(ni.ne.1) print *, 'success 2',nfitness,fitness(1,lsind(ni))
                 fitness(1,lsind(ni))=nfitness
				 cost(lsind(ni))=Total_cost
				 pen1(lsind(ni))=Pen_Rdx
				 pen2(lsind(ni))=Pen_Tnt
				 pen3(lsind(ni))=MB1
				 pen4(lsind(ni))=MB2
				 fevhgae=fevhgae+1
                 bestHGA=max(bestHGA,nfitness)
                 Succ(ni)=1
                 do np=1,nw
                    Qw1(ni,np)=Qwp(np)
                    Qw1(ni,np+4)=Qwp(np+4)
                    if(parent(lsind(ni),newwells+4+np).eq.1) par(ni,np)=dabs(Qwp(np))
                    if(parent(lsind(ni),2*newwells+8+np).eq.1) par(ni,np+4)=dabs(Qwp(np+4))
                 enddo
              endif
           endif
           
        enddo

        ffin=0.d0
        do ni=1,nind+1
           ffin=ffin+fitness(1,lsind(ni))
        enddo
 
        DeltaHGA=dabs((finit-ffin)/dble(nind+1))
        Ratio2=DeltaHGA/dble(fevHGA)
        if(Ratio2.le.Ratio1) goto 20
        
     enddo
  
20   fev=fev+fevhga
     feve=feve+fevhgae

     do ni=1,nind+1
        do np=1,4
  		   nwell=nint(parent(lsind(ni),np))
           parent(lsind(ni),newwells+np)=par(ni,np)
           parent(lsind(ni),2*newwells+8+np)=par(ni,np+4)
        enddo
     enddo
     
     return

   end subroutine SAHGA

!##################################################################################
   subroutine InitEvolution(nind,lb,lsind)

     IMPLICIT none
     save

     integer :: j,ni,nb,ind,nind,nbald,nn1,index(npopsiz),nitt(maxlsind),nfin(maxlsind)
     integer :: lsind(maxlsind),lb(maxlsind),aa,bb,jworst

     double precision :: fworst

     do j=1,npopsiz
        index(j)=j
     enddo
                 
     nind=nint(probls*npopsiz)
	 nind=max(2,nind)

	 lsind(1)=1

     call bubblesort1(npopsiz,fitness,index)

     nn1=npopsiz/nind
     
     do j=1,nind-1
        nitt(j)=1+nn1*(j-1)
        nfin(j)=nn1*j
     enddo
     nitt(nind)=nfin(nind-1)+1
     nfin(nind)=npopsiz
     do ni=2,nind+1
        aa=nitt(ni-1)
        bb=nfin(ni-1)
  10    call ran3(idumls,rand_x)
        jworst=int(aa+(bb-aa)*rand_x)         
        ind=index(jworst)
		if(ind.eq.1) goto 10
        lsind(ni)=index(jworst)
     enddo

     nBald=nint(propBald*nind)
     do ni=1,nind+1
        lb(ni)=0
     enddo
  
     nb=0

     if(propBald.gt.0.d0) then 
115     do ni=2,nind+1
           call ran6(idumb,rand_x)
           if(rand_x.le.1.5d0*propBald.and.lb(ni).eq.0) then
              lb(ni)=1
              nb=nb+1
              if(nb.eq.nBald) goto 120
           endif
        enddo
                 
        if(nb.lt.nBald) goto 115
     endif

120  return

   end subroutine InitEvolution

!##################################################################################
   subroutine Baldwin(nind,lsind,Succ,lb)

     IMPLICIT none
     save

     integer :: ni,np,nind,j
     integer :: lsind(maxlsind),lb(maxlsind),Succ(maxlsind)

     do ni=1,nind+1         
        if(lb(ni).eq.0) then
           if(ni.eq.1) then
              do np=1,nchrome
                 ibest(1,np)=iparent(lsind(1),np)
              enddo
           endif
           if(Succ(ni).eq.1) then                                
              do np=newwells+1,newwells+4
                 call code(lsind(ni),np,parent,iparent)
              enddo
              do np=2*newwells+5,2*newwells+8
                 call code(lsind(ni),np,parent,iparent)
              enddo
              call decode(lsind(ni),parent,iparent)
           endif
        endif    
     enddo
              
     return
  
   end subroutine Baldwin
   
  !##################################################################################
  !This subroutine establishes the best individual.
  ! Modified By Eva Sinha for Multiscale_II
  subroutine BestValue(best)

    IMPLICIT none
    save

    integer :: m
    double precision :: best

    best = huge(best)

    do j=1,npopsiz
       if (multiscale2) then
          if (fine_flag(j) .eq. 2) then
             if(fitness(1,j).lt.best) then 
                best = fitness(1,j)
                jbest(1) = j
                !Writing the coded form of the best individual
                do m = 1,nchrome
                   ibest(1,m)=iparent(j,m)
                enddo
             endif
          end if
       else				! if not multiscale2
          if(fitness(1,j).lt.best) then 
             best = fitness(1,j)
             jbest(1) = j
             !Writing the coded form of the best individual
             do m = 1,nchrome
                ibest(1,m)=iparent(j,m)
             enddo
          end if
       end if
    enddo

    return

  end subroutine BestValue

!#######################################################################################

end program uma
 










