subroutine Obj_Func_Coarse(Fitness, Total_cost, Pen_Rdx, Pen_Tnt, gridflagx,gridflagy,drycell_c, drycell_h)
!*********************************************************************
!  This program calculates the values of the fitness function of    
!  optimization simulation results on Umatilla Chemical Depot, and    
!  checks to see if the constraints are satisfied.
!  Modified by Abhishek Singh, based on the original objective function,                    
!  By Yan Zhang, GeoTrans, Inc.                                       
!      units: concentration ---- ug/L                                 
!             length ----------- ft                                   
!             mass ------------- kg                                   
!             time ------------- year                                 
!             pumping rate ----- gpm                                  
!             cost ------------- thousands of dollars                 
!*********************************************************************
	  USE PARAMS
      IMPLICIT NONE
	  SAVE
! A lot of the parameters for the fitness function are set here. It is desirable to
! shift these to a param file, so that they can be changed when needed.

! Original Buffers are addded by Eva Sinha
! the entire umatilla grid is  made half, in order to do so the buffers
! OriPmpZon, OriBuff, OriDelr, OriDelc, OriBotElev are read from the corresponding files and then
! they are saved in new buffers by the name PmpZon, Buff, Delr, Delc, BotElev
! which are half the size of the previous buffers
! for the buffers Head, CRdx, CTnt the conversion was not required as they get their values 
! directly from buffers HEADfrmMODF and CONCfrmMT3D respectively.


      integer :: PmpZon(Nrow1,Ncol1), Buff(Nrow1,Ncol1),UMAERROR,&
	         & OriPmpZon(OriNrow1,OriNcol1), OriBuff(OriNrow1,OriNcol1),&
		 & Nwel(Nstress), NumCells(Nstress), LocWel(3,Nstress,MxWel1),&
		 & WelZon(Nstress,MxWel1),&
                 & NewWell(Nstress), NewBasin(Nstress), NewGac(Nstress),&
                 & CountPmpCap(Nstress), PmpRchSP(Nstress),&
                 & CountBufRdx(Nstress), CountBufTnt(Nstress),&
                 & ExceedRdx(Nrow1,Ncol1,Nstress), ExceedTnt(Nrow1,Ncol1,Nstress),&
                 & FormulNum, RchLoc, Numy, Ny_Rdx, Ny_Tnt, Ny, &
                 & FlgCleanup,PeriodEw2, NumNewWell, NumNewBasin,CountConstr,&
                 & ir,ic,iy,iy_start,iy_end,K,I,J,Year, temp_int, iter, i2
      integer, intent(inout) :: gridflagx, gridflagy, drycell_c, drycell_h
	  !integer, external :: Ew2StartPeriod

      real :: Delr(Ncol1), Delc(Nrow1),OriDelr(OriNcol1), OriDelc(OriNrow1),&
	     & PmpQtotal(Nstress), RchQtotal(Nstress),&
             & PmpRate(Nstress,MxWel1), RchRate(Nstress,MxWel1),&
             & WelConc(MxYr1,MxWel1), PlumeArea(Nstress), PlumeMass(MxYr1),&
             & CRdx(Nrow1,Ncol1,MxYr1), CTnt(Nrow1,Ncol1,MxYr1),V_Rdx, V_Tnt,&
             & Head(Nrow1,Ncol1,Nstress), BotElev(Nrow1,Ncol1), &
			 & OriBotElev(OriNrow1,OriNcol1), &
             & Dsc_Rate, Alpha, InitialArea, Qmax1,&
             & CCW, CCB, CCG, FCL, FCE, VCE, VCG, VCS, ObjFunc, final_mass, temp_real
      double precision, intent(inout) :: Fitness, Total_cost, Pen_Rdx, Pen_Tnt

	  real :: chromosome(20)
	  
	  real, allocatable :: CONCfrmMT3D(:,:,:,:), HEADfrmMODF(:,:,:)

	  !this is how an external C subroutine is called with Visual Fortran. 
	  !This may not be correct with another compiler.
	  
	 !real, external :: CCWfunc, CCBfunc, CCGfunc, PvAnnuity, VCEfunc, VCGfunc,&
     !       & VCSfunc, AreaPlume2

      character*40 infil,wel_fil,sc_fil,rdx_fil,tnt_fil,hds_fil,out_fil
      character idbg
      character*80 temp_flname
      character*200 namefile
      character*50 namefilemt3d
      
      
      pen_wt_Rdx = 10000.0d0 
      pen_wt_Tnt = 10000.0d0
! Add code to prepare files and call modflow and MT3DMS
! the code for reading the well file will be changed so that a seperate
! file will not be needed. This information will be obtained from the 
! chromosome.
! This is also where the sampling loop will start, and the bcf file will
! need to be rewritten. 
! some changed will have to be made to the input file as well.
      if ((gridflagx .eq. 1) .and. (gridflagy.eq.1))then
         namefile = "TEST_coarse.nam"
         namefilemt3d = "test_mt3d_coarse.nam"
      end if
      if ((gridflagx .eq. 2) .and. (gridflagy.eq.2))then
         namefile = "TEST_fine.nam"
         namefilemt3d = "test_mt3d_fine.nam"
      end if
	  
      allocate(CONCfrmMT3D(Nrow1,Ncol1,Nstress * PeriodLength,NoSpecies))
      allocate(HEADfrmMODF(Nrow1,Ncol1,Nstress))
    !print *, 'input no. of iterations'
	!read *, iter
	!do i2 = 1,iter
	HEADfrmMODF = 0.00d0
	UMAERROR=0
	!write(*,*) 'calling modflow'
	call modflow_sub(namefile, HEADfrmMODF, Ncol1, Nrow1, Nstress,UMAERROR)
	CONCfrmMT3D = 0.00d0
    IF(UMAERROR.EQ.0) THEN
	   call mt3d_sub(namefilemt3d, CONCfrmMT3D, Ncol1,Nrow1,Nstress*PeriodLength,NoSpecies)
	ELSE
	   CONCfrmMT3D = 999.0d0
	ENDIF
	!end do

  ! Opening the response file for the input information
   if ((gridflagx .eq. 1) .and. (gridflagy.eq.1))then
     temp_flname = 'umatilla_input_coarse.fil'
   end if
   if ((gridflagx .eq. 2) .and. (gridflagy.eq.2))then
     temp_flname = 'umatilla_input_fine.fil'
   end if
   OPEN(UNIT = 103, FILE = temp_flname, STATUS = 'UNKNOWN')

!print *, ' Reading information from the response file'
!-------------------------------------------------------------
     read (103,*) FormulNum
!'Print the Input Data (Y or N)? 
     
      read (103,*) idbg
! Input Recharge Basin Number 
      read (103,*) RchLoc
! Input File for Post-Processing information
50    read (103,'(a40)' ) infil
      if(infil .eq. ' ') then
        write(*,*) '     Error: File Name Not Given.'
        write(*,*) '     Please Try Again =>'
        go to 50
      end if
! MODFLOW WEL Package
51    read (103,'(a40)') wel_fil
      if(wel_fil .eq. ' ') then
        write(*,*) '     Error: File Name Not Given.'
        write(*,*) '     Please Try Again =>'
        go to 51
      end if
! MT3D SCONC Array File
52    read (103,'(a40)') sc_fil
      if(sc_fil .eq. ' ') then
        write(*,*) '     Error: File Name Not Given.'
        write(*,*) '     Please Try Again =>'
        go to 52
      end if
! MT3D RDX UCN File
53    read (103,'(a40)') rdx_fil
      if(rdx_fil .eq. ' ') then
        write(*,*) '     Error: File Name Not Given.'
        write(*,*) '     Please Try Again =>'
        go to 53
      end if

! MT3D TNT UCN File
54    read (103,'(a40)') tnt_fil
      if(tnt_fil .eq. ' ') then
        write(*,*) '     Error: File Name Not Given.'
        write(*,*) '     Please Try Again =>'
        go to 54
      end if

! MODFLOW Unformatted Head File
55    read (103,'(a40)') hds_fil
      if(hds_fil .eq. ' ') then
        write(*,*) '     Error: File Name Not Given.'
        write(*,*) '     Please Try Again =>'
        go to 55
      end if

! Output File for Post-Process
! ----------------------------
!      write(*,'(/5x,''Output File =>'',$)')
56    read (103,'(a40)') out_fil
      if(out_fil .eq. ' ') then
        write(*,*) '     Error: File Name Not Given.'
        write(*,*) '     Please Try Again =>'
        go to 56
      end if
!      write(*,'(20x,a40)') out_fil
!
close (103)


!print *,' Initializing'
! ------------
      CCW=0
      CCB=0
      CCG=0
      FCL=0
      FCE=0
      VCE=0
      VCG=0
      VCS=0
      Numy = Nstress * PeriodLength
      FlgCleanup = 0
      CountConstr = 0
      NumNewWell = 0
      NumNewBasin = 0
      do K = 1,Numy
        PlumeMass(K) = 0.0
      enddo
!
!print *,' Open input files'
! ----------------
      temp_flname = out_fil
!      call APPEND_PID(temp_flname)
      open(iout,file=temp_flname,status='unknown')
      call opnfil(infil,iin,iout,idbg,OriNlay1,OriNrow1,OriNcol1,Dsc_Rate,Alpha,&
     &            OriDelr,OriDelc,OriPmpZon,OriBuff,OriBotElev)
!     added by Eva Sinha
!     creating new grid values using the original grids
      call CreateNewGrid (Nrow1,Ncol1,Delr,Delc,PmpZon,Buff,BotElev, &
	  &                   OriNrow1,OriNcol1,OriDelr,OriDelc,OriPmpZon,OriBuff,OriBotElev)
      call opnwel(iwel,iout,wel_fil,idbg,RchLoc,MxWel1,Nstress,LocWel,&
     &            PmpRate,Nwel,NumCells,RchRate,NewBasin)
      call opnsc(isc,iout,sc_fil,idbg,Nrow1,Ncol1,OriNrow1,OriNcol1,Cl_Rdx,Cl_Tnt,Delc,&
     &           Delr,InitialArea)
      call opnhds(ihds,iout,hds_fil,idbg,Nstress,Nlay1,Nrow1,Ncol1,Head)    
	  call opnucn(1,iout,rdx_fil,Numy,Nrow1,Ncol1,Cl_Rdx,CRdx,Ny_Rdx)
!      write(*,'(/1x,''The cleanup year for RDX: '',i5)') Ny_Rdx
      call opnucn(2,iout,tnt_fil,Numy,Nrow1,Ncol1,Cl_Tnt,CTnt,Ny_Tnt)
!      write(*,'(/1x,''The cleanup year for TNT: '',i5)') Ny_Tnt
     

	  deallocate(CONCfrmMT3D)
	  deallocate(HEADfrmMODF)

!print *,' Cleanup Year'
! ------------
      Ny = Numy
      if(Ny_Rdx .gt. 0 .and. Ny_Tnt .gt. 0) then
        FlgCleanup = 1
        Ny = Max(Ny_Rdx,Ny_Tnt)
      endif

!
!print *,' Location, Rate, Concentrations, and Zone Number of Each Well'
! ------------------------------------------------------------
!      write(iout,6)
      do K = 1,Nstress
        iy_start = (K-1)*5+1
        iy_end = K*5
!        write(iout,7) K
        do i = 1,Nwel(K)
	  ir = LocWel(2,K,I)
	  ic = LocWel(3,K,I)
          do iy = iy_start,iy_end
             if (CRdx(ir,ic,iy) .lt. 0.0) CRdx(ir,ic,iy) = -CRdx(ir,ic,iy)
             if (CTnt(ir,ic,iy) .lt. 0.0) CTnt(ir,ic,iy) = -CTnt(ir,ic,iy)            
            WelConc(iy,I) = CRdx(ir,ic,iy) + CTnt(ir,ic,iy)
          end do
          WelZon(K,i) = PmpZon(ir,ic)
!          write(iout,8)(LocWel(J,K,I),J=1,3),PmpRate(K,I), &
!     &                 (WelConc(iy,I),iy=iy_start,iy_end), &
!     &                  WelZon(K,I)
        end do
      end do
6     format(/5x,'Wells Used in Each Stress Period'/10x,'Layer',7x, &
     & 'Row',4x,'Column',2x,'Pumping Rate (gpm)',3x,'Concentration ', &
     & 'of RDX & TNT (ug/L) for Each Year',2x,'Zone Number'/10x,5('-'), &
     & 7x,3('-'),4x,6('-'),2x,18('-'),3x,47('-'),2x,11('-'))
7     format(10x,'Stress Period: ',i3)
8     format(5x,3i10,10x,f10.3,5f10.3,8x,i5)
!
!print *,' Stress Period when EW-2 Operates'
! --------------------------------
      call Ew2StartPeriod (LocWel, Nwel, MxWel1, Nstress, PeriodEw2)
!      write(iout,9)PeriodEw2
9     format(/5x,'Stress Period When EW-2 Starts'/10x,i5)
!
!print *,' New Wells in Each Stress Period'
! -------------------------------
      call CountNewWell (LocWel, Nwel, MxWel1, NewWell, Nstress)
!      write(iout,10)
      do K = 1,Nstress
!        write(iout,11)NewWell(K)
      end do
10    format(/5x,'Number of New Wells in Each Stress Period')
11    format(10x,i5)
!
!print *,' New Recharge Basins in Each Stress Period'
! -----------------------------------------
!      write(iout,12)
      do K = 1,Nstress
!        write(iout,11)NewBasin(K)
      end do
12    format(/5x,'Number of New Recharge Basins in Each Stress Period')
!
!print *,' Total Pumping and Recharge Rates in Each Stress Period'
! ------------------------------------------------------
      Qmax1=0
      call TotalPumping(Nwel, PmpRate, MxWel1, Nstress, PmpQtotal)
      call TotalPumping(NumCells, RchRate, MxWel1, Nstress, RchQtotal)
!      write(iout,13)
      do K = 1,Nstress
        if(PmpQtotal(K) .gt. Qmax1) Qmax1 = PmpQtotal(K)
!        write(iout,14) PmpQtotal(K),RchQtotal(K)
      end do
13    format(/5x,'Total Pumping and Recharge Rates in Each Stress ', &
     & 'Period (gpm)'/10x,'Pumping Rate',10x,'Recharge Rate' &
     & /10x,12('-'),10x,13('-'))
14    format(10x,f10.3,12x,f10.3)
!
!print *,' Number of GACs Installed in Each Stress Period'
! ----------------------------------------------
      call CalcNewGac (PmpQtotal, Alpha, NewGac, Nstress)
!      write(iout,15)
      do K = 1,Nstress
!        write(iout,11)NewGac(K)
      end do
15    format(/5x,'Number of GACs Installed in Each Stress Period')
!
!print *, ' Plume Area in Each Stress Period'
! --------------------------------
!      write(iout,16)
      PlumeArea(1) = InitialArea
      do K = 2,Nstress
        Year = (K-1)*5
        call AreaPlume2(CRdx, CTnt, Delr, Delc, Ncol1, &
     &                            Nrow1, Cl_Rdx, Cl_Tnt, MxYr1, Year, temp_real)
	    PlumeArea(K) = temp_real
		temp_real = 0.00
      enddo
      do K = 1,Nstress
!        write(iout,17)K,PlumeArea(K)
      enddo
16    format(/5x,'Plume Area at the Beginning of Each Stress Period', &
     & /10x,'Stress Period',5x,'Plume Area (ft*ft)' &
     & /10x,13('-'),5x,18('-'))
17    format(10x,i5,16x,e15.6)

!
! Objective Function Calculation. Skip Cost Calculation if Formulation 3
! ----------------------------------------------------------------------
!      write(iout,20)
20    format(//27x,'Objective Function Calculation'/27x,30('-'))
      !if(FormulNum .eq. 3) goto 500
!
! Calculate CCW: Capital Costs of New Wells
! -----------------------------------------
      call CCWfunc (PeriodEw2, NewWell, Dsc_Rate, Nstress, temp_real)
	  CCW = temp_real
!      write(iout,21) CCW
21    format(/5x,'The Capital Costs of New Wells (thousand of dollars)'&
     & /8x,f10.3)

	  temp_real = 0.00
!   'The Capital Costs of New Wells (thousand of dollars)' 
     
!
! Calculate CCB: Capital Costs of New Recharge basins
! ---------------------------------------------------
      call CCBfunc (NewBasin, Dsc_Rate, Nstress, temp_real)
	  CCB =  temp_real
!      write(iout,22) CCB
22    format(/5x,'The Capital Costs of New Recharge Basins (thousand',&
     & ' of dollars)'/8x,f10.3)

	  temp_real = 0.00
!   'The Capital Costs of New Recharge Basins (thousand of dollars)
!      
! Calculate CCG: Capital Costs of New GAC unit
! --------------------------------------------
      call CCGfunc(NewGac, Dsc_Rate, Nstress, temp_real)
	  CCG = temp_real
!      write(iout,23) CCG
23    format(/5x,'The Capital Costs of New GAC Units (thousand of ',&
     & 'dollars)'/8x,f10.3)
 
	  temp_real = 0.00
!   'The Capital Costs of New GAC Units (thousand of dollars)
!
! Calculate FCL: Fixed Costs of Labor
! -----------------------------------
     call PvAnnuity (Ny, AnnualCostLabor, Dsc_Rate, temp_real)
	  FCL = temp_real
!      write(iout,24) FCL
24    format(/5x,'The Fixed Costs of Labor (thousand of dollars)'&
     & /8x,f10.3)

	  temp_real = 0.00
!   'The Fixed Costs of Labor (thousand of dollars)'
!
! Calculate FCE: Fixed Costs of Electricity
! -----------------------------------------
      call PvAnnuity (Ny, AnnualCostElec, Dsc_Rate, temp_real)
	  FCE = temp_real
!      write(iout,25) FCE
25    format(/5x,'The Fixed Costs of Electricity (thousand of dollars)'&
     & /8x,f10.3)

      temp_real = 0.00
!  'The Fixed Costs of Electricity (thousand of dollars)' 
     
!
! Calculate VCE: Variable Electrical Costs of Operating Wells
! -----------------------------------------------------------
      call VCEfunc (Ny, Nwel, PmpRate, MxWel1, Dsc_Rate, Nstress, Alpha, temp_real)
	  VCE = temp_real
!      write(iout,26) VCE
26    format(/5x,'The Variable Costs of Electricity for Operating ',&
     & 'Wells (thousand of dollars)'/8x,f10.3)

	   temp_real = 0.00
     
!   'The Variable Costs of Electricity for Operating '

!      
! Calculate VCG: Variable Costs of Changing GAC Units
! ---------------------------------------------------
      call VCGfunc (Ny, Nwel, PmpRate, MxWel1, WelConc, Dsc_Rate, &
     &  Nstress, MxYr1, temp_real)
	 VCG = temp_real
!      write(iout,27) VCG
27    format(/5x,'The Variable Costs of Changing GAC Units (thousand ',&
     & 'of dollars)'/8x,f10.3)

	 temp_real = 0.00

!    'The Variable Costs of Changing GAC Units (thousand of dollars)'
!
! Calculate VCS: Variable Costs of Sampling
! -----------------------------------------
      call VCSfunc (Ny, PlumeArea, Dsc_Rate, Nstress, temp_real)
	  VCS = temp_real
!      write(iout,28) VCS
28    format(/5x,'The Variable Costs of Sampling (thousand of dollars)'&
     & /8x,f10.3)
!   'The Variable Costs of Sampling (thousand of dollars)' 

!
!print *, ' Cost Function for Formulation'
! -----------------------------------------
      Total_cost = CCW + CCB + CCG + FCL + FCE + VCE + VCG + VCS
      call Violation(CRdx, NRow1, NCol1,Numy, V_Rdx, Cl_Rdx, MxYr1) 
      call Violation(CTnt, NRow1, NCol1,Numy, V_Tnt, Cl_Tnt, MxYr1)
      Pen_Rdx = pen_wt_Rdx*V_Rdx
      Pen_Tnt = pen_wt_Tnt*V_Tnt
      Fitness = Total_cost + Pen_Rdx + Pen_Tnt

!      print *, 'Total Cost', Total_cost
      write(iout,30) FormulNum,Total_cost
30    format(/5x,'The Objective Function Value (thousands of dollars) ',&
     & 'for Formulation #',i3/8x,f10.3)

      !goto 501
! 'The Objective Function Value (thousands of dollars) '
     
!
!print *, ' Mass removal Function for Objective 2'
! ------------------------------------
500   write(iout,31) FormulNum
      call TotalMass(CRdx, CTnt, Delr, Delc, Head, BotElev, Ncol1, &
     &               Nrow1, Nstress, MxYr1, PlumeMass)
      do K = 1, Numy
        write(iout,32) K,PlumeMass(K)
      enddo
31    format(//5x,'The Objective Function Value for Formulation ',i2 &
     & /10x,'Modeling Year',10x,'Total Mass (kg)' &
     & /10x,13('-'),10x,15('-'))
32    format(10x,i5,18x,e15.6)

	 final_mass = PlumeMass(Max(Ny, Numy) )
!	 print *, "Final Mass", final_mass
!	 Print *, "Clean up time", Ny
      
!
!print *, 'Check Constraints'
!-----------------

!
!print *, ' Cleanup Year Constraint'
! -----------------------
      
      if(Ny .le. Numy .and. FlgCleanup .eq. 1) then
        !write(iout,102) Ny
      else
        !write(iout,103)
        CountConstr = CountConstr + 1
!************CLEAN UP YEARS MORE THAN 20!!!******************************	   	
      end if

!
! Total Pumping Rate Constraint
! -----------------------------
      
      if((FormulNum .ne. 2 .and. (Qmax1/Alpha) .le. cap_gac(1)) .or. &
     &   (FormulNum .eq. 2 .and. (Qmax1/Alpha) .le. cap_gac(3))) then
!***********TOTAL PUMPING RATE EXCEDED!!!********************************        
      else

        CountConstr = CountConstr + 1
      end if
!
! Pumping Capacity Constraint
! ---------------------------

      I = 0
      call PmpCapConstr(Nstress, Nwel, PmpRate, WelZon, MxWel1, Alpha, &
     &  CountPmpCap)
      do K = 1,Nstress
        I = I + CountPmpCap(K)
      enddo
      if(I .le. 0) then
        !write(iout,109)
		!Pumping capacity constraint satisfied
      else
        !write(iout,110)
		!Pumping capacity constraint not satisfied
        CountConstr = CountConstr + 1
        do K = 1,Nstress
          if(CountPmpCap(K) .gt. 0) then
		    !write(iout,111) K,CountPmpCap(K)
			!No of wells with more than allowed pumping rates
		   end if
        enddo
      endif
108   format(//5x,'--- Pumping Capacity Constraint ---')
109   format(/5x,'The Pumping Capacity Constraint Satisfied')
110   format(/5x,'The Pumping Capacity Constraint Not Satisfied' &
     & /10x,'Stress Period',10x,'Number of Wells Not Satisfied' &
     & /10x,13('-'),10x,29('-'))
111   format(12x,i5,26x,i5)
!
! Pumping-Recharge Balance Constraint
! -----------------------------------
      I = 0
      call PmpRchConstr(Nstress, PmpQtotal, RchQtotal, PmpRchSP)
      do K = 1,Nstress
        I = I + PmpRchSP(K)
      enddo
      if(I .le. 0) then
        !write(iout,113)
		!Pumping recharge rate constraint satisfied
      else
        !write(iout,114)
		!Pumping recharge rate constraint not satisfied
        CountConstr = CountConstr + 1
        do K = 1,Nstress
          if(PmpRchSP(K) .gt. 0) then 
		  !write(iout,115) K
		  ! stress period for constraint violation
		  end if
        enddo
      endif      
112   format(//5x,'--- Pumping-Recharge Balance Constraint ---')
113   format(/5x,'The Pumping-Recharge Balance Constraint Satisfied')
114   format(/5x,'The Pumping-Recharge Balance Constraint Not ', &
     & 'Satisfied In Stress Period')
115   format(10x,i5)
!
! Buffer Zone Constraint
! ----------------------
      I = 0
      J = 0
      call BufZonConstr(Nrow1,Ncol1,Nstress,MxYr1,Buff,CRdx,Cl_Rdx, &
     &  CountBufRdx,ExceedRdx)
      call BufZonConstr(Nrow1,Ncol1,Nstress,MxYr1,Buff,CTnt,Cl_Tnt, &
     &  CountBufTnt,ExceedTnt)
      do K = 1,Nstress
        I = I + CountBufRdx(K)
        J = J + CountBufTnt(K)
      enddo
      if(I .le. 0 .and. J .le. 0) then
	    !write(iout,117)
		!Buffer Zone constraint satisfied
      else
        !write(iout,118)
		!Buffer Zone constraint not satisfied
        CountConstr = CountConstr + 1
        do K = 1,Nstress
          if(CountBufRdx(K) .gt. 0) then 
		     !write(iout,119)K
			 !Constraint violated for RDX
		  end if
          do ir = 1,Nrow1
            do ic = 1,Ncol1
              if(ExceedRdx(ir,ic,K) .gt. 0) then
			  !write(iout,120)ir,ic
			  !For RDX in stress period K
			  end if
            enddo
          enddo
        enddo
        do K = 1,Nstress
          if(CountBufTnt(K) .gt. 0) then
		  !write(iout,121)K
		  !Constraint violated for TNT
		  end if
          do ir = 1,Nrow1
            do ic = 1,Ncol1
              if(ExceedTnt(ir,ic,K) .gt. 0) then
			  !write(iout,120)ir,ic
			  !For TNT in stress period K
			  end if
            enddo
          enddo
        enddo
      endif
116   format(//5x,'--- Buffer Zone Constraint ---')
117   format(/5x,'The Buffer Zone Constraint Satisfied')
118   format(/5x,'The Buffer Zone Constraint Not Satisfied' &
     & /10x,'Stress Period and Cell Location')
119   format(10x,'For RDX, In Stress Period',i5)
120   format(12x, '(',i5,',',i5,')')
121   format(10x,'For TNT, In Stress Period',i5)
!
! Maximum Number of New Wells Constraint for Formulation #3
! ---------------------------------------------------------
! THis constraint can be dealt with hard-coding within the chromosome.
      !if(FormulNum .eq. 3) then
        !write(iout,122)
         do K = 1,Nstress
           NumNewWell = NumNewWell + NewWell(K)
         enddo
        !write(iout,123)NumNewWell
        if(NumNewWell .le. MaxNewWells) then
          !write(iout,124)
        else
          !write(iout,125)
          CountConstr = CountConstr + 1
        endif
      !endif
!122   format(//5x,'--- Maximum Number of New Wells Constraint ---')
!123   format(/5x,'Total Number of New Wells Ever Installed'/10x,i5)
!124   format(/5x,'The Maximum Number of New Wells Constraint Satisfied')
!125   format(/5x,'The Maximum Number of New Wells Constraint Not ', &
!     & 'Satisfied')
!
! Maximum Number of New Recharge Basins Constraint for Formulatin #3
! ------------------------------------------------------------------
! THis constraint can be dealt with hard-coding within the chromosome. 
      !if(FormulNum .eq. 3) then
        !write(iout,126)
        do K = 1,Nstress
          NumNewBasin = NumNewBasin + NewBasin(K)
        enddo
        !write(iout,127)NumNewBasin
        if(NumNewBasin .le. MaxNewBasin) then
          !write(iout,128)
        else
          !write(iout,129)
          CountConstr = CountConstr + 1
        endif
      !endif
!126   format(//5x,'--- Maximum Number of New Recharge Basins ', &
!    & 'Constraint ---')
!127   format(/5x,'Total Number of New Recharge Basins Installed'/10x,i5)
!128   format(/5x,'The Maximum Number of New Recharge Basins Constraint', &
!    & ' Satisfied')
!129   format(/5x,'The Maximum Number of New Recharge Basins Constraint',&
!     & ' Not Satisfied')
!
! Number of Constraints Not Satisfied
! -----------------------------------
!       write(*,130)CountConstr
!*************CALCULATE TOTAL PENALTY HERE*************************************
130   format(//5x,'Number of Constraints Not Satisfied'/10x,i5)
!
!print *, ' End of the post-processor'
! -------------------------
!	  return

contains
      


!*********************************************************************c
      subroutine opnfil(infil,iin,iout,idbg,OriNlay1,OriNrow1,OriNcol1, &
	 &                  Dsc_Rate, Alpha,OriDelr, OriDelc, &
	 &                  OriPmpZon, OriBuff, OriBotElev)
!       This subroutine opens the input file, read the OriDelr, OriDelc, Orizone,
!       and oribuffer zone. And print to output file if idbg is 'Y'
!         Dsc_Rate = Discount Rate
!         Alpha = Fraction of Time Pumps Are Operated
!         OriDelr(Ncol1) = Spacing Along Rows
!         OriDelc(Nrow1) = Spacing Along Columns
!         OriPmpZon(Nrow1,Ncol1) = Pumping Capacity Zone Number of Each Cell
!         OriBuff(Nrow1,Ncol1) = Plume Concentration Buffer Zone
!         OriBotElev(Nrow1,Ncol1) = Bottom Elevation of Layer 1

      implicit none
      character infil*40,fmtin*20,idbg, temp_flname*80
      integer :: iin, iout, &
	  &           OriNlay1, OriNrow1, OriNcol1, OriPmpZon(OriNrow1,OriNcol1), &
      &           OriBuff(OriNrow1,OriNcol1), i, j
      real :: Dsc_Rate, Alpha, OriDelr(OriNcol1),  &
      &        OriDelc(OriNrow1), OriBotElev(OriNrow1,OriNcol1)

!
! Initializing
! ------------
      do i = 1,OriNrow1
        do j = 1,OriNcol1
          OriPmpZon(i,j) = 0
        end do
      end do
!
! Open input file
! ---------------
      open (iin,file=infil,status='old',access='sequential')
!
! Discount rate
! -------------
      read (iin,*) Dsc_Rate
      if(idbg .eq. 'Y' .or. idbg .eq. 'y') write(iout,10) dsc_rate
10    format(/10x,'The discount rate: ',f10.3)
!
! Coefficient factor accounting for system up-time
! ------------------------------------------------
      read (iin,*) Alpha
      if(idbg .eq. 'Y' .or. idbg .eq. 'y') write(iout,11) Alpha
11    format(/10x,'The coefficient factor accounting for system ', &
     & 'up-time: ',f10.3)
!
! Delr and Delc arrays
! -------------------------
      call rd1darr(iin,OriNcol1,Oridelr)
      if(idbg .eq. 'Y' .or. idbg .eq. 'y') &
     &        write(iout,12) (Oridelr(i),i=1,OriNcol1)
      call rd1darr(iin,OriNrow1,Oridelc)
      if(idbg .eq. 'Y' .or. idbg .eq. 'y') &
     &        write(iout,13) (Oridelc(i),i=1,OriNrow1)
12    format(/27x,'Width Along Rows (delr)'/27x,22('-')/10(1x,g15.6))
13    format(/27x,'Width Along Cols (delc)'/27x,22('-')/10(1x,g15.6))
!
! Pumping Capacity Zone. Maximum rate is 400 gpm for the wells 
! in zone 1 and 1000gpm for the wells in zone 2.
! ------------------------------------------------------------
      read (iin,'(20x,a20)') fmtin
      do i = 1,OriNrow1
        read (iin,fmtin) (OriPmpZon(i,j),j = 1,OriNcol1)
      end do
      if(idbg .eq. 'Y' .or. idbg .eq. 'y') then
        write(iout,25)
        do i = 1,OriNrow1
          write(iout,26) (OriPmpZon(i,j),j = 1,OriNcol1)
        end do
      endif
25    format(/27x,'Extraction Capacity Zone'/27x,24('-'))
26    format(20(i5))
!
! Plume Concentration Buffer Zone. > 0 for the cells inside the 
! buffer zone where the plume concentration is allowed to exceed 
! the cleanup limits; = 0 for the cells outside the buffer zone
! --------------------------------------------------------------
      read (iin,'(20x,a20)') fmtin
      do i = 1,OriNrow1
        read (iin,fmtin) (OriBuff(i,j),j = 1,OriNcol1)
      end do
      if(idbg .eq. 'Y' .or. idbg .eq. 'y') then
        write(iout,29)
        do i=1,OriNrow1
          write(iout,30) (OriBuff(i,j),j = 1,OriNcol1)
        end do
      endif
29    format(/27x,'Plume Concentration Buffer Zone'/27x,31('-'))
30    format(20(i5))
!
! Bottom Elevation Array of Layer 1
! ---------------------------------
      read (iin,'(20x,a20)') fmtin
      do i = 1,OriNrow1
        read (iin,fmtin) (OriBotElev(i,j),j = 1,OriNcol1)
      end do
      if(idbg .eq. 'Y' .or. idbg .eq. 'y') then
        write(iout,31)
        do i = 1,OriNrow1
          write(iout,32) (OriBotElev(i,j),j = 1,OriNcol1)
        end do
      endif
31    format(/27x,'Bottom Elevation of Layer 1'/27x,27('-'))
32    format(10f10.3)
!
! File opened successfully, return
! --------------------------------
!      write(*,'(/1x,''Input file opened successfully'')')
      close(iin)
      return
      end subroutine opnfil



!*********************************************************************c
      subroutine opnwel(in,iout,filename,idbg,RchLoc,MxWel1,Nstress,LocWel, &
     &                  PmpRate,Nwel,NumCells,RchRate,NewBasin)
!        This subroutine reads MODFLOW WEL package. Return well 
!        locations, pumping rates, recharge rates,etc.
!         RchLoc = 1 if Recharge Basin Number is in Col 41-50;
!                  2 if Recharge Basin Number is in Col 51-60.
!         Nwel(Nstress): Number of Extraction Wells in Each Stress Period
!         LocWel(3,Nstress,MxWel1) = L,R,C of Each Well in Each Period
!         PmpRate(Nstress,MxWel1) = Q of Each Well in Each Stress Period
!         NumCells(Nstress) = Number of Recharge Basins in Each Period
!         RchRate(Nstress,MxWel1) = Q of Each Recharge Well in Each Period
!                                Well in Each Stress Period

      implicit None
      integer :: in,iout,MxWel1,Nstress,RchLoc, LocWel(3,Nstress,MxWel1), irch, welltype, compnum
      integer :: Nwel(Nstress),NumCells(Nstress),NewBasin(Nstress), TotNumBas
      integer :: I,J,K,MaxWell,itmp,il,ir,ic,II,NumZero, NumExt, NumInj
      real :: PmpRate(Nstress,MxWel1),RchRate(Nstress,MxWel1), q
      real, parameter :: togpm = 1.4238e-5
      character filename*40,idbg
	  character(80) :: temp_flname
!
! Initializing
! ------------
      TotNumBas = 3		!gives the total number of injection basins, initially there are 3 old injection basins
      do I = 1,MxWel1
        do K = 1,Nstress
          do J = 1,3
            LocWel(J,K,I) = 0
          end do
		  ! for all possible wells, stress periods set
		  ! pumping/recharge rate as zero, and basin number as zero
          PmpRate(K,I) = 0
          RchRate(K,I) = 0
        end do
      end do
      do K = 1,Nstress
        Nwel(K) = 0
        NumCells(K) = 0
		NewBasin(K)=0
      end do
!
! Open MODFLOW WEL package
! ------------------------
      temp_flname = filename
!	  call APPEND_PID (temp_flname)
      open (in,file=temp_flname,status='old')
 !     read (in,'(i10)') MaxWell
 !     if ((MaxWell+4) .gt. MxWel1) then			!!!!CHECK!!!! we r not printing this....
 !	write(iout,1)
 !	stop
 !    end if
!      if(idbg .eq. 'Y' .or. idbg .eq. 'y') write(iout,2)MaxWell
1     format(1x,'***Error: Parameter MxWel1 must be assigned larger***')
2     format(/1x,'Maximum number of wells: ',i5)
!
! Read wells in each stress period
! --------------------------------
      do K = 1,Nstress
        NumZero = 0
		! itmp gives the number of wells in the current stress period
        read (in,'(i10)') itmp
        if(idbg .eq. 'Y' .or. idbg .eq. 'y') write(iout,'(i10)') itmp
!
!       If itmp=-1, set the wells same as those in last stress period
!       -------------------------------------------------------------
        if (itmp .lt. 0) then
          Nwel(K) = Nwel(K-1)
	      NumCells(K) = NumCells(K-1)
	      do I = 1,Nwel(K)
	         do J = 1,3
	           LocWel(J,K,I) = LocWel(J,K-1,I)
	         end do
	         PmpRate(K,I) = PmpRate(K-1,I)
             if(idbg .eq. 'Y' .or. idbg .eq. 'y') then
	             write(iout,3)(LocWel(J,K,I),J=1,3),PmpRate(K,I)
	         endif
	      end do
	      if(NumCells(K) .ge. 1) then
	          do I = 1,NumCells(K)
	             RchRate(K,I) = RchRate(K-1,I)
                 if(idbg .eq. 'Y' .or. idbg .eq. 'y') then
	                 write(iout,4)RchRate(K,I)
	             endif
	          end do
	      end if
!
!       Read L,R,C,Q,Rch# for Each Well If itmp > 0
!       -------------------------------------------
        else

		  if(itmp .eq. 0) then
		  ! itmp = 0 corresponds to no wells.
	        go to 100
	      end if
	      do I = 1,itmp			!when itmp > 0
	         if(RchLoc .eq. 1) then
				read (in,*) irch
				read (in,*) il
				read (in,*) ir
				read (in,*) ic
				read (in,*) q				
	         elseif(RchLoc .eq. 2) then
				read (in,*) irch
				read (in,*) il
				read (in,*) ir
				read (in,*) ic
				read (in,*) q
	         endif 
	         if(q .le. 0) then
	            if(q .eq. 0) then
	               NumZero = NumZero + 1
	            else		! q < 0
		           ! q < 0 corresponds to Extraction wells
	               NumExt = I - NumCells(K) - NumZero
				   ! No of extraction wells in the stress period
	               Nwel(K) = Nwel(K) + 1
	               LocWel(1,K,NumExt) = il
	               LocWel(2,K,NumExt) = ir
	               LocWel(3,K,NumExt) = ic
	               PmpRate(K,NumExt) = -q * togpm
                   if(idbg .eq. 'Y' .or. idbg .eq. 'y') then
	                 write(iout,3)(LocWel(J,K,NumExt),J=1,3),PmpRate(K,NumExt)
	               endif
	            endif
	         else			! q > 0
	            NumInj = I - Nwel(K) - NumZero
				! No. of injection cells in the stress period
				NumCells(K) = NumCells(K) + 1
				RchRate(K,NumInj) = q * togpm
				! This is storing the basin number in binary form?
				!read(irch,'(i10)') BasinNum(K,NumInj) = irch
				if(idbg .eq. 'Y' .or. idbg .eq. 'y') then
				  write(iout,4) RchRate(K,NumInj)
				endif
			end if
		  end do
		end if
        
100   enddo		!loop for stress period

      do K = 1,Nstress
	    read(in,*) NewBasin(K)
	  end do
3     format(3i10,f15.3)
4     format(17x,'Recharge Well',f15.3,i10)
!
! File opened successfully, return
! --------------------------------
!      write(*,'(/1x,''MODFLOW Wel file opened successfully'')')
      close(in)
      return
      end subroutine opnwel
      

      
!*********************************************************************c
      subroutine opnsc(in,iout,filename,idbg,Nrow1,Ncol1,OriNrow1, OriNcol1, &
     &                 Limit1,Limit2,delr,delc,area)
!       This subroutine opens MT3D SCONC array file, reads SCONC arrays,   
!       and calcualtes the combined plume area of RDX and TNT Given        
!         Nrow1 = Number of Rows
!         Ncol1 = Number of Columns
!         Cl_Rdx = Cleanup Limit for Species 1
!         Cl_Tnt = Cleanup Limit for Species 2
!         Delr(Ncol1) = Grid Spacing Along Row
!         Delc(Nrow1) = Grid Spacing Along Column

      implicit None
      character filename*40,fmtin*20,idbg
      integer :: in,iout,Nrow1,Ncol1,OriNrow1, OriNcol1,iread,i,j
      real :: C1(Nrow1,Ncol1),C2(Nrow1,Ncol1),delr(Ncol1),delc(Nrow1)
	  real :: OriC1(OriNrow1,OriNcol1),OriC2(OriNrow1,OriNcol1)
      real :: Limit1,Limit2,area,cnstnt
!
! Open file
! ---------
      area=0

      open(in,file=filename,status='old')
!
! Read SCONC for layer 1 for RDX
! ------------------------------
      read(in,'(i10,f10.0,a20)')iread,cnstnt,fmtin
      do i = 1,OriNrow1
        read(in,fmtin) (OriC1(i,j),j = 1,OriNcol1)
        do j = 1,OriNcol1
          if(cnstnt .ne. 0) OriC1(i,j) = OriC1(i,j) * cnstnt
        end do
      end do
! Added by Eva Sinha
! Creating new grid using the original grid values
	  do i = 1, Nrow1
	    do j = 2, (Ncol1-2)
		  C1(i,j) = OriC1(2*i, 2*j - 1)
		end do
	  end do
	  do i = 1, Nrow1
		  C1(i, 1) = OriC1(2*i, 1)
		  C1(i, Ncol1-1) = OriC1(2*i, (Ncol1-1)*2-2)
		  C1(i, Ncol1) = OriC1(2*i, Ncol1*2 -3)
	  end do
      if(idbg .eq. 'Y' .or. idbg .eq. 'y') then
        write(iout,1)
        do i = 1,OriNrow1
          write(iout,fmtin) (OriC1(i,j),j = 1,OriNcol1)
        end do
      endif
1     format(/27x,'Initial Concentration of RDX in Layer 1 (ug/L)' &
     & /27x,46('-'))
!
! Read SCONC for layer 1 for TNT
! ------------------------------
      read(in,'(i10,f10.0,a20)')iread,cnstnt,fmtin
      do i = 1,OriNrow1
        read(in,fmtin) (OriC2(i,j),j = 1,OriNcol1)
        do j = 1,OriNcol1
          if(cnstnt .ne. 0) OriC2(i,j) = OriC2(i,j) * cnstnt
        end do
      end do
! Added by Eva Sinha
! Creating new grid using the original grid values
	  do i = 1, Nrow1
	    do j = 2, (Ncol1-2)
		  C2(i,j) = OriC2(2*i, 2*j-1)
		end do
	  end do
	  do i = 1, Nrow1
		  C2(i, 1) = OriC2(2*i, 1)
		  C2(i, Ncol1-1) = OriC2(2*i, (Ncol1-1)*2-2)
		  C2(i, Ncol1) = OriC2(2*i, Ncol1*2 -3)
	  end do
      if(idbg .eq. 'Y' .or. idbg .eq. 'y') then
        write(iout,3)
        do i = 1,OriNrow1
          write(iout,fmtin) (OriC2(i,j),j = 1,OriNcol1)
        end do
      endif
3     format(/27x,'Initial Concentration of TNT in Layer 1 (ug/L)' &
     & /27x,46('-'))
!
! Calculate plume area
! --------------------
      do i = 1,Nrow1
        do j = 1,Ncol1
          if(C1(i,j) .gt. Limit1 .or. &
     &       C2(i,j) .gt. Limit2) area = area + delr(j) * delc(i)
        end do
      end do
      if(idbg .eq. 'Y' .or. idbg .eq. 'y') write(iout,4)area
4     format(/5x,'The initial plume area, both RDX and TNT (ft*ft): ', &
     & e15.6)
!
! File opened successfully, return
! --------------------------------
!      write(*,'(/1x,''SCONC array file opened successfully'')')
      close(in)
      return
      end subroutine opnsc


      
!*********************************************************************c
      subroutine opnucn(in,iout,Filename,Numy,Nrow1,Ncol1,Cl,C,Ny)
!       This subroutine opens MT3D UCN file and decides ny which is the
!       year when cleanup occurs.
!         Numy = Number of Simulated Years
!         Nrow1 = Number of Rows
!         Ncol1 = Number of Columns
!         Cl = Cleanup Limit
!         C(Nrow1,Ncol1,Numy) = Concentration in Layer 1
!         Ny = Cleanup Year
          
      implicit None
      character filename*40,text*16
      integer :: in, iout, numy, Nrow1, Ncol1, ny, ntrans, kstp, kper,ilay, iy, i,k &
	  &, j, nc, nr
      real :: cl, c(Nrow1,Ncol1,numy), time2, cmax, temp

! Open File      
!      open (in,file=filename,form='unformatted',status='old')

! Initialize Cleanup Year to 0
      ny=0

! Read Unformatted File

	        
!100   read(in,end=888) ntrans,kstp,kper,time2,text,Ncol1,Nrow1,ilay
	do k = 1,Numy
		cmax=0
		iy=k
 !       read(in,end=888) ((c(i,j,iy),j=1,Ncol1),i=1,Nrow1)
		do i = 1,Nrow1
			do j = 1,Ncol1
				c(i,j,iy) = CONCfrmMT3D(i,j,iy,in)
				if(c(i,j,iy) < 0.0d0) c(i,j,iy)= 0.0 
			end do
		end do

        do i=1,Nrow1
			do j=1,Ncol1
				if(c(i,j,iy) .gt. cmax) cmax=c(i,j,iy)
			end do
        end do
!        write(*,'(/1x,''The max. conc for year # '',i5,f10.3)')iy,cmax
         
            if(abs(max(cmax,cl)/cl-1.) .le. 0.05 .and. ny .le. 0) then
                ny=iy
                exit
             end if

		end do
!     else
!        read(in,end=888) ((temp,j=1,Ncol1),i=1,Nrow1)
!      end if
!      go to 100
!888   do iy=1,numy
!        write(iout,1)iy
!        do ir=1,Nrow1
!          write(iout,2)(c(ir,ic,iy),ic=1,Ncol1)
!        end do
!      end do
!      write(iout,3)ny
!      write(*,3)ny
!1     format(/1x,'The concentration in year: ',i5)
!2     format(10e15.6)
!3     format(/1x,'The cleanup year: ',i5)
      
!      write(*,'(/1x,''MT3D UCN file opened successfully'')')
!888   write(*,'(/1x,''MT3D UCN file opened successfully'')')

      close(in)
      return
      end subroutine opnucn
      
      
      
!*********************************************************************c
      subroutine opnhds(in,iout,Filename,idbg,Nstress,Nlay1,Nrow1,Ncol1,Head)
!       Open MODFLOW Unformatted Head File and Read Head in Layer 1 for 
!       for Each Year.
!         Numy = Number of Simulated Years
!         Nrow1 = Number of Rows
!         Ncol1 = Number of Columns
!         Nstress = Number of Stress Periods
!         Head(Nrow1,Ncol1,Nstress) = Head in Layer 1
          
      implicit none
      character Filename*40,Text*16,idbg
      integer :: in, iout, Nstress, Nlay1, Nrow1, Ncol1, Kper, Kstp, &
     &           Ilay, I, J, K
      real :: Pertim, Totim, Head(Nrow1,Ncol1,Nstress), Temp

!     Open File      
!     open (in,file=filename,form='unformatted',status='old')

!     Read Head Only for Layer 1 and Skip Other Layers
!100  read(in,End=888) Kstp,Kper,Pertim,Totim,Text,Ncol1,Nrow1,Ilay
      !if(Ilay .eq. 1) then
        !read(in,End=888) ((
		do Kper = 1,Nstress
		 do I = 1,Nrow1
		  do J = 1,Ncol1
				Head(I,J,Kper) = HEADfrmMODF(I,J,Kper)
                if(Head(I,j,Kper) .lt.0 .and. Head(I,J,Kper) .ne. -999 .and. abs(Head(I,J,Kper)) .gt. 0.01)then
                      Head(I,j,Kper) = botelev(i,j) + 150
                      drycell_h = drycell_h + 1
				end if
		  end do
		 end do 
		end do
		!,J=1,Ncol1),I=1,Nrow1)
      !else
        !read(in,End=888) ((Temp,J=1,Ncol1),I=1,Nrow1)
      !endif
      !goto 100

!     Print Head to Output File Or Not?
888   if(idbg .eq. 'Y' .or. idbg .eq. 'y') then
        do K = 1,Nstress
          write(iout,1) K
          do I = 1,Nrow1
           write(iout,2)(Head(I,J,K),J = 1,Ncol1)
          end do
        end do
      endif
1     format(/27x,'The Layer 1 Head in Stress Period ',i2/27x,36('-'))
2     format(10f15.3)
      
!      write(*,'(/1x,''MODFLOW Unformatted Head file opened '', &
!     & ''successfully'')')
      close(in)
      return
      end subroutine opnhds
      
      
      
      
!**********************************************************************
      subroutine rd1darr(in,n,array)
!       This subroutine reads MODFLOW 1D array.
!         Array(N)

      implicit None
      character fmtin*20
      integer :: in, n, locat, i
      real :: array(n), cnstnt
      
      read(in,'(i10,f10.0,a20)')locat,cnstnt,fmtin
      if(locat .gt. 0) then
	read(in,fmtin) (array(i),i=1,n)
      elseif(locat .eq. 0) then
	do i=1,n
	  array(i)=cnstnt
	end do
	return
      else
	read(unit=-locat) (array(i),i=1,n)
      end if
      
      if(cnstnt .ne. 0) then
        do i=1,n
          array(i)=cnstnt*array(i)
        end do
      end if

      return
      end subroutine rd1darr



!**********************************************************************
      subroutine CountNewWell (LocWel, Nwel, MxWel1, NewWell, Nstress)
!      Determine How Many New Extraction Wells Are Installed in Each Stress Period
!        Given:
!        LocWel(3,Nstress,MxWel1) = L,R,C of each well in each stress period
!        Nwel(Nstress) = Number of Extraction Wells in each stress period
!      Store Results in NewWell(Nstress)
!      Don't Count if one of 4 Original Wells, Given by LocOrig(3,4)

        implicit none
        integer :: NumOrig =  4, LocOrig(3,4)
        integer :: MxWel1, Nstress, Nwel(Nstress), LocWel(3,Nstress,MxWel1),  &
     &    NewWell(Nstress), Lay,Row, Col, K, I, KK, II

        LocOrig = reshape ( (/ 1,83,84, 1,60,65, 1,53,59, 1,85,86 /), &
     &    (/3,4/) )
!      Initialize NewWell to 0
        NewWell(:) = 0

!      Step Through Each Stress Period, Each Well
        do K = 1, Nstress
          do I = 1, Nwel(K)

!          Store Location of This Well
            Lay = LocWel(1,K,I)
            Row = LocWel(2,K,I)
            Col = LocWel(3,K,I)

!          See if it an original
            do II = 1, NumOrig
              if (Lay.eq.LocOrig(1,II) .and. Row.eq.LocOrig(2,II) .and. &
     &          Col.eq.LocOrig(3,II)) goto 100
            enddo

!          See if it was used in prior Stress Period
            if (K.eq.1) then
              NewWell(K) = NewWell(K) + 1
              goto 100
            endif
            do KK = 1, K-1
              do II = 1, Nwel(KK)
                if (Lay.eq.LocWel(1,KK,II) .and. Row.eq.LocWel(2,KK,II) &
     &          .and. Col.eq.LocWel(3,KK,II)) goto 100
              enddo
            enddo

!          No Match, Add to NewWell
            NewWell(K) = NewWell(K)+1

  100     enddo		! loop for number of wells in that stress period
        enddo		! loop for stress period

        return
      end subroutine CountNewWell

!**********************************************************************
      subroutine TotalPumping(Nwel, PumpRate, MxWel1, Nstress, Qtotal)
!      Adds all Pumping in Each Stress Period, Stores in Qtotal(Nstress)
!      Nwel(Nstress) = Number of Wells in Each Stress Period
!      PumpRate(Nstress,MxWel1) = Each Well's Pumping Rate, Each Period

        implicit none
        integer :: MxWel1, Nstress, Nwel(Nstress), K, I
        real :: PumpRate(Nstress,MxWel1), Qtotal(Nstress)

!      Initialize Qtotal to 0.0
        Qtotal(:) = 0.0

!      Step Through Each Stress Period, Each Well
        do K = 1, Nstress
          if (Nwel(K) .gt. 0) then
            do I = 1, Nwel(K)

!          Add This Wells Pumping Rate to Qtotal
              Qtotal(K) = Qtotal(K) + PumpRate(K,I)
            enddo
          endif
        enddo

        return
      end subroutine TotalPumping



!**********************************************************************
      subroutine CalcNewGac (Qtotal, Alpha, NewGac, Nstress)
!        Determine the Number of New GAC Units Required in Each Stress
!        Period to Handle Pumping Stored in Qtotal(Nstress).  Alpha is the
!        Fraction of Time Pumps Are Operated (The Operational Rate is
!        Qtotal/Alpha).  Results in NewGac(Nstress).
!        Originally, 2 Units are Required; Max Operational Pumping is
!        1300 gpm.  An additional GAC unit for each additional 325 gpm.

        implicit none
        real, parameter :: GacCapacity = 325.0
        real, parameter :: InitialGac = 1300
        integer :: Nstress, NewGac(Nstress), NumGac, RequiredGac, K
        real :: Qtotal(Nstress), Alpha, ExtraQ

!      Initialize Number of New GACs
        NumGac = 0

!      Step Through Stress Periods
        do K = 1, Nstress

!        Calculate Additional Pumping Rate
          ExtraQ = Qtotal(K)/Alpha - InitialGac

!        See if More Are Needed, Set NewGac(K)
          RequiredGac = Ceiling (ExtraQ / GacCapacity)
          if (RequiredGac .gt. NumGac) then
            NewGac(K) = RequiredGac - NumGac
            NumGac = RequiredGac
          else
            NewGac(K) = 0
          endif

        enddo

        return
      end subroutine CalcNewGac 
!***********************************************************************
      subroutine PmpCapConstr(Nstress, Nwel, PmpRate, WelZon, MxWel1, Alpha,CountPmpCap)
!       Check the Pumping Capacity Constraint Given
!         Nstress = Number of Stress Period
!         Nwel(Nstress) = Number of Wells in Each Stress Period
!         PmpRate(Nstress,MxWel1) = Pumping Rate Each Period, Each Well
!         WelZon(Nstress,MxWel1) = Well Zone Number Each Period, Each Well
!         MxWel1 = Maximum Number of Wells in Each Period
!         Alpha = Fraction of Time Pumps Operate
!       The Results Stored in CountPmpCap(Nstress), Number of Wells which 
!       Cosntraint Not Satisfied

        implicit none
        integer :: Nstress, MxWel1, Nwel(Nstress), WelZon(Nstress,MxWel1) 
        integer :: CountPmpCap(Nstress), K, J
        real :: PmpRate(Nstress,MxWel1), Alpha, Q

!       Initialize to 0
        do K = 1,Nstress
          CountPmpCap(K) = 0
        enddo

!       Step Through Stress Periods K
        do K = 1,Nstress

!         Step Through Wells in This Period
          do J = 1,Nwel(K)

!           Get Pumping Rate for This Well
            Q = PmpRate(K,J)

!           Check Well Zone Number and Pumping Rate. If Not Satisfied,
!           Increase counting
            if((WelZon(K,J) .eq. 1 .and. (Q/Alpha) .gt. 400) .or. &
     &         (WelZon(K,J) .ne. 1 .and. (Q/Alpha) .gt. 1000)) then
              CountPmpCap(K) = CountPmpCap(K) + 1
            endif

          enddo
        
        enddo

        return
      end subroutine PmpCapConstr



!***********************************************************************
      subroutine PmpRchConstr(Nstress, PmpQtotal, RchQtotal, PmpRchSP)
!       Check Pumping-Recharge Balance Constraint Given
!         Nstress = Number of Stress Period
!         PmpQtotal(Nstress) = Total Pumping Rate, Each Period
!         RchQtotal(Nstress) = Total Recharge Rate, Each Period
!       The Results Stored in PmpRchSP(Nstress), Stress Period Number when
!       Constraint Not Satisfied

        implicit none
        integer :: Nstress, PmpRchSP(Nstress), K 
        real :: PmpQtotal(Nstress), RchQtotal(Nstress)

!       Initialize to 0
        do K = 1,Nstress
          PmpRchSP(K) = 0
        enddo

!       Step Through Each Period, Check the Constraint
        do K = 1,Nstress
          if(abs(PmpQtotal(K) - RchQtotal(K)) .gt. 1) then
            PmpRchSP(K) = K
          endif
        enddo

        return
      end subroutine PmpRchConstr



!***********************************************************************
      subroutine BufZonConstr(Nrow1,Ncol1,Nstress,MxYear,Buff,C,Limit, &
     &  CountBuf,ExceedCell)
!       Check Buffer Zone Constraint Given
!         Nrow1 = Number of Rows
!         Ncol1 = Number of Columns
!         Nstress = Number of Stress Period
!         MxYear = Maximum Number of Years
!         Buff(Nrow1,Ncol1) = Buffer Zone ID for Each Cell
!         C(Nrow1,Ncol1,MxYear) = Concentration
!         Limit = Cleanup Limit
!       The Results Stored in CountBuf(Nstress), Number of Cells which 
!       Constraint Not Satisfied

        implicit none
        integer :: Nrow1, Ncol1, Nstress, MxYear, Buff(Nrow1,Ncol1)
        integer :: CountBuf(Nstress), K, I, J, Year
        integer :: ExceedCell(Nrow1,Ncol1,Nstress)
        real :: C(Nrow1,Ncol1,MxYear), Limit

!       Initialize to 0
        do K = 1,Nstress
          CountBuf(K) = 0
          do I = 1,Nrow1
            do J = 1,Ncol1
              ExceedCell(I,J,K) = 0
            enddo
          enddo
        enddo

!       Step Through Each Period
        do K = 1,Nstress

!         Step Through Each Cell in This Period
          do I = 1,Nrow1
            do J = 1,Ncol1

!             Year at the End of This Period
              Year = K * 5
              
!             If C > Limit in Buffer Zone 0, Increase the Counting
!             and Locate the Cell
              if(Buff(I,J) .eq. 0 .and. C(I,J,Year) .gt. Limit) then
                CountBuf(K) = CountBuf(K) + 1
                ExceedCell(I,J,K) = 1
              endif
              

            end do
          end do
          
        enddo

        return
      end subroutine BufZonConstr



!*********************************************************************c
      subroutine TotalMass(C1, C2, Delr, Delc, Head, BotElev,Ncol1, Nrow1,  &
     &  Nstress, MxYear, PlumeMass)
!       Calculate the Total Mass of a Plume in Layer 1
!         C1 & C2 have Dimensions (Nrow1,Ncol1,MxYear) 
!         Delr(Ncol1) is Spacing in X, Delc(Nrow1) is Spacing in Y
!         Head(Nrow1,Ncol1,Nstress) = Simulated Head of Layer 1
!         BotElev(Nrow1,Ncol1) = Bottom Elevation of Layer 1
!       The Results Stored in PlumeMass(MxYear)

        implicit none
        real, parameter :: RDXFactor = 0.668, TNTFactor = 13.2
        integer :: Ncol1, Nrow1, Nstress, MxYear
        integer :: StartYear, EndYear, Year, I, J, K
        real :: C1(Nrow1,Ncol1,MxYear), C2(Nrow1,Ncol1,MxYear), &
     &    Head(Nrow1,Ncol1,Nstress), BotElev(Nrow1,Ncol1), &
     &    Delr(Ncol1), Delc(Nrow1), PlumeMass(MxYear), VolCell
     
!       Initialize Mass to Zero
        do Year = 1, MxYear
          PlumeMass(Year) = 0.0
        enddo

!       Step Through Stress Periods
        do K = 1, Nstress

!       Calculate Starting and Ending Years of This Period
          StartYear = (K-1)*5+1
          EndYear = K*5
          
!         Step Through Years of This Period
          do Year = StartYear, EndYear

!           Step Through Cells
            do I = 1, Nrow1
              do J = 1, Ncol1

!               Calculate Volume of This Cell
                VolCell = Delc(I) * Delr(J) *  &
     &                    (Head(I,J,K) - BotElev(I,J))
            
!               Add This Cell's Mass If C1 > 0
                if (C1(I,J,Year) .gt. 0.0) then
                  PlumeMass(Year) = PlumeMass(Year) +  &
     &                    C1(I,J,Year) * VolCell * RDXFactor * 2.832e-8
                endif

!               Add This Cell's Mass If C2 > 0
                if (C2(I,J,Year) .gt. 0.0) then
                  PlumeMass(Year) = PlumeMass(Year) + &
     &                    C2(I,J,Year) * VolCell * TNTFactor * 2.832e-8
                endif

              enddo
            enddo
        
          !write(*,1) Year,K,PlumeMass(Year)
1         format(/1x,'Year: ',i2,3x,'Stress Period: ',i2,3x, &
     &     'Total Mass Remaining in Layer 1:',e15.6)

          enddo
        enddo

        return
      end subroutine TotalMass
!**********************************************************************
      subroutine CCWfunc(PeriodEw2, NewWell, Rate, Nstress, CCWf)
!      Calculate Capital Cost of Wells Given:
!        PeriodEw2 = Stress period when Well EW-2 Begins Pumping [0,Nstress]
!        NewWell(Nstress) = Number of New Wells installed Each Stress Period
!        Rate = Discount Rate
!        Nstress = Number of Stress Periods Before Cleanup
!      Parameters:
!        Ew2Cost = Cost to Begin Operation at EW-2
!        NewCost = Cost to Add a New Well

        implicit none
        real, parameter :: Ew2Cost = 25.0, NewCost = 75.0
        integer :: Nstress, PeriodEw2, NewWell(Nstress), Year, NewWell1, K
        real :: Rate, CapCostEw2, CapCostNew, CCWf
        real :: Pv

!      Initialize CCW to 0
        CCWf = 0

!      Calc Cost for EW-2
        if (PeriodEw2 .gt. 0 .and. PeriodEw2 .le. Nstress) then
          Year = (PeriodEw2-1)*5+1
          CapCostEw2 = Ew2Cost / ((1+Rate)**(Year-1))
          CCWf = CCWf + CapCostEw2
        endif

!      Calc Cost for Adding Wells
        do K = 1, Nstress
          NewWell1 = NewWell(K)
          if(NewWell1 .le. 0) cycle
          Year = (K-1)*5+1
          CapCostNew = NewCost*NewWell1 / ((1+Rate)**(Year-1))
          CCWf = CCWf + CapCostNew
        enddo

        return
      end subroutine CCWfunc



!**********************************************************************
      subroutine CCBfunc (NewBasin, Rate, Nstress, CCBf)
!      Calculate Capital Cost of New Recharge Basins Given:
!        NewBasin(Nstress) = Number of new Basins each Stress Period
!        Rate = Discount Rate
!        Nstress = Number of Stress Periods Before Cleanup
!      Parameters:
!        CostBasin = Cost to Construct a Single Basin

        implicit none
        real, parameter :: CostBasin = 25.0
        integer :: Nstress, NewBasin(Nstress), K, Year, NewBasin1
        real :: Rate, CapCostBasin, CCBf
        real :: Pv

!      Initialize CCB to 0
        CCBf = 0.0

!      Calc Cost for New Basins Each Stress Period
        do K = 1, Nstress
          NewBasin1 = NewBasin(K)
          if (NewBasin1 .le. 0) cycle
          Year = (K-1)*5+1
          CapCostBasin =  CostBasin*NewBasin1 / ((1+Rate)**(Year-1))
          CCBf = CCBf + CapCostBasin
        enddo

        return
      end Subroutine CCBfunc



!**********************************************************************
      subroutine CCGfunc(NewGac, Rate, Nstress, CCGf)
!      Calculate Capital Cost of New GAC Treatment Units Given:
!        NewGac(Nstress) = Number of new GACs each Stress Period
!        Rate = Discount Rate
!        Nstress = Number of Stress Periods Before Cleanup
!      Parameters:
!        CostGac = Cost to Add a GAC

        implicit none
        real, parameter :: CostGac = 150.0
        integer :: Nstress, NewGac(Nstress), K, Year, NewGac1
        real :: Rate, CapCostGac, CCGf
        real :: Pv

!      Initialize CCG to 0
        CCGf = 0.0

!      Calc Cost for New GACs Each Stress Period
        do K = 1, Nstress
          NewGac1 = NewGac(K)
          if (NewGac1 .le. 0) cycle
          Year = (K-1)*5+1
          CapCostGac =  CostGac*NewGac1 / ((1+Rate)**(Year-1))
          CCGf = CCGf + CapCostGac
        enddo

        return
      end subroutine CCGfunc



!**********************************************************************
      subroutine PvAnnuity (Ny, AnnualCost, Rate, PvAn)
!      Calculate the Present Value of a Fixed Yearly Cost Given:
!        Ny = Number of Years
!        AnnualCost = Annual Cost
!        Rate = Discount Rate

        implicit none
        integer :: Ny, K
        real :: AnnualCost, Rate, CostThisYear, PvAn
        real :: Pv

!      Initialize PvAnnuity to 0
        PvAn = 0.0

        do K = 1, Ny
          CostThisYear =  AnnualCost / ((1+Rate)**(K-1))
          PvAn = PvAn + CostThisYear
        enddo

        return
      end subroutine PvAnnuity



!**********************************************************************
      subroutine VCEfunc (Ny, Nwel, PumpRate, MxWel1, Rate, Nstress, &
     &  Alpha, VCEf)
!      Calculate Variable Electrical Costs for Pumping Given:
!        Ny = Number of Years to Cleanup
!        Nwel(Nstress) = Number of Extraction Wells in Each Stress Period
!        PumpRate(Nstress,MxWel1) = Pumping Rate (gpm) Each Period, Every Well
!        MxWel1 = Maximum Number of Pumping Wells
!        Rate = Discount Rate
!        Nstress = Total Number of Stress Period
!        Alpha = Fraction of time pumps operate
!      Uses a Cost vs. Pumping Rate Function

        implicit none
        integer :: Ny, MxWel1, Nstress, Nwel(Nstress), K, I, StartYear, &
     &    EndYear, Year, Nstress_Cleanup
        real :: PumpRate(Nstress,MxWel1), Rate, Cost, Alpha, VCEf
        real :: ElecCost, Pv, RealNum

!      Initialize VCE = 0
        VCEf = 0.0

!      Calculate Number of Stress Period When Cleanup
        RealNum = Real(Ny) / 5.0
        Nstress_Cleanup = Ceiling(RealNum)

!      Step Through Stress Periods K, Calc Start & End Year
        do K = 1, Nstress_Cleanup
          StartYear = (K-1)*5+1
          EndYear = Min(Ny,StartYear+4)

!        Step Through Wells in this Period
          do I = 1, Nwel(K)

!         Calculate Annual Elec Cost for This Well
!            Note, Pass Pump Rate / Alpha to the Function
             if ( (PumpRate(K,I)/Alpha) .le. 400.0) then
               Cost = 0.01 * (PumpRate(K,I)/Alpha)
             else
               Cost = 0.025 * (PumpRate(K,I)/Alpha) - 6.0
             endif
            !Cost = ElecCost (PumpRate(K,I)/Alpha)

!          Sum PV of Elec Cost for Whole Stress Period
            do Year = StartYear, EndYear
              VCEf = VCEf +  Cost / ((1+Rate)**(Year-1))
            enddo
          enddo
        enddo

        return
      end subroutine VCEfunc



!**********************************************************************
      subroutine VCGfunc (Ny, Nwel, PumpRate, MxWel1, WellConc, Rate,&
     &  Nstress, MxYear, VCGf)
!      Calculate Variable Costs for Changing GAC Units Given:
!        Ny = Number of Years to Cleanup
!        Nwel(Nstress) = Number of Extraction Wells in Each Stress Period
!        PumpRate(Nstress,MxWel1) = Pumping Rate (gpm) Each Period, Every Well
!        MxWel1 = Maximum Number of Pumping Wells
!        WellConc(MxYear,MxWel1) = Total Concentration (ug/L RDX + TNT)
!          Each Year, Every Well
!        Rate = Discount Rate
!        Nstress = Number of Stress Periods
!        MxYear = Maximum Number of Years
!      Uses the "GammaFunc" Function to Get Cost Per Kg Mass Removed

        implicit none
        integer :: Ny, MxWel1, Nstress, MxYear, Nwel(Nstress), K, I, StartYear, &
     &    EndYear, Year, Nstress_Cleanup
        real :: PumpRate(Nstress,MxWel1), WellConc(MxYear,MxWel1), Rate, &
     &    Cost, MassRemoved, TotalPumping, InfluentConc, VCGf
        real :: GammaFunc, Pv, RealNum

!      Initialize VCG = 0
        VCGf = 0.0

!      Calculate Number of Stress Period When Cleanup
        RealNum = Real(Ny) / 5.0
        Nstress_Cleanup = Ceiling(RealNum)
      
!      Step Through Stress Periods K, Calc Start & End Year
        do K = 1, Nstress_Cleanup
          StartYear = (K-1)*5+1
          EndYear = Min(Ny,StartYear+4)

!        Step Through Each Year in This Period
          do Year = StartYear, EndYear

!          Initialize Accumulators
            MassRemoved = 0.0
            TotalPumping = 0.0

!          Step Through Wells in this Period
            do I = 1, Nwel(K)

!            Add Terms to Accumulators
              TotalPumping = TotalPumping + PumpRate(K,I)
              MassRemoved = MassRemoved + PumpRate(K,I) * &
     &          WellConc(Year,I)
            enddo

!          Calculate Influent Concentration (ug/L)
            if (TotalPumping .le. 0.0) then
              InfluentConc = 0.0
            else
              InfluentConc = MassRemoved / TotalPumping
            endif

!          Convert Mass Removed from gpm*ug/L to kg/yr
            MassRemoved = MassRemoved * 1.989E-3

!         Calculate Cost For This Year
            Cost = 0.001 * (225.0 - 0.5*InfluentConc) * MassRemoved
            if (cost .lt. 0.0) Cost = 9999.99   ! if there is a dry cell make the cost too high
			 !GammaFunc = 0.001 * (225.0 - 0.5*C)

!          Add PV of Cost to Total
            VCGf = VCGf + Cost / ((1+Rate)**(Year-1))

          enddo
        enddo

        return
      end subroutine VCGfunc 


!**********************************************************************
       subroutine VCSfunc (Ny, PlumeArea, Rate, Nstress, VCSf)
!        Calculate Variable Costs for Sampling Given:
!        Ny = Number of Years to Cleanup
!        PlumeArea(Nstress) = Area of Plume at Beginning Each Period
!        Rate = Discount Rate
!        Nstress = Number of Stress Period
!        MxYear = Maximum Number of Years
!        Parameters:
!        BaseSampCost = Baseline Sampling Cost

        implicit none
        real, parameter :: BaseSampCost = 150.0
        integer :: Ny, Year, Period, Nstress
        real :: PlumeArea(Nstress), Rate, InitialArea, CostThisYear, VCSf
        real :: Pv

!       Initialize VCS = 0
        VCSf  = 0.0

!       Set Initial Area
        InitialArea = PlumeArea(1)

!       Step Through Each Year
        do Year = 1, Ny

!         Calculate Current Stress Period
          Period = int((Year-1)/5) +1

!         Calculate Cost This Year From Plume Area
          CostThisYear = BaseSampCost * PlumeArea(Period) / InitialArea
          
!         Add PV to VCS
          VCSf = VCSf +  CostThisYear / ((1+Rate)**(Year-1))
        enddo

        return
      end subroutine VCSfunc



!***********************************************************************
      subroutine AreaPlume2(C1, C2, Delr, Delc, Ncol1, Nrow1, Limit1, &
     &  Limit2, MxYear, Year, AreaPm2)
!       Calculate the Area of a Plume Where EITHER C1 > Limit OR
!         C2 > Limit2 for Year
!       C1 & C2 have Dimensions (Nrow1,Ncol1,MxYear) 
!      Delr(Ncol1) is Spacing in X, Delc(Nrow1) is Spacing in Y

        implicit none
        integer :: Ncol1, Nrow1, MxYear, Year, I, J
        real :: C1(Nrow1,Ncol1,MxYear), C2(Nrow1,Ncol1,MxYear), &
     &    Delr(Ncol1), Delc(Nrow1), Limit1, Limit2, AreaPm2

!       Initialize Area to Zero
        AreaPm2 = 0.0

!       Step Through Cells
        do I = 1, Nrow1
          do J = 1, Ncol1

!           Skip if C1 or C2 below or equal to Limits
            if (C1(I,J,Year) .gt. Limit1 .or. &
     &          C2(I,J,Year) .gt. Limit2) then

!             Add This Cell's Area
              AreaPm2 = AreaPm2 + Delc(I) * Delr(J)

            endif
          enddo
        enddo

        return
      end subroutine AreaPlume2

!***********************************************************************
      subroutine Ew2StartPeriod (LocWel, Nwel, MxWel1, Nstress, Ew2StartPer)
!       Determine Which Period Ew2 Starts
!         Given:
!         LocWel(3,Nstress,MxWel1) = L,R,C of each well in each stress period
!         Nwel(Nstress) = Number of Extraction Wells in each stress period

        implicit none
        integer, parameter :: Ew2Lay = 1, Ew2Row = 83, Ew2Col = 84
        integer :: MxWel1, Nstress, K, I
        integer :: Nwel(Nstress), LocWel(3,Nstress,MxWel1), Ew2StartPer

!       Step Through Each Stress Period, Each Well
        do K = 1, Nstress
          do I = 1, Nwel(K)

!           See if This One is EW2
            if (Locwel(1,K,I).eq.Ew2Lay .and. LocWel(2,K,I).eq.Ew2Row &
     &          .and. LocWel(3,K,I).eq.Ew2Col) then
              Ew2StartPer = K
              return
            endif
          enddo
        enddo

!       Set Ew2StartPeriod to 0 (Never on)
        Ew2StartPer = 0
        return

      end subroutine Ew2StartPeriod
!***************************************************************************** 
!	  Added by Eva Sinha
!     creating new grid values using the original grids
      subroutine CreateNewGrid (Nrow1, Ncol1, Delr, Delc, PmpZon, Buff, BotElev, &
	  &                   OriNrow1, OriNcol1,OriDelr, OriDelc, OriPmpZon, OriBuff, OriBotElev)
	  
	    implicit none
		integer :: Nrow1, Ncol1, OriNrow1, OriNcol1
		real :: TotDelc, TotDelr
		integer :: PmpZon(Nrow1, Ncol1), Buff(Nrow1, Ncol1)
		integer :: OriPmpZon(OriNrow1, OriNcol1), OriBuff(OriNrow1, OriNcol1)
		integer :: i,j
		real :: Delr(Ncol1), Delc(Nrow1), BotElev(Nrow1, Ncol1)
		real :: OriDelr(OriNcol1), OriDelc(OriNrow1), OriBotElev(OriNrow1, OriNcol1)

		do i = 1, Nrow1
		  do j = 2, (Ncol1-2)
		    PmpZon(i,j) = OriPmpZon(2*i, 2*j-1 )
			Buff(i,j) =  OriBuff(2*i, 2*j-1 )
			BotElev(i,j) =  OriBotElev(2*i, 2*j-1 )
		  end do
		end do
		do i = 1, Nrow1
!         entering the values for column 1
		  PmpZon(i, 1) = OriPmpZon(2*i, 1)
		  Buff(i, 1) = OriBuff(2*i, 1)
		  BotElev(i, 1) = OriBotElev(2*i, 1)
!         entering the values for column NCol1-1
		  PmpZon(i, Ncol1-1) = OriPmpZon(2*i, (Ncol1-1)*2-2)
		  Buff(i, Ncol1-1) = OriBuff(2*i, (Ncol1-1)*2-2)
		  BotElev(i, Ncol1-1) = OriBotElev(2*i, (Ncol1-1)*2-2)
!         entering the values for column NCol
		  PmpZon(i, Ncol1) = OriPmpZon(2*i, Ncol1*2 - 3)
		  Buff(i, Ncol1) = OriBuff(2*i, Ncol1*2 - 3)
		  BotElev(i, Ncol1) = OriBotElev(2*i, Ncol1*2 - 3)
		end do
		do i = 1, Nrow1
		  Delc(i) = OriDelc(2*i - 1) + OriDelc(2*i)
		end do
		do i = 2, (Ncol1-2)
		  Delr(i) = OriDelr(2*i - 1) + OriDelr(2*i - 2)
		end do
		Delr(1) = OriDelr(1)					! entering the value for column 1
		Delr(Ncol1-1) = OriDelr(2*(Ncol1-1)-2)	! entering the value for column Ncol1-1
		Delr(Ncol1) = OriDelr(2*Ncol1-3)		! entering the value for column Ncol
!
!     Check
        TotDelc = 0
		TotDelr = 0
		do i = 1, Nrow1
		  TotDelc = TotDelc + Delc(i)
		end do
		do i = 1, Ncol1
		  TotDelr = TotDelr + Delr(i)
		end do
	  return
	  end subroutine
!**************************************************************************
!Subroutine for calculating the violation for RDX or TNT
subroutine Violation(C, NRow1, NCol1,Numy, Violate, C_Limit, MxYr1)
  
  implicit none
  save
  
  real, intent(inout) :: Violate
  integer :: NRow1, NCol1, Numy, i,j, MxYr1
  real :: C(Nrow1,Ncol1,MxYr1), C_Limit
  real :: cmax
  
  cmax = 0
 
 cmax = maxval(c(:,:,Numy))

   	write (*,*) 'cmax = ', cmax

  if (cmax .gt. C_Limit) then
     Violate = (cmax/C_Limit - 1)
  else
     Violate = 0
  end if
  return
end subroutine  Violation
!*****************************************************************************

!*****************************************************************************

!      real function Pv (Rate, Year, Cost)
!      Calculate the Present Value of a Cost given the Year Incurred and
!        the Discount Rate

!        implicit none
!        real :: Rate, Cost
!        integer :: Year

!      Calculate Present Value
!        Pv = Cost / ((1+Rate)**(Year-1))
!        return
!      end function Pv

!**********************************************************************
!      real function ElecCost(Q)
!      Calculate the Electrical Cost Due to a Well Pumping at Rate
!        of Q (gpm)

!        implicit none
!        real :: Q 

!      Calculate Cost
!        if (Q .le. 400.0) then
!          ElecCost = 0.01 * Q
!        else
!          ElecCost = 0.025 * Q - 6.0
!        endif

!      end function ElecCost

!**********************************************************************
!      real function GammaFunc(C)
!      Calculate Cost per Kg Mass Removed Based on Average Influent
!        Concentration, C

!        implicit none
!        real :: C

!      Calculate GammaFunc
!        GammaFunc = 0.001 * (225.0 - 0.5*C)

!        return
!      end function GammaFunc

end subroutine Obj_Func_Coarse




