!  gasurr.f90 
!
!  FUNCTIONS:
!	gasurr      - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: gasurr
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

	program surrga
	use surrfit

	implicit none

	! Variables


	call PreSurrgate
	call DoSurrFitFunc
	call PostSurrgate

	end program surrga

