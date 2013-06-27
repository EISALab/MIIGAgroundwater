module STD_HELPER
implicit none

logical, parameter :: debug_on = .true.

contains

subroutine OpenInputFile( hFile, strFile )
implicit none
	integer, intent(in) :: hFile
	character(*), intent(in) :: strFile
	integer :: ioerr
	open( unit=hFile, file=strFile, status='old', iostat=ioerr )
	if( ioerr/=0 )then
		print *, strFile, ' open error!'
		stop
	endif
end subroutine OpenInputFile

subroutine OpenOutputFile( hFile, strFile )
implicit none
	integer, intent(in) :: hFile
	character(*), intent(in) :: strFile
	integer :: ioerr
	open( unit=hFile, file=strFile, status='unknown', iostat=ioerr )
	if( ioerr/=0 )then
		print *, strFile, ' open error!'
		stop
	endif
end subroutine OpenOutputFile

subroutine SwapInt( a, b )
implicit none
	integer, intent(inout) :: a, b
	!variable
	integer :: t
	t = a
	a = b
	b = t
end subroutine SwapInt

subroutine SwapDouble( a, b )
implicit none
	double precision, intent(inout) :: a, b
	!variable
	double precision t
	t = a
	a = b
	b = t
end subroutine

subroutine assert( f )
implicit none
	logical, intent(in) :: f

	if( debug_on )then
		if( .not. f )then
			print*, 'assertion failed\n'
			stop
		endif
	endif
end subroutine assert

!##################################################################################
!Returns a uniform random deviate between 0.0 and 1.0.  Set idum to 
!any negative value to initialize or reinitialize the sequence.
!This function is taken from W.H. Press', "Numerical Recipes" p. 199.!

double precision function UniRand( idum )
implicit none
save
	!argument
	optional :: idum
	integer :: idum
	
	!variable
	integer :: i, iff,ii,k,inext,inextp
	double precision :: mbig=4000000.d0,mseed=1618033.d0,mz=0.d0,fac=1./4000000.d0
	double precision :: mj,mk,ma

	dimension ma(55)

	data iff /0/

	if( present(idum) )then
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
	endif

	inext=inext+1
	if(inext.eq.56) inext=1
	inextp=inextp+1
	if(inextp.eq.56) inextp=1
	mj=ma(inext)-ma(inextp)
	if(mj.lt.mz) mj=mj+mbig
	ma(inext)=mj
	UniRand=mj*fac

end function UniRand

end module STD_HELPER