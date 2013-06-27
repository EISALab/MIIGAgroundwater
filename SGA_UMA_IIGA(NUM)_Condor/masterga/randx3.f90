module randx3
implicit none

	double precision :: mbig=8800000.d0,mseed=1618033.d0,mz=0.d0,fac=1./8800000.d0
	double precision :: mj,mk,ma

	integer :: iff,inext,inextp
	dimension ma(55)
	data iff /0/

contains

subroutine save_ran3( hFile )
implicit none
	integer, intent(in) :: hFile
	
	write( hFile, * ) mbig, mseed, mz, fac, mj, mk
	write( hFile, * ) iff, inext, inextp
	write( hFile, * ) ma
end subroutine save_ran3

subroutine load_ran3( hFile )
implicit none
	integer, intent(in) :: hFile
	
	read( hFile, * ) mbig, mseed, mz, fac, mj, mk
	read( hFile, * ) iff, inext, inextp
	read( hFile, * ) ma
end subroutine load_ran3

!##################################################################################
!Returns a uniform rand_xom deviate between 0.0 and 1.0.  Set idum to 
!any negative value to initialize or reinitialize the sequence.
!This function is taken from W.H. Press', "Numerical Recipes" p. 199.!

subroutine ran3(idum,rand_x)
	IMPLICIT none
	save

	double precision :: rand_x
	integer :: idum

	integer :: i, ii, k

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

end module randx3