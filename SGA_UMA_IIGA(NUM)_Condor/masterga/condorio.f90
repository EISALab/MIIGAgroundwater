module condorio
use condor_helper
use std_helper
implicit none

integer, parameter :: hSurrBack	= 1500
integer, parameter :: hSurrGo	= 1501

character(*), parameter :: strSurrGo	= 'surrgo.dat'
character(*), parameter :: strSurrBack	= 'surrbak.dat'
character(*), parameter :: strFileList	= '../sendfiles.dat'

character(*), parameter :: strSlave		= 'slave'
character(*), parameter :: strStatic	= 'static'
character(*), parameter :: strStopFile	= 'grid/ga.stop'
character(*), parameter :: strRestart	= 'grid/ga.restart'

!logical, parameter :: bLocalRun = .true.
logical, parameter :: bLocalRun = .false.

contains

subroutine MakeStaticFolder
implicit none
	call RemoveDir( strStatic )
	call MakeDir( strStatic )
end subroutine MakeStaticFolder

subroutine PreSlave( nId, nGen )
implicit none
	integer, intent(in) :: nId, nGen

	!variable
	character(80) :: strSlaveFolder

	call MakeSlaveName( strSlave, nId, strSlaveFolder )
	call MakeDir( strSlaveFolder )
	call ChangeDir( strSlaveFolder )

	if( nGen/=-1 )call CopySlaveFiles( strFileList )

end subroutine PreSlave

subroutine WaitForSlaves(arrJobIds, nCount)
implicit none
	!argument
	integer, dimension(*), intent(in) :: arrJobIds
	integer, intent(in) :: nCount

	!variables
	integer :: ret
	logical :: stopflag
	integer :: i

	!clean stop file flag
	ret = RemoveFileC( strStopFile )

	if( bLocalRun )then
		do i=1, nCount
			call PreSlave(arrJobIds(i), -1)
			call WaiteForExec( 'pdefit.exe' )
			call PostSlave
		end do
	else
		call RouteSlaves( arrJobIds, nCount )
		!call WaiteForExec( 'grid/control' )
	endif

	stopflag = .false.
	inquire( FILE=strStopFile, EXIST=stopflag )
	if( stopflag )then
		print*, 'control signals ga to stop, stoppping!'
		stop;
	endif
end subroutine

subroutine PostSlave
implicit none
	
	call ChangeDir( '..' )
end subroutine PostSlave

subroutine BroadcastSlave(individual, generation, gridflagx, gridflagy )
implicit none
	!arugments
	integer, intent(in) :: individual, generation
	integer, intent(in) :: gridflagx, gridflagy

	!variables
	integer :: ret, i

	call PreSlave( individual, int(generation) )

	call OpenOutputFile( hSurrGo, strSurrGo )
	rewind(hSurrGo)

	write( hSurrGo, * )individual, int(generation)
	write( hSurrGo, * )gridflagx, gridflagy

	close( hSurrGo )
	
	!add 5/22/03 to handle crashed salves
	ret = RemoveFileC( strSurrBack )

	call PostSlave
end subroutine BroadcastSlave

subroutine CollectSlave( individual, generation, rFitness, rTotCost, Pen_Rdx,Pen_Tnt,drycell_c, drycell_h )
implicit none
	!argument
	integer, intent(in) :: individual, generation
	double precision, intent(out) :: rFitness
	double precision, intent(out) :: rTotCost, Pen_Rdx, Pen_Tnt
	integer, intent(out) :: drycell_c, drycell_h;

	!variable
	logical :: backexist
	integer :: indiv_read, gen_read, i

	call PreSlave( individual, -1 )

	!add 5/22/03 to handl crashed slave
	inquire( FILE=strSurrBack, EXIST=backexist )
	if( backexist )then
		call OpenInputFile( hSurrBack, strSurrBack )
		rewind( hSurrBack )

		read( hSurrBack, * )indiv_read, gen_read
		read( hSurrBack, * )rFitness, rTotCost
		read( hSurrBack, * )Pen_Rdx, Pen_Tnt
		read( hSurrBack, * )drycell_c, drycell_h

		close( hSurrBack )
	else
		print*, 'warning: surrback file is not found!', individual
		!stop
	endif

	call PostSlave
end subroutine CollectSlave


end module condorio
