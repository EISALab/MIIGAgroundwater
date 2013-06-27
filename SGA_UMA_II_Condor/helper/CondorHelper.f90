module condor_helper
use dfport
implicit none
!this is the module file which contains the necessary file and directory manipulation subroutines
!to create the necessary directories for file transportation in condor system

interface
	double precision function StrToFC( str )
		character(*), intent(in) :: str
	end function StrToFC

	integer function CopyFileC( strSrcFile, strDesFile )
		character(*), intent(in) :: strSrcFile, strDesFile
	end function CopyFileC

	integer function CopyDirC( strSrcPath, strDesPath )
		character(*), intent(in) :: strSrcPath, strDesPath
	end function CopyDirC
	
	integer function RemoveDirC( strPath )
		character(*), intent(in) :: strPath
	end function RemoveDirC

	integer function RemoveFileC( strFile )
		character(*), intent(in) :: strFile
	end function RemoveFileC

	subroutine MakeSlaveName( str, nId, strRet )
		character(*), intent(in) :: str
		integer, intent(in) :: nId
		character(*), intent(out) :: strRet
	end subroutine

	subroutine CopySlaveFiles( strFileList )
		character(*), intent(in) :: strFileList
	end subroutine CopySlaveFiles

	subroutine WaiteForExec( strExec )
		character(*), intent(in) :: strExec
	end subroutine WaiteForExec

	subroutine InitSocket
	end subroutine InitSocket

	subroutine ClearSocket
	end subroutine ClearSocket

	subroutine RefreshHosts
	end subroutine RefreshHosts

	subroutine RouteSlaves( arrJobIds, nJobCount )
		integer, dimension(*), intent(in) :: arrJobIds
		integer, intent(in) :: nJobCount
	end subroutine RouteSlaves
end interface

contains

subroutine ChangeDir( strPath )
implicit none

	character(*), intent(in) :: strPath
	integer :: istatus

	istatus = CHDIR(strPath)
	if( istatus/=0 )then
		print*, 'ERROR:trying to change path to ', strPath, 'failed!'
		stop
	end if
end subroutine ChangeDir

subroutine CopyFile( strSrc, strDes )
implicit none
	character(*), intent(in) :: strSrc, strDes
	integer :: istatus

	istatus = CopyFileC(strSrc, strDes)
	if( istatus/=0 )then
		print*, 'ERROR:copy file ', strSrc, ' to ', strDes, 'failed!'
		stop
	end if
end subroutine CopyFile

subroutine MakeDir( strNewPath )
use dflib
implicit none
	character(*), intent(in) :: strNewPath

	logical(4) :: lResult

	lResult = makedirqq(strNewPath)

end subroutine MakeDir

subroutine CopyDir( strSrcPath, strDesPath )
implicit none
	character(*), intent(in) :: strSrcPath, strDesPath
	integer :: istatus

	istatus = CopyDirC( strSrcPath, strDesPath )
	if( istatus/=0 )then
		print*, 'ERROR:copy directory ', strSrcPath, ' to ', strDesPath, ' failed!'
		stop
	end if
end subroutine CopyDir
	
subroutine RemoveDir( strPath )
implicit none
	character(*), intent(in) :: strPath
	integer :: istatus

	istatus = RemoveDirC( strPath )
	if( istatus/=0 )then
		print*, 'ERROR:remove directory ', strPath, ' failed!'
		!stop
	end if
end subroutine RemoveDir

end module condor_helper
