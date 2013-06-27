SUBROUTINE APPEND_PID(FLNAME)

      USE MPI_MODULE

      IMPLICIT NONE
      CHARACTER , INTENT(INOUT) :: FLNAME*80
      CHARACTER PID_EXTENSION*6
      CHARACTER BUF*4
      INTEGER LENGTH1, LENGTH2

!*******************************************************
!appends processor id at the end of file name
!for example, noreacttest4.out becomes noreacttest4.out.p1

      LENGTH1 = LEN_TRIM('.p')
      PID_EXTENSION = '.p'      


      PID_EXTENSION((LENGTH1+1):(LENGTH1+LEN_TRIM(MPI_PID)))=MPI_PID
    
     ! PID_EXTENSION(3:5) = MPI_PID(2:4) 

     ! PID_EXTENSION((LENGTH1+1):(LENGTH1+LEN_TRIM(MPI_PID)))= MPI_PID(2:4)


    !   PRINT *, 'PID_EXTENSION', PID_EXTENSION(1:6)    

      LENGTH2 = LEN_TRIM(FLNAME)
      FLNAME(LENGTH2+1:LENGTH2+LEN_TRIM(PID_EXTENSION))=PID_EXTENSION

    !  WRITE(*,*)'FILE I/O: ', FLNAME
!**********************************************************

END SUBROUTINE APPEND_PID
