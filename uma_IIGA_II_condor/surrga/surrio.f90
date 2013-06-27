module surrio
use STD_HELPER
implicit none

integer, parameter :: hSurrBack	= 1000
integer, parameter :: hSurrGo	= 1001

character(*), parameter :: strSurrGo	= 'surrgo.dat'
character(*), parameter :: strSurrBack	= 'surrbak.dat'

integer :: g_nIndividual, g_nGeneration
integer :: g_nGridFlagX, g_nGridFlagY
contains

subroutine PreSurrgate
implicit none
	!vairable
	integer :: i, indiv, gen

	!init global variables
	call OpenInputFile( hSurrGo, strSurrGo )
	rewind(hSurrGo)

	read( hSurrGo, * )g_nIndividual, g_nGeneration
	read( hSurrGo, * )g_nGridFlagX, g_nGridFlagY

	close( hSurrGo );

end subroutine PreSurrgate

subroutine PostSurrgate
implicit none

!	call SaveSurrogateInfo( g_rFitness, g_rTotCost, g_rPenHead, g_rPenRisk, g_rRiskB, g_rRiskW, g_arrHeads, g_arrOldHeads, g_nRemWells )
end subroutine PostSurrgate

subroutine SaveSurrogateInfo( rFitness, rTotCost, Pen_Rdx,Pen_Tnt, drycell_c, drycell_h )
implicit none
	!argument
	double precision, intent(in) :: rFitness, rTotCost
	double precision, intent(in) :: Pen_Rdx,Pen_Tnt
	integer, intent(in) :: drycell_c, drycell_h

	!variables

	call OpenOutputFile( hSurrBack, strSurrBack )
	rewind(hSurrBack)

	write( hSurrBack, * )g_nIndividual, g_nGeneration
	write( hSurrBack, * )rFitness, rTotCost
	write( hSurrBack, * )Pen_Rdx,Pen_Tnt
	write( hSurrBack, * )drycell_c, drycell_h

	close( hSurrBack )
	
end subroutine SaveSurrogateInfo

end module surrio
