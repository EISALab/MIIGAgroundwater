module surrfit
use surrio
implicit none

contains

subroutine DoSurrFitFunc
implicit none
	double precision :: rFitness, rTotalCost, Pen_Rdx, Pen_Tnt
	integer :: drycell_c, drycell_h

	if ((g_nGridFlagX.eq.1) .and. (g_nGridFlagY.eq.1))then
		call Obj_Func_Coarse(rFitness,rTotalCost,Pen_Rdx,Pen_Tnt,g_nGridFlagX, g_nGridFlagY,drycell_c, drycell_h)
	endif

	!fitfess function for the fine grid
	if ((g_nGridFlagX.eq.2) .and. (g_nGridFlagY.eq.2))then
		call Obj_Func_Fine(rFitness,rTotalCost,Pen_Rdx,Pen_Tnt,g_nGridFlagX, g_nGridFlagY, drycell_c, drycell_h)
	endif

	call SaveSurrogateInfo( rFitness, rTotalCost, Pen_Rdx,Pen_Tnt, drycell_c, drycell_h)

end subroutine

end module surrfit
