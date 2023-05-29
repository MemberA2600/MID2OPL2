!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module MID2OPL2Globals
use iflogm
use ifwinty

implicit none

!  Parameters

integer*4, parameter, public :: SIZEOFAPPNAME = 100, textLen = 500

!  Global data

integer(HANDLE)		ghInstance
integer(HANDLE)		ghModule
integer(HANDLE)		ghwndMain
type (dialog) gdlg
character(len=textLen) :: loadText, outPath
logical                :: already = .FALSE.
character(len=8)       :: currentDate 

end module
