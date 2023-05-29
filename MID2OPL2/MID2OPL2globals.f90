!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

include 'midi.f90'
    
module MID2OPL2Globals
use iflogm
use ifwinty
use midi

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
type(midiFile)         :: midiF

end module
