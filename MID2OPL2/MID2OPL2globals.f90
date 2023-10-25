!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

include 'midi.f90'
include 'sb.f90'
include 'envelopeTable.f90'
include 'player.f90'
include 'vgmFile.f90'

module MID2OPL2Globals
use iflogm
use ifwinty
use midi
use soundbank
use player
use VGM

implicit none

!  Parameters

integer*4, parameter, public :: SIZEOFAPPNAME = 100, textLen = 500
logical, parameter, public   :: dbg = .FALSE.

!  Global data

integer(HANDLE)		ghInstance
integer(HANDLE)		ghModule
integer(HANDLE)		ghwndMain
type (dialog) gdlg
character(len=textLen) :: loadText, outPath, sbName = ""
logical                :: already = .FALSE.
character(len=8)       :: currentDate 
type(midiFile)         :: midiF
type(midiPlayer)       :: midiP
type(soundB)           :: sBank
type(vgmFile)          :: myVGM
integer                :: selectedSB = 0
integer(kind = 1)      :: maxNumberOfMembers = 9
integer(kind = 1)      :: maxPercussItems = 3
integer(kind = 1)      :: octaveChange = 0

end module
