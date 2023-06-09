!  MID2OPL2.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   MID2OPL2Sub()  - Callback routine for the main dialog box
!   MID2OPL2Apply()- Callback routine for the APPLY button
!

!****************************************************************************
!
!  FUNCTION: WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!
!  PURPOSE:  Entry point for the application
!
!  COMMENTS: Displays the main window and processes the message loop
!
!****************************************************************************

function WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_WinMain@16' :: WinMain
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'WinMain' :: WinMain
!DEC$ ENDIF

    use user32
    use kernel32
    use iflogm
    use ifcom
    use ifauto
    use MID2OPL2Globals
    use functions

    implicit none

    integer(SINT) :: WinMain
    integer(HANDLE) hInstance
    integer(HANDLE) hPrevInstance
    integer(LPWSTR) lpszCmdLine
    integer(SINT)   nCmdShow

    include 'resource.fd'

    external MID2OPL2Close
    external MID2OPL2MidiLOAD
    external MID2OPL2VGMPath
    external MID2OPL2BoxChanged
    external MID2OPL2Convert

    ! Variables
    type (T_MSG)      :: mesg
    integer*4         :: ret
    integer(LRESULT)  :: lret
    integer(BOOL)     :: bret
    integer(SINT)     :: iret, retlog
    
    ghInstance = hInstance
    ghModule   = GetModuleHandle(NULL)
    ghwndMain  = NULL
    call COMINITIALIZE(ret)

    lret = DlgInit(IDD_MID2OPL2_DIALOG, gdlg)
    if (lret == FALSE) goto 99999
    lret = DlgSetSub(gdlg, IDC_BUTTON_CLOSE, MID2OPL2Close)
    !lret = DlgSetSub(gdlg, IDC_BUTTON_TEST, MID2OPL2Test)
    lret = DlgSetSub(gdlg, IDC_MIDI_BUTTON, MID2OPL2MidiLOAD)
    lret = DlgSetSub(gdlg, IDC_PATH_BUTTON, MID2OPL2VGMPath)
    lret = DLGSETSUB(gdlg, IDC_OUTPUT, MID2OPL2BoxChanged)
    lret = DlgSetSub(gdlg, IDC_BUTTON_CONVERT, MID2OPL2Convert)
    
    retlog = DLGSET(gdlg, IDC_LOAD, "Ready to work!")
    retlog = DLGSET(gdlg, IDC_OUTPUT, "You must construct additional pylons!")
    retlog = DLGSET(gdlg, IDC_SYSTEMNAME, "PC XT/AT")
    retlog = DLGSET(gdlg, IDC_OKBOX, "!!!")
    
    call DATE_AND_TIME(date = currentDate)
    
    retlog = DLGSET(gdlg, IDC_YEAR, currentDate(1:4))
    retlog = DLGSET(gdlg, IDC_MONTH, currentDate(5:6))
    retlog = DLGSET(gdlg, IDC_DAY, currentDate(7:8))

    
    lret = DlgModeless(gdlg, nCmdShow)
    if (lret == FALSE) goto 99999

    ! Read and process messsages
    do while( GetMessage (mesg, NULL, 0, 0) ) 
       if ( DlgIsDlgMessage(mesg) .EQV. FALSE ) then
           bret  = TranslateMessage( mesg )
           lret  = DispatchMessage( mesg )
       end if
    end do
    call DlgUninit(gdlg)
    call COMUNINITIALIZE()

    WinMain = mesg.wParam
    return

99999 &

    iret = MessageBox(ghwndMain, "Error initializing application MID2OPL2"C, &
                     "Error"C, MB_OK)
    call COMUNINITIALIZE()
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: MID2OPL2Sub ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for initialization and destroy
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE MID2OPL2Close( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: MID2OPL2Close

  use user32
  use iflogm
  use ifcom
  use ifauto

  implicit none

  type (dialog) :: dlg
  integer       :: id, callbacktype

  call PostQuitMessage(0)

  END SUBROUTINE 

!****************************************************************************
!
!  FUNCTION: MID2OPL2Apply ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for APPLY button
!
!  COMMENTS:
!
!****************************************************************************

!SUBROUTINE MID2OPL2Test( dlg, id, callbacktype )
!!DEC$ ATTRIBUTES DEFAULT :: MID2OPL2Test

!  use iflogm
!  use ifcom
!  use ifauto
!  use MID2OPL2Globals
!  use user32
!  use iso_c_binding
  
!  implicit none

!  type (dialog)     :: dlg
!  integer           :: id, callbacktype, length
!  integer(SINT)     :: iret
!  character(c_char) :: string
!  character(20)     :: base  
  
!  write(base, "(I0)") callbacktype
!  string = base(len_trim(base):len_trim(base)) // c_null_char
   
!  iret = MessageBox(NULL, string, &
!                    "WOW"C, MB_OK)
!  END SUBROUTINE 


SUBROUTINE MID2OPL2MidiLOAD( dlg, id, callbacktype)
!DEC$ ATTRIBUTES DEFAULT :: MID2OPL2MidiLOAD
 
  use iflogm
  use ifcom
  use ifauto
  use MID2OPL2Globals
  use user32
  use iso_c_binding 
  use kernel32
  use functions
  use, intrinsic :: iso_c_binding  
  
  implicit none
  
  include 'resource.fd'
  
  type (dialog)        :: dlg
  integer              :: id, callbacktype, length, iostat
  integer(SINT)        :: iret, retlog
  character(c_char)    :: string

  loadText = fdialog('"OpenFile" "Open File" "*"')  
  if (loadText /= "") then
      retlog = DLGSET(gdlg, IDC_LOAD, loadText)
      call midiF%loadFile(loadText)
  end if    
      
END SUBROUTINE 

SUBROUTINE MID2OPL2VGMPath( dlg, id, callbacktype)
!DEC$ ATTRIBUTES DEFAULT :: MID2OPL2VGMPath
 
  use iflogm
  use ifcom
  use ifauto
  use MID2OPL2Globals
  use user32
  use iso_c_binding 
  use kernel32
  use functions
  use, intrinsic :: iso_c_binding  
  
  implicit none
  
  include 'resource.fd'
  
  type (dialog)        :: dlg
  integer              :: id, callbacktype, length, iostat
  integer(SINT)        :: iret, retlog
  character(c_char)    :: string

  loadText = fdialog('"OpenFolder" "Open Folder" "*"')  
  if (loadText /= "") retlog = DLGSET(gdlg, IDC_OUTPUT, loadText)
  outPath = loadText 
  call checkOutPut(loadText)
  
    END SUBROUTINE 

SUBROUTINE MID2OPL2BoxChanged( dlg, id, callbacktype)
!DEC$ ATTRIBUTES DEFAULT :: MID2OPL2BoxChanged
 
  use iflogm
  use ifcom
  use ifauto
  use MID2OPL2Globals
  use user32
  use iso_c_binding 
  use kernel32
  use functions
  use, intrinsic :: iso_c_binding  
  
  implicit none
  include 'resource.fd'
  
  type (dialog)            :: dlg
  integer                  :: id, callbacktype, length, iostat
  integer(SINT)            :: iret, retlog
  character(c_char)        :: string
  logical                  :: exist
  character(len = textLen) :: tempDir, dummy
  
  if (already .EQV. .FALSE.) then
      already = .TRUE.
      retlog = DLGGET(gdlg, IDC_OUTPUT, tempDir)
      
      inquire(directory = tempDir, exist = exist)
      if (exist .EQV. .TRUE.) then
          outPath = tempDir
      end if
          !dummy = fdialog('"Error" "Invalid Folder" "The given folder for output does not exist"')
      call checkOutPut(tempDir)    
      
      already = .FALSE.
  end if
  END SUBROUTINE 

SUBROUTINE MID2OPL2Convert( dlg, id, callbacktype)
!DEC$ ATTRIBUTES DEFAULT :: MID2OPL2Convert
 
  use iflogm
  use ifcom
  use ifauto
  use MID2OPL2Globals
  use user32
  use iso_c_binding 
  use kernel32
  use functions
  use, intrinsic :: iso_c_binding  
  
  implicit none
  include 'resource.fd'
  
  type (dialog)            :: dlg
  integer                  :: id, callbacktype, length, iostat, isItVGM, index, fLen
  integer(SINT)            :: iret, retlog
  character(c_char)        :: string
  logical                  :: exist, cancelled
  character(len = textLen) :: text, tempOK, tempPath, answer, fileName, fullName
  character                :: dummy
  character(len = 4)       :: shortDummy
 
  retlog = DLGGET(gdlg, IDC_OKBOX , tempOK)
  retlog = DLGGET(gdlg, IDC_OUTPUT, tempPath)
  
  if (tempOK == "OK") then
     outPath = tempPath
  else
     outPath = findLastValidPath(tempPath)
  end if
  
  if (outPath == "") then
     answer = fdialog('"Error" "Path Unreachable" "The choosen path does not exist!"') 
     goto 99998 
  end if    

  if (outPath /= tempPath) then
     answer = fdialog('"YesOrNo" "Path Unreachable" "The choosen path does not exist, however, shortened path ' // "'" // trim(outPath) // "'" // ' is available. Use it instead?"') 
     if (answer == "No") goto 99998   
     retlog = DLGSET(gdlg, IDC_OUTPUT, outPath)
     retlog = DLGSET(gdlg, IDC_OKBOX , "OK")
  end if    

  fLen = len_trim(outPath)
  
  do while(outPath(1:1) == " ")
     outPath = outPath(2:fLen)
     fLen    = fLen - 1
  end do    
  
  retlog = DLGSET(gdlg, IDC_OUTPUT, outPath)
  retlog = DLGGET(gdlg, IDC_FILENAME , fileName)
  
  isItVGM = checkIfVGM(fileName)
  
 ! write(shortDummy, "(I0)") isItVGM
 ! dummy = fdialog('"Error" "!!!" "' // shortDummy // '"') 
  
  select case(isItVGM)
    case(1)
        dummy = fdialog('"Error" "Invalid Characters" "Invalid characters found in filename ' // "'" // trim(fileName) // "'" // ' !"') 
    case(2)    
        fLen = len_trim(fileName)
        do while(fileName(1:1) == " ")
           fileName = fileName(2:fLen)
           fLen     = fLen - 1
        end do    
        
        do while(fileName(fLen:fLen) == ".")
           fLen     = fLen - 1
           fileName = fileName(1:fLen)
        end do  
        
        retlog = DLGSET(gdlg, IDC_FILENAME , trim(fileName))
    case(9)
        dummy = fdialog('"Error" "No FileName" "No filename found!"') 

    end select    
  
    fLen = len_trim(outPath)
    if (outPath(fLen:fLen) == "\" .OR. outPath(fLen:fLen) == "/") outPath = outPath(1:(fLen-1))
    
    fullName = trim(outPath) // "\" // trim(fileName) // ".vgm"
    !dummy = fdialog('"Error" "!!!" "' // trim(fullName) // '"') 
    
    inquire(file = fullName, exist = exist)
    
    if (exist .EQV. .TRUE.) then
       answer = fdialog('"YesOrNo" "File Already Exists!" "The given file ' // trim(fullName) // ' already exist! Overwrite file?"') 
       if (answer == "No") goto 99998
        
    end if    
    
    inquire(file = loadText, exist = exist)
    if (exist .EQV. .FALSE.) then
       answer = fdialog('"Error" "Source Midi Not Loaded!" "There is no midi file loaded!"') 
       goto 99998
        
    end if    
    
99998 &  
  END SUBROUTINE 