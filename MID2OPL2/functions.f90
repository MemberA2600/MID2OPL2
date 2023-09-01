module functions
  use iflogm
  use ifwinty
  use, intrinsic :: iso_c_binding  

  implicit none

  integer, parameter         :: textLen2 = 500
  
  contains  
  function fdialog(params) result(message)
      character(len=*)       :: params
      character(len=textLen2) :: message
      integer                :: iostat
      logical                :: exist
   
      message = ""
  
      call execute_command_line('filedialogs.exe ' // params, wait=.TRUE., cmdstat = iostat, cmdmsg = message)
      if (message /= "") call execute_command_line('filedialogs.exe "Error" "Error occured!" "' // trim(message) // '"', wait=.TRUE., cmdstat = iostat, cmdmsg = message)
      
      INQUIRE(FILE = "error.txt", EXIST = exist)
      
      if (exist .EQV. .TRUE.) then
          OPEN(unit = 11, file = "error.txt", iostat = iostat)
          READ(11, "(A)", iostat = iostat) message  
          call execute_command_line('filedialogs.exe "Error" "Error occured!" "' // trim(message) // '"', wait=.TRUE., cmdstat = iostat, cmdmsg = message)
          message = ""
      else
          OPEN(unit = 11, file = "response.txt", iostat = iostat)
          READ(11, "(A)", iostat = iostat) message  
      end if    
      CLOSE(11, status = 'DELETE')
  
  
  end function fdialog
  
  subroutine checkOutPut(path)
      use ifcom
      use ifauto
      use MID2OPL2Globals
      use user32
      use iso_c_binding 
      use kernel32
      use, intrinsic :: iso_c_binding  
  
      implicit none
      include 'resource.fd'
  
      type (dialog)        :: dlg
      integer              :: id, callbacktype, length, iostat
      integer(SINT)        :: iret, retlog
      character(c_char)    :: string
      logical              :: exist
      character(len = *)   :: path
  
      inquire(directory = path, exist = exist)
      if (exist .EQV. .TRUE.) then
          retlog = DLGSET(gdlg, IDC_OKBOX, "OK")
      else
          retlog = DLGSET(gdlg, IDC_OKBOX, "!!!")
      end if    
           
  end subroutine
  
  function findLastValidPath(path) result(lastPath)
    character(len = *)              :: path
    character(len = textLen2)       :: lastPath
    integer                         :: iostat, tempPoz, index, num
    character                       :: dummy
    integer, dimension(50)          :: positions
    logical                         :: found
    
    positions = 0
    lastPath  = ""
    index     = 0
    
    do tempPoz = 1, len_trim(path), 1
       if (path(tempPoz:tempPoz) == "/" .OR. path(tempPoz:tempPoz) == "\") then
          index            = index + 1
          positions(index) = tempPoz 
       end if
       
    end do 
    
    found = .FALSE. 
    
    do num = index, 0, -1
       if (found .EQV. .FALSE.) then
           if (num == 0) then
              lastPath = trim(path)
           else   
              lastPath = path(1:positions(index))       
              !dummy = fdialog('"Error" "FUCK" "' // lastPath // '"')
           end if 
       
           inquire(directory = lastPath, exist = found)
       else
           exit
       end if
    end do    
    
    if (found .EQV. .FALSE.) lastPath = ""
    
  end function
  
  function checkIfVGM(fileName) result(isIt)
  
    integer                         :: isIt, index, fLen, subIndex
    character(len=*)                :: fileName
    character(len=1), dimension(11) :: invalid
    
    isIt    = 0
    fLen    = len_trim(fileName)
    
    invalid = (/ c_null_char, "\", "/", ":", "*", "?", '"', "<", ">", "|", "'"  /)
    
    if (fLen > 0) then   
        ! 0: OK
        ! 1: Invalid characters 
        ! 2: Space on start / end, or "." at end
        ! 9: Filename not found
        
        if (fileName(1:1) == " " .OR. fileName(fLen:fLen) == " " .OR. fileName(fLen:fLen) == ".") isIt = 2

        do index = 1, fLen, 1
           do subIndex = 1, size(invalid), 1
              if (invalid(subIndex) == fileName(index:index)) then
                  isIt = 1
                  exit
              end if
           end do 
                   
          if (isIt == 1) exit
        end do
        
    else
        isIt = 9
    end if
        
  end function

  subroutine enableDisableConvertButton() 
      use iflogm
      use ifwinty
      use ifcom
      use ifauto
      use MID2OPL2Globals
      use user32
      use iso_c_binding 
      use kernel32
      use, intrinsic :: iso_c_binding  
      
      implicit none

      integer(SINT) :: WinMain
      integer(HANDLE) hInstance
      integer(HANDLE) hPrevInstance
      integer(LPWSTR) lpszCmdLine
      integer(SINT)   nCmdShow

      include 'resource.fd'
      
      character(len = textLen2) :: tempOK, tempPath, tempName, tempLoad, saveName
      integer(SINT)             :: iret, retlog
      logical                   :: inputOK, outputOK, fileNameOK, loadedOK, ok, sbLoaded
      
      
      retlog = DLGGET(gdlg, IDC_OKBOX   ,  tempOK)
      retlog = DLGGET(gdlg, IDC_OUTPUT  ,  tempPath)
      retlog = DLGGET(gdlg, IDC_FILENAME,  tempName) 
      retlog = DLGGET(gdlg, IDC_LOAD    ,  tempLoad) 
  
      inquire(file = tempLoad, exist = inputOK)
      inquire(file = tempPath, exist = outputOK)
      loadedOK     = midiF%Loaded
      fileNameOK   = checkIfVGM(tempName)
      sbLoaded     = sBank%loaded  
      
      ok = .TRUE.
      
      if (inputOK    .EQV. .FALSE.) ok = .FALSE.
      if (outputOK   .EQV. .FALSE.) ok = .FALSE.
      if (fileNameOK .EQV. .FALSE.) ok = .FALSE.
      if (loadedOK   .EQV. .FALSE.) ok = .FALSE.
      if (sbLoaded   .EQV. .FALSE.) ok = .FALSE.

      
      ! This is for debug only!
      ok = .TRUE.
      
      if (ok .EQV. .TRUE.) then
          retlog = DLGSET (gdlg, IDC_BUTTON_CONVERT, .TRUE., DLG_ENABLE)
      else    
          retlog = DLGSET (gdlg, IDC_BUTTON_CONVERT, .FALSE., DLG_ENABLE)
      end if
 

      
  end subroutine
  
  end module functions