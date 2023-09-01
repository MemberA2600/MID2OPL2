module soundbank
    implicit none
    
    private 
    public                              :: soundB, loadSBList, importBank
    
    logical, parameter                  :: debug  = .TRUE.
    
    type instrument
        character(len = 32)             :: name       
        logical                         :: fixedPitch, doubleVoice
        integer  (kind = 2)             :: modulatorByte1       ! Tremolo / vibrato / sustain / KSR / multi 
        integer  (kind = 2)             :: modulatorByte2       ! Attack rate / decay rate 

        
    end type    
    
    type soundB
        character(len = 500)            :: name = ""
        logical                         :: loaded = .FALSE.
        integer                         :: numOfInstruments = 175
        
        contains
        procedure                       :: LoadSBList => loadSBList
        procedure                       :: ImportBank => importBank

    
    end type

    contains
    
    subroutine loadSBList(this, listOfSB)
        class(soundB), intent(inout)                               :: this 
        character(len=*), dimension(:), allocatable, intent(inout) :: listOfSB
        integer                                                    :: io, lenOfLines, index, allocator
        character(len=500)                                         :: templine, dataBuffer
        logical                                                    :: ok 
        
        lenOfLines = 0
        allocator  = 0
        
        if (debug .EQV. .TRUE.) then
           inquire( file = "debug.txt", exist = ok)
           if (ok .EQV. .TRUE.) then
               open(19, file = "debug.txt", iostat = io, action = "WRITE")
               close(19, status = "delete")
           end if 
        end if   
           
        call system('dir "SoundBanks" /B > temp.txt')   
        
        open(14, file = "temp.txt", iostat = io, action = "READ", status = "OLD")
        do
          read(14, "(A)", iostat = io) templine
          if (io /= 0) exit
          lenOfLines = lenOfLines + 1
          
          if (isItOP2(tempLine) .EQV. .TRUE.) allocator = allocator + 1
        end do    
        
        if (debug .EQV. .TRUE.) then 
            write(dataBuffer, "(I0)") lenOfLines
            call writeLine("Number Of Files in SoundBank folder: " // dataBuffer)
        end if
            
        if (allocator > 0 ) then
            rewind(14)
            allocate(listOfSB(allocator), stat = io)
            allocator = 0
            
            do index = 1, lenOfLines, 1
              read(14, "(A)") tempLine
              if (isItOP2(tempLine) .EQV. .TRUE.) then
                  allocator           = allocator + 1
                  listOfSB(allocator) = tempLine
              end if    
            end do
        end if
        
        close(14, status = "delete")
            
    end subroutine
    
    function isItOP2(line) result(ok)
        character(len = 500)            :: line
        integer                         :: index, lineLen
        logical                         :: ok
        
        ok =  .FALSE.
        lineLen = len_trim(line)
        
        if (lineLen > 3) then
           if (line(lineLen - 3:lineLen) == ".OP2" .OR. line(lineLen - 3: lineLen) == ".op2") ok = .TRUE. 
        end if    
    end function   
    
    subroutine writeLine(text)
        integer                     :: io
        character(len = *)          :: text 
    
        if (debug .EQV. .TRUE.) then
           open(19, file = "debug.txt", iostat = io, action = "WRITE", access = "APPEND")
           write(19, "(A)") trim(text)
           close(19)
        end if   
    
    end subroutine
    
    subroutine writeNumber(num)
        integer(kind = 8)       :: num
        character(len = 50)     :: c
        
        write(c, "(I0)") num
        call writeLine(c)
    
    end subroutine    
    
    function numToText(num) result(c)
        integer(kind = 8)       :: num
        character(len = 50)     :: c
        
        write(c, "(I0)") num
    end function
        
    subroutine importBank(this, path)
        class(soundB), intent(inout)                 :: this
        character(len = *)                           :: path
        logical                                      :: ok = .FALSE.
        integer(kind = 2)                            :: stat
        integer(kind = 8)                            :: numOfBytes, index
        integer(kind = 1), dimension(:), allocatable :: bytes
        character(len = 8)                           :: theFormat
        
        this%loaded = .FALSE.
        
        inquire(file = path, size = numOfBytes)
        allocate(bytes(numOfBytes), stat = stat)
        
        OPEN(unit=17, file=path, access="stream", status="old", action="read",iostat=stat)     
        read(17, iostat=stat) bytes
        CLOSE(17)
        
        this%numOfInstruments = (numOfBytes - 8) / 68
        if (debug .EQV. .TRUE. ) call writeLine("Number of instruments: " // trim(numToText(this%numOfInstruments)))
     
        do index = 1, 8, 1
           !if (debug .EQV. .TRUE. ) call writeLine("ByteIndex #" // trim(numToText(index)) // ": " // trim(numToText(bytes(index))))
           theFormat(index:index) = achar(bytes(index))
        end do    
        
        if (debug .EQV. .TRUE. ) call writeLine("Detected Format: " // theFormat)
        if (theFormat /= "#OPL_II#") goto 888
        
        
        this%loaded = .TRUE.

888 &        
    end subroutine
    
end module