   
module soundbank
    
    use envelope

    implicit none
        
    private 
    public                              :: soundB, loadSBList, importBank, instrument
    
    logical, parameter                  :: debug  = .TRUE.
    
    type instrument
        character(len = 32)              :: name       
        logical                          :: fixedPitch, doubleVoice ! DOP                           $C0 - $C8 (0)
        integer (kind = 2)               :: fineTuning, fixedNote  
        integer (kind = 2), dimension(2) :: byte1       ! Tremolo / vibrato / sustain / KSR / multi $20 - $35
        integer (kind = 2), dimension(2) :: byte2       ! Attack rate / decay rate                  $60 - $75
        integer (kind = 2), dimension(2) :: byte3       ! Sustain level / release rate              $80 - $95
        integer (kind = 2), dimension(2) :: byte4       ! Waveform select                           $E0 - $F5
        integer (kind = 2), dimension(2) :: byte5       ! Key scale level                           $40 - $55 (7-6)
        integer (kind = 2), dimension(2) :: byte6       ! Output level                              $40 - $55 (5-0)
        integer (kind = 2)               :: feedback    ! Feedback                                  $C0 - $C8 (3-1) 
        integer (kind = 4)               :: noteOffset
        integer (kind = 2)               :: synthInstrument, percussionInstrument

    end type    
    
    type soundB
        character(len = 500)                                :: name = ""
        logical                                             :: loaded = .FALSE.
        integer                                             :: numOfInstruments = 175
        type(instrument), dimension(:), allocatable         :: instruments
        type(adTable)                                       :: eTable
        
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
        
        call this%eTable%initialize()
        
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
        integer(kind = 8)                            :: numOfBytes, index, nameIndex, counter, subIndex
        integer(kind = 1), dimension(:), allocatable :: bytes
        character(len = 8)                           :: theFormat
        
        integer(kind = 1), dimension(36)             :: instData
        integer(kind = 1), dimension(32)             :: instName
        character(len=16)                            :: tempWord
        character(len=100)                           :: dump
        integer(kind = 2)                            :: percussNum, instrumNum
        
        instrumNum = 0
        percussNum = 34
        
        this%loaded = .FALSE.
        
        inquire(file = path, size = numOfBytes)
        allocate(bytes(numOfBytes), stat = stat)

        if (allocated(this%instruments) .EQV. .TRUE.) deallocate(this%instruments, stat = stat)
        
        OPEN(unit=17, file=path, access="stream", status="old", action="read",iostat=stat)     
        read(17, iostat=stat) bytes
        CLOSE(17)
        
        this%numOfInstruments = (numOfBytes - 8) / 68
        if (debug .EQV. .TRUE. ) call writeLine("Number of instruments: " // trim(numToText(this%numOfInstruments)))
     
        allocate(this%instruments(this%numOfInstruments), stat = stat)
        do index = 1, 8, 1
           !if (debug .EQV. .TRUE. ) call writeLine("ByteIndex #" // trim(numToText(index)) // ": " // trim(numToText(bytes(index))))
           theFormat(index:index) = achar(bytes(index))
        end do    
        
        if (debug .EQV. .TRUE. ) call writeLine("Detected Format: " // theFormat)
        if (theFormat /= "#OPL_II#") goto 888
 
        index     = 9                                - 36
        nameIndex = 9 + (this%numOfInstruments * 36) - 32
        
        do counter = 1, this%numOfInstruments, 1
           index     = index     + 36
           nameIndex = nameIndex + 32
            
           instData  = bytes(index      : index     + 36)
           instName  = bytes(nameIndex  : nameIndex + 32)
           
           do subIndex = 1, 32, 1
              this%instruments(counter)%name(subIndex:subIndex) = achar(instName(subIndex)) 
           end do    
           if (debug .EQV. .TRUE. ) call writeLine("Name of Instrument #" // trim(numToText(counter)) // ": " // this%instruments(counter)%name)
           
           tempWord = ""
           write(tempWord(1:8) , "(B8)") instData(2) 
           write(tempWord(9:16), "(B8)") instData(1) 
                     
           do subIndex = 1, 16, 1
              if (tempword(subindex:subindex) == " ") tempword(subindex:subindex) = "0"
           end do    
           
           this%instruments(counter)%fixedPitch  = .FALSE.
           this%instruments(counter)%doubleVoice = .FALSE.
           
           if (tempWord(16:16) == "1") this%instruments(counter)%fixedPitch  = .TRUE.           
           if (tempWord(14:14) == "1") this%instruments(counter)%doubleVoice = .TRUE.
           
           if (debug .EQV. .TRUE. ) then
               call writeLine("Flag Bytes: " // tempWord)

               write(dump, "(A, 1X, L)") "Fixed Pitch Flag:", this%instruments(counter)%fixedPitch     
               
               write(dump, "(A, 1X, L)") "Double Voice Flag:", this%instruments(counter)%doubleVoice     
               call writeLine(trim(dump))
               
           end if

           if (counter > 128) then
               percussNum = percussNum + 1
               this%instruments(counter)%percussionInstrument = percussNum
               this%instruments(counter)%synthInstrument      = 0
               
               if (debug .EQV. .TRUE. ) call writeLine("Percussion Number: " // trim(numToText(percussNum)))
           else
               instrumNum = instrumNum + 1
               this%instruments(counter)%percussionInstrument = 0
               this%instruments(counter)%synthInstrument      = instrumNum
               if (debug .EQV. .TRUE. ) call writeLine("Instrument Number: " // trim(numToText(instrumNum)))

           end if    

           
           this%instruments(counter)%fineTuning = convertByteToInteger(instData(3)) - 128
           this%instruments(counter)%fixedNote  = convertByteToInteger(instData(4))
           
           if (debug .EQV. .TRUE. ) then
              call writeLine("Fine Tuning: " // trim(numToText(this%instruments(counter)%fineTuning)))
              call writeLine("Fixed Note:  " // trim(numToText(this%instruments(counter)%fixedNote)))
           end if
           
           this%instruments(counter)%byte1(1) = instData(5 )
           this%instruments(counter)%byte2(1) = instData(6 )        
           this%instruments(counter)%byte3(1) = instData(7 )
           this%instruments(counter)%byte4(1) = instData(8 )
           this%instruments(counter)%byte5(1) = instData(9 )
           this%instruments(counter)%byte6(1) = instData(10)
           
           this%instruments(counter)%feedback = instData(11)

           if (this%instruments(counter)%doubleVoice .EQV. .FALSE.) then
               this%instruments(counter)%byte1(2) = 0
               this%instruments(counter)%byte2(2) = 0           
               this%instruments(counter)%byte3(2) = 0
               this%instruments(counter)%byte4(2) = 0
               this%instruments(counter)%byte5(2) = 0
               this%instruments(counter)%byte6(2) = 0
           else
               this%instruments(counter)%byte1(2) = instData(12)
               this%instruments(counter)%byte2(2) = instData(13)          
               this%instruments(counter)%byte3(2) = instData(14)
               this%instruments(counter)%byte4(2) = instData(15)
               this%instruments(counter)%byte5(2) = instData(16)
               this%instruments(counter)%byte6(2) = instData(17)
           end if  
           
           ! Should we add +12?
           this%instruments(counter)%noteOffset = twoCompilantsWordStringToNumber(instData(19:20)) 
           if (debug .EQV. .TRUE. ) call writeLine("Note Offset: " // numToText(this%instruments(counter)%noteOffset) // "(" // tempWord // ")")

        end do    

        this%loaded = .TRUE.

888 &        
        deallocate(bytes, stat = stat)
    end subroutine
    
    function twoCompilantsWordStringToNumber(instData) result(r)
        integer(kind = 1), dimension(2)     :: instData
        character(len = 16)                 :: tempWord
        integer(kind  = 4)                  :: r, subIndex
        logical                             :: negative
        
        tempWord = ""
        write(tempWord(1:8) , "(B8)") instData(2) 
        write(tempWord(9:16), "(B8)") instData(1) 
        
        do subIndex = 1, 16, 1
           if (tempword(subindex:subindex) == " ") tempword(subindex:subindex) = "0"
        end do    
    
        !if (debug .EQV. .TRUE.) call writeLine("From Bytes: " // tempWord)
        read(tempWord, "(B16)") r
        r = r - 1
        
        write(tempWord, "(B16)") r
        
        negative = .FALSE.
        if (tempWord(1:1) == "1") negative = .TRUE.
        
        do subIndex = 1, 16, 1
           if (tempword(subindex:subindex) == " " .OR. tempword(subindex:subindex) == "0") then
              tempword(subindex:subindex) = "1"
           else
              tempword(subindex:subindex) = "0"
           end if 
        end do    
        read(tempWord, "(B16)") r  

        tempWord(1:1) = "0"
        if (negative .EQV. .TRUE.) r = r * -1
    
        
    end function
    
    function twoCompilantsByteStringToNumber(instData) result(r)
        integer(kind = 1)                   :: instData
        character(len = 8)                  :: tempByte
        integer(kind  = 2)                  :: r, subIndex
        logical                             :: negative
        
        tempByte = ""
        write(tempByte , "(B8)") instData
                
        do subIndex = 1, 8, 1
           if (tempByte(subindex:subindex) == " ") tempByte(subindex:subindex) = "0"
        end do    
        !if (debug .EQV. .TRUE.) call writeLine("From Bytes: " // tempByte)

        read(tempByte, "(B8)") r
        r = r - 1
        
        write(tempByte, "(B8)") r
        
        negative = .FALSE.
        if (tempByte(1:1) == "1") negative = .TRUE.
        
        do subIndex = 1, 8, 1
           if (tempByte(subindex:subindex) == " " .OR. tempByte(subindex:subindex) == "0") then
              tempByte(subindex:subindex) = "1"
           else
              tempByte(subindex:subindex) = "0"
           end if 
        end do    
        tempByte(1:1) = "0"
        read(tempByte, "(B8)") r  
        
        if (negative .EQV. .TRUE.) r = r * -1
    
    end function
    
        
    function convertByteToInteger(b) result(r)
        integer(kind = 1)           :: b
        character(len=16)           :: tempWord
        integer(kind = 2)           :: r
        integer                     :: subIndex
    
        write(tempWord(9:16), "(B8)") b
        tempWord(1:8) = "00000000"
           
        do subIndex = 9, 16, 1
           if (tempword(subindex:subindex) == " ") tempword(subindex:subindex) = "0"
        end do    
           
        read(tempWord, "(B16)") r
        
    end function
    
end module