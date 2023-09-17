module midi
    use                                                 :: iso_fortran_env
    use, intrinsic                                      :: iso_c_binding  
    
    implicit none
    
    private
    public                                              :: midiFile, LoadFile, midiData, message

                                                         ! Turn these off at the end!
    logical, parameter                                  :: debug    = .FALSE., streamMode = .FALSE., dbg2 = .FALSE.  
    logical, parameter                                  :: VLQdebug = .FALSE.
    
    type chunk
        logical                                         :: header
        integer(kind = 8)                               :: theSize
        character(len=2), dimension(:), allocatable     :: hexas
        character(len=8), dimension(:), allocatable     :: binaries
          
        contains 
        procedure                                       :: allocateChunk   => AllocateChunk
        procedure                                       :: deAllocateChunk => DeAllocateChunk
        procedure                                       :: processChunk    => ProcessChunk

        
    end type 
   
    type chunkList
       type(chunk), dimension(:), allocatable           :: listOfChunks
       integer(kind = 8)                                :: theSize, last
       
       contains
       procedure                                        :: initList       => InitList
       procedure                                        :: deAllocateList => DeAllocateList
       procedure                                        :: doubleSize     => DoubleSize
       procedure                                        :: nextChunk      => NextChunk
       
    end type    
    
    type metaMessage   
         integer(kind = 8)                              :: lenght, valueAsNum = 0
         character(len=2)                               :: typeAsHex
         character(len=20)                              :: typeAsText
         character(len=250)                             :: valueAsText = ""
         character(len=2), dimension(:), allocatable    :: valueAsHex
         
    end type 
        
    type sysMessage
         integer(kind = 8)                              :: lenght
         character(len=2)                               :: typeAsHex
         character(len=20)                              :: typeAsText
         character(len=2), dimension(:), allocatable    :: valueAsHex
    end type
    
    type midiData
         integer(kind = 8)                              :: lenght
         integer(kind = 1)                              :: channelNum
         character(len=8)                               :: typeAsBin
         character(len=50)                              :: typeAsText
         character(len=8), dimension(:), allocatable    :: valueAsBin
         character                                      :: usage
         character(len = 3)                             :: onOff                                  
         
    end type
    
    type message   
        integer(kind = 8)                              :: deltaTime !, trueDeltaTime
        character(len = 2)                             :: messageType
        type(metaMessage)                              :: metaM
        type(sysMessage)                               :: sysM
        type(midiData)                                 :: midiD
        
    end type    
        
    type track
        integer                                        :: trackNum
        integer(kind = 8)                              :: lastMessage = 0, arraySize
        type(message), dimension(:), allocatable       :: messages
        type(midiFile), pointer                        :: midiF 
        
        contains 
        procedure                                      :: buildTrack        => BuildTrack
        procedure                                      :: addMessage        => AddMessage
        procedure                                      :: doubleMe          => DoubleMe
        procedure                                      :: processAsMeta     => ProcessAsMeta
        procedure                                      :: processAsSysMes   => ProcessAsSysMes
        procedure                                      :: processAsMidiData => ProcessAsMidiData
        procedure                                      :: writeDump         => WriteDump
        
        end type   
    
    type midiFile
        logical                                         :: loaded = .FALSE., divisionMode = .FALSE.
        integer                                         :: numOfBytes, midiType, numberOfTracks, TPQN, fps, ptf
        integer(kind=1) , dimension(:), allocatable     :: bytes 
        character(len=2), dimension(:), allocatable     :: hexas
        character(len=8), dimension(:), allocatable     :: binaries
        type(chunkList)                                 :: chunks
        type(track)     , dimension(:), allocatable     :: tracks
        integer(kind=8), dimension(16)                  :: deltaSums
        type(track)                                     :: midiStream
        
        contains
        procedure                                       :: loadFile    => LoadFile
        
   end type
   
    contains
   !
   ! Track Routines
   !
    
   subroutine DoubleMe(this)
       class(track), intent(inout)               :: this
       integer                                   :: stat
       type(message), dimension(:), allocatable  :: tempMessages 
       integer(kind = 8)                         :: index, subIndex
       
       allocate(tempMessages(this%arraySize), stat = stat) 
       
       do index = 1, this%lastMessage, 1
          tempMessages(index)%deltaTime   = this%messages(index)%deltaTime 
          tempMessages(index)%messageType = this%messages(index)%messageType 
          !tempMessages(index)%trueDeltaTime   = this%messages(index)%trueDeltaTime 

          select case(tempMessages(index)%messageType)
          case ("MT")    
               !tempMessages(index)%metaM  = this%messages(index)%metaM
               tempMessages(index)%metaM%lenght      = this%messages(index)%metaM%lenght 
               tempMessages(index)%metaM%typeAsHex   = this%messages(index)%metaM%typeAsHex 
               tempMessages(index)%metaM%typeAsText  = this%messages(index)%metaM%typeAsText 
               tempMessages(index)%metaM%valueAsText = this%messages(index)%metaM%valueAsText 
               tempMessages(index)%metaM%valueAsNum  = this%messages(index)%metaM%valueAsNum

               
               if (this%messages(index)%metaM%lenght > 0) then
                   allocate(tempMessages(index)%metaM%valueAsHex(this%messages(index)%metaM%lenght), stat = stat)
                   do subIndex = 1, this%messages(index)%metaM%lenght, 1
                      tempMessages(index)%metaM%valueAsHex(subIndex) = this%messages(index)%metaM%valueAsHex(subIndex)
                   end do    
                   deallocate(this%messages(index)%metaM%valueAsHex, stat = stat)
               end if
          
          case("SE")
               tempMessages(index)%sysM%lenght      = this%messages(index)%sysM%lenght 
               tempMessages(index)%sysM%typeAsHex   = this%messages(index)%sysM%typeAsHex 
               tempMessages(index)%sysM%typeAsText  = this%messages(index)%sysM%typeAsText 
               
                if (this%messages(index)%sysM%lenght > 0) then
                   allocate(tempMessages(index)%sysM%valueAsHex(this%messages(index)%sysM%lenght), stat = stat)
                   do subIndex = 1, this%messages(index)%sysM%lenght, 1
                      tempMessages(index)%sysM%valueAsHex(subIndex) = this%messages(index)%sysM%valueAsHex(subIndex)
                   end do    
                   deallocate(this%messages(index)%sysM%valueAsHex, stat = stat)
                end if
           case("MD")    
                tempMessages(index)%midiD%lenght     = this%messages(index)%midiD%lenght 
                tempMessages(index)%midiD%channelNum = this%messages(index)%midiD%channelNum
                tempMessages(index)%midiD%typeAsBin  = this%messages(index)%midiD%typeAsBin               
                tempMessages(index)%midiD%typeAsText = this%messages(index)%midiD%typeAsText    
                tempMessages(index)%midiD%usage      = this%messages(index)%midiD%usage   
                tempMessages(index)%midiD%onOff      = this%messages(index)%midiD%onOff

                
                if (this%messages(index)%midiD%lenght > 0) then
                   allocate(tempMessages(index)%midiD%valueAsBin(this%messages(index)%midiD%lenght), stat = stat)
                   do subIndex = 1, this%messages(index)%midiD%lenght, 1
                      tempMessages(index)%midiD%valueAsBin(subIndex) = this%messages(index)%midiD%valueAsBin(subIndex)
                   end do    
                   deallocate(this%messages(index)%midiD%valueAsBin, stat = stat)
                end if
                
          end select    
       end do    
       
       this%arraySize = this%arraySize * 2 
       
       deallocate(this%messages                , stat = stat)
       allocate  (this%messages(this%arraySize), stat = stat)
       
       do index = 1, this%lastMessage, 1
          this%messages(index)%deltaTime   = tempMessages(index)%deltaTime
          this%messages(index)%messageType = tempMessages(index)%messageType
          !this%messages(index)%trueDeltaTime   = tempMessages(index)%trueDeltaTime

          
          select case(this%messages(index)%messageType)
          case ("MT")    
               !this%messages(index)%metaM  = tempMessages(index)%metaM
               this%messages(index)%metaM%lenght      = tempMessages(index)%metaM%lenght 
               this%messages(index)%metaM%typeAsHex   = tempMessages(index)%metaM%typeAsHex 
               this%messages(index)%metaM%typeAsText  = tempMessages(index)%metaM%typeAsText 
               this%messages(index)%metaM%valueAsText = tempMessages(index)%metaM%valueAsText 
               this%messages(index)%metaM%valueAsNum  = tempMessages(index)%metaM%valueAsNum
               
                if (this%messages(index)%metaM%lenght > 0) then
                   allocate(this%messages(index)%metaM%valueAsHex(this%messages(index)%metaM%lenght), stat = stat)
                   do subIndex = 1, this%messages(index)%metaM%lenght, 1
                      this%messages(index)%metaM%valueAsHex(subIndex) = tempMessages(index)%metaM%valueAsHex(subIndex)
                   end do  
                   deallocate(tempMessages(index)%metaM%valueAsHex, stat = stat)

                end if     
          case("SE")
               this%messages(index)%sysM%lenght      = tempMessages(index)%sysM%lenght 
               this%messages(index)%sysM%typeAsHex   = tempMessages(index)%sysM%typeAsHex 
               this%messages(index)%sysM%typeAsText  = tempMessages(index)%sysM%typeAsText 
               
               if (this%messages(index)%sysM%lenght > 0) then
                   allocate(this%messages(index)%sysM%valueAsHex(this%messages(index)%sysM%lenght), stat = stat)
                   do subIndex = 1, this%messages(index)%sysM%lenght, 1
                      this%messages(index)%sysM%valueAsHex(subIndex) = tempMessages(index)%sysM%valueAsHex(subIndex)
                   end do  
                   deallocate(tempMessages(index)%sysM%valueAsHex, stat = stat)

               end if    
          case("MD")     
                this%messages(index)%midiD%lenght     = tempMessages(index)%midiD%lenght 
                this%messages(index)%midiD%channelNum = tempMessages(index)%midiD%channelNum
                this%messages(index)%midiD%typeAsBin  = tempMessages(index)%midiD%typeAsBin               
                this%messages(index)%midiD%typeAsText = tempMessages(index)%midiD%typeAsText    
                this%messages(index)%midiD%usage      = tempMessages(index)%midiD%usage  
                this%messages(index)%midiD%onOff      = tempMessages(index)%midiD%onOff  
                
                if (this%messages(index)%midiD%lenght > 0) then
                   allocate(this%messages(index)%midiD%valueAsBin(this%messages(index)%midiD%lenght), stat = stat)
                   do subIndex = 1, this%messages(index)%midiD%lenght, 1
                      this%messages(index)%midiD%valueAsBin(subIndex) = tempMessages(index)%midiD%valueAsBin(subIndex)
                   end do  
                   deallocate(tempMessages(index)%midiD%valueAsBin, stat = stat)
               end if   
               
          end select    
          
       end do    
       
       deallocate(tempMessages, stat = stat)
       
   end subroutine
    
   subroutine calculateVLQ(theNumber, binArray, messageDataIndex)
     integer(kind = 8), intent(inout)       :: theNumber, messageDataIndex
     character(len = 8), dimension(*)       :: binArray  
     logical                                :: foundLast = .FALSE.
     integer(kind = 8)                      :: index
     character(len = 200)                   :: tempString
     integer                                :: stat
     character(len = 10)                    :: formatNum
     
     index       = 0
     tempString  = ""
     foundLast   = .FALSE.
     
     if (VLQdebug .EQV. .TRUE.) call dumpTest("Dump VLQ calculation!")
     do while (foundLast .EQV. .FALSE.)
        index = index + 1
        if (binArray(messageDataIndex)(1:1) == "0") foundLast = .TRUE.
        if (VLQdebug .EQV. .TRUE.) call dumpTest(binArray(messageDataIndex))
        
        tempString = trim(tempString) // binArray(messageDataIndex)(2:8)
        messageDataIndex = messageDataIndex + 1
     end do
        
     WRITE(formatNum, "(I0)") index * 7
     formatNum = "(B" // trim(formatNum) // ")"
     
     read(tempstring(1:index * 7), formatNum) theNumber
     
   end subroutine
   
   subroutine addMessage(this, byteIndex, hexArray, binArray, arrSize)
       class(track), intent(inout)            :: this
       character(len = 2), dimension(*)       :: hexArray
       character(len = 8), dimension(*)       :: binArray  
       integer                                :: stat
       integer(kind = 8)                      :: index, channelNum, tempIndex
       integer(kind = 8), intent(inout)       :: byteIndex
       integer(kind = 8)                      :: arrSize
       character(len=255)                     :: byteIndexAsText, lenghtAsText, deltaTimeAsText !, trueDeltaTimeAsText  
       
       if (debug .EQV. .TRUE.) then
          open(12, file = "midiDump.txt", action="write", position="append")
          write(byteIndexAsText, "(I0)") byteIndex   
          write(12, "(A)") ""
          write(12, "(A)") "-> Very First Index / Byte: " // trim(byteIndexAsText) // " / " // trim(hexArray(byteIndex))
          close(12) 
       end if    
       
       if (this%lastMessage == size(this%messages)) call this%doubleMe()
       this%lastMessage = this%lastMessage + 1
       
       this%messages(this%lastMessage)%deltaTime = 0
       
       if (dbg2 .EQV. .TRUE.) then
           tempIndex = byteIndex
       end if    
       
       call calculateVLQ(this%messages(this%lastMessage)%deltaTime, binArray, byteIndex)
       
       if (dbg2 .EQV. .TRUE.) then
          open(37, file = "deltas.txt", action = "write", position = "append")
          write(37, "(A, I0, 1x, I0)") "Indexes: ", tempIndex, byteIndex - 1
          write(37, "(A)", advance = "NO") "Bytes:"
          
          do index = tempIndex, byteIndex-1, 1              
             write(37, "(A)", advance = "NO") " " // hexArray(index)
          end do 
          
          write(37, "(A)") ""
          close(37)
       end if 
             
       if (debug .EQV. .TRUE.) then
         write(byteIndexAsText, "(I0)") byteIndex            
         write(deltaTimeAsText, "(I0)") this%messages(this%lastMessage)%deltaTime 
         open(12, file = "midiDump.txt", action="write", position="append")
         write(12, "(A)") "-> Data Starter ByteIndex: " // trim(byteIndexAsText) // " || DeltaTime Before: " // trim(deltaTimeAsText)
         close(12) 
       end if    
       
       !call dumpTest(hexArray(byteIndex))
       select case(hexarray(byteIndex))
       ! Meta Message
       case("FF")    
           this%messages(this%lastMessage)%messageType = "MT"
           byteIndex = byteIndex + 1
           channelNum = 1
           call this%processAsMeta(byteIndex, hexArray, binArray, arrSize)
       ! System Exclusive Message
       case("F0")
           this%messages(this%lastMessage)%messageType = "SE"
           channelNum = 1
           byteIndex = byteIndex + 1
           call this%processAsSysMes(byteIndex, hexArray, binArray, arrSize)

       ! Midi Message    
       case default    
           this%messages(this%lastMessage)%messageType = "MD"
           !byteIndex = byteIndex + 1
           
           call this%processAsMidiData(byteIndex, hexArray, binArray, arrSize)   
           channelNum = this%messages(this%lastMessage)%midiD%channelNum
                      
       end select
       
     !trueDeltaTimeAsText   = ""

     if (dbg2 .EQV. .TRUE.) then
         open(37, file = "deltas.txt", action = "write", position = "append")
         write(37, "(I0, 1x, I0)") channelNum, this%messages(this%lastMessage)%deltaTime 
         close(37)
     end if 
     
     this%midiF%deltaSums(channelNum) = this%midiF%deltaSums(channelNum) + this%messages(this%lastMessage)%deltaTime 
     !do index = 1, 16, 1
          !this%midiF%deltaSums(index) = this%midiF%deltaSums(index) + this%messages(this%lastMessage)%deltaTime 
          
          !if (index == channelNum) then
              !this%messages(this%lastMessage)%trueDeltaTime =  this%midiF%deltaSums(index) 
              
              !this%midiF%deltaSums(index)  = 0
              !write(trueDeltaTimeAsText, "(I0)") this%messages(this%lastMessage)%trueDeltaTime 
          !end if 
     !end do      
       
     !open(12, file = "midiDump.txt", action="write", position="append")
     !write(12, "(A)") "-> True DeltaTime: " // trim(trueDeltaTimeAsText)
     !close(12) 
     
     if (debug .EQV. .TRUE.) then
         open(12, file = "midiDump.txt", action="write", status="old", position="append")
         call this%writeDump()
         close(12) 
     end if    
       
   end subroutine 
     
   subroutine WriteDump(this)
       class(track), intent(in)            :: this
       character(len = 512)                :: tempText, valAsNumText, channelAsText       
       integer(kind = 8)                   :: index, endIndex, valAsNum
       
       select case(this%messages(this%lastMessage)%messageType)
       case("MT") 
            write(12, "(A)") "Type: Meta Message" 
            
            if (this%messages(this%lastMessage)%metaM%valueAsText /= "") then 
                write(12, "(A)") trim(this%messages(this%lastMessage)%metaM%typeAsText) // " = " // trim(this%messages(this%lastMessage)%metaM%valueAsText)
            else
                write(12, "(A)") trim(this%messages(this%lastMessage)%metaM%typeAsText) 
            end if
                
       case("SE")    
            write(12, "(A)") "Type: System Environment" 
            write(12, "(A)") "Device: " // trim(this%messages(this%lastMessage)%sysM%typeAsText)
            
            tempText     = ""
            valAsNumText = ""
            do index = 1, this%messages(this%lastMessage)%sysM%lenght, 1
               endIndex                         = index * 2  
               tempText(endIndex - 1: endIndex) = this%messages(this%lastMessage)%sysM%valueAsHex(index)
            end do    
            
            read(tempText     , "(Z512)") valAsNum 
            write(valAsNumText, "(I0)"  ) valAsNum
            
            write(12, "(A)") "Value: " // trim(tempText) // " (" // trim(valAsNumText) // ")"
            
       case("MD")
            write(12, "(A)") "Type: Midi Data" 
            channelAsText = ""
            write(channelAsText, "(I0)") this%messages(this%lastMessage)%midiD%channelNum
            
            if (this%messages(this%lastMessage)%midiD%channelNum /= -1) then
                write(12, "(A)") "Message Type: " // trim(this%messages(this%lastMessage)%midiD%typeAsText) // " Channel: " // trim(channelAsText)
            else
                write(12, "(A)") "Message Type: " // trim(this%messages(this%lastMessage)%midiD%typeAsText) 
            end if 
                
            tempText     = ""
            valAsNumText = ""
            do index = 1, this%messages(this%lastMessage)%midiD%lenght, 1
               endIndex                         = index * 8  
               tempText(endIndex - 7: endIndex) = this%messages(this%lastMessage)%midiD%valueAsBin(index)
            end do    
            
            read(tempText     , "(B512)") valAsNum 
            write(valAsNumText, "(I0)"  ) valAsNum
            
            write(12, "(A)") "Value: " // trim(tempText) // " (" // trim(valAsNumText) // ")" // " Usage: " // this%messages(this%lastMessage)%midiD%usage
          
       end select    
       
   end subroutine 
   
   function getSysTypeFromHex(hex) result(text)
   character(len = 2)                         :: hex
   character(len = 20)                        :: text
   
   select case(hex)
   case("01")
       text = "Sequential Circuits"
   case("02")
       text = "Big Briar"
   case("03")
       text = "Octave / Plateau"    
   case("04")
       text = "Moog"    
   case("05")
       text = "Passport Designs"   
   case("06")
       text = "Lexicon"   
   case("07")
       text = "Kurzweil"   
   case("08")
       text = "Fender"   
   case("09")
       text = "Gulbransen"   
   case("0A")
       text = "Delta Labs"   
   case("0B")
       text = "Sound Comp."   
   case("0C")
       text = "General Electro"          
   case("0D")
       text = "Techmar"   
   case("0E")
       text = "Matthews Research"    
   case("10")
       text = "Oberheim"   
   case("11")
       text = "PAIA"   
   case("12")
       text = "Simmons"   
   case("13")
       text = "Gentle Electric"   
   case("14")
       text = "Fairlight"   
   case("15")
       text = "JL Cooper"   
   case("16")
       text = "Lowery"   
   case("17")
       text = "Lin"   
   case("18")
       text = "Emu"   
   case("1B")
       text = "Peavey"   
   case("20")
       text = "Bon Tempi"   
   case("21")
       text = "S.I.E.L."   
   case("23")
       text = "SyntheAxe"   
   case("24")
       text = "Hohner"   
   case("25")
       text = "Crumar"   
   case("26")
       text = "Solton"   
   case("27")
       text = "Jellinghous Ms"   
   case("28")
       text = "CTS"   
   case("29")
       text = "PPG"   
   case("2F")
       text = "Elka"   
   case("40")
       text = "Kawai"   
   case("41")
       text = "Roland"   
   case("42")
       text = "Korg"   
   case("43")
       text = "Yamaha"   
   case("44")
       text = "Casio"   
   case("45")
       text = "Akai"   
   case("46")
       text = "Roland"   
   case("7E")
       text = "Universal Non-Real Time"  
   case("7F")
       text = "Universal Real Time"  
   case default
       text = "Undefined" 
   end select
   
   end function
   
   function getTypeOfContrMes(hexByte) result(text)
       character(len=2)                              :: hexByte
       character(len=50)                             :: text
       
       select case(hexByte)
       case("00")    
           text = "Bank Select"
       case("01")    
           text = "Modulation Wheel"           
       case("02")    
           text = "Breath control"               
       case("04")    
           text = "Foot controller"              
       case("05")    
           text = "Portamento Time"               
       case("06")    
           text = "Data Entry"                       
       case("07")    
           text = "Channel Volume"     
       case("08")    
           text = "Balance"              
       case("0A")    
           text = "Pan"      
       case("0B")    
           text = "Expression Controller"                 
       case("0C")    
           text = "Effect Control 1"     
       case("0D")    
           text = "Effect Control 2"
       case("10")    
           text = "General Purpose Controller #1"               
       case("11")    
           text = "General Purpose Controller #2"    
       case("12")    
           text = "General Purpose Controller #3"    
       case("13")    
           text = "General Purpose Controller #4"    
           
       case("20")    
           text = "Bank Select"
       case("21")    
           text = "Modulation Wheel"           
       case("22")    
           text = "Breath control"               
       case("24")    
           text = "Foot controller"              
       case("25")    
           text = "Portamento Time"               
       case("26")    
           text = "Data Entry"                       
       case("27")    
           text = "Channel Volume"     
       case("28")    
           text = "Balance"              
       case("2A")    
           text = "Pan"      
       case("2B")    
           text = "Expression Controller"                 
       case("2C")    
           text = "Effect Control 1"     
       case("2D")    
           text = "Effect Control 2"
       case("30")    
           text = "General Purpose Controller #1"               
       case("31")    
           text = "General Purpose Controller #2"    
       case("32")    
           text = "General Purpose Controller #3"    
       case("33")    
           text = "General Purpose Controller #4"      

       case("40")    
           text = "Damper pedal (Sustain)"
       case("41")    
           text = "Portamento"
       case("42")    
           text = "Sustenuto"          
       case("43")    
           text = "Soft pedal"    
       case("44")
           text = "Legato Footswitch"
       case("45")
           text = "Hold 2"           
       case("46")
           text = "Sound Controller 1 (Sound Variation)"  
       case("47")
           text = "Sound Controller 2 (Timbre)"             
       case("48")
           text = "Sound Controller 3 (Release Time)"             
       case("49")
           text = "Sound Controller 4 (Attack Time)"             
       case("4A")
           text = "Sound Controller 5 (Brightness)"  
       case("4B")
           text = "Sound Controller 6"             
       case("4C")
           text = "Sound Controller 7"             
       case("4D")
           text = "Sound Controller 8"              
       case("4E")
           text = "Sound Controller 9"  
       case("4F")
           text = "Sound Controller 10"             
       case("50")
           text = "General Purpose Controller #5"             
       case("51")
           text = "General Purpose Controller #6"  
       case("52")
           text = "General Purpose Controller #7"             
       case("53")
           text = "General Purpose Controller #8"  
       case("54")
           text = "Portamento Control"             
       case("5B")
           text = "Effects 1 Depth"    
       case("5C")
           text = "Effects 2 Depth"         
       case("5D")
           text = "Effects 3 Depth"         
       case("5E")
           text = "Effects 4 Depth"      
       case("5F")
           text = "Effects 5 Depth"
             
       case("60")
           text = "Data Entry +1"         
       case("61")
           text = "Data Entry -1"         
       case("62")
           text = "Non-Registered Parameter Number LSB	0"   
       case("63")
           text = "Non-Registered Parameter Number MSB	0"    
       case("64")
           text = "* Registered Parameter Number LSB"         
       case("65")
           text = "* Registered Parameter Number MSB"         
       case("78")
           text = "All Sound Off"   
       case("79")
           text = "* Reset All Controllers"    
       case("7A")
           text = "Local Control On/Off"         
       case("7B")
           text = "* All Notes Off"         
       case("7C")
           text = "Omni Mode Off (+ All Notes Off)"   
       case("7D")
           text = "Omni Mode On (+ All Notes Off)"              
       case("7E")
           text = "Poly Mode On/Off (+ All Notes Off)"   
       case("7F")
           text = "Poly Mode On (Incl Mono=Off +All Notes Off)"   
           
       case default
           text = "Undefined"
           
       end select    
   
   end function
   
   subroutine processAsMidiData(this, byteIndex, hexArray, binArray, arrSize)
       class(track), intent(inout)                   :: this
       character(len = 2), dimension(*)              :: hexArray
       character(len = 8), dimension(*)              :: binArray  
       integer                                       :: stat, channelNum
       integer(kind = 8)                             :: index, saveIndex
       integer(kind = 8), intent(inout)              :: byteIndex
       integer(kind = 8)                             :: arrSize
       character(len = 2)                            :: channelNumAsText

       character(len = 20)                           :: tempText
       character(len = 255)                          :: sizeAsText
       character(len = 3)                            :: advance
       character(len = 2)                            :: tempHex
       integer(kind = 2)                             :: tempNum
       logical                                       :: gotIt
            
       this%messages(this%lastMessage)%midiD%typeAsBin  = binArray(byteIndex)
       this%messages(this%lastMessage)%midiD%lenght = 0        

       this%messages(this%lastMessage)%midiD%channelNum = -1
       
       read(binArray(byteIndex)(5:8), "(B4)") channelNum
       channelNum = channelNum + 1
       write(channelNumAsText, "(I2)")        channelNum
       
       if (channelNumAsText(1:1) == " ") channelNumAsText(1:1) = "0"
       
       this%messages(this%lastMessage)%midiD%usage             = "L"
       gotIt                                                   = .FALSE.       
       this%messages(this%lastMessage)%midiD%onOff             = "ON "

       !write(tempText, "(I0)") this%messages(this%lastMessage)%deltaTime
       !call dumpTest(binArray(byteIndex) // " || " // tempText)
       
       select case(binArray(byteIndex)(1:4))
       case ("1000")
           this%messages(this%lastMessage)%midiD%lenght     = 2
           this%messages(this%lastMessage)%midiD%typeAsText = "Note Off (Channel " // channelNumAsText // ")"
           this%messages(this%lastMessage)%midiD%channelNum = channelNum
           this%messages(this%lastMessage)%midiD%typeAsBin  = this%messages(this%lastMessage)%midiD%typeAsBin(1:4) // "0000" 
           gotIt = .TRUE.
       case ("1001")
           this%messages(this%lastMessage)%midiD%lenght     = 2     
           this%messages(this%lastMessage)%midiD%typeAsText = "Note On (Channel " // channelNumAsText // ")"
           this%messages(this%lastMessage)%midiD%channelNum = channelNum
           this%messages(this%lastMessage)%midiD%typeAsBin  = this%messages(this%lastMessage)%midiD%typeAsBin(1:4) // "0000" 
           gotIt = .TRUE.

       case ("1010")
           this%messages(this%lastMessage)%midiD%lenght     = 2  
           this%messages(this%lastMessage)%midiD%typeAsText = "Polyphonic Key Pressure (Channel " // channelNumAsText // ")"
           this%messages(this%lastMessage)%midiD%channelNum = channelNum
           this%messages(this%lastMessage)%midiD%typeAsBin  = this%messages(this%lastMessage)%midiD%typeAsBin(1:4) // "0000"            
           gotIt = .TRUE.
       case ("1011")
           ! Controller Message
           byteIndex                                        = byteIndex + 1
           this%messages(this%lastMessage)%midiD%typeAsBin  = binArray(byteIndex)
           this%messages(this%lastMessage)%midiD%lenght     = 1 
           this%messages(this%lastMessage)%midiD%channelNum = channelNum
           this%messages(this%lastMessage)%midiD%typeAsText = getTypeOfContrMes(hexArray(byteIndex)) // " (" // channelNumAsText // ")"         
           
           if (this%messages(this%lastMessage)%midiD%typeAsBin(1:4) == "0000" .OR. this%messages(this%lastMessage)%midiD%typeAsBin(1:4) == "0001" &
         &.OR. this%messages(this%lastMessage)%midiD%typeAsBin  == "01100011" .OR. this%messages(this%lastMessage)%midiD%typeAsBin  == "01100101") then
              this%messages(this%lastMessage)%midiD%usage = "M" 
               
         end if
           
         if   (this%messages(this%lastMessage)%midiD%typeAsBin  == "01000000" .OR. &
              &this%messages(this%lastMessage)%midiD%typeAsBin  == "01000001" .OR. &
              &this%messages(this%lastMessage)%midiD%typeAsBin  == "01000010" .OR. &
              &this%messages(this%lastMessage)%midiD%typeAsBin  == "01000011" .OR. &               
              &this%messages(this%lastMessage)%midiD%typeAsBin  == "01000100" .OR. &               
              &this%messages(this%lastMessage)%midiD%typeAsBin  == "01000101" .OR. &
              &this%messages(this%lastMessage)%midiD%typeAsBin  == "01111010"       ) then
              
              this%messages(this%lastMessage)%midiD%usage = binArray(byteIndex)(2:2)
              
              if (this%messages(this%lastMessage)%midiD%usage == "0") this%messages(this%lastMessage)%midiD%onOff = "OFF"
              
          end if     
          gotIt = .TRUE.

          if (this%messages(this%lastMessage)%midiD%typeAsBin(1:5) == "01111" .AND. &
             &this%messages(this%lastMessage)%midiD%typeAsBin      /= "01111010"    ) then
              select case(this%messages(this%lastMessage)%midiD%typeAsBin(6:8))
              case("101")
                  this%messages(this%lastMessage)%midiD%usage = "1"
                  this%messages(this%lastMessage)%midiD%onOff = "ON "
              case("111")
                  this%messages(this%lastMessage)%midiD%usage = "1"
                  this%messages(this%lastMessage)%midiD%onOff = "ON "    
              case default
                  this%messages(this%lastMessage)%midiD%usage = "0"
                  this%messages(this%lastMessage)%midiD%onOff = "OFF"                  
     
              end select    
          
          end if
          
       case ("1100")
           this%messages(this%lastMessage)%midiD%lenght     = 1    
           this%messages(this%lastMessage)%midiD%typeAsText = "Program Change (Channel " // channelNumAsText // ")"
           this%messages(this%lastMessage)%midiD%channelNum = channelNum
           this%messages(this%lastMessage)%midiD%typeAsBin  = this%messages(this%lastMessage)%midiD%typeAsBin(1:4) // "0000"            
           gotIt = .TRUE.

       case ("1101")
           this%messages(this%lastMessage)%midiD%lenght     = 1  
           this%messages(this%lastMessage)%midiD%typeAsText = "Channel Pressure (Channel " // channelNumAsText // ")"
           this%messages(this%lastMessage)%midiD%channelNum = channelNum 
           this%messages(this%lastMessage)%midiD%typeAsBin  = this%messages(this%lastMessage)%midiD%typeAsBin(1:4) // "0000"            
           gotIt = .TRUE.

       
       case ("1110")
           this%messages(this%lastMessage)%midiD%lenght     = 2           
           this%messages(this%lastMessage)%midiD%typeAsText = "Pitch Wheel Change (Channel " // channelNumAsText // ")"   
           this%messages(this%lastMessage)%midiD%channelNum = channelNum 
           this%messages(this%lastMessage)%midiD%typeAsBin  = this%messages(this%lastMessage)%midiD%typeAsBin(1:4) // "0000"            
           gotIt     = .TRUE.
           
           this%messages(this%lastMessage)%midiD%usage      = "B"      
           
       end select     
       
       if (gotIt .EQV. .FALSE.) then
          select case(binArray(byteIndex)) 
          case("11110010")
              this%messages(this%lastMessage)%midiD%lenght     = 2           
              this%messages(this%lastMessage)%midiD%typeAsText = "Song Position Pointer"   
              this%messages(this%lastMessage)%midiD%typeAsBin  = binArray(byteIndex)
              this%messages(this%lastMessage)%midiD%usage      = "B"
          case("11110011")
              this%messages(this%lastMessage)%midiD%lenght     = 1           
              this%messages(this%lastMessage)%midiD%typeAsText = "Song Select"   
              this%messages(this%lastMessage)%midiD%typeAsBin  = binArray(byteIndex)
          case("11110110")
              this%messages(this%lastMessage)%midiD%lenght     = 0           
              this%messages(this%lastMessage)%midiD%typeAsText = "Tune Request"   
              this%messages(this%lastMessage)%midiD%typeAsBin  = binArray(byteIndex)
          case("11111000")
              this%messages(this%lastMessage)%midiD%lenght     = 0           
              this%messages(this%lastMessage)%midiD%typeAsText = "Timing Clock"   
              this%messages(this%lastMessage)%midiD%typeAsBin  = binArray(byteIndex)          
          case("11111010")
              this%messages(this%lastMessage)%midiD%lenght     = 0           
              this%messages(this%lastMessage)%midiD%typeAsText = "Start Seq Playing"   
              this%messages(this%lastMessage)%midiD%typeAsBin  = binArray(byteIndex)  
          case("11111100")
              this%messages(this%lastMessage)%midiD%lenght     = 0           
              this%messages(this%lastMessage)%midiD%typeAsText = "Stop Seq Playing"   
              this%messages(this%lastMessage)%midiD%typeAsBin  = binArray(byteIndex)                
          case("11111110")
              this%messages(this%lastMessage)%midiD%lenght     = 0           
              this%messages(this%lastMessage)%midiD%typeAsText = "Active Sensing"   
              this%messages(this%lastMessage)%midiD%typeAsBin  = binArray(byteIndex)    
          case("11111111")
              this%messages(this%lastMessage)%midiD%lenght     = 0           
              this%messages(this%lastMessage)%midiD%typeAsText = "Reset"   
              this%messages(this%lastMessage)%midiD%typeAsBin  = binArray(byteIndex)    
                            
          end select
       end if
       
       byteIndex = byteIndex + 1
       saveIndex = 1
       allocate(this%messages(this%lastMessage)%midiD%valueAsBin(this%messages(this%lastMessage)%midiD%lenght), stat = stat) 
       
       !call dumpTest(trim(this%messages(this%lastMessage)%midiD%typeAsBin) // " " // trim(this%messages(this%lastMessage)%midiD%typeAsText))
       
       if (this%messages(this%lastMessage)%midiD%lenght /= 0) then  
           if (this%messages(this%lastMessage)%midiD%usage /= "B") then
               do index = byteIndex, byteIndex + this%messages(this%lastMessage)%midiD%lenght - 1, 1
                  if (this%messages(this%lastMessage)%midiD%usage == "M") then 
                      this%messages(this%lastMessage)%midiD%valueAsBin(saveIndex) = "0" // flipTheBits(binArray(index)(2:8), 7)
                  else
                      this%messages(this%lastMessage)%midiD%valueAsBin(saveIndex) = binArray(index) 
                  end if     
                  saveIndex = saveIndex + 1 
                  byteIndex = byteIndex + 1
               end do    
           
           else 
               this%messages(this%lastMessage)%midiD%valueAsBin(1) = binArray(index) 
               byteIndex = byteIndex + 1
               this%messages(this%lastMessage)%midiD%valueAsBin(2) = "0" // flipTheBits(binArray(index)(2:8), 7)
               byteIndex = byteIndex + 1
           
           end if    
       end if
       
       if (debug .EQV. .TRUE.) then
           open(12, file = "midiDump.txt", action="write", position="append")
           write(sizeAsText, "(I0)") this%messages(this%lastMessage)%midiD%lenght
           write(12, "(A)") "-> Size: " // trim(sizeAsText)

           do index = 1, this%messages(this%lastMessage)%midiD%lenght, 1
              if (mod(index, 16) == 0 .OR. index == this%messages(this%lastMessage)%midiD%lenght) then
                  advance = "YES"
              else
                  advance = "NO "               
              end if 
              
              read(this%messages(this%lastMessage)%midiD%valueAsBin(index), "(B8)") tempNum
              write(tempHex, "(Z2)") tempNum
              
              if (tempHex(1:1) == " ") tempHex(1:1) = "0"
              
              write(12, "(A, 1x)", advance = advance) tempHex
           end do    
           close(12)           
       end if
       
   end subroutine    
       
   subroutine dumpTest(text)
     character(len = *)                              ::  text
   
     open( 12, file = "midiDump.txt", action="write", position="append")
     write(12, "(A)") "!!! Test: " // trim(text)
     close(12)   
   
   end subroutine
   
   
   subroutine ProcessAsSysMes(this, byteIndex, hexArray, binArray, arrSize)
       class(track), intent(inout)                   :: this
       character(len = 2), dimension(*)              :: hexArray
       character(len = 8), dimension(*)              :: binArray  
       integer                                       :: stat
       integer(kind = 8)                             :: index, saveIndex, tempLen
       integer(kind = 8), intent(inout)              :: byteIndex
       integer(kind = 8)                             :: arrSize
       character(len = 2), dimension(:), allocatable :: tempArr
       
       character(len = 255)                          :: sizeAsText    
       character(len = 3)                            :: advance   
       
       this%messages(this%lastMessage)%sysM%typeAsHex  = hexArray(byteIndex + 1)
       this%messages(this%lastMessage)%sysM%typeAsText = getSysTypeFromHex(this%messages(this%lastMessage)%sysM%typeAsHex)
       this%messages(this%lastMessage)%sysM%lenght = 0
       
       byteIndex = byteIndex - 1
       
       do 
           ! Should stand on "F0' or "F7"
           if (byteIndex + 2 > arrSize)                                                   exit          
           if (hexArray(byteIndex + 2) /= this%messages(this%lastMessage)%sysM%typeAsHex) exit
            
           byteIndex = byteIndex + 1
           call calculateVLQ(tempLen, binArray, byteIndex)

           tempLen   = tempLen   - 1
           
           if (allocated(this%messages(this%lastMessage)%sysM%valueAsHex) .EQV. .FALSE.) then
               allocate(this%messages(this%lastMessage)%sysM%valueAsHex(tempLen), stat = stat) 
           else
               allocate(tempArr(this%messages(this%lastMessage)%sysM%lenght), stat = stat)
               do index = 1, this%messages(this%lastMessage)%sysM%lenght, 1
                  tempArr(index) = this%messages(this%lastMessage)%sysM%valueAsHex(index) 
               end do    
               deallocate(this%messages(this%lastMessage)%sysM%valueAsHex, stat = stat)
               
               allocate(this%messages(this%lastMessage)%sysM%valueAsHex(this%messages(this%lastMessage)%sysM%lenght + tempLen), stat = stat)
               do index = 1, this%messages(this%lastMessage)%sysM%lenght, 1
                  this%messages(this%lastMessage)%sysM%valueAsHex(index) = tempArr(index)
               end do     
               deallocate(tempArr, stat = stat)
               
           end if    
           
           do index = this%messages(this%lastMessage)%sysM%lenght + 1, this%messages(this%lastMessage)%sysM%lenght + tempLen, 1
              this%messages(this%lastMessage)%sysM%valueAsHex(index) = hexArray(byteIndex)
              byteIndex                                              = byteIndex + 1
           end do     
           
           this%messages(this%lastMessage)%sysM%lenght = this%messages(this%lastMessage)%sysM%lenght + tempLen
           
       end do
       
       if (debug .EQV. .TRUE.) then
           open(12, file = "midiDump.txt", action="write", position="append")
           write(sizeAsText, "(I0)") this%messages(this%lastMessage)%sysM%lenght
           write(12, "(A)") "-> Size: " // trim(sizeAsText)

           do index = 1, this%messages(this%lastMessage)%sysM%lenght, 1
              if (mod(index, 16) == 0 .OR. index == this%messages(this%lastMessage)%sysM%lenght) then
                  advance = "YES"
              else
                  advance = "NO "               
              end if 
              write(12, "(A, 1x)", advance = advance) this%messages(this%lastMessage)%sysM%valueAsHex(index)
           end do    
           close(12)           
       end if
       
   end subroutine
       
   function getMetaTypeFromHex(hex) result(text)
   character(len = 2)                         :: hex
   character(len = 20)                        :: text
   
   select case(hex)
   case("00")
       text = "Sequence Number"
   case("01")
       text = "Text"
   case("02")
       text = "Copyright Notice"
   case("03")
       text = "Track Name"
   case("04")
       text = "Instrument Name"
   case("05")
       text = "Lyrics"    
   case("06")
       text = "Marker" 
   case("07")
       text = "Cue Point"       
   case("20")
       text = "Channel Prefix" 
   case("2F")
       text = "End of Track"    
   case("51")
       text = "Set Tempo"     
   case("54")
       text = "SMPTE Offset" 
   case("58")
       text = "Time Signature"
   case("59")
       text = "Key Signature"       
   case("7F")
       text = "Sequencer Specific"     
   case("FF")
       text = "Reset"
   case default
       text = "Undefined" 
   end select
   
   end function
   
   subroutine ProcessAsMeta(this, byteIndex, hexArray, binArray, arrSize)
       class(track), intent(inout)            :: this
       character(len = 2), dimension(*)       :: hexArray
       character(len = 8), dimension(*)       :: binArray  
       integer                                :: stat
       integer(kind = 8)                      :: index, saveIndex, lenght
       integer(kind = 8), intent(inout)       :: byteIndex
       integer(kind = 8)                      :: arrSize
       
       character(len=255)                     :: sizeAsText  
       character(len=3)                       :: advance
       
       this%messages(this%lastMessage)%metaM%typeAsHex = hexArray(byteIndex)
       !call dumpTest(hexArray(byteIndex))
       
       byteIndex = byteIndex + 1
       this%messages(this%lastMessage)%metaM%typeAsText = getMetaTypeFromHex(this%messages(this%lastMessage)%metaM%typeAsHex) 
       
       this%messages(this%lastMessage)%metaM%lenght = 0
       !call dumpTest(binArray(byteIndex))
       call calculateVLQ(this%messages(this%lastMessage)%metaM%lenght, binArray, byteIndex)
       
       if (this%messages(this%lastMessage)%metaM%lenght > 0) then
           allocate(this%messages(this%lastMessage)%metaM%valueAsHex(this%messages(this%lastMessage)%metaM%lenght), stat = stat)
       else    
           goto 7777
       end if
       this%messages(this%lastMessage)%metaM%valueAsText = ""
       ! allocate(this%messages(this%lastMessage)%metaM%valueAsHex(this%messages(this%lastMessage)%metaM%lenght), stat = stat)
             
       saveIndex = 1
       do index = byteIndex, this%messages(this%lastMessage)%metaM%lenght + byteIndex - 1, 1         
          this%messages(this%lastMessage)%metaM%valueAsHex(saveIndex) = hexArray(index)  
          saveIndex = saveIndex + 1            
       end do   
       
       if (debug .EQV. .TRUE.) then
           open(12, file = "midiDump.txt", action="write", position="append")
           write(sizeAsText, "(I0)") this%messages(this%lastMessage)%metaM%lenght
           write(12, "(A)") "-> Size: " // trim(sizeAsText)
                 
           do index = 1, this%messages(this%lastMessage)%metaM%lenght, 1
              if (mod(index, 16) == 0 .OR. index == this%messages(this%lastMessage)%metaM%lenght) then
                  advance = "YES"
              else
                  advance = "NO "               
              end if 
              write(12, "(A, 1x)", advance = advance) this%messages(this%lastMessage)%metaM%valueAsHex(index)
           end do    
           close(12)           
       end if
       
       if (this%messages(this%lastMessage)%metaM%typeAsHex == "00" .OR. this%messages(this%lastMessage)%metaM%typeAsHex == "20" .OR. &
          &this%messages(this%lastMessage)%metaM%typeAsHex == "51" .OR. this%messages(this%lastMessage)%metaM%typeAsHex == "54" .OR. &
          &this%messages(this%lastMessage)%metaM%typeAsHex == "58" .OR. this%messages(this%lastMessage)%metaM%typeAsHex == "59" ) then
       
           lenght = 0
           select case(this%messages(this%lastMessage)%metaM%typeAsHex)
           case("00")
               lenght = 2
           case("20")
               lenght = 1
           case("51")
               lenght = 3
           case("54")
               lenght = 5
           case("58")
               lenght = 4
           case("59")
               lenght = 2
           end select 
           
           this%messages(this%lastMessage)%metaM%valueAsNum  = getNumberFromHexData(this%messages(this%lastMessage)%metaM%valueAsHex, lenght)
           
           select case(this%messages(this%lastMessage)%metaM%typeAsHex)
           case("51")
               this%messages(this%lastMessage)%metaM%valueAsNum = 60000000 / this%messages(this%lastMessage)%metaM%valueAsNum
           end select
           
           select case(this%messages(this%lastMessage)%metaM%typeAsHex)
           case("58")    
               this%messages(this%lastMessage)%metaM%valueAsText = getTimeSignature(this%messages(this%lastMessage)%metaM%valueAsHex)
           case default 
                write(this%messages(this%lastMessage)%metaM%valueAsText, "(I0)") this%messages(this%lastMessage)%metaM%valueAsNum
           end select

       else
           this%messages(this%lastMessage)%metaM%valueAsNum  = 0
           this%messages(this%lastMessage)%metaM%valueAsText = getASCIIFromBytes(this%messages(this%lastMessage)%metaM%valueAsHex, &
                                                                               & this%messages(this%lastMessage)%metaM%lenght)
       end if
       byteIndex = byteIndex + this%messages(this%lastMessage)%metaM%lenght
7777 &       
   end subroutine    
       
   function getTimeSignature(hexArray) result(timeSign)
        character(len = 2), dimension(*)      :: hexArray
        integer(kind = 8)                     :: index
        integer(kind = 2)                     :: numerator, denominator, metronomeClicks, X32ndNotesPerBeat
        character(len = 250)                  :: timeSign
        
        timeSign  = ""
        read(hexArray(1), "(Z2)") numerator   
        read(hexArray(2), "(Z2)") denominator
        read(hexArray(3), "(Z2)") metronomeClicks
        read(hexArray(4), "(Z2)") X32ndNotesPerBeat

        write(timeSign, "(I0, A, I0, A, I0, A, I0)") numerator, "/", 2 ** denominator, " || Clicks: ", metronomeClicks, " || 32nd Notes/Beat: ", X32ndNotesPerBeat

   end function
   
   function getNumberFromHexData(hexArray, lenght) result(num)
        integer(kind = 8)                     :: num
        integer(kind = 8)                     :: index
        character(len = 2), dimension(*)      :: hexArray
        integer(kind = 8)                     :: lenght
        character(len = lenght * 2)           :: tempText   
        character(len = 20)                   :: F  
        
        do index = 1, lenght, 1
           tempText((index * 2) - 1 : index * 2) = hexArray(index) 
        end do    
               
        write(F, "(A, I0, A)") "(Z", lenght * 2, ")" 
        !call dumpTest(F)
        !call dumpTest(tempText)

        read(tempText, F) num
        
   end function 
   
   subroutine BuildTrack(this, hexArray, binArray, arrSize)
       class(track), intent(inout)            :: this
       character(len = 2), dimension(*)       :: hexArray
       character(len = 8), dimension(*)       :: binArray  
       integer                                :: stat
       integer(kind = 8)                      :: byteIndex
       integer(kind = 8)                      :: arrSize
       character(len = 255)                   :: trackNumAsText, sizeAsText   
       character(len = 3)                     :: advance
             
       this%lastMessage = 0
       if (allocated(this%messages) .EQV. .FALSE.) then
           allocate(this%messages(64), stat = stat)
           this%arraySize = size(this%messages)
       end if 
   
       if (debug .EQV. .TRUE.) then
           open(12, file = "midiDump.txt", action="write", position="append")
           write(12, "(A)") ""
           write(trackNumAsText, "(I0)") this%trackNum
           write(sizeAsText    , "(I0)") arrSize
           write(12, "(A)") "---> TrackNum: " // trim(trackNumAsText) // " || Number of Bytes: " // trim(sizeAsText)

           do byteIndex = 1, arrSize, 1
              if (mod(byteIndex, 16) == 0 .OR. byteIndex == arrSize) then
                  advance = "YES"
              else
                  advance = "NO "               
              end if 
              write(12, "(A, 1x)", advance = advance) hexArray(byteIndex)
           end do    
           write(12, "(A)") ""
           write(12, "(A)") "------------------"
           write(12, "(A)") "List Of Messages:"
           write(12, "(A)") "------------------"
           close(12)
       end if    
       
       byteIndex        = 1
       do while (byteIndex < arrSize) 
          call this%addMessage(byteIndex, hexArray, binArray, arrSize)
       end do    
   end subroutine
    
   ! 
   ! Chunk Routines 
   !   
   
   subroutine AllocateChunk(this)
     class(chunk), intent(inout) :: this
     integer                     :: stat
   
     if (allocated(this%hexas) .EQV. .FALSE.) then
         allocate(this%hexas(this%theSize)   , stat = stat)
         allocate(this%binaries(this%theSize), stat = stat)
     end if
   end subroutine
   
   subroutine DeAllocateChunk(this)
     class(chunk), intent(inout) :: this
     integer                     :: stat
   
     if (allocated(this%hexas) .EQV. .TRUE.) then
         deallocate(this%hexas   , stat = stat)
         deallocate(this%binaries, stat = stat)
     end if
   end subroutine
   
   subroutine ProcessChunk(this, midiF, trackNum)
      class(chunk), intent(inout)            :: this
      class(midiFile), intent(inout), target :: midiF
      character(len=2), dimension(4)         :: tempArray
      integer                                :: stat 
      integer(kind = 4), intent(inout)       :: trackNum
      integer(kind = 8)                      :: arrSize
      
      tempArray = (/ "00", "00", "00", "00" /)
      
      if (this%header .EQV. .TRUE.) then
          tempArray(3)      = this%hexas(1)
          tempArray(4)      = this%hexas(2)          
          
          midiF%midiType    = getInt32FromBytes(tempArray)
          
          tempArray(3)      = this%hexas(3)
          tempArray(4)      = this%hexas(4)          
          
          midiF%numberOfTracks = getInt32FromBytes(tempArray)          
          
          allocate(midiF%tracks(midiF%numberOfTracks), stat = stat)
          call setMidiTiming(midiF, this%binaries(5) // this%binaries(6))
      else
          if (streamMode .EQV. .FALSE.) then  
              trackNum = trackNum + 1
              midiF%tracks(trackNum)%trackNum = trackNum
              midiF%tracks(trackNum)%midiF    => midiF
              arrSize                         = size(this%hexas)
          
              call midiF%tracks(trackNum)%BuildTrack(this%hexas, this%binaries, arrSize)
              
          end if    
      end if    
      
      call this%DeAllocateChunk()
   
   end subroutine
     
   subroutine setMidiTiming(midiF, timingData)
     class(midiFile), intent(inout)     :: midiF
     character(len=16)                  :: timingData
     integer                            :: index
     
     
     if (timingData(1:1) == "0") then
         read(timingData, "(B16)") midiF%TPQN 
         midiF%divisionMode = .FALSE.
     else
         timingData(2:8) = flipTheBits(timingData(2:8), 7)
         read(timingData(2:8),"(B7)") midiF%fps
         midiF%fps = midiF%fps + 1
         
         read(timingData(9:16),"(B8)") midiF%ptf
         midiF%divisionMode = .TRUE.
         
     end if    
   
   end subroutine
   
   function flipTheBits(input, L) result(output)
       integer            :: index, L
       character(len = L) :: input, output
       
       do index = 1, L, 1
          if (input(index:index) == "0") then
             output(index:index) = "1"
          else
             output(index:index) = "0"
          end if             
       end do    
   
   end function
   
   !
   ! ChunkList Routines
   !
   
   subroutine InitList(this)
      class(chunkList), intent(inout) :: this
      integer                         :: stat
      
      if (allocated(this%listOfChunks) .EQV. .TRUE.) then
          call this%DeAllocateList() 
          deallocate(this%listOfChunks, stat = stat)
      end if    
      this%theSize = 16
      this%last    = 0
      
      allocate(this%listOfChunks(this%theSize), stat = stat)
      
   end subroutine
   
   subroutine NextChunk(this, hexArray, binArray, currentIndex)
      class(chunkList), intent(inout)       :: this
      character(len = 2), dimension(*)      :: hexArray 
      character(len = 8), dimension(*)      :: binArray 
      integer(kind = 8), intent(inout)      :: currentIndex
      integer(kind = 8)                     :: index
      
      if (this%theSize == this%last) call this%doubleSize()
      this%last = this%last + 1
           
      !open(13, file = "C:\Jaj.txt", position="append", action = "write")
      !write(13, "(I0)") this%last   
      !close(13)
      
      if (getASCIIFromBytes(hexArray(1:4), 4) .EQ. "MThd") then
         this%listOfChunks(this%last)%header = .TRUE.
      else
         this%listOfChunks(this%last)%header = .FALSE.
      end if
      
      this%listOfChunks(this%last)%theSize = getInt32FromBytes(hexArray(5:8))

      !OPEN(unit=12, file="szar.txt", position="append", action = "write")
      !WRITE(12, *) getASCIIFromBytes(hexArray(1:4), 4), currentIndex, this%listOfChunks(this%last)%theSize, hexArray(8:this%listOfChunks(this%last)%theSize + 8)
      !CLOSE(12)
      
      call this%listOfChunks(this%last)%allocateChunk()
      
      do index = 1, this%listOfChunks(this%last)%theSize, 1
         this%listOfChunks(this%last)%hexas(index)    = hexArray(index + 8)        
         this%listOfChunks(this%last)%binaries(index) = binArray(index + 8)
      end do    
      
      currentIndex = currentIndex + 8 + this%listOfChunks(this%last)%theSize
      
   end subroutine   
   
   function getASCIIFromBytes(hexArray, theSize) result(theText)
     integer(kind = 8)                      :: theSize
     integer                                :: index
     character(len = 2), dimension(theSize) :: hexArray 
     character(len = theSize)               :: theText
     integer(kind = 2)                      :: asNum
     
     do index = 1, theSize, 1
        READ(hexArray(index), "(Z2)") asNum
        theText(index:index) = char(asNum)
     end do    
   
   end function

   function getInt32FromBytes(hexArray) result(theNumber)
     integer                                    :: index, subIndex
     character(len = 2), dimension(4)           :: hexArray 
     character(len = 8)                         :: theText
     integer(kind = 8)                          :: theNumber
     
     do index = 1, 4, 1
        subIndex = (index - 1) * 2 + 1  
        theText(subIndex  :subIndex  ) = hexArray(index)(1:1)
        theText(subIndex+1:subIndex+1) = hexArray(index)(2:2)       
     end do    
   
     read(theText, "(Z8)") theNumber
     
   end function 
     
   subroutine DoubleSize(this)
      class(chunkList), intent(inout)        :: this
      integer                                :: stat
      integer(kind = 8)                      :: index, subIndex, oldSize
      type(chunk), dimension(:), allocatable :: tempList

      allocate(tempList(this%theSize), stat = stat)   
      
      do index = 1, this%theSize, 1
         tempList(index)%header    = this%listOfChunks(index)%header 
         tempList(index)%theSize   = this%listOfChunks(index)%theSize
         !tempList(index)%trackNum  = this%listOfChunks(index)%trackNum

         call templist(index)%allocateChunk()   
         
         do subIndex = 1, this%listOfChunks(index)%theSize, 1
            tempList(index)%hexas(subIndex)    = this%listOfChunks(index)%hexas(subIndex)
            tempList(index)%binaries(subIndex) = this%listOfChunks(index)%binaries(subIndex)
         end do    
         
         call this%listOfChunks(index)%deAllocateChunk()
      end do    
   
      oldSize      = this%theSize   
      this%theSize = this%theSize * 2
      
      deallocate(this%listOfChunks               , stat = stat)
      allocate(  this%listOfChunks(this%theSize) , stat = stat)
      
      do index = 1, oldSize, 1
         this%listOfChunks(index)%header   = tempList(index)%header
         this%listOfChunks(index)%theSize  = tempList(index)%theSize
         !this%listOfChunks(index)%trackNum = tempList(index)%trackNum

         call this%listOfChunks(index)%allocateChunk()   
         
         do subIndex = 1, tempList(index)%theSize, 1
            this%listOfChunks(index)%hexas(subIndex)    = tempList(index)%hexas(subIndex)
            this%listOfChunks(index)%binaries(subIndex) = tempList(index)%binaries(subIndex)  
         end do    
         
         call tempList(index)%deAllocateChunk()
      end do 
      
      deallocate(tempList, stat = stat)
      
   end subroutine 
   
   subroutine DeAllocateList(this)
      class(chunkList), intent(inout) :: this
      integer                         :: index
        
      do index = 1, this%theSize, 1
         if (allocated(this%listOfChunks(index)%hexas)) then 
            call this%listOfChunks(index)%DeAllocateChunk()
         end if
      end do    
      
   end subroutine
     
   !
   ! MidiFile Routines
   !
   
   subroutine LoadFile(this, path)
     class(midiFile), intent(inout) :: this
     character(len = *)             :: path
     integer                        :: theSize, stat, trackNum
     integer(kind = 8)              :: index, subIndex, currentIndex
     
     if (debug .EQV. .TRUE.) then
         inquire(file = "midiDump.txt", exist = stat)
         if (stat .EQV. .TRUE.) then
             open(12, file = "midiDump.txt", action="read")
             close(12, status = "DELETE")
         end if
     end if    
     
     this%loaded = .FALSE.
     this%TPQN   = 0
     this%fps    = 0
     this%ptf    = 0
     
     if (dbg2 .EQV. .TRUE.) then
         open(37, file = "deltas.txt", action = "write")
         write(37, "(A)") ""
         close(37)
     end if 
     
     if (allocated(this%bytes)    .EQV. .TRUE.)   deallocate(this%bytes)  
     if (allocated(this%hexas)    .EQV. .TRUE.)   deallocate(this%hexas)  
     if (allocated(this%binaries) .EQV. .TRUE.)   deallocate(this%binaries)  
     
     inquire(file = path, size = theSize)
     this%numOfBytes = theSize
     
     allocate(this%bytes(theSize)   , stat = stat)
     allocate(this%hexas(theSize)   , stat = stat)
     allocate(this%binaries(theSize), stat = stat)

     OPEN(unit=11, file=path, access="stream", status="old", action="read",iostat=stat)     
     read(11, iostat=stat) this%bytes
     CLOSE(11)

     !
     !  Get pure hexString and binString data
     !
     do index = 1, theSize, 1
        WRITE(this%hexas(index)   , '(Z2)', iostat = stat) this%bytes(index)
        WRITE(this%binaries(index), '(B8)', iostat = stat) this%bytes(index)
        
        if (this%hexas(index)(1:1) == " ") this%hexas(index)(1:1) = "0"
        if (this%hexas(index)(2:2) == " ") this%hexas(index)(2:2) = "0"
        
        do subIndex = 1, 8, 1
           if (this%binaries(index)(subindex:subindex) == " ") then
               this%binaries(index)(subindex:subindex) = "0"
           else
               exit
           end if     
               
        end do     
         
     end do    
     deallocate(this%bytes, stat = stat)   
     
     call this%chunks%initList()
     currentIndex = 1
    
     !
     !  Create chunks from binary: Chunk Type (Header / Track), Size, Data of Chunk
     !
     do while (currentIndex < size(this%hexas))        
        call this%chunks%NextChunk(this%hexas(currentIndex:size(this%hexas)), &
                                 & this%binaries(currentIndex:size(this%binaries)), currentIndex) 
         
     end do
         
     deallocate(this%hexas    , stat = stat)
     deallocate(this%binaries , stat = stat)

     !
     !  Create Header / Track Messages for each chunk!
     !
     open(13, file = "C:\leszarom.txt", action = "write")
     write(13, "(I0)") this%chunks%last
     close(13)
     
     trackNum = 0
     this%deltaSums = 0
     
     do currentIndex = 1, this%chunks%last, 1  
        call this%chunks%listOfChunks(currentIndex)%processChunk(this, trackNum)
     end do

     this%loaded = .TRUE.
     
   end subroutine
    
    
end module