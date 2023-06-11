module midi
    use iso_fortran_env
    use, intrinsic :: iso_c_binding  
    
    implicit none
    
    private
    public :: midiFile, LoadFile
    !public :: chunkList, initList
    !public :: chunk, allocateChunk, deAllocateChunk
       
    type chunk
        logical                                         :: header
        integer(kind = 8)                               :: theSize
        ! integer                                       :: trackNum
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
         integer(kind = 8)                              :: lenght
         character(len=2)                               :: typeAsHex
         character(len=20)                              :: typeAsText
         character(len=250)                             :: valueAsText
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
         character(len=2)                               :: typeAsHex
         character(len=20)                              :: typeAsText
         character(len=2), dimension(:), allocatable    :: valueAsHex
    end type
    
    type message   
        integer(kind = 8)                              :: deltaTime
        character(len = 2)                             :: messageType
        type(metaMessage)                              :: metaM
        type(sysMessage)                               :: sysM
        type(midiData)                               :: midiD
        
    end type    
        
    type track
        integer                                        :: trackNum
        integer(kind = 8)                              :: lastMessage = 0, arraySize
        type(message), dimension(:), allocatable       :: messages
      
        contains 
        procedure                                      :: buildTrack        => BuildTrack
        procedure                                      :: addNessage        => AddMessage
        procedure                                      :: doubleMe          => DoubleMe
        procedure                                      :: processAsMeta     => ProcessAsMeta
        procedure                                      :: processAsSysMes   => ProcessAsSysMes
        ! procedure                                      :: processAsMidiData => ProcessAsMidiData

    end type   
    
    
    type midiFile
        logical                                         :: loaded = .FALSE., divisionMode = .FALSE.
        integer                                         :: numOfBytes, midiType, numberOfTracks, TPQN, fps, ptf
        integer(kind=1) , dimension(:), allocatable     :: bytes 
        character(len=2), dimension(:), allocatable     :: hexas
        character(len=8), dimension(:), allocatable     :: binaries
        type(chunkList)                                 :: chunks
        type(track)     , dimension(:), allocatable     :: tracks
        
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

          select case(tempMessages(index)%messageType)
          case ("MT")    
               !tempMessages(index)%metaM  = this%messages(index)%metaM
               tempMessages(index)%metaM%lenght      = this%messages(index)%metaM%lenght 
               tempMessages(index)%metaM%typeAsHex   = this%messages(index)%metaM%typeAsHex 
               tempMessages(index)%metaM%typeAsText  = this%messages(index)%metaM%typeAsText 
               tempMessages(index)%metaM%valueAsText = this%messages(index)%metaM%valueAsText 
               
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
               
          end select    
       end do    
       
       this%arraySize = this%arraySize * 2 
       
       deallocate(this%messages                , stat = stat)
       allocate  (this%messages(this%arraySize), stat = stat)
       
       do index = 1, this%lastMessage, 1
          this%messages(index)%deltaTime   = tempMessages(index)%deltaTime
          this%messages(index)%messageType = tempMessages(index)%messageType

          select case(this%messages(index)%messageType)
          case ("MT")    
               !this%messages(index)%metaM  = tempMessages(index)%metaM
               this%messages(index)%metaM%lenght      = tempMessages(index)%metaM%lenght 
               this%messages(index)%metaM%typeAsHex   = tempMessages(index)%metaM%typeAsHex 
               this%messages(index)%metaM%typeAsText  = tempMessages(index)%metaM%typeAsText 
               this%messages(index)%metaM%valueAsText = tempMessages(index)%metaM%valueAsText 
               
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
     
     do while (foundLast .EQV. .FALSE.)
        index = index + 1
        if (binArray(index)(1:1) == "0") foundLast = .TRUE.
        tempString = binArray(messageDataIndex)(2:8) // trim(tempString)
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
       integer(kind = 8)                      :: index
       integer(kind = 8), intent(inout)       :: byteIndex
       integer(kind = 8)                      :: arrSize
       
       if (this%lastMessage == size(this%messages)) call this%doubleMe()
       this%lastMessage = this%lastMessage + 1
       
       call calculateVLQ(this%messages(this%lastMessage)%deltaTime, binArray, byteIndex)
       
       select case(hexarray(byteIndex))
       ! Meta Message
       case("FF")    
           this%messages(this%lastMessage)%messageType = "MT"
           byteIndex = byteIndex + 1
           
           call this%processAsMeta(byteIndex, hexArray, binArray, arrSize)
       ! System Exclusive Message
       case("F0")
           this%messages(this%lastMessage)%messageType = "SE"
           byteIndex = byteIndex + 1
           call this%processAsSysMes(byteIndex, hexArray, binArray, arrSize)

       ! Midi Message    
       case default    
           this%messages(this%lastMessage)%messageType = "MD"
           byteIndex = byteIndex + 1
           
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
       text = "Invalid" 
   end select
   
   end function
   
   subroutine ProcessAsSysMes(this, byteIndex, hexArray, binArray, arrSize)
       class(track), intent(inout)                   :: this
       character(len = 2), dimension(*)              :: hexArray
       character(len = 8), dimension(*)              :: binArray  
       integer                                       :: stat
       integer(kind = 8)                             :: index, saveIndex, tempLen
       integer(kind = 8), intent(inout)              :: byteIndex
       integer(kind = 8)                             :: arrSize
       character(len = 2), dimension(:), allocatable :: tempArr
 
       
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
           tempLen = tempLen - 1
           
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
           
           do index = this%messages(this%lastMessage)%sysM%lenght, this%messages(this%lastMessage)%sysM%lenght + tempLen, 1
              this%messages(this%lastMessage)%sysM%valueAsHex(index) = hexArray(byteIndex)
              byteIndex                                              = byteIndex + 1
           end do     
           
           this%messages(this%lastMessage)%sysM%lenght = this%messages(this%lastMessage)%sysM%lenght + tempLen
           
       end do
       
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
       text = "Invalid" 
   end select
   
   end function
   
   subroutine ProcessAsMeta(this, byteIndex, hexArray, binArray, arrSize)
       class(track), intent(inout)            :: this
       character(len = 2), dimension(*)       :: hexArray
       character(len = 8), dimension(*)       :: binArray  
       integer                                :: stat
       integer(kind = 8)                      :: index, saveIndex
       integer(kind = 8), intent(inout)       :: byteIndex
       integer(kind = 8)                      :: arrSize
       
       this%messages(this%lastMessage)%metaM%typeAsHex = hexArray(byteIndex)
       byteIndex = byteIndex + 1
       this%messages(this%lastMessage)%metaM%typeAsText = getMetaTypeFromHex(this%messages(this%lastMessage)%metaM%typeAsHex)
       
       this%messages(this%lastMessage)%metaM%lenght = 0
       call calculateVLQ(this%messages(this%lastMessage)%metaM%lenght, binArray, byteIndex)

       if (this%messages(this%lastMessage)%metaM%lenght > 0) then
           allocate(this%messages(this%lastMessage)%metaM%valueAsHex(this%messages(this%lastMessage)%metaM%lenght), stat = stat)
       end if
       this%messages(this%lastMessage)%metaM%valueAsText = ""
       
       allocate(this%messages(this%lastMessage)%metaM%valueAsHex(this%messages(this%lastMessage)%metaM%lenght), stat = stat)
       
       saveIndex = 1
       do index = byteIndex, this%messages(this%lastMessage)%metaM%lenght + byteIndex, 1
          this%messages(this%lastMessage)%metaM%valueAsHex(saveIndex) = hexArray(index)  
          saveIndex = saveIndex + 1  
       end do
       
       this%messages(this%lastMessage)%metaM%valueAsText = getASCIIFromBytes(this%messages(this%lastMessage)%metaM%valueAsHex, &
                                                                           & this%messages(this%lastMessage)%metaM%lenght)
       
       byteIndex = byteIndex + this%messages(this%lastMessage)%metaM%lenght + 1
       
   end subroutine    
       
   subroutine BuildTrack(this, hexArray, binArray, arrSize)
       class(track), intent(inout)            :: this
       character(len = 2), dimension(*)       :: hexArray
       character(len = 8), dimension(*)       :: binArray  
       integer                                :: stat
       integer(kind = 8)                      :: byteIndex
       integer(kind = 8)                      :: arrSize

       this%lastMessage = 0
       if (allocated(this%messages) .EQV. .FALSE.) then
           allocate(this%messages(64), stat = stat)
           this%arraySize = size(this%messages)
       end if 
   
       byteIndex        = 1
       do while (byteIndex < arrSize) 
          call this%addNessage(byteIndex, hexArray, binArray, arrSize)
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
   
   subroutine ProcessChunk(this, midiF)
      class(chunk), intent(inout)        :: this
      class(midiFile), intent(inout)     :: midiF
      character(len=2), dimension(4)     :: tempArray
      integer                            :: stat, trackNum
      integer(kind = 8)                  :: arrSize
      
      tempArray = (/ "00", "00", "00", "00" /)
      trackNum = 0
      
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
          trackNum                        = trackNum + 1
          midiF%tracks(trackNum)%trackNum = trackNum
          arrSize                         = size(this%hexas)
          call midiF%tracks(trackNum)%BuildTrack(this%hexas, this%binaries, arrSize)
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
     integer                        :: theSize, stat
     integer(kind = 8)              :: index, subIndex, currentIndex
     
     this%loaded = .FALSE.
     this%TPQN   = 0
     this%fps    = 0
     this%ptf    = 0 
     
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
     
     do currentIndex = 1, this%chunks%last, 1
        call this%chunks%listOfChunks(currentIndex)%processChunk(this)
     end do
          
   end subroutine
    
    
end module