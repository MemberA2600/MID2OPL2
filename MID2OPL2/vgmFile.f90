module VGM
    use player
    use soundbank
    
    implicit none
    
    private
    public                                         :: vgmFile, buildVGM, done
    logical                                        :: first = .TRUE., done
    logical, parameter                             :: debug = .FALSE., saveRaw = .TRUE. 
    
    type(midiPlayer), pointer                      :: midiP
    type(soundB)    , pointer                      :: sBank   
    
    type VGMHeader
        !
        ! Offsets are the absolute position - the current position - 1 (because it starts with 0)
        ! If data starts at byte 129 (or 128, in C), the offset should be 4C (76), since the byte position
        ! is 53 (or 52).
        !
        !  Volume changer should be controllable in the future.
        !
        integer(kind = 8)                          :: eofOffset = 0, gd3Offset = 0, allWaitSamples = 0, dataOffset = z'4c' 
        integer(kind = 1)                          :: volume = 0 
        integer(kind = 8)                          :: gd3Size, gd3Last
        
        !
        ! Based on Monkey Island intro VGM.
        !
        integer(kind = 1), dimension(128)          :: headerBytes = (/&
                                                    & z'56', z'67', z'6D', z'20', z'4F', z'A0', z'01', z'00', &
                                                    & z'51', z'01', z'00', z'00', z'00', z'00', z'00', z'00', & 
                                                    & z'00', z'00', z'00', z'00', z'69', z'9F', z'01', z'00', & 
                                                    & z'64', z'59', z'42', z'00', z'00', z'00', z'00', z'00', &
                                                    & z'00', z'00', z'00', z'00', z'E8', z'03', z'00', z'00', & 
                                                    & z'00', z'00', z'00', z'00', z'00', z'00', z'00', z'00', &
                                                    & z'00', z'00', z'00', z'00', z'4C', z'00', z'00', z'00', &
                                                    & z'00', z'00', z'00', z'00', z'00', z'00', z'00', z'00', &
                                                    & z'00', z'00', z'00', z'00', z'00', z'00', z'00', z'00', &
                                                    & z'00', z'00', z'00', z'00', z'00', z'00', z'00', z'00', &
                                                    & z'99', z'9E', z'36', z'00', z'00', z'00', z'00', z'00', &
                                                    & z'00', z'00', z'00', z'00', z'00', z'00', z'00', z'00', &
                                                    & z'00', z'00', z'00', z'00', z'00', z'00', z'00', z'00', &
                                                    & z'00', z'00', z'00', z'00', z'00', z'00', z'00', z'00', &
                                                    & z'00', z'00', z'00', z'00', z'00', z'00', z'00', z'00', &
                                                    & z'00', z'00', z'00', z'00', z'00', z'00', z'00', z'00' /)
        
        integer(kind = 1), dimension(:), allocatable &
                                                   & :: gd3Bytes
        
        contains
        procedure                                   :: changeHeader => changeHeader
        procedure                                   :: doubleGD3    => doubleGD3
        procedure                                   :: addGD3       => addGD3
        
    end type    
    
    type byteTriple
        integer(kind = 2)                           :: command, register, value
    end type    
       
    type VGMData
        type(byteTriple), dimension(:), allocatable :: byteTriples
        integer(kind = 8)                           :: lastOne = 0  
    end type
    
    type deltaAndBytes
        type(byteTriple), dimension(14)             :: byteTriples
        integer(kind = 8)                           :: startDelta = 0, endDelta = 0
        integer(kind = 2)                           :: lastOne = 0  
        integer(kind = 2)                           :: tempo
        contains
        
        procedure                                   :: addNote  => addNote
        
    end type    
        
    type dataPointer
        type(deltaAndBytes), pointer                :: p
        integer(kind = 8)                           :: startDelta   = 0
        integer(kind = 8)                           :: framesBefore = 0
        logical                                     :: noteOn       = .FALSE.
        
    end type    
    
    type dataChannel
        type(deltaAndBytes), dimension(:), allocatable :: timedData
        integer(kind = 8)                              :: lastOne  = 0  
        integer(kind = 2)                              :: currInst = 0 
        logical                                        :: firstNote = .TRUE.
        
        contains
        
        procedure                                      :: addData => addData 
        
        end type 
    
    type dataPointerChannel
        type(dataPointer), dimension(:), allocatable   :: timedData
        integer(kind = 8)                              :: lastOne  = 0, size = 0, tempStartIndex = 1        
        
        contains
        procedure                                      :: AddByteP     => AddByteP 
        procedure                                      :: getPosition  => getPosition 
        
    end type     
        
    type byteList
        integer(kind = 1), dimension(:), allocatable   :: bytes 
        integer(kind = 8)                              :: lastOne = 0, theSize = 0 
        
        contains
        procedure                                      :: doubleMe   => doubleMe
        procedure                                      :: addBytes   => addBytes
        procedure                                      :: addTriple  => addTriple
        procedure                                      :: addOne     => addOne
        procedure                                      :: initMe     => initMe
        
    end type    
        
    type vgmFile
        type(VGMHeader)                                :: header
        type(VGMData)                                  :: chipData
        type(dataChannel), dimension(9)                :: dataChannels 
        type(dataPointerChannel)                       :: pointers
        type(byteList)                                 :: bList 
        
        contains
        procedure                                      :: buildVGM         => buildVGM
             
   end type
    
   contains     
    
   subroutine addGD3(this, tag_, double)
      class(vgmHeader), intent(inout)                 :: this
      character(len = 500)                            :: tag  
      character(len = 500), intent(in)                :: tag_        
      integer(kind = 2)                               :: index, num, subIndex, lenOfText, stat, letterIndex
      character(len = 4)                              :: asHex
      character(len = 2), dimension(:), allocatable   :: hexas
      character(:), allocatable                       :: string 
      logical, intent(in)                             :: double
      logical                                         :: again
      
      again = double
      
      tag = tag_
      
      lenOfText = 0
      do index = 500, 1, -1
         if (tag(index:index) /= " " .AND. ichar(tag(index:index)) /= 0 .AND. ichar(tag(index:index)) /= 32) then
            lenOfText = index   
            exit
         end if 
      end do     
            
      if (allocated(hexas) .EQV. .TRUE.) deallocate(hexas, stat = stat)
      
      if (lenOfText > 0) then
          do while(tag(1:1) == " ") 
             tag(1:lenOfText-1) = tag(2:lenOfText) 
             lenOfText = lenOfText - 1
             
             if (lenOfText == 0) exit
          end do    
          
          call debugLog("Converting tag: " // tag(1:lenOfText) // " Len: " // trim(numToText(lenOfText)))
      
          allocate(hexas((lenOfText * 2) + 2), stat = stat)
          letterIndex = 1
          
          do index = 1, lenOfText, 1
             num   = ichar(tag(index:index), 2) 
             write(asHex, "(Z4)") num
         
             do subIndex = 1, 4, 1
                if (asHex(subIndex : subIndex) == " ") asHex(subIndex : subIndex) = "0" 
             end do    
         
             hexas(letterIndex)   = asHex(3:4)
             hexas(letterIndex+1) = asHex(1:2)
             
             letterIndex          = letterIndex + 2 
             ! call debugLog("Letter: " // tag(index:index) // " " // trim(numToText(num)) // " " // asHex) 
         
          end do
      else
          allocate(hexas(2), stat = stat)
          letterIndex = 1
      end if  
      
      hexas(letterIndex)   = "00"
      hexas(letterIndex+1) = "00"
      
      letterIndex = letterIndex + 1
      
911   &
      if (this%gd3Size < this%gd3Last + letterIndex) call this%doubleGD3()
      
      do index = 1, letterIndex, 1
         string = string // hexas(index) // " "
         this%gd3Last = this%gd3Last + 1
         read(hexas(index), "(Z2)") this%gd3Bytes(this%gd3Last)
      end do     
      
      call debugLog("As bytes: " // string)
      
      if (again .EQV. .TRUE.) then
          again = .FALSE.
          goto 911            
      end if
      string = ""
      
   end subroutine
    
   subroutine doubleGD3(this)
      class(vgmHeader), intent(inout)                 :: this
      integer(kind = 2)                               :: stat     
      integer(kind = 1), dimension(:), allocatable    :: tempBytes
      integer(kind = 8)                               :: index
      
      if (allocated(tempBytes) .EQV. .TRUE.) deallocate(tempBytes, stat = stat) 
      
      allocate(tempBytes(this%gd3Size), stat = stat)
      
      do index = 1, this%gd3Last, 1
         tempBytes(index) = this%gd3Bytes(index)  
      end do    
      
      deallocate(this%gd3Bytes, stat = stat)
      allocate(this%gd3Bytes(this%gd3Size * 2), stat = stat)
      
      do index = 1, this%gd3Last, 1
         this%gd3Bytes(index) = tempBytes(index)  
      end do   
      
      this%gd3Size = this%gd3Size * 2
      
      deallocate(tempBytes, stat = stat) 
      
   end subroutine
    
   subroutine changeHeader(this, offset, value)
       class(vgmHeader), intent(inout)                 :: this
       integer(kind = 1)                               :: offset 
       character(len = 8)                              :: bytes4 
       integer(kind = 2)                               :: stat, index 
       integer(kind = 8)                               :: value 
       integer(kind = 1), dimension(4)                 :: theBytes                                 
       
       write(bytes4, "(Z8)") value
       
       do index = 1, 4, 1
          read(bytes4((index * 2) - 1 : (index * 2) ), "(Z2)") theBytes(5 - index)
       end do    
       
       do index = 1, 4, 1
          this%headerBytes(index + offset) = theBytes(index) 
       end do    
           
   end subroutine
    
   subroutine doubleMe(this) 
        class(byteList), intent(inout)                 :: this
        integer(kind = 1), dimension(:), allocatable   :: tempBytes 
        integer(kind = 2)                              :: stat 
        integer(kind = 8)                              :: index 
        
        if (allocated(tempBytes) .EQV. .TRUE. ) deallocate(tempBytes, stat = stat)
        allocate(tempBytes(this%theSize), stat = stat)
        
        do index = 1, this%theSize, 1
           tempBytes(index)   =   this%bytes(index) 
        end do    
        
        deallocate(this%bytes, stat = stat)
        allocate(this%bytes(this%theSize * 2), stat = stat)
        
        do index = 1, this%theSize, 1
           this%bytes(index) = tempBytes(index) 
        end do   
        
        this%theSize = this%theSize * 2 
        deallocate(tempBytes, stat = stat)
        
   end subroutine     

   subroutine addBytes(this, bytes, waits) 
        class(byteList), intent(inout)         :: this
        integer(kind = 8), intent(inout)       :: waits 
        type(dataPointer)                      :: bytes 
        integer(kind = 1), dimension(3)        :: threeBytes
        integer(kind = 2)                      :: index
        integer(kind = 1)                      :: single
        character(len= 2)                      :: hexByte
        integer(kind = 8)                      :: buffer 
        character(len = 4)                     :: hex4 
               
        if (bytes%FramesBefore > 0) then
            buffer = bytes%FramesBefore

            do while(buffer >= 65535)
               threeBytes(1) = z'61'
               threeBytes(2) = z'FF'
               threeBytes(3) = z'FF'
                        
               call this%addTriple(threeBytes)
               call debugLog("Wait 65535 samples!")        
               buffer = buffer - 65535
               waits  = waits + 65535
            end do  

            waits  = waits + buffer
            
            if (buffer > 0 ) then
                if (buffer < 16) then
                    hexByte      = numToHex(buffer - 1)
                    hexByte(1:1) = "7" 
                
                    read(hexByte, "(Z2)") single
                    call this%addOne(single)
                    call debugLog("Wait " // trim(numToText(buffer)) // " samples!")   
                    
                else
                    select case(buffer)
                    case(735)
                         call this%addOne(z'62')   
                         call debugLog("Wait 735 samples!") 

                    case(882)
                         call this%addOne(z'63')
                         call debugLog("Wait 882 samples!") 

                    case default    
                        threeBytes(1) = z'61'
                        
                        hex4 = ""
                        write(hex4, "(Z4)") buffer
                        
                        do index = 1, 4, 1
                           if (hex4(index : index) == " ") hex4(index : index) = "0"
                        end do    
                        
                        read(hex4(3:4), "(Z2)") threeBytes(2) 
                        read(hex4(1:2), "(Z2)") threeBytes(3)
                        

                        call this%addTriple(threeBytes) 
                        call debugLog("Wait " // trim(numToText(buffer)) // " samples!") 

                    end select    
                end if    
            end if    
            
        end if
        
        if (bytes%noteOn .EQV. .FALSE.) then
            call debugLog("-- Note Off --") 
            threeBytes(1) = bytes%p%byteTriples(bytes%p%lastOne)%command
            threeBytes(2) = bytes%p%byteTriples(bytes%p%lastOne)%register
            threeBytes(3) = bytes%p%byteTriples(bytes%p%lastOne)%value
        
            call this%addTriple(threeBytes)
        else
            call debugLog("-- Note On --") 
            do index = 1, bytes%p%lastOne - 1, 1
               threeBytes(1) = bytes%p%byteTriples(index)%command
               threeBytes(2) = bytes%p%byteTriples(index)%register
               threeBytes(3) = bytes%p%byteTriples(index)%value 
                
               call this%addTriple(threeBytes)
            end do
        end if
        
   end subroutine        

   subroutine addOne(this, b)
        class(byteList), intent(inout)         :: this
        integer(kind = 1)                      :: b 
        integer(kind = 1)                      :: index 
        
        if ((this%lastOne + 1) > this%theSize) call this%doubleMe()  
        call debugLog(numToHex(b))
        
        this%bytes(this%lastOne + 1) = b
        this%lastOne = this%lastOne + 1 
        
   end subroutine   
   
   subroutine addTriple(this, bytes)
        class(byteList), intent(inout)         :: this
        integer(kind = 1), dimension(3)        :: bytes 
        integer(kind = 1)                      :: index 
        
        if ((this%lastOne + 3) > this%theSize) call this%doubleMe()  
        !call debugLog(trim(numToText(bytes(1))) // " | " // trim(numToText(bytes(2))) // " | " // trim(numToText(bytes(3))))
        call debugLog(numToHex(bytes(1)) // " | " // numToHex(bytes(2)) // " | " // numToHex(bytes(3)))
        
        do index = 1, 3, 1
           this%bytes(index + this%lastOne) = bytes(index) 
        end do    
        
        this%lastOne = this%lastOne + 3 
        
   end subroutine
    
   subroutine initMe(this)
        class(byteList), intent(inout)         :: this
        integer(kind = 2)                      :: stat 
        integer(kind = 2), parameter           :: defSize = 256
        integer(kind = 1), dimension(3)        :: bytes
        
        if (allocated(this%bytes) .EQV. .TRUE.) deallocate(this%bytes, stat = stat)
        allocate(this%bytes(defSize), stat = stat)
        
        this%theSize = defSize
        this%lastOne = 0
        
        call debugLog("/// Write the Output bytes! \\\")
        
        !
        !  Check if Y3812
        !
        bytes = (/ z'5a', z'01', z'20' /)
        call this%addTriple(bytes)

        !
        ! Turn off speech synthesis
        !
        bytes = (/ z'5a', z'08', z'00' /)
        call this%addTriple(bytes)

        !
        ! Turn off usage of built-in percussion
        !
        bytes = (/ z'5a', z'BD', z'00' /)
        call this%addTriple(bytes)
         
        
   end subroutine
      
   subroutine getPosition(this, insertIndex, deltaTime, word, insert)
       class(dataPointerChannel), intent(inout)         :: this
       integer(kind = 8), intent(inout)                 :: insertIndex
       logical, intent(inout)                           :: insert          ! False = Add, True = Instert
       integer(kind = 8), intent(in)                    :: deltaTime
       character(len = 15), intent(inout)               :: word 
       integer(kind = 8)                                :: tempIndex
       
       insertIndex = this%lastOne+1 
       
       if (this%lastOne > 0) then
           insertIndex = 1
           do tempIndex = this%lastOne, 1, -1
             if (deltaTime < this%timedData(tempIndex)%startDelta) then
                cycle
             else
                insertIndex = tempIndex + 1
                exit
             end if     
           end do 
       end if    
       
       insert = (insertIndex <= this%lastOne)
       
       if (insert .EQV. .TRUE.) then           
           do tempIndex = this%lastOne+1, insertIndex + 1, -1
              this%timedData(tempIndex)%p            => this%timedData(tempIndex-1)%p 
              this%timedData(tempIndex)%startDelta   =  this%timedData(tempIndex-1)%startDelta
              this%timedData(tempIndex)%framesBefore =  0
              this%timedData(tempIndex)%noteOn       =  this%timedData(tempIndex-1)%noteOn    
           end do 
       end if   
       
       if (insert .EQV. .TRUE.) then
           word = "Inserted"
       else
           word = "Added"
       end if
       
   end subroutine
   
   subroutine AddByteP(this, note)
       class(dataPointerChannel), intent(inout)         :: this
       type(deltaAndBytes), intent(in), target          :: note
       !type(dataPointer), dimension(:), allocatable     :: tempData

       integer(kind = 2)                                :: stat, byteIndex
       integer(kind = 8)                                :: insertIndex, tempIndex
       logical                                          :: insert = .FALSE.     ! False = Add, True = Instert
       character(len = 15)                              :: word      
       
       insertIndex = 1
       insert      = .FALSE.
       
       call this%getPosition(insertIndex, note%startDelta, word, insert) 
           
       call debugLog(trim(word) // " Note ON bytes to position #" // trim(numToText(insertIndex)) // "!")
       
       this%lastOne                             =  this%lastOne + 1
       
       this%timedData(insertIndex)%startDelta   =  note%startDelta
       this%timedData(insertIndex)%framesBefore =  0
       this%timedData(insertIndex)%noteOn       =  .TRUE.
       this%timedData(insertIndex)%p            => note
       
       call debugLog("Deltatime: " // trim(numToText(note%startDelta)))
       call debugLog("Byte(s):" )
       do byteIndex = 1, note%lastOne - 1, 1
          call debugLog(numToHex(note%byteTriples(byteIndex)%register) // " | " // numToHex(note%byteTriples(byteIndex)%value))
       end do 
              
       call this%getPosition(insertIndex, note%endDelta, word, insert) 

       call debugLog(trim(word) // " Note OFF bytes to position #" // trim(numToText(insertIndex)) // "!")

       this%lastOne                             = this%lastOne + 1
       
       this%timedData(insertIndex)%startDelta   =  note%endDelta
       this%timedData(insertIndex)%framesBefore =  0
       this%timedData(insertIndex)%noteOn       =  .FALSE.
       this%timedData(insertIndex)%p            => note

       call debugLog("Deltatime: " // trim(numToText(note%endDelta)))
       call debugLog("Byte(s):" )
       call debugLog(numToHex(note%byteTriples(note%lastOne)%register) // " | " // numToHex(note%byteTriples(note%lastOne)%value))
       call debugLog("-----------------------------------------------------------------------")

   end subroutine
    
   subroutine buildVGM(this, midiPP, sBankP, tags, path) 
       use player
       use soundBank
       
       class(vgmFile),   intent(inout)                :: this
       type(soundB),     intent(in), target           :: sBankP
       type(midiPlayer), intent(in), target           :: midiPP
       
       integer(kind = 2)                              :: stat, channelIndex
       integer(kind = 8)                              :: noteIndex, offset
       logical                                        :: double = .FALSE.
       
       character(len = 8)                             :: lenAsHex  
       character(len = 500), dimension(7)             :: tags
       
       integer(kind = 1), dimension(:), allocatable   :: outBytes
       character(len = 500)                           :: path  
       real(kind = 8)                                 :: fraction, frames  
       
       midiP => midiPP
       sBank => sBankP
       
       first = .TRUE.
       done = .FALSE.
       
       if (allocated(this%chipData%byteTriples) .EQV. .TRUE.) deallocate(this%chipData%byteTriples, stat = stat)
       this%chipData%lastOne      = 0
       this%header%eofOffset      = 0
       this%header%gd3Offset      = 0
       this%header%allWaitSamples = 0
       this%header%dataOffset     = z'4c'
       
       do channelIndex = 1, 9, 1
          if (allocated(this%dataChannels(channelIndex)%timedData) .EQV. .TRUE.) deallocate(this%dataChannels(channelIndex)%timedData, stat = stat) 
          this%dataChannels(channelIndex)%lastOne   = 0        
          this%dataChannels(channelIndex)%currInst  = 0     
          this%dataChannels(channelIndex)%firstNote = .TRUE.     
          
          if (midiP%notePointerChannels(channelIndex)%lastOne > 0) then
              allocate(this%dataChannels(channelIndex)%timedData(midiP%notePointerChannels(channelIndex)%lastOne), stat = stat)
              
              do noteIndex = 1, midiP%notePointerChannels(channelIndex)%lastOne, 1
                 call this%dataChannels(channelIndex)%addData(midiP%notePointerChannels(channelIndex)%notePointers(noteIndex), channelIndex) 
              end do    
          end if          
       end do    
       
       this%pointers%lastOne = 0
       this%pointers%size    = 0

       do channelIndex  = 1, 9, 1
          this%pointers%size = this%pointers%size + this%dataChannels(channelIndex)%lastOne 
       end do 
       
       !
       ! Note On and Off is different pointer now. 
       !
       this%pointers%size = this%pointers%size * 2
       
       if (allocated(this%pointers%timedData) .EQV. .TRUE.) deallocate(this%pointers%timedData, stat = stat)
       allocate(this%pointers%timedData(this%pointers%size), stat = stat) 
       
       call debugLog("---> Align the data into one channel")
       
       do channelIndex = 1, 9, 1
          call debugLog(">>> Channel #" // trim(numToText(channelIndex)) // ":")
          this%pointers%tempStartIndex = 1
          do noteIndex = 1, this%dataChannels(channelIndex)%lastOne, 1 
             call debugLog("Addig Data Bytes Array #" // trim(numToText(noteIndex)) // " of Channel #" // trim(numToText(channelIndex)) // "!") 
             call this%pointers%AddByteP(this%dataChannels(channelIndex)%timedData(noteIndex)) 
          end do   
       end do    
       
       call this%bList%initMe()
       
       fraction = 0
       
       do noteIndex = 1, this%pointers%lastOne, 1
          if (noteIndex == 1) then
              frames = &
            & deltaToSamples(this%pointers%timedData(noteIndex)%startDelta, this%pointers%timedData(noteIndex)%p%tempo)
          else                 
              frames = &
            & deltaToSamples((this%pointers%timedData(noteIndex)%startDelta - this%pointers%timedData(noteIndex-1)%startDelta), &
            & this%pointers%timedData(noteIndex)%p%tempo)
              
          end if     
          
          frames = frames + fraction
          this%pointers%timedData(noteIndex)%framesBefore = frames
          
          fraction = frames - this%pointers%timedData(noteIndex)%framesBefore
          
          call debugLog("---||" //  trim(numToText(noteIndex)) // "||---") 
          call this%bList%addBytes(this%pointers%timedData(noteIndex), this%header%allWaitSamples)   
       end do
       
       call this%bList%addOne(z'66')
       
       this%header%gd3Offset = this%bList%lastOne + 128 - 20
       
       if (allocated(this%header%gd3Bytes) .EQV. .TRUE.) deallocate(this%header%gd3Bytes, stat = stat) 
       
       this%header%gd3Size     = 256
       this%header%gd3Last     = 12
       
       allocate(this%header%gd3Bytes(this%header%gd3Size), stat = stat)
       
       !
       !  'GD3 ' // Version Number (1.00) // 32 bit integer of size
       !
       this%header%gd3Bytes(1:12) = (/ z'47', z'64', z'33', z'20', z'00', z'01', z'00', z'00', z'00', z'00', z'00', z'00' /)
       
       do channelIndex = 1, 7, 1
          double = .TRUE. 
          if (channelIndex > 4) double = .FALSE.  
          call this%header%addGD3(tags(channelIndex), double) 
       end do    
       
       write(lenAsHex, "(Z8)") (this%header%gd3Last - 12)
       
       do channelIndex = 1, 8, 1
          if (lenAsHex(channelIndex : channelIndex) == " ") lenAsHex(channelIndex : channelIndex) = "0" 
       end do    
       
       do channelIndex = 1, 4, 1
          read(lenAsHex((2 * channelIndex) - 1 : (2 * channelIndex)), "(Z2)") this%header%gd3Bytes(8 + (5-channelIndex))  
       end do    
       
       this%header%eofOffset = this%bList%lastOne + this%header%gd3Last + 123

       call this%header%changeHeader(20, this%header%gd3Offset)
       call this%header%changeHeader(24, this%header%allWaitSamples)
       call this%header%changeHeader(4 , this%header%eofOffset)
       
       !write(lenAsHex, "(Z8)") this%header%allWaitSamples

       !open(76, file = "fos.txt", action = "write")
       !write(76, "(I0)") this%header%eofOffset
       !close(76)
       
       !do channelIndex = 1, 8, 1
       !   if (lenAsHex(channelIndex : channelIndex) == " ") lenAsHex(channelIndex : channelIndex) = "0" 
       !end do           
       
       !do channelIndex = 1, 4, 1
       !   read(lenAsHex((2 * channelIndex) - 1 : (2 * channelIndex)), "(Z2)") this%header%headerBytes(24 + (5-channelIndex))  
       !end do    
       
       if (allocated(outBytes) .EQV. .TRUE.) deallocate(outBytes, stat = stat)       
       allocate(outBytes(this%bList%lastOne + this%header%gd3Last + 128), stat = stat)
       
       outBytes = 0
       
       do noteIndex = 1, 128, 1
          outBytes(noteIndex) = this%header%headerBytes(noteIndex)
       end do    
       offset = 128
       
       do noteIndex = 1, this%bList%lastOne, 1
          outBytes(noteIndex + offset) = this%bList%bytes(noteIndex)
       end do
           
       offset = 128 + this%bList%lastOne
       
       do noteIndex = 1, this%header%gd3Last, 1
          outBytes(noteIndex + offset) = this%header%gd3Bytes(noteIndex)
       end do 

       done = .TRUE.
       
       open (53, file = path, access="stream", action="write", iostat=stat)     
       write (53, iostat=stat) outBytes
       CLOSE(53)
       
       deallocate(outBytes, stat = stat) 
       call midiP%deAllocator()
       
   end subroutine
   
   
   
   subroutine addData(this, noteP, channelIndex)
      use player 
   
      class(dataChannel)     , intent(inout)        :: this
      type(playerNotePointer), intent(in)           :: noteP
      logical                                       :: newIstrument = .FALSE.
      integer(kind = 2)                             :: channelIndex
      
      this%lastOne           = this%lastOne + 1
      newIstrument           = .FALSE.
      
      if (this%lastOne == 0) then
          newIstrument = .TRUE.
      else
          if (this%currInst /= noteP%p%instrument) newIstrument = .TRUE.
      end if
      
      call this%timedData(this%lastOne)%addNote(noteP, noteP%p%instrument, this%currInst, newIstrument, channelIndex, this%firstNote)
      this%currInst   = noteP%p%instrument
      
   end subroutine
   
   subroutine addNote(this, noteP, instru, old, newOne, channelIndex, firstNote)
      class(deltaAndBytes)   , intent(inout)        :: this
      type(playerNotePointer), intent(in)           :: noteP
      logical                                       :: newOne
      integer(kind = 2)                             :: instru, old
      type(instrument), pointer                     :: newI, oldI
      integer(kind = 2)                             :: dataIndex, slotIndex
      integer(kind = 2)                             :: channelIndex
      character(len = 8)                            :: tempByte
      integer(kind = 1)                             :: bitIndex, lenOfName
      character(len = 10)                           :: bits10      
      logical, intent(inout)                        :: firstNote
      character(len = 32)                           :: iName
      
      call debugLog("*** Adding New Note to Channel #" // trim(numToText(channelIndex)) // "!")
      
      lenOfName = 0
      iName     = sBank%instruments(noteP%P%instrument)%name
      
      do bitIndex = 1, len(iName), 1
         if (iName(bitIndex:bitIndex) /= " " .AND. &
            &iachar(iName(bitIndex:bitIndex)) /= 0) then
             lenOfName = bitIndex
         end if
      end do     
      
      call debugLog("Duration: " // trim(numToText(noteP%startDelta)) // "-" // trim(numToText(noteP%endDelta)) // " (" // &
                   & trim(numToText(noteP%endDelta-noteP%startDelta)) // ")")
      
      call debugLog("Instrument: " // trim(numToText(noteP%P%instrument)) // " (" // &
                                     & iName(1:lenOfName) // ")")
      
      call debuglog("NoteNum: " // trim(numToText(noteP%P%note)) // ", Octave: " // trim(numToText(noteP%P%octave)))
      call debuglog("Frequency: " // trim(realToText(noteP%P%freq)) // ", FreqNum: " // trim(numToText(noteP%P%fnumber)) // &
                  & " (" // numToBin(noteP%P%fnumber, 10) // ")")
            
      do dataIndex = 1, size(this%byteTriples), 1
         this%byteTriples(dataIndex)%command  = z'5a'  
         this%byteTriples(dataIndex)%register = 0  
         this%byteTriples(dataIndex)%value    = 0  
      end do    
      
      this%lastOne      = 0
      this%startDelta   = noteP%startDelta
      this%endDelta     = noteP%endDelta
      this%tempo        = noteP%p%tempo 
      
      if (newOne .EQV. .TRUE.) then
          newI => sBank%instruments(instru)     
          if (firstNote .EQV. .FALSE.) oldI => sBank%instruments(old)         
          
          call debuglog(">> Instrument Settings Updating! <<")
                    
          !
          ! This goto thing is an ugly hack, so it will never check the old instrument, 
          ! if we are on the very first one.
          !
          
          if (firstNote .EQV. .TRUE.) goto 1000
          
          if (oldI%feedback /= newI%feedback .OR. (oldI%doubleVoice .NEQV. newI%doubleVoice)) then
1000 &
              this%lastOne = this%lastOne + 1
              tempByte     = ""
                          
              this%byteTriples(this%lastOne)%register = z'C0' + (channelIndex - 1)
             
              write(tempByte(5:8), "(B4)") newI%feedback 
              
              do bitIndex = 1, 8, 1
                 if (tempByte(bitIndex:bitIndex) == " ") tempByte(bitIndex:bitIndex) = "0" 
              end do   
              
              if (newI%doubleVoice .EQV. .TRUE.) tempByte(8:8) = "1"

              read(tempByte, "(B8)") this%byteTriples(this%lastOne)%value

              call debugLog("Update Feedback / Double Voice on Channel #" // trim(numToText(channelIndex)) // ":")
              call debugLog("-Register: " // numToHex(this%byteTriples(this%lastOne)%register))
              call debugLog("-Value: "    // numToHex(this%byteTriples(this%lastOne)%value))

              if (firstNote .EQV. .TRUE.) goto 1001
          end if    
1001 &
          do slotIndex = 1, 2, 1
             if (firstNote .EQV. .TRUE.) goto 1002
          
             if (newI%byte1(slotIndex) /= oldI%byte1(slotIndex)) then 
1002 &
                 this%lastOne = this%lastOne + 1
   
                 this%byteTriples(this%lastOne)%register = getAddress(channelIndex, slotIndex, z'20')
                 this%byteTriples(this%lastOne)%value    = newI%byte1(slotIndex)
                 
                 call debugLog("Update Tremolo / Vibrato / Sustain / KSR / Multi on Channel #"&
                           & // trim(numToText(channelIndex)) // " Slot #" // trim(numToText(slotIndex)) // ":")
                 call debugLog("-Register: " // numToHex(this%byteTriples(this%lastOne)%register))
                 call debugLog("-Value: "    // numToHex(this%byteTriples(this%lastOne)%value))
                 
                 if (firstNote .EQV. .TRUE.) goto 1003
             end if
             
             if (newI%byte2(slotIndex) /= oldI%byte2(slotIndex)) then 
1003 &
                 this%lastOne = this%lastOne + 1
   
                 this%byteTriples(this%lastOne)%register = getAddress(channelIndex, slotIndex, z'60')
                 this%byteTriples(this%lastOne)%value    = newI%byte2(slotIndex)
                                 
                 call debugLog("Update Attack Rate / Decay Rate on Channel #"&
                           & // trim(numToText(channelIndex)) // " Slot #" // trim(numToText(slotIndex)) // ":")
                 call debugLog("-Register: " // numToHex(this%byteTriples(this%lastOne)%register))
                 call debugLog("-Value: "    // numToHex(this%byteTriples(this%lastOne)%value))
                 
                 if (firstNote .EQV. .TRUE.) goto 1004
             end if
             
             if (newI%byte3(slotIndex) /= oldI%byte3(slotIndex)) then 
1004 &
                 this%lastOne = this%lastOne + 1
   
                 this%byteTriples(this%lastOne)%register = getAddress(channelIndex, slotIndex, z'80')
                 this%byteTriples(this%lastOne)%value    = newI%byte3(slotIndex)
                 
                 call debugLog("Update Sustain level / Release rate on Channel #"&
                           & // trim(numToText(channelIndex)) // " Slot #" // trim(numToText(slotIndex)) // ":")
                 call debugLog("-Register: " // numToHex(this%byteTriples(this%lastOne)%register))
                 call debugLog("-Value: "    // numToHex(this%byteTriples(this%lastOne)%value))
                 
                 if (firstNote .EQV. .TRUE.) goto 1005
             end if             
             
             if (newI%byte4(slotIndex) /= oldI%byte4(slotIndex)) then 
1005 &
                 this%lastOne = this%lastOne + 1
   
                 this%byteTriples(this%lastOne)%register = getAddress(channelIndex, slotIndex, z'e0')
                 this%byteTriples(this%lastOne)%value    = newI%byte4(slotIndex)
                 
                 call debugLog("Update Waveform Select on Channel #"&
                           & // trim(numToText(channelIndex)) // " Slot #" // trim(numToText(slotIndex)) // ":")
                 call debugLog("-Register: " // numToHex(this%byteTriples(this%lastOne)%register))
                 call debugLog("-Value: "    // numToHex(this%byteTriples(this%lastOne)%value))
                 
                 if (firstNote .EQV. .TRUE.) goto 1006
             end if                      

             if (newI%byte5(slotIndex) /= oldI%byte5(slotIndex) .OR. newI%byte6(slotIndex) /= oldI%byte6(slotIndex)) then 
1006 &
                 this%lastOne = this%lastOne + 1
   
                 this%byteTriples(this%lastOne)%register = getAddress(channelIndex, slotIndex, z'40')
                                 
                 tempByte = ""
                 write(tempByte(1:2), "(B2)") newI%byte5(slotIndex)
                 write(tempByte(3:8), "(B6)") newI%byte6(slotIndex)
                 
                 do bitIndex = 1, 8, 1
                    if (tempByte(bitIndex:bitIndex) == " ") tempByte(bitIndex:bitIndex) = "0" 
                 end do   

                 call debugLog(tempByte // " " // trim(numToText(newI%byte5(slotIndex))) // " " // trim(numToText(newI%byte6(slotIndex))))
                 
                 read(tempByte, "(B8)") this%byteTriples(this%lastOne)%value
                 
                 call debugLog("Update Key Scale Level / Output Level on Channel #"&
                           & // trim(numToText(channelIndex)) // " Slot #" // trim(numToText(slotIndex)) // ":")
                 call debugLog("-Register: " // numToHex(this%byteTriples(this%lastOne)%register))
                 call debugLog("-Value: "    // numToHex(this%byteTriples(this%lastOne)%value))
                 
                 if (firstNote .EQV. .TRUE.) goto 1007
             end if   
1007 &
             
          end do    
          if (firstNote .EQV. .TRUE.) goto 1008
      end if

1008  &
      firstNote    = .FALSE.
          
      this%lastOne = this%lastOne + 1
      bits10       = ""
          
      write(bits10, "(B10)") noteP%p%fNumber 
      do bitIndex = 1, 10, 1
          if (bits10(bitIndex:bitIndex) == " ") bits10(bitIndex:bitIndex) = "0"
      end do  
         
      ! low byte    
      this%byteTriples(this%lastOne)%register = z'A0' + (channelIndex - 1)
      read(bits10(3:10), "(B8)") this%byteTriples(this%lastOne)%value

      call debugLog("Set New Frequency Low Byte on Channel #" // trim(numToText(channelIndex)) // ":")
      call debugLog("-Register: " // numToHex(this%byteTriples(this%lastOne)%register))
      call debugLog("-Value: "    // numToHex(this%byteTriples(this%lastOne)%value))
          
      this%lastOne = this%lastOne + 1
      ! high byte
          
      this%byteTriples(this%lastOne)%register = z'B0' + (channelIndex - 1)
      tempByte      = "00000000"
      tempByte(7:8) = bits10(1:2)
         
      write(tempByte(4:6), "(B3)") noteP%p%octave
      do bitIndex = 1, 8, 1
          if (tempByte(bitIndex:bitIndex) == " ") tempByte(bitIndex:bitIndex) = "0"
      end do  
          
      tempByte(3:3) = "1"
      read(tempByte, "(B8)") this%byteTriples(this%lastOne)%value
                
      call debugLog("Set New Frequency High Byte / Octave / None ON on Channel #" // trim(numToText(channelIndex)) // ":")
      call debugLog("-Register: " // numToHex(this%byteTriples(this%lastOne)%register))
      call debugLog("-Value: "    // numToHex(this%byteTriples(this%lastOne)%value))
          
      ! Note Off Byte
      this%lastOne = this%lastOne + 1
          
      tempByte(3:3) = "0"
      read(tempByte, "(B8)") this%byteTriples(this%lastOne)%value
      this%byteTriples(this%lastOne)%register = this%byteTriples(this%lastOne-1)%register    
      
      call debugLog("Set New Frequency High Byte / Octave / None OFF on Channel #" // trim(numToText(channelIndex)) // ":")
      call debugLog("-Register: " // numToHex(this%byteTriples(this%lastOne)%register))
      call debugLog("-Value: "    // numToHex(this%byteTriples(this%lastOne)%value))
          
      call debugLog("**************************************************")
      
   end subroutine
   
   function getAddress(channel, slot, base) result(address)
      integer(kind = 2)                :: address   
      integer(kind = 1)                :: channel, slot
      integer(kind = 2)                :: index  
      integer(kind = 2)                :: base 
      
      integer(kind = 1), dimension(22) :: channels, slots
      
      channels = (/ 1, 2, 3, 1, 2, 3, -1, -1, 4, 5, 6, 4, 5, 6, -1, -1, 7, 8, 9, 7, 8, 9 /)
      slots    = (/ 1, 1, 1, 2, 2, 2, -1, -1, 1, 1, 1, 2, 2, 2, -1, -1, 1, 1, 1, 2, 2, 2 /)
   
      do index = 1, 22, 1
         if (channel == channels(index) .AND. slot == slots(index)) then
             address = base + index - 1
             exit
         end if 
      end do
   
   end function
   
   !  It's given in samples, 1 second has 44100 samples in VGMs. (44.1 in a millisecond)
   !  Wait n samples, n can range from 0 to 65535 (approx 1.49 seconds).
   
   function deltaToSamples(ticks, tempo)           result(res)
       integer(kind = 8), intent(in)            :: ticks, tempo
       real(kind = 8)                           :: res       
       real(kind = 8)                           :: microSeconds, milliSeconds
       real(kind = 4), parameter                :: sampleRate = 44.1 
       
       microSeconds = midiP%deltaTimeToMS(ticks, tempo)
       milliSeconds = microseconds / 1000.0 
    !
    !  The fraction part must be handled seperately at the caller position!
    !
       res = milliSeconds * sampleRate
       
   end function
 
   subroutine debugLog(txt)
        character(len = *), intent(in)         :: txt
            
        if (debug .eqv. .TRUE.) then
            if (first .EQV. .TRUE.) then
                first = .FALSE.
                open(91, file = "vgmLog.txt", action = "write")
                
            else
                open(91, file = "vgmLog.txt", action = "write", position = "append")
            end if 
            
            write(91, "(A)") txt
            close(91)
         end if
            
   end subroutine
   
    function numToText(number) result(txt)
        integer(kind = 8), intent(in)           :: number
        character(len = 20)                     :: txt
           
        txt                                     = ""
        write(txt, "(I20)") number
        
        do while(txt(1:1) == " ")
           txt(1:19)  = txt(2:20)
           txt(20:20) = " "
        end do    
    
    end function

    function realToText(number) result(txt)
        real(kind = 8), intent(in)              :: number
        character(len = 20)                     :: txt
           
        txt                                     = ""
        write(txt, "(f0.2)") number
        
        do while(txt(1:1) == " ")
           txt(1:19)  = txt(2:20)
           txt(20:20) = " "
        end do    
    
    end function   

    function numToHex(input) result(txt)
        integer(kind = 2), intent(in)          :: input
        integer(kind = 2)                      :: number
        character(len = 2)                     :: txt
        integer(kind = 1)                      :: index   
        
        !
        !  In case of KIND 1 input numbers
        !
        
        number = input
        
        if (number < 0) then
            number = number * -1
            number = 256 - number
        end if    
        
        txt                                     = ""
        write(txt, "(Z2)") number
        
        do index = 1, 2, 1
           if (txt(index:index) == " ") txt(index:index) = "0"  
        end do    
    
    end function
  
    function numToBin(input, bits) result(txt)
        integer(kind = 1), intent(in)           :: bits
        integer(kind = 2), intent(in)           :: input
        integer(kind = 2)                       :: number
        
        character(len = bits)                   :: txt
        integer(kind = 1)                       :: index      
        
        !
        !  In case of KIND 1 input numbers
        !
        
        number = input
        
        if (number < 0) then
            number = number * -1
            number = 256 - number
        end if    
        
        txt                                     = ""
        write(txt, "(B"// trim(numToText(bits)) // ")") number
        
        do index = 1, bits, 1
           if (txt(index:index) == " ") txt(index:index) = "0"  
        end do     
    
    end function
    
    
    
 end module
    
    