module VGM
    use player
    use soundbank
    
    implicit none
    
    private
    public                                         :: vgmFile, buildVGM
    
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

    end type    
    
    type byteTriple
        integer(kind = 8)                           :: command, register, value
    end type    
       
    type VGMData
        type(byteTriple), dimension(:), allocatable :: byteTriples
        integer(kind = 8)                           :: lastOne = 0  
    end type
    
    type deltaAndBytes
        
        type(byteTriple), dimension(14)             :: byteTriples
        integer(kind = 8)                           :: startDelta = 0, endDelta = 0
        integer(kind = 8)                           :: framesBefore = 0
        integer(kind = 2)                           :: lastOne = 0  
        
        contains
        
        procedure                                   :: addNote  => addNote
        
    end type    
    
    type dataChannel
        type(deltaAndBytes), dimension(:), allocatable :: timedData
        integer(kind = 8)                              :: lastOne  = 0  
        integer(kind = 2)                              :: currInst = 0 
        
        contains
        
        procedure                                      :: addData => addData 
        
    end type 
    
    type vgmFile
        type(VGMHeader)                                :: header
        type(VGMData)                                  :: chipData
        type(dataChannel), dimension(9)                :: dataChannels 
        
        contains
        procedure                                      :: buildVGM         => buildVGM
        procedure                                      :: deltaToSamples   => deltaToSamples
        
   end type
    
   contains     
    
   subroutine buildVGM(this, midiPP, sBankP) 
       use player
       use soundBank

       class(vgmFile),   intent(inout)                :: this
       type(soundB),     intent(in), target           :: sBankP
       type(midiPlayer), intent(in), target           :: midiPP
       
       integer(kind = 2)                              :: stat, channelIndex
       integer(kind = 8)                              :: noteIndex
       
       midiP => midiPP
       sBank => sBankP
       
       if (allocated(this%chipData%byteTriples) .EQV. .TRUE.) deallocate(this%chipData%byteTriples, stat = stat)
       this%chipData%lastOne      = 0
       this%header%eofOffset      = 0
       this%header%gd3Offset      = 0
       this%header%allWaitSamples = 0
       this%header%dataOffset     = z'4c'
       
       do channelIndex = 1, 9, 1
          if (allocated(this%dataChannels(channelIndex)%timedData) .EQV. .TRUE.) deallocate(this%dataChannels(channelIndex)%timedData, stat = stat) 
          this%dataChannels(channelIndex)%lastOne  = 0        
          this%dataChannels(channelIndex)%currInst = 0        
       
          if (midiP%notePointerChannels(channelIndex)%lastOne > 0) then
              allocate(this%dataChannels(channelIndex)%timedData(midiP%notePointerChannels(channelIndex)%lastOne), stat = stat)
              
              do noteIndex = 1, midiP%notePointerChannels(channelIndex)%lastOne, 1
                 call this%dataChannels(channelIndex)%addData(midiP%notePointerChannels(channelIndex)%notePointers(noteIndex), channelIndex) 
              end do    
          end if          
       end do    
       
       call midiP%deAllocator()
       
   end subroutine
   
   subroutine addData(this, noteP, channelIndex)
      use player 
   
      class(dataChannel)     , intent(inout)        :: this
      type(playerNotePointer), intent(in)           :: noteP
      logical                                       :: newIstrument = .FALSE.
      integer(kind = 2)                             :: channelIndex
      
      this%lastOne           = this%lastOne + 1
      
      if (this%lastOne == 1) then
          newIstrument = .TRUE.
      else
          if (this%currInst /= noteP%p%instrument) newIstrument = .TRUE.
      end if
      
      call this%timedData(this%lastOne)%addNote(noteP, noteP%p%instrument, this%currInst, newIstrument, channelIndex)
      this%currInst   = noteP%p%instrument
      
   end subroutine
   
   subroutine addNote(this, noteP, instru, old, newOne, channelIndex)
      class(deltaAndBytes)   , intent(inout)        :: this
      type(playerNotePointer), intent(in)           :: noteP
      logical                                       :: newOne
      integer(kind = 2)                             :: instru, old
      type(instrument), pointer                     :: newI, oldI
      integer(kind = 2)                             :: dataIndex, slotIndex
      integer(kind = 2)                             :: channelIndex
      character(len = 8)                            :: tempByte
      integer(kind = 1)                             :: bitIndex
      character(len = 10)                           :: bits10

      
      do dataIndex = 1, size(this%byteTriples), 1
         this%byteTriples(dataIndex)%command  = z'5a'  
         this%byteTriples(dataIndex)%register = 0  
         this%byteTriples(dataIndex)%value    = 0  
      end do    
      
      this%lastOne      = 0
      this%startDelta   = 0
      this%endDelta     = 0
      this%framesBefore = 0
      
      if (newOne .EQV. .TRUE.) then
          newI => sBank%instruments(instru)         
          oldI => sBank%instruments(old)         

          if (oldI%feedback /= newI%feedback .OR. (oldI%doubleVoice .NEQV. newI%doubleVoice)) then
              this%lastOne = this%lastOne + 1
              tempByte     = ""
              
              this%byteTriples(this%lastOne)%register = z'C0' + (channelIndex - 1)

              write(tempByte(5:7), "(B3)") newI%feedback 
              
              do bitIndex = 1, 8, 1
                 if (tempByte(bitIndex:bitIndex) == " ") tempByte(bitIndex:bitIndex) = "0" 
              end do   
              
              if (newI%doubleVoice .EQV. .TRUE.) tempByte(8:8) = "1"
              
              read(tempByte, "(B8)") this%byteTriples(this%lastOne)%value
              
          end if    
          
          do slotIndex = 1, 2, 1
             
             if (newI%byte1(slotIndex) /= oldI%byte1(slotIndex)) then 
                 this%lastOne = this%lastOne + 1
   
                 this%byteTriples(this%lastOne)%register = getAddress(channelIndex, slotIndex, z'20')
                 this%byteTriples(this%lastOne)%value    = newI%byte1(slotIndex)
             end if
             
             if (newI%byte2(slotIndex) /= oldI%byte2(slotIndex)) then 
                 this%lastOne = this%lastOne + 1
   
                 this%byteTriples(this%lastOne)%register = getAddress(channelIndex, slotIndex, z'60')
                 this%byteTriples(this%lastOne)%value    = newI%byte2(slotIndex)
             end if
             
             if (newI%byte3(slotIndex) /= oldI%byte3(slotIndex)) then 
                 this%lastOne = this%lastOne + 1
   
                 this%byteTriples(this%lastOne)%register = getAddress(channelIndex, slotIndex, z'80')
                 this%byteTriples(this%lastOne)%value    = newI%byte3(slotIndex)
             end if             
             
             if (newI%byte4(slotIndex) /= oldI%byte4(slotIndex)) then 
                 this%lastOne = this%lastOne + 1
   
                 this%byteTriples(this%lastOne)%register = getAddress(channelIndex, slotIndex, z'e0')
                 this%byteTriples(this%lastOne)%value    = newI%byte4(slotIndex)
             end if                      

             if (newI%byte5(slotIndex) /= oldI%byte5(slotIndex) .OR. newI%byte6(slotIndex) /= oldI%byte6(slotIndex)) then 
                 this%lastOne = this%lastOne + 1
   
                 this%byteTriples(this%lastOne)%register = getAddress(channelIndex, slotIndex, z'40')
                 
                 tempByte = ""
                 write(tempByte(1:2), "(B2)") newI%byte5(slotIndex)
                 write(tempByte(3:8), "(B6)") newI%byte4(slotIndex)
                 
                 do bitIndex = 1, 8, 1
                    if (tempByte(bitIndex:bitIndex) == " ") tempByte(bitIndex:bitIndex) = "0" 
                 end do   
                 
                 read(tempByte, "(B8)") this%byteTriples(this%lastOne)%value

             end if   
             
          end do    
          this%lastOne = this%lastOne + 1
          bits10       = ""
          
          write(bits10, "(B10)") noteP%p%fNumber 
          do bitIndex = 1, 10, 1
              if (bits10(bitIndex:bitIndex) == " ") bits10 = "0"
          end do  
          
          ! low byte    
          this%byteTriples(this%lastOne)%register = z'A0' + (channelIndex - 1)
          read(bits10(3:10), "(B8)") this%byteTriples(this%lastOne)%value

          this%lastOne = this%lastOne + 1
          ! high byte
          
          this%byteTriples(this%lastOne)%register = z'B0' + (channelIndex - 1)
          tempByte      = "00000000"
          tempByte(7:8) = bits10(1:2)
          
          write(tempByte(4:6), "(B3)") noteP%p%octave
          do bitIndex = 1, 8, 1
              if (tempByte(bitIndex:bitIndex) == " ") tempByte = "0"
          end do  
          
          tempByte(3:3) = "1"
          read(tempByte, "(B8)") this%byteTriples(this%lastOne)%value
          
          ! Note Off Byte
          this%lastOne = this%lastOne + 1
          
          tempByte(3:3) = "0"
          read(tempByte, "(B8)") this%byteTriples(this%lastOne)%value
          
      end if
      
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
             address = base + index
             exit
         end if 
      end do
   
   end function
   
   !  It's given in samples, 1 second has 44100 samples in VGMs. (44.1 in a millisecond)
   !  Wait n samples, n can range from 0 to 65535 (approx 1.49 seconds).
   
   function deltaToSamples(this, ticks, tempo) result(res)
       class(vgmFile), intent(in)               :: this
       integer(kind = 8), intent(in)            :: ticks, tempo
       integer(kind = 8)                        :: microSeconds
       real(kind = 8)                           :: res       
       real(kind = 8)                           :: milliSeconds
       real(kind = 4), parameter                :: sampleRate = 44.1 
       
       microSeconds = midiP%deltaTimeToMS(ticks, tempo)
       milliSeconds = microseconds / 1000.0 
    !
    !  The fraction part must be handled seperately at the caller position!
    !
       res = milliSeconds / sampleRate
       
   end function
   
end module