module player   
    use midi
    use soundbank
    
    implicit none
    
    private
    public                                      :: midiPlayer, initPlayer
    
    ! index = octave block
    real(kind = 8), dimension(8), parameter     :: freqBases = (/ 0.047, 0.094, 0.189, 0.379, 0.758, 1.517, 3.034, 6.068 /)
    real(kind = 8), dimension(8), parameter     :: freqSteps = (/ 0.048, 0.095, 0.190, 0.379, 0.759, 1.517, 3.034, 6.069 /)
    
    ! multi nibble on $20-$35
    real(kind = 4), dimension(16), parameter    :: multiNums = (/ 0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, &
                                                                 &7.0, 8.0, 9.0, 10.0, 10.0, 12.0, 12.0, 15.0, 15.0/)
    
    
    logical, parameter                          :: debug                = .FALSE. , printTable = .TRUE.
    logical                                     :: dbgLogFirst          = .FALSE.
    logical                                     :: ignorePercussion     = .FALSE.
    type(midiFile), pointer                     :: midiF
    type(soundB)  , pointer                     :: sBank
    integer(kind = 1), parameter                :: maxNumberOfMembers   = 9, &
                                                 & maxPercussItems      = 3
    
    type playerNotePointer
        type(playerNote), pointer               :: p
        !logical                                :: used = .FALSE. 
        integer(kind = 8)                       :: startDelta, endDelta        

    end type    
    
    type playerNote
        integer(kind = 2)                       :: instrument, volume, note, fNumber, octave
        real(kind = 8)                          :: freq
        integer(kind = 8)                       :: startDelta, endDelta        
        type(instrument), pointer               :: instrumentP
        logical                                 :: closed = .TRUE., placed = .FALSE.

    end type    
    
    type playerChannel
        type(playerNote), dimension(:), allocatable :: playerNotes
        logical                                     :: hasAnyNotes = .FALSE.
        
        integer(kind = 8)                           :: numOfNotes = 0, sumOfDuration = 0
        
        type(instrument), pointer                   :: instrumentP
        integer(kind = 8)                           :: lastNote = 0, currInst = 0
        
    end type    
    
    type instrumentOccurence
        integer(kind = 8)                            :: occurs    = 0
        integer(kind = 2)                            :: instruNum = 0
        
    end type    
    
    type instrumentOccurenceTable
        type(instrumentOccurence), dimension(:), allocatable    :: ioTable
        type(instrumentOccurence), dimension(:), allocatable    :: tempTable
        integer(kind = 2)                                       :: lenght, lastOne
        
    end type
    
    type ranking
        integer(kind = 1)                            :: channelNum  = 0  
        real(kind = 8)                               :: value       = 0.0   
    end type    
    
    type noteFreqPair
        integer(kind = 2)                           :: note
        real(kind = 8)                              :: freq
        
    end type    
      
    type freqCombo2   
        integer(kind = 2)                           :: instrument
        type(instrument), pointer                   :: instrumentP
        integer(kind = 2)                           :: fnum, octave      
        real(kind = 8)                              :: value, diff, freq
        real(kind = 4)                              :: multi
        
    end type
    
    type freqCombo
    !  
    !   There are 128 different non-percussion instruments 
    !    
        real(kind = 8)                              :: freq
        type(freqCombo2), dimension(:), allocatable :: instruTable
        
    end type
    
    type pointerChannel
        type(playerNotePointer), dimension(:), allocatable :: notePointers
        integer(kind = 8)                                  :: lastOne = 0
        
    end type
    
    type midiPlayer
        logical                                     :: success = .FALSE., divisionMode = .FALSE., dbgLogFirst
        integer                                     :: TPQN, fps, ptf, tempo = 120 
        integer(kind = 8)                           :: maxTime, maxNumberOfNotes
        type(playerChannel), &
        &     dimension(16, maxNumberOfMembers)     :: channels
        type(pointerChannel), dimension(9)          :: notePointerChannels
        
        integer(kind = 1), dimension(16)            :: channelMemberNums = 1
        type(noteFreqPair), dimension(128)          :: noteFreqTable
        logical                                     :: loadedTable       = .FALSE.
       
        !
        ! 128 midi notes = 128 freq values
        !
        type(freqCombo),    dimension(128)          :: comboTable
        logical                                     :: loadedComboTable  = .FALSE.

        
        contains                        
        procedure                                   :: InitPlayer                 => initPlayer
        procedure                                   :: DeltaTimeToMS              => deltaTimeToMS
        procedure                                   :: InitTempo                  => initTempo    
        procedure                                   :: FillChannel                => fillChannel    
        procedure                                   :: LoadTable                  => loadTable    
        procedure                                   :: getFNumAndOctave           => getFNumAndOctave
        procedure                                   :: LoadComboTable             => loadComboTable    
        procedure                                   :: reAlignNotes               => reAlignNotes    
        procedure                                   :: checkChannelInstruMonotony => checkChannelInstruMonotony
        procedure                                   :: fixChannel10               => fixChannel10
        procedure                                   :: detectBestPlaceForNote     => detectBestPlaceForNote
        procedure                                   :: insertTheNote              => insertTheNote
        procedure                                   :: printTheTable              => printTheTable
        
    end type
        
    contains    
    
    subroutine fixChannel10(this, note, channel)    
         class(midiPlayer)  , intent(inout)        :: this       
         type(playerNote)   , intent(inout)        :: note
         type(playerChannel), intent(inout)        :: channel
         
         channel%currInst = note%note + 129 - 35
         note%instrument  = channel%currInst
         
         if (debug .EQV. .TRUE.) call debugLog("Fix Channel 10's " // &
                                &trim(numToText(note%note)) // ", instrument is set to " // trim(numToText(channel%currInst)) // &
                                & "(" // trim(sBank%instruments(note%instrument)%name) // ")") 
         note%note        = fixNoteNum(channel%currInst)
         if (debug .EQV. .TRUE.) call debugLog("The new note is " // trim(numToText(note%note))) 
         
    end subroutine
    
    function fixNoteNum(instrument) result(res)
         integer(kind = 2), intent(in)      :: instrument
         integer(kind = 2)                  :: res
   
         ! The percussion instruments start after the 127th byte, 
         ! but the very first valid is the 35th on general midi 
         res = sBank%instruments(instrument)%fixedNote
         
    end function     
         
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
    
    subroutine debugLog(txt)
        character(len = *)                      :: txt
    
        if (dbgLogFirst .EQV. .TRUE.) then
            dbgLogFirst = .FALSE.
            open(89, file = "playerDBG.txt", action = "write")
        else
            open(89, file = "playerDBG.txt", action = "write", position = "append")
        end if 
            
        write(89, "(A)") txt
        close(89)
    
    end subroutine
    
    subroutine initPlayer(this, midiFP, sBankP, ignore)
        use midi
        use soundbank
    
        class(midiPlayer), intent(inout)        :: this
        type(midiFile), intent(in), target      :: midiFP
        type(soundB),   intent(in), target      :: sBankP
        integer(kind = 8)                       :: index, longestDelta, subIndex, memberIndex
        integer(kind = 1)                       :: stat
        logical                                 :: ignore
        
        ignorePercussion                        = ignore
        dbgLogFirst                             = .TRUE.
        
        midiF                                   => midiFP       
        sBank                                   => sBankP
        
        this%success                            = .FALSE.
        this%TPQN                               = midiF%TPQN     
        this%fps                                = midiF%fps
        this%ptf                                = midiF%ptf 
        this%divisionMode                       = midiF%divisionMode
        
        this%channelMemberNums = 1
        
        if (this%loadedTable      .EQV. .FALSE.) call this%loadTable()
        if (this%loadedComboTable .EQV. .FALSE.) call this%loadComboTable()
        
        this%tempo                              = this%initTempo("M")
        if (debug .EQV. .TRUE.) then
            !call debugLog("Full Delta:  " // trim(numToText(this%midiF%deltaFull)), this%dbgLogFirst)  
            
            if (midiF%divisionMode .EQV. .FALSE.) then
                call debugLog("Max Tempo:   " // trim(numToText(this%tempo)))    
                call debugLog("TPQN:        " // trim(numToText(midiF%TPQN)))    
            else
                call debugLog("FPS:         " // trim(numToText(midiF%fps)))    
                call debugLog("PTF:         " // trim(numToText(midiF%ptf)))   
            end if
        end if   
        
        longestDelta     = 0
        this%maxNumberOfNotes = 0
        
        do index = 1, midiF%numberOfTracks , 1
           if (midiF%deltaSums(index) > longestDelta) longestDelta = midiF%deltaSums(index)
           if (midiF%tracks(index)%lastMessage > this%maxNumberOfNotes) this%maxNumberOfNotes = midiF%tracks(index)%lastMessage
        end do     
        
        if (debug .EQV. .TRUE.) then
            call debugLog("Delta Max:  " // trim(numToText(longestDelta)))  
        end if     
        
        this%maxTime                            = this%deltaTimeToMS(longestDelta, this%tempo)
        this%tempo                              = this%initTempo("F") 
        
        this%maxTime = longestDelta
               
        if (debug .EQV. .TRUE.) then
            call debugLog("Max Time:   " // trim(numToText(this%maxTime)))  
            if (midiF%divisionMode .EQV. .FALSE.) then
                call debugLog("First Tempo:" // trim(numToText(this%tempo)))    
            end if
        end if   
        

        do index = 1, 9, 1
           if (allocated(this%notePointerChannels(index)%notePointers) .EQV. .TRUE.)&
            & deallocate(this%notePointerChannels(index)%notePointers, stat = stat)
           allocate(this%notePointerChannels(index)%notePointers(this%maxNumberOfNotes) , stat = stat) 
        
           this%notePointerChannels(index)%lastOne = 0
            
        end do

        if (debug .EQV. .TRUE.) then
            call debugLog("Allocate note pointer array as 16:" // trim(numToText(this%maxNumberOfNotes)) // ": " // trim(numToText(stat)))  
        end if
        
        !if (allocated(this%percussionPointers) .EQV. .TRUE.) deallocate(this%percussionPointers, stat = stat)
        !allocate(this%percussionPointers(5, this%maxNumberOfNotes)  , stat = stat) 
        if (debug .EQV. .TRUE.) then
            call debugLog("Allocate percussion pointer array as " // trim(numToText(maxNumberOfMembers)) // ":" &
                 &// trim(numToText(this%maxNumberOfNotes)) // ": " // trim(numToText(stat)))  
        end if
        
        do index = 1, 16, 1
           do memberIndex = 1, maxNumberOfMembers, 1 
              if (allocated(this%channels(index, memberIndex)%playerNotes) .EQV. .TRUE.) &
                 &deallocate(this%channels(index, memberIndex)%playerNotes, stat = stat)
              allocate(this%channels(index, memberIndex)%playerNotes(this%maxNumberOfNotes), stat = stat)  
              
              if (debug .EQV. .TRUE.) then
                 call debugLog("Allocate notes array " // trim(numToText(index)) // "," // trim(numToText(memberIndex)) // " as " // & 
                               &trim(numToText(this%maxNumberOfNotes)) // ": " // trim(numToText(stat)))    
              end if
              
              this%channels(index, memberIndex)%lastNote       = 0
              this%channels(index, memberIndex)%currInst       = 0
              this%channels(index, memberIndex)%hasAnyNotes    = .FALSE.
              this%channels(index, memberIndex)%numOfNotes     = 0
              this%channels(index, memberIndex)%sumOfDuration  = 0

              
              
              do subIndex = 1, this%maxNumberOfNotes, 1
                  this%channels(index, memberIndex)%playerNotes(subIndex)%instrument = 0
                  this%channels(index, memberIndex)%playerNotes(subIndex)%volume     = 0
                  this%channels(index, memberIndex)%playerNotes(subIndex)%note       = 0
                  this%channels(index, memberIndex)%playerNotes(subIndex)%startDelta = 0
                  this%channels(index, memberIndex)%playerNotes(subIndex)%endDelta   = 0
                  this%channels(index, memberIndex)%playerNotes(subIndex)%closed     = .TRUE.
                  this%channels(index, memberIndex)%playerNotes(subIndex)%fNumber    = 0
                  this%channels(index, memberIndex)%playerNotes(subIndex)%octave     = 0
                  this%channels(index, memberIndex)%playerNotes(subIndex)%freq       = 0
                  this%channels(index, memberIndex)%playerNotes(subIndex)%placed     = .FALSE.

              end do
           end do
        end do    

        do index = 1, midiF%numberOfTracks, 1 
           call this%fillChannel(index)
        end do
        
        call this%reAlignNotes()
        
    end subroutine
    
    subroutine reAlignNotes(this)
    
      class(midiPlayer), intent(inout), target :: this
      integer(kind = 8)                        :: channelIndex, memberIndex, saveIndex, lastIndex,     &
                                                & largestRankChannel, noteIndex, subIndex, startIndex, &
                                                & iIndex, insertIndex, insertHere
      real(kind = 8)                           :: largestRank, lastLargest
      type(ranking), dimension(16)             :: rankings, orderedRankings  
      type(instrumentOccurenceTable)           :: ioT
      integer(kind = 1), dimension(16)         :: rankedOnes  
      integer(kind = 2)                        :: stat
      real(kind = 8)                           :: monotony
      integer(kind = 2), dimension(2)          :: chanMemb
      logical                                  :: foundIt, finished 
      integer(kind = 8), dimension(9,128)      :: instumentsAgain
      integer(kind = 2)                        :: mostCommonOne
      integer(kind = 2), dimension(9)          :: dominants 
      integer(kind = 8)                        :: largestCount, iNum, saveHere
      integer(kind = 8), dimension(9)          :: startIndexes 
      
      saveIndex     = 0
      lastIndex     = 0
      
      rankedOnes    = -1
      rankedOnes(1) = 10
      
      if (allocated(iot%ioTable)   .EQV. .TRUE.) deallocate(iot%ioTable,   stat = stat)
      if (allocated(iot%tempTable) .EQV. .TRUE.) deallocate(iot%tempTable, stat = stat)     
      
      do channelIndex = 1, 16, 1
         if (channelIndex == 10) cycle    
             
         if (this%channels(channelIndex,1)%hasAnyNotes .EQV. .TRUE.) then
             saveIndex = saveIndex +1 
             lastIndex = saveIndex 
                    
             call this%checkChannelInstruMonotony(channelIndex, ioT, monotony)
             
             rankings(saveIndex)%channelNum  =  channelIndex
             rankings(saveIndex)%value       =  this%channels(channelIndex,1)%numOfNotes * (this%channels(channelIndex,1)%sumOfDuration / 10) * monotony

             if (debug .EQV. .TRUE.) then
                call debugLog("ChannelIndex: " // trim(numToText(channelIndex))                                // &
                         &" Number of Notes: " // trim(numToText(this%channels(channelIndex,1)%numOfNotes))    // & 
                         &" Sum of Duration: " // trim(numToText(this%channels(channelIndex,1)%sumOfDuration)) // &
                         &" Value: "           // trim(realToText(rankings(saveIndex)%value)))
                 
                !call debugLog(trim(numToText(saveIndex)) // " " // trim(numToText(rankings(saveIndex)%channelNum)))
                
             end if         
         end if  
      end do     
      
      if (allocated(iot%ioTable)   .EQV. .TRUE.) deallocate(iot%ioTable,   stat = stat)
      if (allocated(iot%tempTable) .EQV. .TRUE.) deallocate(iot%tempTable, stat = stat)  
      
      lastLargest           = 4294967295
      do saveIndex = 1, lastIndex, 1
         largestRank        = 0
         largestRankChannel = 0
          
         do channelIndex = 1, lastIndex , 1 
             
            if (rankings(channelIndex)%value > largestRank  .AND. &
            &   rankings(channelIndex)%value > 0            .AND. &    
            &   rankings(channelIndex)%value < lastLargest) then
                largestRank        = rankings(channelIndex)%value
                largestRankChannel = rankings(channelIndex)%channelNum
                !call debugLog(trim(realToText(rankings(channelIndex)%value)))                        
            end if
         end do
         
         if (largestRank == 0) then
             lastIndex = saveIndex - 1
             exit
         end if
             
         orderedRankings(saveIndex)%channelNum = largestRankChannel
         orderedRankings(saveIndex)%value      = largestRank
         lastLargest                           = largestRank
         if (debug .EQV. .TRUE.) then
              
             call debugLog("#" // trim(numToText(saveIndex)) // " channel: " // &
                                & trim(numToText(largestRankChannel))        // " with value " // &
                                & trim(realToText(largestRank)) // "!")
             
         end if   
      end do      

      do saveIndex = 1, lastIndex, 1
         if (ignorePercussion .EQV. .FALSE.) then
             rankedOnes(saveIndex + 1) = orderedRankings(saveIndex)%channelNum 
         else
             rankedOnes(saveIndex)     = orderedRankings(saveIndex)%channelNum 
         end if 
      end do
             
      saveIndex  = 0
      startIndex = 1
         
      if (ignorePercussion .EQV. .FALSE.) then 
          lastIndex  = lastIndex + 1
          startIndex = 2
          if (debug .EQV. .TRUE.) then
              call debugLog("--- Start Alignment of Percussion Notes ---")    
          end if  
          
          do memberIndex = 1, maxNumberOfMembers, 1 
             if (this%channels(10, &
                 &memberIndex)%hasAnyNotes .EQV. .FALSE.      ) exit
             if (memberIndex      > this%channelMemberNums(10)) exit
             if (memberIndex      > maxPercussItems           ) exit
             saveIndex = memberIndex + 1
             
             do noteIndex = 1, this%channels(10, memberIndex)%lastNote, 1  
                if (debug .EQV. .TRUE.) then
                    call debugLog("Saving Percussion Channel (#10) Member #" // trim(numToText(memberIndex)) // &
                        & "'s Note #" // trim(numToText(noteIndex)) // " on the Note #" // &
                        & trim(numToText(this%notePointerChannels(memberIndex)%lastOne + 1)) // " of Pointer Channel #" // &
                        & trim(numToText(memberIndex)) // "!")    
                end if    
                
                this%notePointerChannels(memberIndex)%lastOne = this%notePointerChannels(memberIndex)%lastOne + 1
                
                this%notePointerChannels(memberIndex)%notePointers(this%notePointerChannels(memberIndex)%lastOne)%p => &
                    &this%channels(10, memberIndex)%playerNotes(noteIndex)
                
                this%notePointerChannels(memberIndex)%notePointers(this%notePointerChannels(memberIndex)%lastOne)%startDelta = &
              & this%channels(10, memberIndex)%playerNotes(noteIndex)%startDelta
                
                this%notePointerChannels(memberIndex)%notePointers(this%notePointerChannels(memberIndex)%lastOne)%endDelta   = &
              & this%channels(10, memberIndex)%playerNotes(noteIndex)%endDelta               
                    
             end do   
         end do      
      end if
      
      chanMemb     = (/ 0 , 0 /)
      channelIndex = rankedOnes(startIndex)  
      saveIndex    = saveIndex - 1
      
      call debugLog(trim(numToText(saveIndex)))  
      do memberIndex   = 1, maxNumberOfMembers, 1
         if (saveIndex > 9 ) then
             exit 
         end if
         do subIndex  = startIndex, lastIndex, 1
             channelIndex = rankedOnes(subIndex)
             
             if (this%channels(channelIndex, &
                 &memberIndex)%hasAnyNotes .EQV. .FALSE.           ) cycle
             if (memberIndex > this%channelMemberNums(channelIndex)) cycle
             
             saveIndex = saveIndex + 1              
             if (saveIndex > 9 ) then
                 exit 
             end if
             
             do noteIndex = 1, this%channels(channelIndex, memberIndex)%lastNote, 1  
                if (debug .EQV. .TRUE.) then
                    call debugLog("Saving Channel #" // trim(numToText(channelIndex)) // " Member #" // trim(numToText(memberIndex)) // &
                        & "'s Note #" // trim(numToText(noteIndex)) // " on the Note #" // &
                        & trim(numToText(this%notePointerChannels(saveIndex)%lastOne + 1)) // " of Pointer Channel #" // &
                        & trim(numToText(saveIndex)) // "!")    
                end if    
                this%notePointerChannels(saveIndex)%lastOne = this%notePointerChannels(saveIndex)%lastOne + 1
                
                this%notePointerChannels(saveIndex)%notePointers(this%notePointerChannels(saveIndex)%lastOne)%p => &
                    &this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)
                
                this%notePointerChannels(saveIndex)%notePointers(this%notePointerChannels(saveIndex)%lastOne)%startDelta = &
              & this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)%startDelta
                
                this%notePointerChannels(saveIndex)%notePointers(this%notePointerChannels(saveIndex)%lastOne)%endDelta   = &
              & this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)%endDelta               
                    
             end do   
             
             chanMemb(1) = subIndex
             chanMemb(2) = memberIndex
             
         end do    
         if (saveIndex > 9 ) then
             exit 
         end if
      
      end do

      if (debug .EQV. .TRUE.) then
          call debugLog(trim(numToText(saveIndex)) // " " // trim(numToText(chanMemb(1))) // " " // trim(numToText(chanMemb(2)))) 
      end if
      
      if ( chanMemb(2) <= maxNumberOfMembers .AND. &
         & chanMemb(1) <= lastIndex) then
           if (chanMemb(1) == lastIndex) then
               chanMemb(1)  = 0
               chanMemb(2)  = chanMemb(2) + 1
           else
               chanMemb(1)  = chanMemb(1) + 1
           end if 
           
       end if    
       
       if (saveIndex > 9) then
       
           call debugLog("Place what remained!")
           do iIndex = 1, 9, 1 
              instumentsAgain                       = 0
              dominants      (iIndex)               = 0
              mostCommonOne                         = 0
              largestCount                          = 0
           
              do noteIndex = 1, this%notePointerChannels(iIndex)%lastOne, 1
                 iNum = this%notePointerChannels(iIndex)%notePointers(noteIndex)%p%instrument
                 if (iNum > 0 .AND. iNum < 129) instumentsAgain(iIndex, iNum) = instumentsAgain(iIndex, iNum) + 1                  
              end do    
              
              do iNum = 1, 128, 1 
                 if (instumentsAgain(iIndex, iNum) > largestCount) then
                    largestCount  = instumentsAgain(iIndex, iNum) 
                    mostCommonOne = iNum
                 end if
              end do   
              
              if (mostCommonOne /= 0 ) then 
                  dominants(iIndex) = mostCommonOne
                  if (debug .EQV. .TRUE.) then
                     call debugLog("Dominant of channel #" // trim(numToText(iIndex)) // ": " // trim(numToText(mostCommonOne)) // "!") 
                  end if
              end if   
           end do   
           
           do memberIndex     = chanMemb(2), maxNumberOfMembers                               , 1
              do subIndex     = chanMemb(1), lastIndex                                        , 1  
                 channelIndex = rankedOnes(subIndex)
                 if (this%channels(channelIndex, &
                     &memberIndex)%hasAnyNotes .EQV. .FALSE.           ) cycle
                 if (memberIndex > this%channelMemberNums(channelIndex)) cycle
                 
                 startIndexes = 1
                 
                 do noteIndex = 1 , this%channels(channelIndex, memberIndex)%lastNote, 1  
                     foundIt      = .FALSE.
                     insertHere   = -1

                     call debugLog("Find a place for channel #" // trim(numToText(channelIndex)) // "'S member #" // trim(numToText(memberIndex)) // &
                                  &"'s note #" // trim(numToText(noteIndex)) // " on start delta " // &
                                  &trim(numToText(this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)%startDelta)) // "!")  
                       
                     
                     call this%detectBestPlaceForNote(this%channels(channelIndex, memberIndex)%playerNotes(noteIndex), &
                                                     &"BOTH" ,startIndexes, insertHere, saveHere, foundIt,            &
                                                     &this%channels(channelIndex, memberIndex)%lastNote)
                     
                     if (foundIt .EQV. .FALSE.) then
                        call this%detectBestPlaceForNote(this%channels(channelIndex, memberIndex)%playerNotes(noteIndex), &
                            &"ONE " ,startIndexes, insertHere, saveHere, foundIt,            &
                            &this%channels(channelIndex, memberIndex)%lastNote) 
                     end if    
                     
                     if (foundIt .EQV. .FALSE.) then
                        call this%detectBestPlaceForNote(this%channels(channelIndex, memberIndex)%playerNotes(noteIndex), &
                            &"NONE" ,startIndexes, insertHere, saveHere, foundIt,            &
                            &this%channels(channelIndex, memberIndex)%lastNote) 
                     end if    
                     
                     if (foundIt .EQV. .TRUE.) &
                        &call this%insertTheNote(this%channels(channelIndex, memberIndex)%playerNotes(noteIndex), insertHere, saveHere)
                                   
                    end do
                 end do              
              end do    
       end if   
       
       if (printTable .EQV. .TRUE.) call this%printTheTable()
       
    end subroutine
    
    subroutine printTheTable(this)
       class(midiPlayer), intent(inout)                 :: this

       
       
       
    end subroutine
           
    subroutine insertTheNote(this, note, insertHere, saveHere)
       class(midiPlayer), intent(inout)                 :: this
       type(playerNote), target                         :: note
       integer(kind = 8)                                :: insertHere, saveHere 
       type(pointerChannel)                             :: tempChannel 
       integer(kind = 2)                                :: stat
       integer(kind = 8)                                :: newSize, index, plusOne, oldSize
        
       oldSize = size(this%notePointerChannels(saveHere)%notePointers)
       plusOne = this%notePointerChannels(saveHere)%lastOne + 1
       
       if (allocated(tempChannel%notePointers) .EQV. .TRUE. ) deallocate(tempChannel%notePointers, stat = stat)
       allocate(tempChannel%notePointers(plusOne), stat = stat) 

       if (debug .EQV. .TRUE.) call debugLog("Insertion Data: Size of the original array: " // trim(numToText(oldSize))                    // &
                                           & ", the last note's position: " // trim(numToText(this%notePointerChannels(saveHere)%lastOne))    )

       if (debug .EQV. .TRUE.) call debugLog("Position of insertion: " // trim(numToText(insertHere)))       
       if (debug .EQV. .TRUE.) call debugLog("Source: " // trim(numToText(size(this%notePointerChannels(saveHere)%notePointers))) &
                                          & // ", Destination: " // trim(numToText(size(tempChannel%notePointers))))       
       
       if (insertHere > 1) then
           do index = 1, insertHere - 1, 1
              tempChannel%notePointers(index)%p          => this%notePointerChannels(saveHere)%notePointers(index)%p 
              tempChannel%notePointers(index)%startDelta =  this%notePointerChannels(saveHere)%notePointers(index)%startDelta
              tempChannel%notePointers(index)%endDelta   =  this%notePointerChannels(saveHere)%notePointers(index)%endDelta       
           end do    
       end if
           
       tempChannel%notePointers(insertHere)%p            => note
       tempChannel%notePointers(insertHere)%startDelta   =  note%startDelta
       tempChannel%notePointers(insertHere)%endDelta     =  note%endDelta
       
       if (insertHere < this%notePointerChannels(saveHere)%lastOne) then
           do index = insertHere + 1, this%notePointerChannels(saveHere)%lastOne, 1
              tempChannel%notePointers(index)%p          => this%notePointerChannels(saveHere)%notePointers(index-1)%p 
              tempChannel%notePointers(index)%startDelta =  this%notePointerChannels(saveHere)%notePointers(index-1)%startDelta
              tempChannel%notePointers(index)%endDelta   =  this%notePointerChannels(saveHere)%notePointers(index-1)%endDelta       
           end do  
       end if
       
       if (plusOne > oldSize) then
           deallocate(this%notePointerChannels(saveHere)%notePointers, stat = stat)
           newSize = size(this%notePointerChannels(saveHere)%notePointers) * 2
       
           allocate(this%notePointerChannels(saveHere)%notePointers(newSize), stat = stat)
       end if       
       
       do index = 1, plusOne, 1
          this%notePointerChannels(saveHere)%notePointers(index)%p          => tempChannel%notePointers(index)%p
          this%notePointerChannels(saveHere)%notePointers(index)%startDelta =  tempChannel%notePointers(index)%startDelta
          this%notePointerChannels(saveHere)%notePointers(index)%endDelta   =  tempChannel%notePointers(index)%endDelta     
       end do   
       
       this%notePointerChannels(saveHere)%lastOne = plusOne
       
       deallocate(tempChannel%notePointers, stat = stat)
       
    end subroutine
    
    subroutine detectBestPlaceForNote(this, note, mode, startIndexes, insertHere, saveHere, foundIt, lastNote)
       class(midiPlayer), intent(inout)               :: this
       type(playerNote)                               :: note
       character(len = 4)                             :: mode
       integer(kind = 8), dimension(9), intent(inout) :: startIndexes
    
       integer(kind = 8)                              :: insertIndex, saveIndex
       logical                                        :: finished               
       
       integer(kind = 8), intent(inout)               :: insertHere, saveHere
       logical, intent(inout)                         :: foundIt
       integer(kind = 8), intent(in)                  :: lastNote
       logical                                        :: f1, f2
       saveHere     = -1
       
       do saveIndex = 1, 9, 1
          finished = .FALSE. 
                        
          do insertIndex = startIndexes(saveIndex), this%notePointerChannels(saveIndex)%lastOne, 1
                          
             ! If the given channel is full, don't try to do anything. 
             if (startIndexes(saveIndex) == -1) exit
                          
             if (this%notePointerChannels(saveIndex)%notePointers(insertIndex)%endDelta < note%startDelta) then
                 startIndexes(saveIndex) = insertIndex + 1

                 if (startIndexes(saveIndex) > this%maxNumberOfNotes) then
                     startIndexes(saveIndex) = -1
                     finished                = .TRUE.
                     exit
                 end if  
                             
                 cycle
              end if
                       
              if (insertIndex < this%maxNumberOfNotes .AND. insertIndex < lastNote) then    
                  if (this%notePointerChannels(saveIndex)%notePointers(insertIndex+1)%startDelta > note%endDelta) then
                         startIndexes(saveIndex) = -1 
                         finished                = .TRUE.
                         exit
                     end if
              end if
                 
              f1 = .FALSE.
              f2 = .FALSE.
              
              if (mode /= "NONE") then
              
                  if (this%notePointerChannels(saveIndex)%notePointers(insertIndex)%p%instrument == note%instrument) then
                      f1 = .TRUE.
                  end if   
                         
                  if (insertIndex  < this%maxNumberOfNotes .AND. insertIndex  < lastNote) then  
                      if (this%notePointerChannels(saveIndex)%notePointers(insertIndex + 1)%p%instrument == note%instrument) then
                            f2 = .TRUE.
                      end if    
                  end if       
                  
                  if (insertIndex  == lastNote) then 
                      f2                      = .TRUE.
                      startIndexes(saveIndex) = -1 
                      finished                = .TRUE.
                  end if
                  
                  if (mode == "BOTH") then
                      if (f1 .EQV. .TRUE. .AND. f2 .EQV. .TRUE.) then
                          foundIT    = .TRUE.
                      end if
                  else
                      if (f1 .EQV. .TRUE. .OR.  f2 .EQV. .TRUE.) then
                          foundIT    = .TRUE.
                      end if
                  end if    
              else
                  foundIT    = .TRUE.
              end if
              
              if (foundIt .EQV. .TRUE.) then
                  insertHere = insertIndex
                  saveHere   = saveIndex
                  exit
              end if
          end do
          if (foundIt .EQV. .TRUE.) then
              call debugLog("Found place for note on pointerChannel #" // trim(numToText(saveHere)) // &
                           &" on position #" // trim(numToText(insertHere)) // "!")
              
              exit
          end if
        end do 
       
       
       
    end subroutine
    
    
    subroutine checkChannelInstruMonotony(this, channelIndex, ioT, res)
          class(midiPlayer), intent(inout)              :: this
          type(instrumentOccurenceTable), intent(inout) :: ioT
          integer(kind = 1)                             :: channelIndex  
          integer(kind = 8)                             :: noteIndex, instruIndex, foundIndex = 0, subIndex, biggestOne, allOfThem
          integer(kind = 2)                             :: stat, memberIndex
          integer(kind = 2), parameter                  :: defSize = 16
          real(kind = 8), intent(inout)                 :: res                  
          character                                     :: L
          
          res        = 0.50
          biggestOne = 0
          allOfThem  = 0
          
          allocate(ioT%ioTable(defSize), stat = stat)
          ioT%lenght  = defSize
          ioT%lastOne = 0
          
          do memberIndex = 1, maxNumberOfMembers, 1
             do noteIndex = 1, this%channels(channelIndex, memberIndex)%lastNote, 1
                foundIndex = 0
                if (ioT%lastOne > 0) then
                    do instruIndex = 1, ioT%lastOne, 1 
                       if (this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)%instrument == ioT%ioTable(instruIndex)%instruNum) then
                          foundIndex = instruIndex
                          exit
                       end if 
                    end do  
                end if
                
                if (foundIndex == 0) then
                                        
                    if (ioT%lastOne == ioT%lenght) then
                        allocate(iot%tempTable(ioT%lenght), stat = stat)

                        do subIndex = 1, ioT%lastOne, 1
                           iot%tempTable(subIndex)%occurs    = ioT%ioTable(instruIndex)%occurs
                           iot%tempTable(subIndex)%instruNum = ioT%ioTable(instruIndex)%instruNum                        
                        end do
                    
                        deallocate(ioT%ioTable, stat = stat)
                        ioT%lenght = ioT%lenght * 2
                        allocate(ioT%ioTable(ioT%lenght), stat = stat)
                    
                        if (ioT%lastOne > ioT%lastOne) then
                            do subIndex = 1, ioT%lenght, 1
                               ioT%ioTable(instruIndex)%occurs    = iot%tempTable(subIndex)%occurs
                               ioT%ioTable(instruIndex)%instruNum = iot%tempTable(subIndex)%instruNum                         
                            end do
                        end if    

                        deallocate(iot%tempTable, stat = stat)
                    end if    
                    
                    ioT%lastOne                        = ioT%lastOne + 1 
                    foundIndex                         = ioT%lastOne
                    ioT%ioTable(foundIndex)%instruNum  = this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)%instrument
                    ioT%ioTable(foundIndex)%occurs     = 0
                end if     
                !if (debug .EQV. .TRUE.) then
                !    write(L, "(L1)") this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)%closed
                !    call debugLog(trim(numToText(this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)%startDelta)) // " - " &
                !              &// trim(numToText(this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)%endDelta)) // &
                !              & " | " // L // " | " // trim(numToText(memberIndex)))
                !end if
                ioT%ioTable(foundIndex)%occurs         = ioT%ioTable(foundIndex)%occurs + (this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)%endDelta - this%channels(channelIndex, memberIndex)%playerNotes(noteIndex)%startDelta)
                
             end do
          end do    
          
          do subIndex = 1, ioT%lastOne, 1
             allOfThem = allOfThem + ioT%ioTable(subIndex)%occurs  
             if (biggestOne < ioT%ioTable(subIndex)%occurs) biggestOne = ioT%ioTable(subIndex)%occurs   
          end do    
          
          res = biggestOne / allOfThem
          if (debug .EQV. .TRUE.) then    
             call debugLog("Monotony Value of #" // trim(numToText(channelIndex)) // ": " // &
                          & trim(realToText(res)) // "( " // trim(numToText(biggestOne)) // "/" // trim(numToText(allOfThem)) //  " )!")
          end if 
          
          
          deallocate(ioT%ioTable, stat = stat)
          
    end subroutine
          
    subroutine loadTable(this)        
        class(midiPlayer), intent(inout)        :: this
        integer(kind = 2)                       :: stat, index
        character(len = 30)                     :: writeOut
        
        open(41, file = "note_freq.txt", action = "read")
        
        do index = 128, 1, -1
            read(41, *) this%noteFreqTable(index)%note, this%noteFreqTable(index)%freq
            
            if (debug .EQV. .TRUE.) then
                write(writeOut, "(I0, 1x, F0.2)") this%noteFreqTable(index)%note, this%noteFreqTable(index)%freq
                call debugLog("Loaded to NoteFreqTable: " // writeOut)
            end if    
        end do
        
        close(41)
        this%loadedTable = .TRUE.
        
    end subroutine
        
    subroutine loadComboTable(this)        
        class(midiPlayer), intent(inout)              :: this
        type(freqCombo2), dimension(:), allocatable   :: calculator
        integer(kind = 2)                             :: stat, noteIndex, multiIndex, instruIndex, fnum, oct, calcIndex, lowIndex
        real(kind = 4)                                :: multi
        real(kind = 8)                                :: lowestDiff 
        character(len = 25)                           :: val1, val2, val3  
        
        !
        ! NoteIndex : 1-128, InstruIndex = 1-128, MultiIndex = 1-13
        !
        ! 10 bits (1024) fnum * 3bit (8) oct  
        !
        
        if (allocated(calculator) .EQV. .TRUE.) deallocate(calculator, stat = stat)
        allocate(calculator(8192), stat = stat)
        
        !
        !  Notes are 0-127
        !
        do noteIndex = 1, 128, 1
           this%comboTable(noteIndex)%freq = this%noteFreqTable(noteIndex)%freq
           
           if (allocated(this%comboTable(noteIndex)%instruTable) .EQV. .TRUE.) deallocate(this%comboTable(noteIndex)%instruTable, stat = stat)
           allocate(this%comboTable(noteIndex)%instruTable(numOfInstruments), stat = stat)     
               
        !
        !  Non-Percussion Instruments are 0-127
        !
           do instruIndex = 1, numOfInstruments , 1                  
              
              if (instruIndex > 128) then
                  if (noteIndex /= sBank%instruments(instruIndex)%fixedNote) then
                     this%comboTable(noteIndex)%instruTable(instruIndex)%fnum   = 0
                     this%comboTable(noteIndex)%instruTable(instruIndex)%octave = 0
                     this%comboTable(noteIndex)%instruTable(instruIndex)%value  = 0
                     this%comboTable(noteIndex)%instruTable(instruIndex)%diff   = 0
                     cycle 
                  end if   
              end if
              
              this%comboTable(noteIndex)%instruTable(instruIndex)%instrument  =  instruIndex 
              this%comboTable(noteIndex)%instruTable(instruIndex)%instrumentP => sBank%instruments(instruIndex)
        !   
        !   There is a freq multi that is preset in the sound bank and can be different for
        !   the two slots of the channel. We are gonna use the second one only for calculation. 
        !             
              multiIndex = fetchDataFromInstrumentByte(&
                           &this%comboTable(noteIndex)%instruTable(instruIndex)%instrumentP, 2, 1, 0, 4)
              multi      = multiNums(multiIndex + 1)  

              this%comboTable(noteIndex)%instruTable(instruIndex)%multi = multi
              
              calcIndex = 1
              
              do fnum = 0, 1023, 1
                 do oct = 0, 7, 1
                    calculator(calcIndex)%fnum   = fnum
                    calculator(calcIndex)%octave = oct                    
                    
                    calculator(calcIndex)%freq         =  this%comboTable(noteIndex)%freq
                    calculator(calcIndex)%instrument   =  this%comboTable(noteIndex)%instruTable(instruIndex)%instrument
                    calculator(calcIndex)%multi        =  this%comboTable(noteIndex)%instruTable(instruIndex)%multi
                    
        !
        !   freq = freqBases(octave+1) + freqSteps(octave+1) * Fnum
        !
                    if (fnum == 0) then
                        calculator(calcIndex)%value = 0
                    else    
                        calculator(calcIndex)%value = (freqBases(oct + 1) +  (freqSteps(oct+1)  * fnum)) * multi
                    end if
                  
                    calculator(calcIndex)%diff = abs(calculator(calcIndex)%value - calculator(calcIndex)%freq) 
                    calcIndex = calcIndex + 1
                 end do 
              end do                              
              if (debug .EQV. .TRUE.) then
                  call debugLog("Calculated all variants on comboTable item for note " // trim(numToText(noteIndex)) // " and instrument " // trim(numToText(instruIndex)) // "!")
              end if   
              
              lowestDiff = -1
              lowIndex   = 0
              do calcIndex = 1, size(calculator), 1
                 if (calculator(calcIndex)%diff < lowestDiff .OR. lowestDiff == -1) then
                     lowestDiff = calculator(calcIndex)%diff
                     lowIndex   = calcIndex
                     
                     if (lowestDiff == 0) exit
                 end if 
              end do    
              if (debug .EQV. .TRUE.) then
                  
                  write(val1, "(F0.2)") calculator(lowIndex)%diff
                  write(val2, "(F0.2)") calculator(lowIndex)%freq
                  write(val3, "(F0.2)") calculator(lowIndex)%value
                  
                  call debugLog("The lowest difference is " // trim(val1) // " on " // trim(val2) // " - " // &
                                                              &trim(val3) // " as index " // trim(numToText(lowIndex)) // "!")
                  
                  call debugLog("Selected fnum is " // trim(numToText(calculator(lowIndex)%fnum)) // " and octave " //&
                      & trim(numToText(calculator(lowIndex)%octave)) // "!")
              end if
              
              this%comboTable(noteIndex)%instruTable(instruIndex)%fnum   = calculator(lowIndex)%fnum
              this%comboTable(noteIndex)%instruTable(instruIndex)%octave = calculator(lowIndex)%octave
              this%comboTable(noteIndex)%instruTable(instruIndex)%value  = calculator(lowIndex)%value
              this%comboTable(noteIndex)%instruTable(instruIndex)%diff   = calculator(lowIndex)%diff
              
           end do    
           
        end do    
        this%loadedComboTable = .TRUE.
        deallocate(calculator, stat = stat)
        
    end subroutine    
        
    subroutine getFNumAndOctave(this, pNote)
        class(midiPlayer), intent(inout)        :: this 
        type(playerNote), intent(inout)         :: pNote
        integer(kind = 2)                       :: note
        integer(kind = 2)                       :: instru
        
        !
        !   Sound bank has a changer offset, we will see if we really needed it.
        !
        note   = pNote%note + pNote%instrumentP%noteOffset 
        instru = pNote%instrument

        if (debug .eqv. .TRUE.) call debugLog("Note with offset: " // trim(numToText(note)) // ", Instrument: " // trim(numToText(instru))) 
        
        pNote%fNumber = this%comboTable(note)%instruTable(instru)%fnum
        pNote%octave  = this%comboTable(note)%instruTable(instru)%octave         
                   
    end subroutine
    
    

    function fetchDataFromInstrumentByte(instru, slotNum, byteNum, startBit, lenght) result(res)
        type(instrument)            :: instru
        integer(kind = 1)           :: byteNum, startBit, lenght, slotNum
        integer(kind = 1)           :: index, sBit, eBit
    
        integer(kind = 2)           :: res
        character(len = 16)         :: bitString        

        bitString                   = ""
        select case(byteNum)
        case (1)
            write(bitString, "(B16)") instru%byte1(slotNum)
        case (2)
            write(bitString, "(B16)") instru%byte2(slotNum)
        case (3)
            write(bitString, "(B16)") instru%byte3(slotNum)
        case (4)
            write(bitString, "(B16)") instru%byte4(slotNum)
        case (5)
            write(bitString, "(B16)") instru%byte5(slotNum)
        case (6)
            write(bitString, "(B16)") instru%byte6(slotNum)
        case (7)
            write(bitString, "(B16)") instru%feedback
        end select    
        
        do index = 1, 16, 1
           if (bitString(index:index) == " ") bitString(index:index) = "0"  
        end do   
        
        sBit = 9 + (7 - startBit)
        eBit = sBit - lenght
        
        read(bitString(eBit:sBit), "(B" // trim(numToText(lenght)) // ")") res    
    
    end function
    
    subroutine fillChannel(this, channelNum)
        class(midiPlayer), intent(inout)        :: this 
        integer(kind = 1)                       :: channelNum, midiChannelNum, memberIndex, subIndex
        integer(kind = 8)                       :: index, channel, lastNote, nextNote, tempLastNote
        type(instrument), pointer               :: iProgram 
        logical                                 :: LR
        integer(kind = 8)                       :: deltaBuffer
        integer(kind = 2)                       :: note, currInst
        character(len = 16)                     :: word
        
        deltaBuffer         = 0
        memberIndex         = 1
                   
        do index = 1, midiF%tracks(channelNum)%lastMessage, 1
           deltaBuffer = deltaBuffer + midiF%tracks(channelNum)%messages(index)%deltaTime
                  
           if (debug .EQV. .TRUE.) then
               call debugLog("Source Channel: " // trim(numToText(channelNum))) 
           end if     
           select case(midiF%tracks(channelNum)%messages(index)%messageType)
           case("MT")
               select case(midiF%tracks(channelNum)%messages(index)%metaM%typeAsHex)
               case("51")
                    if (debug .EQV. .TRUE.) then
                        call debugLog("MT Tempo")  
                    end if 
                    this%tempo = midiF%tracks(channelNum)%messages(index)%metaM%valueAsNum
               end select
           case("MD")
               channel  = midiF%tracks(channelNum)%messages(index)%midiD%channelNum
               lastNote = this%channels(channel, memberIndex)%lastNote
               if (debug .EQV. .TRUE.) then
                   call debugLog("MD: " // trim(numToText(channel)) // " " // trim(numToText(memberIndex)))  
               end if 
               
               select case(midiF%tracks(channelNum)%messages(index)%midiD%typeAsBin(1:4))
               case("1000") 
               ! Note Off 
                 if (debug .EQV. .TRUE.) then
                     call debugLog("Enter Note OFF")  
                 end if
                 
                 memberIndex = 1  

                 do subIndex = 1, this%channelMemberNums(channel), 1 
                    if (this%channels(channel, subIndex)%hasAnyNotes .EQV. .FALSE.) cycle 
                     
                    tempLastNote = this%channels(channel, subIndex)%lastNote     
                    read(midiF%tracks(channelNum)%messages(index)%midiD%valueAsBin(1), "(B8)") note
                    
                    if (channel == 10) note = fixNoteNum(note + 129 - 35)
                    
                    if (note /= this%channels(channel, subIndex)%playerNotes(tempLastNote)%note) cycle
                    if (        this%channels(channel, subIndex)%currInst /= &
                      &         this%channels(channel, subIndex)%playerNotes(tempLastNote)%instrument) cycle
                    
                    memberIndex = subIndex
                    lastNote    = tempLastNote
                    exit
                 end do
                 
                 this%channels(channel, memberIndex)%playerNotes(lastNote)%endDelta = deltaBuffer
                 this%channels(channel, memberIndex)%playerNotes(lastNote)%closed   = .TRUE.
                 if (this%channels(channel, memberIndex)%playerNotes(lastNote)%endDelta == &
                    &this%channels(channel, memberIndex)%playerNotes(lastNote)%startDelta) then

                     this%channels(channel, memberIndex)%playerNotes(lastNote)%instrument = 0
                     this%channels(channel, memberIndex)%playerNotes(lastNote)%volume     = 0
                     this%channels(channel, memberIndex)%playerNotes(lastNote)%note       = 0
                     this%channels(channel, memberIndex)%playerNotes(lastNote)%startDelta = 0
                     this%channels(channel, memberIndex)%playerNotes(lastNote)%endDelta   = 0       
                     this%channels(channel, memberIndex)%playerNotes(lastNote)%fNumber    = 0
                     this%channels(channel, memberIndex)%playerNotes(lastNote)%octave     = 0
                     this%channels(channel, memberIndex)%playerNotes(lastNote)%freq       = 0
                     this%channels(channel, memberIndex)%lastNote                         = lastNote - 1
                 else    
                     this%channels(channel, memberIndex)%numOfNotes    = this%channels(channel, memberIndex)%numOfNotes    + 1
                     this%channels(channel, memberIndex)%sumOfDuration = this%channels(channel, memberIndex)%sumOfDuration + &
                                                                       & (this%channels(channel, memberIndex)%playerNotes(lastNote)%endDelta -&
                                                                       &  this%channels(channel, memberIndex)%playerNotes(lastNote)%startDelta)

                 end if

                 if (debug .EQV. .TRUE.) then
                     call debugLog("OFF: " // trim(numToText(channel)) // " " &
                                          &// trim(numToText(memberIndex)) // " " &
                                          &// trim(numToText(this%channels(channel, memberIndex)%playerNotes(lastNote)%endDelta)) // " " &
                                          &// trim(numToText(note)))  
                 end if
                 
               case("1001") 
               ! Note On
                 if (debug .EQV. .TRUE.) then  
                    call debugLog("Enter Note ON")
                 end if   
                 memberIndex = 1  
                 
                 do subIndex = 1, maxNumberOfMembers, 1
                    lastNote = this%channels(channel, subIndex)%lastNote 
                    
                    if (lastNote == 0) then
                        memberIndex = subIndex    
                        exit                    
                    end if
                        
                    if (this%channels(channel, subIndex)%playerNotes(lastNote)%closed     .EQV. .FALSE. ) cycle
                    if (this%channels(channel, subIndex)%playerNotes(lastNote)%startDelta == deltabuffer) cycle
                        memberIndex = subIndex    
                        exit                        
                 end do    
                 
                 !call debugLog("FUCK " // trim(numToText(memberIndex)))
                 
                 lastNote = this%channels(channel, memberIndex)%lastNote
                 nextNote = lastNote + 1
                 
                 if (this%channelMemberNums(channel) < memberIndex) this%channelMemberNums(channel) = memberIndex
                 
                 this%channels(channel, memberIndex)%playerNotes(nextNote)%startDelta =  deltaBuffer
                 this%channels(channel, memberIndex)%playerNotes(nextNote)%instrument =  this%channels(channel, memberIndex)%currInst
                 
                 if (this%channels(channel, memberIndex)%playerNotes(nextNote)%instrument /= 0) then
                     this%channels(channel, memberIndex)%playerNotes(nextNote)%instrumentP => this%channels(channel, memberIndex)%instrumentP
                 end if
                 
                 read(midiF%tracks(channelNum)%messages(index)%midiD%valueAsBin(1), "(B8)") &
                     &this%channels(channel, memberIndex)%playerNotes(nextNote)%note                 

                 read(midiF%tracks(channelNum)%messages(index)%midiD%valueAsBin(2), "(B8)") &
                     &this%channels(channel, memberIndex)%playerNotes(nextNote)%volume   

                 this%channels(channel, memberIndex)%hasAnyNotes = .TRUE.
                 this%channels(channel, memberIndex)%playerNotes(nextNote)%closed = .FALSE.
                 this%channels(channel, memberIndex)%lastNote  = nextNote
                 
                 if (channel == 10) call this%fixChannel10(this%channels(channel, memberIndex)%playerNotes(nextNote),&
                     & this%channels(channel, memberIndex))

                 call this%getFNumAndOctave(this%channels(channel, memberIndex)%playerNotes(nextNote))
                                  
                 if (debug .EQV. .TRUE.) then
                     call debugLog("ON:  " // trim(numToText(channel)) // " " // &
                                         &trim(numToText(memberIndex)) // " " // trim(numToText(deltaBuffer)) //   " Note: "          // &
                    &trim(numToText(this%channels(channel, memberIndex)%playerNotes(nextNote)%note))          // " | Velocity: "      // &
                    &trim(numToText(this%channels(channel, memberIndex)%playerNotes(nextNote)%volume))        // " | Freq Number: "   // &
                    &trim(numToText(this%channels(channel, memberIndex)%playerNotes(nextNote)%fNumber))       // " | Octave Number: " // &
                    &trim(numToText(this%channels(channel, memberIndex)%playerNotes(nextNote)%octave))        // " | Instrument: "    // &
                    &trim(numToText(this%channels(channel, memberIndex)%playerNotes(nextNote)%instrument)))  
                 end if
                 
                 
               case("1100")
               ! Program Change  
                 if (debug .EQV. .TRUE.) then
                     call debugLog("Enter Program Change")
                 end if    
                 read(midiF%tracks(channelNum)%messages(index)%midiD%valueAsBin, "(B8)") currInst   
                 currInst = currInst + 1                     
                 
                 do subIndex = 1, maxNumberOfMembers,1 
                     this%channels(channel, subIndex)%currInst = currInst
                     if (currInst /= 0) then
                         this%channels(channel, subIndex)%instrumentP => sBank%instruments(currInst)
                     end if
                 
                 end do
                 
                 if (debug .EQV. .TRUE.) then
                     call debugLog("Instrument:  " // trim(numToText(channel)) // " " // trim(numToText(memberIndex)) // &
                                  &" " // trim(numToText(currInst)))  
                 end if
               end select
           end select 
           
        end do    
        
    end subroutine
    
    function initTempo(this, mode) result(tempo)
        class(midiPlayer), intent(inout)        :: this 
        real(kind = 8)                          :: tempo
        integer(kind = 2)                       :: index, subIndex
        character                               :: mode 
        logical                                 :: endIt
        
        tempo = 0
        endIt = .FALSE.
        
        do index = 1, midiF%numberOfTracks, 1
            do subIndex = 1, midiF%tracks(index)%lastMessage, 1
                if (midiF%tracks(index)%messages(subIndex)%messageType == 'MT') then     
                    if (midiF%tracks(index)%messages(subIndex)%metaM%typeAsHex == '51') then
                        
                        if (midiF%tracks(index)%messages(subIndex)%metaM%valueAsNum > tempo) then
                            tempo = midiF%tracks(index)%messages(subIndex)%metaM%valueAsNum
                            if (mode == 'F') endIt = .TRUE.
                        end if    
                    
                    end if                    
                end if    
                if (endIt .EQV. .TRUE.) exit
            end do   
            if (endIt .EQV. .TRUE.) exit
        end do    
        
    end function        
    !
    ! https://github.com/codenotes/mf2t/blob/master/libmidifile-20150710/midifile.c
    ! 
    ! if (division > 0)
    !    return ((float) (((float)(ticks) * (float)(tempo)) /
    !            ((float)(division) * 1000000.0)));
    !else {
    !    smpte_format = upperbyte(division);
    !    smpte_resolution = lowerbyte(division);
    !    return (float) ((float) ticks / (smpte_format * smpte_resolution *
    !            1000000.0));
    !}
    
    !
    ! https://github.com/FluidSynth/fluidsynth/blob/master/src/midi/fluid_midi.c
    !
    !static void fluid_player_update_tempo(fluid_player_t *player)
    !{
    !    int tempo; /* tempo in micro seconds by quarter note */
    !    float deltatime;
    !
    !    /* do nothing if the division is still unknown to avoid a div by zero */
    !    if(player->division == 0)
    !    {
    !        return;
    !    }
    !
    !    if(fluid_atomic_int_get(&player->sync_mode))
    !    {
    !        /* take internal tempo from MIDI file */
    !        tempo = fluid_atomic_int_get(&player->miditempo);
    !        /* compute deltattime (in ms) from current tempo and apply tempo multiplier */
    !        deltatime = (float)tempo / (float)player->division / (float)1000.0;
    !        deltatime /= fluid_atomic_float_get(&player->multempo); /* multiply tempo */
    !    }
    !    else
    !    {
    !        /* take  external tempo */
    !        tempo = fluid_atomic_int_get(&player->exttempo);
    !        /* compute deltattime (in ms) from current tempo */
    !        deltatime = (float)tempo / (float)player->division / (float)1000.0;
    !    }
    !
    !    fluid_atomic_float_set(&player->deltatime, deltatime);
    !
    !    player->start_msec = player->cur_msec;
    !    player->start_ticks = player->cur_ticks;
    !
    !    FLUID_LOG(FLUID_DBG,
    !              "tempo=%d, tick time=%f msec, cur time=%d msec, cur tick=%d",
    !              tempo, player->deltatime, player->cur_msec, player->cur_ticks);
    !
    !}
  
    !
    ! TPQN = Last 7 bits of the division
    ! fps  = bits 8-14 
    ! ptf  = biut 0-7
    !    
    function deltaTimeToMS(this, ticks, tempo) result(res)
        class(midiPlayer), intent(inout)        :: this
        integer(kind = 8), intent(in)           :: ticks
        integer          , intent(in)           :: tempo   
        real(kind = 8)                          :: res 
        real(kind = 8)                          :: tempoReal, tpqnREAL, fpsREAL, ptfREAL, realTicks
        
        res = 0
        realTicks = ticks
        
        if (this%divisionMode .EQV. .FALSE.) then           
            tempoReal = 60000000 / tempo
            tpqnREAL  = this%TPQN
            
            res       = realTicks * ( tempoReal / tpqnREAL)
        else    
            ptfREAL   = this%ptf
            fpsREAL   = this%fps
            
            res       = realTicks / (fpsREAL * ptfREAL)

        end if           
        
    end function 

    !
    !   This is just a guessing, so we can determinate where are really no
    !   notes played, so we back move the notes to the empty parts and make it 
    !   more compressed for the six only channels.
    !
    
    function valueOfPartOfByte(byteNum, startBit, lenght) result(res)
        integer(kind = 2)                       :: byteNum, startBit, lenght 
        character(len = 16)                     :: bitString
        integer(kind = 1)                       :: index
        integer(kind = 2)                       :: res
        
        write(bitString, "(B16)") byteNum
        do index = 1, 16, 1
           if (bitString(index:index) == " ") bitString(index:index) = "0"
        end do
    
        read(bitString(startBit : startBit+lenght), "(I2)") res
        
        
        
    end function
    
    
end module