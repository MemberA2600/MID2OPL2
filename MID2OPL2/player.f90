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
    
    
    logical, parameter                          :: debug                = .TRUE. , printTable = .TRUE.
    logical                                     :: dbgLogFirst          = .FALSE.
    type(midiFile), pointer                     :: midiF
    type(soundB)  , pointer                     :: sBank
    integer, parameter                          :: maxNumberOfMembers   = 5
    
    type playerNotePointer
        type(playerNote), pointer               :: p
         
    end type    
    
    type playerNote
        integer(kind = 2)                       :: instrument, volume, note, fNumber, octave
        real(kind = 8)                          :: freq
        integer(kind = 8)                       :: startDelta, endDelta        
        type(instrument), pointer               :: instrumentP
        logical                                 :: closed

    end type    
    
    type playerChannel
        type(playerNote), dimension(:), allocatable :: playerNotes
        logical                                     :: hasAnyNotes = .FALSE.
        
        type(instrument), pointer                   :: instrumentP
        integer(kind = 8)                           :: lastNote = 0, currInst = 0
        
    end type    
    
    type noteFreqPair
        integer(kind = 2)                           :: note
        real(kind = 8)                              :: freq
        
    end type    
    
    type freqCombo3
        integer(kind = 2)                           :: fnum, octave      
        real(kind = 8)                              :: value, difference
        real(kind = 4)                              :: multi
        
    end type        
 
    type freqCombo2
    !  
    !   There are 13 different freq multipliers 
    !    
        integer(kind = 2)                           :: instrument
        type(instrument), pointer                   :: instrumentP
        type(freqCombo3), dimension(13)             :: multiTable
        
    end type
    
    type freqCombo
    !  
    !   There are 128 different non-percussion instruments 
    !    
        real(kind = 8)                              :: freq
        type(freqCombo2), dimension(128)            :: instruTable
        
    end type
    
    type midiPlayer
        logical                                     :: success = .FALSE., divisionMode = .FALSE., dbgLogFirst
        integer                                     :: TPQN, fps, ptf, tempo = 120 
        integer(kind = 8)                           :: maxTime, maxNumberOfNotes
        type(playerChannel), &
             dimension(16, maxNumberOfMembers)      :: channels
        type(playerNotePointer), dimension(:,:),&
             & allocatable                          :: notePointers
        type(playerNotePointer), dimension(:,:),&
             & allocatable                          :: percussionPointers

        
        integer(kind = 1), dimension(16)            :: channelMemberNums = 1
        type(noteFreqPair), dimension(128)          :: noteFreqTable
        logical                                     :: loadedTable       = .FALSE.
        
        !
        ! 128 midi notes = 128 freq values
        !
        type(freqCombo),    dimension(128)          :: comboTable
        logical                                     :: loadedComboTable  = .FALSE.

        
        contains                        
        procedure                                   :: InitPlayer     => initPlayer
        procedure                                   :: DeltaTimeToMS  => deltaTimeToMS
        procedure                                   :: InitTempo      => initTempo    
        procedure                                   :: FillChannel    => fillChannel    
        procedure                                   :: LoadTable      => loadTable    
        procedure                                   :: GetFNums       => getFNums
        procedure                                   :: LoadComboTable => loadComboTable    
        
    end type
        
    contains    
    
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
    
    subroutine initPlayer(this, midiFP, sBankP)
        use midi
        use soundbank
    
        class(midiPlayer), intent(inout)        :: this
        type(midiFile), intent(in), target      :: midiFP
        type(soundB),   intent(in), target      :: sBankP
        integer(kind = 8)                       :: index, longestDelta, subIndex, memberIndex
        integer(kind = 1)                       :: stat
        
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
        
        if (allocated(this%notePointers) .EQV. .TRUE.) deallocate(this%notePointers, stat = stat)
        allocate(this%notePointers(16, this%maxNumberOfNotes)        , stat = stat) 
        if (debug .EQV. .TRUE.) then
            call debugLog("Allocate note pointer array as 16:" // trim(numToText(this%maxNumberOfNotes)) // ": " // trim(numToText(stat)))  
        end if
        
        if (allocated(this%percussionPointers) .EQV. .TRUE.) deallocate(this%percussionPointers, stat = stat)
        allocate(this%percussionPointers(5, this%maxNumberOfNotes)  , stat = stat) 
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
              
              this%channels(index, memberIndex)%lastNote    = 0
              this%channels(index, memberIndex)%currInst    = 0
              this%channels(index, memberIndex)%hasAnyNotes = .FALSE.
              
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

              end do
           end do
        end do    

        do index = 1, midiF%numberOfTracks, 1 
           !if (debug .EQV. .TRUE.) then
           !    call debugLog("Size of Channel Array " // trim(numToText(index)) // ":" // trim(numToText(size(this%channels(index, 1)%playerNotes)))) 
           !end if
           call this%fillChannel(index)
        end do
        
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
        integer(kind = 2)                             :: stat, noteIndex, multiIndex, saveMultiIndex, instruIndex
        real(kind = 4)                                :: lastMulti, currMulti 
               
        !
        ! NoteIndex : 1-128, InstruIndex = 1-128, MultiIndex = 1-13
        !
        ! 10 bits (1024) * 
        !
        
        if (allocated(calculator) .EQV. .TRUE.) deallocate(calculator, stat = stat)
        allocate(calculator(1), stat = stat)
        
        !
        !  Notes are 0-127
        !
        do noteIndex = 1, 128, 1
           this%comboTable(noteIndex)%freq = this%noteFreqTable(noteIndex)%freq
        !
        !  Non-Percussion Instruments are 0-127
        !
           do instruIndex = 1, 128 ,1
              this%comboTable(noteIndex)%instruTable(instruIndex)%instrument  =  instruIndex 
              this%comboTable(noteIndex)%instruTable(instruIndex)%instrumentP => sBank%instruments(instruIndex)
           
              lastMulti                   = 0
              saveMultiIndex              = 0  
              
              do multiIndex = 1, 15, 1
                 currMulti = multiNums(multiIndex) 
                 if (currMulti == lastMulti) cycle 
                 
                 lastMulti                = currMulti
                 saveMultiIndex           = saveMultiIndex + 1
                 
                 this%comboTable(noteIndex)%instruTable(instruIndex)%multiTable(saveMultiIndex)%multi = lastMulti
                 
              end do    
           
           end do    
           
        end do    
        
    end subroutine    
        
    subroutine getFNums(this, pNote)
        class(midiPlayer), intent(inout)        :: this 
        type(playerNote), intent(inout)         :: pNote
        integer(kind = 2)                       :: note, fNum
        integer(kind = 1)                       :: octave, index, multi
        character(len = 16)                     :: bitString
        
        !
        !   Sound bank has a changer, we will see if we really needed it.
        !

        note = pNote%note + pNote%instrumentP%noteOffset
    
        !   
        !   There is a freq multi that is preset in the sound bank and can be different for
        !   the two slots of the channel. We are gonna use the second one only for calculation. 
        !
    
        bitString = ""
        write(bitString, "(B16)") pNote%instrumentP%byte1(2)
        do index = 1, 16, 1
           if (bitString(index:index) == " ") bitString(index:index) = "0"  
        end do   
        
        read(bitString(12:16), "(B4)") multi       
        
        !
        !   freq = freqBases(octave+1) + freqSteps(octave+1) * Fnum
        !

        do index = 1, 128, 1
           if (pNote%note == this%noteFreqTable(index)%note) then
               pNote%freq = this%noteFreqTable(index)%freq
               exit
           end if    
        end do
        
        
        
    end subroutine
    
    subroutine fillChannel(this, channelNum)
        class(midiPlayer), intent(inout)        :: this 
        integer(kind = 1)                       :: channelNum, midiChannelNum, memberIndex, subIndex
        integer(kind = 8)                       :: index, channel, lastNote, nextNote, tempLastNote
        type(instrument), pointer               :: iProgram 
        logical                                 :: LR
        integer(kind = 8)                       :: deltaBuffer
        integer(kind = 2)                       :: note
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
                    if (this%channels(channel, subIndex)%playerNotes(tempLastNote)%closed .EQV. .TRUE.) cycle 

                    read(midiF%tracks(channelNum)%messages(index)%midiD%valueAsBin(1), "(B8)") note                     
                    if (this%channels(channel, subIndex)%playerNotes(tempLastNote)%note == note .AND. &
                      & this%channels(channel, subIndex)%playerNotes(tempLastNote)%instrument == &
                      & this%channels(channel, subIndex)%currInst) then
                        memberIndex = subIndex
                        lastNote    = tempLastNote
                        exit
                    end if    
                 end do

                 if (debug .EQV. .TRUE.) then
                     call debugLog("OFF: " // trim(numToText(channel)) // " " // trim(numToText(memberIndex)) // " " // trim(numToText(note)))  
                 end if
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
                 end if
               case("1001") 
               ! Note On
                 if (debug .EQV. .TRUE.) then  
                    call debugLog("Enter Note ON")
                 end if   
                 memberIndex = 1  
                 
                 do subIndex = 1, maxNumberOfMembers, 1
                    nextNote = this%channels(channel, subIndex)%lastNote + 1
                    if (this%channels(channel, subIndex)%playerNotes(nextNote)%note == 0) then
                        memberIndex = subIndex
                        exit
                    end if    
                 end do
                 
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

                 call this%getFNums(this%channels(channel, memberIndex)%playerNotes(nextNote))
                 
                 if (debug .EQV. .TRUE.) then
                     call debugLog("ON:  " // trim(numToText(channel)) // " " // trim(numToText(memberIndex)) // " Note: " // &
                         &trim(numToText(this%channels(channel, memberIndex)%playerNotes(nextNote)%note)) // " | Velocity: " // &
                         &trim(numToText(this%channels(channel, memberIndex)%playerNotes(nextNote)%volume)) )  
                 end if
                 
               case("1100")
               ! Program Change  
                 if (debug .EQV. .TRUE.) then
                     call debugLog("Enter Program Change")
                 end if    
                 read(midiF%tracks(channelNum)%messages(index)%midiD%valueAsBin, "(B8)") &
                     &this%channels(channel, memberIndex)%currInst   
                 
                 this%channels(channel, memberIndex)%currInst = this%channels(channel, memberIndex)%currInst + 1
                 
                 if (this%channels(channel, memberIndex)%currInst /= 0) then
                     this%channels(channel, memberIndex)%instrumentP => sBank%instruments(this%channels(channel, memberIndex)%currInst)
                 end if
                 
                 if (debug .EQV. .TRUE.) then
                     call debugLog("Instrument:  " // trim(numToText(channel)) // " " // trim(numToText(memberIndex)) // &
                                  &" " // trim(numToText(this%channels(channel, memberIndex)%currInst)))  
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