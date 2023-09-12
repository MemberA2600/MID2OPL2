module player   
    use midi
    use soundbank
    
    implicit none
    
    private
    public                                      :: midiPlayer, initPlayer
    
    logical, parameter                          :: debug = .TRUE.
    
    type midiPlayer
        logical                                 :: success = .FALSE., divisionMode = .FALSE., dbgLogFirst
        integer                                 :: TPQN, fps, ptf, tempo = 120 
        type(midiFile), pointer                 :: midiF
        type(soundB)  , pointer                 :: sBank
        integer(kind = 8)                       :: maxTime
        
        contains                        
        procedure                               :: InitPlayer    => initPlayer
        procedure                               :: DeltaTimeToMS => deltaTimeToMS
        procedure                               :: InitTempo     => initTempo    

    end type
        
    contains    
    
    function numToText(number) result(txt)
        integer(kind = 8), intent(in)           :: number
        character(len = 20)                     :: txt
           
        txt                                     = ""
        write(txt, "(I20)") number
    
    end function
    
    subroutine debugLog(txt, dbgLogFirst)
        character(len = *)                      :: txt
        logical, intent(inout)                  :: dbgLogFirst
        character(len = 6)                      :: mode 
    
        if (dbgLogFirst .EQV. .TRUE.) then
            dbgLogFirst = .FALSE.
            mode        = "write "
        else
            mode        = "append"
        end if 
            
        open(89, file = "playerDBG.txt", action = mode)
        write(89, "(A)") txt
        close(89)
    
    end subroutine
    
    subroutine initPlayer(this, midiF, sBank)
        use midi
        use soundbank
    
        class(midiPlayer), intent(inout)        :: this
        type(midiFile), intent(in), target      :: midiF
        type(soundB),   intent(in), target      :: sBank
        
        this%dbgLogFirst                        = .TRUE.
        
        this%midiF                              => midiF       
        this%sBank                              => sBank
        
        this%success                            = .FALSE.
        this%TPQN                               = midiF%TPQN     
        this%fps                                = midiF%fps
        this%ptf                                = midiF%ptf 
        this%divisionMode                       = midiF%divisionMode
        
        this%tempo                              = this%initTempo("M")
        if (debug .EQV. .TRUE.) then
            call debugLog("Max Tempo:" // numToText(this%tempo), this%dbgLogFirst)
            
        end if   
        
        this%maxTime                            = this%deltaTimeToMS(this%midiF%deltaFull, this%tempo)
        this%tempo                              = this%initTempo("F") 
        
    end subroutine
        
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
    ! TPQN = Last 7 bits of the division
    ! fps  = bits 8-14 
    ! ptf  = biut 0-7
    !
    
    function initTempo(this, mode) result(tempo)
        class(midiPlayer), intent(inout)        :: this 
        real(kind = 8)                          :: tempo
        integer(kind = 2)                       :: index, subIndex
        character                               :: mode 
        logical                                 :: endIt
        
        tempo = 0
        endIt = .FALSE.
        
        do index = 1, this%midiF%numberOfTracks, 1
            do subIndex = 1, this%midiF%tracks(index)%lastMessage, 1
                if (this%midiF%tracks(index)%messages(subIndex)%messageType == 'MT') then     
                    if (this%midiF%tracks(index)%messages(subIndex)%metaM%typeAsHex == '51') then
                        
                        if (this%midiF%tracks(index)%messages(subIndex)%metaM%valueAsNum > tempo) then
                            tempo = this%midiF%tracks(index)%messages(subIndex)%metaM%valueAsNum
                            if (mode == 'F') endIt = .TRUE.
                        end if    
                    
                    end if                    
                end if    
                if (endIt .EQV. .TRUE.) exit
            end do   
            if (endIt .EQV. .TRUE.) exit
        end do    
        
    end function        
        
    function deltaTimeToMS(this, ticks, tempo) result(res)
        class(midiPlayer), intent(inout)        :: this
        integer(kind = 8), intent(in)           :: ticks
        integer          , intent(in)           :: tempo   
        real(kind = 8)                          :: res
    
        res = 0
        
        if (this%divisionMode .EQV. .FALSE.) then
            res = (ticks * tempo) / (1000000.0 * this%TPQN)
        else    
            res = ticks / (this%fps * this%ptf * 1000000.0)  
        end if    
        
    end function 

    
end module