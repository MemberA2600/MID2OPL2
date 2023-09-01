module sequencer
    use                                     :: iso_fortran_env
    use, intrinsic                          :: iso_c_binding  
    use midi

    implicit none
    
    private
    public                                  :: midiPlayer, initialize          
    
    type midiState
        integer(kind = 8)                   :: startDeltaTime = 0, endDeltaTime = 0
        
        logical                             :: noteOn     = .FALSE. !               byte: 1000XXXX / 1001XXXX
        integer(kind = 1)                   :: instrument = 0       ! Progam Change byte: 1100XXXX
        integer(kind = 1)                   :: velocity   = 0       ! Velocity      byte: 1000XXXX / 1001XXXX 
        integer(kind = 1)                   :: note       = 0       ! NoteNum       byte: 1000XXXX / 1001XXXX
               
    end type
    
    type midiChannel
        type(midiState), dimension(:), allocatable   :: midiStates
        integer(kind = 8)                            :: lastOne = 0, size = 0, deltaBuffer = 0
        logical                                      :: inited  = .FALSE.   
        
        contains
        procedure                                    :: InitChannel       => initChannel
        procedure                                    :: DoubleChannelSize => doubleChannelSize
        procedure                                    :: AddMidiState      => addMidiState
        
    end type    
    
    type chipState
        integer(kind = 8)                           :: startDeltaTime = 0, endDeltaTime = 0
        integer(kind = 1)                           :: volume  = 0           ! Main Volume         byte: 00000111 + 00100111
        integer(kind = 1)                           :: vibratioRate  = 0     !                     byte: 01001100 
        integer(kind = 1)                           :: vibratioDepth = 0     !                     byte: 01001101 
        integer(kind = 1)                           :: vibratioDelay = 0     !                     byte: 01001110 
        integer(kind = 8)                           :: tempo = 120           ! Meta 
        
        
    end type    
        
        
    type midiPlayer
        type(midiFile), pointer                        :: midiF
        type(midiChannel), dimension(16)               :: midiChannels
        type(chipState), dimension(:), allocatable     :: chipStates
        integer(kind = 8)                              :: chipStateSize = 0, chipStateLast = 0, deltaBuffer = 0 
        logical                                        :: SMPTEMode = .FALSE.
        integer(kind = 8)                              :: ticksPerQuarterNote = 0
        integer                                        :: fps = 0, resolution = 0
        
        contains
        procedure                                      :: Initialize       => initialize
        procedure                                      :: DoubleChipStates => doubleChipStates
        procedure                                      :: AddChipState     => addChipState
        procedure                                      :: AddState         => addState

    end type
    
    contains 
    
    !    
    !  midiPlayer routines
    !    
    subroutine initialize(this, midiF)    
        class(midiPlayer), intent(inout)          :: this   
        type(midiFile), target, intent(in)        :: midiF
        integer(kind = 2)                         :: index, stat
        integer(kind = 8)                         :: subIndex  
        
        this%midiF => midiF
        
        this%ticksPerQuarterNote = midiF%TPQN
        this%fps                 = midiF%fps
        this%resolution          = midiF%ptf
        
        if (this%ticksPerQuarterNote = 0) SMPTEMode = .TRUE.
        
        allocate(this%chipStates(64), stat = stat)
        this%chipStateSize = 64
        this%chipStateLast = 64 
        
        
        do index = 1, 16, 1
           !call this%midiChannels(index)%initChannel()
           
           this%deltaBuffer = 0 
           if (midiF%tracks(index)%lastMessage > 0) then
              do subIndex = 1, midiF%tracks(index)%lastMessage, 1
                 !call this%midiChannels(index)%addState(midiF%tracks(index)%messages(subIndex))
                  call this%addState(midiF%tracks(index)%messages(subIndex))
              end do    
           end if 
        end do     
        
    end subroutine
    
    subroutine doubleChipStates(this)
        class(midiPlayer), intent(inout)             :: this   
        integer(kind = 2)                            :: stat
        integer(kind = 8)                            :: index
        type(chipState), dimension(:), allocatable   :: tempStates
    
        allocate(tempStates(this%chipStateSize), stat = stat)
        
        do index = 1, this%chipStateSize, 1
           tempStates(index)%startDeltaTime = this%chipStates(index)%startDeltaTime
           tempStates(index)%endDeltaTime   = this%chipStates(index)%endDeltaTime
           tempStates(index)%volume         = this%chipStates(index)%volume
           tempStates(index)%vibratioRate   = this%chipStates(index)%vibratioRate
           tempStates(index)%vibratioDepth  = this%chipStates(index)%vibratioDepth
           tempStates(index)%vibratioDelay  = this%chipStates(index)%vibratioDelay
            
        end do    
        
        deallocate(this%chipStates, stat = stat)
        
        this%chipStateSize = this%chipStateSize * 2
        
        allocate(this%chipStates(this%chipStateSize), stat = stat)
        
        do index = 1, this%chipStateLast, 1
           this%chipStates(index)%startDeltaTime = tempStates(index)%startDeltaTime
           this%chipStates(index)%endDeltaTime   = tempStates(index)%endDeltaTime
           this%chipStates(index)%volume         = tempStates(index)%volume
           this%chipStates(index)%vibratioRate   = tempStates(index)%vibratioRate
           this%chipStates(index)%vibratioDepth  = tempStates(index)%vibratioDepth
           this%chipStates(index)%vibratioDelay  = tempStates(index)%vibratioDelay

        end do
        
        deallocate(tempStates, stat = stat)
        
    end subroutine    
        
    subroutine addChipState(this, midiM)
        use midi
        class(midiPlayer), intent(inout)             :: this
        type(message)                                :: midiM 
        
        if (this%chipStateSize == this%chipStateLast) call this%doubleChipStates()    
        
        
    end subroutine
    
    subroutine addState(this, midiM)
        use midi
        class(midiPlayer), intent(inout)             :: this
        type(message)                                :: midiM 
        integer(kind = 8)                            :: deltaTime
        
        deltaTime = midiM%deltaTime
        
        select case(midiM%messageType)
        case("MT')
            !
        case("SE")
            !
        case("MD")
            !
        end select
        
        
    end subroutine
    
    !
    !   midiChannel routines
    !
    
    subroutine initChannel(this)
        class(midiChannel), intent(inout)       :: this
        integer(kind = 2)                       :: stat
    
        allocate(this%midiStates(64), stat =  stat)
        this%lastOne = 64
        this%size    = 64
        this%inited  = .TRUE.
        
    end subroutine

    subroutine doubleChannelSize(this)
        class(midiChannel), intent(inout)            :: this
        integer(kind = 2)                            :: stat
        integer(kind = 8)                            :: index
        type(midiState), dimension(:), allocatable   :: tempStates
        
        allocate(tempStates(this%size), stat =  stat)
        
        do index = 1, this%size, 1
           tempStates(index)%noteOn         = this%midiStates(index)%noteOn
           tempStates(index)%instrument     = this%midiStates(index)%instrument
           tempStates(index)%velocity       = this%midiStates(index)%velocity
           tempStates(index)%note           = this%midiStates(index)%note
           tempStates(index)%startDeltaTime = this%midiStates(index)%startDeltaTime
           tempStates(index)%endDeltaTime   = this%midiStates(index)%endDeltaTime
           
        end do
        
        deallocate(this%midiStates, stat = stat)
        
        this%size = this%size * 2 
        
        allocate(this%midiStates(this%size), stat = stat)
        do index = 1, this%lastOne, 1
           this%midiStates(index)%noteOn         = tempStates(index)%noteOn
           this%midiStates(index)%instrument     = tempStates(index)%instrument
           this%midiStates(index)%velocity       = tempStates(index)%velocity
           this%midiStates(index)%note           = tempStates(index)%note
           this%midiStates(index)%startDeltaTime = tempStates(index)%startDeltaTime 
           this%midiStates(index)%endDeltaTime   = tempStates(index)%endDeltaTime
           
        end do
        deallocate(tempStates, stat = stat)

    end subroutine    
    
    subroutine addMidiState(this, midiM, midiP)       
        class(midiChannel), intent(inout)            :: this
        type(midiPlayer), intent(inout)              :: midiP
        type(message)                                :: midiM 
        
        if (this%inited .EQV. .FALSE.) call this%initChannel()
        if (this%size == this%lastOne) call this%doubleChannelSize()
        this%lastOne  = this%lastOne + 1
        
    end subroutine
end module