module envelope
    
    implicit none
    
    private
    public                                          :: adTable, initialize, getValue 
    
    logical, parameter                              :: debug = .FALSE.
    
    type adtRecord
        integer(kind = 1)                           :: rate, RKS
        real(kind = 8), dimension(2)                :: attack, decay
    end type    
    
    type adTable                               
        type(adtRecord), dimension(:), allocatable  :: records
        integer                                     :: numOfRecords
        
        contains
        procedure                                   :: Initialize   => initialize 
        procedure                                   :: GetValue     => getValue
        procedure                                   :: ChangeValues => changeValues
        
    end type
    
    contains
    
    subroutine initialize(this)
        class(adTable), intent(inout)               :: this
        integer(kind = 1)                           :: stat
        character                                   :: dummy
        integer(kind = 2)                           :: numOfLines, index
        logical                                     :: ok
        
        inquire(file = "attack_decay.txt", exist = ok)
        if (ok .EQV. .FALSE.) goto 123
        
        open(unit = 22, file = "attack_decay.txt", action = "read")    
            
        numOfLines = 0
        do 
            read(22, "(A)", iostat = stat) dummy
            if (stat /= 0) exit
            numOfLines = numOfLines + 1
        end do    
        
        rewind(22)
        
        allocate(this%records(numOfLines), stat = stat)
        
        do index = 1, numOfLines, 1
           read(22, *) this%records(index)%rate     , this%records(index)%RKS     , &
                     & this%records(index)%attack(1), this%records(index)%decay(1), &
                     & this%records(index)%attack(2), this%records(index)%decay(2)             
        end do    
        
        close(22)
        
        if (debug .EQV. .TRUE.) then
            open(43, file = "tableDebug.txt", action = "write")
            do index = 1, numOfLines, 1
                write(43, "(I2, 1x, I1, 1x, F8.2, 1x, F8.2, 1x, F8.2, 1x,F8.2)") &
                                                            & this%records(index)%rate     , this%records(index)%RKS     , &
                                                            & this%records(index)%attack(1), this%records(index)%decay(1), &
                                                            & this%records(index)%attack(2), this%records(index)%decay(2) 
            end do
            close(43)
        end if    
123     &            
    end subroutine
    
        
    ! RM  = Bits of Attack / Decay Rate
    ! RL  = RKS
    ! KSR = KSR bit, changing values a little
    ! C   = 0: Not "First Time", Attack 
    ! C   = 1: Not "First Time", Decay 
    ! C   = 2: "First Time"    , Attack 
    ! C   = 3: "First Time"    , Decay 
    !    
    function getValue(this, RM, RL, C, KSR) result(value)
        class(adTable), intent(inout)                       :: this
        integer(kind = 2)                                   :: offset, RM, RL, C
        real(kind = 8)                                      :: value
        real(kind = 8), dimension(4)                        :: temps
        logical                                             :: KSR
         
        if (KSR .EQV. .TRUE.) call this%changeValues(RM, RL) 
        
        if (RM == 0) then
           value = 0 
        else    
           offset   = 60 - ((RM-1) * 4 + RL +1)
           temps(1) = this%records(offset)%attack(1)
           temps(2) = this%records(offset)%decay (1)
           temps(3) = this%records(offset)%attack(2)
           temps(4) = this%records(offset)%decay (2)
           value    = temps(C)
        end if
        
    end function
        
    subroutine changeValues(this, RM, RL)
        class(adTable), intent(inout)                       :: this
        integer(kind = 2), intent(inout)                    :: RM, RL
        integer(kind = 2)                                   :: index
        integer                                             :: temp
        character(6)                                        :: bitString
        
        temp      = (4 * RM) + RL
        bitString = ""
        
        write(bitString, "(I0)") temp
        
        do index = 1, 6, 1
           if (bitString(index:index) == " " ) bitString(index:index) = "0" 
        end do
        
        read(bitString(1:4), "(I0)") RM
        read(bitString(5:6), "(I0)") RL
        
    end subroutine    
end module
    
    