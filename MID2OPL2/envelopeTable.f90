module envelope
    
    implicit none
    
    private
    public                                          :: adTable, initialize, getValue 
    
    logical, parameter                              :: debug  = .TRUE.
    
    type adtRecord
        integer(kind = 1)                           :: rate, RKS
        real(kind = 8), dimension(2)                :: attack, decay
    end type    
    
    type adTable                               
        type(adtRecord), dimension(:), allocatable  :: records
        integer                                     :: numOfRecords
        real(kind = 8)                              :: lastVal
    
        contains
        procedure                                   :: Initialize   => initialize 
        procedure                                   :: GetValue     => getValue 
        
    end type
    
    contains
    
    subroutine initialize(this)
        class(adTable), intent(inout)               :: this
        integer(kind = 1)                           :: stat
        character                                   :: dummy
        integer(kind = 2)                           :: numOfLines, index, subIndex
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
        this%numOfRecords = numOfLines
        
        do index = 1, numOfLines, 1
           read(22, *) this%records(index)%rate     , this%records(index)%RKS     , &
                     & this%records(index)%attack(1), this%records(index)%decay(1), &
                     & this%records(index)%attack(2), this%records(index)%decay(2)             
        end do    
        
        close(22)
        
        if (debug .EQV. .TRUE.) then
            open(43, file = "tableDebug.txt", action = "write")
            do index = 0, 15, 1 
               do subIndex = 0, 3, 1  
                   write(43, "(F0.5)") this%getValue(index, subIndex, "A", .FALSE.) 
                   write(43, "(F0.5)") this%getValue(index, subIndex, "D", .FALSE.) 
                   write(43, "(F0.5)") this%getValue(index, subIndex, "A", .TRUE.) 
                   write(43, "(F0.5)") this%getValue(index, subIndex, "D", .TRUE.) 
               end do 
            end do 
            close(43)
        end if    
123     &            
    end subroutine
    
        
    function getValue(this, rate, rks, typ, KSR) result(value)    
        class(adTable), intent(inout)                       :: this
        integer(kind = 2)                                   :: rate, rks, num
        real(kind = 8)                                      :: value
        logical                                             :: KSR
        character                                           :: typ

        
        if (rate == 0) then
           value = this%lastVal 
           if (debug .EQV. .TRUE.) write(43, "(I0, 1x, I0, 1x, L, 1x, I0)") rate, rks, KSR , value
        else    
           do num = 1, this%numOfRecords, 1
              if (this%records(num)%rate == rate .AND. this%records(num)%rks == rks) then  
                   if (typ == "A") then
                       if (KSR .EQV. .FALSE.) then
                           value = this%records(num)%attack(1)
                       else
                           value = this%records(num)%attack(2)
                       end if    
                   else
                       if (KSR .EQV. .FALSE.) then
                          value = this%records(num)%decay(1)
                       else
                          value = this%records(num)%decay(2)
                       end if   
                       this%lastVal = value
                   end if  
                   exit
                 end if
            end do
            if (debug .EQV. .TRUE.) write(43, "(I0, 1x, I0, 1x, L, 1x, I0)") rate, rks, KSR, num
         end if
    end function
           
end module
    
    