module midi
    use iso_fortran_env
    use, intrinsic :: iso_c_binding  
     
    implicit none
    
    private
    public :: midiFile, LoadFile
    
    type midiFile
        logical                                         :: loaded = .FALSE.
        integer                                         :: numOfBytes
        integer(kind=1), dimension(:), allocatable      :: bytes 
        character(len=2), dimension(:), allocatable     :: hexas
        
        contains
        procedure                                       :: loadFile => LoadFile
        
   end type

   contains
        
   subroutine LoadFile(this, path)
     class(midiFile), intent(inout) :: this
     character(len = *)             :: path
     integer                        :: theSize, stat, index
     
     inquire(file = path, size = theSize)
     this%numOfBytes = theSize
     
     allocate(this%bytes(theSize), stat = stat)
     ! allocate(this%hexas(theSize), stat = stat)
     
     OPEN(unit=11, file=path, access="stream", status="old", action="read",iostat=stat)     
     read(11, iostat=stat) this%bytes
     CLOSE(11)

     OPEN(unit=12, file="C:\output.txt")
     do index = 1, theSize, 1
        WRITE(this%hexas(index), '(Z2)', iostat = stat) this%bytes(index)
        
        if (this%hexas(index)(1:1) == " ") this%hexas(index)(1:1) = "0"
        ! WRITE(12, "(A)") this%hexas(index)
        
     end do    
   
     !CLOSE(12)
     
   end subroutine
    
    
end module