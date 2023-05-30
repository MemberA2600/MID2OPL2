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
        integer                                         :: trackNum
        character(len=2), dimension(:), allocatable     :: hexas
        character(len=8), dimension(:), allocatable     :: binaries
          
        contains 
        procedure                                       :: allocateChunk   => AllocateChunk
        procedure                                       :: deAllocateChunk => DeAllocateChunk
                                                           
    end type 
   
    type chunkList
       type(chunk), dimension(:), allocatable           :: listOfChunks, tempList
       integer(kind = 8)                                :: theSize
       
       contains
       procedure                                        :: initList       => initList
       procedure                                        :: deAllocateList => DeAllocateList
       
    end type    
        
    type midiFile
        logical                                         :: loaded = .FALSE.
        integer                                         :: numOfBytes
        integer(kind=1) , dimension(:), allocatable     :: bytes 
        character(len=2), dimension(:), allocatable     :: hexas
        character(len=8), dimension(:), allocatable     :: binaries
        type(chunkList)                                 :: chunks
        
        contains
        procedure                                       :: loadFile => LoadFile
        
   end type
   
   contains
   
   ! 
   ! Chunk routines 
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
   
   !
   ! ChunkList Routines
   !
   
   subroutine InitList(this)
      class(chunkList), intent(inout) :: this
      
      if (allocated(this%listOfChunks) .EQV. .TRUE.) call this%DeAllocateList() 
      this%theSize = 8
      
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
     integer                        :: theSize, stat, index, subIndex
     
     this%loaded = .FALSE.
     
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
     deallocate(this%bytes)   
    
     
     
   end subroutine
    
    
end module