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
       type(chunk), dimension(:), allocatable           :: listOfChunks
       integer(kind = 8)                                :: theSize, last
       
       contains
       procedure                                        :: initList       => InitList
       procedure                                        :: deAllocateList => DeAllocateList
       procedure                                        :: doubleSize     => DoubleSize
       procedure                                        :: nextChunk      => NextChunk
       
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
      integer                         :: stat
      
      if (allocated(this%listOfChunks) .EQV. .TRUE.) then
          call this%DeAllocateList() 
          deallocate(this%listOfChunks, stat = stat)
      end if    
      this%theSize = 16
      this%last    = 0
      
      allocate(this%listOfChunks(this%theSize), stat = stat)
      
   end subroutine
   
   subroutine NextChunk(this, hexArray, binArray, currentIndex)
      class(chunkList), intent(inout)       :: this
      character(len = 2), dimension(*)      :: hexArray 
      character(len = 8), dimension(*)      :: binArray 
      integer(kind = 8), intent(inout)      :: currentIndex
      integer(kind = 8)                     :: index
      
      if (this%theSize == this%last) call this%doubleSize()
      this%last = this%last + 1
     
      if (getASCIIFromBytes(hexArray(1:4), 4) .EQ. "MThd") then
         this%listOfChunks(this%last)%header = .TRUE.
      else
         this%listOfChunks(this%last)%header = .FALSE.
      end if
      
      this%listOfChunks(this%last)%theSize = getInt32FromBytes(hexArray(5:8))

      !OPEN(unit=12, file="szar.txt", position="append", action = "write")
      !WRITE(12, *) getASCIIFromBytes(hexArray(1:4), 4), currentIndex, this%listOfChunks(this%last)%theSize, hexArray(8:this%listOfChunks(this%last)%theSize + 8)
      !CLOSE(12)
      
      call this%listOfChunks(this%last)%allocateChunk()
      
      do index = 1, this%listOfChunks(this%last)%theSize, 1
         this%listOfChunks(this%last)%hexas(index)    = hexArray(index + 8)        
         this%listOfChunks(this%last)%binaries(index) = binArray(index + 8)
      end do    
      
      currentIndex = currentIndex + 8 + this%listOfChunks(this%last)%theSize
      
   end subroutine   
   
   function getASCIIFromBytes(hexArray, theSize) result(theText)
     integer                                :: theSize, index
     character(len = 2), dimension(theSize) :: hexArray 
     character(len = theSize)               :: theText
     integer(kind = 2)                      :: asNum
     
     do index = 1, theSize, 1
        READ(hexArray(index), "(Z2)") asNum
        theText(index:index) = char(asNum)
     end do    
   
   end function

   function getInt32FromBytes(hexArray) result(theNumber)
     integer                                    :: index, subIndex
     character(len = 2), dimension(4)           :: hexArray 
     character(len = 8)                         :: theText
     integer(kind = 8)                          :: theNumber
     
     do index = 1, 4, 1
        subIndex = (index - 1) * 2 + 1  
        theText(subIndex  :subIndex  ) = hexArray(index)(1:1)
        theText(subIndex+1:subIndex+1) = hexArray(index)(2:2)       
     end do    
   
     read(theText, "(Z8)") theNumber
     
   end function 
     
   subroutine DoubleSize(this)
      class(chunkList), intent(inout)        :: this
      integer                                :: stat
      integer(kind = 8)                      :: index, subIndex, oldSize
      type(chunk), dimension(:), allocatable :: tempList

      allocate(tempList(this%theSize), stat = stat)   
      
      do index = 1, this%theSize, 1
         tempList(index)%header    = this%listOfChunks(index)%header 
         tempList(index)%theSize   = this%listOfChunks(index)%theSize
         tempList(index)%trackNum  = this%listOfChunks(index)%trackNum

         call templist(index)%allocateChunk()   
         
         do subIndex = 1, this%listOfChunks(index)%theSize, 1
            tempList(index)%hexas(subIndex)    = this%listOfChunks(index)%hexas(subIndex)
            tempList(index)%binaries(subIndex) = this%listOfChunks(index)%binaries(subIndex)
         end do    
         
         call this%listOfChunks(index)%deAllocateChunk()
      end do    
   
      oldSize      = this%theSize   
      this%theSize = this%theSize * 2
      
      deallocate(this%listOfChunks               , stat = stat)
      allocate(  this%listOfChunks(this%theSize) , stat = stat)
      
      do index = 1, oldSize, 1
         this%listOfChunks(index)%header   = tempList(index)%header
         this%listOfChunks(index)%theSize  = tempList(index)%theSize
         this%listOfChunks(index)%trackNum = tempList(index)%trackNum

         call this%listOfChunks(index)%allocateChunk()   
         
         do subIndex = 1, tempList(index)%theSize, 1
            this%listOfChunks(index)%hexas(subIndex)    = tempList(index)%hexas(subIndex)
            this%listOfChunks(index)%binaries(subIndex) = tempList(index)%binaries(subIndex)  
         end do    
         
         call tempList(index)%deAllocateChunk()
      end do 
      
      deallocate(tempList, stat = stat)
      
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
     integer                        :: theSize, stat
     integer(kind = 8)              :: index, subIndex, currentIndex
     
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

     !
     !  Get pure hexString and binString data
     !
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
     deallocate(this%bytes, stat = stat)   
     
     call this%chunks%initList()
     currentIndex = 1
    
     !
     !  Create chunks from binary: Chunk Type (Header / Track), Size, Data of Chunk
     !
     do while (currentIndex < size(this%hexas))
        call this%chunks%NextChunk(this%hexas(currentIndex:size(this%hexas)), &
                                 & this%binaries(currentIndex:size(this%binaries)), currentIndex) 
         
     end do
         
     deallocate(this%hexas    , stat = stat)
     deallocate(this%binaries , stat = stat)

     !
     !  Create Header / Track Messages for each chunk!
     !
     
     do currentIndex = 1, this%chunks%last, 1
        ! TODO 
         
     end do
     
   end subroutine
    
    
end module