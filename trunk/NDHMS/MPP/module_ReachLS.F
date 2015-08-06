!   This is used as a coupler with the WRF model.
MODULE MODULE_mpp_ReachLS

  use module_mpp_land, only:  io_id, my_id, mpp_status, mpp_land_max_int1
  implicit none


  TYPE Grid2ReachMap
      real,allocatable, dimension(:) :: sv
      real,allocatable, dimension(:) :: rv
      real,allocatable, dimension(:) :: rvId
      real,allocatable, dimension(:) :: snId
  end TYPE Grid2ReachMap

  interface ReachLS_decomp
     module procedure ReachLS_decompReal
     module procedure ReachLS_decompInt 
  end interface

  interface ReachLS_write_io
     module procedure ReachLS_wReal
     module procedure ReachLS_wReal2
     module procedure ReachLS_wInt
     module procedure ReachLS_wInt2
  end interface

  interface gBcastValue
     module procedure gbcastReal
     module procedure gbcastInt 
     module procedure gbcastReal2
  end interface

  interface updateLinkV
     module procedure updateLinkV8
     module procedure updateLinkV4
  end interface
  


  include "mpif.h"

  integer,allocatable,dimension(:) :: sDataRec  ! sending data size
  integer,allocatable,dimension(:) :: rDataRec  ! receiving data size
  integer,allocatable,dimension(:) :: linkls_s  ! receiving data size
  integer,allocatable,dimension(:) :: linkls_e  ! receiving data size
  integer,allocatable,dimension(:) :: ToInd  ! size of toInd

  integer ::  numprocs
  integer, allocatable, dimension(:) :: LLINKIDINDX, aLinksl
  integer :: LLINKLEN, gNlinksl, tmpnlinksl, l_nlinksl, max_nlinkSL

  contains


  subroutine updateLinkV8(LinkV, outV)
     implicit none
     real, dimension(:) :: outV 
     real*8, dimension(:) :: LinkV
     real*8, dimension(gNlinksl) :: gLinkV,gLinkV_r
     real, dimension(gNlinksl) :: gLinkV_r4
     integer :: ierr, i, tag
     gLinkV = 0.0
     gLinkV_r = 0.0
     do i = 1, LLINKLEN 
         gLinkV(LLINKIDINDX(i)) = LinkV(i)
     end do

     if(my_id .ne. IO_id) then
          tag = 102
          call mpi_send(gLinkV,gnlinksl,MPI_DOUBLE, IO_id,     &
                tag,MPI_COMM_WORLD,ierr)
      else
          gLinkV_r = gLinkV 
          do i = 0, numprocs - 1
            if(i .ne. IO_id) then
               tag = 102
               call mpi_recv(gLinkV,gnlinksl,&
                   MPI_DOUBLE,i,tag,MPI_COMM_WORLD,mpp_status,ierr)
               gLinkV_r = gLinkV_r + gLinkV
            end if
          end do
      end if
      gLinkV_r4 = gLinkV_r

      call ReachLS_decompReal(gLinkV_r4,outV)      
  end subroutine updateLinkV8

  subroutine updateLinkV4(LinkV, outV)
     implicit none
     real, dimension(:) :: outV  
     real, dimension(:) :: LinkV
     real, dimension(gNlinksl) :: gLinkV,gLinkV_r
     real, dimension(gNlinksl) :: gLinkV_r4
     integer :: ierr, i, tag
     gLinkV = 0.0 
     gLinkV_r = 0.0
     do i = 1, LLINKLEN
         gLinkV(LLINKIDINDX(i)) = LinkV(i)
     end do

     if(my_id .ne. IO_id) then
          tag = 102
          call mpi_send(gLinkV,gnlinksl,MPI_REAL, IO_id,     &
                tag,MPI_COMM_WORLD,ierr)
      else
          gLinkV_r = gLinkV    
          do i = 0, numprocs - 1
            if(i .ne. IO_id) then
               tag = 102
               call mpi_recv(gLinkV,gnlinksl,&
                   MPI_REAL,i,tag,MPI_COMM_WORLD,mpp_status,ierr)
               gLinkV_r = gLinkV_r + gLinkV
            end if
          end do
      end if
      gLinkV_r4 = gLinkV_r
      call ReachLS_decompReal(gLinkV_r4,outV)
  end subroutine updateLinkV4

  subroutine gbcastReal(inV, outV)
     implicit none
     real, dimension(:) :: outV
     real, dimension(:) :: inV  
     integer :: ierr
     call ReachLS_write_io(inV,outV)
     call mpi_bcast(outV(1:gnlinksl),gnlinksl,MPI_REAL,   &
            IO_id,MPI_COMM_WORLD,ierr) 
  end subroutine gbcastReal

  subroutine gbcastReal2(index,size1,inV, insize, outV)
     implicit none
     integer :: size1, insize
     integer,dimension(:) :: index
     real, dimension(:) :: outV
     real, dimension(:) :: inV  
     real, dimension(max_nlinkSL) :: tmpV  
     integer :: ierr, k, i, m, j, bsize

     outV = 0
     do i = 0, numprocs -1
            bsize = linkls_e(i+1) - linkls_s(i+1) + 1
         if(linkls_e(i+1) .gt. 0) then
            if(my_id .eq. i) tmpV(1:bsize) = inV(1:bsize)
            call mpi_bcast(tmpV(1:bsize),bsize,MPI_REAL,   &
                i,MPI_COMM_WORLD,ierr) 

            do j = 1, size1
                do k = 1, bsize
                   if(index(j) .eq. (linkls_s(i+1) + k -1) ) then
                      outV(j) = tmpV(k)
                      goto  100
                   endif
                end do
 100            continue
            end do

         endif
     end do

  end subroutine gbcastReal2


  subroutine gbcastInt(inV, outV)
     implicit none
     integer, dimension(:) :: outV
     integer, dimension(:) :: inV  
     integer :: ierr
     call ReachLS_write_io(inV,outV)
     call mpi_bcast(outV(1:gnlinksl),gnlinksl,MPI_INTEGER,   &
            IO_id,MPI_COMM_WORLD,ierr) 
  end subroutine gbcastInt 


  subroutine getLocalIndx(glinksl,LINKID, LLINKID)
       implicit none
       integer, dimension(:) :: LINKID, LLINKID
       integer :: i,k, glinksl, ierr
       integer :: gLinkId(glinksl)
       LLINKLEN = size(LLINKID,1)
       allocate(LLINKIDINDX(LLINKLEN))
       LLINKIDINDX = 0
       gNlinksl = glinksl

       call ReachLS_write_io(LINKID,gLinkId)

       call mpi_bcast(gLinkId(1:glinksl),glinksl,MPI_INTEGER,   &
            IO_id,MPI_COMM_WORLD,ierr)

       do i = 1, LLINKLEN
           do k = 1, glinksl
               if(LLINKID(i) .eq. gLinkId(k)) then
                    LLINKIDINDX(i) = k
                    goto 1001
               endif
           end do
1001       continue
       end do 
       
       call mpp_land_sync()
  end subroutine getLocalIndx

  subroutine ReachLS_ini(glinksl,nlinksl,linklsS, linklsE)
     implicit none
     integer, intent(in) :: glinksl
     integer, intent(out) :: nlinksl, linklsS, linklsE
     integer :: i, ii, ierr

! get my_id and numprocs 
     call MPI_COMM_RANK( MPI_COMM_WORLD, my_id, ierr )
     call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )

     nlinksl = glinksl / numprocs 
     allocate(linkls_s(numprocs))
     allocate(linkls_e(numprocs))
     allocate(aLinksl(numprocs))
     allocate(ToInd(numprocs))
     
     ToInd = -1

     linkls_s(1) = 1
     linkls_e(1) = nlinksl
     aLinksl = nlinksl

     do i = 2, mod(glinksl, numprocs)+1
         aLinksl(i) = aLinksl(i) + 1
     end do
     do i = 2, numprocs
        linkls_s(i) = linkls_e(i-1)+1
        linkls_e(i) = linkls_s(i) + aLinksl(i)-1
     end do

     nlinksl = aLinksl(my_id+1) 
     
     linklsS = linkls_s(my_id+1)
     linklsE = linkls_e(my_id+1)
     tmpnlinksl = aLinksl(my_id+1) 
     l_nlinksl = nlinksl

     max_nlinksl = l_nlinksl
     call mpp_land_max_int1(max_nlinksl)
     
  end subroutine ReachLS_ini
  
  subroutine MapGrid2ReachIni(in2d)
     implicit none
     integer, intent(in),dimension(:,:) :: in2d 
     integer :: ix, jx, i,j,n,ntotal, ierr
     integer, dimension(numprocs) :: tmpS

     allocate(sDataRec(numprocs))
     allocate(rDataRec(numprocs))
     
     ntotal = 0
     sDataRec = 0
     rDataRec = 0
     ix = size(in2d,1)
     jx = size(in2d,2)
     do j = 1, jx
        do i = 1, ix
           if(in2d(i,j) .gt. 0) then
              do n = 1, numprocs
                  if((in2d(i,j) .ge. linkls_s(n)) .and. (in2d(i,j) .le. linkls_e(n)) ) then
                              sDataRec(n) = sDataRec(n) + 1
                  endif
              end do
           endif
        enddo
     enddo   

     do n = 1, numprocs
         if(my_id .eq. n-1) then
             tmpS = sDataRec
         endif
         call mpi_bcast(tmpS,numprocs,MPI_INTEGER,   &
            n-1,MPI_COMM_WORLD,ierr)
         rDataRec(n) = tmpS(n)
     enddo
    
  end subroutine MapGrid2ReachIni


  subroutine ReachLS_decompReal(inV,outV)
      implicit none
      real,INTENT(in),dimension(:) :: inV
      real,INTENT(out),dimension(:) :: outV
      integer ::  i, ierr, tag
      tag = 11
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(1:(linkls_e(i)-linkls_s(i)+1) ) = inV(linkls_s(i):linkls_e(i))
                endif
            else
                if(aLinksl(i) .gt. 0) then
                    call mpi_send(inV(linkls_s(i):linkls_e(i)), &
                        aLinksl(i), &
                        MPI_REAL, i-1 ,tag,MPI_COMM_WORLD,ierr)
                endif
            endif
         end do
      else
         if(aLinksl(my_id+1) .gt. 0) then
             call mpi_recv(outV(1:(linkls_e(my_id+1)-linkls_s(my_id+1)+1) ), &
              aLinksl(my_id+1),                                        &
              MPI_REAL, io_id,tag,MPI_COMM_WORLD,mpp_status,ierr)
         endif
      endif
      call mpp_land_sync()
  END subroutine ReachLS_decompReal

  subroutine ReachLS_decompInt(inV,outV)
      implicit none
      integer,INTENT(in),dimension(:) :: inV
      integer,INTENT(out),dimension(:) :: outV
      integer ::  i, ierr, tag
      tag = 11
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                    outV(1:linkls_e(i)-linkls_s(i)+1) = inV(linkls_s(i):linkls_e(i))
                endif
            else
               if(aLinksl(i) .gt. 0) then
                  call mpi_send(inV(linkls_s(i):linkls_e(i)), &
                      aLinksl(i),                &
                      MPI_INTEGER, i-1,tag,MPI_COMM_WORLD,ierr)
               endif
            endif
         end do
      else
          if(aLinksl(my_id+1) .gt. 0) then
               call mpi_recv(outV(1:linkls_e(my_id+1)-linkls_s(my_id+1)), &
                    alinksl(my_id+1),                           &
                    MPI_INTEGER, io_id,tag,MPI_COMM_WORLD,mpp_status,ierr)
          endif
      endif

      call mpp_land_sync()

  END subroutine ReachLS_decompInt

  subroutine ReachLS_wReal(inV,outV)
      implicit none
      real,INTENT(in),dimension(:) :: inV
      real,INTENT(out),dimension(:) :: outV
      integer :: i, ierr, tag, ss  , mm
      outV = 0
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            tag = 12
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(linkls_s(i):linkls_e(i)) = inV(1:linkls_e(i)-linkls_s(i)+1)
                endif
            else
                if(aLinksl(i) .gt. 0) then

                    call mpi_recv(outV(linkls_s(i):linkls_e(i)), &
                         aLinksl(i),                            &
                         MPI_REAL,i-1,tag,MPI_COMM_WORLD,mpp_status,ierr)
                endif
            endif
         end do
      else
          if(aLinksl(my_id+1) .gt. 0) then
               tag = 12
               ss = size(inv,1)
               call mpi_send(inV(1:aLinksl(my_id+1) ), &
                      aLinksl(my_id+1),                      &
                      MPI_REAL,io_id,tag,MPI_COMM_WORLD,ierr)
          endif
      endif
      call mpp_land_sync()
  END subroutine ReachLS_wReal

  subroutine ReachLS_wInt(inV,outV)
      implicit none
      integer,INTENT(in),dimension(:) :: inV
      integer,INTENT(out),dimension(:) :: outV
      integer :: i, ierr, tag
      outV = 0
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(linkls_s(i):linkls_e(i)) = inV(1:linkls_e(i)-linkls_s(i)+1)
                endif
            else
               if(aLinksl(i) .gt. 0) then
                  tag = 12
                  call mpi_recv(outV(linkls_s(i):linkls_e(i)), &
                       aLinksl(i),                             &
                       MPI_INTEGER,i-1,tag,MPI_COMM_WORLD,mpp_status,ierr)
               endif
            endif
         end do
      else
           if(aLinksl(my_id+1) .gt. 0) then
                tag = 12
                call mpi_send(inV(1:aLinksl(my_id+1) ), &
                      aLinksl(my_id+1),                      &
                      MPI_INTEGER,io_id,tag,MPI_COMM_WORLD,ierr)
           endif
      endif
      call mpp_land_sync()
  END subroutine ReachLS_wInt 


  subroutine ReachLS_wInt2(inV,outV,len,glen)
      implicit none
      integer  :: len, glen
      integer,INTENT(in),dimension(len) :: inV
      integer,INTENT(out),dimension(glen) :: outV
      integer :: i, ierr, tag
      outV = 0
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(linkls_s(i):linkls_e(i)) = inV(1:linkls_e(i)-linkls_s(i)+1)
                endif
            else
               if(aLinksl(i) .gt. 0) then
                  tag = 12
                  call mpi_recv(outV(linkls_s(i):linkls_e(i)), &
                       aLinksl(i),                             &
                       MPI_INTEGER,i-1,tag,MPI_COMM_WORLD,mpp_status,ierr)
               endif
            endif
         end do
      else
           if(aLinksl(my_id+1) .gt. 0) then
                tag = 12
                call mpi_send(inV(1:aLinksl(my_id+1) ), &
                      aLinksl(my_id+1),                      &
                      MPI_INTEGER,io_id,tag,MPI_COMM_WORLD,ierr)
           endif
      endif  
      call mpp_land_sync()
  END subroutine ReachLS_wInt2

  subroutine ReachLS_wReal2(inV,outV,len,glen)
      implicit none
      integer :: len, glen
      real,INTENT(in),dimension(len) :: inV
      real,INTENT(out),dimension(glen) :: outV
      integer :: i, ierr, tag
      outV = 0
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(linkls_s(i):linkls_e(i)) = inV(1:linkls_e(i)-linkls_s(i)+1)
                endif
            else
                if(aLinksl(i) .gt. 0) then
                    tag = 12
                    call mpi_recv(outV(linkls_s(i):linkls_e(i)), &
                         aLinksl(i),                            &
                         MPI_REAL,i-1,tag,MPI_COMM_WORLD,mpp_status,ierr)
                endif
            endif
         end do
      else
          if(aLinksl(my_id+1) .gt. 0) then
               tag = 12
               call mpi_send(inV(1:aLinksl(my_id+1) ), &
                      aLinksl(my_id+1),                      &
                      MPI_REAL,io_id,tag,MPI_COMM_WORLD,ierr)
          endif
      endif
      call mpp_land_sync()   
  END subroutine ReachLS_wReal2

  subroutine getFromInd(linkid,to,ind,indLen)
      integer,dimension(:) :: linkid, to
      integer, allocatable, dimension(:) ::ind
      integer :: k, m, kk, mm,indLen
      integer, dimension(gnlinksl) :: glinkid   
      call ReachLS_write_io(linkid,glinkid)
      mm = size(to,1)
      kk = 0
      do k = 1, gnlinksl
          do m = 1, mm
             if(glinkid(k) .eq. to(m) ) then
                 kk = kk +1
                 goto 2001
             endif
          end do
2001      continue
      end do
      allocate(ind(kk))
      kk = 0
      do k = 1, gnlinksl
          do m = 1, mm
             if(glinkid(k) .eq. to(m) ) then
                 kk = kk +1
                 ind(kk) = glinkid(k)
                 goto 2002
             endif
          end do
2002      continue
      end do
      indLen = kk      

  end subroutine getFromInd

  subroutine getToInd(from,to,ind,indLen,gToNodeOut)
      integer,dimension(:) :: from, to
      integer, allocatable, dimension(:) ::ind, gToNodeOut
      integer :: k, m, kk, mm,indLen, i, ierr
      integer, dimension(gnlinksl) :: gto

      call gBcastValue(to,gto)

!      mm = size(from,1)
       mm = l_nlinksl

      kk = 0 
      do k = 1, gnlinksl
          do m = 1, mm
             if(gto(k) .eq. from(m) ) then
                 kk = kk +1
             endif
          end do
      end do 

      allocate(ind(kk))
      allocate(gToNodeOut(kk))
      indLen = kk

      kk = 0 
      do k = 1, gnlinksl
          do m = 1, mm
             if(gto(k) .eq. from(m) ) then
                 kk = kk +1
                 !yw ind(kk) = gto(k)
                 ind(kk) = k
                 gToNodeOut(kk) = gto(k)
             endif
          end do
      end do 
      ToInd(my_id+1) = kk
      do i = 0, numprocs - 1
         call mpi_bcast(ToInd(i+1),1,MPI_INTEGER,   &
            i,MPI_COMM_WORLD,ierr)
      end do

  end subroutine getToInd

END MODULE MODULE_mpp_ReachLS


