! This subrouting includs the data structure and tools used for NHDPlus network mapping.
module module_UDMAP

use module_namelist, only: nlst_rt
#ifdef MPP_LAND
use module_mpp_land, only: my_id, local_startx_rt, local_starty_rt,  &
         local_endx_rt,local_endy_rt, left_id, right_id, down_id, up_id, mpp_collect_1d_int_mem, &
         IO_id 
use module_mpp_land, only: mpp_land_bcast_int, mpp_land_bcast_real8_1d, mpp_land_bcast_int1

use module_mpp_land, only: sum_int1d, global_rt_nx, global_rt_ny, write_IO_rt_int

use MODULE_mpp_ReachLS, only : updatelinkv, ReachLS_write_io, com_write1dInt, com_decomp1dInt

#endif

implicit none

#include <netcdf.inc>

type userDefineMapping
    integer, allocatable, dimension(:) :: grid_i, grid_j
    real, allocatable, dimension(:) :: weight, nodeArea, cellArea
    integer :: ngrids, nlands
    integer :: myid
!   for bucket model definition
    real, allocatable, dimension(:) :: cellWeight
    integer, allocatable, dimension(:) :: cell_i, cell_j
    integer :: ncell
end type userDefineMapping

TYPE ( userDefineMapping ), allocatable, DIMENSION (:) :: LUDRSL

integer, allocatable, dimension(:) :: bufi,bufj,bufsize, bufid
real*8 , allocatable, dimension(:) :: bufw
integer :: LNUMRSL  ! number of local links
integer :: ter_rt_flag
real*8, allocatable, dimension(:) :: basns_area
integer :: gnpid

contains
    subroutine UDMP_ini(nlinksl,ixrt,jxrt,rtmask, OVRTSWCRT, SUBRTSWCRT,cell_area)
!This is the driver for user defined mapping file funciton application.
        integer :: ixrt, jxrt, OVRTSWCRT, SUBRTSWCRT, nlinksl
        integer, intent(in), dimension(ixrt,jxrt):: rtmask
        integer :: npid    !local variable.
        real,dimension(:,:) :: cell_area
        ter_rt_flag = 0
        if(OVRTSWCRT .eq. 1 .or. SUBRTSWCRT .eq. 1) then
            ter_rt_flag = 1
        endif
        call readUDMP(npid,nlinksl)
        call UDMP2LOCAL(npid,ixrt,jxrt,rtmask, ter_rt_flag)
        call getUDMP_area(cell_area)
    end subroutine UDMP_ini

    subroutine readUDMP(npid, nlinksl)
        implicit none
        integer :: i,j,Ndata, did, Npid, nlinksl, k, m
        integer,allocatable,dimension(:) :: linkid ,bufidflag, bufid_tmp
        integer :: ix_bufid
        did = 1
        call get_dimension(trim(nlst_rt(did)%UDMAP_FILE), ndata, npid) 
        gnpid = npid

        allocate(bufi(ndata))
        allocate(bufj(ndata))
        allocate(bufw(ndata))
        allocate(bufsize(npid))
        allocate(bufid(npid))
        allocate(linkid(nlinksl))

!       read the data
#ifdef MPP_LAND
       if(my_id .eq. IO_id) then
#endif

          call get1d_real8(trim(nlst_rt(did)%UDMAP_FILE),"regridweight",bufw)
          call get1d_int(trim(nlst_rt(did)%UDMAP_FILE),"polyid",bufid)
          call get1d_int(trim(nlst_rt(did)%UDMAP_FILE),"overlaps",bufsize)
!          call get1d_int(trim(nlst_rt(did)%UDMAP_FILE),"IDmask",idmask)
          call get1d_int(trim(nlst_rt(did)%UDMAP_FILE),"i_index",bufi)
          call get1d_int(trim(nlst_rt(did)%UDMAP_FILE),"j_index",bufj)
!      check  polyid and linkid
          call get1d_int(trim(nlst_rt(did)%route_link_f),"link",linkid)
          write(6,*) "nlinksl = ", nlinksl, "npid = ", npid
#ifdef MPP_LAND
      endif
#else 
      call hydro_stop("Fatal error: sequential run for NHDPlus not defined.")
#endif

#ifdef MPP_LAND
       call mpp_land_bcast_int(nlinksl,linkid)
       call com_decomp1dInt(bufid,npid,bufid_tmp,ix_bufid)
#endif
       if(ix_bufid .gt. 0) then
          allocate(bufidflag(ix_bufid))
          bufidflag = -999
      endif

       do i = 1, ix_bufid
          do j = 1, nlinksl
               if(bufid_tmp(i) .eq. linkid(j)) then
                  bufidflag(i) = bufid_tmp(i)
                  goto 102
               endif
          end do
102       continue
       end do

#ifdef MPP_LAND
      call com_write1dInt(bufidflag,ix_bufid,bufid,npid)
#endif

      if(ix_bufid .gt. 0) then
          deallocate(bufidflag)
          deallocate(bufid_tmp)
      endif


!   check bufsize
!   m = 0
!   do j = 1, npid
!      m = m + bufsize(j)
!   enddo
!  write(6,*) "m = ", m, "ndata = ", ndata
     
     goto 101
 
!     k = 1  
!     j = 1
!     do i = 1, ndata-1
!        if(idmask(i) .ne. idmask(i+1) ) then 
!           bufsize(j) = k
!           j = j + 1
!           k = 1 
!        else
!!           k = k + 1
!        end if
!     end do      
!     bufsize(j) = k

!     write(6,*) "j = ", j, "npid = ", npid

101   continue

          deallocate(linkid)

#ifdef MPP_LAND
       call mpp_land_bcast_int(npid,bufid)
       call mpp_land_bcast_int(npid,bufsize)
       call mpp_land_bcast_real8_1d(bufw)
       call mpp_land_bcast_int(ndata,bufi)
       call mpp_land_bcast_int(ndata,bufj)
#endif

!      if(my_id .eq. io_id) write(28) bufid 
!      call hydro_finish()

    end subroutine readUDMP

    subroutine UDMP2LOCAL(npid,ix,jx,rtmask, ter_rt_flag)
        implicit none
        integer :: i,j,k, nlands, ngrids, ix,jx, starti,startj, endi,endj, ii,jj, npid, kk
        integer, intent(in), dimension(ix,jx) :: rtmask
        integer, dimension(npid) :: lndflag,gridflag , tmpgridflag
        integer :: ter_rt_flag, m, c


!   find ngrids is 0 so that we need to mapping from subsurface runoff.
#ifdef MPP_LAND
        if(left_id .ge. 0) then
           starti = local_startx_rt  + 1
        else
           starti = local_startx_rt 
        endif
        if(down_id .ge. 0) then
           startj = local_starty_rt  + 1
        else
           startj = local_starty_rt 
        endif
        if(right_id .ge. 0) then
           endi = local_startx_rt + ix -2
        else
           endi = local_startx_rt + ix -1
        endif
        if(up_id .ge. 0) then
           endj = local_starty_rt + jx -2
        else
           endj = local_starty_rt + jx -1
        endif
#else
        starti = 1
        startj = 1
        endi = ix
        endj = jx
#endif
        gridflag = 0
        lndflag = 0

        i = 1
        do k = 1, npid   
           do j = 1, bufsize(k) 
              if(bufid(k) .ge. 0) then
                if(bufi(i) .ge. starti .and. bufj(i) .ge. startj .and. &
                    bufi(i) .le. endi   .and. bufj(i) .le. endj) then
                    lndflag(k) = lndflag(k) + 1
                    if(ter_rt_flag .eq. 1) then
#ifdef MPP_LAND
                        if(rtmask(bufi(i)-local_startx_rt+1,bufj(i)-local_starty_rt+1) .ge. 0) then
#else
                        if(rtmask(bufi(i),bufj(i)) .ge. 0) then
#endif
                             gridflag(k) = gridflag(k) + 1 
                        endif
                    endif
                 endif
              endif
              i = i + 1
           end do
        end do

!        call sum_int1d(lndflag,npid)
!        call sum_int1d(gridflag,npid)
!        if(my_id .eq. io_id) then
!            write(60,*) lndflag
!            write(61,*) gridflag
!        endif
!         write(my_id+70,*) "rtmask=",rtmask 
!        call hydro_finish()


! decide how many mapping land grids on current domain
        tmpgridflag = gridflag
#ifdef MPP_LAND
        call mpp_collect_1d_int_mem(npid,tmpgridflag)
#endif

! decide how many user defined links on current domain
        LNUMRSL = 0 
        do k = 1, npid   
           if(lndflag(k) .gt. 0) LNUMRSL = LNUMRSL + 1
        enddo
        if(LNUMRSL .gt. 0) then 
               allocate(LUDRSL(LNUMRSL))
               allocate( basns_area(LNUMRSL) )
        else
               write(6,*) "Warning: no routing links found."
               call cleanBuf()
               return
        endif

!       write(6,*) "my_id, starti, endi, startj,endj",starti, endi, startj,endj
!       write(6,*) " LNUMRSL = ", LNUMRSL
!       write(my_id+60,*) "lndflag=",lndflag
!       write(my_id+60,*) "gridflag=",gridflag
!       write(my_id+60,*) "tmpgridflag=",tmpgridflag
!       call flush(my_id+60)
!       call hydro_finish()

        kk = 1
        do k = 1, npid   
           if( (bufid(k) .ge. 0) .and.  (lndflag(k) .gt. 0) ) then 
               LUDRSL(kk)%myid = bufid(k) 
               LUDRSL(kk)%nlands = -999
               LUDRSL(kk)%ngrids = -999
               if(tmpgridflag(k) .gt. 0) then
                   LUDRSL(kk)%ngrids = gridflag(k)
                   LUDRSL(kk)%nlands = -999
                   if(LUDRSL(kk)%ngrids .gt. 0) then
                      allocate( LUDRSL(kk)%weight(LUDRSL(kk)%ngrids))
                      allocate( LUDRSL(kk)%grid_i(LUDRSL(kk)%ngrids))
                      allocate( LUDRSL(kk)%grid_j(LUDRSL(kk)%ngrids))
                      allocate( LUDRSL(kk)%nodeArea(LUDRSL(kk)%ngrids))
                   endif
               else
                   LUDRSL(kk)%ngrids = -999
                   LUDRSL(kk)%nlands = lndflag(k)
                   allocate( LUDRSL(kk)%weight(LUDRSL(kk)%nlands))
                   allocate( LUDRSL(kk)%grid_i(LUDRSL(kk)%nlands))
                   allocate( LUDRSL(kk)%grid_j(LUDRSL(kk)%nlands))
                   allocate( LUDRSL(kk)%nodeArea(LUDRSL(kk)%nlands))
               endif
!  define bucket variables
                 LUDRSL(kk)%ncell = lndflag(k) 
                 allocate( LUDRSL(kk)%cellweight(LUDRSL(kk)%ncell))
                 allocate( LUDRSL(kk)%cell_i(LUDRSL(kk)%ncell))
                 allocate( LUDRSL(kk)%cell_j(LUDRSL(kk)%ncell))
                 allocate( LUDRSL(kk)%cellArea(LUDRSL(kk)%ncell))

               kk = kk + 1
           endif
        enddo

!        call sum_int1d(checkbuf,npid)
!        if(my_id .eq. io_id) then
!            write(60,*) tmpgridflag
!            write(71,*) checkbuf
!            call flush(71)
!            call flush(60)
!        endif
!        call hydro_finish()

! maping grid_i, grid_j and weight
        i = 1
        kk = 1
        do k = 1, npid   
           m  = 1
           c  = 1
           do j = 1, bufsize(k) 
               if( (bufid(k) .ge. 0) .and.  (lndflag(k) .gt. 0) ) then 
                   if(bufi(i) .ge. starti .and. bufj(i) .ge. startj .and. &
                      bufi(i) .le. endi   .and. bufj(i) .le. endj) then
                     if(LUDRSL(kk)%nlands .gt. 0) then 
#ifdef MPP_LAND
                          LUDRSL(kk)%grid_i(m) = bufi(i) - local_startx_rt+1
                          LUDRSL(kk)%grid_j(m) = bufj(i) - local_starty_rt+1
#else
                          LUDRSL(kk)%grid_i(m) = bufi(i) 
                          LUDRSL(kk)%grid_j(m) = bufj(i) 
#endif
                          LUDRSL(kk)%weight(m) = bufw(i) 
                          m  = m  + 1
                      else
#ifdef MPP_LAND
                          if(rtmask(bufi(i)-local_startx_rt+1,bufj(i)-local_starty_rt+1) .ge. 0) then
                             LUDRSL(kk)%grid_i(m) = bufi(i) - local_startx_rt+1
                             LUDRSL(kk)%grid_j(m) = bufj(i) - local_starty_rt+1
#else
                          if(rtmask(bufi(i),bufj(i)) .ge. 0) then
                             LUDRSL(kk)%grid_i(m) = bufi(i)
                             LUDRSL(kk)%grid_j(m) = bufj(i) 
#endif
                             LUDRSL(kk)%weight(m) = bufw(i) 
                             m  = m  + 1
                          endif
                      endif
!! begin define bucket variables
#ifdef MPP_LAND
                          LUDRSL(kk)%cell_i(c) = bufi(i) - local_startx_rt+1
                          LUDRSL(kk)%cell_j(c) = bufj(i) - local_starty_rt+1
#else
                          LUDRSL(kk)%cell_i(c) = bufi(i)
                          LUDRSL(kk)%cell_j(c) = bufj(i)
#endif
                          LUDRSL(kk)%cellWeight(c) = bufw(i)
                          c  = c  + 1
!! end define bucket variables 
                   endif
                endif
                i = i + 1
           end do
           if( (bufid(k) .ge. 0) .and.  (lndflag(k) .gt. 0) )  kk = kk + 1
        end do
         write(6,*) "LNUMRSL = ", LNUMRSL, "kk = ", kk - 1
         write(6,*) "check ndata = ", i - 1

!        do k = 1, LNUMRSL
!              write(6,*) " k,nlands,ngrids = ", k, LUDRSL(k)%nlands, LUDRSL(k)%ngrids
!        end do
!        call hydro_finish()

        call cleanBuf()
    end subroutine UDMP2LOCAL

    subroutine cleanBuf()
        deallocate(bufi,bufj,bufsize,bufw, bufid)
    end subroutine cleanBuf

     subroutine get_dimension(fileName, ndata,npid)
            implicit none
            character(len=*) fileName
            integer ncid , iret, ndata,npid, dimid
#ifdef MPP_LAND
            if(my_id .eq. IO_id) then
#endif
               iret = nf_open(fileName, NF_NOWRITE, ncid)
               if (iret /= 0) then
                  write(*,'("Problem opening mapping file: ''", A, "''")') &
                       trim(fileName)
                  call hydro_stop("get_dimension")
               endif
        
               iret = nf_inq_dimid(ncid, "polyid", dimid)
        
               if (iret /= 0) then
                  print*, "nf_inq_dimid:  polyid"
                  call hydro_stop("get_dimension")
               endif
           
               iret = nf_inq_dimlen(ncid, dimid, npid)
           
               iret = nf_inq_dimid(ncid, "data", dimid)
               if (iret /= 0) then
                          print*, "nf_inq_dimid:  data"
                          call hydro_stop("get_file_dimension")
               endif
        
               iret = nf_inq_dimlen(ncid, dimid, ndata)
               iret = nf_close(ncid)
#ifdef MPP_LAND
            endif
            call mpp_land_bcast_int1(ndata)
            call mpp_land_bcast_int1(npid)
#endif
            return
     end subroutine get_dimension

       subroutine get1d_real8(fileName,var_name,out_buff)
          implicit none
          integer :: ivar, iret,varid,ncid
          real*8 out_buff(:)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName

          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"failed to open the netcdf file: ",trim(fileName)
            call hydro_stop("fatal error! ")
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          if(ivar .ne. 0) then
               write(6,*) "Read Variable Error file: ",trim(fileName)
               write(6,*) "Read Error: could not find ",trim(var_name)
               call hydro_stop("fatal error! ")
          end if
          iret = nf_get_var_double(ncid, varid, out_buff)
          iret = nf_close(ncid)
      end subroutine get1d_real8

       subroutine get1d_int(fileName,var_name,out_buff)
          implicit none
          integer :: ivar, iret,varid,ncid
          integer out_buff(:)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName

          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"failed to open the netcdf file: ",trim(fileName)
            call hydro_stop("fatal error! ")
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          if(ivar .ne. 0) then
               write(6,*) "Read Variable Error file: ",trim(fileName)
               write(6,*) "Read Error: could not find ",trim(var_name)
               call hydro_stop("fatal error! ")
          end if
          iret = nf_get_var_int(ncid, varid, out_buff)
          iret = nf_close(ncid)
      end subroutine get1d_int

      subroutine getUDMP_area(cell_area)
         implicit none
         integer i,j,k, m
         real, dimension(:,:) :: cell_area
         do k  = 1, LNUMRSL
            if(LUDRSL(k)%nlands .gt. 0) then
                do m = 1, LUDRSL(k)%nlands
                    LUDRSL(k)%nodeArea(m) = cell_area(LUDRSL(k)%grid_i(m),LUDRSL(k)%grid_j(m)) 
                enddo
            else
                do m = 1, LUDRSL(k)%ngrids
                    LUDRSL(k)%nodeArea(m) = cell_area(LUDRSL(k)%grid_i(m),LUDRSL(k)%grid_j(m)) 
                enddo
            endif
                do m = 1, LUDRSL(k)%ncell
                    LUDRSL(k)%cellArea(m) = cell_area(LUDRSL(k)%cell_i(m),LUDRSL(k)%cell_j(m)) 
                enddo
           
            basns_area(k) = 0 
            do m = 1, LUDRSL(k)%ncell
                    basns_area(k) = basns_area(k) + &
                          cell_area(LUDRSL(k)%cell_i(m),LUDRSL(k)%cell_j(m)) * LUDRSL(k)%cellWeight(m) 
            enddo
            
         end do
      end subroutine getUDMP_area
    
      subroutine get_basn_area_nhd(inOut)
         implicit none
         real, dimension(:) :: inOut
         real, dimension(gnpid) :: buf
#ifdef MPP_LAND
         call updateLinkV(basns_area, inOut)
#else
         inOut = basns_area
#endif

!         call ReachLS_write_io(inOut,buf)
!         if(my_id .eq. io_id) then
!            write(90,*) buf
!            call flush(90)
!         endif
!         call hydro_finish()
      
      end subroutine get_basn_area_nhd


end module module_UDMAP
