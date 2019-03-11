! Purpose :
!   Change date of Netcdf file (WRF). Make test data.
!   Only work for file having one time data
! Usage:
!   chg_nc_date edite-file new-time(YYYY-MM-DD_HH:MM:SS)
!   Note: edite-file will be updated
! Author: zeng
! Date: 2013/15
program chg_nc_date

  implicit none

  include 'netcdf.inc'
  character(len=256)               :: file, edit_file 
  integer                          :: rcode,cdfid,id_var
  character(len=256)               :: name,value
!
  integer                          :: i,ivtype,ndims,dimids(10),dims(4),id_time,natts
  integer                          :: istart_t(2),iend_t(2)
  character(len=80)                :: times(999), varnam
!
  character(len=100)               :: prg_name
  character(len=19)                :: new_time
  integer                          :: num_arg

  num_arg = nargs()
  if (num_arg .lt. 3) then
     call getarg(0, prg_name)
     write(*,'(A,1X,A,1X,A)') 'Usage: ',trim(prg_name),'edit-file new-time(YYYY-MM-DD_HH:MM:SS)'
     call exit(9)
  end if
  call getarg(1, edit_file)
  call getarg(2, new_time)
  write(*,'(2A)') 'New time: ',new_time


  rcode = nf_open(trim(edit_file), NF_WRITE, cdfid )
  if( rcode /= 0 ) call exit(9)

  name = 'SIMULATION_START_DATE'
  rcode = nf_get_att_text( cdfid, NF_GLOBAL, name, value )
  if( rcode /= 0 ) call exit(9)

  call null2sp(value)
  write(6,'(2A)')' start_date: ',trim(value)

  rcode = nf_inq_varid( cdfid, 'Times', id_var )
  if( rcode /= 0 ) call exit(9)
  id_time = ncvid( cdfid, 'Times', rcode )
  if( rcode /= 0 ) call exit(9)

  rcode = nf_inq_var( cdfid, id_time, varnam, ivtype, ndims, dimids, natts )
  if( rcode /= 0 ) call exit(9)
  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), dims(i) )
    if( rcode /= 0 ) call exit(9)
  enddo

  if(dims(2).ne.1) call exit(9)
  do i=1,dims(2)
    istart_t(1) = 1
    istart_t(2) = i
    iend_t(1)   = dims(1)
    iend_t(2)   = 1
    rcode = nf_get_vara_text( cdfid, id_time, istart_t, iend_t, times(i) )
    if( rcode /= 0 ) call exit(9)
    call null2sp(times(i))
    write(*,'(2A)') 'Times: ', trim(times(i))
  enddo

  rcode = nf_put_vara_text( cdfid, id_time, istart_t, iend_t, new_time )
  if( rcode /= 0 ) call exit(2)

  rcode = nf_redef( cdfid )
  if( rcode /= 0 ) call exit(1)

!  rcode = nf_put_vara_text( cdfid, id_time, istart_t, iend_t, new_time )
!  if( rcode /= 0 ) call exit(2)

  times = new_time
  rcode = nf_put_att_text( cdfid, NF_GLOBAL, name, len_trim(times(1)), trim(times(1)) )
  if( rcode /= 0 ) call exit(3)

  name = 'START_DATE'
  rcode = nf_put_att_text( cdfid, NF_GLOBAL, name, len_trim(times(1)), trim(times(1)) )
  if( rcode /= 0 ) call exit(4)


  rcode = nf_enddef( cdfid )
  if( rcode /= 0 ) call exit(9)
  write(6,'(2A)') ' chg date  =  ',trim(times(1))
!
  write(6,*) 'SUCCESS COMPLETE'
!
end program chg_nc_date
!
! Convert null character to space
! Argument:
!   ch : character, input/output
subroutine null2sp(ch)
  character(len=*) :: ch
  integer :: i, j, l
  l=len(ch)
  i = 1
! Searching first null character
  do while (i .le. l)
!     if (ch(i:i) .eq. '\0') exit
     if (ch(i:i) .eq. char(0)) exit
     i = i + 1
  end do
! Replacing null character to space (from first null character to end of string)
  do while (i .le. l)
     ch(i:i) = ' '
     i = i + 1
  end do
end subroutine null2sp
