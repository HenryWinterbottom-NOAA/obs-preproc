module bufr_obs_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: bufr_obs_interface
  ! Copyright (C) 2019 Henry R. Winterbottom

  ! Email: henry.winterbottom@noaa.gov

  ! This program is free software: you can redistribute it and/or
  ! modify it under the terms of the GNU General Public License as
  ! published by the Free Software Foundation, either version 3 of the
  ! License, or (at your option) any later version.

  ! This program is distributed in the hope that it will be useful,
  ! but WITHOUT ANY WARRANTY; without even the implied warranty of
  ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  ! General Public License for more details.

  ! You should have received a copy of the GNU General Public License
  ! along with this program.  If not, see
  ! <http://www.gnu.org/licenses/>.

  ! Review the README, within the top-level directory, which provides
  ! relevant instructions and (any) references cited by algorithms
  ! within this software suite.

  !=======================================================================

  ! Define associated modules and subroutines

  use namelist_interface
  use time_methods_interface
  use variable_interface
  
  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: bufr_obs_update

  ! Define local variables

  integer                                                               :: iret
  integer, parameter                                                    :: unit_in  = 10
  integer, parameter                                                    :: unit_out = 20
  integer, parameter                                                    :: unit_tbl = 30
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! bufr_obs_update.f90

  ! DESCRIPTION:

  !

  !-----------------------------------------------------------------------

  subroutine bufr_obs_update()

    ! Define variables computed within routine

    type(timeinfo_struct)                                               :: timeinfo
    
    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Define local variables

    call valid_times(timeinfo)
    call init_bufr(bufr_obs_filename(1))
    
    ! Loop through local variables

    do i = 1, size(bufr_obs_filename)
    
       ! Define local variables

       call readwrite_bufrobs(bufr_obs_filename(i),timeinfo)

    end do ! do i = 1, size(bufr_obs_filename)

    ! Define local variables

    call close_bufr()

    !=====================================================================

  end subroutine bufr_obs_update

  !=======================================================================

  ! SUBROUTINE:

  ! close_bufr.f90

  ! DESCRIPTION:

  ! This subroutine closes all open input and output files designated
  ! by the unit_in and unit_out local variables, respectively.

  !-----------------------------------------------------------------------

  subroutine close_bufr()

    !=====================================================================
    
    ! Define local variables

    call closbf(unit_out)
    close(unit_tbl)
    
    !=====================================================================

  end subroutine close_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! init_bufr.f90

  ! DESCRIPTION:

  ! This subroutine initializes the BUFR interfaces; this subroutine
  ! writes the BUFR table, parsed from an existing BUFR-formatted
  ! file, to an external file specified by the user; the external
  ! file, specified by the user, remains open unless the subroutine
  ! close_bufr.f90 is called.

  ! INPUT VARIABLES:
  
  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  !-----------------------------------------------------------------------

  subroutine init_bufr(filename)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & convert='big_endian')
    open(unit_tbl,file=trim(adjustl(datapath))//'bufr_tbl',                &
         & action='write')
    call openbf(unit_in,'IN',unit_in)
    call dxdump(unit_in,unit_tbl)
    close(unit_tbl)
    call closbf(unit_in)
    open(unit_tbl,file=trim(adjustl(datapath))//'bufr_tbl',action='read')
    open(unit_out,file=trim(adjustl(bufr_filepath)),action='write',form=   &
         & 'unformatted',convert='big_endian')
    call openbf(unit_out,'OUT',unit_tbl)
 
    !=====================================================================  

  end subroutine init_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! readwrite_bufrobs.f90

  ! DESCRIPTION:

  ! This subroutine parses the BUFR records from a user specified file
  ! and computes the time-stamp attributes; if the observation
  ! time-stamp attributes occur between the user specified threshold
  ! values (relative to the analysis time-stamp), the respective BUFR
  ! record is written to the external file defined by the unit_out
  ! local variable.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  ! * timeinfo; a FORTRAN timeinfo_struct variable containing the
  !   Julian day values corresponding to the observation wind
  !   specified by the user.

  !-----------------------------------------------------------------------

  subroutine readwrite_bufrobs(filename,timeinfo)

    ! Define variables passed to routine

    type(timeinfo_struct)                                               :: timeinfo
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=10)                                                   :: datestr
    character(len=8)                                                    :: subset
    real(r_double)                                                      :: bufrtype(6)   
    real(r_double)                                                      :: obs_jday
    integer                                                             :: dd
    integer                                                             :: hh    
    integer                                                             :: idate
    integer                                                             :: ibfmsg(16000/4)
    integer                                                             :: ireadmg
    integer                                                             :: ireadsb
    integer                                                             :: mm
    integer                                                             :: nn
    integer                                                             :: ss
    integer                                                             :: yyyy
    
    !=====================================================================
    
    ! Define local variables

    open(unit_in,file=trim(adjustl(filename)),form='unformatted',            &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)

    ! Loop through local variable

    msg_report: do while(ireadmg(unit_in,subset,idate) .eq. 0)

       ! Loop through local variable

       sb_report: do while(ireadsb(unit_in) .eq. 0)

          ! Define local variables

          call ufbint(unit_in,bufrtype,size(bufrtype),1,iret,                &
               & 'YEAR MNTH DAYS HOUR MINU SECO')
          yyyy = int(bufrtype(1))
          mm   = int(bufrtype(2))
          dd   = int(bufrtype(3))
          hh   = int(bufrtype(4))
          nn   = int(bufrtype(5))
          ss   = int(bufrtype(6))

          ! Compute local variables

          call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,obs_jday)

          ! Check local variable and proceed accordingly

          if((obs_jday .ge. timeinfo%minjday) .and. (obs_jday .le.           &
               & timeinfo%maxjday)) then 

             ! Define local variables

             call openmb(unit_out,subset,idate)
             call writsb(unit_out)
             call closmg(unit_out)
          
          end if ! if((obs_jday .ge. timeinfo%minjday) .and. (obs_jday
                 ! .le. timeinfo%maxjday))
             
       end do sb_report ! do while(ireadsb(unit_in) .eq. 0)

    end do msg_report ! do while(ireadmg(unit_in,subset,idate) .eq. 0)

    ! Define local variables
    
    call closbf(unit_in)
    
    !=====================================================================

  end subroutine readwrite_bufrobs

  !=======================================================================

  ! SUBROUTINE:

  ! valid_times.f90

  ! DESCRIPTION:

  ! This subroutine computes the Julian day values corresponding to
  ! the observation window specified by the user.

  ! INPUT VARIABLES:

  ! * timeinfo; a FORTRAN timeinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * timeinfo; a FORTRAN timeinfo_struct variable containing the
  !   Julian day values corresponding to the observation wind
  !   specified by the user.

  !-----------------------------------------------------------------------

  subroutine valid_times(timeinfo)

    ! Define variables passed to routine

    type(timeinfo_struct)                                               :: timeinfo

    ! Define variables computed within routine

    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: mm
    integer                                                             :: nn
    integer                                                             :: ss
    integer                                                             :: yyyy

    !=====================================================================

    ! Define local variables

    call time_methods_date_attributes(bufr_obs_mindate,yyyy,mm,dd,hh,nn,   &
         & ss)
    call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,timeinfo%minjday)
    call time_methods_date_attributes(bufr_obs_maxdate,yyyy,mm,dd,hh,nn,   &
         & ss)
    call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,timeinfo%maxjday)

    !=====================================================================

  end subroutine valid_times
 
  !=======================================================================

end module bufr_obs_interface
