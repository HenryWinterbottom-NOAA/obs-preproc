module recon_tdr_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: recon_tdr_interface
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

  use bufrio_interface
  use namelist_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: recon_tdr
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! recon_tdr.f90

  ! DESCRIPTION:

  !

  !-----------------------------------------------------------------------

  subroutine recon_tdr()

    ! Define variables computed within routine

    type(tdr_struct)                                                    :: tdr

    !=====================================================================

    ! Define local variables

    call tdr_stmid(tdr)

    call write_tdr_status(tdr)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(tdr)

    !=====================================================================

  end subroutine recon_tdr

  !=======================================================================

  ! SUBROUTINE:

  ! tdr_stmid.f90

  ! DESCRIPTION:

  ! This subroutine determines the tropical cyclone (TC) identifier
  ! for which the tail-Doppler radar (TDR) observations are collected.

  ! INPUT VARIABLES:

  ! * tdr; a FORTRAN tdr_struct variable.

  ! OUTPUT VARIABLES:

  ! * tdr; a FORTRAN tdr_struct variable containing the number of TC
  !   event TDR observations within the BUFR-formatted file and the
  !   corresponding TC identifiers.

  !-----------------------------------------------------------------------

  subroutine tdr_stmid(tdr)

    ! Define variables passed to routine

    type(tdr_struct)                                                    :: tdr

    ! Define variables computed within routine

    type(bufr_struct)                                                   :: bufr
    type(bufrhdr_struct)                                                :: bufrhdr
    character(len=3)                                                    :: stmid
    real(r_double)                                                      :: hdr
    integer                                                             :: tcid
    
    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Define local variables

    equivalence(hdr,stmid)
    bufr%hdstr = 'STMID'
    call bufrio_interface_readhdr(recon_tdr_filepath,bufr,bufrhdr)
    tdr%nstmid = size(bufrhdr%hdr)
    call variable_interface_setup_struct(tdr)

    ! Loop through local variable

    do i = 1, size(bufrhdr%hdr)

       ! Define local variables

       hdr = bufrhdr%hdr(i)
       read(stmid,'(i)') tcid
       
       ! Check local variable and proceed accordingly

       if((tcid .ge. dble(100.0)) .and. (tcid .lt. dble(200.0))) then

          ! Define local variables

          write(tdr%stmid(i),500) int(tcid - dble(100.0))

       else if((tcid .ge. dble(200.0)) .and. (tcid .lt. dble(300.0)))      &
            & then

          ! Define local variables

          write(tdr%stmid(i),501) int(tcid - dble(200.0))

       else if(tcid .lt. dble(300.0)) then

          ! Define local variables

          write(tdr%stmid(i),502) int(tcid - dble(300.0))

       end if ! if((tcid .ge. dble(100.0)) .and. (tcid
              ! .lt. dble(200.0)))

    end do ! do i = 1, size(bufrhdr%hdr)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(bufrhdr)

    ! Define local variables

500 format(i2.2,'L')
501 format(i2.2,'E')
502 format(i2.2,'C')
    
    !=====================================================================
    
  end subroutine tdr_stmid

  !=======================================================================

  ! SUBROUTINE:

  ! write_tdr_status.f90

  ! DESCRIPTION:

  !

  !-----------------------------------------------------------------------

  subroutine write_tdr_status(tdr)

    ! Define variables passed to routine

    type(tdr_struct)                                                    :: tdr

    !=====================================================================

    ! Define local variables
    
    open(99,file=trim(adjustl(recon_tdrstatus_filepath)),form=             &
         & 'formatted')


    close(99)
500 format('NUMBER OF TDR EVENT: ',i3)
501 format('TDR STORM: ',a4)
    
    !=====================================================================

  end subroutine write_tdr_status

  !=======================================================================

end module recon_tdr_interface
