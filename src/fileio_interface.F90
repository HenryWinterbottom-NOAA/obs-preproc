module fileio_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: fileio_interface
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

  use kinds_interface
  use namelist_interface
  use netcdf
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: fileio_interface_read
  interface fileio_interface_read
     module procedure read_sonde_filenames
  end interface fileio_interface_read
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! read_sonde_filenames.f90

  ! DESCRIPTION:

  ! This subroutine reads into an array sonde filename pathes.

  ! INPUT VARIABLES:

  ! * sonde; a FORTRAN sonde_struct variable.

  ! OUTPUT VARIABLES:

  ! * sonde; a FORTRAN sonde_struct variable containing an array of
  !   sonde file pathes.
  
  !-----------------------------------------------------------------------

  subroutine read_sonde_filenames(sonde)

    ! Define variables passed to routine

    type(sonde_struct)                                                  :: sonde

    ! Define variables computed within routine

    character(len=500)                                                  :: dummy
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    sonde%nsondes = 0
    open(99,file=trim(adjustl(sonde_filelist)),form='formatted')
1000 read(99,*,end=1001) dummy
    sonde%nsondes = sonde%nsondes + 1
    goto 1000
1001 continue
    close(99)

    ! Define local variables

    call variable_interface_setup_struct(sonde)

    ! Define local variables

    open(99,file=trim(adjustl(sonde_filelist)),form='formatted')

    ! Loop through local variable

    do i = 1, sonde%nsondes

       ! Define local variables

       read(99,*) sonde%sonde_filename(i)
       if(debug) write(6,500) trim(adjustl(sonde%sonde_filename(i)))

    end do ! do i = 1, sonde%nsondes

    ! Define local variables

    close(99)
500 format('READ_SONDE_FILENAMES: Reading in file ',a,' to be processed.')

    !=====================================================================

  end subroutine read_sonde_filenames

  !=======================================================================

end module fileio_interface
