module sonde_tempdrop_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: sonde_tempdrop_interface
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

  use fileio_interface
  use kinds_interface
  use namelist_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: sonde_tempdrop
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! sonde_tempdrop.f90

  ! DESCRIPTION:

  ! This is the driver routine for the decoding and formatting of the
  ! National Oceanic and Atmospheric Administration (NOAA) Atlantic
  ! Oceanographic and Meteorological Laboratory (AOML) Hurricane
  ! Research Division (HRD) TEMPDROP sondes and subqeuently preparing
  ! a Binary Universal Formatted (BUFR) file.

  !-----------------------------------------------------------------------

  subroutine sonde_tempdrop()

    ! Define variables computed within routine

    type(sonde_struct)                                                  :: sonde

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(sonde)



    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(sonde)
    
    !=====================================================================

  end subroutine sonde_tempdrop

  !=======================================================================

end module sonde_tempdrop_interface
