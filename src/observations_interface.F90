module observations_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: observations_interface
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

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: observations

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! observations.f90

  ! DESCRIPTION:

  ! This is the driver routine for the preparation of all observation
  ! types.

  !-----------------------------------------------------------------------

  subroutine observations()

    !=====================================================================


    !=====================================================================
    
  end subroutine observations

  !=======================================================================

  ! SUBROUTINE:

  ! sonde.f90

  ! DESCRIPTION:

  ! This is the driver routine for the preparation of all observations
  ! collected from sondes; currently the following platforms are
  ! supported:

  ! + American Oceanographic and Meteorological Laboratory (AOML)
  !   Hurricane Research Division (HRD) TEMP-DROP sondes.

  !-----------------------------------------------------------------------

  subroutine sonde()

    !=====================================================================


    !=====================================================================

  end subroutine sonde
  
  !=======================================================================

end module observations_interface
