module forecast_model_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: forecast_model_interface
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
  public :: forecast_model_fv3

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! forecast_model_fv3.f90

  ! DESCRIPTION:

  !

  !-----------------------------------------------------------------------

  subroutine forecast_model_fv3()

    ! Define variables computed within routine

    type(tcv_struct),           dimension(:),               allocatable :: tcv

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(tcv_filename,tcv) 




    ! Deallocate memory for local variables

    if(allocated(tcv)) deallocate(tcv)
    
    !=====================================================================

  end subroutine forecast_model_fv3
  
  !=======================================================================
  
end module forecast_model_interface
