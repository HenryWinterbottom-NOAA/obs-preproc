module recon_vdm_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: recon_vdm_interface
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
  use fileio_interface
  use grid_methods_interface
  use kinds_interface
  use math_methods_interface
  use meteo_methods_interface
  use namelist_interface
  use time_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: recon_vdm
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! obs_locations.f90

  ! DESCRIPTION:

  ! This subroutine estimates the flight-level observation locations
  ! using the fix location and the bearing (heading) and distance
  ! relative to the respective fix location.

  ! INPUT VARIABLES:

  ! * vdm; a FORTRAN vdm_struct variable.

  ! OUTPUT VARIABLES:

  ! * vdm; a FORTRAN vdm_struct variable with latitude and longitude
  !   estimates computed from the flight-level observation attributes.

  !-----------------------------------------------------------------------

  subroutine obs_locations(vdm)

    ! Define variables passed to routine

    type(vdm_struct)                                                    :: vdm

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid

    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Loop through local variable

    do i = 1, vdm%nvdm

       ! Loop through local variable
       
       do j = 1, vdm%nobs
       
          ! Define local variables

          grid%gclat = vdm%fix_lat(i)
          grid%gclon = vdm%fix_lon(i)

          ! Check local variable and proceed accordingly

          if((vdm%obs_head(i,j) .ne. spval) .and. (vdm%obs_dist(i,j) .ne.  &
               & spval)) then

             ! Define local variables

             grid%gchead = vdm%obs_head(i,j) - 90.0
             grid%gcdist = vdm%obs_dist(i,j)

             ! Compute local variables

             call grid_methods_gcgeo(grid)

             ! Define local variables

             vdm%obs_lat(i,j) = grid%gclat
             vdm%obs_lon(i,j) = grid%gclon

          end if ! if((vdm%obs_head(i,j) .ne. spval)
                 ! .and. (vdm%obs_dist(i,j) .ne. spval))

       end do ! do j = 1, vdm%nobs

    end do ! do i = 1, vdm%nvdm

    !=====================================================================

  end subroutine obs_locations
  
  !=======================================================================
  
  ! SUBROUTINE:

  ! recon_vdm.f90

  ! DESCRIPTION:

  !

  !-----------------------------------------------------------------------

  subroutine recon_vdm()

    ! Define variables computed within routine

    type(vdm_struct)                                                    :: vdm

    ! Define counting variables

    integer                                                             :: i,j 
    
    !=====================================================================

    ! Define local variables

    call fileio_interface_read(recon_filelist,vdm)
    
    ! Compute local variables

    call obs_locations(vdm)

    do i = 1, vdm%nvdm
       do j = 1, vdm%nobs
          print*, vdm%obs_time(i,j), vdm%obs_lat(i,j), vdm%obs_lon(i,j), vdm%fix_lat(i), vdm%fix_lon(i)
       end do
    end do
       
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vdm)
    
    !=====================================================================
    
  end subroutine recon_vdm
    
  !=======================================================================
  
end module recon_vdm_interface
