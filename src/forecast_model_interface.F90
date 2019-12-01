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
  use grid_methods_interface
  use kinds_interface
  use math_methods_interface
  use namelist_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: forecast_model_fv3
  interface observation_assignments
     module procedure fv3_observations
  end interface observation_assignments
  
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

    type(fcstmdl_struct),       dimension(:),               allocatable :: fcstmdl
    type(tcinfo_struct),        dimension(:),               allocatable :: tcinfo
    type(fv3_struct)                                                    :: fv3
    type(grid_struct)                                                   :: grid
    
    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(tcinfo_filename,tcinfo)
    call fileio_interface_read(fv3)
    grid%nx = fv3%nx
    grid%ny = fv3%ny
    call variable_interface_setup_struct(grid)
    
    ! Allocate memory for local variables

    if(.not. allocated(fcstmdl)) allocate(fcstmdl(size(tcinfo)))

    ! Define local variables

    fcstmdl(:)%nz = fv3%nz
    grid%lat      = fv3%lat
    grid%lon      = fv3%lon
    
    ! Loop through local variables

    do i = 1, size(tcinfo)

       ! Define local variables

       grid%gclat = tcinfo(i)%mdl_clat
       grid%gclon = tcinfo(i)%mdl_clon

       ! Compute local variables

       call grid_methods_polarcoords(grid)
       call observation_locations(grid,fcstmdl(i))
       call observation_assignments(fcstmdl(i),fv3)

       
       call variable_interface_cleanup_struct(fcstmdl(i)) ! for now
       
    end do ! do i = 1, size(tcinfo)
    
    ! Deallocate memory for local variables

    if(allocated(fcstmdl)) deallocate(fcstmdl)
    if(allocated(tcinfo))  deallocate(tcinfo)
    call variable_interface_cleanup_struct(fv3)
    call variable_interface_cleanup_struct(grid)
    
    !=====================================================================

  end subroutine forecast_model_fv3

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_observations.f90

  ! DESCRIPTION:

  ! This subroutine populates the arrays within the FORTRAN
  ! fcstmdl_struct variable with values from the FORTRAN fv3_struct
  ! variable.

  ! INPUT VARIABLES:

  ! * fcstmdl; a FORTRAN fcstmdl_struct variable which has been
  !   initialized and observation locations assigned.

  ! * fv3; a FORTRAN fv3_struct variable containing with variable
  !   arrays which have been populated from the contents of the
  !   external FV3 netcdf files.
  
  ! OUTPUT VARIABLES:

  ! * fcstmdl; a FORTRAN fcstmdl_struct variable which has been
  !   populated with the contents from the FORTRAN fv3_struct
  !   variable.

  !-----------------------------------------------------------------------

  subroutine fv3_observations(fcstmdl,fv3)

    ! Define variables passed to routine

    type(fcstmdl_struct)                                                :: fcstmdl
    type(fv3_struct)                                                    :: fv3

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================  

    ! Loop through local variable

    do i = 1, fcstmdl%nobs

       ! Define local variables

       fcstmdl%p(i,:)   = fv3%p(fcstmdl%idx(i),:)
       fcstmdl%q(i,:)   = fv3%q(fcstmdl%idx(i),:)
       fcstmdl%t(i,:)   = fv3%t(fcstmdl%idx(i),:)
       fcstmdl%u(i,:)   = fv3%u(fcstmdl%idx(i),:)
       fcstmdl%v(i,:)   = fv3%v(fcstmdl%idx(i),:)
       fcstmdl%lat(i)   = fv3%lat(fcstmdl%idx(i))
       fcstmdl%lon(i)   = fv3%lon(fcstmdl%idx(i))
       fcstmdl%slmsk(i) = fv3%slmsk(fcstmdl%idx(i))
       
    end do ! do i = 1, fcstmdl%nobs
       
    !=====================================================================  
    
  end subroutine fv3_observations
  
  !=======================================================================

  ! SUBROUTINE:

  ! observation_locations.f90

  ! DESCRIPTION:

  ! This subroutine determines all grid locations representing
  ! observations to be processed; the algorithm below utilizes KD-tree
  ! searching algorithm to identify forecast model grid cell locations
  ! that fall within a user specified radius; further, thinning is
  ! performed in accordance with the user specified namelist variable
  ! 'sample_radius'.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the geographical
  !   locations of the forecast model grid as well as the reference
  !   location about which to determine observation locations.

  ! * fcstmdl; a FORTRAN fcstmdl_struct variable.

  ! OUTPUT VARIABLES:

  ! * fcstmdl; a FORTRAN fcstmdl_struct variable which has been
  !   initialized and contains the grid cell locations (idx) for
  !   observations to be processed.

  !-----------------------------------------------------------------------

  subroutine observation_locations(grid,fcstmdl)

    ! Define variables passed to routine

    type(fcstmdl_struct)                                                :: fcstmdl
    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    type(kdtree_struct)                                                 :: kdtree
    real(r_kind)                                                        :: radius_max
    real(r_kind)                                                        :: radius_min

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================    

    ! Define local variables
    
    kdtree%ncoords = 1
    kdtree%nn      = grid%ncoords
    call variable_interface_setup_struct(kdtree)
    kdtree%nalloc  = grid%ncoords
    kdtree%r2      = (tc_radius*tc_radius)

    ! Compute local variables

    call math_methods_kdtree_r2(grid,grid,kdtree)
    
    ! Check local variable and proceed accordingly

    if(sample_radius .eq. spval) then

       ! Define local variables

       fcstmdl%nobs = kdtree%nfound
       call variable_interface_setup_struct(fcstmdl)
       fcstmdl%idx  = kdtree%idx(1,1:kdtree%nfound)

    else   ! if(sample_radius .eq. spval)

       ! Define local variables

       fcstmdl%nobs = 0
       radius_min   = 0.0
       radius_max   = radius_min + sample_radius

       ! Loop through local variable

       do while(radius_max .le. tc_radius)

          ! Loop through local variable
       
          do i = 1, kdtree%nfound

             ! Check local variable

             if((sqrt(kdtree%r2dist(1,i)) .ge. radius_min) .and.           &
                  & (sqrt(kdtree%r2dist(1,i)) .lt. radius_max)) then

                ! Define local variables

                fcstmdl%nobs = fcstmdl%nobs + 1

             end if ! if((sqrt(kdtree%r2dist(1,i)) .ge. radius_min)
                    ! .and. (sqrt(kdtree%r2dist(1,i))
                    ! .lt. radius_max))

          end do ! do i = 1, kdtree%nfound

          ! Define local variables

          radius_min = radius_max + sample_radius
          radius_max = radius_min + sample_radius
          
       end do ! do while(radius .le. tc_radius)

       ! Define local variables

       call variable_interface_setup_struct(fcstmdl)
       fcstmdl%nobs = 0
       radius_min   = 0.0
       radius_max   = radius_min + sample_radius

       ! Loop through local variable

       do while(radius_max .le. tc_radius)

          ! Loop through local variable
       
          do i = 1, kdtree%nfound

             ! Check local variable

             if((sqrt(kdtree%r2dist(1,i)) .ge. radius_min) .and.           &
                  & (sqrt(kdtree%r2dist(1,i)) .lt. radius_max)) then

                ! Define local variables

                fcstmdl%nobs              = fcstmdl%nobs + 1
                fcstmdl%idx(fcstmdl%nobs) = kdtree%idx(1,i)

             end if ! if((sqrt(kdtree%r2dist(1,i)) .ge. radius_min)
                    ! .and. (sqrt(kdtree%r2dist(1,i))
                    ! .lt. radius_max))

          end do ! do i = 1, kdtree%nfound

          ! Define local variables

          radius_min = radius_max + sample_radius
          radius_max = radius_min + sample_radius
          
       end do ! do while(radius .le. tc_radius)       
       
    end if ! if(sample_radius .eq. spval)
    
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(kdtree)    

    !=====================================================================
    
  end subroutine observation_locations

  !=======================================================================
  
end module forecast_model_interface
