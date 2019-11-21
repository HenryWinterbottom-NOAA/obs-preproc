module variable_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: variable_interface
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
  public :: bufr_mxlv
  public :: bufr_mxmn
  public :: bufr_spval
  public :: bufr_struct
  public :: obs_flag_struct
  public :: sonde_struct
  public :: spval
  public :: variable_interface_cleanup_struct
  public :: variable_interface_setup_struct
  interface variable_interface_cleanup_struct
     module procedure finalize_bufr_struct
     module procedure finalize_obs_flag_struct
     module procedure finalize_sonde_struct
  end interface variable_interface_cleanup_struct
  interface variable_interface_setup_struct
     module procedure initialize_bufr_struct
     module procedure initialize_obs_flag_struct
     module procedure initialize_sonde_struct
  end interface variable_interface_setup_struct

  ! Define local variables

  real(r_double), parameter                                             :: bufr_spval = 10.e10
  real(r_kind),   parameter                                             :: spval      = huge(0.0)
  type bufr_struct
     character(len=80)                                                  :: obstr
     character(len=80)                                                  :: hdstr
     character(len=80)                                                  :: qcstr
     character(len=80)                                                  :: oestr
     character(len=19)                                                  :: cdate
     character(len=8)                                                   :: subset
     real(r_double),            dimension(:,:),             allocatable :: obs
     real(r_double),            dimension(:,:),             allocatable :: qcf
     real(r_double),            dimension(:,:),             allocatable :: oer
     real(r_double),            dimension(:),               allocatable :: hdr
     integer                                                            :: idate
     integer                                                            :: mxmn
     integer                                                            :: mxlv
     integer                                                            :: nrecs
  end type bufr_struct            ! type bufr_struct
  type obs_flag_struct
     character(len=500)                                                 :: filename
     character(len=10),         dimension(:),               allocatable :: mneumonic
     character(len=8),          dimension(:),               allocatable :: subset
     real(r_kind),              dimension(:),               allocatable :: val
     integer,                   dimension(:),               allocatable :: obs_type
     integer                                                            :: nflag
  end type obs_flag_struct        ! type obs_flag_struct
  type sonde_struct
     character(len=500),        dimension(:),               allocatable :: sonde_filename
     integer                                                            :: nsondes
  end type sonde_struct           ! type sonde_struct
  integer,        parameter                                             :: bufr_mxlv  = 200
  integer,        parameter                                             :: bufr_mxmn  = 35

    !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_bufr_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! bufr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN bufr_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_bufr_struct(grid)

    ! Define variables passed routine

    type(bufr_struct)                                                   :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%obs)) deallocate(grid%obs)
    if(allocated(grid%qcf)) deallocate(grid%qcf)
    if(allocated(grid%oer)) deallocate(grid%oer)
    if(allocated(grid%hdr)) deallocate(grid%hdr)

    !=====================================================================

  end subroutine finalize_bufr_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_obs_flag_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! obs_flag_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN obs_flag_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_obs_flag_struct(grid)

    ! Define variables passed routine

    type(obs_flag_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%mneumonic)) deallocate(grid%mneumonic)
    if(allocated(grid%subset))    deallocate(grid%subset)
    if(allocated(grid%val))       deallocate(grid%val)
    if(allocated(grid%obs_type))  deallocate(grid%obs_type)

    !=====================================================================

  end subroutine finalize_obs_flag_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_sonde_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! sonde_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN sonde_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_sonde_struct(grid)

    ! Define variables passed to routine

    type(sonde_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%sonde_filename)) deallocate(grid%sonde_filename)
    
    !=====================================================================

  end subroutine finalize_sonde_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_bufr_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! bufr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN bufr_struct variable containing the variables
  !   necessary to allocate and initialize the respective variable
  !   arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN bufr_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_bufr_struct(grid)

    ! Define variables passed routine

    type(bufr_struct)                                                   :: grid

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(grid%mxmn .eq. 0) grid%mxmn = bufr_mxmn       
    if(grid%mxlv .eq. 0) grid%mxlv = bufr_mxlv

    ! Allocate memory for local variables

    if(.not. allocated(grid%obs)) allocate(grid%obs(grid%mxmn,grid%mxlv))
    if(.not. allocated(grid%qcf)) allocate(grid%qcf(grid%mxmn,grid%mxlv))
    if(.not. allocated(grid%oer)) allocate(grid%oer(grid%mxmn,grid%mxlv))
    if(.not. allocated(grid%hdr)) allocate(grid%hdr(grid%mxmn))

    ! Define local variables

    grid%obs = bufr_spval
    grid%qcf = bufr_spval
    grid%oer = bufr_spval
    grid%hdr = bufr_spval

    !=====================================================================

  end subroutine initialize_bufr_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_obs_flag_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! obs_flag_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN obs_flag_struct variable containing the
  !   variables necessary to allocate and initialize the respective
  !   variable arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN obs_flag_struct variable containing allocated
  !   and initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_obs_flag_struct(grid)

    ! Define variables passed routine

    type(obs_flag_struct)                                               :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%mneumonic))                                    &
         & allocate(grid%mneumonic(grid%nflag))
    if(.not. allocated(grid%subset))                                       &
         & allocate(grid%subset(grid%nflag))
    if(.not. allocated(grid%val))                                          &
         & allocate(grid%val(grid%nflag))
    if(.not. allocated(grid%obs_type))                                     &
         & allocate(grid%obs_type(grid%nflag))

    !=====================================================================

  end subroutine initialize_obs_flag_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_sonde_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! sonde_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN sonde_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN sonde_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_sonde_struct(grid)

    ! Define variables passed to routine

    type(sonde_struct)                                                  :: grid    

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%sonde_filename))                               &
         & allocate(grid%sonde_filename(grid%nsondes))

    !=====================================================================

  end subroutine initialize_sonde_struct

  !=======================================================================

end module variable_interface
