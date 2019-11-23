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
  public :: fileio_interface_write
  interface fileio_interface_read
     module procedure read_hsa
     module procedure read_sonde_filenames
  end interface fileio_interface_read
  interface fileio_interface_write
     module procedure write_sonde_decode_table
  end interface fileio_interface_write
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! read_hsa.f90

  ! DESCRIPTION:

  ! This subroutine reads a National Oceanic and Atmospheric
  ! Administration (NOAA) Atlantic Oceanographic and Meteorological
  ! Laboratory (AOML) Hurricane Research Division (HRD) HRD Spline
  ! Analysis (HSA) formatted observation files.

  ! INPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable.

  ! OUTPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable containing the HSA file
  !   contents.

  !-----------------------------------------------------------------------

  subroutine read_hsa(hsa)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa

    ! Define variables computed within routine

    real(r_kind)                                                        :: dummy

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    hsa%nz = 0
    open(99,file=trim(adjustl(hsa%filename)),form='formatted')
1000 read(99,*,end=1001) dummy
    hsa%nz = hsa%nz + 1
    goto 1000
1001 continue
    close(99)
    call variable_interface_setup_struct(hsa)
    open(99,file=trim(adjustl(hsa%filename)),form='formatted')

    ! Loop through local variable

    do i = 1, hsa%nz

       ! Define local variables

       read(99,500,err=1002) hsa%wx(i), hsa%yymmdd(i), hsa%gmt(i),        &
            & hsa%lat(i), hsa%lon(i), hsa%p(i), hsa%t(i), hsa%rh(i),      &
            & hsa%z(i), hsa%u(i), hsa%v(i), hsa%tail(i)

    end do ! do i = 1, hsa%nz

    ! Define local variables

    close(99)
    return
1002 continue
500 format(i2,1x,f7.0,1x,i4,1x,2(f7.3,1x),3(f6.1,1x),f7.1,2(f6.1,1x),a4)

    !=====================================================================

  end subroutine read_hsa
  
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

       read(99,*) sonde%filename(i)
       if(debug) write(6,500) trim(adjustl(sonde%filename(i)))

    end do ! do i = 1, sonde%nsondes

    ! Define local variables

    close(99)
500 format('READ_SONDE_FILENAMES: Reading in file ',a,' to be processed.')

    !=====================================================================

  end subroutine read_sonde_filenames

  !=======================================================================

  ! SUBROUTINE:

  ! write_sonde_decode_table.f90

  ! DESCRIPTION:

  ! This subroutine writes a column-delimted table containing the
  ! TEMP-DROP encoded observation file and the corresponding decoded
  ! HSA-formatted observation file.

  ! INPUT VARIABLES:

  ! * table_filename; a FORTRAN character string specifying the
  !   filename path for the column-delimited table; if this file does
  !   not exist upon call to this routine, it will be created; if the
  !   file does exist upon call to this routine, it will be appended.

  ! * sonde_filename; a FORTRAN character string specifying the
  !   filename path for the TEMP-DROP encoded observation file.

  ! * hsa; a FORTRAN hsa_struct variable containing (at minimum) the
  !   filename path for the decoded HSA-formatted observation file
  !   (within the attribute 'filename').
  
  !-----------------------------------------------------------------------

  subroutine write_sonde_decode_table(table_filename,sonde_filename,hsa)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa
    character(len=500)                                                  :: sonde_filename
    character(len=500)                                                  :: table_filename

    ! Define variables computed within routine

    logical                                                             :: exist
    
    !=====================================================================

    ! Define local variables

    inquire(file = trim(adjustl(table_filename)),exist = exist)

    ! Check local variable and proceed accordingly

    if(exist) then

       ! Define local variables

       open(99,file=trim(adjustl(table_filename)),form='formatted',       &
            & status='old',position='append',action='write')
       
    else   ! if(exist)

       ! Define local variables

       open(99,file=trim(adjustl(table_filename)),form='formatted',       &
            & status='new',action='write')
       
    end if ! if(exist)

    ! Define local variables

    write(99,500) trim(adjustl(sonde_filename)),                          &
         & trim(adjustl(hsa%filename))
    if(debug) write(6,501) trim(adjustl(sonde_filename)),                 &
         & trim(adjustl(hsa%filename))
    close(99)
500 format(a,1x,a)
501 format('WRITE_SONDE_DECODE_TABLE: Mapping observation file ',a,' to ' &
         & 'decoded observation file ',a,'.')

    !=====================================================================
  
  end subroutine write_sonde_decode_table
    
  !=======================================================================

end module fileio_interface
