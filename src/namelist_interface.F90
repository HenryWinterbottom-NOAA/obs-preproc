module namelist_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: namelist_interface
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

  !-----------------------------------------------------------------------

  ! DESCRIPTION (alphabetized):

  ! * analdate; a FORTRAN character string specifying the analysis
  !   date about which to define the observation times; formatted as,
  !   assuming UNIX convention, ccyy-mm-dd_HH:MM:SS.

  ! * bufr_filepath; a FORTRAN character string specifying the
  !   full-path to the BUFR file to be written (or appended) to.

  ! * bufr_tblpath; a FORTRAN character string specifying the
  !   full-path to the external file containing the BUFR table to be
  !   written (or appended) to the output BUFR file. 
  
  ! * datapath; a FORTRAN character string specifying the full-path to
  !   the directory to contain output files written by the respective
  !   routines.

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  ! * sonde_filelist; a FORTRAN character string specifying the
  !   full-path to the external file containing a list of TEMPDROP
  !   formatted sondes to be decoded.

  ! * tempdrop_compute_drift; a FORTRAN logical value specifying
  !   whether to estimate the sonde drift, and the respective
  !   geographical locations, from the collected TEMP-DROP formatted
  !   observations.

  ! * tempdrop_normalize; a FORTRAN logical value specifying whether
  !   to normalize the geographical coordinate values computed for the
  !   advection trajectory of the TEMP-DROP formatted observations.
  
  ! * tempdrop_write_hsa; a FORTRAN logical value specifying whether
  !   to write National Oceanic and Atmospheric Administration (NOAA)
  !   Atlantic Oceanographic and Meteorological Laboratory (AOML)
  !   Hurricane Research Division (HRD) spline analysis (HSA)
  !   formatted files.

  ! * tempdrop_write_hsa_drift; a FORTRAN logical value specifying
  !   whether to write National Oceanic and Atmospheric Administration
  !   (NOAA) Atlantic Oceanographic and Meteorological Laboratory
  !   (AOML) Hurricane Research Division (HRD) spline analysis (HSA)
  !   formatted files which include the geographical positions
  !   estimated as a function of sonde drift; tempdrop_compute_drift
  !   must be true.

  ! * tempdrop_write_nc_skewt; a FORTRAN logical value specifying
  !   whether to write a network common data format (netcdf) file
  !   containing interpolated National Oceanic and Atmospheric
  !   Administration (NOAA) Atlantic Oceanographic and Meteorological
  !   Laboratory (AOML) Hurricane Research Division (HRD) spline
  !   analysis (HSA) values; tempdrop_write_hsa_drift and
  !   tempdrop_compute_drift must both be true.
  
  !-----------------------------------------------------------------------

  ! Define local variables

  character(len=500)                                                    :: &
       & bufr_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & bufr_tblpath = 'NOT USED'
  character(len=500)                                                    :: &
       & datapath = './'
  character(len=500)                                                    :: &
       & sonde_filelist = 'NOT USED'
  character(len=500)                                                    :: &
       & tempdrop_hsa_table_file = './tempdrop-hsa.table' ! NEED
  character(len=19)                                                     :: &
       & analdate = '2000-01-01_00:00:00'
  logical                                                               :: &
       & debug = .false.
  logical                                                               :: &
       & is_sonde = .false. ! NEED
  logical                                                               :: &
       & is_sonde_tempdrop = .false. ! NEED
  logical                                                               :: &
       & tempdrop_compute_drift = .false.
  logical                                                               :: &
       & tempdrop_normalize = .false. 
  namelist /share/    analdate, datapath, debug, is_sonde
  namelist /bufrio/   bufr_filepath, bufr_tblpath  
  namelist /sonde/    is_sonde_tempdrop, sonde_filelist,                   &
       & tempdrop_compute_drift, tempdrop_hsa_table_file,                  &
       & tempdrop_normalize
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE: 

  ! namelist.f90

  ! DESCRIPTION: 

  ! This subroutine acts as the interface to the namelist file,
  ! provided as 'obs-preproc.input' by the user.

  !-----------------------------------------------------------------------
  
  subroutine namelist()

    ! Define variables computed within routine

    character(len=500)                                                  :: nml_filename
    logical                                                             :: is_it_there
    integer                                                             :: unit_nml

    !=================================================================== 

    ! Define local variables

    nml_filename = './obs-preproc.input'
    unit_nml     = 9
    is_it_there  = .false.
    inquire(file = trim(adjustl(nml_filename)),exist = is_it_there)
    
    ! Check local variable and proceed accordingly

    if(is_it_there) then

       ! Define local variables

       open(file   = trim(adjustl(nml_filename)),                          &
            unit   = unit_nml        ,                                     &
            status = 'old'         ,                                       &
            form   = 'formatted'     ,                                     &
            action = 'read')
       read(unit_nml,NML = share)
       read(unit_nml,NML = sonde)
       close(unit_nml)

    else  ! if(is_it_there)

       ! Define local variables

       write(6,500) trim(adjustl(nml_filename))
       stop(99)
       
    end if ! if(is_it_there)

500 format('NAMELISTPARAMS: ', a, ' not found in the current working ',    &
         & 'directory. ABORTING!!!!')
    
    !===================================================================

  end subroutine namelist

  !=======================================================================

end module namelist_interface
