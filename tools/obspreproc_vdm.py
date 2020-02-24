#    Copyright (C) 2019 Henry R. Winterbottom

#    Email: Henry.Winterbottom@noaa.gov

#    This file is part of obs-preproc.

#    obs-preproc is free software: you can redistribute it and/or
#    modify it under the terms of the GNU General Public License as
#    published by the Free Software Foundation, either version 3 of
#    the License, or (at your option) any later version.

#    obs-preproc is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with obs-preproc.  If not, see
#    <http://www.gnu.org/licenses/>.

# ----

"""
SCRIPT:   

   obspreproc_vdm.py

AUTHOR: 

   Henry R. Winterbottom; 28 January 2020

ABSTRACT:

   (1) ObsPreProcVDM: This is the base-class object to
       pre-process/format available vortex data message (VDM) files in
       preparation for down-stream applications.

   (2) ObsPreProcVDMError: This is the base-class for all module
       raised exceptions; it is a subclass of Exception.

   (3) ObsPreProcVDMOptions: This is the base-class object used to
       collect command line arguments provided by the user.

   (4) VortexDataMessage: This is the base-class object to collect
       information from vortex data message (VDM) files and to prepare
       a Pythong dictionary (vdm_dict) that contains the available
       observation information.

   * main; This is the driver-level method to invoke the tasks within
     this script.

USAGE:

   python obspreproc_vdm.py

     --cycle <a string specifying the forecast cycle timestamp;
              formatted as (assuming UNIX convention)
              %Y-%m-%d_%H:%M:%S>

     --datapath <a string specifying the path to the vortex data
                 message file(s)>

     --window <an optional float variable specifying the temporal
               interval, centered on the forecast cycle, for which to
               collect vortex data message(s)>

HISTORY:

   2020-01-28: Henry R. Winterbottom -- Initial implementation.

"""

# ----

import argparse
import os
import re

import util as hafsutil

# ----

__author__ = "Henry R. Winterbottom"
__copyright__ = "2019 Henry R. Winterbottom, NOAA/NCEP/EMC"
__version__ = "1.0.0"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__status__ = "Development"

# ----


class ObsPreProcVDM(object):
    """
    DESCRIPTION:

    This is the base-class object to pre-process/format available
    vortex data message (VDM) files in preparation for down-stream
    applications.

    INPUT VARIABLES:

    * opts_obj; a Python object containing the user command line
      options.

    """

    def __init__(self, opts_obj):
        """ 
        DESCRIPTION:

        Creates a new ObsPreProcVDM object.

        """
        self.opts_obj = opts_obj
        kwargs = {'opts_obj': self.opts_obj}
        self.vdm = VortexDataMessage(**kwargs)

    def createfilelist(self):
        """
        DESCRIPTION:

        This method creates a formatted list of files in accordance
        with the expectations of the down-stream application
        executable(s).

        """
        workpath = os.getcwd()
        with open(os.path.join(workpath, 'filelist.list'), 'w') as f:
            for key in self.vdm_dict.keys():
                filename = os.path.basename(self.vdm_dict[key]['filename'])
                f.write('"%s"\n' % os.path.join(workpath, filename))

    def format_vdm(self):
        """
        DESCRIPTION:

        This method invokes the base-class object attribute vdm to
        collect vortex data message (VDM) observations and
        subsequently contruct a Python dictionary (vdm_dict)
        containing the observations available within the user temporal
        window.

        """
        self.vdm_dict = self.vdm.run()

    def write_vdm(self):
        """
        DESCRIPTION:

        This method prepares a formatted file containing each
        available vortex data message (VDM) observations; the output
        file is formatted as follows:

        Line 1: <A> <B> <C>

        A: a Python character string specifying the fix timestamp.

        B: a Python float valued variable specifying the fix location
           latitude coordinate (degrees North).

        C: a Python float valued variable specifying the fix location
           longitude coordinate (degrees East).

        Lines 2-3: <A> <B> <C> <D> <E> <F> <G>

        A: a Python character string specifying the flight-level wind
           observation timestamp.

        B: a Python float valued variable specifying the isobaric
           elevation for the observation (Pascals).

        C: a Python float valued variable specifying the minimum
           elevation of the isobaric level (m).

        D: a Python float valued variable specifying the distance,
           relative to the fix location, for the flight-level
           observations (meters).

        E: a Python float valued variable specifying the bearing,
           relative to the fix location, for the flight-level
           observations (degrees).

        F: a Python float valued variable specifying the wind
           direction observation (degrees).

        G: a Python float valued variable specifying the wind speed
           observation (meters per second).

        """
        fix_info = ('{0} {1} {2}\n')
        flwind_info = ('{0} {1} {2} {3} {4} {5} {6}\n')
        for key in self.vdm_dict.keys():
            filename = os.path.basename(self.vdm_dict[key]['filename'])
            fix_lat = self.vdm_dict[key]['fix_lat']
            fix_lon = self.vdm_dict[key]['fix_lon']
            fix_timestamp = self.vdm_dict[key]['fix_timestamp']
            with open(filename, 'wt') as f:
                f.write(fix_info.format(fix_timestamp, fix_lat, fix_lon))
                lines_seen = set()
                for i in range(len(self.vdm_dict[key]['flwinds']['obstime'])):
                    alt = self.vdm_dict[key]['flwinds']['alt'][i]
                    bearing = self.vdm_dict[key]['flwinds']['bearing'][i]
                    distance = self.vdm_dict[key]['flwinds']['distance'][i]
                    obstime = self.vdm_dict[key]['flwinds']['obstime'][i]
                    plev = self.vdm_dict[key]['flwinds']['plev'][i]
                    wdir = self.vdm_dict[key]['flwinds']['wdir'][i]
                    wspd = self.vdm_dict[key]['flwinds']['wspd'][i]
                    line = flwind_info.format(
                        obstime, plev, alt, distance, bearing, wdir, wspd)
                    if line not in lines_seen:
                        f.write(line)
                        lines_seen.add(line)

    def run(self):
        """
        DESCRIPTION:

        This method performs the following tasks:

        (1) Collect vortex data message (VDM) observations.

        (2) Prepares formatted files for each available VDM
            file/observations.

        (3) Creates a list of files for down-stream applications.

        """
        self.format_vdm()
        self.write_vdm()
        self.createfilelist()

# ----


class ObsPreProcVDMError(Exception):
    """
    DESCRIPTION:

    This is the base-class for all module raised exceptions.

    INPUT VARIABLES:

    * msg; a Python string to accompany the raised exception.

    """

    def __init__(self, msg):
        """
        DESCRIPTION:

        Creates a new ObsPreProcVDMError object.

        """
        super(ObsPreProcVDMError, self).__init__(msg)

# ----


class ObsPreProcVDMOptions(object):
    """
    DESCRIPTION:

    This is the base-class object used to collect command line
    arguments provided by the user.

    """

    def __init__(self):
        """
        DESCRIPTION:

        Creates a new ObsPreProcVDMOptions object.

        """
        self.parser = argparse.ArgumentParser()
        self.parser = argparse.ArgumentParser()
        self.parser.add_argument('-c', '--cycle', help='The forecast cycle timestamp; '
                                 'formatted as (assuming UNIX convention) %Y-%m-%d_%H:%M:%S.',
                                 default=None)
        self.parser.add_argument('-d', '--datapath', help='The path to the vortex data message '
                                 '(VDM) file(s).', default=None)
        self.parser.add_argument('-w', '--window', help='The temporal interval, centered on '
                                 'the forecast cycle, for which to collect vortex data message(s).',
                                 default=10800)
        self.opts_obj = lambda: None

    def run(self):
        """
        DESCRIPTION:

        This method collects the user-specified command-line
        arguments; the available command line arguments are as
        follows:

        -c; The forecast cycle timestamp; formatted as (assuming UNIX
            convention) %Y-%m-%d_%H:%M:%S.

        -d; The path to the vortex data message(s) (VDM) for the
            respective forecast cycle.

        OUTPUT VARIABLES:

        * opts_obj; a Python object containing the user command line
          options.

        """
        opts_obj = self.opts_obj
        args = self.parser.parse_args()
        args_list = ['cycle', 'datapath']
        for item in args_list:
            value = getattr(args, item)
            if value is None:
                msg = ('The argument %s cannot be NoneType. Aborting!!!' % item)
                raise ObsPreProcVDMError(msg=msg)
            else:
                setattr(opts_obj, item, value)
        args_list = ['window']
        for item in args_list:
            value = getattr(args, item)
            setattr(opts_obj, item, value)
        return opts_obj

# ----


class VortexDataMessage(object):
    """
    DESCRIPTION:

    This is the base-class object to collect information from vortex
    data message (VDM) files and to prepare a Pythong dictionary
    (vdm_dict) that contains the available observation information.

    INPUT VARIABLES:

    * opts_obj; a Python object containing the user command line
      options.

    """

    def __init__(self, opts_obj):
        """ 
        DESCRIPTION:

        Creates a new VortexDataMessage object.

        """
        opts_list = ['cycle', 'datapath', 'window']
        for item in opts_list:
            value = getattr(opts_obj, item)
            setattr(self, item, value)
        self.get_julianwindow()
        self.vdm_dict = dict()
        self.missing_data = -9.e30

    def _filetimestamp(self, filename):
        """
        DESCRIPTION:

        This method defines the timestamp as determined from the
        filename.

        """
        filename_strs = re.findall(r"[\d']+", os.path.basename(filename))
        for filename_str in filename_strs:
            if len(filename_str) >= 8:
                digit_list = [int(s) for s in filename_str if s.isdigit()]
                datestr = str()
                for item in digit_list:
                    datestr = datestr+str(item)
                kwargs = {'datestr': datestr, 'in_frmttyp': '%Y%m%d%H%M',
                          'out_frmttyp': '%Y-%m-%d_%H:%M:%S'}
                datestr = hafsutil.date_interface.datestrfrmt(**kwargs)
                break
        return datestr

    def _read_vdm(self, filename):
        """
        DESCRIPTION:

        This method reads a vortex data message (VDM) file and returns
        a Python list containing the contents.

        INPUT VARIABLES:

        * filename; a Python string specifying the path for the VDM
          file to be read.

        OUTPUT VARIABLES:

        * vdm_data; a Python list containing the contents of the VDM
          file.

        """
        with open(filename, 'rt') as f:
            vdm_data = f.read()
        vdm_data = vdm_data.splitlines()
        vdm_data = list(filter(None, vdm_data))
        return vdm_data

    def get_fixlocations(self):
        """
        DESCRIPTION:

        This method defines the fix geographical location (e.g.,
        latitude and longitude coordinates) and assigns values to the
        base-class attribute fix_lat and fix_lon within the base-class
        Python dictionary vdm_dict.

        """
        locinfo_dict = {'lat': 1, 'hemis_sn': 3, 'lon': 4, 'hemis_we': 6}
        for key in self.vdm_dict.keys():
            filename = self.vdm_dict[key]['filename']
            kwargs = {'filename': filename}
            vdm_data = self._read_vdm(**kwargs)
            def loc_obj(): return None
            for item in vdm_data:
                if 'B.' in item:
                    for locinfo_key in locinfo_dict.keys():
                        value = item.split()[locinfo_dict[locinfo_key]]
                        setattr(loc_obj, locinfo_key, value)
                    if getattr(loc_obj, 'hemis_sn').lower() == 's':
                        hemis_sn_scale = -1.0
                    else:
                        hemis_sn_scale = 1.0
                    if getattr(loc_obj, 'hemis_we').lower() == 'w':
                        hemis_we_scale = -1.0
                    else:
                        hemis_we_scale = 1.0
                    lat = float(getattr(loc_obj, 'lat'))
                    lon = float(getattr(loc_obj, 'lon'))
            self.vdm_dict[key]['fix_lat'] = hemis_sn_scale*lat
            self.vdm_dict[key]['fix_lon'] = hemis_we_scale*lon

    def get_fixtimes(self):
        """
        DESCRIPTION:

        This method defines the timestamp attributes for the vortex
        data message (VDM) fix time collected from the respective VDM
        files and defines the base-class attribute fix_timestamp
        within the base-class Python dictionary vdm_dict; checks are
        performed to make sure the time stamps collected from the
        filename and that of the observation (e.g., fix) are sane.

        """
        for key in self.vdm_dict.keys():
            timestamp = self.vdm_dict[key]['timestamp']
            kwargs = {'datestr': timestamp}
            timestamp_datecomps = hafsutil.date_interface.datestrcomps(
                **kwargs)
            timestamp_dom = getattr(timestamp_datecomps, 'day')
            filename = self.vdm_dict[key]['filename']
            kwargs = {'filename': filename}
            vdm_data = self._read_vdm(**kwargs)
            for item in vdm_data:
                if 'A.' in item:
                    vdm_datestr = item.split()[1]
                    dom = vdm_datestr.split('/')[0]
                    if dom < timestamp_dom:
                        kwargs = {'datestr': timestamp, 'in_frmttyp': '%Y-%m-%d_%H:%M:%S',
                                  'out_frmttyp': '%Y-%m-%d_%H:%M:%S', 'offset_seconds':
                                  86400}
                        datestr = hafsutil.date_interface.datestrfrmt(
                            **kwargs)
                        kwargs = {'datestr': datestr}
                        datecomps = hafsutil.date_interface.datestrcomps(
                            **kwargs)
                        year = getattr(datecomps, 'year')
                        month = getattr(datecomps, 'month')
                    else:
                        year = getattr(timestamp_datecomps, 'year')
                        month = getattr(timestamp_datecomps, 'month')
                    kwargs = {'datestr': vdm_datestr, 'in_frmttyp': '%d/%H:%M:%SZ',
                              'out_frmttyp': '%d_%H:%M:%S'}
                    datestr = hafsutil.date_interface.datestrfrmt(**kwargs)
                    fix_time = '%s-%s-%s' % (year, month, datestr)
                    self.vdm_dict[key]['fix_timestamp'] = fix_time

    def get_flwinds(self):
        """
        DESCRIPTION:

        This method parses the vortex data message (VDM) and returns
        the inbound and outbound flight-level wind information and
        updates the base-class Python dictionary vdm_dict attributes
        describing the flight-level wind observations.

        """
        timestamp_items = ['year', 'month', 'day']
        flwind_items = ['alt', 'bearing', 'distance',
                        'obstime', 'plev', 'wdir', 'wspd']
        for key in self.vdm_dict.keys():
            def timestamp_obj(): return None
            self.vdm_dict[key]['flwinds'] = dict()
            for item in flwind_items:
                self.vdm_dict[key]['flwinds'][item] = list()
            timestamp = self.vdm_dict[key]['fix_timestamp']
            kwargs = {'datestr': timestamp}
            timestamp_datecomps = hafsutil.date_interface.datestrcomps(
                **kwargs)
            for item in timestamp_items:
                setattr(timestamp_obj, item, getattr(
                    timestamp_datecomps, item))
            filename = self.vdm_dict[key]['filename']
            kwargs = {'filename': filename}
            vdm_data = self._read_vdm(**kwargs)
            for item in vdm_data:
                if 'C.' in item:
                    try:
                        plev = float(item.split()[1])*100.0
                    except:
                        plev = self.missing_data
                    try:
                        alt = float(item.split()[3])
                    except:
                        alt = self.missing_data
                if 'J.' in item:
                    try:
                        wdir = float(item.split()[1])
                    except:
                        wdir = self.missing_data
                    try:
                        wspd = float(item.split()[3])*0.51444
                    except:
                        wspd = self.missing_data
                if 'K.' in item:
                    try:
                        bearing = float(item.split()[1])
                    except:
                        bearing = self.missing_data
                    try:
                        distance = float(item.split()[3])*1852.0
                    except:
                        distance = self.missing_data
                    obstime = item.split()[5]
                    datestr = ('%s-%s-%s_%s') % (timestamp_obj.year,
                                                 timestamp_obj.month, timestamp_obj.day, obstime)
                    kwargs = {'datestr': datestr, 'in_frmttyp': '%Y-%m-%d_%H:%M:%SZ',
                              'out_frmttyp': '%Y-%m-%d_%H:%M:%S'}
                    obstime = hafsutil.date_interface.datestrfrmt(**kwargs)
            for item in flwind_items:
                self.vdm_dict[key]['flwinds'][item].append(eval(item))
            for item in vdm_data:
                if 'C.' in item:
                    try:
                        plev = float(item.split()[1])*100.0
                    except:
                        plev = self.missing_data
                    try:
                        alt = float(item.split()[3])
                    except:
                        alt = self.missing_data
                if 'N.' in item:
                    try:
                        wdir = float(item.split()[1])
                    except:
                        wdir = self.missing_data
                    try:
                        wspd = float(item.split()[3])*0.51444
                    except:
                        wspd = self.missing_data
                if 'O.' in item:
                    try:
                        bearing = float(item.split()[1])
                    except:
                        bearing = self.missing_data
                    try:
                        distance = float(item.split()[3])*1852.0
                    except:
                        distance = self.missing_data
                    obstime = item.split()[5]
                    datestr = ('%s-%s-%s_%s') % (timestamp_obj.year,
                                                 timestamp_obj.month, timestamp_obj.day, obstime)
                    kwargs = {'datestr': datestr, 'in_frmttyp': '%Y-%m-%d_%H:%M:%SZ',
                              'out_frmttyp': '%Y-%m-%d_%H:%M:%S'}
                    obstime = hafsutil.date_interface.datestrfrmt(**kwargs)
            for item in flwind_items:
                self.vdm_dict[key]['flwinds'][item].append(eval(item))

    def get_julianwindow(self):
        """
        DESCRIPTION:

        This method defines the base-class object attributes
        julian_start and julian_stop which specify the Julian dates
        for the user specified data assimilation window centered on
        the user specified forecast cycle/analysis time.

        """
        datestr = self.cycle
        kwargs = {'datestr': datestr, 'in_frmttyp': '%Y-%m-%d_%H:%M:%S',
                  'out_frmttyp': '%Y-%m-%d_%H:%M:%S', 'offset_seconds':
                  -1.0*int(self.window)}
        datestr = hafsutil.date_interface.datestrfrmt(**kwargs)
        kwargs = {'datestr': datestr}
        datecomps = hafsutil.date_interface.datestrcomps(**kwargs)
        self.julian_start = getattr(datecomps, 'julian_day')
        datestr = self.cycle
        kwargs = {'datestr': datestr, 'in_frmttyp': '%Y-%m-%d_%H:%M:%S',
                  'out_frmttyp': '%Y-%m-%d_%H:%M:%S', 'offset_seconds':
                  int(self.window)}
        datestr = hafsutil.date_interface.datestrfrmt(**kwargs)
        kwargs = {'datestr': datestr}
        datecomps = hafsutil.date_interface.datestrcomps(**kwargs)
        self.julian_stop = getattr(datecomps, 'julian_day')

    def get_vdmfiles(self):
        """
        DESCRIPTION:

        This method defines the base-class object attribute vdm_dict
        which is a Python dictionary containing both the filename
        paths that are within the user specified data assimilation
        window centered on the user specified forecast cycle/analysis
        time and also the time-stamp string corresponding to the
        filename attributes.

        """
        vdm_dict = dict()
        filenames = os.listdir(self.datapath)
        for filename in filenames:
            kwargs = {'filename': os.path.join(self.datapath, filename)}
            datestr = self._filetimestamp(**kwargs)
            kwargs = {'datestr': datestr}
            datecomps = hafsutil.date_interface.datestrcomps(**kwargs)
            julian_day = getattr(datecomps, 'julian_day')
            vdm_dict[julian_day] = dict()
            vdm_dict[julian_day]['filename'] =\
                os.path.join(self.datapath, filename)
            vdm_dict[julian_day]['timestamp'] = datestr
        self.vdm_dict = {k: v for k, v in vdm_dict.items() if (k >=
                                                               self.julian_start and k <= self.julian_stop)}

    def run(self):
        """
        DESCRIPTION:

        This method performs the following tasks:

        (1) Collects the vortex data message (VDM) files (if any)
            within the temporal period specified by the user.

        (2) Determines the VDM fix times.

        (3) Determines the VDM fix locations.

        (4) Collects the flight-level wind observations.

        """
        self.get_vdmfiles()
        self.get_fixtimes()
        self.get_fixlocations()
        self.get_flwinds()
        return self.vdm_dict

# ----


def main():
    """ 
    DESCRIPTION:

    This is the driver-level method to invoke the tasks within this
    script.

    """
    options = ObsPreProcVDMOptions()
    opts_obj = options.run()
    formatvdm = ObsPreProcVDM(opts_obj=opts_obj)
    formatvdm.run()

# ----


if __name__ == '__main__':
    main()
