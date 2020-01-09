# =========================================================================

# $$$ MODULE DOCUMENTATION BLOCK

# obs-preproc :: util/date_interface.py
# Copyright (C) 2019 Henry R. Winterbottom

# Email: henry.winterbottom@noaa.gov

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

# =========================================================================

"""
MODULE: 

   util.date_interface.py

DESCRIPTION:

   This module contains methods in order to manipulate date and time
   strings as required by the system workflow.

AUTHORS: 

   Henry R. Winterbottom; 08 January 2020

ABSTRACT:

   * datestrcomps; This method returns a Python object containing the
     user specified date string component values; the following
     attributes are returned:

       + year (year)

       + month of year (month)

       + day of month (day)

       + hour of day (hour)

       + minute of hour (minute)

       + second of minute (second)

       + full month name (month_name_long)

       + abbreviated month name (month_name_short)

       + full day name (weekday_long)

       + abbreviate day name (weekday_short)

       + date string (date_string; formatted as %Y-%m-%d_%H:%M:%S,
         assuming the UNIX convention).

       + cycle string (cycle_string; formatted as %Y%m%d%H, assuming
         the UNIX convention).

       + Julian date (julian_day).

   * datestrfrmt; This method ingests a date string of format
     (assuming UNIX convention) yyyy-mm-dd_HH:MM:SS; optional argument
     'offset_seconds' defines a new datestr relative to the user
     provided datestr and the number of seconds.

"""

# ----

import datetime
import sqlite3

# ----

__author__ = "Henry R. Winterbottom"
__copyright__ = "2019 Henry R. Winterbottom, NOAA/NCEP/EMC"
__version__ = "1.0.0"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__status__ = "Development"

# ----


def datestrcomps(datestr):
    """
    DESCRIPTION:

    This method returns a Python object containing the user specified
    date string component values; the following attributes are
    returned:

      + year (year)

      + month of year (month)

      + day of month (day)

      + hour of day (hour)

      + minute of hour (minute)

      + second of minute (second)

      + full month name (month_name_long)

      + abbreviated month name (month_name_short)

      + full day name (weekday_long)

      + abbreviate day name (weekday_short)

      + date string (date_string; formatted as %Y-%m-%d_%H:%M:%S,
        assuming the UNIX convention).

      + cycle string (cycle_string; formatted as %Y%m%d%H, assuming
        the UNIX convention).

      + Julian date (julian_day).

    INPUT VARIABLES:

    * datestr; a Python string containing a date string of, assuming
      the UNIX convention, format yyyy-mm-dd_HH:MM:SS.    

    OUTPUT VARIABLES:

    * date_comps_obj; a Python object containing the date string
      component values for the user specfied date string.

    """
    def date_comps_obj(): return None
    dateobj = datetime.datetime.strptime(datestr, '%Y-%m-%d_%H:%M:%S')
    comps_list = list()
    date_comps_dict = {'year': '%Y', 'month': '%m', 'day': '%d', 'hour': '%H',
                       'minute': '%M', 'second': '%S', 'month_name_long': '%B',
                       'month_name_short': '%b', 'weekday_long': '%A',
                       'weekday_short': '%a', 'date_string': '%Y-%m-%d_%H:%M:%S',
                       'cycle': '%Y%m%d%H'}
    for key in date_comps_dict.keys():
        value = datetime.datetime.strftime(dateobj, date_comps_dict[key])
        setattr(date_comps_obj, key, value)
        comps_list.append(key)
    connect = sqlite3.connect(':memory:')
    datestr = '%s-%s-%s %s:%s:%s' % (getattr(date_comps_obj, 'year'),
                                     getattr(date_comps_obj, 'month'), getattr(
                                         date_comps_obj, 'day'),
                                     getattr(date_comps_obj, 'hour'), getattr(
                                         date_comps_obj, 'minute'),
                                     getattr(date_comps_obj, 'second'))
    value = list(connect.execute('select julianday("%s")' % datestr))[0][0]
    setattr(date_comps_obj, 'julian_day', value)
    comps_list.append('julian_day')
    setattr(date_comps_obj, 'comps_list', comps_list)
    return date_comps_obj

# ----


def datestrfrmt(datestr, in_frmttyp, out_frmttyp, offset_seconds=None):
    """
    DESCRIPTION:

    This method ingests a date string of format (assuming UNIX
    convention) yyyy-mm-dd_HH:MM:SS; optional argument
    'offset_seconds' defines a new datestr relative to the user
    provided datestr and the number of seconds.

    INPUT VARIABLES:

    * datestr; a Python string containing a date string of, assuming
      the UNIX convention yyyy-mm-dd_HH:MM:SS.

    OPTIONAL INPUT VARIABLES:

    * offset_seconds; a Python integer defining the total number of
      offset-seconds relative to the datestr variable (see above) for
      the output time-stamp/date-string; the default is NoneType.

    OUTPUT VARIABLES:

    * outdatestr; a Python string containing the appropriately
      formatted time-stamp/date-string.

    """
    dateobj = datetime.datetime.strptime(datestr, in_frmttyp)
    if offset_seconds is not None:
        dateobj = dateobj+datetime.timedelta(0, offset_seconds)
    outdatestr = datetime.datetime.strftime(dateobj, out_frmttyp)
    return outdatestr
